####### This is how we estimated the original proportions, Roland then updated these which became the plastic_sector_classification.xlsx file in the raw folder #############
# Data
original_sectors <- read_xlsx(path=file.path(here::here("data/archive/2024_12_16_USEEIO_and_NAICS_crosswalk_2017_schema.xlsx")),
                              sheet = "Crosswalk") |> 
  clean_names() |> 
  # Remove NAICS... this is really just based on BEA and there are a lot of duplicates if you keep NAICS
  dplyr::select(bea_sector, bea_summary, bea_detail) |> 
  distinct()

plastic_sectors <- read_xlsx(path=file.path(here::here("data/archive/USEEIO_326_Leontif_RG_v2.xlsx")),
                             sheet = "Metadata") |> 
  clean_names()

results <- read_xlsx(path=file.path(here::here("data/archive/USEEIO_326_Leontif_RG_v2.xlsx")),
                     sheet = "IO_transposed") |> 
  clean_names()

bea_defs <- read_xlsx(path=file.path(here::here("data/archive/BEA-Industry-and-Commodity-Codes-and-NAICS-Concordance.xlsx")))

# Clean BEA definitions for summaries 
## Sectors and names
bea_sub <- bea_defs[, 2:3]
colnames(bea_sub) <- c("bea_sector", "sector_name")
bea_sectors <- bea_sub |> 
  filter(!is.na(bea_sector) & !is.na(sector_name)) |> 
  filter(!bea_sector %in% c("Summary")) |> 
  mutate(sector_name = str_to_sentence(sector_name))

# Detailed codes and names 
bea_sub2 <- bea_defs[,4:5]
colnames(bea_sub2) <- c("bea_detail", "bea_description")
bea_details <- bea_sub2 |> 
  filter(!is.na(bea_description) & !is.na(bea_detail))


# The code column relates the reclassified sectors to the original sectors 
## This is the original classification Darcy and I did mapped on to all BEA sectors
classify <- original_sectors |> 
  # Remove the Final, Used, Other, and V sectors - these aren't really relevant 
  dplyr::filter(! bea_sector %in% c("F010", "F020", "F030", "F040", "F050", "F100",
                                    "Other", "Used", "V001", "V002", "V003")) |> 
  left_join(plastic_sectors |> 
              filter(!is.na(plastic_relevant_sector)) |> 
              dplyr::select(code, category, plastic_sector = plastic_relevant_sector, name, purchaser_id=id),
            by = c("bea_summary" = "code")) #402 sectors

## Reclassify: trying to allocate things out of Other and into more appropriate categories
## Using the BEA-Industry-and-Commodity-Codes-and-NAICS-Concordance.xlsx to define the BEA detailed categories 
reclassify <- classify |> 
  dplyr::mutate(plastic_sector = case_when(
    # Anything already in Other - Healthcare to just Healthcare
    plastic_sector == "Other - Healthcare" ~ "Healthcare",
    # The entire summary category is changing to a new sector  
    bea_summary %in% c("3361MV", "3364OT", "441") ~ "Transportation",
    bea_summary %in% c("22", "5415", "514") ~ "Electrical/Electronic",
    bea_summary %in% c("711AS", "713", "721") ~ "Household / Leisure / Sports",
    bea_summary %in% c("511", "5411", "5412OP", "55", "561", "61", "624",
                       "GFGD", "GFGN", "GSLE") ~ "Commercial / Institutional",
    # Only part of the summary category is changing; grouped by the sector they are changing to 
    bea_detail %in% c("325310", "325320") ~ "Agriculture",
    bea_detail %in% c("325110", "325120", "325180", "325190", "325211", "325510", "3259A0",
                      "326120", "326130", "326220", 
                      "339960",
                      "423800",
                      "811300") ~ "Building & Construction",
    bea_detail %in% c("339940",
                      "423400",
                      "813100", "813A00", "813B00",
                      "491000", "S00102",
                      "S00203",
                      "GSLGE", "GSLGO") ~ "Commercial / Institutional",
    bea_detail %in% c("425000", 
                      "811200",
                      "S00101", 
                      "S00202") ~ "Electrical/Electronic",
    bea_detail %in% c("325411", "325412", "325413", "325414",
                      "339112", "339113", "339114", "339115", "339116",
                      "GSLGH") ~ "Healthcare",
    bea_detail %in% c("325520", "325610", "325620", "325910", 
                      "326150",
                      "339910", "339920", "339930", "339950",
                      "423600",
                      "444000", "446000", "4B0000",
                      "811400", "812100", "812200", "812900", "814000") ~ "Household / Leisure / Sports",
    bea_detail %in% c("424400",
                      "448000","812300") ~ "Packaging",
    bea_detail %in% c("325130", "3252A0") ~ "Textiles",
    bea_detail %in% c("326210",
                      "423100",
                      "811100",
                      "S00201") ~ "Transportation",
    bea_detail %in% c("326190", "326290") ~ "Other - Plastic",
    TRUE ~ plastic_sector)
  )

# Save a version for the supplement including: plastic sector, bea industry, bea summary code, bea specific code, industry name
## Arrange by plastic sector 
sector_assign_save <- reclassify |> 
  left_join(bea_sectors, by = c("bea_summary" = "bea_sector")) |> 
  left_join(bea_details, by = "bea_detail") |> 
  dplyr::select(plastic_sector, bea_sector=bea_summary, bea_sector_name = sector_name, bea_detail, bea_description) |> 
  arrange(plastic_sector)


# Reallocate based on the relative consumption from the US model 
us_props <- fed_consumption |> 
  left_join(reclassify |> 
              dplyr::select(bea_detail, bea_summary, plastic_sector),
            by = "bea_detail") |> 
  # Filter out sectors we aren't keeping 
  dplyr::filter(!bea_detail %in% c('S00401', 'S00402', 'S00300', 'S00900')) |> 
  ## Fill in a few NAs - doesn't seem relevant but want to do this for completeness 
  dplyr::mutate(bea_summary = case_when(bea_detail %in% c('335221', '335222', '335224', '335228') ~ '335',
                                        bea_detail %in% c('562111', '562HAZ', '562212', '562213', '562910',
                                                          '562920', '562OTH') ~ '562',
                                        bea_detail == '33391A' ~ '333',
                                        TRUE ~ bea_summary),
                plastic_sector = case_when(bea_detail %in% c('335221', '335222', '335224', '335228') ~ 'Electrical/Electronic',
                                           bea_detail %in% c('562111', '562HAZ', '562212', '562213', '562910',
                                                             '562920', '562OTH') ~ 'Other - Services',
                                           bea_detail == '33391A' ~ 'Building & Construction',
                                           TRUE ~ plastic_sector)) |> 
  group_by(bea_summary) |> 
  mutate(bea_sector_total = sum(us_consumption)) |> 
  ungroup() |> 
  group_by(bea_summary, bea_sector_total, plastic_sector) |> 
  summarize(us_consumption = sum(us_consumption)) |> 
  ungroup() |> 
  mutate(prop = us_consumption / bea_sector_total) 

# Send to Roland
us_props_save <- us_props |> 
  left_join(plastic_sectors |> 
              filter(!is.na(plastic_relevant_sector)) |> 
              dplyr::select(id, bea_summary=code) |> 
              distinct(),
            by = "bea_summary") |> 
  filter(!is.na(bea_summary)) |> 
  dplyr::select(sector_code = id, plastic_sector, proportion=prop)


####### Old approach to get full Leontief results, we are now using the power series approach #######
io_raw <- read_xlsx(path=file.path(here::here("data/raw/CAEEIO_326_output_2012_2020_v3.xlsx")),
                    sheet = "Deflated plastic intensity") |> 
  replace_na(list("Plastic Intensity ($/metric ton)" = "NA")) |> 
  janitor::row_to_names(row_number = 5) # use sector as column name

# Keep only the relevant columns of the IO data with years
io_annual <- io_raw |> 
  pivot_longer(cols = c("111CA/US-CA":"Other/RoUS"),
               names_to = "sector",
               values_to = "value") |> 
  dplyr::rename("variable" = "NA") |> 
  filter(variable %in% c("CA Plastic Consumption 2012", "CA Plastic Consumption 2013", "CA Plastic Consumption 2014",
                         "CA Plastic Consumption 2015", "CA Plastic Consumption 2016", "CA Plastic Consumption 2017",
                         "CA Plastic Consumption 2018", "CA Plastic Consumption 2019", "CA Plastic Consumption 2020")) |> 
  dplyr::mutate(year = as.numeric(parse_number(variable)),
                value = as.numeric(value)) |> 
  dplyr::select(year, sector_code = sector, ca_consumption = value)

# Classify BEA industries into plastic-relevant sectors
classify_io <- io_annual |> 
  # remove the CA-US or RoUS so it matches to the sector lookup 
  dplyr::mutate(sector_match = str_split(sector_code, "/", simplify=T)[,1]) |> 
  filter(!sector_match %in% c("Used", "Other")) |> 
  left_join(plastic_sectors, by = c("sector_match" = "bea_summary")) |> 
  dplyr::mutate(plastic_consumption = ca_consumption * prop) |> 
  # Group automotive and transportation together 
  dplyr::mutate(plastic_sector = ifelse(plastic_sector == "Automotive", "Transportation", plastic_sector)) |> 
  group_by(year, plastic_sector) |> 
  summarize(plastic_consumption_mt = sum(plastic_consumption)) |> 
  ungroup()

# Save
write_csv(classify_io, file.path(here::here("data/output/CA_EEIO_2012_2020_by_plastic_sectors.csv")))