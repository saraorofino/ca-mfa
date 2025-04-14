# ----------- Graph from old outputs for CLC meeting 02.04.25 ---------------------------
# Data
io_results <- read_csv(file.path(here::here("data/output/CA_EEIO_2012_2020_by_plastic_sectors.csv")))
waste <- read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA IO v2.xlsx")),
                   sheet ="Waste Generation") |> 
  row_to_names(row=1)

fate <- read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA IO v2.xlsx")),
                  sheet ="Waste Management") 

# Color palette 
lisa <- paletteer::paletteer_d("lisa::SandroBotticelli")
fish <- paletteer::paletteer_d("fishualize::Sardinella_brasiliensis")
arches <- paletteer::paletteer_d("nationalparkcolors::Arches")
crater <- paletteer::paletteer_d("nationalparkcolors::CraterLake")

sector_pal <- c(crater[c(3,6)], arches[5], fish[3], fish[4], fish[5], lisa[1], fish[2], lisa[2], arches[6], fish[1])

# Sector breakdown 
avg_prop_sector <- io_results |> 
  group_by(year) |>
  dplyr::mutate(annual_consumption_mt = sum(plastic_consumption_mt)) |>
  ungroup() |>
  group_by(plastic_sector) |> 
  summarize(plastic_consumption_mt = mean(plastic_consumption_mt),
            annual_consumption_mt = mean(annual_consumption_mt)) |> 
  ungroup() |> 
  dplyr::mutate(percent = (plastic_consumption_mt/annual_consumption_mt) * 100,
                rounded_tons = round(plastic_consumption_mt/1000000, 1)) |> 
  arrange(-percent)

sector_levs <- avg_prop_sector$plastic_sector

sector_plot <- ggplot(avg_prop_sector,
                      aes(x=percent, y=factor(plastic_sector, levels=sector_levs))) + 
  geom_col(aes(fill=factor(plastic_sector, levels=sector_levs))) +
  geom_text(aes(label = ifelse(plastic_sector == "Agriculture",
                               "0.02M tons",
                               paste0(rounded_tons, "M tons"))),
            position = position_nudge(x=2.9)) + 
  scale_fill_manual(values=sector_pal) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,45)) + 
  scale_y_discrete(expand = c(0,0),
                   limits = rev) + 
  labs(x="Percent of California annual plastic consumption 2012-2020",
       y="",
       fill = "Sector") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none") 


# Timeseries of consumption 
io_timeseries <- io_results |> 
  dplyr::mutate(plastic_consumption_millions = plastic_consumption_mt/1000000,
                plastic_sector = factor(plastic_sector, levels = sector_levs)) |> 
  ggplot() + 
  geom_line(aes(x=year, y=plastic_consumption_millions, color=plastic_sector, group=plastic_sector),
            linewidth = 0.8) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,3.5),
                     breaks = seq(0,3.5,0.5)) + 
  scale_color_manual(values = sector_pal) + 
  labs(x="",
       y="Plastic consumption (millions of tons)",
       color = "Sector") + 
  theme_bw() +
  theme(panel.grid.minor.y = element_blank())

# Waste plot by sector
waste_tidy <- waste |> 
  clean_names() |> 
  dplyr::select(-year_2) |> 
  pivot_longer(cols = c("agriculture":"consumption"),
               names_to = "sector",
               values_to = "waste_generation_mt") |> 
  dplyr::mutate(year = as.numeric(year),
                waste_generation_mt = as.numeric(waste_generation_mt), 
                sector = case_when(sector == "commercial_institutional" ~ "Commercial / Institutional",
                                   sector == "electrical_electronic" ~ "Electrical/Electronic",
                                   sector == "household_leisure_sports" ~ "Household / Leisure / Sports",
                                   TRUE ~ str_to_sentence(sector))) |> 
  dplyr::filter(!sector %in% c("Consumption", "Waste")) |> 
  dplyr::mutate(sector = factor(sector, levels=sector_levs))


waste_plot <- ggplot(waste_tidy) + 
  #geom_point(aes(x=year, y=waste_generation_mt, color=sector)) + 
  geom_line(aes(x=year, y=waste_generation_mt, 
                color=sector, group = sector),
            linewidth = 0.8) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,5)) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1950,2050,10)) + 
  scale_color_manual(values = sector_pal) + 
  labs(x="",
       y="Waste generation (mt)",
       color = "Sector") + 
  theme_bw() + 
  theme(panel.grid.minor.y=element_blank())



# Average fate 2012-2020
fate_sub <- fate[,c(1,4,6,8)]
colnames(fate_sub) <- c("year", "incinerated", "recycled", "landfilled")
fate_avg <- fate_sub |> 
  dplyr::filter(year != "year") |> 
  dplyr::mutate(across(.cols = c("year":"landfilled"),
                       .fns = function(x){as.numeric(x)})) |> 
  #dplyr::filter(year >= 2012 & year <= 2020) |> 
  pivot_longer(cols = c("incinerated":"landfilled"),
               names_to = "fate",
               values_to = "prop") |> 
  group_by(fate) |> 
  summarize(avg_prop = mean(prop, na.rm=T)) |> 
  ungroup()

# Save all plots
ggsave(filename = file.path(here::here("figures/sector_breakdown.png")),
       plot = sector_plot,
       width = 8, height = 6)

ggsave(filename = file.path(here::here("figures/plastic_consumption_by_sector_timeseries.png")),
       plot = io_timeseries,
       width = 8, height = 6)

ggsave(filename = file.path(here::here("waste_by_sector_timeseries.png")),
       plot = waste_plot,
       width = 8, height = 6)