library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(ggplot2)
library(paletteer)
library(showtext) # for Barlow font
library(ggalluvial)
library(igraph)
library(ggraph)

# Figure elements -----------------------------------
# Font
font_add_google("Barlow", family = "barlow")

# Plot theme 
black40 = "#abadb0"
black100 = "#000000"
plot_theme <- theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color=black40, size=0.18),
        panel.background = element_rect(fill="#F2F2F2", color=NA),
        panel.border = element_blank(),
        legend.position = "right",
        legend.text = element_text(color=black100, family="barlow", size=8),
        legend.title = element_text(color=black100, family="barlow", size=8),
        axis.text = element_text(color=black100, family="barlow", size=8),
        axis.title = element_text(color=black100, family="barlow", size=8),
        axis.ticks.x = element_line(color=black40, size=0.18))

# Width/height for saving
w <- 6.915
h <- 1.9618

# Color palettes (from Danielle)
## 11 colors for the different sectors 
sector_pal <- c("#05641c", "#80bb42", "#7bccc4", "#a8deb5", "#4fb3d2", "#2b8cbf",
                "#0768ac", "#074081", "#df8226", "#fbb764", "#d0bea9")

## Waste management palette
wm_pal <- c("#fdcb6b", "#06063d", "#abdddf")

## BAU total 
total_color <- "#d1bfaa"

## Policy interventions palette
policy_pal <- c("#49a842", "#074081", "#2b8cbf", "#05641c")

## Impact palette 
impact_pal <- c("#80bb42", "#00291f")


# Data -----------------------------------
results_file <- file.path(here::here("data/output/CA Plastic MFA Model v14.xlsx"))

## Aggregated policy results
results_raw <- read_xlsx(path = results_file,
                    sheet ="Inputs & Results")

consumption_agg <- results_raw |> 
  row_to_names(16) |> 
  clean_names() |> 
  dplyr::select(year, bau, sb54, byo) |> 
  pivot_longer(cols = c("bau":"byo"),
               names_to = "scenario",
               values_to = "consumption_mt") |> 
  mutate(across(.cols=c("year", "consumption_mt"),
                .fns=function(x){as.numeric(x)}))

waste_agg <- results_raw |> 
  row_to_names(16) |> 
  clean_names() |> 
  dplyr::select(year, bau=bau_2, sb54=sb54_2, byo=byo_2) |> 
  pivot_longer(cols = c("bau":"byo"),
               names_to = "scenario",
               values_to = "waste_generation_mt") |> 
  mutate(across(.cols=c("year", "waste_generation_mt"),
                .fns=function(x){as.numeric(x)}))

fate_agg <- results_raw |> 
  row_to_names(16) |> 
  clean_names() |> 
  dplyr::select(year, bau=bau_3, sb54=sb54_3, byo=byo_3) |> 
  pivot_longer(cols = c("bau":"byo"),
               names_to = "scenario",
               values_to = "waste_generation_mt") |> 
  mutate(fate = "landfilled") |> 
  bind_rows(results_raw |> 
              row_to_names(16) |> 
              clean_names() |> 
              dplyr::select(year, bau=bau_4, sb54=sb54_4, byo=byo_4) |> 
              pivot_longer(cols = c("bau":"byo"),
                           names_to = "scenario",
                           values_to = "waste_generation_mt") |> 
              mutate(fate = "recycled")) |> 
  mutate(across(.cols=c("year", "waste_generation_mt"),
                .fns=function(x){as.numeric(x)}))

# Figures tab has policy results 
policy_raw <- read_xlsx(path = results_file,
                        sheet = "Figures") |> 
  clean_names()

totals <- policy_raw |> 
  dplyr::select(intervention=x3, total_reduction=decrease_in_plastic_production_and_disposal_in_mt) |> 
  filter(!is.na(total_reduction)) |> 
  bind_cols(metric = c(rep("plastic_production", 8), 'NA', rep('ghg', 8))) |> 
  filter(!is.na(intervention)) |> 
  mutate(total_reduction = as.numeric(total_reduction)) |> 
  # BAU totals from GHG layer sheet 
  mutate(bau_values = ifelse(metric=="plastic_production", 307.06, 1109.91)) |> 
  mutate(percent_delta = (total_reduction/bau_values) * 100)

# Combine all policy results 
## Very manual -- specific code for each set of policies... 
policy_levers <- policy_raw |> 
  dplyr::select(intervention=x3, sr, reduced_recycling) |> 
  slice(c(1:2, 17:18)) |> 
  bind_cols(metric = c(rep('plastic_production', 2), rep('ghg', 2))) |> 
  pivot_longer(cols = c('sr','reduced_recycling'),
               names_to = 'mechanism',
               values_to = 'value') |> 
  bind_rows(policy_raw |> 
              dplyr::select(intervention=x3, oos=sr, is=reduced_recycling) |> 
              slice(c(3:4, 19:20)) |> 
              filter(intervention == '40% PCR') |> 
              bind_cols(metric = c('plastic_production','ghg')) |>
              pivot_longer(cols = c('oos','is'),
                           names_to = 'mechanism',
                           values_to = 'value')) |> 
  bind_rows(policy_raw |> 
              dplyr::select(intervention=x3, sr, reduced_recycling) |> 
              slice(c(5:7, 21:23)) |> 
              filter(intervention != 'SB54') |> 
              bind_cols(metric = c(rep('plastic_production', 2), rep('ghg', 2))) |> 
              pivot_longer(cols = c('sr','reduced_recycling'),
                           names_to = 'mechanism',
                           values_to = 'value')) |> 
  bind_rows(policy_raw |> 
              dplyr::select(intervention=x3, sr, oos=reduced_recycling, cfr=x7, cfr_pcr=x8) |> 
              slice(c(8:10, 24:26)) |> 
              filter(intervention != 'All 3 interventions') |> 
              bind_cols(metric = c(rep('plastic_production', 2), rep('ghg', 2))) |> 
              pivot_longer(cols = c('sr':'cfr_pcr'),
                           names_to = 'mechanism',
                           values_to = 'value')) |> 
  mutate(mechanism = case_when(metric=='ghg' & mechanism=='cfr' ~ 'cfr_pcr',
                               intervention %in% c('25% relative SR & 65% CfR', '25% absolute SR & 65% CfR') 
                               & mechanism == 'reduced_recycling' ~ 'cfr',
                               TRUE ~ mechanism),
         value = as.numeric(value)) |> 
  filter(!is.na(value)) |> 
  # Add in 65% CfR, which has only totals 
  bind_rows(totals |> 
              filter(intervention=='65% CfR') |> 
              mutate(mechanism='cfr',
                     value=as.numeric(total_reduction)) |> 
              dplyr::select(intervention, metric, mechanism, value))

## GHG results 
ghg <- read_xlsx(path = results_file,
                 sheet ="GHG layer") |> 
  row_to_names(1) |> 
  clean_names() |> 
  dplyr::select(na:byo_3) |> 
  head(33) |> # keep just top half, bottom half is plastic values not co2e
  dplyr::filter(!is.na(na)) |> 
  pivot_longer(cols = c("bau":"byo_3"),
               names_to = "scenario",
               values_to = "co2e_mt") |> 
  dplyr::mutate(metric = case_when(scenario %in% c("bau", "sb54", "byo", "oos_recycling") ~ "production",
                                    scenario %in% c("bau_2", "sb54_2", "byo_2") ~ "disposal",
                                    scenario %in% c("bau_3", "sb54_3", "byo_3") ~ "avoided_production"),
                scenario = case_when(str_detect(scenario, "bau") ~ "BAU",
                                     str_detect(scenario, "sb54") ~ "SB 54",
                                     str_detect(scenario, "byo") ~ "BYO",
                                     str_detect(scenario, "oos_recycling") ~ "BYO"),
                co2e_mt = as.numeric(co2e_mt)) |> 
  group_by(na, scenario, metric) |> 
  summarize(co2e_mt = sum(co2e_mt)) |> 
  ungroup() |> 
  dplyr::select(year=na, scenario, metric, co2e_mt) |> 
  filter(!is.na(metric))

cumulative_ghg <- ghg |> 
  filter(year == '2025-2050') |> 
  group_by(year, scenario) |> 
  summarize(total_co2e_mt = sum(co2e_mt)) |> 
  ungroup() |> 
  mutate(diff_from_bau = total_co2e_mt - total_co2e_mt[scenario=='BAU'])

# Data by sector -----------------------------------
consumption_sector <- read_xlsx(path = results_file,
                         sheet ="BAU Consum") |> 
  row_to_names(row=1) |> 
  clean_names() |> 
  dplyr::select(year, annual_consumption_mt = io, ag:other) |> 
  mutate(scenario = "BAU") |> 
  filter(!is.na(ag)) |> 
  bind_rows(read_xlsx(path = results_file,
                      sheet ="SB54 Consum") |> 
              row_to_names(row=2) |> 
              clean_names() |> 
              dplyr::select(year, annual_consumption_mt = in_mt_2, ag:other) |> 
              mutate(scenario = "SB 54")) |> 
  pivot_longer(cols = c("ag":"other"),
              names_to = "plastic_sector",
              values_to = "plastic_consumption_mt") |> 
  mutate(across(.cols=c("year", "annual_consumption_mt", "plastic_consumption_mt"),
                .fns=function(x){as.numeric(x)})) |> 
  mutate(plastic_sector = case_when(plastic_sector == 'pack' ~ "Packaging",
                                    plastic_sector == 'b_c' ~ "Building/Construction",
                                    plastic_sector == 'trans' ~ "Transportation",
                                    plastic_sector == 'health' ~ "Healthcare",
                                    plastic_sector == 'com_inst' ~ "Commercial/Institutional",
                                    plastic_sector == 'e_e' ~ "Electrical/Electronic",
                                    plastic_sector == 'hl_s' ~ "Household/Leisure/Sports",
                                    plastic_sector == 'mach' ~ "Machinery",
                                    plastic_sector == 'tex' ~ "Textiles",
                                    plastic_sector == 'other' ~ "Other",
                                    plastic_sector == 'ag' ~ "Agriculture"))

waste_sector <- read_xlsx(path = results_file,
                   sheet ="BAU Waste Gen") |> 
  row_to_names(row=1) |> 
  clean_names() |> 
  dplyr::select(year, annual_waste_mt = waste, ag:other) |> 
  mutate(scenario = "BAU") |> 
  bind_rows(read_xlsx(path = results_file,
                      sheet ="SB54 Waste Gen") |> 
              row_to_names(row=1) |> 
              clean_names() |> 
              dplyr::select(year, annual_waste_mt = sb54_2, ag:other) |> 
              mutate(scenario = "SB 54") |> 
              filter(!is.na(year))) |> 
  pivot_longer(cols = c("ag":"other"),
               names_to = "plastic_sector",
               values_to = "plastic_waste_mt") |> 
  mutate(across(.cols=c("year", "annual_waste_mt", "plastic_waste_mt"),
                .fns=function(x){as.numeric(x)})) |> 
  mutate(plastic_sector = case_when(plastic_sector == 'pack' ~ "Packaging",
                                    plastic_sector == 'b_c' ~ "Building/Construction",
                                    plastic_sector == 'trans' ~ "Transportation",
                                    plastic_sector == 'health' ~ "Healthcare",
                                    plastic_sector == 'com_inst' ~ "Commercial/Institutional",
                                    plastic_sector == 'e_e' ~ "Electrical/Electronic",
                                    plastic_sector == 'hl_s' ~ "Household/Leisure/Sports",
                                    plastic_sector == 'mach' ~ "Machinery",
                                    plastic_sector == 'tex' ~ "Textiles",
                                    plastic_sector == 'other' ~ "Other",
                                    plastic_sector == 'ag' ~ "Agriculture"))

collection_rates <- read_xlsx(path = results_file,
                              sheet ="BAU Waste Man") |> 
  row_to_names(row=2) |> 
  clean_names() |> 
  dplyr::select(year, collection_rate = in_percent_3) |> 
  mutate(scenario = "BAU") |> 
  bind_rows(read_xlsx(path = results_file,
                      sheet ="SB54 Waste Man") |> 
              row_to_names(row=2) |> 
              clean_names() |> 
              dplyr::select(year, collection_rate = in_percent) |> 
              mutate(scenario = "SB 54")) |> 
  mutate(across(.cols=c("year", "collection_rate"),
                .fns=function(x){as.numeric(x)}))

bau_fate <- read_xlsx(path = results_file,
                      sheet ="BAU Waste Man") |> 
  row_to_names(row=2) |> 
  clean_names() |> 
  dplyr::select(year, recycled = in_kt, incinerated = in_kt_2, landfilled = in_kt_3) |> 
  pivot_longer(cols = c("recycled":"landfilled"),
               names_to = "fate",
               values_to = "waste_generation_kt") |> 
  mutate(across(.cols=c("year", "waste_generation_kt"),
                .fns=function(x){as.numeric(x)})) |> 
  mutate(waste_generation_mt = waste_generation_kt / 1000,
         fate = factor(fate, levels = c("landfilled", "recycled", "incinerated"))) |> 
  dplyr::select(-waste_generation_kt)

# Validation data -----------------------------------
prj_methods <- read_xlsx(path = results_file,
                         sheet ="BAU Consum") |> 
  row_to_names(row=1) |> 
  clean_names() |> 
  dplyr::select(year:gdp) |> 
  pivot_longer(cols=c("pop":"gdp"),
               names_to = "projection_method",
               values_to = "consumption_mt") |> 
  mutate(across(.cols=c("year", "consumption_mt"),
                .fns=function(x){as.numeric(x)}))

# Economic sectors -----------------------
industry_to_plastic <- read_csv(file.path(here::here("data/processed/industry_to_plastic_sector.csv"))) |> 
  mutate(plastic_sector = case_when(plastic_sector == "Automotive" ~ "Transportation",
                                    plastic_sector == "Construction" ~ "Building/Construction",
                                    plastic_sector == "Household / Leisure / Sports" ~ "Household/Leisure/Sports",
                                    plastic_sector == "Commercial / Institutional" ~ "Commercial/Institutional",
                                    TRUE ~ plastic_sector),
         bea_summary = ifelse(bea_sector=='562', '56', bea_summary),
         summary_name = case_when(bea_sector=='562' ~ '56: Administrative and Support and Waste Management and Remediation Services',
                                  bea_summary=='Other Activities/Government' ~ 'Other Activities/Government',
                                  TRUE ~ summary_name)) |> 
  # Remove Used/Other global adjustments for this
  filter(!is.na(summary_name)) |> 
  dplyr::select(bea_summary, summary_name, bea_sector, plastic_sector, plastic_consumption)

# Stats-----------------------------------
# CA population 2020 = 7.05 million
ca_pop <- 7.05390369249804*1000000

## 2020 per capita consumption in CA 
consumption2020 <- consumption_agg |> 
  filter(year == 2020 & scenario == 'bau') |> 
  pull(consumption_mt)

# Tons per person
per_cap_2020 <- (consumption2020*1000000)/ca_pop
per_cap_2020kg <- per_cap_2020 * 1000 # kg to match other values

## 2020 GHG emissions per capita 
ghg2020 <- ghg |> 
  filter(year == '2020' & scenario == 'BAU') |> 
  group_by(year) |> 
  summarize(co2e_mt = sum(co2e_mt)) |> 
  ungroup()

per_cap_ghg <- (ghg2020$co2e_mt * 1000000) / ca_pop

# By 2050 total plastic consumption and % packaging 
proj2050 <- consumption_sector |> 
  filter(scenario == "BAU" & year == 2050) |> 
  mutate(prop = plastic_consumption_mt / annual_consumption_mt)

# Main Figures-----------------------------------
consumption_levs <- c("Packaging", "Building/Construction", "Transportation", "Healthcare",
                      "Commercial/Institutional", "Electrical/Electronic", "Household/Leisure/Sports",
                      "Machinery", "Textiles", "Agriculture", "Other") # match Danielle's preference

# Fig. 1 basic sankey diagram 2025-2035 -- Danielle will do the final touches of this 
by_sector <- consumption_sector |> 
  dplyr::filter(year >= 2012 & year <= 2020) |> 
  group_by(plastic_sector) |> 
  summarize(plastic_consumption_mt = sum(plastic_consumption_mt)) |> 
  ungroup() |> 
  left_join(waste_sector |> 
              dplyr::filter(year >= 2012 & year <= 2020) |> 
              group_by(plastic_sector) |> 
              summarize(plastic_waste_mt = sum(plastic_waste_mt)) |> 
              ungroup(), by="plastic_sector") |> 
  dplyr::mutate(recycled_mt = plastic_waste_mt * 0.086,
                landfilled_mt = plastic_waste_mt - recycled_mt) |> 
  pivot_longer(cols = c("recycled_mt":"landfilled_mt"),
               names_to = "class",
               values_to = "plastic_mt") |> 
  mutate(alluvium = ifelse(class == "recycled_mt", paste(plastic_sector, "recycled", sep="-"),
                              paste(plastic_sector, "landfill", sep="-")),
         class = "fate") |> 
  bind_rows(data.frame(plastic_sector=unique(consumption_sector$plastic_sector)) |> 
                          mutate(alluvium = paste(plastic_sector, "lost", sep="-"),
                                   class = "fate",
                                   plastic_mt = 0)) 

## Totals by fate for plot labels
fate_total <- waste_sector |> 
  dplyr::filter(year >= 2012 & year <= 2020) |> 
  group_by(plastic_sector) |> 
  summarize(plastic_waste_mt = sum(plastic_waste_mt)) |> 
  ungroup() |> 
  dplyr::mutate(recycled_mt = plastic_waste_mt * 0.086,
                landfilled_mt = plastic_waste_mt - recycled_mt) |> 
  pivot_longer(cols = c("recycled_mt":"landfilled_mt"),
               names_to = "fate",
               values_to = "plastic_mt") |> 
  group_by(fate) |> 
  summarize(plastic_mt = sum(plastic_mt)) |> 
  ungroup()

# by sector represents the final pillar of the sankey, need to add in allivials for other two pillars
connections <- by_sector |> 
  dplyr::select(plastic_sector, alluvium, class, plastic_mt) |> 
  # Waste is exactly the same as fate
  bind_rows(by_sector |> 
              dplyr::mutate(class="waste") |> 
              dplyr::select(plastic_sector, alluvium, class, plastic_mt)) |> 
  # Consumption has nonzero "lost" to make up the difference from consumption to waste 
  bind_rows(by_sector |> 
              filter(!is.na(plastic_consumption_mt)) |> 
              mutate(class="consumption") |> 
              dplyr::select(plastic_sector, alluvium, class, plastic_mt) |> 
              bind_rows(by_sector |> 
                          filter(!is.na(plastic_consumption_mt)) |> 
                          mutate(plastic_mt = plastic_consumption_mt-plastic_waste_mt,
                                 class="consumption",
                                 alluvium = paste(plastic_sector, "lost", sep="-")) |> 
                          dplyr::select(plastic_sector, alluvium, class, plastic_mt) |> 
                          distinct()
                        )
            ) |> 
  # Create stratum for displaying sections
  mutate(stratum = case_when(class=="fate" & str_detect(alluvium, "recycled") ~ "Recycled",
                             class=="fate" & str_detect(alluvium, "landfill") ~ "Landfilled",
                             class=="fate" & str_detect(alluvium, "lost") ~ "Landfilled",
                             class %in% c("consumption", "waste") ~ plastic_sector)) |> 
  # Factors
  mutate(class=factor(class, levels=c("consumption", "waste", "fate")),
         plastic_sector=factor(plastic_sector, levels=consumption_levs),
         stratum=factor(stratum, levels=c(consumption_levs, "Landfilled", "Recycled")))


fig1 <- ggplot(connections,
       aes(x=class, y=plastic_mt,
           stratum=stratum, alluvium=alluvium, 
           label=stratum, fill=stratum)) + 
  geom_flow() +
  geom_stratum(color='white', size=0.15) +
  scale_x_discrete(expand = c(.1, .1),
                   breaks = c("consumption", "waste", "fate"),
                   labels = c("Plastic consumption", "Plastic waste generation", "Plastic waste management")) +
  #geom_text(stat = "stratum", size = 3, family="Barlow") +
  scale_fill_manual(values = c(sector_pal, wm_pal[1], wm_pal[2])) + 
  labs(x="", y="") + 
  plot_theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank()) 

ggsave(filename = file.path(here::here('figures/fig1.pdf')),
       plot = fig1,
       width = w, height = h)  

# Fig. 2 Multi-panel MFA results for BAU (save individually, Danielle will finalize)
## A: BAU consumption
## Consumption
bau_consumption_timeseries <- consumption_sector |> 
  filter(scenario == "BAU") |> 
  dplyr::mutate(plastic_sector = factor(plastic_sector, levels = consumption_levs)) |> 
  ggplot() + 
  geom_line(aes(x=year, y=plastic_consumption_mt, color=plastic_sector, group=plastic_sector),
            linewidth = 0.8) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1950,2050,10)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,6),
                     breaks = seq(0,6,2)) + 
  scale_color_manual(values = c(sector_pal)) + 
  labs(x="",
       y="Plastic (Mt)",
       #subtitle = "Consumption",
       color = "") + 
  plot_theme + 
  theme(legend.key.height = unit(4, "mm"),
        legend.justification = "top",
        legend.box = "vertical") + 
  guides(color = guide_legend(ncol = 1))

ggsave(filename = file.path(here::here('figures/fig2_a.eps')),
       plot = bau_consumption_timeseries,
       width = w, height = h, units="in")

## B: BAU waste generation 
bau_waste_timeseries <- waste_sector |> 
  filter(scenario == "BAU") |> 
  dplyr::mutate(plastic_sector = factor(plastic_sector, levels = consumption_levs)) |> 
  ggplot() + 
  geom_line(aes(x=year, y=plastic_waste_mt, 
                color=plastic_sector, group = plastic_sector),
            linewidth = 0.8) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,6),
                     breaks = seq(0,6,2)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(1950,2050),
                     breaks = seq(1950,2050,10)) + 
  scale_color_manual(values = sector_pal) + 
  labs(x="",
       y="Plastic (Mt)",
       #subtitle = "Waste Generation",
       color = "") + 
  plot_theme + 
  theme(legend.key.height = unit(4, "mm"),
        legend.justification = "top",
        legend.box = "vertical") + 
  guides(color = guide_legend(ncol = 1))

ggsave(filename = file.path(here::here('figures/fig2_b.png')),
       plot = bau_waste_timeseries,
       width = w, height = h, units="in")


## C: BAU waste management
bau_fate_timeseries <- ggplot() + 
  geom_line(data = bau_fate, 
            aes(x=year, y=waste_generation_mt, color=fate, group = fate),
            linewidth = 0.8) + 
  scale_color_manual(values = wm_pal,
                     labels = c("Landfilled", "Recycled", "Incinerated")) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,12),
                     breaks = seq(0,12,3)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(1950,2050),
                     breaks = seq(1950,2050,10)) + 
  labs(x="",
       y="Plastic (Mt)",
       #subtitle = "Waste management",
       color = "") + 
  plot_theme + 
  theme(legend.key.height = unit(4, "mm"),
        legend.justification = "top",
        legend.box = "vertical") + 
  guides(color = guide_legend(ncol = 1))

ggsave(filename = file.path(here::here('figures/fig2_c.eps')),
       plot = bau_fate_timeseries,
       width = w, height = h, units="in")

# Fig. 3 Policy results 
policy_lev <- c('25% absolute SR', '65% CfR', '40% PCR',
                '25% absolute SR & 65% CfR', 
                '25% absolute SR, 40% PCR & 65% CfR')
mechanism_lev <- c('is', 'oos', 'cfr', 'sr') 

# A: Avoided plastic production 
fig3_a <- policy_levers |> 
  filter(mechanism != "reduced_recycling" & metric=='plastic_production') |>
  # Combine cfr_pcr with cfr because it's tiny 
  mutate(mechanism = ifelse(mechanism == "cfr_pcr", "cfr", mechanism)) |> 
  # Keep only the relative source reductions 
  filter(!intervention %in% c('25% relative SR', '25% relative SR & 65% CfR','25% relative SR, 40% PCR & 65% CfR')) |>  
  mutate(intervention = factor(intervention, levels=policy_lev),
         mechanism = factor(mechanism, levels=mechanism_lev)) |> 
  ggplot() + 
  geom_col(aes(x=intervention, y=value, fill=mechanism),
           color='white', size=0.15) + 
  scale_fill_manual(values = policy_pal,
                    breaks = c("is", "oos", "cfr", "sr"),
                    labels = c("Recycled content\n(in-state)", "Recycled content\n(out-of-state)",
                               "Collection\nfor recycling", "Source\nreduction")) +
  scale_x_discrete(expand = c(0.01,0.01),
                   breaks = c('25% absolute SR', '65% CfR', '40% PCR',
                              '25% absolute SR & 65% CfR', 
                              '25% absolute SR, 40% PCR & 65% CfR'),
                   labels = c('25% source\nreduction',
                              '65% collection\nfor recycling', '40% recycled\ncontent', 
                              '25% source reduction\n65% collection for recycling',
                              '25% source reduction\n65% collection for recycling\n40% recycled content')) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,75),
                     breaks = seq(0,75,25)) + 
  labs(x="", y="Avoided plastic production (Mt)",
       fill="Mechanism") + 
  guides(fill=guide_legend(reverse=TRUE, nrow=2)) + 
  plot_theme + 
  theme(legend.position = "none")

ggsave(filename = file.path(here::here('figures/fig3_a.eps')),
       plot = fig3_a,
       width = w, height = h, units = "in")

# B:GHG emissions 
fig3_b <- policy_levers |> 
  filter(mechanism != "reduced_recycling" & metric=='ghg') |>
  # Keep only the relative source reductions 
  filter(!intervention %in% c('25% relative SR', '25% relative SR & 65% CfR','25% relative SR, 40% PCR & 65% CfR')) |>  
  mutate(intervention = factor(intervention, levels=policy_lev),
         mechanism = factor(mechanism, levels=c("cfr_pcr", mechanism_lev))) |> 
  ggplot() + 
  geom_col(aes(x=intervention, y=value, fill=mechanism),
           color='white', size=0.15) + 
  scale_fill_manual(breaks = c('cfr_pcr', 'is', 'oos', 'cfr', 'sr'),
                    values = c('#d1bfaa', policy_pal),
                    labels = c('Collection for reycling\n& recycled content',
                               'Recycled content\n(in-state)', 'Recycled content\n(out-of-state)',
                               'Collection\nfor recycling', 'Source\nreduction')) +
  scale_x_discrete(expand = c(0.01,0.01),
                   breaks = c('25% absolute SR', '65% CfR', '40% PCR',
                              '25% absolute SR & 65% CfR', 
                              '25% absolute SR, 40% PCR & 65% CfR'),
                   labels = c('25% source\nreduction',
                              '65% collection\nfor recycling', '40% recycled content', 
                              '25% source reduction\n65% collection for recycling',
                              '25% source reduction\n65% collection for recycling\n40% recycled content')) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,200),
                     breaks = seq(0,200,50)) + 
  labs(x="", y="Avoided GHG emissions (Mt)",
       fill="Mechanism") + 
  guides(fill=guide_legend(reverse=TRUE, nrow=2)) + 
  plot_theme + 
  theme(legend.position = "bottom")

ggsave(filename = file.path(here::here('figures/fig3_b.eps')),
       plot = fig3_b,
       width = w, height = 3, units = "in")

# Fig. 4 Percent change 
fig4 <- totals |> 
  filter(!intervention %in% c('25% relative SR', '25% relative SR & 65% CfR','25% relative SR, 40% PCR & 65% CfR')) |>  
  mutate(intervention = factor(intervention, levels=policy_lev),
         metric = factor(metric, levels=c("plastic_production", "ghg"))) |> 
  ggplot() + 
  geom_col(aes(y=percent_delta, x=intervention, fill=metric),
           position='dodge') + 
  scale_x_discrete(expand = c(0.01,0.01),
                   breaks = c('25% absolute SR', '65% CfR', '40% PCR',
                              '25% absolute SR & 65% CfR', 
                              '25% absolute SR, 40% PCR & 65% CfR'),
                   labels = c('25% source\nreduction',
                              '65% collection\nfor recycling', '40% recycled content', 
                              '25% source reduction\n65% collection for recycling',
                              '25% source reduction\n65% collection for recycling\n40% recycled content')) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,25),
                     breaks = seq(0,25,5)) + 
  scale_fill_manual(values = impact_pal,
                    labels = c("Plastic production", "Greenhouse gas emissions")) + 
  labs(y="Percent change from BAU", x="", fill="Impact") + 
  plot_theme + 
  theme(legend.position = "bottom")

ggsave(filename = file.path(here::here('figures/fig4.eps')),
       plot = fig4,
       width = w, height = 3, units = "in")

# Supplemental/Report Figures-----------------------------------

#----------- MFA data -----------
# Average % of consumption by sector (for years we actually modeled 2012-2020) 
avg_prop_sector_consumption <- consumption_sector |>
  filter(scenario == "BAU" & year >= 2012 & year <= 2020) |> 
  mutate(percent = plastic_consumption_mt / annual_consumption_mt * 100) |> 
  group_by(plastic_sector) |> 
  summarize(plastic_consumption_mt = mean(plastic_consumption_mt),
            annual_consumption_mt = mean(annual_consumption_mt)) |> 
  ungroup() |> 
  dplyr::mutate(percent = (plastic_consumption_mt/annual_consumption_mt) * 100,
                rounded_tons = round(plastic_consumption_mt, 1)) |>
  arrange(-percent)

sector_levs <- avg_prop_sector_consumption$plastic_sector #slightly adjusted here to match actual data order
pal <- c(sector_pal[1:9], sector_pal[11], sector_pal[10])

sector_consumption_plot <- ggplot(avg_prop_sector_consumption,
                                  aes(x=percent, y=factor(plastic_sector, levels=sector_levs))) + 
  geom_col(aes(fill=factor(plastic_sector, levels=sector_levs))) +
  geom_text(aes(label = ifelse(plastic_sector == "Agriculture",
                               "0.04Mt/year",
                               paste0(rounded_tons, "Mt/year"))),
            position = position_nudge(x=3),
            size = 7.5, size.unit = "pt") + 
  scale_fill_manual(values=pal) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,50)) + 
  scale_y_discrete(expand = c(0,0),
                   limits = rev) + 
  labs(x="Percent of California annual plastic consumption 2012-2020",
       y="",
       fill = "Sector") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color=black40, linewidth = 0.18),
        axis.text= element_text(color=black100, family="barlow", size=8),
        axis.title = element_text(color=black100, family="barlow", size=8),
        legend.position = "none") 

ggsave(filename = file.path(here::here("figures/sector_breakdown_consumption.eps")),
       plot = sector_consumption_plot,
       width = w, height = h, units = "in")

# Average % of waste generation by sector (for years we actually modeled 2012-2020) 
avg_prop_sector_waste <- waste_sector |>
  filter(scenario == "BAU" & year >= 2012 & year <= 2020) |> 
  mutate(percent = plastic_waste_mt / annual_waste_mt * 100) |> 
  group_by(plastic_sector) |> 
  summarize(plastic_waste_mt = mean(plastic_waste_mt),
            annual_waste_mt = mean(annual_waste_mt)) |> 
  ungroup() |> 
  dplyr::mutate(percent = (plastic_waste_mt/annual_waste_mt) * 100,
                rounded_tons = round(plastic_waste_mt, 1)) |>
  arrange(-percent)

sector_waste_plot <- ggplot(avg_prop_sector_waste,
                            aes(x=percent, y=factor(plastic_sector, levels=sector_levs))) + 
  geom_col(aes(fill=factor(plastic_sector, levels=sector_levs))) +
  geom_text(aes(label = ifelse(plastic_sector == "Agriculture",
                               "0.03Mt/year",
                               paste0(rounded_tons, "Mt/year"))),
            position = position_nudge(x=3),
            size = 7.5, size.unit = "pt") + 
  scale_fill_manual(values=pal) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,55)) + 
  scale_y_discrete(expand = c(0,0),
                   limits = rev) + 
  labs(x="Percent of California annual plastic waste generation 2012-2020",
       y="",
       fill = "Sector") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color=black40, linewidth = 0.18),
        axis.text= element_text(color=black100, family="barlow", size=8),
        axis.title = element_text(color=black100, family="barlow", size=8),
        legend.position = "none") 

ggsave(filename = file.path(here::here("figures/sector_breakdown_waste.eps")),
       plot = sector_waste_plot,
       width = w, height = h, units = "in")

# Timeseries of plastic waste generation by sector as a proportion of the total
percent_waste_plot <- waste_sector |>
  filter(scenario == "BAU" & year>=2012) |> 
  mutate(percent = plastic_waste_mt / annual_waste_mt * 100,
         plastic_sector = factor(plastic_sector, levels=consumption_levs)) |>  
  ggplot() + 
  geom_line(aes(x=year, y=percent, color=plastic_sector, group=plastic_sector)) + 
  scale_x_continuous(expand = c(0.01,0.01),
                     limits = c(2012,2050),
                     breaks = seq(2015,2050,5)) + 
  scale_color_manual(values=sector_pal) + 
  labs(x="", y="Percent of annual waste generation",
       color="") + 
  plot_theme +
  theme(legend.key.height = unit(4, "mm"),
        legend.justification = "top",
        legend.box = "vertical") + 
  guides(color = guide_legend(ncol = 1))

ggsave(filename = file.path(here::here("figures/percent_waste_generation_by_sector.eps")),
       plot = percent_waste_plot,
       width = w, height = h, units = "in")

#----------- Validation data  -----------
## Timeseries 1950-2050 of MFA model, scaled NA by GDP, scaled NA by population
proj_plot <- ggplot(prj_methods) +
  geom_line(aes(x=year, y=consumption_mt, linetype=projection_method, group=projection_method)) + 
  scale_linetype_manual(breaks = c("gdp", "io", "pop"),
                        values = c("dashed", "solid", "dotted"),
                        labels = c("GDP-scaled\nNorth America model",
                                   "Input-output model",
                                   "Population-scaled\nNorth America model")) + 
  scale_x_continuous(expand=c(0.01,0.01),
                     limits=c(2020,2050),
                     breaks = seq(2020,2050, 10)) + 
  labs(x="", y="Plastic consumption (Mt)",
       linetype="") + 
  plot_theme

ggsave(filename = file.path(here::here('figures/projection_validation.eps')),
       plot = proj_plot,
       width = w, height = h, units="in")

#----------- Economic to plastic -----------
# Flows from economic sectors to plastic sectors 

# all to-from levels
edges <- industry_to_plastic |> 
  # From plastic sector to bea summary 
  mutate(to=paste(bea_summary, plastic_sector, sep="-")) |> 
  dplyr::select(from=plastic_sector, to) |> 
  distinct() |> 
  # From "root" to plastic sector 
  bind_rows(industry_to_plastic |> 
              dplyr::select(to=plastic_sector) |> 
              distinct() |> 
              mutate(from="root"))
  
vertices <- industry_to_plastic |> 
  mutate(name = paste(bea_summary, plastic_sector, sep="-")) |>
  group_by(name) |> 
  summarize(size=sum(plastic_consumption)) |> 
  ungroup() |> 
  bind_rows(industry_to_plastic |> 
              select(name=plastic_sector) |> 
              distinct() |> 
              dplyr::mutate(size=0)) |> 
  distinct() |> 
  bind_rows(data.frame(name="root",
                       size=0))

# Circular dendrogram 
econ_to_plastic <- ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(color=black40) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=factor(group, levels=consumption_levs), size=size),
                  alpha = 0.8) +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2.7, alpha=1) +
  scale_colour_manual(values= sector_pal) +
  theme_void() + 
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  )

ggsave(filename = file.path(here::here('figures/economic_to_plastic_sector.pdf')),
       plot = econ_to_plastic,
       # Make it more vertical than others
       width = 3, height = 4)  

#----------- Policy -----------
# Packaging only BAU vs. SB54 
packaging_consumption <- consumption_sector |> 
  dplyr::filter(plastic_sector == "Packaging")
scenario_levs <- c("bau", "sb54", "BAU", "SB 54")

packaging_plot <- ggplot(data = packaging_consumption |> 
         mutate(scenario = factor(scenario, levels=scenario_levs))) + 
geom_line(aes(x=year, y=plastic_consumption_mt, linetype = scenario, group=scenario),
          color = sector_pal[1],
          linewidth = 0.8) + 
  geom_vline(aes(xintercept = 2032),
             linetype = "dotted", color='gray30') + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,5.5),
                     breaks = seq(0, 5, 1)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(2012,2050),
                     breaks = seq(2010,2050,5)) + 
  labs(x="",
       y="Plastic (Mt)",
       linetype = "") + 
  guides(linetype = guide_legend(override.aes=list(linewidth=0.5))) + 
  plot_theme 
  
ggsave(filename = file.path(here::here('figures/packaging_sector.eps')),
       plot = packaging_plot,
       width = w, height = h, units = "in")

# Multi-panel timeseries of BAU vs. SB54 (save separately Danielle will finalize)
## A: Consumption 
bau_v_sb54_consumption <- ggplot() + 
  # Aggregated total consumption under BAU and SB54
  geom_line(data = consumption_agg |> 
              filter(scenario %in% c("bau", "sb54")) |> 
              mutate(scenario = factor(scenario, levels=scenario_levs)),
            aes(x=year, y=consumption_mt, linetype=scenario, group=scenario),
            color = total_color,
            linewidth = 0.8) + 
  # Packaging consumption under BAU and SB 54
  geom_line(data = packaging_consumption |> 
              mutate(scenario = factor(scenario, levels=scenario_levs)),
            aes(x=year, y=plastic_consumption_mt, linetype = scenario, group=scenario),
            color = sector_pal[1],
            linewidth = 0.8) + 
  # All other sectors (consumption is the same in both scenarios so only plot one)
  geom_line(data = consumption_sector |> 
              filter(scenario == "BAU" & plastic_sector != "Packaging"),
            aes(x=year, y=plastic_consumption_mt, group=plastic_sector),
            color = "#B2B2B2",
            linewidth = 0.6) + 
  geom_vline(aes(xintercept = 2032),
             linetype = "dotted", color='gray30') + 
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"),
                        breaks = c("bau", "sb54", "BAU", "SB 54"),
                        labels = c("BAU total", "SB 54 total", "BAU packaging sector", "SB 54 packaging sector")) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,15),
                     breaks = seq(0, 15, 3)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(2025,2050),
                     breaks = seq(2025,2050,5)) + 
  labs(x="",
       y="Plastic (Mt)",
       linetype = "") + 
  guides(linetype = guide_legend(override.aes=list(linewidth=0.5))) + 
  plot_theme 

ggsave(filename = file.path(here::here('figures/bau_v_sb54_consumption.png')),
       plot = bau_v_sb54_consumption,
       width = w, height = h, units = "in")

## B: Waste generation 
packaging_waste <- waste_sector |> 
  dplyr::filter(plastic_sector == "Packaging")

bau_v_sb54_waste <- ggplot() + 
  # Aggregated total waste under BAU and SB54
  geom_line(data = waste_agg |> 
              filter(scenario %in% c("bau", "sb54")) |> 
              mutate(scenario = factor(scenario, levels=scenario_levs)),
            aes(x=year, y=waste_generation_mt, linetype=scenario, group=scenario),
            color = total_color,
            linewidth = 0.8) + 
  # Packaging waste under BAU and SB 54
  geom_line(data = packaging_waste |> 
              mutate(scenario = factor(scenario, levels=scenario_levs)),
            aes(x=year, y=plastic_waste_mt, linetype = scenario, group=scenario),
            color = sector_pal[1],
            linewidth = 0.8) + 
  # All other sectors (waste is the same in both scenarios so only plot one)
  geom_line(data = waste_sector |> 
              filter(scenario == "BAU" & plastic_sector != "Packaging"),
            aes(x=year, y=plastic_waste_mt, group=plastic_sector),
            color = "#B2B2B2",
            linewidth = 0.6) + 
  geom_vline(aes(xintercept = 2032),
             linetype = "dotted", color='gray30') + 
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"),
                        breaks = c("bau", "sb54", "BAU", "SB 54"),
                        labels = c("BAU total", "SB 54 total", "BAU packaging sector", "SB 54 packaging sector")) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,15),
                     breaks = seq(0, 15, 3)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(2025,2050),
                     breaks = seq(2025,2050,5)) + 
  guides(linetype = guide_legend(override.aes=list(linewidth=0.5))) + 
  labs(x="",
       y="Plastic (Mt)",
       linetype = "") + 
  plot_theme

ggsave(filename = file.path(here::here('figures/bau_v_sb54_waste_generation.eps')),
       plot = bau_v_sb54_waste,
       width = w, height = h, units = "in")

## C: waste management  
fate_agg_ids <- fate_agg |> 
  filter(scenario %in% c("bau", "sb54")) |> 
  mutate(aes_id = case_when(scenario == "bau" & fate == "landfilled" ~ "BAU landfilled",
                            scenario == "sb54" & fate == "landfilled" ~ "SB 54 landfilled",
                            scenario == "bau" & fate == "recycled" ~ "BAU recycled",
                            scenario == "sb54" & fate == "recycled" ~ "SB 54 recycled",
                            fate == "incinerated" ~ "incinerated"))
annual_totals <- fate_agg |> 
  filter(scenario %in% c("bau", "sb54")) |>
  group_by(year, scenario) |> 
  summarize(annual_waste_mt = sum(waste_generation_mt)) |> 
  ungroup()

fate_levs <- c("BAU total", "SB 54 total", "BAU landfilled", "SB 54 landfilled", "BAU recycled", "SB 54 recycled", "Incinerated")

bau_v_sb54_fate <- ggplot() + 
  geom_line(data = annual_totals |> 
              mutate(aes_id = ifelse(scenario == "bau", "BAU total", "SB 54 total"),
                     aes_id = factor(aes_id, levels = fate_levs)),
            aes(x=year, y=annual_waste_mt, linetype=aes_id, group=aes_id),
            linewidth = 0.8,
            color = total_color) +
  geom_line(data = fate_agg_ids |> 
              mutate(aes_id = factor(aes_id, levels = fate_levs)) |> 
              filter(fate == "landfilled"), 
            aes(x=year, y=waste_generation_mt, linetype=aes_id, group = aes_id),
            color = wm_pal[1],
            linewidth = 0.8) + 
  geom_line(data = fate_agg_ids |> 
              mutate(aes_id = factor(aes_id, levels = fate_levs)) |> 
              filter(fate == "recycled"), 
            aes(x=year, y=waste_generation_mt, linetype=aes_id, group = aes_id),
            color = wm_pal[2],
            linewidth = 0.8) + 
  geom_vline(aes(xintercept = 2032),
             linetype = "dotted", color='gray30') + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,15),
                     breaks = seq(0,15,3)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(2025,2050),
                     breaks = seq(2025,2050,5)) + 
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  guides(linetype = guide_legend(override.aes=list(linewidth=0.5))) +  
  labs(x="",
       y="Plastic (Mt)",
       linetype = "") + 
  plot_theme

ggsave(filename = file.path(here::here('figures/bau_v_sb54_waste_management.eps')),
       plot = bau_v_sb54_fate,
       width = w, height = h, units = "in")
