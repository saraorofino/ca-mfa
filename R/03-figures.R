library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(ggplot2)
library(paletteer)
library(showtext) # for Barlow font

# Figure elements -----------------------------------
# Font
font_add_google("Barlow", family = "barlow")

# Plot theme 
black40 = "#666666"
black100 = "#000000"
plot_theme <- theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color=black40, size=0.25),
        legend.position = "bottom",
        legend.text = element_text(color=black100, family="barlow", size=7.5),
        axis.text = element_text(color=black100, family="barlow", size=7.5),
        axis.title = element_text(color=black100, family="barlow", size=7.5),
        axis.ticks.x = element_line(color=black40, size=0.25))

# Color palettes (from Danielle)
## 11 colors for the different sectors 
sector_pal <- c("#05631c", "#4aa842", "#78a12e", "#c2cc24", "#fce56e", "#b0b087",
                "#e3b024", "#e56b2b", "#73401c", "#133d8d", "#d1bfab")

## Landfilled, Recycled, Incinerated 
fate_pal <- c("#FFB000", "#0057A3", "#DDCC77")

## Total 
total_color <- "#44AA99"

# Data -----------------------------------
results_file <- file.path(here::here("data/output/CA Plastic MFA Model v10.xlsx"))

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

policy_results <- results_raw[,c(16,19:20)]
names(policy_results) <- c("scenario", "policy_metric", "cumulative_avoided_production_mt")
policy_results_clean <- policy_results |> 
  fill(scenario, .direction=c("down")) |> 
  filter(!is.na(scenario) & !is.na(policy_metric))

## GHG results 
ghg <- read_xlsx(path = results_file,
                 sheet ="GHG layer") |> 
  row_to_names(1) |> 
  clean_names() |> 
  pivot_longer(cols = c("bau":"byo_3"),
               names_to = "scenario",
               values_to = "co2e_mt") |> 
  dplyr::mutate(metric = case_when(scenario %in% c("bau", "sb54", "byo") ~ "production",
                                    scenario %in% c("bau_2", "sb54_2", "byo_2") ~ "disposal",
                                    scenario %in% c("bau_3", "sb54_3", "byo_3") ~ "avoided_production"),
                scenario = case_when(str_detect(scenario, "bau") ~ "BAU",
                                     str_detect(scenario, "sb54") ~ "SB 54",
                                     str_detect(scenario, "byo") ~ "BYO"),
                co2e_mt = as.numeric(co2e_mt)) |> 
  dplyr::select(year=na, scenario, metric, co2e_mt) |> 
  filter(!is.na(year))

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
                                    plastic_sector == 'b_c' ~ "Buiding/Construction",
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
                                    plastic_sector == 'b_c' ~ "Buiding/Construction",
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

# Figures-----------------------------------
# Sector breakdown (for years we actually modeled 2012-2020) 
avg_prop_sector <- consumption_sector |>
  filter(scenario == "BAU" & year >= 2012 & year <= 2020) |> 
  mutate(percent = plastic_consumption_mt / annual_consumption_mt * 100) |> 
  group_by(plastic_sector) |> 
  summarize(plastic_consumption_mt = mean(plastic_consumption_mt),
            annual_consumption_mt = mean(annual_consumption_mt)) |> 
  ungroup() |> 
  dplyr::mutate(percent = (plastic_consumption_mt/annual_consumption_mt) * 100,
                rounded_tons = round(plastic_consumption_mt, 1)) |>
  arrange(-percent)

# Sector levels and colors have to be slightly adjusted here compared to the BAU consumption graph 
sector_levs <- avg_prop_sector$plastic_sector
pal <- c(sector_pal[1:9], sector_pal[11], sector_pal[10])

sector_plot <- ggplot(avg_prop_sector,
                      aes(x=percent, y=factor(plastic_sector, levels=sector_levs))) + 
  geom_col(aes(fill=factor(plastic_sector, levels=sector_levs))) +
  geom_text(aes(label = ifelse(plastic_sector == "Agriculture",
                               "0.04M tons",
                               paste0(rounded_tons, "M mt/year"))),
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
        panel.grid.major.x = element_line(color=black40, linewidth = 0.25),
        axis.text= element_text(color=black100, family="barlow", size=7.5),
        axis.title = element_text(color=black100, family="barlow", size=7.5),
        legend.position = "none") 

ggsave(filename = file.path(here::here("figures/sector_breakdown.eps")),
       plot = sector_plot,
       width = 7, height = 2.5)

# Multi-panel MFA timeseries results BAU conditions by sector 
consumption_levs <- c(sector_levs[1:9], sector_levs[11], sector_levs[10]) # match Danielle's preference
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
       y="Plastic (millions of metric tons)",
       #subtitle = "Consumption",
       color = "") + 
  guides(color = guide_legend(nrow=3)) + 
  plot_theme + 
  theme(plot.margin = margin(t=10,r=10,b=1,l=1))

ggsave(filename = file.path(here::here('figures/bau_consumption.eps')),
       plot = bau_consumption_timeseries,
       width = 7, height = 3.5)

## Waste generation 
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
                     limits = c(2012,2050),
                     breaks = seq(2010,2050,5)) + 
  scale_color_manual(values = sector_pal) + 
  labs(x="",
       y="Plastic (millions of metric tons)",
       #subtitle = "Waste Generation",
       color = "") + 
  guides(color = guide_legend(nrow=3)) + 
  plot_theme + 
  theme(plot.margin = margin(t=10,r=10,b=1,l=1)) 

ggsave(filename = file.path(here::here('figures/bau_waste_generation.eps')),
       plot = bau_waste_timeseries,
       width = 7, height = 3.5)


# BAU waste management
bau_fate_timeseries <- ggplot() + 
  geom_line(data = bau_fate, 
            aes(x=year, y=waste_generation_mt, color=fate, group = fate),
            linewidth = 0.8) + 
  scale_color_manual(values = fate_pal,
                     labels = c("Landfilled", "Recycled", "Incinerated")) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,12),
                     breaks = seq(0,12,3)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(2012,2050),
                     breaks = seq(2010,2050,5)) + 
  labs(x="",
       y="Plastic (millions of metric tons)",
       #subtitle = "Waste management",
       color = "") + 
  plot_theme + 
  theme(plot.margin = margin(t=10,r=10,b=1,l=1))

ggsave(filename = file.path(here::here('figures/bau_waste_management.eps')),
       plot = bau_fate_timeseries,
       width = 7, height = 3)
  

# Multi-panel timeseries of BAU vs. SB54
## Consumption 
packaging_consumption <- consumption_sector |> 
  dplyr::filter(plastic_sector == "Packaging")

scenario_levs <- c("bau", "sb54", "BAU", "SB 54")
  
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
            color = "gray70",
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
                     limits = c(2012,2050),
                     breaks = seq(2010,2050,5)) + 
  labs(x="",
       y="Plastic (millions of metric tons)",
       linetype = "") + 
  guides(linetype = guide_legend(override.aes=list(linewidth=0.5))) + 
  plot_theme + 
  theme(plot.margin = margin(t=10,r=10,b=1,l=1))

ggsave(filename = file.path(here::here('figures/bau_v_sb54_consumption.eps')),
       plot = bau_v_sb54_consumption,
       width = 7, height = 3)

## Waste generation 
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
            color = "gray70",
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
                     limits = c(2012,2050),
                     breaks = seq(2010,2050,5)) + 
  guides(linetype = guide_legend(override.aes=list(linewidth=0.5))) + 
  labs(x="",
       y="Plastic (millions of metric tons)",
       linetype = "") + 
  plot_theme + 
  theme(plot.margin = margin(t=10,r=10,b=1,l=1))

ggsave(filename = file.path(here::here('figures/bau_v_sb54_waste_generation.eps')),
       plot = bau_v_sb54_waste,
       width = 7, height = 3)

## Waste management (fate) 
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
            color = fate_pal[1],
            linewidth = 0.8) + 
  geom_line(data = fate_agg_ids |> 
              mutate(aes_id = factor(aes_id, levels = fate_levs)) |> 
              filter(fate == "recycled"), 
            aes(x=year, y=waste_generation_mt, linetype=aes_id, group = aes_id),
            color = fate_pal[2],
            linewidth = 0.8) + 
  geom_vline(aes(xintercept = 2032),
             linetype = "dotted", color='gray30') + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,15),
                     breaks = seq(0,15,3)) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(2012,2050),
                     breaks = seq(2010,2050,5)) + 
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed", "solid", "dashed")) +
  guides(linetype = guide_legend(nrow=1, override.aes=list(linewidth=0.5))) +  
  labs(x="",
       y="Plastic (millions of metric tons)",
       linetype = "") + 
  plot_theme + 
  theme(plot.margin = margin(t=10,r=10,b=1,l=1))

ggsave(filename = file.path(here::here('figures/bau_v_sb54_fate.eps')),
       plot = bau_v_sb54_fate,
       width = 7, height = 3)
