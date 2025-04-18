library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(paletteer)
library(cowplot)
library(patchwork)

# Data
## NOTES: I don't think Roland is actually using the right values from this power series in the MFA model
## it's using the Leontief values not the sum of the power series & uses a static % contribution for each plastic sector instead of using the actual values from each plastic sector 

## Aggregated policy results
results_raw <- read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA Model v7.xlsx")),
                    sheet ="Inputs & Results")

consumption_agg <- results_raw |> 
  row_to_names(16) |> 
  clean_names() |> 
  dplyr::select(year, bau, sb54, byo) |> 
  pivot_longer(cols = c("bau":"byo"),
               names_to = "scenario",
               values_to = "consumption_mt")

waste_agg <- results_raw |> 
  row_to_names(16) |> 
  clean_names() |> 
  dplyr::select(year, bau=bau_2, sb54=sb54_2, byo=byo_2) |> 
  pivot_longer(cols = c("bau":"byo"),
               names_to = "scenario",
               values_to = "waste_generation_mt")

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
              mutate(fate = "recycled"))

policy_results <- results_raw[,c(16,19:20)]
names(policy_results) <- c("scenario", "policy_metric", "cumulative_avoided_production_mt")
policy_results_clean <- policy_results |> 
  fill(scenario, .direction=c("down")) |> 
  filter(!is.na(scenario) & !is.na(policy_metric))

# Data by sector -----------------------------------
consumption_sector <- read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA Model v7.xlsx")),
                         sheet ="BAU Consum") |> 
  row_to_names(row=1) |> 
  clean_names() |> 
  dplyr::select(year, annual_consumption_mt = io, ag:other) |> 
  mutate(scenario = "BAU") |> 
  bind_rows(read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA Model v7.xlsx")),
                      sheet ="SB54 Consum") |> 
              row_to_names(row=2) |> 
              clean_names() |> 
              dplyr::select(year, annual_consumption_mt = in_mt_2, ag:other) |> 
              mutate(scenario = "SB54")) |> 
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

waste_sector <- read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA Model v7.xlsx")),
                   sheet ="BAU Waste Gen") |> 
  row_to_names(row=1) |> 
  clean_names() |> 
  dplyr::select(year, annual_waste_mt = waste, ag:other) |> 
  mutate(scenario = "BAU") |> 
  bind_rows(read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA Model v7.xlsx")),
                      sheet ="SB54 Waste Gen") |> 
              row_to_names(row=1) |> 
              clean_names() |> 
              dplyr::select(year, annual_waste_mt = sb54_2, ag:other) |> 
              mutate(scenario = "SB54") |> 
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

# Note: not sure what the right column is for recycling %... they don't seem to add up to 100 in any given year? 
## This also isn't really sector specific? can probably delete this and just fate_agg above 
fate_agg <- read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA Model v7.xlsx")),
                      sheet ="BAU Waste Man") |> 
  row_to_names(row=2) |> 
  clean_names() |> 
  dplyr::select(year, amt_recycled = in_kt, amt_incinerated = in_kt_2, 
                amt_landfilled = in_kt_3, total_amt = in_kt_5) |> 
  mutate(across(.cols=c("year", "amt_recycled", "amt_incinerated", "amt_landfilled", "total_amt"),
                .fns=function(x){as.numeric(x)})) |> 
  mutate(scenario = "BAU") |> 
  bind_rows(read_xlsx(path = file.path(here::here("data/output/CA Plastic MFA Model v7.xlsx")),
                      sheet ="SB54 Waste Man") |> 
              row_to_names(row=2) |> 
              clean_names() |> 
              dplyr::select(year, amt_recycled = in_kt, amt_incinerated = in_kt_2, 
                            amt_landfilled = in_kt_3) |> 
              mutate(across(.cols=c("year", "amt_recycled", "amt_incinerated", "amt_landfilled"),
                            .fns=function(x){as.numeric(x)})) |> 
              mutate(scenario = "SB54",
                     total_amt = amt_recycled + amt_incinerated + amt_landfilled)) |> 
  pivot_longer(cols = c("amt_recycled":"amt_landfilled"),
               names_to = "fate",
               values_to = "plastic_kt") |> 
  mutate(fate = case_when(fate == "amt_recycled" ~ "Recycled",
                          fate == "amt_incinerated" ~ "Incinerated",
                          fate == "amt_landfilled" ~ "Landfill"),
         # Millions of metric tons to match other graphs 
         annual_waste_mt = (total_amt*1000)/1000000,
         plastic_waste_mt = (plastic_kt*1000)/1000000) |> 
  dplyr::select(year, scenario, annual_waste_mt, fate, plastic_waste_mt)

# Color palette 
lisa <- paletteer::paletteer_d("lisa::SandroBotticelli")
fish <- paletteer::paletteer_d("fishualize::Sardinella_brasiliensis")
arches <- paletteer::paletteer_d("nationalparkcolors::Arches")
crater <- paletteer::paletteer_d("nationalparkcolors::CraterLake")

sector_pal <- c(crater[c(3,6)], arches[5], fish[3], fish[4], fish[5], lisa[1], fish[2], lisa[2], arches[6], fish[1])

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

sector_levs <- avg_prop_sector$plastic_sector

sector_plot <- ggplot(avg_prop_sector,
                      aes(x=percent, y=factor(plastic_sector, levels=sector_levs))) + 
  geom_col(aes(fill=factor(plastic_sector, levels=sector_levs))) +
  geom_text(aes(label = ifelse(plastic_sector == "Agriculture",
                               "0.04M tons",
                               paste0(rounded_tons, "M mt/year"))),
            position = position_nudge(x=3.9)) + 
  scale_fill_manual(values=sector_pal) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,50)) + 
  scale_y_discrete(expand = c(0,0),
                   limits = rev) + 
  labs(x="Percent of California annual plastic consumption 2012-2020",
       y="",
       fill = "Sector") + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none") 

ggsave(filename = file.path(here::here("figures/sector_breakdown.png")),
       plot = sector_plot,
       width = 8, height = 6)

# Multi-panel MFA timeseries results BAU conditions by sector 
## Consumption
total_consumption <- consumption_sector |> 
  filter(scenario == "BAU") |> 
  group_by(year) |> 
  summarize(plastic_consumption_mt = sum(plastic_consumption_mt)) |> 
  ungroup()

consumption_timeseries <- consumption_sector |> 
  filter(scenario == "BAU") |> 
  dplyr::mutate(plastic_sector = factor(plastic_sector, levels = c("Total", sector_levs)),
                color_id = "Total") |> 
  ggplot() + 
  geom_line(aes(x=year, y=annual_consumption_mt, color=color_id),
            linetype = "dashed") +
  geom_line(aes(x=year, y=plastic_consumption_mt, color=plastic_sector, group=plastic_sector),
            linewidth = 0.8) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1950,2050,10)) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,15),
                     breaks = seq(0,15,3)) + 
  scale_color_manual(values = c(sector_pal, "gray30")) + 
  labs(x="",
       y="Plastic (millions of metric tons)",
       subtitle = "Consumption",
       color = "Sector") + 
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank())

# ggsave(filename = file.path(here::here("figures/plastic_consumption_by_sector_timeseries.png")),
#        plot = consumption_timeseries,
#        width = 8, height = 6)

## Waste generation 
total_waste <- waste_sector |> 
  filter(scenario == "BAU") |> 
  group_by(year) |> 
  summarize(plastic_waste_mt = sum(plastic_waste_mt)) |> 
  ungroup()

waste_timeseries <- waste_sector |> 
  filter(scenario == "BAU") |> 
  dplyr::mutate(plastic_sector = factor(plastic_sector, levels = c("Total", sector_levs)),
                color_id = "Total") |> 
  ggplot() + 
  geom_line(aes(x=year, y=annual_waste_mt, color=color_id),
            linetype = "dashed") +
  geom_line(aes(x=year, y=plastic_waste_mt, 
                color=plastic_sector, group = plastic_sector),
            linewidth = 0.8) + 
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,15,3)) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1950,2050,10)) + 
  scale_color_manual(values = c(sector_pal, "gray30")) + 
  labs(x="",
       y="Plastic (millions of metric tons)",
       subtitle = "Waste Generation",
       color = "Sector") + 
  theme_bw() + 
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank())

# ggsave(filename = file.path(here::here("figures/waste_by_sector_timeseries.png")),
#        plot = waste_timeseries,
#        width = 8, height = 6)

## Combine A/B ones with the shared legend 
c_w_sectors <- consumption_timeseries + labs(tag="A") + waste_timeseries + labs(tag="B") + 
  plot_layout(ncol=1, guides = "collect") & theme(legend.position="bottom")

ggsave(filename = file.path(here::here('figures/combined_mfa_by_sector_bau_results.png')),
       plot = c_w_sectors,
       width = 10, height = 8)

## Fate (not sector specific though)
fate_timeseries <- fate |> 
  filter(scenario == "BAU") |> 
  dplyr::mutate(fate = factor(fate, levels = c("Landfill", "Recycled", "Incinerated")),
                color_id = "Total") |> 
  ggplot() + 
  geom_line(aes(x=year, y=annual_waste_mt, color=color_id),
            linetype = "dashed") +
  geom_line(aes(x=year, y=plastic_waste_mt, 
                color=fate, group = fate),
            linewidth = 0.8) + 
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,15,3)) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(1950,2050,10)) + 
  scale_color_manual(values = c("#6C3428", "#117554", "#7E1717", "gray30")) + 
  labs(x="",
       y="Plastic (millions of metric tons)",
       subtitle = "Waste Management",
       tag = "C",
       color = "Fate") + 
  theme_bw() + 
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "bottom")

# ggsave(filename = file.path(here::here("figures/fate_timeseries.png")),
#        plot = fate_timeseries,
#        width = 8, height = 6)


