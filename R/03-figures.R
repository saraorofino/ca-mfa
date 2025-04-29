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
results_file <- file.path(here::here("data/output/CA Plastic MFA Model v7-SO.xlsx"))

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

# Color palette 
lisa <- paletteer::paletteer_d("lisa::SandroBotticelli")
fish <- paletteer::paletteer_d("fishualize::Sardinella_brasiliensis")
arches <- paletteer::paletteer_d("nationalparkcolors::Arches")
crater <- paletteer::paletteer_d("nationalparkcolors::CraterLake")

sector_pal <- c(crater[c(3,6)], arches[5], fish[3], fish[4], fish[5], lisa[1], fish[2], lisa[2], arches[6], fish[1])

# Figures-----------------------------------
# Stat: by 2050 total plastic consumption and % packaging 
proj2050 <- consumption_sector |> 
  filter(scenario == "BAU" & year == 2050) |> 
  mutate(prop = plastic_consumption_mt / annual_consumption_mt)

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

ggsave(filename = file.path(here::here("figures/sector_breakdown.eps")),
       plot = sector_plot,
       width = 8, height = 6)

# Multi-panel MFA timeseries results BAU conditions by sector 
## Consumption
bau_consumption_timeseries <- consumption_sector |> 
  filter(scenario == "BAU") |> 
  dplyr::mutate(plastic_sector = factor(plastic_sector, levels = sector_levs)) |> 
  ggplot() + 
  geom_line(aes(x=year, y=plastic_consumption_mt, color=plastic_sector, group=plastic_sector),
            linewidth = 1.2) + 
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
  guides(color = guide_legend(nrow=2)) + 
  theme_bw() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        plot.margin = margin(t=10,r=30,b=1,l=3))

ggsave(filename = file.path(here::here('figures/bau_consumption.eps')),
       plot = bau_consumption_timeseries,
       width = 10, height = 6)

## Waste generation 
bau_waste_timeseries <- waste_sector |> 
  filter(scenario == "BAU") |> 
  dplyr::mutate(plastic_sector = factor(plastic_sector, levels = sector_levs)) |> 
  ggplot() + 
  geom_line(aes(x=year, y=plastic_waste_mt, 
                color=plastic_sector, group = plastic_sector),
            linewidth = 1.2) + 
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
  guides(color = guide_legend(nrow=2)) + 
  theme_bw() + 
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        plot.margin = margin(t=10,r=30,b=1,l=3)) 

ggsave(filename = file.path(here::here('figures/bau_waste_generation.eps')),
       plot = bau_waste_timeseries,
       width = 10, height = 6)


# BAU waste management
bau_fate_timeseries <- ggplot() + 
  geom_line(data = bau_fate, 
            aes(x=year, y=waste_generation_mt, color=fate, group = fate),
            linewidth = 1.2) + 
  scale_color_manual(values = c("#6C3428", "#117554", "firebrick"),
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
  theme_bw() +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        plot.margin = margin(t=10,r=30,b=1,l=3))

ggsave(filename = file.path(here::here('figures/bau_waste_management.eps')),
       plot = bau_fate_timeseries,
       width = 10, height = 6)
  

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
            color = 'black',
            linewidth = 1.2) + 
  # Packaging consumption under BAU and SB 54
  geom_line(data = packaging_consumption |> 
              mutate(scenario = factor(scenario, levels=scenario_levs)),
            aes(x=year, y=plastic_consumption_mt, linetype = scenario, group=scenario),
            color = "#BE9C9DFF",
            linewidth = 1.2) + 
  # All other sectors (consumption is the same in both scenarios so only plot one)
  geom_line(data = consumption_sector |> 
              filter(scenario == "BAU" & plastic_sector != "Packaging"),
            aes(x=year, y=plastic_consumption_mt, group=plastic_sector),
            color = "gray70",
            linewidth = 0.8) + 
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
       subtitle = "Consumption",
       linetype = "") + 
  guides(linetype = guide_legend(override.aes=list(linewidth=0.5))) + 
  theme_bw() + 
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "none",
        plot.subtitle = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        plot.margin = margin(t=10,r=30,b=1,l=3))

## Waste generation 
packaging_waste <- waste_sector |> 
  dplyr::filter(plastic_sector == "Packaging")

bau_v_sb54_waste <- ggplot() + 
  # Aggregated total waste under BAU and SB54
  geom_line(data = waste_agg |> 
              filter(scenario %in% c("bau", "sb54")) |> 
              mutate(scenario = factor(scenario, levels=scenario_levs)),
            aes(x=year, y=waste_generation_mt, linetype=scenario, group=scenario),
            color = 'black',
            linewidth = 1.2) + 
  # Packaging waste under BAU and SB 54
  geom_line(data = packaging_waste |> 
              mutate(scenario = factor(scenario, levels=scenario_levs)),
            aes(x=year, y=plastic_waste_mt, linetype = scenario, group=scenario),
            color = "#BE9C9DFF",
            linewidth = 1.2) + 
  # All other sectors (waste is the same in both scenarios so only plot one)
  geom_line(data = waste_sector |> 
              filter(scenario == "BAU" & plastic_sector != "Packaging"),
            aes(x=year, y=plastic_waste_mt, group=plastic_sector),
            color = "gray70",
            linewidth = 0.8) + 
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
       subtitle = "Waste generation",
       linetype = "") + 
  theme_bw() + 
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        plot.subtitle = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        plot.margin = margin(t=10,r=30,b=1,l=3))

## Combine consumption/waste with shared legend 
bau_v_sb54_combined <- bau_v_sb54_consumption + labs(tag="A") + bau_v_sb54_waste + labs(tag="B") + 
  plot_layout(ncol=1)

ggsave(filename = file.path(here::here('figures/combined_bau_v_sb54_results.eps')),
       plot = bau_v_sb54_combined,
       width = 10, height = 8)

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
            linewidth = 1.2) +
  geom_line(data = fate_agg_ids |> 
              mutate(aes_id = factor(aes_id, levels = fate_levs)) |> 
              filter(fate == "landfilled"), 
            aes(x=year, y=waste_generation_mt, linetype=aes_id, group = aes_id),
            color = "#6C3428",
            linewidth = 1.2) + 
  geom_line(data = fate_agg_ids |> 
              mutate(aes_id = factor(aes_id, levels = fate_levs)) |> 
              filter(fate == "recycled"), 
            aes(x=year, y=waste_generation_mt, linetype=aes_id, group = aes_id),
            color = "#117554",
            linewidth = 1.2) + 
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
       #subtitle = "Waste Management",
       linetype = "") + 
  theme_bw() + 
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        #plot.subtitle = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        plot.margin = margin(t=10,r=30,b=1,l=3))

ggsave(filename = file.path(here::here('figures/bau_v_sb54_fate.eps')),
       plot = bau_v_sb54_fate,
       width = 10, height = 4)

## Collection for recycling rates
bau_v_sb54_coll_rate <- collection_rates |> 
  ggplot() +
  geom_line(aes(x=year, y=collection_rate, linetype=scenario, group=scenario),
            color = "black",
            linewidth=1.2) + 
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.3),
                     breaks = seq(0,0.3,0.1),
                     labels = scales::percent) + 
  scale_x_continuous(expand = c(0,0),
                     limits = c(2012,2050),
                     breaks = seq(2010,2050,5)) + 
  geom_vline(aes(xintercept = 2032),
             linetype = "dotted", color='gray30') + 
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x="",
       y="Percent collected for recycling",
       subtitle = "Collection Rate",
       linetype = "") + 
  theme_bw() +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        plot.subtitle = element_text(size=12),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14),
        plot.margin = margin(t=10,r=30,b=1,l=3))

ggsave(filename = file.path(here::here('figures/bau_v_sb54_recycling_collection_rates.png')),
       plot = bau_v_sb54_coll_rate,
       width = 10, height = 4)

## Combine fate and collection for recycling
bau_v_sb54_management_combined <- bau_v_sb54_fate + labs(tag="A") + bau_v_sb54_coll_rate + labs(tag="B") + 
  plot_layout(ncol=1)

ggsave(filename = file.path(here::here('figures/combined_bau_v_sb54_management_results.png')),
       plot = bau_v_sb54_management_combined,
       width = 10, height = 8)
