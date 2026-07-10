# Calculate avoided virgin plastic production
# 
# Inputs: consum_total_byo, rc_perc_byo
##### Static placeholders delete later: consum_total_byo_54, rc_perc_byo_54
##### Recycled Content Mandate in SB 54 is 0% so no avoided virgin plastic due to RC 
# Output: avoid_virgin

# BAU Consumption stand-in long form, not an input here 
library(tidyr)
library(dplyr)

consum_total_bau <- read.csv("data/static/consum_total_bau.csv") # saved tidy long form

# Filler df for output of consum_total_byo

consum_total_byo_54 <- read.csv("data/static/consum_total_byo_54.csv") 

consum_total_byo_54_clean <-  pivot_longer(
  consum_total_byo_54,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_byo"
)

write.csv(consum_total_byo_54_clean, "data/static/consum_total_byo_54_clean.csv", row.names = FALSE)

# Filler df for output of rc_perc_byo
rc_perc_byo_54 <- read.csv("data/static/rc_perc_byo_54.csv")

rc_perc_byo_54_clean <-pivot_longer(
  rc_perc_byo_54,
cols = -year,          # everything except year
names_to = "sector",
values_to = "mt_plastic_rc"
)

write.csv(rc_perc_byo_54_clean, "data/static/rc_perc_byo_54_clean.csv", row.names = FALSE)

# calc_avoid_virgin hard code
avoid_virign <- consum_total_byo_54_clean %>%
  left_join(rc_perc_byo_54_clean, by = c("year", "sector")) %>%
  mutate(mt_plastic_virgin = mt_plastic_byo - mt_plastic_rc) %>%
  select(year, sector, mt_plastic_virgin)

# calc_avoid_virgin function 