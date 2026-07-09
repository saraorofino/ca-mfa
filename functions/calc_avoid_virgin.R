# Calculate avoided virgin plastic production
# 
# Inputs: consum_total_byo, rc_perc_byo
# Output: avoid_virgin

# Play around with long form df 
library(tidyr)

consum_bau <- pivot_longer(
  consum_bau,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic"
)

write.csv(consum_bau, "data/static/consum_bau.csv", row.names = FALSE)

library(dplyr)

avoid_virign  <- consum_bau %>%
  left_join(rc_perc_byo, by = c("year", "sector"), suffix = c("_total", "_rc")) %>%
  mutate(mt_plastic_virgin = mt_plastic_total - mt_plastic_rc) %>%
  select(year, sector, mt_plastic_virgin)