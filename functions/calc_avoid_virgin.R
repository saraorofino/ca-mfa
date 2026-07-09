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