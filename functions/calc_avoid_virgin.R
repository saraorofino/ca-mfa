#' @title Calculate avoided virgin plastic production
#'
#' @param consum_total_byo, rc_perc_byo
#' Static placeholders delete later: consum_total_byo_54, rc_perc_byo_54 Recycled Content Mandate in SB 54 is 0% so no avoided virgin plastic due to RC 
#' @output avoid_virgin

# load data ---------------------------------------------------------------
# Delete in function version 
library(tidyr)
library(dplyr)

consum_total_bau <- read.csv("data/static/consum_total_bau.csv") # saved tidy long form

consum_total_byo_54 <- read.csv("data/static/consum_total_byo_54.csv") # convert to long form 

rc_perc_byo_54 <- read.csv("data/static/rc_perc_byo_54.csv")

# clean filler df --------------------------------------------------------
# consum_byo clean long format filler data frame clean 
consum_total_byo_54_clean <-  pivot_longer( # change to 
  consum_total_byo_54,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_byo"
)

write.csv(consum_total_byo_54_clean, "data/static/consum_total_byo_54_clean.csv", row.names = FALSE)

# rc_perc_byo clean long format filler data frame for output

rc_perc_byo_54_clean <-pivot_longer(
  rc_perc_byo_54,
cols = -year,          # everything except year
names_to = "sector",
values_to = "mt_plastic_rc"
)

write.csv(rc_perc_byo_54_clean, "data/static/rc_perc_byo_54_clean.csv", row.names = FALSE)

# hardcode calc avoid virgin ----------------------------------------------

#avoid_virgin <- consum_total_byo_54_clean %>%
 # left_join(rc_perc_byo_54_clean, by = c("year", "sector")) %>%
 # mutate(mt_plastic_virgin = mt_plastic_byo - mt_plastic_rc) %>%
 # select(year, sector, mt_plastic_virgin)


# test function  ----------------------------------------------------------

avoid_virgin <- avoid_virgin_function(consum_total_byo_54_clean, rc_perc_byo_54_clean) 