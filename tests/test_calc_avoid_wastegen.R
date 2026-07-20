
wastegen_byo_clean <- wastegen_byo |> 
  filter(sector != "all_sec") |> 
  arrange(desc(year), sector)

wastegen_bau_clean <- wastegen_bau |> 
  arrange(desc(year), sector)


avoid_wastegen <- wastegen_bau_clean |> 
  inner_join(
    wastegen_byo_clean,
    by = c("year", "sector"),
    suffix = c("_bau", "_byo")) |> 

  mutate(mt_avoid_wastegen = (mt_plastic_wastegen_bau - mt_plastic_wastegen_byo)) |> 
  select("year", "sector", "mt_avoid_wastegen")

sum(avoid_wastegen$mt_avoid_wastegen)
