# reading in files (will delete, will take place in pre-processing)

consum_bau <- read_csv(here('data',"consum_bau_clean.csv"))


# consum_sr ---------------------------------------------------------------

#produced the same values as excel model
consum_sr <- calc_consum_sr(consum_bau, 2032, 0.25, 'pack', 2023, 2024) 

# produced slightly different, see meeting notes 07/21
avoid_prod <- calc_avoid_prod(consum_bau, consum_sr, summary = TRUE) #note: currently consum_bau does not have all_sec (needs to be done in preproccessing)

# produced the same values as excel model
rc_perc <- calc_rc_perc(consum_sr, 0.4, 2032, 2024, 'pack', 0)


# produced the same values as excel model
scrap_input <- calc_scrap_input(rc_perc, 0.5)

# need to double check
avoid_virgin <- calc_avoid_virgin(consum_bau, consum_sr, rc_perc, 0.5, summary= TRUE)



# writing outputs as CSV's to confirm with spreadsheet --------------------


write.csv(consum_sr, here('data', 'consum_sr.csv'))
write.csv(rc_perc, here('data', 'rc_perc.csv'))
write.csv(avoid_prod, here('data', 'avoid_prod.csv'))
write.csv(scrap_input, here('data','scrap_input.csv'))
write.csv(avoid_virgin, here('data', 'avoid_virgin.csv'))

