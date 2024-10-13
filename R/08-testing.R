p_cor = function(theta, b, discrimination = 0.182){
  exp(discrimination*(theta-b))/(1+exp(discrimination*(theta-b)))
}
p_cor = Vectorize(p_cor) 



naming <- suppressMessages(read_csv(here::here("data", "words 2024-08-16_numeric.csv"), col_types = cols())) |> 
  select(lemma = item, source, agreement, filename = picture) |> 
  group_by(lemma) |> slice_sample(n = 1) |> 
  distinct() |> 
  filter(agreement >= 75) |> 
  group_by(lemma) |> 
  mutate(source = paste(source, collapse = ", ")) |> 
  distinct()

item_params = read_csv(here::here("data", "AoA-phonemes-freq_combined_2024_08_16.csv")) |> 
  select(Word, LgSUBTLCD, Age_Of_Acquisition, NPhon)

fuzz_join = suppressMessages(read_csv(here::here("data", "join_checked_automated_08-2024.csv"),
                                      col_types = cols())) |> 
  filter(match == 1 | dist <0.001) |> 
  select(source:lemma_dis) |> 
  filter(percent >= 33, agreement >= 75) |> 
  select(source, lemma=lemma_naming, stimuli, percent, lemma_dis) |> 
  distinct()


diff = naming |> 
  full_join(item_params, by = c("lemma" = "Word")) |> 
  left_join(fuzz_join, by = join_by(lemma, source), relationship = "many-to-many") |> 
  mutate(difficulty = 39.47 + 1.08*NPhon - 2.01*LgSUBTLCD + 1.45*Age_Of_Acquisition)
