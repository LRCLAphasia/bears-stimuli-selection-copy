
library(here)
library(tidyverse)
library(readxl)

new_words = read_excel(here::here("data", "words 2024-08-29.xlsx")) |> 
  mutate(agreement = ifelse(agreement < 2, agreement*100, agreement))

write_csv(new_words, here::here("data", "words 2024-08-29_numeric.csv"))
write_csv(new_words, here::here("output", "words 2024-08-29_numeric.csv"))
write_csv(new_words, here::here("shiny", "data", "words 2024-08-29_numeric.csv"))
