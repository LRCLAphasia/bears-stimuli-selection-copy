
# This file joins naming and discourse items. 
# It is mostly automated, BUT there is one place noted below that may benefit
# from some hand checking and tweaking. 

library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(textstem)
library(textclean)
library(udpipe)
library(fuzzyjoin)

# these are the lemmas from discourse
all <- read.csv(here("output", paste0("2023-08-14", "_nounCounts.csv")))

# Blacklist specific words within specific discourse items d/t those words being in written form
all <- all |>  
  subset(!(lemma == "lion" & stimuli == "ancient_rome") & !(lemma == "slide" & stimuli == "make_a_splash") 
         & !(lemma == "book" & stimuli == "picture_this") & !(lemma == "elephant" & stimuli == "airport") 
         & !(lemma == "helicopter" & stimuli == "airport") & !(lemma == "flower" & stimuli == "campsite") 
         & !(lemma == "fish" & stimuli == "deep_sea_divers") & !(lemma == "tent" & stimuli == "campsite") 
         & !(lemma == "penguin" & stimuli == "birthday_party_fun") & !(lemma == "penguin" & stimuli == "escape_from_the_zoo") 
         & !(lemma == "penguin" & stimuli == "make_a_splash") & !(lemma == "sea" & stimuli == "deep_sea_divers") 
         & !(lemma == "train" & stimuli == "wild_west") & !(lemma == "watch" & stimuli == "airport") 
         & !(lemma == "penguin" & stimuli == "by_the_lake") & !(lemma == "icecreamcone" & stimuli == "escape_from_the_zoo") 
         & !(lemma == "fountain" & stimuli == "flowerman"))


# only keep the ones with sufficient salience
df_30 = all |> 
  mutate(n = as.integer(n), percent = as.double(percent)) |> 
  filter(
    percent >= 25) |> 
  select(stimuli, lemma_hc = lemma, n, percent)

#multiwords from 02-multiword-words.R
multi <- read.csv(here("output", "multiword.csv")) |> 
  filter(percent_found >= .25) |> 
  select(stimuli, lemma_hc = modal_with_spaces, n = total_found, percent = percent_found) |> 
  mutate(percent = percent*100, lemma_hc = str_remove(lemma_hc, " "))

multi2 <- read.csv(here("output", "exactjoins_Merge_Phase3Norming_v3.csv")) |> 
  separate(stimuli,sep = ", ", into = paste0("stimuli", c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))) |> 
  pivot_longer(cols = stimuli1:stimuli10) |> 
  drop_na(value) |> 
  select(stimuli = value, lemma_hc = OriginalLemma, phase3_name = Phase3_Name)

df_30 = df_30 |> 
  left_join(multi2, by = c("stimuli", "lemma_hc"))  |> 
  mutate(lemma_hc = ifelse(is.na(phase3_name), lemma_hc, phase3_name)) |> 
  select(-phase3_name)

# combine to get all nouns
df_30 <- bind_rows(multi, df_30) |> rename(lemma = lemma_hc) 

# this is the naming database file with all the potential naming items
# limit to agreement has to be better than 70. 
naming <- read_csv(here("data", "words 2024-08-29_numeric.csv"), col_select = 1:7) |> 
  select(lemma = modal, source, agreement, target = picture) |> 
  distinct() |> 
  filter(agreement > 70) |> 
  group_by(lemma) |> 
  mutate(source = paste(source, collapse = ", ")) |> 
  distinct()

# This joins the naming and discourse items with a tiny bit of flexibility
# for plural/ or other small differences like tshirt vs. shirt.
stringdist_join(
  naming, df_30,
  by = "lemma",
  mode = "left",
  ignore_case = TRUE, 
  method = "jw", 
  max_dist = 0.08, 
  distance_col = "dist"
) |> arrange(desc(dist)) |> 
  select(source, agreement, stimuli, n, percent, lemma_naming = lemma.x, lemma_dis = lemma.y, dist) -> fuzz_join

# 131 words that are close but < 0.07 as of 2/1/2024
# re-run this after changing the words or vtt files to see if you get a number close to 131
# If you do, then you're good to go. Otherwise, you should probably re-check these by hand.
sum(fuzz_join$dist > 0, na.rm = TRUE)
# [1] 208


# this saves the raw output without checking
# If you want to re-recheck the matches, then save this file, 
# open it in excel, and check all of the columns that have a dist > 0
# (inexact matches) and see if there are any that really are a match even if 
# the strings are slightly different. Put a 1 if you consider it a match. leave
# it as NA otherwise. Then saved that file as join_checked.csv and run
# the code below to read it back in and join it to the fuzz_join data. 
# Then re-save the fuzz_join dataframe after the left_join below.
# write.csv(fuzz_join |> mutate(match = NA) |> filter(dist>0), here("output", "join_checked_2-2025.csv"), row.names = FALSE)

## automate the hand-fixed...
## join_checked is a file that I fixed by hand and reuploaded
## so its still hand-fixed, but I'm just re-using the same hand-fixes if
## this script is re-run
fixes <- read.csv(here("output", "join_checked_2-2025.csv")) |> 
  filter(match == 1, dist > 0) |> 
  select(lemma_naming, lemma_dis, match) |> 
  distinct()

fuzz_join <- fuzz_join |> left_join(fixes, by = c("lemma_naming" = "lemma_naming", "lemma_dis" = "lemma_dis"))

write.csv(fuzz_join, here::here("output", "join_checked_automated_02-2025.csv"), row.names = FALSE)
write.csv(fuzz_join, here::here("data", "join_checked_automated_02-2025.csv"), row.names = FALSE)
write.csv(fuzz_join, here::here("shiny", "data", "join_checked_automated_02-2025.csv"), row.names = FALSE)




#### Needed these for generating targets for second round of stimuli norming...
#### not used for the processing stream
#### 
df_30$id = seq(1, nrow(df_30), 1)

exact_join = left_join(df_30,  naming, by = "lemma")

with_stim = exact_join |>
  select(stimuli, lemma, source, agreement, target) |>
  group_by(lemma) |>
  summarize(stimuli = paste(stimuli, collapse = ", "))

exact_join2 = left_join(df_30 |> distinct(lemma), naming, by = "lemma") |>
  left_join(with_stim, by = "lemma")

# write.csv(exact_join2, here("data", "discourse_lemmas_matched_to_naming.csv"))
# write.csv(exact_join, here("data", "found_in_discourse_exact.csv"))



