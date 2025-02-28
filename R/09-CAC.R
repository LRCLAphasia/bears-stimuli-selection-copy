# These are the packages needed
library(here)
library(tidyverse)
library(tidytext)
library(readxl)
library(fuzzyjoin)
library(anticlust) # use dev version!
library(patchwork)

# load files from shiny app
for(i in list.files(here::here("shiny", "R"), full.names = TRUE)){source(i)}

# Hey! this is all available in the shiny app. You can run it by doing
#source(here::here("shiny", "app.R"))
# here()
#shinyApp(ui, server)

# or you can just navigate to shiny/app.R and hitting run app
# but if you don't want to use the shiny app, here's how to do it line by line:

# run the select_stimuli() function
# There are additional arguments - look at the script if you need to change them

select_stimuli(
  participant_theta = 55,
  participant_id = "a",
  shiny = FALSE,
  seed = 42,
  total_tx_items = 180
  #min_naming_agreement = 70,
  #min_discourse_salience = 30,
 # min_words_per_discourse_item = 4
)


thetas <- seq(35, 60, 1)
select_stimuli_dat = lst()
    for(i in seq_along(thetas)){
      tmp = tryCatch({
        select_stimuli(
          participant_theta = thetas[i],
          participant_id = paste0(thetas[i]),
          shiny = FALSE,
          seed = 42,
          total_tx_items = 180,
          min_naming_agreement = 70,
          min_discourse_salience = 30,
          min_words_per_discourse_item = 4
        )[["dat"]]
      }, error = function(e) NULL)
    
    if(is_tibble(tmp)){
      select_stimuli_dat[[i]] = tmp
    }
    }

out_dat = bind_rows(select_stimuli_dat, .id = "theta")

select_stimuli_time = lst()
for(i in seq_along(thetas)){
  tmp = tryCatch({
    select_stimuli(
      participant_theta = thetas[i],
      participant_id = paste0(thetas[i]),
      shiny = FALSE,
      seed = 42,
      total_tx_items = 180,
      min_naming_agreement = 70,
      min_discourse_salience = 30,
      min_words_per_discourse_item = 4
    )[["time"]]
  }, error = function(e) NULL)
  
  if(is_tibble(tmp)){
    select_stimuli_time[[i]] = tmp |> mutate(participant_id = paste0(thetas[i]))
  }
}

out_time = bind_rows(select_stimuli_time, .id = "theta")

summary(out_dat)

dat_summary = out_dat |> 
  mutate(across(where(is.character), as.factor)) |> 
  group_by(participant_id, condition, in_discourse, tx) |> 
  summarize(diff = mean(item_difficulty),
            diff_sd = sd(item_difficulty),
            agree = mean(agreement),
            agree_sd = sd(agreement),
            cl = mean(core_lex_percent, na.rm = TRUE),
            cl_sd = sd(core_lex_percent, na.rm = TRUE),
            prob = mean(p_correct),
            prob_sd = sd(p_correct)
            ) |> 
  rename(theta = participant_id)

dat_summary |> 
  ggplot(aes(x = theta, y = diff, color = in_discourse, shape = tx)) +
  geom_point()

# write_csv(out_dat, here("R", "cac", "out_dat.csv"))
# write_csv(out_time, here("R", "cac", "out_time.csv"))

