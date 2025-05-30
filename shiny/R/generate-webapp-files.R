

#' Data wrangling to generate app input file from selected stimuli
#'
#' @param df_selected_stimuli dataframe of selected stimuli
#' @param naming_only if naming_only (no disocourse), set to 1
#' @param c1 name of condition 1. "em", "am", or "eab"
#' @param c2 name of condition 2. "em", "am", or "eab"
#' @param c3 name of condition 3. "em", "am", or "eab"
#'
#' @return dataframe of app input
create_app_input_file <- function(df_selected_stimuli, naming_only = 0, c1, c2, c3, study = '180'){
  
  # condition assignments
  conditions <- tibble(
    condition_num = as.factor(c(1, 2, 3)),
    condition = c(c1, c2, c3)
  )
  
  # we only need these columns
  df <- df_selected_stimuli |> 
    mutate(condition = as.factor(condition)) |> 
    select(word, in_discourse, discourse_stimuli, condition_num = condition, tx, filename, participant_id) |> 
    left_join(conditions, join_by(condition_num))

# test
  # check for NA values
  # check for balanced conditions
  # check for 180 unique words
if(study == "180"){
  stopifnot(
    "unbalanced conditions or < 180 words present in selected stimuli. can't generate input files" = 
      all(df |> distinct(condition, word) |> count(condition) |> pull(n) == 60)
  )
} else {
  #print( df |> count(condition) |> arrange(n) |> pull(n)) # distinct(condition, word) |> 
  # stopifnot(
  #   "unbalanced conditions or < 180 words present in selected stimuli. can't generate input files" = 
  #     df |>  count(condition) |> arrange(n) |> pull(n) == c(60, 220, 220) # distinct(condition, word) |>
  # )
  print(df |>  count(condition) |> arrange(n))
}

  # TO DO...need to change these column names!!!
  if(study == "180"){ # change for study 2.         
    
    # why getting NA values?
    # the normal case where we have discoures and naming items
      if(naming_only != 1){

    # discourse item formatting
          df_discourse <- 
            df |> 
              filter(in_discourse == 1) |> 
              drop_na(discourse_stimuli) |> 
              distinct(participant_id, discourse_stimuli, condition) |> 
              mutate(value = 1, condition2 = condition, type = "discourse",) |> 
              pivot_wider(names_from = condition, values_from = value) |> 
              rename('eab-discourse-probes' = eab,
                     'em-discourse-probes' = em,
                     'am-discourse-probes' = am,
                     condition = condition2) |> 
              mutate(
                     'eab-naming-probes' = NA_real_,
                     'eab-treatment-probes' = NA_real_,
                     'em-naming-probes' = NA_real_,
                     'em-treatment-probes' = NA_real_,
                     'am-naming-probes' = NA_real_,
                     'am-treatment-probes' = NA_real_
                     ) |> 
               mutate(across(contains('probes'), ~replace_na(as.double(.), 0))) |> 
              select(
                participant_id,
                item = discourse_stimuli,
                type,
                condition,
                'eab-treatment' = 'eab-treatment-probes', 'eab-naming-probes', 'eab-discourse-probes',
                'em-treatment'  = 'em-treatment-probes', 'em-naming-probes', 'em-discourse-probes',
                'am-treatment'  = 'am-treatment-probes', 'am-naming-probes', 'am-discourse-probes'
              ) |> 
            mutate(discourse_stimuli = NA)
        }
      
        # naming item formatting
        df_naming <- 
          df |> 
            distinct(participant_id, word, discourse_stimuli, filename, condition, tx) |> 
            mutate(value = 1,
                   condition2 = condition,
                   type = "naming") |> 
            pivot_wider(names_from = condition, values_from = value) |> 
            rename('eab-naming-probes' = eab,
                   'em-naming-probes' = em,
                   'am-naming-probes' = am,
                   condition = condition2) |> 
            mutate(
              
              'eab-discourse-probes' = NA_real_,
              'eab-treatment-probes' = NA_real_,
              'em-discourse-probes' = NA_real_,
              'em-treatment-probes' = NA_real_,
              'am-discourse-probes' = NA_real_,
              'am-treatment-probes' = NA_real_,
              
              'eab-treatment-probes' = ifelse(tx == 1, `eab-naming-probes`, NA_real_),
              'em-treatment-probes' = ifelse(tx == 1, `em-naming-probes`, NA_real_),
              'am-treatment-probes' = ifelse(tx == 1, `am-naming-probes`, NA_real_)
              ) |> 
            mutate(across(contains('probes'), ~replace_na(as.double(.), 0))) |> 
            select(
              participant_id,
              item = word,
              discourse_stimuli,
              filename,
              type,
              condition,
              tx,
              'eab-treatment' = 'eab-treatment-probes', 'eab-naming-probes', 'eab-discourse-probes',
              'em-treatment'  = 'em-treatment-probes', 'em-naming-probes', 'em-discourse-probes',
              'am-treatment'  = 'am-treatment-probes', 'am-naming-probes', 'am-discourse-probes'
            ) |> 
            arrange(condition, tx)
  } else {
    
    
    ###################### REPEAT HERE BUT FOR 500 #########################
    # why getting NA values?
    # the normal case where we have discoures and naming items
    if(naming_only != 1){
      
      # discourse item formatting
      df_discourse <- 
        df |> 
        filter(in_discourse == 1) |> 
        drop_na(discourse_stimuli) |> 
        distinct(participant_id, discourse_stimuli, condition) |> 
        mutate(value = 1, condition2 = condition, type = "discourse") |> 
        pivot_wider(names_from = condition, values_from = value) |> 
        rename('a-discourse-probes' = a,
               'ss-discourse-probes' = ss,
               'sl-discourse-probes' = sl,
               condition = condition2) |> 
        mutate(
          'a-naming-probes' = NA_real_,
          'a-treatment-probes' = NA_real_,
          'ss-naming-probes' = NA_real_,
          'ss-treatment-probes' = NA_real_,
          'sl-naming-probes' = NA_real_,
          'sl-treatment-probes' = NA_real_
        ) |> 
        mutate(across(contains('probes'), ~replace_na(as.double(.), 0))) |> 
        select(
          participant_id,
          item = discourse_stimuli,
          type,
          condition,
          'a-treatment' = 'a-treatment-probes', 'a-naming-probes', 'a-discourse-probes',
          'ss-treatment'  = 'ss-treatment-probes', 'ss-naming-probes', 'ss-discourse-probes',
          'sl-treatment'  = 'sl-treatment-probes', 'sl-naming-probes', 'sl-discourse-probes'
        ) |> 
        mutate(discourse_stimuli = NA)
    }
    
    # naming item formatting
    df_naming <- 
      df |> 
      distinct(participant_id, word, discourse_stimuli, filename, condition, tx) |> 
      mutate(value = 1,
             condition2 = condition,
             type = "naming") |> 
      pivot_wider(names_from = condition, values_from = value) |> 
      rename('a-naming-probes' = a,
             'ss-naming-probes' = ss,
             'sl-naming-probes' = sl,
             condition = condition2) |> 
      mutate(
        
        'a-discourse-probes' = NA_real_,
        'a-treatment-probes' = NA_real_,
        'ss-discourse-probes' = NA_real_,
        'ss-treatment-probes' = NA_real_,
        'sl-discourse-probes' = NA_real_,
        'sl-treatment-probes' = NA_real_,
        
        'a-treatment-probes' = ifelse(tx == 1, `a-naming-probes`, NA_real_),
        'ss-treatment-probes' = ifelse(tx == 1, `ss-naming-probes`, NA_real_),
        'sl-treatment-probes' = ifelse(tx == 1, `sl-naming-probes`, NA_real_)
      ) |> 
      mutate(across(contains('probes'), ~replace_na(as.double(.), 0))) |> 
      select(
        participant_id,
        item = word,
        discourse_stimuli,
        filename,
        type,
        condition,
        tx,
        'a-treatment' = 'a-treatment-probes', 'a-naming-probes', 'a-discourse-probes',
        'ss-treatment'  = 'ss-treatment-probes', 'ss-naming-probes', 'ss-discourse-probes',
        'sl-treatment'  = 'sl-treatment-probes', 'sl-naming-probes', 'sl-discourse-probes'
      ) |> 
      arrange(condition, tx)
    
  }
  
  # test for NA values that we don't want
  stopifnot(
    "NA values detected in input columns" = 
      (sum(is.na(df_naming |> select(8:16))) == 0 & sum(is.na(df_discourse |> select(4:12))) == 0)
  )

  # if naming and discourse, bind the two together, otherwise just return naming
  if(naming_only != 1){
      df_out <- bind_rows(df_naming, df_discourse)
  } else {
    df_out = df_naming
  }

  return(df_out)
}

