library(tidyverse)
library(googlesheets4)
library(fixest)
library(modelsummary)


# Read Data ---------------------------------------------------------------

data <- range_read(ss = "https://docs.google.com/spreadsheets/d/10yWn6S78KfhaMq22Nc-TIzA_IiekYzEfGmTi-JkhHhE/edit?gid=0#gid=0") |> 
  na.omit() |> 
  mutate(REACTION_TIME = as.numeric(REACTION_TIME)) |> 
  mutate(HEART_RATE_MANUAL = as.numeric(HEART_RATE_MANUAL)) |> 
  mutate(AWAKE_SUBJ = as.numeric(AWAKE_SUBJ)) |> 
  mutate(ANXIOUS_SUBJ = as.numeric(ANXIOUS_SUBJ)) |> 
  mutate(OVERALL_SUBJ = as.numeric(OVERALL_SUBJ)) |> 
  mutate(SEQUENCE_MEMORY = as.numeric(SEQUENCE_MEMORY)) |> 
  mutate(AIM_TRAINER = as.numeric(AIM_TRAINER)) |> 
  mutate(TREATMENT = fct_relevel(as.factor(TREATMENT), "Placebo")) |>  
  mutate(correct_guess = if_else(SUBJ_GUESS == TREATMENT, 1, 0))


# Regression --------------------------------------------------------------

# Reaction Time
model_1 <- fixest::feols(REACTION_TIME ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_1) 

# Correct Guess
model_2 <- fixest::feglm(correct_guess ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_2) 

# Correct Guess
model_2 <- fixest::feglm(correct_guess ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_2) 

# Heart Rate
model_3 <- fixest::feols(HEART_RATE_MANUAL ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_3) 

# Awake
model_4 <- fixest::feols(AWAKE_SUBJ ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_4) 

# ANXIOUS 
model_5 <- fixest::feols(ANXIOUS_SUBJ ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_5) 

# Overall 
model_6 <- fixest::feols(OVERALL_SUBJ ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_6) 

# SEQUENCE_MEMORY 
model_7 <- fixest::feols(SEQUENCE_MEMORY ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_7) 

# Aim Trainer
model_8 <- fixest::feols(AIM_TRAINER ~ TREATMENT | PARTICIPANT_ID, data = data) 
modelsummary::modelplot(model_8) 

# Display -----------------------------------------------------------------


