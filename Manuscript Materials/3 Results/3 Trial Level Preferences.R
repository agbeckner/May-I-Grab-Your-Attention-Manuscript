################################################################################
#   May I Grab Your Attention Lookit study
#   Script analyzing the trial level preferences 
#   Written by: Christian Nelson & Lisa Oakes
#   Last edit: 06/27/21
################################################################################

# This is a script in the series of scripts for processing the May I Grab Your Attention Data
# It analyzes the trial level preferences 
#
#   Infant Cognition Laboratory at the University of California, Davis 
#   https://oakeslab.ucdavis.edu 
#
#   This work is licensed under the Creative Commons Attribution 4.0 
#   International License. To view a copy of this license, visit 
#   http://creativecommons.org/licenses/by/4.0/ or send a letter to 
#   Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


#### 000: set up ####

library(tidyverse)
library(lmerTest)
library(performance)

#### 010: read in files ####

look_dat <- read.csv("~/Box/May I Grab Your Attention/Manuscript Materials/1 Data Files/finalDat.csv")
stim_info <- read.csv("~/Box/May I Grab Your Attention/Manuscript Materials/1 Data Files/StimKey_Map.csv")


#### 020: create files for analysis ####
look_dat %>% 
  arrange(child_hashed_id,new_trial_order) %>% 
  group_by(child_hashed_id) %>% 
  dplyr::select(child_hashed_id, age_in_days, motorLevel, new_trial_order, centered_handle_pref, avg_handle_salience, prop_salience_difference, key, filename)-> look_dat2

obj_size_info <- stim_info %>% select(image_id,prop_size_difference)
obj_size_info <- unique(obj_size_info)

look_dat3 <- merge(look_dat2,obj_size_info, by.x="filename",by.y="image_id")


#### 030:  Determine which variables should be included in the final model by checking collinearity #####

lmer(centered_handle_pref ~ motorLevel+new_trial_order+scale(age_in_days)+prop_salience_difference+prop_size_difference + (1|filename) + (1|child_hashed_id), data = look_dat3) -> model_collinearity

check_collinearity(model_collinearity) 

# Motor Level and Age in days are collinear, we will include motor level in the analysis because it is our variable of interest

#### 040:  Conduct the final LMM, including the variables that are not collinear, and including the interaction term ####

lmer(centered_handle_pref ~ motorLevel*new_trial_order+prop_salience_difference+prop_size_difference + (1|filename) + (1|child_hashed_id), data = look_dat3) -> motorXtrial
summary(motorXtrial)
anova(motorXtrial) 

## This model revealed only a marginal effect of salience level.

#### 050:  Sanity check--include age in days instead of motor level in the analysis ####

lmer(centered_handle_pref ~ scale(age_in_days)*new_trial_order+prop_salience_difference+prop_size_difference + (1|filename) + (1|child_hashed_id), data = look_dat3) -> motorXtrial2
summary(motorXtrial2)
anova(motorXtrial2) 

## This model revealed only a marginal effect of salience level.
