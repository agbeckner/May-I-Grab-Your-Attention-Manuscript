################################################################################
#   May I Grab Your Attention Lookit study
#   Script analyzing the general characteristics of infant looking preference in this study
#   Written by: Christian Nelson & Lisa Oakes
#   Last edit: 06/26/21
################################################################################

# This is a script in the series of scripts for processing the May I Grab Your Attention Data
# It analyzes the general characteristics of infant looking preference in this study
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

# read in the data
look_dat <- read.csv("~/Box/May I Grab Your Attention/Manuscript Materials/1 Data Files/finalDat.csv") #721 trials
avg_dat <- read.csv("~/Box/May I Grab Your Attention/Manuscript Materials/1 Data Files/avgDat.csv") # 107 infants
rle_dat <- read.csv("~/Box/May I Grab Your Attention/Manuscript Materials/1 Data Files/RLEdat.csv") # 340 individual fixations

#### 010: General Characteristics of Infants' Looking ####

as.data.frame(table(look_dat$child_hashed_id)) -> table1 

# calculate the number of trials infants completed
table1 %>% 
  summarise(avg_trials = round(mean(Freq), 2), # 6.74
            sd_trials = round(sd(Freq), 2), # 1.74
            minimum = min(Freq), # 1
            maximum = max(Freq))  # 8

## calculate average total looking to the stimulus
avg_dat %>% 
  summarise(avg_total_look = round(mean(avg_look), 2), # 5079.3 ms
            sd_total_look = round(sd(avg_look), 2)) # 1084.86 ms


#### 020: Correlations with age and motor level and interest in the task ####

## correlation between total look and age in days
cor.test(avg_dat$age_in_days, avg_dat$avg_look)  # r = .004

## format data
table1 %>% rename(child_hashed_id = Var1, n_trials = Freq) -> table2 # rename cols for merge

merge(table2, avg_dat, by = "child_hashed_id") -> avg_dat2 # merge dataframes

avg_dat2 %>% 
  group_by(child_hashed_id) %>% 
  dplyr::select(child_hashed_id, age_in_days, n_trials, motorLevel) %>% unique()-> avg_dat3

# correlation between n_trials complete and age in days
cor.test(avg_dat3$age_in_days, avg_dat3$n_trials) # r = .01

# correlation between average duration of looking and motor level
cor.test(avg_dat$motorLevel, avg_dat$avg_look, method = "spearman") # r = .09

# correlation between n_trials complete and motor level
cor.test(avg_dat3$motorLevel, avg_dat3$n_trials, method = "spearman") # r = .11

# correlation between age in days and motor level
cor.test(avg_dat3$motorLevel, avg_dat3$age_in_days,, method = "spearman") # r = .87
