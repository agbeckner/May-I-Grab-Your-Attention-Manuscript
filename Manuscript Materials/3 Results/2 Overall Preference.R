################################################################################
#   May I Grab Your Attention Lookit study
#   Script analyzing the overall looking preferences for infants in this study 
#   Written by: Christian Nelson & Lisa Oakes
#   Last edit: 06/26/21
################################################################################

# This is a script in the series of scripts for processing the May I Grab Your Attention Data
# It analyzes the overall looking preferences for infants in this study and generates a table of the t test output
#
#   Infant Cognition Laboratory at the University of California, Davis 
#   https://oakeslab.ucdavis.edu 
#
#   This work is licensed under the Creative Commons Attribution 4.0 
#   International License. To view a copy of this license, visit 
#   http://creativecommons.org/licenses/by/4.0/ or send a letter to 
#   Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
#

#### 000: set up ####
library(tidyverse)
library(lsr)
library(purrr)
library(broom)
library(flextable)


look_dat <- read.csv("~/Box/May I Grab Your Attention/Manuscript Materials/1 Data Files/finalDat.csv") # 721 trials
avg_dat <- read.csv("~/Box/May I Grab Your Attention/Manuscript Materials/1 Data Files/avgDat.csv") # 107 infants
rle_dat <- read.csv("~/Box/May I Grab Your Attention/Manuscript Materials/1 Data Files/RLEdat.csv") # 340 individual fixations

#### 010: Calcualting and evaluating handle preference score across all infants ####

## calculate handle preference score across all infants
avg_dat %>% 
  summarise(avg_handlepref = round(mean(avg_handle_pref), 2), # 0.54
            sd_handlepref = round(sd(avg_handle_pref), 2)) # 0.10

## t test between  handle pref and chance
t.test(avg_dat$avg_handle_pref, alternative = "two.sided", mu = 0.5) # t(106) = 4.23, p < .01

# cohen's d for  handle pref compared to chance
round(cohensD(avg_dat$avg_handle_pref, mu = .5), digits = 2) # 0.41


#### 020: Calcualting and evaluating handle preference score across for each motor group ####

## between motor levels:

# subset into the 5 different motor levels
avg_dat %>% filter(motorLevel == "1") -> preSit
avg_dat %>% filter(motorLevel == "2") -> sit
avg_dat %>% filter(motorLevel == "3") -> crawl
avg_dat %>% filter(motorLevel == "4") -> stand
avg_dat %>% filter(motorLevel == "5") -> walk

# t tests within motor groups
t.test(preSit$avg_handle_pref, alternative = "two.sided", mu = 0.5) -> presit1 # M = .51; p = .27
t.test(sit$avg_handle_pref, alternative = "two.sided", mu = 0.5) -> sit1 # M = .53; p = .23
t.test(crawl$avg_handle_pref, alternative = "two.sided", mu = 0.5) -> crawl1 # M =.55, p = .03
t.test(stand$avg_handle_pref, alternative = "two.sided", mu = 0.5) -> stand1 # M = .54, p = .04
t.test(walk$avg_handle_pref, alternative = "two.sided", mu = 0.5)  -> walk1 # M = .61; p < .01

# put them all together
tab <- map_df(list(presit1, sit1, crawl1, stand1, walk1), tidy)

# format p values
tab$p.value <- round(tab$p.value, digits = 2)


# calculate cohen's d
tab$d <- "NA"
tab$d[[1]] <- round(cohensD(preSit$avg_handle_pref, mu = .5), digits = 2)
tab$d[[2]] <- round(cohensD(sit$avg_handle_pref, mu = .5), digits = 2)
tab$d[[3]] <- round(cohensD(crawl$avg_handle_pref, mu = .5), digits = 2)
tab$d[[4]] <- round(cohensD(stand$avg_handle_pref, mu = .5), digits = 2)
tab$d[[5]] <- round(cohensD(walk$avg_handle_pref, mu = .5), digits = 2)

# put it altogether in a data frame
data.frame(MotorLevel = c("pre-sit", "sit", "crawl", "stand", "walk"),
           df = tab$parameter,
           Mean = tab$estimate <- round(tab$estimate, digits = 2),
           t = tab$statistic <- round(tab$statistic, digits = 2),
           p = ifelse(tab$p.value < .01, "<.01", tab$p.value),
           d = tab$d) -> tab_01

# format table
t <- flextable(tab_01) %>%
  italic(italic = TRUE, part = "header", j = 2:6) %>%
  align(align = "center", part = "header", j = 2:6) %>%
  align(align = "right", i = 1:5, j = 2:6) %>%
  flextable::footnote(i=1,j=5, part="header",
                      value = as_paragraph("One sample t-tests comparing the mean to chance.")) %>%
  set_header_labels(values = list(MotorLevel = "Motor Level",
                                  df = "df",
                                  Mean = "mean",
                                  t = "t",
                                  p = "p",
                                  d = "d")) %>%
  add_header_lines(values = "Table 1. Infant Handle Preference by Motor Level")
t


save_as_docx(t,path = "~/Box/May I Grab Your Attention/Manuscript Output/Table 1.docx")
