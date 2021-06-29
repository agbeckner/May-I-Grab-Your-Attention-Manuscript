################################################################################
#   May I Grab Your Attention Lookit study
#   Script conducting the LMM on individual look durations
#   Written by: Christian Nelson & Lisa Oakes
#   Last edit: 06/26/21
################################################################################

# This is a script in the series of scripts for processing the May I Grab Your Attention Data
# It generates the analyses of the individual look durations and creates the figure of the results
#
#   Infant Cognition Laboratory at the University of California, Davis 
#   https://oakeslab.ucdavis.edu 
#
#   This work is licensed under the Creative Commons Attribution 4.0 
#   International License. To view a copy of this license, visit 
#   http://creativecommons.org/licenses/by/4.0/ or send a letter to 
#   Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
#
# %%%%%%%%%%%%%%%%%%%%%%
# DATA FILE DESCRIPTIONS
# %%%%%%%%%%%%%%%%%%%%%%
#
# This script  uses the following files, which was produced by
#    RLEdat.csv


#### 000: set up ####

library(tidyverse)
library(lmerTest)
library(performance)
library(ggpubr)
library(ggplot2)
library(cowplot)


#### 010: read in the data ####

rle_dat <- read.csv("~/Downloads/May-I-Grab-Your-Attention-Manuscript-main/Manuscript Materials/1 Data Files/RLEdat.csv") # 340 individual fixations
stim_info <- read.csv("~/Downloads/May-I-Grab-Your-Attention-Manuscript-main/Manuscript Materials/1 Data Files/stim_for_ind_look.csv")

#### 020: add size info to the data ####

stim_info1 <- stim_info %>% select(stim,size)


look_dat <- merge(rle_dat, stim_info1, by.x=c("stim"),by.y=c("stim"))


#### 030:  Determine which variables should be included in the final model by checking collinearity #####

lmer(adjusted_length ~ motorLevel+stim_handle+fixationnumber+scale(age_in_days)+stim_sal+size + (1|stim) + (1|child_hashed_id), data = look_dat) -> model_collinearity

check_collinearity(model_collinearity) 

# Motor Level and Age in days are collinear, we will include motor level in the analysis because it is our variable of interest

#### 040: overall summary ####
# How many looks, on average, did infants have?

sum_looking <- rle_dat %>% 
  group_by(child_hashed_id,filename) %>% 
  summarise (trial_num_looks = max(fixationnumber))

sum_looking2 <- sum_looking %>% 
  group_by(child_hashed_id) %>% 
  summarise(avg_num_looks = mean(trial_num_looks))

summary <- sum_looking2 %>% 
  summarise(mean_num_looks = mean(avg_num_looks),
            sd_num_looks = sd(avg_num_looks))



## are looks to the handled objects longer?
# Create an average score for each infant
rle_dat %>% group_by(child_hashed_id, stim_handle) %>% 
  summarise(mean_look_duration = mean(adjusted_length)) %>% 
  ungroup() -> avg_look_length
  
t.test(avg_look_length$mean_look_duration ~ avg_look_length$stim_handle, paired = T) # t(106) = 4.297, p < .001

avg_look_length %>% group_by(stim_handle) %>% 
  summarise(mean_length = round(mean(mean_look_duration), digits = 2),
            sd_length = round(sd(mean_look_duration), digits = 2))

# handle: mean = 1334, sd = 502
# non-handle: mean = 1119, sd = 357


#### 030: LMM on individual look durations ####

## LMM stimulus X motor level X fixation index
lmer(adjusted_length ~ motorLevel*stim_handle*fixationnumber + stim_sal + size + (1|stim) + (1|child_hashed_id), data = look_dat) -> handleXmotorLevelXlook_model
summary(handleXmotorLevelXlook_model)
anova(handleXmotorLevelXlook_model)

###### 040: calculate emmeans on 3 way interaction
look_level_model_emmeans <- as.data.frame(emmeans::emmeans(handleXmotorLevelXlook_model, ~ stim_handle*fixationnumber*motorLevel, 
                                                           at = list(motorLevel = seq(1,5,1),
                                                                     fixationnumber = seq(1,7,1)), # each fixation
                                                           pbkrtest.limit = 3340))
look_level_model_emmeans

# create labels for plots:
motor.labs <- c("pre-sit", "sit", "crawl", "stand", "walk")
names(motor.labs) <- c("1", "2", "3", "4", "5")

###### plot the observed data
ggplot(data = rle_dat, 
       aes(x = as.numeric(fixationnumber), 
           y = adjusted_length, 
           group = stim_handle, 
           color = as.factor(stim_handle),
           shape = as.factor(child_gender))) +
  ylab("Observed Mean \n Fixation (ms)") + xlab("") +
  #geom_jitter(shape = 1, alpha = .7, size = 2, width = NULL)+
  geom_jitter(alpha = .7, size = 2, width = NULL)+
  geom_smooth(method=lm, se=T, alpha = .5) + 
  labs(color= "Stimulus", shape = "Sex") + 
  coord_cartesian(ylim = c(0,5000)) + 
  scale_x_continuous(breaks = seq(1, 11, 2)) +
  scale_shape_manual(values = c(1, 5), labels = c("girl", "boy")) +
  scale_color_manual(values = c("#FF8C94", "#355C7D"), labels = c("handle", "not handle")) +
  theme_half_open(25) + 
  background_grid(minor = 'none') + 
  facet_grid(cols = vars(motorLevel),labeller = labeller(motorLevel = motor.labs)) -> observed_plot
observed_plot



## plot differences by motor level
ggplot(data = look_level_model_emmeans, 
       aes(x = as.numeric(fixationnumber), 
           y = emmean, 
           group = stim_handle, 
           color = as.factor(stim_handle))) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, 
                size = 0.7,
                alpha = .8, 
                color = "gray40") +
  ylab("Estimated Mean \n Fixation (ms)") + xlab("Fixation Index") +
  geom_point(size = 2.7) +
  geom_line(size = .8) +
  #labs(color= "Stimulus") + 
  #theme(legend.position = "none")+
  scale_color_manual(values = c("#FF8C94", "#355C7D")) +
  coord_cartesian(ylim = c(0,2500)) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  theme_half_open(25) + 
  background_grid(minor = 'none') + 
  facet_grid(cols = vars(motorLevel),
             labeller = labeller(motorLevel = motor.labs)) -> estimated_plot

estimated_plot +  theme(legend.position = "none") #-> plot2


# put the plots together
cowplot::plot_grid(observed_plot + theme(legend.position = "none"),
                   estimated_plot + theme(legend.position = "none"),
                   labels = c('A', 'B'), 
                   ncol = 1, 
                   label_size = 25) -> combo_plot
combo_plot

# format the legend
legend <- get_legend(observed_plot + theme(legend.box.margin = margin(0, 0, 0, 0)))

# add it
legend_added <- plot_grid(combo_plot, legend, rel_widths = c(3, .3)) 
legend_added


setwd("~/Downloads/May-I-Grab-Your-Attention-Manuscript-main/Manuscript Materials/1 Data Files/")
ggexport(legend_added, filename = "Figure 2.jpg", height = 1000, width = 1700)


#### 060: Sanity check--LMM on individual look durations using age in days rather than motor level ####

## LMM stimulus X motor level X fixation index
lmer(adjusted_length ~ scale(age_in_days)*stim_handle*fixationnumber + stim_sal + size + (1|stim) + (1|child_hashed_id), data = look_dat) -> age_sanity_check_model
summary(age_sanity_check_model)
anova(age_sanity_check_model)

# This model does not reveal interactions betweeen age in days and stimulus type (handled vs non-handled)



