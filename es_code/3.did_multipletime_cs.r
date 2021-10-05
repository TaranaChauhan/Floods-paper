
################################################################################

#PAPER: Do political motivations affect recovery from hazards? Analysis of floods in India
#AUTHOR: Tarana Chauhan (tc678@cornell.edu)
#DATE: October 4, 2021

# FILE PURPOSE: Code for Figure 3, Tables 4, 5 and 6
  # Fig 3: Group-time average treatment effects under conditional parallel trends' assumption
  # Table 3: Group-time average treatment effects with not-yet treated units
  # Table 4: Group-time average treatment effects with never treated units
  # Table 5: Average treatment effects

# Reference: Callaway, Brantly and Pedro H.C. Sant'Anna. 2020 "Difference-in-Differences with Multiple Time Periods." 

# R version: R-4.1.1; R Studio: 2021.09.0 Build 351

# PACKAGES REQUIRED
#install.packages('rtools')
#install.packages('did')
#install.packages("ggplot2")
library(did)
library(tibble)
library(ggplot2)

# sET-UP
setwd("C:/Users/taran/Dropbox/ES_submission") # set directory

################################################################################

# 1. Import data 
cs_data = read.csv("es_data/cs_data.csv", header=TRUE) # not-flooded = not yet flooded constituencies
cs_data2 = read.csv("es_data/cs_data2.csv", header=TRUE) # not-flooded = never flooded constituencies


# 2. run did function


atts_ny <- att_gt(yname = "nl_med",
               tname = "year", # time variable
               idname = "constituency_fe", # id variable
               gname = "ny_group", # first treatment period variable
               data = cs_data, # data
               #xformla = NULL, # no covariates
               xformla = ~ population , # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "notyet_flood", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "constituency_fe", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional

atts_nev <- att_gt(yname = "nl_med",
                  tname = "year", # time variable
                  idname = "constituency_fe", # id variable
                  gname = "nev_group", # first treatment period variable
                  data = cs_data2, # data
                  #xformla = NULL, # no covariates
                  xformla = ~ population , # with covariates
                  est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
                  control_group = "never_flood", # set the comparison group which is either "nevertreated" or "notyettreated" 
                  bstrap = TRUE, # if TRUE compute bootstrapped SE
                  biters = 1000, # number of bootstrap iterations
                  print_details = FALSE, # if TRUE, print detailed results
                  clustervars = "constituency_fe", # cluster level
                  panel = TRUE) # whether the data is panel or repeated cross-sectional

# Aggregate ATT
agg_effects_ny <- aggte(atts_ny, type = "simple", alp = .1)
summary(agg_effects_ny)

agg_effects_nev <- aggte(atts_nev, type = "simple", alp=.1)
summary(agg_effects_nev)



# 3. Figure 3

ggdid(atts_ny)
ggsave("fig3_a.jpg")
ggdid(atts_nev)
ggsave("fig3_b.jpg")

# 4. Tables 3 and 4

# Group-time ATTs
summary(atts_ny)
summary(atts_nev)

# Event-study
agg_effects_es_ny <- aggte(atts_ny, type = "dynamic")
summary(agg_effects_es_ny)
agg_effects_es_nev <- aggte(atts_nev, type = "dynamic")
summary(agg_effects_es_nev)

################################################################################

