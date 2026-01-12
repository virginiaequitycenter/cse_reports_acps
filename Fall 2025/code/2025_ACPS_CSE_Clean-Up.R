##############################################################
# ACPS Career Self-Efficacy Clean-Up                       
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-01-08      
# Summary: A document that cleans the data from the Career Self-Efficacy 
#          survey administered to seventh and eighth graders in ACPS Public Schools
#          in the Fall of 2025
##############################################################

##############################################################
# Library Intros                               
##############################################################

library(tidyverse)
library(dplyr)
library(readxl)
library(uuid)
library(Hmisc)

##############################################################
# Pull in Self-Efficacy Data                    
##############################################################

career <- read_excel("Fall 2025/raw_data/cse_232425_deidentified_ccp_01_02_2026.xlsx",
                     col_types="text")

##############################################################
# Change names of columns, add Labels              
##############################################################

colnames(career) = c("unique_id","year_completed","grade_at_time_of_survey","class_year","gender",                                                                                                                     
                     "race_ethnicity","disabilities","el","econ_dis","starr_hill","avid","school_sy2526",                                                                                                              
                     "SE1", "SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11",
                     "OE1", "OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10",
                     "Occupation_1", "Occupation_2", "Occupation_3","parent_ed","survey_date")

var.labels = c(SE1="I can find information about five occupations I am interested in.",
               SE2="I can make a plan of my educational goals for the next three years.",
               SE3="I can select one occupation from a list of possible occupations I am considering.",
               SE4="I can determine what occupation would be best for me.",
               SE5="I can resist attempts of my family or friends to push me into a career I believe is beyond my abilities or not for me.",
               SE6="I can describe the job skills of a career I might like to enter.",
               SE7="I can choose a career in which most workers are from a different gender.",
               SE8="I can choose a career that will fit my interests.",
               SE9="I can decide what kind of schooling I will need to achieve my career goal.",
               SE10="I can find out the average salary of people in an occupation.",
               SE11="I can talk with a person already employed in a field I am interested in.",
               OE1="If I learn more about different careers, I will make a better career decision.",
               OE2="If I know my interests and abilities, then I will be able to choose a good career for me.",
               OE3="If I make a good career decision, then my family will approve of me.",
               OE4="If I know about the education I need for different careers, I will make a better career.",
               OE5="If I spend enough time gathering information about careers, I can learn what I need to know when I make a decision.",
               OE6="I intend to spend more time learning about careers than I have been.",
               OE7="I plan to talk to lots of people about careers.",
               OE8="I am determined to talk to my teachers about career opportunities.",
               OE9="I am committed to learning more about my abilities and interests.",
               OE10="I intend to get all the education I need for my career choice.",
               Occupation_1="I intend to be a ...for my occupation.",
               Occupation_2="If I cannot be that, I intend to be a...",
               Occupation_3="If I cannot be either of those, I intend to be a ...")

label(career) = as.list(var.labels[match(names(career), names(var.labels))])

##############################################################
# Encode Responses as numbers              
##############################################################

survey_seqs <- c("SE1","SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11")
survey_oeqs <- c("OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10")
survey_qs <- c("SE1","SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11","OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10")

career <- career %>% mutate_all(function(x) ifelse(x=="Strongly Agree","5",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Agree","4",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Not sure","3",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Not Sure","3",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Disagree","2",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Strongly Disagree","1",x))

career <- career %>% mutate_at(survey_seqs, as.numeric)
career <- career %>% mutate_at(survey_oeqs, as.numeric)

##############################################################
# Change Variable Types             
##############################################################

numeric <- c("SE1","SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11",
             "OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10",
             "year_completed","class_year")
career <- career %>% mutate_at(numeric, as.numeric)

##############################################################
# Add Percentiles for each Subscore           
##############################################################

career <- career %>%
  add_column(SE_subscore=rowSums(career[,survey_seqs], na.rm=FALSE))

career <- career %>%
  add_column(OE_subscore=rowSums(career[,survey_oeqs], na.rm=FALSE))

career <- career %>%
  add_column(total_score=career$SE_subscore+career$OE_subscore)

##############################################################
# Add Total Scores on a scale of 5       
##############################################################

career <- career %>% add_column(SE_subscore_5=rowMeans(career[,survey_seqs]) %>% round(digits=2))

career <- career %>% add_column(OE_subscore_5=rowMeans(career[,survey_oeqs]) %>% round(digits=2))

career <- career %>% add_column(total_score_5=rowMeans(career[,survey_qs]) %>% round(digits=2))

##############################################################
# Add First-Gen Indicator Variable      
##############################################################

career <- career %>% add_column(first_gen="")
career <- career %>% mutate(first_gen= ifelse(parent_ed=="Did not finish high school","Yes",first_gen))
career <- career %>% mutate(first_gen= ifelse(parent_ed=="Finished 2-year college (community college/associates degree)","Yes",first_gen))
career <- career %>% mutate(first_gen= ifelse(parent_ed=="Finished 4-year college or more (bachelors, masters, lawyer, doctor, etc.)","No",first_gen))
career <- career %>% mutate(first_gen= ifelse(parent_ed=="Finished high school","Yes",first_gen))
career <- career %>% mutate(first_gen= ifelse(parent_ed=="Went to college but did not finish","Yes",first_gen))
career <- career %>% mutate(first_gen= ifelse(parent_ed=="I don't know","Unknown",first_gen))

##############################################################
# Change Column Order for Ease of Use         
##############################################################

col_order <-  c("unique_id","year_completed","grade_at_time_of_survey","class_year",
                "SE_subscore","OE_subscore","total_score","gender","race_ethnicity","first_gen",
                "parent_ed","disabilities","el","econ_dis","starr_hill","avid","school_sy2526",          
                "SE1","SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10",
                "OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10",                   
                "Occupation_1","Occupation_2","Occupation_3",
                "SE_subscore_5","OE_subscore_5","total_score_5") 
career <- career[, col_order]


##############################################################
# Write CSV to Data Folder        
##############################################################

write_excel_csv(career, file = "Fall 2025/data/Fall 2023-25 Career Self Efficacy Deidentified Clean.csv",na = "",
                append = FALSE, delim = ",")
