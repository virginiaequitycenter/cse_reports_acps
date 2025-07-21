##############################################################
# ACPS Career Self-Efficacy Clean-Up                       
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2023-11-28      
# Summary: A document that cleans the data from the Career Self-Efficacy 
#          survey administered to seventh and eighth graders in ACPS Public Schools
#          in the Fall of 2023
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
# Pull in Raw Self-Efficacy Data                    
##############################################################

career <- read.csv("../raw_data/Fall 2023 Career Self Efficacy Deidentified all ACPS.csv")

##############################################################
# Change names of columns, add Labels              
##############################################################

old_colnames <- colnames(career)
colnames(career) = c("Generated_ID", "Grade_Year", "Gender", "Ethnicity", "Home_Lang_Code",
                     "Gifted_ID", "Immersion_Cohort", "Disability_Code", "WIDA_Level",
                     "Bus_Transportation", "Paper_Opt_In", "Class_Year", "School", "Tags",
                     "SE1", "SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11",
                     "Break", "OE1", "OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10",
                     "Occupation_1", "Occupation_2", "Occupation_3") 


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
# Add Columns for Membership Group Tags     
##############################################################

career <- career %>%
  add_column(AVID=ifelse(grepl("AVID", career$Tags),"AVID", ""))

career <- career %>%
  add_column(Current_EL=ifelse(grepl("ESL", career$Tags),"EL", ""))

career <- career %>%
  add_column(Any_EL=ifelse(grepl("EL", career$Tags),"EL", ""))

career <- career %>%
  add_column(SPED=ifelse(grepl("SPED", career$Tags),"SPED", ""))

career <- career %>%
  add_column(GG1=ifelse(grepl("GG1", career$Tags),"GG1", ""))

career <- career %>%
  add_column(SHP=ifelse(grepl("StarrHill", career$Tags),"SHP", ""))

##############################################################
# Encode Responses as numbers              
##############################################################

survey_seqs <- c("SE1","SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11")
survey_oeqs <- c("OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10")
survey_qs <- c("SE1","SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11",
               "OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10")

career <- career %>% mutate_all(function(x) ifelse(x=="Strongly Agree","5",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Agree","4",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Not sure","3",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Not Sure","3",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Disagree","2",x))
career <- career %>% mutate_all(function(x) ifelse(x=="Strongly Disagree","1",x))

career <- career %>% mutate_at(survey_qs, as.numeric)

career <- select(career, !Break)
career <- select(career, !Tags)

##############################################################
# Create Total Scores and Subscores            
##############################################################

career <- career %>%
  add_column(SE_subscore=rowSums(career[,survey_seqs], na.rm=FALSE))

career <- career %>%
  add_column(OE_subscore=rowSums(career[,survey_oeqs], na.rm=FALSE))

career <- career %>%
  add_column(total_score=career$SE_subscore+career$OE_subscore)

##############################################################
# Add Percentiles for each Subscore           
##############################################################

career$SE_percentile <- 100*(ecdf(career$SE_subscore)(career$SE_subscore)) %>% round(digits=4)
career$OE_percentile <- 100*(ecdf(career$OE_subscore)(career$OE_subscore)) %>% round(digits=4)
career$total_percentile <- 100*(ecdf(career$total_score)(career$total_score)) %>% round(digits=4)

##############################################################
# Change Column Order for Ease of Use         
##############################################################

col_order <-  c("Generated_ID",  "SE_subscore","SE_percentile","OE_subscore","OE_percentile",
                "total_score","total_percentile","Gender","Ethnicity", "School","Grade_Year", 
                "Class_Year","AVID", "Current_EL", "Any_EL", "SPED", "GG1", "SHP","Home_Lang_Code",
                "Gifted_ID", "Immersion_Cohort", "Disability_Code", "WIDA_Level", "Bus_Transportation", 
                "Paper_Opt_In","SE1", "SE2", "SE3", "SE4", "SE5", "SE6", "SE7", "SE8", 
                "SE9", "SE10", "SE11", "OE1", "OE2", "OE3", "OE4", "OE5", "OE6","OE7", "OE8",
                "OE9", "OE10", "Occupation_1", "Occupation_2","Occupation_3") 
career <- career[, col_order]


##############################################################
# Write CSV         
##############################################################

write_excel_csv(career, file = "../data/Fall_2023_CSE_Deidentified_Clean.csv",na = "",
                append = FALSE, delim = ",")



