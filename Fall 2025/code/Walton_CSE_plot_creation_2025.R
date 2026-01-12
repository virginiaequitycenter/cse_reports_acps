##############################################################
# ACPS Career Self-Efficacy Analysis- Walton Middle School Analysis - Main Graphs              
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2026-01-08    
# Summary: A document that analyses the data from the Career Self-Efficacy 
#          survey administered to seventh and eighth graders at Walton Middle School
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
library(ggplot2)
library(scales)
library(ggbreak)
library(extrafont)

##############################################################
# Pull in Self-Efficacy Data                    
##############################################################

career <- read.csv("Fall 2025/data/Fall 2023-25 Career Self Efficacy Deidentified Clean.csv")

career_narm <- career %>% drop_na(total_score) %>% filter(year_completed==2025 & school_sy2526=="Leslie H. Walton Middle School")

##############################################################
# Walton CSE Wrangling (Find Group Level Means)              
##############################################################

mean_walton <- career_narm %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                         obs=n())
# by Ethnicity
mean_Eth <- career_narm %>% group_by(race_ethnicity) %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                                                   obs=n())
eth.labs <- c("Asian Students", "Black Students", "Hispanic Students", "Multiracial Students", "White Students")
names(eth.labs) <- c("AS", "BL", "HL", "MR", "WH")

# by Student Group

mean_acps <- career_narm %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())
career_shp <- subset(career_narm, starr_hill=="Yes")
mean_shp <- career_shp %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                     obs=n())
career_avid <- subset(career_narm, avid=="Yes")
mean_avid <- career_avid %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())
career_el <- subset(career_narm, el=="Yes")
mean_el <- career_el %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                   obs=n())
career_dis <- subset(career_narm, disabilities=="Yes")
mean_dis <- career_dis %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                     obs=n())
career_firstgen <- subset(career_narm, first_gen=="Yes")
mean_firstgen <- career_firstgen %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                               obs=n())

# by Grade

career_narm$grade_at_time_of_survey <- as.character(career_narm$grade_at_time_of_survey)

mean_grade <- career_narm %>% group_by(grade_at_time_of_survey) %>% 
  summarise(mean_score=mean(total_score, na.rm=TRUE),
            obs=n())

mean_grade$grade_at_time_of_survey <- as.character(mean_grade$grade_at_time_of_survey)

grade.labs <- c("6th", "7th", "8th")
names(grade.labs) <- c("6","7","8")

##############################################################
# Walton CSE by Ethnicity Plot                    
##############################################################

ggplot(data=career_narm, aes(y=total_score)) + 
  geom_point(aes(x=0, color=race_ethnicity), position=position_jitter(0.1), alpha=1) +
  geom_hline(data= mean_Eth, aes(yintercept = mean_score,col=race_ethnicity), linewidth=1) +
  labs(x="Ethnicity", y="Career Self-Efficacy Score", 
       title="Walton Middle School Career Self Efficacy Scores by Ethnicity")+
  geom_hline(yintercept=80,linetype="dashed")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(50,110), minor_breaks=seq(50,110,5), breaks=seq(50,110,10))+
  scale_color_manual(values=c("#8D029E","#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~race_ethnicity, labeller= labeller(race_ethnicity=eth.labs))+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave("Fall 2025/plots/Walton_CSE_eth_2025.pdf")

##############################################################
# Walton CSE by Student Group Plot                    
##############################################################

ggplot() + 
  geom_point(data=career_narm, aes(y=total_score, x=-1), position=position_jitter(0.1), alpha=.7, color="black") +
  geom_point(data=career_shp, aes(y=total_score, x=0), position=position_jitter(0.1), alpha=1, color="#8D029E") +
  geom_point(data=career_avid, aes(y=total_score, x=1), position=position_jitter(0.1), alpha=1, color="#D9470C") +
  geom_point(data=career_el, aes(y=total_score, x=2), position=position_jitter(0.1), alpha=1, color="#0C9ED9") +
  geom_point(data=career_dis, aes(y=total_score, x=3), position=position_jitter(0.1), alpha=1, color="#139E02") +
  geom_point(data=career_firstgen, aes(y=total_score, x=4), position=position_jitter(0.1), alpha=1, color="#F8BE3D") +
  scale_y_continuous(limits=c(50,110), minor_breaks=seq(50,110,5), breaks=seq(50,110,10)) +
  geom_segment(data= mean_walton, aes(y=mean_score, yend=mean_score, x=-1.25, xend=-.75), linewidth=1, color ="black") +
  geom_segment(data= mean_shp, aes(y=mean_score, yend=mean_score, x=-.25, xend=.25), linewidth=1, color ="#8D029E") +
  geom_segment(data= mean_avid, aes(y=mean_score, yend=mean_score, x=.75, xend=1.25), linewidth=1, color ="#D9470C") +
  geom_segment(data= mean_el, aes(y=mean_score, yend=mean_score, x=1.75, xend=2.25), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= mean_dis, aes(y=mean_score, yend=mean_score, x=2.75, xend=3.25), linewidth=1, color ="#139E02") +
  geom_segment(data= mean_firstgen, aes(y=mean_score, yend=mean_score, x=3.75, xend=4.25), linewidth=1, color ="#F8BE3D") +
  geom_hline(yintercept=80,linetype="dashed")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(x="Student Group", y="Career Self-Efficacy Score", 
       title="Walton Middle School Career Self Efficacy Scores by Student Group")

ggsave("Fall 2025/plots/Walton_CSE_stugrp_2025.pdf")

##############################################################
# Walton CSE by Grade Plot                 
##############################################################

ggplot(data=career_narm, aes(y=total_score)) + 
  geom_point(aes(x=0, color=grade_at_time_of_survey), position=position_jitter(0.1), alpha=1) +
  geom_hline(data= mean_grade, aes(yintercept = mean_score,col=grade_at_time_of_survey), linewidth=1) +
  labs(x="Ethnicity", y="Career Self-Efficacy Score", 
       title="Walton Middle School Career Self Efficacy Scores by Grade")+
  geom_hline(yintercept=80,linetype="dashed")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(50,110), minor_breaks=seq(50,110,5), breaks=seq(50,110,10))+
  scale_color_manual(values=c("#8D029E","#D9470C", "#0C9ED9"))+
  facet_grid(~grade_at_time_of_survey, labeller= labeller(grade_at_time_of_survey=grade.labs))+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave("Fall 2025/plots/Walton_CSE_grade_2025.pdf")

