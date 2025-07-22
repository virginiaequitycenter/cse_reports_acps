##############################################################
# ACPS Career Self-Efficacy Analysis- Burley Middle School            
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-07-22    
# Summary: A document that analyses the data from the Career 
#          Self-Efficacy survey administered to seventh and eighth
#          graders at Burley Middle School in the Fall of 2024
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

career <- read.csv("../data/Fall 2024 Career Self Efficacy Deidentified Clean.csv")

career_narm <- career %>% drop_na(total_score)

career_burley <- subset(career_narm, school=="Jackson P. Burley Middle School")
mean_burley <- career_burley %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                             obs=n())

##############################################################
# Burley Middle School- Career Self Efficacy by Ethnicity               
##############################################################

# Data Wrangling
#### This section groups Career Self Efficacy Scores by Ethnicity & finds the mean values by those groups
#### Adds labels to Ethnicity markers

mean_Eth <- career_burley %>% group_by(Ethnicity) %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                                                 obs=n())
eth.labs <- c("Asian Students", "Black Students", "Hispanic Students", "Multiracial Students", "White Students")
names(eth.labs) <- c("AS", "BL", "HL", "MR", "WH")

# Plot Creation
#### Plots Burley's Career self efficacy scores by ethnicity with mean value and individual scores

ggplot(data=career_burley, aes(y=total_score)) + 
  geom_point(aes(x=0, color=Ethnicity), position=position_jitter(0.1), alpha=1) +
  geom_hline(data= mean_Eth, aes(yintercept = mean_score,col=Ethnicity), linewidth=1) +
  labs(x="Ethnicity", y="Career Self-Efficacy Score", 
       title="Burley Middle School Career Self Efficacy Scores by Ethnicity")+
  geom_hline(yintercept=80,linetype="dashed")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(50,105), minor_breaks=seq(50,105,5), breaks=seq(50,100,10))+
  scale_color_manual(values=c("#8D029E","#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~Ethnicity, labeller= labeller(Ethnicity=eth.labs))+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave("../plots/Burley_CSE_eth_2024.pdf")

##############################################################
# Burley Middle School- Career Self Efficacy by Student Groups                 
##############################################################

# Data Wrangling
#### This section pulls each Student Group into a subset dataframe to analyze them separately in ggplot
#### Includes calculations of mean values for ggplot lines for each School
#### Using this method rather than grouping by variables so that students in more than one category can be multiply counted

mean_burley <- career_burley %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                             obs=n())
career_shp <- subset(career_burley, Starr_Hill_24=="Yes")
mean_shp <- career_shp %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                     obs=n())
career_avid <- subset(career_burley, AVID=="Yes")
mean_avid <- career_avid %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())
career_el <- subset(career_burley, EL=="Yes")
mean_el <- career_el %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                   obs=n())

career_sped <- subset(career_burley, SPED=="Yes")
mean_sped <- career_sped %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())

# Plot Creation
#### Plot of Burley's Career Self Efficacy Scores for student groups of interest 

ggplot() + 
  geom_point(data=career_burley, aes(y=total_score, x=-1), position=position_jitter(0.1), alpha=1, color="black") +
  geom_point(data=career_shp, aes(y=total_score, x=0), position=position_jitter(0.1), alpha=1, color="#8D029E") +
  geom_point(data=career_avid, aes(y=total_score, x=1), position=position_jitter(0.1), alpha=1, color="#D9470C") +
  geom_point(data=career_el, aes(y=total_score, x=2), position=position_jitter(0.1), alpha=1, color="#0C9ED9") +
  geom_point(data=career_sped, aes(y=total_score, x=3), position=position_jitter(0.1), alpha=1, color="#F8BE3D") +
  scale_y_continuous(limits=c(50,105), minor_breaks=seq(50,105,5), breaks=seq(50,100,10)) +
  geom_segment(data= mean_burley, aes(y=mean_score, yend=mean_score, x=-1.25, xend=-.75), linewidth=1, color ="black") +
  geom_segment(data= mean_shp, aes(y=mean_score, yend=mean_score, x=-.25, xend=.25), linewidth=1, color ="#8D029E") +
  geom_segment(data= mean_avid, aes(y=mean_score, yend=mean_score, x=.75, xend=1.25), linewidth=1, color ="#D9470C") +
  geom_segment(data= mean_el, aes(y=mean_score, yend=mean_score, x=1.75, xend=2.25), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= mean_sped, aes(y=mean_score, yend=mean_score, x=2.75, xend=3.25), linewidth=1, color ="#F8BE3D") +
  geom_hline(yintercept=80,linetype="dashed")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(x="Student Group", y="Career Self-Efficacy Score", 
       title="Burley Middle School Career Self Efficacy Scores by Student Group")

ggsave("../plots/Burley_CSE_studentgrp_2024.pdf")