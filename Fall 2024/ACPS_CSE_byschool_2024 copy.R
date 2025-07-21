##############################################################
# ACPS Career Self-Efficacy Analysis- Burley Pilot Analysis- Main Graphs and Appendices               
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-01-10    
# Summary: A document that analyses the data from the Career Self-Efficacy 
#          survey administered to seventh and eighth graders at Burley Middle School
#          in the Fall of 2023- Final with Beautification including Appendices
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

career <- read.csv("Fall 2024 Career Self Efficacy Deidentified Clean.csv")

career_narm <- career %>% drop_na(total_score)

##############################################################
# ACPS Career Self Efficacy by School                 
##############################################################

# Data Wrangling
#### This section pulls each School into a subset dataframe to analyze them separately in ggplot
#### Includes calculations of mean values for ggplot lines for each School
#### Using this method rather than grouping by the variable School to include all ACPS Schools as a category

mean_ACPS <- career_narm %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())                                          

career_burley <- subset(career_narm, school=="Jackson P. Burley Middle School")
mean_burley <- career_burley %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                           obs=n())
career_lab <- subset(career_narm, school=="Community Lab School")
mean_lab <- career_lab %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                     obs=n())
career_henley <- subset(career_narm, school=="Joseph T. Henley Middle School")
mean_henley <- career_henley %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                           obs=n())  
career_journey <- subset(career_narm, school=="Journey Middle School")
mean_journey <- career_journey %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                             obs=n())
career_lakeside <- subset(career_narm, school=="Lakeside Middle School")
mean_lakeside <- career_lakeside %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                               obs=n())
career_walton <- subset(career_narm, school=="Leslie H. Walton Middle School")
mean_walton <- career_walton %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                           obs=n())

# Plot Creation
#### Plot of all ACPS Middle Schools' Career Self Efficacy Scores for all tested 7th and 8th graders 

ggplot() + 
  geom_point(data=career_narm, aes(y=total_score, x=-1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=career_lab, aes(y=total_score, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=career_burley, aes(y=total_score, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=career_henley, aes(y=total_score, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_point(data=career_journey, aes(y=total_score, x=3), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  geom_point(data=career_lakeside, aes(y=total_score, x=4), position=position_jitter(0.175), alpha=.5, color="#F8BE3D") +
  geom_point(data=career_walton, aes(y=total_score, x=5), position=position_jitter(0.175), alpha=.5, color="#01039E") +
  scale_y_continuous(limits=c(50,105), minor_breaks=seq(50,105,5), breaks=seq(50,100,10)) +
  geom_segment(data= mean_ACPS, aes(y=mean_score, yend=mean_score, x=-1.4, xend=-.6), linewidth=1, color ="black") +
  geom_segment(data= mean_lab, aes(y=mean_score, yend=mean_score, x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= mean_burley, aes(y=mean_score, yend=mean_score, x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= mean_henley, aes(y=mean_score, yend=mean_score, x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= mean_journey, aes(y=mean_score, yend=mean_score, x=2.6, xend=3.4), linewidth=1, color ="#139E02") +
  geom_segment(data= mean_lakeside, aes(y=mean_score, yend=mean_score, x=3.6, xend=4.4), linewidth=1, color ="#F8BE3D") +
  geom_segment(data= mean_walton, aes(y=mean_score, yend=mean_score, x=4.6, xend=5.4), linewidth=1, color ="#01039E") +
  geom_hline(yintercept=80,linetype="dashed")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(x="School", y="Career Self-Efficacy Score", 
       title="ACPS Career Self Efficacy Scores by School")

ggsave("ACPS Career Self Efficacy Scores by School- 2024.pdf")


