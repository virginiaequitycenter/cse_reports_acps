##############################################################
# ACPS Career Self-Efficacy Analysis- All ACPS Schools Pilot Analysis- Main Graphs and Appendices               
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-02-01    
# Summary: A document that analyses the data from the Career Self-Efficacy 
#          survey administered to seventh and eighth graders at ACPS Middle Schools
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

career <- read.csv("../data/Fall 2024 Career Self Efficacy Deidentified Clean.csv")

career_narm <- career %>% drop_na(total_score)

##############################################################
# ACPS Middle School- Career Self Efficacy by Ethnicity               
##############################################################

# Data Wrangling
#### This section groups Career Self Efficacy Scores by Ethnicity & finds the mean values by those groups
#### Adds labels to Ethnicity markers

career_eth <- subset(career_narm, career_narm$Ethnicity!="AI" )

mean_Eth <- career_eth %>% group_by(Ethnicity) %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                                                 obs=n())
eth.labs <- c("Asian Students", "Black Students", "Hispanic Students", "Multiracial Students", "White Students")
names(eth.labs) <- c("AS", "BL", "HL", "MR", "WH")

# Plot Creation
#### Plots Journey's Career self efficacy scores by ethnicity with mean value and individual scores

ggplot(data=career_eth, aes(y=total_score)) + 
  geom_point(aes(x=0, color=Ethnicity), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_Eth, aes(yintercept = mean_score,col=Ethnicity), linewidth=1) +
  labs(x="Ethnicity", y="Career Self-Efficacy Score", 
       title="ACPS Middle Schools Career Self Efficacy Scores by Ethnicity")+
  geom_hline(yintercept=80,linetype="dashed")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(50,105), minor_breaks=seq(50,105,5), breaks=seq(50,100,10))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~Ethnicity, labeller= labeller(Ethnicity=eth.labs))+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave("../plots/ACPS_CSE_eth_2024.pdf")

##############################################################
# ACPS- Career Self Efficacy by Student Groups                 
##############################################################

# Data Wrangling
#### This section pulls each Student Group into a subset dataframe to analyze them separately in ggplot
#### Includes calculations of mean values for ggplot lines for each School
#### Using this method rather than grouping by variables so that students in more than one category can be multiply counted

mean_narm <- career_narm %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                             obs=n())
career_shp <- subset(career_narm, Starr_Hill_24=="Yes")
mean_shp <- career_shp %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                     obs=n())
career_avid <- subset(career_narm, AVID=="Yes")
mean_avid <- career_avid %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())
career_el <- subset(career_narm, EL=="Yes")
mean_el <- career_el %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                   obs=n())
career_sped <- subset(career_narm, SPED=="Yes")
mean_sped <- career_sped %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())

# Plot Creation
#### Plot of Journey's Career Self Efficacy Scores for student groups of interest 

ggplot() + 
  geom_point(data=career_narm, aes(y=total_score, x=-1), position=position_jitter(0.1), alpha=.7, color="black") +
  geom_point(data=career_shp, aes(y=total_score, x=0), position=position_jitter(0.1), alpha=.7, color="#8D029E") +
  geom_point(data=career_avid, aes(y=total_score, x=1), position=position_jitter(0.1), alpha=.7, color="#D9470C") +
  geom_point(data=career_el, aes(y=total_score, x=2), position=position_jitter(0.1), alpha=.7, color="#0C9ED9") +
  geom_point(data=career_sped, aes(y=total_score, x=3), position=position_jitter(0.1), alpha=.7, color="#F8BE3D") +
  scale_y_continuous(limits=c(50,105), minor_breaks=seq(50,105,5), breaks=seq(50,100,10)) +
  geom_segment(data= mean_narm, aes(y=mean_score, yend=mean_score, x=-1.25, xend=-.75), linewidth=1, color ="black") +
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
       title="ACPS Middle Schools Career Self Efficacy Scores by Student Group")

ggsave("../plots/ACPS_CSE_stugrp_2024.pdf")

##############################################################
# ACPS- Career Self Efficacy by Grade                
##############################################################

mean_grade <- career_narm %>% group_by(class_year) %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                                                obs=n())

career_narm$class_year <- as.character(career_narm$class_year)
mean_grade$class_year <- as.character(mean_grade$class_year)

ggplot(data=career_narm, aes(y=total_score)) + 
  geom_point(aes(x=0, color=class_year), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_grade, aes(yintercept = mean_score,col=class_year), linewidth=1) +
  labs(x="Graduating Year", y="Career Self-Efficacy Score", 
       title="ACPS Middle Schools Career Self Efficacy Scores by Grade")+
  geom_hline(yintercept=80,linetype="dashed")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(50,105), minor_breaks=seq(50,105,5), breaks=seq(50,100,10))+
  scale_color_manual(values=c("#8D029E", "#D9470C"))+
  facet_grid(~class_year)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave("../plots/ACPS_CSE_grade_2024.pdf")

##############################################################
# ACPS- Career Self Efficacy - 2023 7th Grade vs 2024 8th Grade               
##############################################################

career_23 <- read.csv("../../Fall 2023/data/Fall_2023_CSE_Deidentified_Clean.csv")
career_23<- career_23 %>% drop_na(total_score)
career_24 <- read.csv("../data/Fall 2024 Career Self Efficacy Deidentified Clean.csv")
career_24 <- career_24 %>% drop_na(total_score)

career_23_gr7 <- career_23 %>% subset(career_23$Class_Year=="2029")
mean_23_gr7 <- career_23_gr7 %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                     obs=n())

career_24_gr8 <- career_24 %>% subset(career_24$class_year=="2029")
mean_24_gr8 <- career_24_gr8 %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                           obs=n())

ggplot() + 
  geom_point(data=career_23_gr7, aes(y=total_score, x=-1), position=position_jitter(0.1), alpha=.7, color="#0C9ED9") +
  geom_point(data=career_24_gr8, aes(y=total_score, x=0), position=position_jitter(0.1), alpha=.7, color="#8D029E") +
  scale_y_continuous(limits=c(50,105), minor_breaks=seq(50,105,5), breaks=seq(50,100,10)) +
  geom_segment(data= mean_23_gr7, aes(y=mean_score, yend=mean_score, x=-1.25, xend=-.75), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= mean_24_gr8, aes(y=mean_score, yend=mean_score, x=-.25, xend=.25), linewidth=1, color ="#8D029E")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(x="Grade-Year", y="Career Self-Efficacy Score", 
       title="ACPS Middle Schools Career Self Efficacy Scores: 7th Grade 2023 vs 8th Grade 2024")



