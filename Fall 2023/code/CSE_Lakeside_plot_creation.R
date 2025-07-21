##############################################################
# ACPS Career Self-Efficacy Analysis- Lakeside Pilot Analysis- Main Graphs and Appendices               
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-01-10    
# Summary: A document that analyses the data from the Career Self-Efficacy 
#          survey administered to seventh and eighth graders at Lakeside Middle School
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

career <- read.csv("../data/Fall_2023_CSE_Deidentified_Clean.csv")

career_narm <- career %>% drop_na(total_score)

career_lakeside <- subset(career_narm, School=="Lakeside Middle School")
mean_lakeside <- career_lakeside %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                             obs=n())

##############################################################
# Lakeside Middle School- Career Self Efficacy by Ethnicity               
##############################################################

# Data Wrangling
#### This section groups Career Self Efficacy Scores by Ethnicity & finds the mean values by those groups
#### Adds labels to Ethnicity markers

career_lakeside_eth <- subset(career_lakeside, career_lakeside$Ethnicity!="AI" )

mean_Eth <- career_lakeside_eth %>% group_by(Ethnicity) %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                                                 obs=n())
eth.labs <- c("Asian Students", "Black Students", "Hispanic Students", "Multiracial Students", "White Students")
names(eth.labs) <- c("AS", "BL", "HL", "MR", "WH")

# Plot Creation
#### Plots Lakeside's Career self efficacy scores by ethnicity with mean value and individual scores

ggplot(data=career_lakeside_eth, aes(y=total_score)) + 
  geom_point(aes(x=0, color=Ethnicity), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_Eth, aes(yintercept = mean_score,col=Ethnicity), linewidth=1) +
  labs(x="Ethnicity", y="Career Self-Efficacy Score", 
       title="Lakeside Middle School Career Self Efficacy Scores by Ethnicity")+
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

ggsave("../plots/Lakeside Middle School Career Self Efficacy Scores by Ethnicity.pdf")

##############################################################
# Lakeside Middle School- Career Self Efficacy by Student Groups                 
##############################################################

# Data Wrangling
#### This section pulls each Student Group into a subset dataframe to analyze them separately in ggplot
#### Includes calculations of mean values for ggplot lines for each School
#### Using this method rather than grouping by variables so that students in more than one category can be multiply counted

mean_lakeside <- career_lakeside %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                             obs=n())
career_shp <- subset(career_lakeside, SHP=="SHP")
mean_shp <- career_shp %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                     obs=n())
career_avid <- subset(career_lakeside, AVID=="AVID")
mean_avid <- career_avid %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())
career_el <- subset(career_lakeside, Current_EL=="EL")
mean_el <- career_el %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                   obs=n())
career_gg1 <- subset(career_lakeside, GG1=="GG1")
mean_gg1 <- career_gg1 %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                     obs=n())
career_sped <- subset(career_lakeside, SPED=="SPED")
mean_sped <- career_sped %>% summarise(mean_score=mean(total_score, na.rm=TRUE),
                                       obs=n())

# Plot Creation
#### Plot of Lakeside's Career Self Efficacy Scores for student groups of interest 

ggplot() + 
  geom_point(data=career_lakeside, aes(y=total_score, x=-1), position=position_jitter(0.1), alpha=.7, color="black") +
  geom_point(data=career_shp, aes(y=total_score, x=0), position=position_jitter(0.1), alpha=.7, color="#8D029E") +
  geom_point(data=career_el, aes(y=total_score, x=1), position=position_jitter(0.1), alpha=.7, color="#0C9ED9") +
  geom_point(data=career_gg1, aes(y=total_score, x=2), position=position_jitter(0.1), alpha=.7, color="#139E02") +
  geom_point(data=career_sped, aes(y=total_score, x=3), position=position_jitter(0.1), alpha=.7, color="#F8BE3D") +
  scale_y_continuous(limits=c(50,105), minor_breaks=seq(50,105,5), breaks=seq(50,100,10)) +
  geom_segment(data= mean_lakeside, aes(y=mean_score, yend=mean_score, x=-1.25, xend=-.75), linewidth=1, color ="black") +
  geom_segment(data= mean_shp, aes(y=mean_score, yend=mean_score, x=-.25, xend=.25), linewidth=1, color ="#8D029E") +
  geom_segment(data= mean_el, aes(y=mean_score, yend=mean_score, x=.75, xend=1.25), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= mean_gg1, aes(y=mean_score, yend=mean_score, x=1.75, xend=2.25), linewidth=1, color ="#139E02") +
  geom_segment(data= mean_sped, aes(y=mean_score, yend=mean_score, x=2.75, xend=3.25), linewidth=1, color ="#F8BE3D") +
  geom_hline(yintercept=80,linetype="dashed")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(x="Student Group", y="Career Self-Efficacy Score", 
       title="Lakeside Middle School Career Self Efficacy Scores by Student Group")

ggsave("../plots/Lakeside Middle School Career Self Efficacy Scores by Student Group.pdf")

##############################################################
# Lakeside Self-Efficacy Subscores- by Ethnicity                          
##############################################################

# Data Wrangling
#### This section groups Self Efficacy subscores by Ethnicity & finds the mean values by those groups
#### Adds labels to Ethnicity markers

mean_ACPS <- career_narm %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                       obs=n())                                          

career_lakeside <- subset(career_narm, School=="Lakeside Middle School")

career_lakeside_eth <- subset(career_lakeside, career_lakeside$Ethnicity!="AI" )

mean_lakeside <- career_lakeside %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                             obs=n())

mean_Eth <- career_lakeside_eth %>% group_by(Ethnicity) %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                                                 obs=n())
eth.labs <- c("Asian Students", "Black Students", "Hispanic Students", "Multiracial Students", "White Students")
names(eth.labs) <- c("AS", "BL", "HL", "MR", "WH")

# Plot Creation
#### Plots Lakeside's self efficacy subscores by ethnicity with mean value and individual scores

ggplot(data=career_lakeside_eth, aes(y=SE_subscore)) + 
  geom_point(aes(x=0, color=Ethnicity), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_Eth, aes(yintercept = mean_score,col=Ethnicity), linewidth=1) +
  labs(x="Ethnicity", y="Self-Efficacy Subscore", 
       title="Lakeside Middle School Self Efficacy Subscores by Ethnicity")+
  geom_hline(yintercept=42,linetype="dashed")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(0,55), minor_breaks=seq(0,55,5), breaks=seq(0,50,10))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~Ethnicity, labeller= labeller(Ethnicity=eth.labs))+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave("../plots/Lakeside Middle School SE Subscores by Ethnicity.pdf")

##############################################################
# Lakeside Self-Efficacy Subscores- by Student Group                        
##############################################################

# Data Wrangling
#### This section pulls each Student Group into a subset dataframe to analyze them separately in ggplot
#### Includes calculations of mean values for Self Efficacy subscores for ggplot lines for each School
#### Using this method rather than grouping by variables so that students in more than one category can be multiply counted

mean_lakeside <- career_lakeside %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                             obs=n())
career_shp <- subset(career_lakeside, SHP=="SHP")
mean_shp <- career_shp %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                     obs=n())
career_avid <- subset(career_lakeside, AVID=="AVID")
mean_avid <- career_avid %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                       obs=n())
career_el <- subset(career_lakeside, Current_EL=="EL")
mean_el <- career_el %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                   obs=n())
career_gg1 <- subset(career_lakeside, GG1=="GG1")
mean_gg1 <- career_gg1 %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                     obs=n())
career_sped <- subset(career_lakeside, SPED=="SPED")
mean_sped <- career_sped %>% summarise(mean_score=mean(SE_subscore, na.rm=TRUE),
                                       obs=n())

# Plot Creation
#### Plot of Lakeside's  Self Efficacy subscores for student groups of interest 

ggplot() + 
  geom_point(data=career_lakeside, aes(y=SE_subscore, x=-1), position=position_jitter(0.1), alpha=.7, color="black") +
  geom_point(data=career_shp, aes(y=SE_subscore, x=0), position=position_jitter(0.1), alpha=.7, color="#8D029E") +
  geom_point(data=career_gg1, aes(y=SE_subscore, x=2), position=position_jitter(0.1), alpha=.7, color="#139E02") +
  geom_point(data=career_sped, aes(y=SE_subscore, x=3), position=position_jitter(0.1), alpha=.7, color="#F8BE3D") +
  scale_y_continuous(limits=c(0,55), minor_breaks=seq(0,55,5), breaks=seq(0,50,10)) +
  geom_segment(data= mean_lakeside, aes(y=mean_score, yend=mean_score, x=-1.25, xend=-.75), linewidth=1, color ="black") +
  geom_segment(data= mean_shp, aes(y=mean_score, yend=mean_score, x=-.25, xend=.25), linewidth=1, color ="#8D029E") +
  geom_segment(data= mean_gg1, aes(y=mean_score, yend=mean_score, x=1.75, xend=2.25), linewidth=1, color ="#139E02") +
  geom_segment(data= mean_sped, aes(y=mean_score, yend=mean_score, x=2.75, xend=3.25), linewidth=1, color ="#F8BE3D") +
  geom_hline(yintercept=42,linetype="dashed")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(x="Student Group", y="Self-Efficacy Subscore", 
       title="Lakeside Middle School Self Efficacy Subscores by Student Group")

ggsave("../plots/Lakeside Middle School SE Subscores by Student Group.pdf")

##############################################################
# Lakeside Outcome Expectancy Subscores- by Ethnicity                        
##############################################################

# Data Wrangling
#### This section groups Outcome Expectancy subscores by Ethnicity & finds the mean values by those groups
#### Adds labels to Ethnicity markers

mean_ACPS <- career_narm %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                       obs=n())                                          

career_lakeside <- subset(career_narm, School=="Lakeside Middle School")

career_lakeside_eth <- subset(career_lakeside, career_lakeside$Ethnicity!="AI" )

mean_lakeside <- career_lakeside %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                             obs=n())

mean_Eth <- career_lakeside_eth %>% group_by(Ethnicity) %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                                                 obs=n())
eth.labs <- c("Asian Students", "Black Students", "Hispanic Students", "Multiracial Students", "White Students")
names(eth.labs) <- c("AS", "BL", "HL", "MR", "WH")

# Plot Creation
#### Plots Lakeside's self efficacy subscores by ethnicity with mean value and individual scores

ggplot(data=career_lakeside_eth, aes(y=OE_subscore)) + 
  geom_point(aes(x=0, color=Ethnicity), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_Eth, aes(yintercept = mean_score,col=Ethnicity), linewidth=1) +
  labs(x="Ethnicity", y="Outcome Expectancy Subscore", 
       title="Lakeside Middle School Outcome Expectancy Subscores by Ethnicity")+
  geom_hline(yintercept=38,linetype="dashed")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(0,50), minor_breaks=seq(0,50,5), breaks=seq(0,50,10))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~Ethnicity, labeller= labeller(Ethnicity=eth.labs))+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggsave("../plots/Lakeside Middle School OE Subscores by Ethnicity.pdf")

##############################################################
# Lakeside Outcome Expectancy Subscores- by Student Group                        
##############################################################

# Data Wrangling
#### This section pulls each Student Group into a subset dataframe to analyze them separately in ggplot
#### Includes calculations of mean values for Outcome Expectancy subscores for ggplot lines for each School
#### Using this method rather than grouping by variables so that students in more than one category can be multiply counted

mean_lakeside <- career_lakeside %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                             obs=n())
career_shp <- subset(career_lakeside, SHP=="SHP")
mean_shp <- career_shp %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                     obs=n())
career_avid <- subset(career_lakeside, AVID=="AVID")
mean_avid <- career_avid %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                       obs=n())
career_el <- subset(career_lakeside, Current_EL=="EL")
mean_el <- career_el %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                   obs=n())
career_gg1 <- subset(career_lakeside, GG1=="GG1")
mean_gg1 <- career_gg1 %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                     obs=n())
career_sped <- subset(career_lakeside, SPED=="SPED")
mean_sped <- career_sped %>% summarise(mean_score=mean(OE_subscore, na.rm=TRUE),
                                       obs=n())

# Plot Creation
#### Plot of Lakeside's Outcome Expectancy subscores for student groups of interest 

ggplot() + 
  geom_point(data=career_lakeside, aes(y=OE_subscore, x=-1), position=position_jitter(0.1), alpha=.7, color="black") +
  geom_point(data=career_shp, aes(y=OE_subscore, x=0), position=position_jitter(0.1), alpha=.7, color="#8D029E") +
  geom_point(data=career_gg1, aes(y=OE_subscore, x=2), position=position_jitter(0.1), alpha=.7, color="#139E02") +
  geom_point(data=career_sped, aes(y=OE_subscore, x=3), position=position_jitter(0.1), alpha=.7, color="#F8BE3D") +
  scale_y_continuous(limits=c(0,50), minor_breaks=seq(0,50,5), breaks=seq(0,50,10)) +
  geom_segment(data= mean_lakeside, aes(y=mean_score, yend=mean_score, x=-1.25, xend=-.75), linewidth=1, color ="black") +
  geom_segment(data= mean_shp, aes(y=mean_score, yend=mean_score, x=-.25, xend=.25), linewidth=1, color ="#8D029E") +
  geom_segment(data= mean_gg1, aes(y=mean_score, yend=mean_score, x=1.75, xend=2.25), linewidth=1, color ="#139E02") +
  geom_segment(data= mean_sped, aes(y=mean_score, yend=mean_score, x=2.75, xend=3.25), linewidth=1, color ="#F8BE3D") +
  geom_hline(yintercept=38,linetype="dashed")+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  labs(x="Student Group", y="Outcome Expectancy Subscore", 
       title="Lakeside Middle School Outcome Expectancy Subscores by Student Group")

ggsave("../plots/Lakeside Middle School OE Subscores by Student Group.pdf")
