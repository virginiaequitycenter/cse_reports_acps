##############################################################
# ACPS Career Self-Efficacy - Change Data                        
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-01-13  
# Summary: A document that transforms CSE data from 2023 to 2025
#          to monitor change in 7th and 8th grade ACPS students
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
library(janitor)

##############################################################
# Pull in Self-Efficacy Data                    
##############################################################

career <- read.csv("Fall 2025/data/Fall 2023-25 Career Self Efficacy Deidentified Clean.csv") %>%
          select("unique_id","year_completed","grade_at_time_of_survey","class_year","SE_subscore",
                 "OE_subscore","total_score","gender","race_ethnicity","first_gen","parent_ed",
                 "disabilities","el","avid","starr_hill","school_sy2526") %>%
          drop_na(total_score)

##############################################################
# Data Wrangling- Widen Data                   
##############################################################

career <- career %>% mutate(class_year=ifelse(unique_id=="FD16656A",2030,class_year))

career <- career %>% pivot_wider(names_from = year_completed, 
                                 values_from = c(SE_subscore, OE_subscore, total_score,
                                                 grade_at_time_of_survey, first_gen, parent_ed))

career_check <- career %>% group_by(unique_id) %>% filter(n() > 1) %>% ungroup()

##############################################################
# Data Wrangling- Combine Columns                  
##############################################################

career_change <- career[rowSums(!is.na(career[, c("total_score_2023", "total_score_2024", "total_score_2025")])) >= 2, ]

career_change <- career_change %>% add_column(total_change_2024=career_change$total_score_2024-career_change$total_score_2023)
career_change <- career_change %>% add_column(total_change_2025=career_change$total_score_2025-career_change$total_score_2024)
career_change <- career_change %>% add_column(SE_change_2024=career_change$SE_subscore_2024-career_change$SE_subscore_2023)
career_change <- career_change %>% add_column(SE_change_2025=career_change$SE_subscore_2025-career_change$SE_subscore_2024)
career_change <- career_change %>% add_column(OE_change_2024=career_change$OE_subscore_2024-career_change$OE_subscore_2023)
career_change <- career_change %>% add_column(OE_change_2025=career_change$OE_subscore_2025-career_change$OE_subscore_2024)

career_change <- career_change %>% add_column(years="")
career_change <- career_change %>% mutate(years=ifelse(!is.na(total_change_2024),"2023-2024",years))
career_change <- career_change %>% mutate(years=ifelse(!is.na(total_change_2025),"2024-2025",years))

##############################################################
# Data Wrangling- Select Columns of Interest                  
##############################################################

career_change <- career_change %>% select(c("unique_id","class_year","gender","race_ethnicity","starr_hill",
                                            "disabilities","el","avid","school_sy2526","first_gen_2025",
                                            "parent_ed_2025","total_change_2024","total_change_2025",
                                            "SE_change_2024","SE_change_2025","OE_change_2024","OE_change_2025",
                                            "years"))

career_change <- career_change %>% 
                 pivot_longer(cols = c("total_change_2024","total_change_2025"),
                              values_to = "total_change",
                              names_to = "total", values_drop_na = TRUE)

career_change <- career_change %>% 
  pivot_longer(cols = c("SE_change_2024","SE_change_2025"),
               values_to = "SE_change",
               names_to = "total2", values_drop_na = TRUE)

career_change <- career_change %>% 
  pivot_longer(cols = c("OE_change_2024","OE_change_2025"),
               values_to = "OE_change",
               names_to = "total3",values_drop_na = TRUE)

career_change <- career_change %>% select(c("unique_id","class_year","gender","race_ethnicity","disabilities","el",
                                            "starr_hill","avid","school_sy2526","first_gen_2025","parent_ed_2025","years",
                                            "total_change","SE_change","OE_change"))

mean_change_years <- career_change %>% group_by(first_gen_2025) %>% summarise(total_mean_score=mean(total_change, na.rm=TRUE),
                                                               SE_mean_score=mean(SE_change, na.rm=TRUE),
                                                               OE_mean_score=mean(OE_change, na.rm=TRUE),
                                                               obs=n())



ggplot(data=career_change, aes(y=total_change)) + 
  geom_point(aes(x=0, color=first_gen_2025), position=position_jitter(0.1), alpha=.5) +
  geom_hline(data= mean_change_years, aes(yintercept = total_mean_score,col=first_gen_2025), linewidth=1) +
  labs(x="Years", y="Career Self-Efficacy Total Change", 
       title="ACPS School Career Self Efficacy Scores Total Change Over 1 Year")+
  geom_hline(yintercept=0,linetype="dashed")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_color_manual(values=c("#8D029E","#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~first_gen_2025)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")





                                                                               