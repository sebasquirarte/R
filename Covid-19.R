# Based on YT - 'Using R to Analyze COVID-19 | R Programming Project' by Tech Tribe 
# url: https://www.youtube.com/watch?v=D_CNmYkGRUc&t=5s&ab_channel=TechTribe
# Sebastian Quirarte | 20 Jan 23 | sebastianquirajus@gmail.com


rm(list=ls()) #removes all variables stored previously
library(Hmisc) #import Hmisc library

data <- read.csv("~/Desktop/Proyectos Data/R/Covid-19/COVID19_line_list_data.csv")
describe(data) #Hmisc 'describe' command 

# cleaned up death column
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

# AGE
# Claim: People who die are older than those who survive 
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE) # 68.59
mean(alive$age, na.rm = TRUE) # 48.07
# Is this statistically significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# GENDER
# Claim: Gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) # 8.5%
mean(women$death_dummy, na.rm = TRUE) # 3.7%
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant