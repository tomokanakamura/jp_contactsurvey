library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(broom)
library(tidyr)

# Import dataset 
setwd("~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/R script for sharing/RDS")

totalctc <- readRDS("Frequency_Contacts_byAge_Weekday_Weekend_2021-2023.RDS")

#-----------------------------------------------------------------------------------------------------------------------
# Truncate the weekday and weekend contacts at 250
weekday_sum <- totalctc[,c("survey_timept", "age.cat", "grandtotal_contacts_wkday")]

weekday_sum$survey_timept <- factor(weekday_sum$survey_timept, 
                                    levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                               "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                               "Dec2022", "Feb2023"))

weekday_sum$weekday_trunc <- ifelse(weekday_sum$grandtotal_contacts_wkday >= 250, 250, weekday_sum$grandtotal_contacts_wkday)

#Truncate the weekend contacts at 250 as well 
weekend_sum <- totalctc[,c("survey_timept", "age.cat", "grandtotal_contacts_wkend")]

weekend_sum$survey_timept <- factor(weekend_sum$survey_timept, 
                                    levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                               "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                               "Dec2022", "Feb2023"))

weekend_sum$weekend_trunc <- ifelse(weekend_sum$grandtotal_contacts_wkend >= 250, 250, weekend_sum$grandtotal_contacts_wkend)
#-----------------------------------------------------------------------------------------------------------------------

# Loop below to get the bootstrapped mean contacts with their bootstrapped 95% CIs 
# (TRUNCATED weekday contacts at 250) 

set.seed(878755) #set the seed for randomizing numbers prep for bootstrap 
length(unique(weekday_sum$age.cat)) #this gets the # of unique values of age cats

length(unique(weekday_sum$survey_timept)) #this gets the # of unique values of survey time points

weekday_sum$survey_timept <- factor(weekday_sum$survey_timept, 
                                    levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                               "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                               "Dec2022", "Feb2023"))

#create an empty matrix with rows as survey time points and columns as the age categories 
meanmatrix2   <- matrix(NA, nrow = length(unique(weekday_sum$survey_timept)), ncol = length(unique(weekday_sum$age.cat)))
lowcimatrix2  <- matrix(NA, nrow = length(unique(weekday_sum$survey_timept)), ncol = length(unique(weekday_sum$age.cat)))
highcimatrix2 <- matrix(NA, nrow = length(unique(weekday_sum$survey_timept)), ncol = length(unique(weekday_sum$age.cat)))


TimePt_list <- unique(weekday_sum$survey_timept) #lists the unique time points of the survey in a vector 

AgeCat_list <- unique(weekday_sum$age.cat) #lists the unique age categories in each survey in a vector

AgeCat_list <-  factor(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+"),
                       levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+"))

#create a dataframe that takes all the contacts from the weekday_sum df and grouped by the survey time points and age cat
test <- weekday_sum %>% group_by(survey_timept, age.cat)

for (i in 1:length(unique(weekday_sum$survey_timept))) { #indicate the row = survey time point in matrix 
  for (j in 1:length(unique(weekday_sum$age.cat))) { #indicate the column = age cat in matrix 
    
    # I am now in the loop at a place that is only aware of a single time and a single age category.
    # Here I want to have all contacts (from entire dataset) for that point in time and that age category
    # I will now do the bootstrapping on that subset of the full data
    # this gives me statistical values (mean), that I then save in the matrix (temporarily named testmatrix)
    TimeAgeSum      <- test %>% filter(survey_timept==TimePt_list[i] & age.cat==AgeCat_list[j]) 
    
    bootstrap       <- lapply(1:1000, function(i) sample(TimeAgeSum$weekday_trunc, replace = T))
    
    bootstrap.mean  <- mean(sapply(bootstrap, mean))
    
    meanmatrix2[i,j] <- bootstrap.mean
    
    bootstrap.dist  <- sapply(bootstrap, mean)
    
    q <- quantile(bootstrap.dist, c(0.025, 0.975))
    
    lowcimatrix2[i,j]  <- q[1]
    
    highcimatrix2[i,j] <- q[2]
    
    
  }
  
} 

print(meanmatrix2)
print(lowcimatrix2)
print(highcimatrix2)

#-----------------------------------------------------------------------------------------------------------------------
# Prepare to create bar plots for the bootstrapped mean weekday contacts (TRUNCATED) with their 
# bootstrapped 95% CI's
#-----------------------------------------------------------------------------------------------------------------------
meanmatrix2 <- as.data.frame(meanmatrix2)
#relabel the column names 
colnames(meanmatrix2) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+")
#create a new variable with a column of all the survey time points 
meanmatrix2$survey_timept <- c("Feb2021", "Mar2021", "May2021", 
                               "Nov2021", "Dec2021", "Jan2022", 
                               "Feb2022", "Mar2022", "Dec2022", 
                               "Feb2023")
#reorder the columns
meanmatrix2 <- meanmatrix2 %>% select(survey_timept, everything())                          
#reshape the data frame from wide to long 
meanmatrix2_long <- gather(meanmatrix2, age.cat, meancontact, `0-9`:`70+`)

#do the same for the bootstrapped 95% CIs
lowcimatrix2 <- as.data.frame(lowcimatrix2)
highcimatrix2 <- as.data.frame(highcimatrix2)

#relabel the column names 
colnames(lowcimatrix2) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+")
#create a new variable with a column of all the survey time points 
lowcimatrix2$survey_timept <- c("Feb2021", "Mar2021", "May2021", 
                                "Nov2021", "Dec2021", "Jan2022", 
                                "Feb2022", "Mar2022", "Dec2022", 
                                "Feb2023")
#reorder the columns
lowcimatrix2 <- lowcimatrix2 %>% select(survey_timept, everything())  
#reshape the data frame from wide to long 
lowcimatrix2_long <- gather(lowcimatrix2, age.cat, lowci, `0-9`:`70+`)

colnames(highcimatrix2) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+")
#create a new variable with a column of all the survey time points 
highcimatrix2$survey_timept <- c("Feb2021", "Mar2021", "May2021", 
                                 "Nov2021", "Dec2021", "Jan2022", 
                                 "Feb2022", "Mar2022", "Dec2022",
                                 "Feb2023")
#reorder the columns
highcimatrix2 <- highcimatrix2 %>% select(survey_timept, everything())  
#reshape the data frame from wide to long 
highcimatrix2_long <- gather(highcimatrix2, age.cat, highci, `0-9`:`70+`)

#now merge all together with the bootstrapped means and the 95% CIs

bootstrap_wkday2 <- meanmatrix2_long
bootstrap_wkday2 <- merge(bootstrap_wkday2, lowcimatrix2_long, by=c("survey_timept", "age.cat"))
bootstrap_wkday2 <- merge(bootstrap_wkday2, highcimatrix2_long, by=c("survey_timept", "age.cat"))

bootstrap_wkday2$survey_timept <- factor(bootstrap_wkday2$survey_timept, 
                                         levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                                    "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                                    "Dec2022", "Feb2023"))

#plot the TRUNCATED weekday mean contacts that were bootstrapped 
weekday_bootstr_trunc_plot <- ggplot(bootstrap_wkday2, aes(x= age.cat, y= meancontact, fill= survey_timept)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = lowci,
                    #gets the error bar to be at 0 and not negative 
                    #ifelse(test_expression, x, y) where if test_expression is true 
                    #it will follow x but if it's false, it will follow y 
                    ymax = highci), width=.9, position = position_dodge()) +
  theme_bw() + #makes background color as white 
  theme(axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10, face = "bold", color = "black"),
        axis.title.y = element_text(size = 10, face = "bold", color = "black"), 
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_fill_viridis_d(labels = c("Feb 2021", "Mar 2021", "May 2021",
                                  "Nov 2021", "Dec 2021", "Jan 2022", 
                                  "Feb 2022", "Mar 2022", "Dec 2022",
                                  "Feb 2023")) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow=2, byrow = TRUE)) +
  xlab("Age Category of Participants") +
  ylab("Mean Number of Weekday Contacts") +
  ylim(0,25)

print(weekday_bootstr_trunc_plot)
#save the plot in PDF  
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "Fig3a_Weekday_Contacts_Feb2021-Feb2023_Bootstrapped.pdf", weekday_bootstr_trunc_plot, width=8,height=6, units = "in")


#------------------------------------------------------------------------------------------------------------
#-------Weekend Contacts Analysis with Bootstrapped Means and 95% CIs----------------------------------------
#-------with TRUNCATED contacts at 250 (loop below to get the bootstrapped contacts and CIs)-----------------
#------------------------------------------------------------------------------------------------------------


set.seed(6598988) #set the seed for randomizing numbers prep for bootstrap 
length(unique(weekend_sum$age.cat)) #this gets the # of unique values of age cats

length(unique(weekend_sum$survey_timept)) #this gets the # of unique values of survey time points

weekend_sum$survey_timept <- factor(weekend_sum$survey_timept, 
                                    levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                               "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                               "Dec2022", "Feb2023"))

#create an empty matrix with rows as survey time points and columns as the age categories 
meanmatrix2   <- matrix(NA, nrow = length(unique(weekend_sum$survey_timept)), ncol = length(unique(weekend_sum$age.cat)))
lowcimatrix2  <- matrix(NA, nrow = length(unique(weekend_sum$survey_timept)), ncol = length(unique(weekend_sum$age.cat)))
highcimatrix2 <- matrix(NA, nrow = length(unique(weekend_sum$survey_timept)), ncol = length(unique(weekend_sum$age.cat)))


TimePt_list <- unique(weekend_sum$survey_timept) #lists the unique time points of the survey in a vector 

AgeCat_list <- unique(weekend_sum$age.cat) #lists the unique age categories in each survey in a vector

AgeCat_list <-  factor(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+"),
                       levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+"))

#create a dataframe that takes all the contacts from the weekday_sum df and grouped by the survey time points and age cat
test <- weekend_sum %>% group_by(survey_timept, age.cat)

for (i in 1:length(unique(weekend_sum$survey_timept))) { #indicate the row = survey time point in matrix 
  for (j in 1:length(unique(weekend_sum$age.cat))) { #indicate the column = age cat in matrix 
    
    # I am now in the loop at a place that is only aware of a single time and a single age category.
    # Here I want to have all contacts (from entire dataset) for that point in time and that age category
    # I will now do the bootstrapping on that subset of the full data
    # this gives me statistical values (mean), that I then save in the matrix (temporarily named testmatrix)
    TimeAgeSum      <- test %>% filter(survey_timept==TimePt_list[i] & age.cat==AgeCat_list[j]) 
    
    bootstrap       <- lapply(1:1000, function(i) sample(TimeAgeSum$weekend_trunc, replace = T))
    
    bootstrap.mean  <- mean(sapply(bootstrap, mean))
    
    meanmatrix2[i,j] <- bootstrap.mean
    
    bootstrap.dist  <- sapply(bootstrap, mean)
    
    q <- quantile(bootstrap.dist, c(0.025, 0.975))
    
    lowcimatrix2[i,j]  <- q[1]
    
    highcimatrix2[i,j] <- q[2]
    
    
  }
  
} 

print(meanmatrix2)
print(lowcimatrix2)
print(highcimatrix2)


#----------------------------------------------------------------------------------------------------------------------
#Prepare to create bar plots for the bootstrapped mean weekend contacts (TRUNCATED) with their bootstrapped 95% CI's
#----------------------------------------------------------------------------------------------------------------------

meanmatrix2 <- as.data.frame(meanmatrix2)
#relabel the column names 
colnames(meanmatrix2) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+")
#create a new variable with a column of all the survey time points 
meanmatrix2$survey_timept <- c("Feb2021", "Mar2021", "May2021", 
                               "Nov2021", "Dec2021", "Jan2022", 
                               "Feb2022", "Mar2022", "Dec2022", 
                               "Feb2023")
#reorder the columns
meanmatrix2 <- meanmatrix2 %>% select(survey_timept, everything())                          
#reshape the data frame from wide to long 
meanmatrix2_long <- gather(meanmatrix2, age.cat, meancontact, `0-9`:`70+`)

#do the same for the bootstrapped 95% CIs
lowcimatrix2 <- as.data.frame(lowcimatrix2)
highcimatrix2 <- as.data.frame(highcimatrix2)

#relabel the column names 
colnames(lowcimatrix2) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+")
#create a new variable with a column of all the survey time points 
lowcimatrix2$survey_timept <- c("Feb2021", "Mar2021", "May2021", 
                                "Nov2021", "Dec2021", "Jan2022", 
                                "Feb2022", "Mar2022", "Dec2022",
                                "Feb2023")
#reorder the columns
lowcimatrix2 <- lowcimatrix2 %>% select(survey_timept, everything())  
#reshape the data frame from wide to long 
lowcimatrix2_long <- gather(lowcimatrix2, age.cat, lowci, `0-9`:`70+`)

colnames(highcimatrix2) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+")
#create a new variable with a column of all the survey time points 
highcimatrix2$survey_timept <- c("Feb2021", "Mar2021", "May2021", 
                                 "Nov2021", "Dec2021", "Jan2022", 
                                 "Feb2022", "Mar2022", "Dec2022",
                                 "Feb2023")
#reorder the columns
highcimatrix2 <- highcimatrix2 %>% select(survey_timept, everything())  
#reshape the data frame from wide to long 
highcimatrix2_long <- gather(highcimatrix2, age.cat, highci, `0-9`:`70+`)

#now merge all together with the bootstrapped means and the 95% CIs

bootstrap_wkend2 <- meanmatrix2_long
bootstrap_wkend2 <- merge(bootstrap_wkend2, lowcimatrix2_long, by=c("survey_timept", "age.cat"))
bootstrap_wkend2 <- merge(bootstrap_wkend2, highcimatrix2_long, by=c("survey_timept", "age.cat"))

bootstrap_wkend2$survey_timept <- factor(bootstrap_wkend2$survey_timept, 
                                         levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                                    "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                                    "Dec2022", "Feb2023"))

#plot the TRUNCATED weekend mean contacts that were bootstrapped 
weekend_bootstr_trunc_plot <- ggplot(bootstrap_wkend2, aes(x= age.cat, y= meancontact, fill= survey_timept)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = lowci,
                    #gets the error bar to be at 0 and not negative 
                    #ifelse(test_expression, x, y) where if test_expression is true 
                    #it will follow x but if it's false, it will follow y 
                    ymax = highci), width=.9, position = position_dodge()) +
  theme_bw() + #makes background color as white 
  theme(axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10, face = "bold", color = "black"),
        axis.title.y = element_text(size = 10, face = "bold", color = "black"), 
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_fill_viridis_d(labels = c("Feb 2021", "Mar 2021", "May 2021",
                                  "Nov 2021", "Dec 2021", "Jan 2022", 
                                  "Feb 2022", "Mar 2022", "Dec 2022", 
                                  "Feb 2023")) +
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  guides(fill = guide_legend(nrow=2, byrow = TRUE)) +
  xlab("Age Category of Participants") +
  ylab("Mean Number of Weekend Contacts") +
  ylim(0,25)

print(weekend_bootstr_trunc_plot)

#save the plot in PDF  
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "Fig3b_Weekend_Contacts_Feb2021-Feb2023_Bootstrapped.pdf", weekend_bootstr_trunc_plot, width=8,height=6, units = "in")

