library(readr)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(patchwork)

# Import dataset 
setwd("~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/R script for sharing/RDS")

totalctc <- readRDS("Duration_Contacts_byAge_Weekday_Weekend_2021-2023.RDS")

#----------------------------------------------------------------------------------------#
#3. Calculate the duration of contacts using a loop to bootstrap the mean duration of 
#contacts and bootstrapped 95% CIs 
#----------------------------------------------------------------------------------------#

set.seed(698863) #set the seed for randomizing numbers prep for bootstrap 

duration_wkday <- totalctc[,c("survey_timept", "age.cat", "totalduration_wkday")] #subset the original df with 
#only the necessary variables to analyze contact duration 

length(unique(duration_wkday$age.cat)) #this gets the # of unique values of age cats

length(unique(duration_wkday$survey_timept)) #this gets the # of unique values of survey time points

duration_wkday$survey_timept <- factor(duration_wkday$survey_timept, 
                                       levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                                  "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                                  "Dec2022", "Feb2023"))

#create an empty matrix with rows as name of prefecture and columns as the age categories 
meanmatrix2   <- matrix(NA, nrow = length(unique(duration_wkday$survey_timept)), ncol = length(unique(duration_wkday$age.cat)))
lowcimatrix2  <- matrix(NA, nrow = length(unique(duration_wkday$survey_timept)), ncol = length(unique(duration_wkday$age.cat)))
highcimatrix2 <- matrix(NA, nrow = length(unique(duration_wkday$survey_timept)), ncol = length(unique(duration_wkday$age.cat)))


AgeCat_list <- unique(duration_wkday$survey_timept) #lists the unique age categories in each survey in a vector

AgeCat_list <-  factor(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+"),
                       levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+"))

TimePt_list <- unique(duration_wkday$survey_timept)

#create a dataframe that takes the total contact duration from the duration_wkday df and grouped by the prefecture and age cat
test <- duration_wkday %>% group_by(survey_timept, age.cat)

for (i in 1:length(unique(duration_wkday$survey_timept))) { #indicate the row = name of prefecture in matrix 
  for (j in 1:length(unique(duration_wkday$age.cat))) { #indicate the column = age cat in matrix 
    
    # I am now in the loop at a place that is only aware of a single prefecture and a single age category.
    # Here I want to have all contacts (from entire dataset) for that specific prefecture and a single age category
    # and then loop it so I will have the mean contacts across all age categories and all 3 prefectures.
    # I will now do the bootstrapping on that subset of the full data
    # this gives me statistical values (mean), that I then save in the matrix (temporarily named testmatrix)
    
    TimeAgeSum      <- test %>% filter(survey_timept==TimePt_list[i] & age.cat==AgeCat_list[j])
    
    bootstrap       <- lapply(1:1000, function(i) sample(TimeAgeSum$totalduration_wkday, replace = T))
    
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

#----------------------------------------------------------------------------------------#
#4. Now plot the weekday duration of contacts with bootstrapped 95% CIs
#----------------------------------------------------------------------------------------#
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
bootstrap_feb23_duration <- meanmatrix2_long
bootstrap_feb23_duration <- merge(bootstrap_feb23_duration, lowcimatrix2_long, by=c("survey_timept", "age.cat"))
bootstrap_feb23_duration <- merge(bootstrap_feb23_duration, highcimatrix2_long, by=c("survey_timept", "age.cat"))

bootstrap_feb23_duration$survey_timept <- factor(bootstrap_feb23_duration$survey_timept, 
                                                 levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                                            "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                                            "Dec2022", "Feb2023"))


#plot the weekday mean contact duration (in hours) with bootstrapped 95% CIs 
bootstrap_feb23_duration_wkday_plot <- ggplot(bootstrap_feb23_duration, aes(x= age.cat, y= meancontact, fill= survey_timept)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = lowci,
                    ymax = highci), 
                width=.9, 
                position = position_dodge()) +
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
  xlab("Age Category of Participants") +
  ylab("Mean Hours of Weekday Contacts in Fukuoka and Osaka") +
  ylim(0,20)

print(bootstrap_feb23_duration_wkday_plot)

#save the plot 

path <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey"
newfolder <- "Paper_Plots"
dir.create(file.path(path, newfolder))
path <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"

ggsave(filename = file.path(path, "jpmix_durationcontacts_weekday_fig4a.pdf"))


#----------------------------------------------------------------------------------------------------
#-----------------------------WEEKEND DURATION OF CONTACTS-------------------------------------------
#----------------------------------------------------------------------------------------------------

# Calculate the weekend duration of contacts with bootstrapped 95% CIs

set.seed(698863) #set the seed for randomizing numbers prep for bootstrap 

duration_wkend <- totalctc[,c("survey_timept", "age.cat", "totalduration_wkend")] #subset the original df with 
#only the necessary variables to analyze contact duration 

length(unique(duration_wkend$age.cat)) #this gets the # of unique values of age cats

length(unique(duration_wkend$survey_timept)) #this gets the # of unique values of survey time points

duration_wkend$survey_timept <- factor(duration_wkend$survey_timept, 
                                       levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                                  "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                                  "Dec2022", "Feb2023"))

#create an empty matrix with rows as name of prefecture and columns as the age categories 
meanmatrix2   <- matrix(NA, nrow = length(unique(duration_wkend$survey_timept)), ncol = length(unique(duration_wkend$age.cat)))
lowcimatrix2  <- matrix(NA, nrow = length(unique(duration_wkend$survey_timept)), ncol = length(unique(duration_wkend$age.cat)))
highcimatrix2 <- matrix(NA, nrow = length(unique(duration_wkend$survey_timept)), ncol = length(unique(duration_wkend$age.cat)))


AgeCat_list <- unique(duration_wkend$survey_timept) #lists the unique age categories in each survey in a vector

AgeCat_list <-  factor(c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+"),
                       levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59","60-69", "70+"))

TimePt_list <- unique(duration_wkend$survey_timept)

#create a dataframe that takes the total contact duration from the duration_wkday df and grouped by the prefecture and age cat
test <- duration_wkend %>% group_by(survey_timept, age.cat)

for (i in 1:length(unique(duration_wkend$survey_timept))) { #indicate the row = name of prefecture in matrix 
  for (j in 1:length(unique(duration_wkend$age.cat))) { #indicate the column = age cat in matrix 
    
    # I am now in the loop at a place that is only aware of a single prefecture and a single age category.
    # Here I want to have all contacts (from entire dataset) for that specific prefecture and a single age category
    # and then loop it so I will have the mean contacts across all age categories and all 3 prefectures.
    # I will now do the bootstrapping on that subset of the full data
    # this gives me statistical values (mean), that I then save in the matrix (temporarily named testmatrix)
    
    TimeAgeSum      <- test %>% filter(survey_timept==TimePt_list[i] & age.cat==AgeCat_list[j])
    
    bootstrap       <- lapply(1:1000, function(i) sample(TimeAgeSum$totalduration_wkend, replace = T))
    
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



#----------------------------------------------------------------------------------------#
# Now plot the weekend duration of contacts with bootstrapped 95% CIs
#----------------------------------------------------------------------------------------#
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
bootstrap_feb23_duration_wkend <- meanmatrix2_long
bootstrap_feb23_duration_wkend <- merge(bootstrap_feb23_duration_wkend, lowcimatrix2_long, by=c("survey_timept", "age.cat"))
bootstrap_feb23_duration_wkend <- merge(bootstrap_feb23_duration_wkend, highcimatrix2_long, by=c("survey_timept", "age.cat"))

bootstrap_feb23_duration_wkend$survey_timept <- factor(bootstrap_feb23_duration_wkend$survey_timept, 
                                                       levels = c("Feb2021", "Mar2021", "May2021", "Nov2021",
                                                                  "Dec2021","Jan2022", "Feb2022", "Mar2022", 
                                                                  "Dec2022", "Feb2023"))


#plot the weekday mean contact duration (in hours) with bootstrapped 95% CIs 
bootstrap_feb23_duration_wkend_plot <- ggplot(bootstrap_feb23_duration_wkend, aes(x= age.cat, y= meancontact, fill= survey_timept)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = lowci,
                    ymax = highci), 
                width=.9, 
                position = position_dodge()) +
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
  xlab("Age Category of Participants") +
  ylab("Mean Hours of Weekend Contacts in Fukuoka and Osaka") +
  ylim(0,20)

print(bootstrap_feb23_duration_wkend_plot)

#save the plot 

path <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey"
newfolder <- "Paper_Plots"
dir.create(file.path(path, newfolder))
path <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"

ggsave(filename = file.path(path, "jpmix_durationcontacts_weekend_fig4a.pdf"))


