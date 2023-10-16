library(ggplot2)
library(tidyverse)
library(dplyr)
library(incidence2)
library(lubridate)
library(pacman)
library(scales)
library(wesanderson) #colors for ggplot
library(ggrepel) #for geo_text_repel

setwd("~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Japan_COVID_Deaths")

#import the raw data with fukuoka and osaka prefectures incidence of covid from 2020-23 
rawdeaths <- read.csv("osaka_fukuoka_number_of_deaths_daily.csv") 
cumuldeaths <- read.csv("deaths_cumulative_daily_2020-23.csv")

#convert the character variable to date variable 
rawdeaths$date_onset <- ymd(rawdeaths$date_onset)
class(rawdeaths$date_onset)
#do the same thing for cumul death data 
cumuldeaths$date_onset <- ymd(cumuldeaths$Date)
class(cumuldeaths$date_onset)

# check range of onset dates
ggplot(data = rawdeaths)+
  geom_histogram(aes(x = date_onset))

# # create the incidence object, aggregating cases by week
# monthly_covid_jp <- rawdeaths %>% 
#   incidence(       # create incidence object
#   date_index = "date_onset",  # date column 
#   counts = c("all_deaths"),
#   interval = "weekly"
#   )
# 
# plot(monthly_covid_jp)
# 
# max(rawdeaths$all_deaths)
# max(rawdeaths$osaka_deaths)
# max(rawdeaths$fukuoka_deaths)

#convert from wide to long format 
rawdeaths_long <- gather(rawdeaths, region, deaths, 
                            all_deaths:fukuoka_deaths, factor_key=TRUE) 


# #create the deaths grouped by region now 
# monthly_covid_jp_region <- rawdeaths_long %>%
#   incidence(
#     date_index = "date_onset", 
#     groups = "region", 
#     counts = c("deaths"),
#     interval = "weekly"
#   )

# ggplot(monthly_covid_jp_region, 
#        aes(x = date_index, 
#            y = count, 
#            fill = region)) + 
#   geom_bar(stat="identity") #identity = stacked bar chart

#plot(monthly_covid_jp_region)

# monthly_breaks <- seq.Date(from = as.Date("2020-01-16"),
#                            to = as.Date("2023-03-08"),
#                            by = "months")
# monthly_breaks   # print

#filter the dates from Jan 2021 to end of Feb 2023 
rawdeaths_long_trunc <- rawdeaths_long %>% filter(date_onset >= "2020-12-01" &
                                                          date_onset < "2023-03-01")

#for the cumulative data dataset, do the same by filtering the dates 
cumuldeaths_long_trunc <- cumuldeaths %>% filter(date_onset >= "2020-12-01" &
                                                         date_onset < "2023-03-01")

#subset to just the columns that indicate all the national cumulative incidence AND the dates
cumuldeaths_long_trunc <- cumuldeaths_long_trunc[,c(2,50)]
#rename the column name with the cumulative incidence 
colnames(cumuldeaths_long_trunc)[colnames(cumuldeaths_long_trunc) == "ALL"] <- "national_cumdeaths"

#merge the two together in the same data frame 

rawdeaths_long_trunc <- merge(rawdeaths_long_trunc, cumuldeaths_long_trunc, by="date_onset")


#create dataframe with the start/end dates for the emergency and semi-emergency declarations in Fukuoka/Osaka 
startdate <- as.Date(c("2021-01-12", "2021-04-24", "2021-08-02", "2022-01-27"))
enddate   <- as.Date(c("2021-02-28", "2021-06-19", "2021-09-29", "2022-03-21"))
emergstatus    <- c("yes", "yes", "yes", "semi")

#save the above in a dataframe                                                
emerg_timeline <- data.frame(startdate, enddate, emergstatus)

#set the color for the rectangles depending on status of emergency declaration 
emerg_timeline$col <- ifelse(emerg_timeline$emergstatus=="yes", "hotpink", "lightpink2")

# #create dataframe with the introduction of covid variants 
# variant_date <- as.Date(c("2021-03-04", "2021-07-11", "2022-01-30", 
#                           "2022-04-24", "2022-06-26"))
# 
# variant_type <- c("Alpha", "Delta", "BA.1 (Omicron)", "BA.2 (Omicron)", "BA.5 (Omicron)")
# variant_timeline <- data.frame(variant_date, variant_type)
# 
# variant_timeline$ylim <- 400  #250000 #max(rawincidence_long_trunc$cases)
# variant_timeline$ybot <- 350 # 225000
# ylim <- variant_timeline$ylim[1]

#Summer Olympic event in the timeline to highlight 
event_date <- as.Date("2021-07-23")
event_type <- c("Olympics")
event_timeline <- data.frame(event_date, event_type)
event_timeline$ylim <- 400 #350 
event_timeline$ybot <- 350 #300  
ylim2 <- event_timeline$ylim[1]

#holidays during the year (Golden Week, End of Year)
#Golden week starts 2021-04-29, 2022-05-03
#Obon starts 2021-08-13, 2022-08-13
#End of Year/New Year starts 2021-12-29, 2022-12-29
holiday_date <- as.Date(c("2021-04-29", "2022-05-03", 
                          "2021-08-13", "2022-08-13",
                          "2021-12-29", "2022-12-29"))
holiday_type <- c("Golden Week", "Obon", "New Year")
holiday_timeline <- data.frame(holiday_date, holiday_type)
holiday_timeline$ylim <- 400 #350
holiday_timeline$ybot <- 350 #300
ylim3 <- holiday_timeline$ylim[1]


#vaccination campaign dates 

vax_date <- as.Date(c("2021-02-17", "2021-05-17", "2021-06-21"))
vaxcampaign_type <- c("healthcare worker", "65+ yr olds", "unis and workplace")
vax_timeline <- data.frame(vax_date, vaxcampaign_type)
vax_timeline$ylim <- 350 #300 
vax_timeline$ybot <- 300 #250
ylim4 <- vax_timeline$ylim[1]


#--------------- Overlay the daily # of deaths and national cumulative deaths together on one plot ------------------------------

max(rawdeaths_long_trunc$national_cumdeaths)/max(rawdeaths_long_trunc$deaths) #figure out how much the cumul deaths is bigger 
# than the daily death #s so you can figure out the scale when plotting the cumul deaths. Have approx this # as the coef

coeff <- 160

#plot the epi curve with the emergency declarations, survey time points and times when variants were introduced
deathcurve <- ggplot(rawdeaths_long_trunc, 
      aes(x = date_onset, 
          y = deaths, 
          fill = region)) + #fill makes the bar graph stacked by region 
  geom_area(position = "identity") +
  #geom_col(position = "identity") +
  #geom_col() + #this is the same as the code: geom_bar(stat="identity")
  #b/c you want to graph the actual values of cases and not the counts

  geom_line(aes(x = date_onset,
                y = national_cumdeaths/coeff),
            color = "red") +
  
  scale_y_continuous(labels = label_comma(),
                     breaks = seq(0,600,100),
                     name = "Daily Reported Deaths due to COVID-19",
                     sec.axis = sec_axis(
                       trans  = ~. * coeff, # here  you multiply back the coeff to make the second axis with the correct #s of
                       # the cumulative deaths 
                       name   = "National Cumulative Deaths of COVID-19")) +
    xlab("Date") +
  #ylab("Daily Reported Deaths due to COVID-19") +
  
  scale_fill_manual(labels = c("All of Japan", "Osaka", "Fukuoka"), 
                    name = "Region of \nJapan", 
                    values = wes_palette(n=3, name="FantasticFox1")) +
  # scale_fill_brewer(labels = c("National", "Osaka", "Fukuoka"), 
  #                   name = "Region of \n Japan", 
  #                   palette = "PuBuGn") +
  # scale_fill_discrete(labels = c("National", "Osaka", "Fukuoka"), 
  #                     name = "Region of \n Japan") +
  scale_x_date(date_breaks = "1 month",
               date_minor_breaks = "1 month",
               date_labels = "%Y-%m",
               expand = c(0,1)) +  #this sets the min and max value of the x-axis (in dates)
               # guide = guide_axis(n.dodge = 1)) +
  # scale_y_continuous(labels = label_comma(), #shows the actual number on y-axis instead of 
  #                    #abbreviated scientific notation 
  #                    breaks = seq(0, 600, 100)) + #indicating where the tick marks go
  
  geom_vline(xintercept = as.numeric(as.Date(c("2021-02-15", "2021-03-15", 
                                               "2021-05-15", "2021-11-15", 
                                               "2021-12-15", "2022-01-15", 
                                               "2022-02-15", "2022-03-15",
                                               "2022-12-15", "2023-02-15"))),
             linetype = "dashed") +
  
  
  geom_segment(data=event_timeline, inherit.aes = FALSE,
               aes(x = event_date, y = ylim2, xend = event_date, yend = ybot), color = "purple",
               arrow = arrow(length = unit(0.2, "cm")), show.legend = F, linewidth =1) +
  
  # geom_text(data = event_timeline, inherit.aes = FALSE, 
  #           aes(x = event_date+10, y=ylim+28000, label = event_type, angle=90)) +

  geom_segment(data=holiday_timeline, inherit.aes = FALSE,
               aes(x = holiday_date, y = ylim3, xend = holiday_date, yend = ybot), color = "navy",
               arrow = arrow(length = unit(0.2, "cm")), show.legend = F, linewidth =1) +
  
  geom_segment(data=vax_timeline, inherit.aes = FALSE,
               aes(x = vax_date, y = ylim4, xend = vax_date, yend = ybot), color = "green",
               arrow = arrow(length = unit(0.2, "cm")), show.legend = F, linewidth =1) +
  
  geom_rect(data = emerg_timeline, inherit.aes = FALSE,
            #when inherit.aes is false, it overrides the default aesthetics rather than
            #combining them. So this doesn't try to combine with the initial aes that I 
            #indicated in the initial call to ggplot. 
            aes(xmin = startdate, #where the rectangle starts on the x-axis
                xmax = enddate,   #where the rectangle ends on the x-axis
                ymin = -Inf,  #rectangle continues to neg infinity
                ymax = +Inf), #rectangel continues to pos infinity 
                fill = emerg_timeline$col, alpha = 0.2) + 
            #fill = indicates the color with a condition based on variable "col"
            #alpha makes it go behind and the # indicates transparency
            theme_bw() + #makes background white 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y.right = element_text(angle = 90))

print(deathcurve)


#save the plot in PDF  
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
filename = "Fig1b_DeathCurve_COVID19_timeline_overlay_revised.pdf", deathcurve, width=8,height=6, units = "in")



