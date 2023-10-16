library(ggplot2)
library(tidyverse)
library(dplyr)
library(incidence2)
library(lubridate)
library(pacman)
library(scales)
library(wesanderson) #colors for ggplot
library(ggrepel) #for geo_text_repel

setwd("~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Japan_COVID_Incidence")

#import the raw data with fukuoka and osaka prefectures incidence of covid from 2020-23 
rawincidence <- read.csv("osaka_fukuoka_newly_confirmed_cases_daily_2020-23.csv") 
cumulincidence <- read.csv("confirmed_cases_cumulative_daily_2020-2023.csv")

#convert the character variable to date variable 
rawincidence$date_onset <- ymd(rawincidence$date_onset)
class(rawincidence$date_onset)
#do the same thing for cumulative incid data
cumulincidence$date_onset <- ymd(cumulincidence$Date)
class(cumulincidence$date_onset)

# check range of onset dates
ggplot(data = rawincidence)+
  geom_histogram(aes(x = date_onset))

# create the incidence object, aggregating cases by week
monthly_covid_jp <- rawincidence %>% 
  incidence(       # create incidence object
  date_index = "date_onset",  # date column 
  counts = c("all_cases"),
  interval = "weekly"
  )

plot(monthly_covid_jp)

max(rawincidence$all_cases)
max(rawincidence$osaka_cases)
max(rawincidence$fukuoka_cases)

#convert from wide to long format 
rawincidence_long <- gather(rawincidence, region, cases, 
                            all_cases:fukuoka_cases, factor_key=TRUE) 


#for the incidence dataset, filter the dates from Jan 2021 to end of Feb 2023 
rawincidence_long_trunc <- rawincidence_long %>% filter(date_onset >= "2020-12-01" &
                                                          date_onset < "2023-03-01")

#for the cumulative incidence dataset, do the same by filtering the dates 
cumulincidence_long_trunc <- cumulincidence %>% filter(date_onset >= "2020-12-01" &
                                                         date_onset < "2023-03-01")
#subset to just the columns that indicate all the national cumulative incidence AND the dates
cumulincidence_long_trunc <- cumulincidence_long_trunc[,c(2,50)]
#rename the column name with the cumulative incidence 
colnames(cumulincidence_long_trunc)[colnames(cumulincidence_long_trunc) == "ALL"] <- "national_cumincidence"

#merge the two together in the same data frame 

rawincidence_long_trunc <- merge(rawincidence_long_trunc, cumulincidence_long_trunc, by="date_onset")


#create dataframe with the start/end dates for the emergency and semi-emergency declarations in Fukuoka/Osaka 
startdate <- as.Date(c("2021-01-12", "2021-04-24", "2021-08-02", "2022-01-27"))
enddate   <- as.Date(c("2021-02-28", "2021-06-19", "2021-09-29", "2022-03-21"))
emergstatus    <- c("yes", "yes", "yes", "semi")

#save the above in a dataframe                                                
emerg_timeline <- data.frame(startdate, enddate, emergstatus)

#set the color for the rectangles depending on status of emergency declaration 
emerg_timeline$col <- ifelse(emerg_timeline$emergstatus=="yes", "hotpink", "lightpink2")

#create dataframe with the introduction of covid variants 
variant_date <- as.Date(c("2021-03-04", "2021-07-11", "2022-01-30", 
                          "2022-04-24", "2022-06-26"))

variant_type <- c("Alpha", "Delta", "BA.1 (Omicron)", "BA.2 (Omicron)", "BA.5 (Omicron)")
variant_timeline <- data.frame(variant_date, variant_type)

variant_timeline$ylim <- 200000  #250000 #max(rawincidence_long_trunc$cases)
variant_timeline$ybot <- 170000 # 225000
ylim <- variant_timeline$ylim[1]


# ----------------------------- OVERLAY TWO DIFFERENT GRAPHS ON TOP OF EACH OTHER -----------------------------
# The code below overlays the daily reported cases of COVID-19 and national cumulative cases of COVID-19 
# on top of each other with on the same graph 

#plot the epi curve with the emergency declarations, survey time points and times when variants were introduced
coeff <- 150 #divide  by max of cumulative cases by the maximum of the daily reported cases (33 M/0.25M)

epicurve <- ggplot(rawincidence_long_trunc, 
                   aes(x = date_onset, 
                       y = cases, 
                       fill = region)) + #fill makes the bar graph stacked by region 
  geom_area(position = "identity") +
  
  geom_line(aes(x = date_onset, 
                y = national_cumincidence/coeff), 
            color = "red") +
  
  scale_y_continuous(labels = label_comma(), #shows the actual number on y-axis instead of abbreviated scientific notation
                     breaks = seq(0, 300000, 50000),
                     name   = "Daily Reported Cases of COVID-19",
                     sec.axis = sec_axis(
                       trans  = ~. * coeff, # here  you multiply back the coeff to make the second axis with the correct #s of
                                            # the cumulative cases 
                       name   = "National Cumulative Cases of COVID-19")) +
  xlab("Year") +

  scale_fill_manual(labels = c("All of Japan", "Osaka", "Fukuoka"), 
                    name = "Region of \nJapan", 
                    values = wes_palette(n=3, name="FantasticFox1")) +

  scale_x_date(date_breaks = "1 month",
               date_minor_breaks = "1 month",
               date_labels = "%Y-%m",
               expand = c(0,1)) +  #this sets the min and max value of the x-axis (in dates)
  # guide = guide_axis(n.dodge = 1)) +
  
  geom_vline(xintercept = as.numeric(as.Date(c("2021-02-15", "2021-03-15", 
                                               "2021-05-15", "2021-11-15", 
                                               "2021-12-15", "2022-01-15", 
                                               "2022-02-15", "2022-03-15",
                                               "2022-12-15", "2023-02-15"))),
             linetype = "dashed") +
  
  geom_segment(data=variant_timeline, inherit.aes = FALSE,
               aes(x = variant_date, y = ylim, xend = variant_date, yend = ybot), color = "darkgreen",
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


print(epicurve)


#save the plot in PDF  
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "Fig1_EpiCurve_COVID19_timeline_overlay_revised.pdf", epicurve, width=8,height=6, units = "in")

