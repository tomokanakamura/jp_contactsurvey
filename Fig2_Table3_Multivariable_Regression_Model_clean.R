
library(MASS)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(broom) #tidy up the regression coefficients to a dataframe, works w/ dplyr
library(survival)
library(patchwork)
library(plotly) #to make ggplot interactive 
library(rstatix) #provides pipe-friendly R functions for easy stats analyses 


setwd("~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/R script for sharing/RDS")

totalctc <- readRDS("Feb2023_Contacts_Multivariable_Regression.RDS")

#---------------------------------------------------------------------------------------
#univariate regression using weibull distribution for the above variables so far
#but have the outcome as the WEEKDAY contacts only first. 

summary(m0 <- survreg(Surv(weekday_trunc+0.01) ~ 1, data=totalctc, dist = "weibull"))  #null model 

#names(summary(m0))

#Individual, HH, and behavior/attitude characteristics of participants
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(noinHH_cat), data=totalctc, dist = "weibull")) #5 cats, sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(covidview), data=totalctc, dist = "weibull")) #5 cats, sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ sex, data=totalctc, dist = "weibull")) #binary, non-sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ prefecture, data=totalctc, dist = "weibull")) #binary, non-sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(highriskcat), data=totalctc, dist = "weibull")) #binary, non-sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(age.cat), data=totalctc, dist = "weibull")) #7 cats, sig
# summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ partage, data=totalctc, dist = "weibull")) #continuous, sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(occupation), data=totalctc, dist = "weibull")) #sig


#Covid and health related variables
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ covid_pos_HH, data=totalctc, dist = "weibull")) #binary, non-sig
#summary(m1 <- glm.nb(weekday_trunc ~ covid_pos_HH, data=totalctc)) #sig (p-value=0.012)
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(handwashing_cat), data=totalctc, dist = "weibull")) #7 cats,sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(mask.cat), data=totalctc, dist = "weibull")) #5 cats, sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(vax_totaltimes), data=totalctc, dist = "weibull")) #5 cats, sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(vax.cat), data=totalctc, dist = "weibull")) #4 categories, sig 


#Work-related variables 
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(workplace_location), data=totalctc, dist = "weibull")) #4 cats, sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(telework_freq), data=totalctc, dist = "weibull")) #5 cats, sig

#Moving prefecture variables 
# summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ movepref_leisure_freq_cat, data=totalctc, dist = "weibull")) #sig but
#this variable will make more sense to use the weekend contacts as the outcome instead of weekday contacts 
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ movepref_workschool_freq_cat, data=totalctc, dist = "weibull")) #sig, binary


#Location of contacts (specific to weekday contact data)
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(restaurant_wkday), data=totalctc, dist = "weibull")) #sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(bar_wkday), data=totalctc, dist = "weibull")) #sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(school_wkday), data=totalctc, dist = "weibull")) #sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(work_wkday), data=totalctc, dist = "weibull")) #sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ home_wkday, data=totalctc, dist = "weibull")) #sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ otherhome_wkday, data=totalctc, dist = "weibull")) #sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(publictransport_wkday), data=totalctc, dist = "weibull")) #non-sig (p-value: 0.059)
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(store_wkday), data=totalctc, dist = "weibull")) #non-sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(religion_wkday), data=totalctc, dist = "weibull")) #non-sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(gym_wkday), data=totalctc, dist = "weibull")) #non-sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(movie_wkday), data=totalctc, dist = "weibull")) #non-sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(market_wkday), data=totalctc, dist = "weibull")) #non-sig
summary(m1 <- survreg(Surv(weekday_trunc+0.01) ~ factor(otherplace_wkday), data=totalctc, dist = "weibull")) #sig (but we don't know where "other" is)


#Try the forward and backward stepwise selection to see which variables might be good to add in the model:
null.model <- survreg(Surv(weekday_trunc+0.01) ~ 1, data=totalctc, dist = "weibull")
AIC(null.model)

fullmodel <- survreg(Surv(weekday_trunc+0.01) ~
                       factor(noinHH_cat) + factor(covidview) + factor(age.cat) + factor(occupation) +
                       factor(handwashing_cat) + factor(mask.cat) + factor(vax.cat) + 
                       factor(workplace_location) + factor(telework_freq) + 
                       movepref_workschool_freq_cat + restaurant_wkday + school_wkday + bar_wkday +
                       home_wkday + otherhome_wkday, 
                     data = totalctc, dist = "weibull") 


AIC(fullmodel)
summary(fullmodel)

#forward stepwise selection
step.model <- stepAIC(null.model, direction = "forward", scope = list(lower = null.model,
                                                                      upper = fullmodel))
summary(step.model)  
AIC(step.model)
#exponentiate the coefficients from forward selection: 
coef_weekday_forward <- as.data.frame(exp(cbind(coef(step.model), confint(step.model))))
print(coef_weekday_forward)

#adding age back into the model after forward selection since age was kicked out 
step.model_forward_age <- survreg(Surv(weekday_trunc+0.01) ~
                                    factor(workplace_location) + home_wkday + factor(mask.cat) +
                                    school_wkday + factor(noinHH_cat) + bar_wkday + factor(occupation) +
                                    factor(handwashing_cat) + movepref_workschool_freq_cat + restaurant_wkday +
                                    factor(telework_freq) + otherhome_wkday +  factor(covidview) +
                                    factor(vax.cat) + factor(age.cat), 
                                  data = totalctc, dist = "weibull") 

AIC(step.model_forward_age)                                
summary(step.model_forward_age)  
#exponentiate the coefficients from forward selection: 
coef_weekday_forward2 <- as.data.frame(exp(cbind(coef(step.model_forward_age), confint(step.model_forward_age))))
print(coef_weekday_forward2)



#export the coefficients 
#save the weekday regression from forward stepwise selection
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/CSV"
write.csv(coef_weekday_forward2, file.path(path, "weibull_coef_weekday_forward_feb23_revised2.csv"), 
          row.names=T, fileEncoding = "UTF-8")


#backward stepwise selection 
step.model_back <- stepAIC(fullmodel, direction = "backward")
summary(step.model_back)
AIC(step.model_back)
#adding age.cat in the variables selected after backward selection: 
step.model_back_age <- survreg(Surv(weekday_trunc+0.01) ~ factor(noinHH_cat) + 
                                 factor(covidview) + factor(occupation) + factor(handwashing_cat) + 
                                 factor(mask.cat) + factor(vax.cat) + factor(workplace_location) + 
                                 factor(telework_freq) + factor(movepref_workschool_freq_cat) + 
                                 restaurant_wkday + school_wkday + bar_wkday + home_wkday + otherhome_wkday + factor(age.cat), 
                               data = totalctc, dist = "weibull")

summary(step.model_back_age)
AIC(step.model_back_age)



#exponentiate the coefficients from backward selection 
coef_weekday_back_age <- as.data.frame(exp(cbind(coef(step.model_back_age), confint(step.model_back_age))))
print(coef_weekday_back_age)

#save the weekday regression from backward stepwise selection 
#(also after adding back age into model)
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/CSV"
write.csv(coef_weekday_back_age, file.path(path, "weibull_coef_weekday_backward_feb23_revised2.csv"), 
          row.names=T, fileEncoding = "UTF-8")

#In conclusion: Same variables were selected from both forward and backward selection models 
# (with age added back in manually)


##-----------------------------------------------------------------------------------------------##
##------------------Histogram of Predicted vs. Observed Contacts (weekday)-----------------------##
##-------this is to compare how your model predictions compare with the observed data------------##   
##-----------------------------------------------------------------------------------------------##

#predicted contacts from the model selected from the forward stepwise selection 
totalctc$predictedcontacts <- predict(step.model_forward_age)

#totalctc$predictedcontacts <- exp(predict(step.model))

summary(totalctc$predictedcontacts)
summary(totalctc$grandtotal_contacts_wkday)
summary(totalctc$weekday_trunc)

plotd <- totalctc %>% dplyr::select(predictedcontacts,weekday_trunc)

#plotd <- comparemodel %>% dplyr::select(predictedcontacts,weekday_trunc)
# plotd <- totalctc %>% dplyr::select(predictedcontacts,grandtotal_contacts_wkday) 

tmp <- melt(plotd) #make it in long format
head(tmp)
#plot it as a histogram
# ggplot(tmp,aes(x=value,group=variable,fill=variable)) + 
#   geom_histogram(position = "dodge", binwidth = 0.05) +
#   scale_x_continuous(trans = scales::pseudo_log_trans(base=10), #pseudo_log_trans
#   #allows plotting the zero contacts (even though log10 of 0 is infinity)
#   breaks = c(0, 1, 10, 100, 1000)) +
#   xlab("Number of Contacts") + ylab("Frequency") +
#   scale_fill_discrete(labels = c("Predicted", "Observed"), 
#                       name = "Contacts") 

#histogram in linear scale 
plotcloseup <- ggplot(tmp,aes(x=value,group=variable,fill=variable)) + 
  geom_histogram(position = "dodge", 
                 binwidth = 1, #indicating a binwidth of 1 means you have 1 bin per contact 
                 breaks = seq(0-0.5, 100+0.5, by = 1)) + #by indicating the breaks, you are 
  #specifying from where to when does each bin start and end. In this case, it will start from 
  #-0.5 to 1.5, 1.5 to 2.5...up to 100.5. The bin will always then center itself on a whole number.
  #If you want to extend to the entire dataset, you can end the max to 250+0.5 since the 
  #max of dataset will be 250 (based on what we see from the observed, truncated contacts at 250)
  xlab("Number of Reported Contacts per Individual") + ylab("Frequency") +
  scale_fill_discrete(labels = c("Predicted", "Observed"), 
                      name = "Contacts") + 
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  theme_bw() +
  theme(axis.line = element_line(color = "black"), 
        panel.border = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10, face = "bold", color = "black"),
        axis.title.y = element_text(size = 10, face = "bold", color = "black")) +
  xlim(0,50)

print(plotcloseup)

#save the plot in PDF  
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "Fig2_Histogram_ObservedPredicted_closeup.pdf", plotcloseup, width=8,height=6, units = "in")

#plot that is expanded to 250 contacts 
plotall <- ggplot(tmp,aes(x=value,group=variable,fill=variable)) + 
  geom_histogram(position = "dodge", 
                 binwidth = 20,  #indicating a binwidth of 1 means you have 1 bin per contact 
                 breaks = seq(0-0.5, 250+0.5, by = 20)) + #by indicating the breaks, you are 
  #specifying from where to when does each bin start and end. In this case, it will start from 
  #-0.5 to 1.5, 1.5 to 2.5...up to 100.5. The bin will always then center itself on a whole number.
  #If you want to extend to the entire dataset, you can end the max to 250+0.5 since the 
  #max of dataset will be 250 (based on what we see from the observed, truncated contacts at 250)
  xlab("Number of Reported Contacts per Individual") + ylab("Frequency") +
  scale_fill_discrete(labels = c("Predicted", "Observed"), 
                      name = "Contacts") + 
  # scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  theme_bw() +
  theme(axis.line = element_line(color = "black"), 
        panel.border = element_blank(),
        # panel.grid.major = element_blank()) +
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title.x = element_text(size = 10, face = "bold", color = "black"),
        axis.title.y = element_text(size = 10, face = "bold", color = "black")) +
  
  xlim(0,250)

print(plotall)
#save the plot in PDF  
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "Fig2_Histogram_ObservedPredicted_expanded.pdf", plotall, width=8,height=6, units = "in")



##-----------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------------------------------------##
#                     Residual plots for the selected multivariable regression model 
#                   Includes code for making the residual plots shown in Suppl Figure 5
##-----------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------------------------------------##

step.model_forward_age <- survreg(Surv(weekday_trunc+0.01) ~
                                    factor(workplace_location) + home_wkday + factor(mask.cat) +
                                    school_wkday + factor(noinHH_cat) + bar_wkday + factor(occupation) +
                                    factor(handwashing_cat) + movepref_workschool_freq_cat + restaurant_wkday +
                                    factor(telework_freq) + otherhome_wkday +  factor(covidview) +
                                    factor(vax.cat) + factor(age.cat), 
                                  data = totalctc, dist = "weibull") 
AIC(step.model_forward_age)                                
summary(step.model_forward_age)  

coef_weekday_forward2 <- as.data.frame(exp(cbind(coef(step.model_forward_age), confint(step.model_forward_age))))
print(coef_weekday_forward2)


devresiduals <- residuals(step.model_forward_age, type="deviance") 

#The deviance residuals measure the deviation of each observation from the fitted model, 
#standardized by the overall goodness of fit of the model.
#deviance residuals are commonly used for models with non-constant variance or overdispersion, 
#and can help identify observations that are poorly predicted by the model.
rawresiduals <- residuals(step.model_forward_age, type="response") #this is the raw residuals 

#predicted contacts from the selected model for weekday contacts (using the log link function from weibull
#by indicating type = "link") 
totalctc$predictedcontacts_log <- predict(step.model_forward_age, type="link")
max(totalctc$predictedcontacts_linear)

totalctc$predictedcontacts <- predict(step.model_forward_age) #predicted outcome on natural scale 
max(totalctc$predictedcontacts)

#plot with the contacts on natural scale; a lot of the residuals are pushed towards the left 
#which makes it quite difficult to interpret 
#note on source: https://towardsdatascience.com/diagnose-the-generalized-linear-models-66ad01128261
plot(fitted(step.model_forward_age), devresiduals, xlab="Predicted Contacts", ylab="Deviance Residuals")
#add red line to the plot representing the smoothed relatinship b/w residuals and predicted values 
smoothed <- lowess(devresiduals ~ totalctc$predictedcontacts)
lines(smoothed, col = "red")
resid1 <- recordPlot() #save the above plot in base R
print(resid1)

#View(totalctc[,c("predictedcontacts_linear", "predictedcontacts")])

#save the plot in PDF  
setwd("~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots")
dev.copy(pdf, "SuppFig4_ResidualPlot_weibullreg_weekday.pdf", width=8,height=6)
dev.off()



#Put the above code using ggplot 
# y = devresiduals 
# x = fitted(step.model_forward_age)
#create new data frame with the residuals
resid1df <- data.frame(fitted=fitted(step.model_forward_age), resid=devresiduals)

resid1plot <- ggplot(resid1df, aes(x = fitted, y = resid)) +
  geom_point(shape = 1, color = "black", size = 2) + 
  xlab("Predicted Contacts") +
  ylab("Deviance Residuals") +
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme_bw() +
  ggtitle("a") +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.3, color = "black"),
        plot.title = element_text(size = 18, face = "bold")) + 
  stat_smooth(method = "loess", color = "red", span= 1, se = FALSE, size = 0.5)


#now take a look at the same residual plot but without the link function's transformation 
#(i.e. logged predicted outcome)
plot(totalctc$predictedcontacts_log, devresiduals, xlab="Predicted Contacts", ylab="Deviance Residuals")
resid2 <- recordPlot()
print(resid2)

#save the plot in PDF  
setwd("~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots")
dev.copy(pdf, "SuppFig4_ResidualPlot_weibullreg_weekday_loglink.pdf", width=8,height=6)
dev.off()

#now code the above using ggplot 
resid2df <- data.frame(fitted=totalctc$predictedcontacts_log, resid=devresiduals)

resid2plot <- ggplot(resid2df, aes(x = fitted, y = resid)) +
  geom_point(shape = 1, color = "black", size = 2) + 
  xlab("Predicted Contacts") +
  ylab("Deviance Residuals") +
  theme_bw() +
  ggtitle("b") +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.3, color = "black"),
        plot.title = element_text(size = 18, face = "bold")) 



#plotting the residuals against each predictor variable 

#putting it into a data frame with the residuals and predictors 
df_devresid <- data.frame(devresiduals, covar1 = totalctc$age.cat, covar2 = totalctc$vax.cat,
                          covar3 = totalctc$covidview, covar4 = totalctc$otherhome_wkday, covar5 = totalctc$telework_freq,
                          covar6 = totalctc$restaurant_wkday, covar7 = totalctc$movepref_workschool_freq_cat, 
                          covar8 = totalctc$handwashing_cat, covar9 = totalctc$occupation, covar10 = totalctc$bar_wkday, 
                          covar11 = totalctc$noinHH_cat, covar12 = totalctc$school_wkday, covar13 = totalctc$mask.cat, 
                          covar14 = totalctc$home_wkday, covar15 = totalctc$workplace_location)


df_devresid$covar1 <- factor(df_devresid$covar1, 
                             levels = c("1", "2", "3", "4", "5", "6", "7"))
levels(df_devresid$covar1)

devage <- ggplot(df_devresid, aes(x=covar1, y=devresiduals)) +
  geom_jitter(size=0.5) +
  xlab("Age Category") +
  ylab("Deviance Residuals") +
  scale_x_discrete(labels = c("0-5", "6-17", "18-29", 
                              "30-39", "40-49", "50-59", 
                              "60+")) +
  theme_bw() +
  ggtitle("c") +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.3, color = "black"),
        plot.title = element_text(size = 18, face = "bold")) 

path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "SuppFig4a_DevResid_Age_Plot.pdf", devage, width=8,height=6, units = "in")


df_devresid$covar2 <- factor(df_devresid$covar2,
                             levels = c("0", "1", "2", "3+", "NA"))

devvax <- ggplot(subset(df_devresid, covar2 %in% c("0", "1", "2", "3+")),
                 aes(x=covar2, y=devresiduals)) +
  geom_jitter(size=0.5) +
  xlab("Number of Times Vaccinated") +
  ylab("Deviance Residuals") +
  theme_bw() + 
  ggtitle("f") +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.3, color = "black"),
        plot.title = element_text(size = 18, face = "bold")) 

path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "SuppFig4b_DevResid_VaxTimes_Plot.pdf", devvax, width=8,height=6, units = "in")


df_devresid$covar11 <- factor(df_devresid$covar11, 
                              levels = c("1", "2", "3", "4", "5+"))
levels(df_devresid$covar11)

devHH <- ggplot(df_devresid, aes(x=covar11, y=devresiduals)) +
  geom_jitter(size=0.5) +
  xlab("Number of People in Household") +
  ylab("Deviance Residuals") +
  theme_bw() +
  ggtitle("d") +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.3, color = "black"),
        plot.title = element_text(size = 18, face = "bold")) 

path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "SuppFig4c_DevResid_NoinHH_Plot.pdf", devHH, width=8,height=6, units = "in")

#workplace location 
# 1 = telework at home 
# 2 = work at workplace (REFERENCE) changed to this one b/c biggest N 
# 3 = work at school 
# 4 = Not employed 

levels(df_devresid$covar15)

devworkplace <- ggplot(df_devresid, aes(x=covar15, y=devresiduals)) + 
  geom_jitter(size=0.5) +
  xlab("Workplace Location") +
  ylab("Deviance Residuals") + 
  scale_x_discrete(labels = c("Workplace", "Telework at Home", "School", "Not employed")) +
  theme_bw() +
  ggtitle("e") +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 0.3, color = "black"),
        plot.title = element_text(size = 18, face = "bold")) 

#combine all of the residual plots together 
comboplot <- (resid1plot + resid2plot) / (devage + devHH) / (devworkplace + devvax)
print(comboplot)
#save the combined plot 
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
ggsave(path = path, #indicate where you want to save 
       filename = "SuppFig5_combo_allresidual_plots.pdf", comboplot, width=11,height=12, units = "in")

##-----------------------------------------------------------------------------------------------##
##---------------------Table 2 with contact rate ratios (crude and adjusted)---------------------##
##-----------------------------------------------------------------------------------------------##

#the model that was selected from forward stepwise selection with age added back in 
#total of 15 variables 
step.model_forward_age <- survreg(Surv(weekday_trunc+0.01) ~
                                    factor(workplace_location) + home_wkday + factor(mask.cat) +
                                    school_wkday + factor(noinHH_cat) + bar_wkday + factor(occupation) +
                                    factor(handwashing_cat) + movepref_workschool_freq_cat + restaurant_wkday +
                                    factor(telework_freq) + otherhome_wkday +  factor(covidview) +
                                    factor(vax.cat) + factor(age.cat), 
                                  data = totalctc, dist = "weibull") 



#putting the coefficients and 95% CI into a data frame for the adjusted CRRs
coef <- exp(cbind(coef(step.model_forward_age), confint(step.model_forward_age)))
coef <- as.data.frame(coef)
coef$category <- rownames(coef)
colnames(coef)[1] <- "Adjusted Contact Rate Ratio"
colnames(coef)[2] <- "Lower 95% CI (Adjusted)"
colnames(coef)[3] <- "Upper 95% CI (Adjusted)"
#reorder the columns
coef <- coef[, c(4,1,2,3)]

#now you need the crude contact rate ratio from univariate analyses 
m0 <- survreg(Surv(weekday_trunc+0.01) ~ factor(age.cat), data=totalctc, dist = "weibull")
m0 <- exp(cbind(coef(m0), confint(m0)))
m0 <- as.data.frame(m0)
m0 <- m0[-c(1),] #you don't need the intercept for the univariate results
m0$category <- rownames(m0)

m1 <-  survreg(Surv(weekday_trunc+0.01) ~ factor(workplace_location), data=totalctc, dist = "weibull")
m1 <- exp(cbind(coef(m1), confint(m1)))
m1 <- as.data.frame(m1)
m1 <- m1[-c(1),] #you don't need the intercept for the univariate results
m1$category <- rownames(m1)

m2 <-  survreg(Surv(weekday_trunc+0.01) ~ home_wkday, data=totalctc, dist = "weibull")
m2 <- exp(cbind(coef(m2), confint(m2)))
m2 <- as.data.frame(m2)
m2 <- m2[-c(1),] #you don't need the intercept for the univariate results
m2$category <- rownames(m2)

m3 <-  survreg(Surv(weekday_trunc+0.01) ~ factor(mask.cat), data=totalctc, dist = "weibull")
m3 <- exp(cbind(coef(m3), confint(m3)))
m3 <- as.data.frame(m3)
m3 <- m3[-c(1),] #you don't need the intercept for the univariate results
m3$category <- rownames(m3)


m4 <-  survreg(Surv(weekday_trunc+0.01) ~ school_wkday, data=totalctc, dist = "weibull")
m4 <- exp(cbind(coef(m4), confint(m4)))
m4 <- as.data.frame(m4)
m4 <- m4[-c(1),] #you don't need the intercept for the univariate results
m4$category <- rownames(m4)

m5 <-  survreg(Surv(weekday_trunc+0.01) ~ factor(noinHH_cat), data=totalctc, dist = "weibull")
m5 <- exp(cbind(coef(m5), confint(m5)))
m5 <- as.data.frame(m5)
m5 <- m5[-c(1),] #you don't need the intercept for the univariate results
m5$category <- rownames(m5)

m6 <-  survreg(Surv(weekday_trunc+0.01) ~ restaurant_wkday, data=totalctc, dist = "weibull")
m6 <- exp(cbind(coef(m6), confint(m6)))
m6 <- as.data.frame(m6)
m6 <- m6[-c(1),] #you don't need the intercept for the univariate results
m6$category <- rownames(m6)


m7 <-  survreg(Surv(weekday_trunc+0.01) ~ factor(occupation), data=totalctc, dist = "weibull")
m7 <- exp(cbind(coef(m7), confint(m7)))
m7 <- as.data.frame(m7)
m7 <- m7[-c(1),] #you don't need the intercept for the univariate results
m7$category <- rownames(m7)

m8 <-  survreg(Surv(weekday_trunc+0.01) ~ factor(handwashing_cat), data=totalctc, dist = "weibull")
m8 <- exp(cbind(coef(m8), confint(m8)))
m8 <- as.data.frame(m8)
m8 <- m8[-c(1),] #you don't need the intercept for the univariate results
m8$category <- rownames(m8)

m9 <-  survreg(Surv(weekday_trunc+0.01) ~ movepref_workschool_freq_cat, data=totalctc, dist = "weibull")
m9 <- exp(cbind(coef(m9), confint(m9)))
m9 <- as.data.frame(m9)
m9 <- m9[-c(1),] #you don't need the intercept for the univariate results
m9$category <- rownames(m9)

m10 <-  survreg(Surv(weekday_trunc+0.01) ~ otherhome_wkday, data=totalctc, dist = "weibull")
m10 <- exp(cbind(coef(m10), confint(m10)))
m10 <- as.data.frame(m10)
m10 <- m10[-c(1),] #you don't need the intercept for the univariate results
m10$category <- rownames(m10)

m11 <-  survreg(Surv(weekday_trunc+0.01) ~ bar_wkday, data=totalctc, dist = "weibull")
m11 <- exp(cbind(coef(m11), confint(m11)))
m11 <- as.data.frame(m11)
m11 <- m11[-c(1),] #you don't need the intercept for the univariate results
m11$category <- rownames(m11)

m12 <-  survreg(Surv(weekday_trunc+0.01) ~ factor(covidview), data=totalctc, dist = "weibull")
m12 <- exp(cbind(coef(m12), confint(m12)))
m12 <- as.data.frame(m12)
m12 <- m12[-c(1),] #you don't need the intercept for the univariate results
m12$category <- rownames(m12)

m13 <-  survreg(Surv(weekday_trunc+0.01) ~ factor(vax.cat), data=totalctc, dist = "weibull")
m13 <- exp(cbind(coef(m13), confint(m13)))
m13 <- as.data.frame(m13)
m13 <- m13[-c(1),] #you don't need the intercept for the univariate results
m13$category <- rownames(m13)

m14 <-  survreg(Surv(weekday_trunc+0.01) ~ factor(telework_freq), data=totalctc, dist = "weibull")
m14 <- exp(cbind(coef(m14), confint(m14)))
m14 <- as.data.frame(m14)
m14 <- m14[-c(1),] #you don't need the intercept for the univariate results
m14$category <- rownames(m14)

#need teleworkfreq, 

#combine all of the above together by doing rbind 
m_all <- bind_rows(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14)

colnames(m_all)[1] <- "Crude Contact Rate Ratio"
colnames(m_all)[2] <- "Lower 95% CI (Crude)"
colnames(m_all)[3] <- "Upper 95% CI (Crude)"
#reorder the columns
m_all <- m_all[, c(4,1,2,3)]

#merge the adjusted CRRs together with the crude CRRs by "category" name 
total <- merge(m_all, coef, by="category")
#round the numerical columns to 2 decimal points 
total <- total %>% mutate_at(2:7, round, 2)

#reorder the category (variables)
x <- c("factor(age.cat)1", "factor(age.cat)2", "factor(age.cat)3", "factor(age.cat)4", 
       "factor(age.cat)6", "factor(age.cat)7", 
       
       "factor(noinHH_cat)1", "factor(noinHH_cat)2", 
       "factor(noinHH_cat)4", "factor(noinHH_cat)5+", 
       
       "factor(occupation)1", 
       "factor(occupation)3", "factor(occupation)4", "factor(occupation)5", "factor(occupation)7",
       "factor(occupation)8", "factor(occupation)9", "factor(occupation)10", "factor(occupation)11",
       "factor(occupation)12", "factor(occupation)13", "factor(occupation)14", "factor(occupation)15",
       "factor(occupation)16", "factor(occupation)17", "factor(occupation)18", "factor(occupation)19", 
       "factor(occupation)20", "factor(occupation)21", "factor(occupation)22", "factor(occupation)23", 
       
       "movepref_workschool_freq_cat1", 
       
       "factor(workplace_location)1", "factor(workplace_location)3", "factor(workplace_location)4",
       
       "factor(telework_freq)1", "factor(telework_freq)2", "factor(telework_freq)3", "factor(telework_freq)4",
       
       "home_wkday", "otherhome_wkday", "school_wkday", "restaurant_wkday", "bar_wkday",
       
       "factor(mask.cat)2", "factor(mask.cat)3", 
       "factor(mask.cat)4", "factor(mask.cat)5", 
       
       "factor(handwashing_cat)0", "factor(handwashing_cat)2",
       "factor(handwashing_cat)3","factor(handwashing_cat)4", "factor(handwashing_cat)5", 
       "factor(handwashing_cat)6+",
       
       "factor(vax.cat)0", "factor(vax.cat)1", "factor(vax.cat)2", "factor(vax.cat)NA",
       
       "factor(covidview)2", "factor(covidview)3", "factor(covidview)4", "factor(covidview)5", "factor(covidview)6")




total <- total %>% slice(match(x, category))

#Adding # of participants, mean and median # of contacts for each of the covariates to Table 2
total$meancontact <- NA
total$mediancontact <- NA
total$participants <- NA
total$participants_percent <- NA
total$contact_q25 <- NA
total$contact_q75 <- NA

#rename these columns
colnames(total)[8] <- "Mean Number of Contacts"
colnames(total)[9] <- "Median Number of Contacts"
colnames(total)[10] <- "Number of Participants"
colnames(total)[11] <- "% of Participants"
colnames(total)[12] <- "Lower IQR (Contacts)"
colnames(total)[13] <- "Upper IQR (Contacts)"

#create new dataframe so you can put the rows with the category name and the reference category
colnames <- colnames(total)
reftable <- data.frame(matrix(nrow = 15, ncol = length(colnames))) #nrows = number of variables
# so in our case, we have 15 selected predictors from our model 
colnames(reftable) <- colnames

#add the variable names for the reference categories 
category <- c("factor(age.cat)5",
              "factor(noinHH_cat)3", "factor(occupation)2", 
              "movepref_workschool_freq_cat0", "factor(workplace_location)2",
              "factor(telework_freq)0",
              "No Contact at Home", "No Contact at Other Home", "No Contact at School", 
              "No Contact at Restaurant", "No Contact at Bar",
              "factor(mask.cat)1", 
              "factor(handwashing_cat)1", "factor(vax.cat)3+", "factor(covidview)1")

reftable$category <- category
#then merge this table2 with the total dataframe 
total <- do.call("rbind", list(total, reftable))

#reorder the category (variables)
x <- c("factor(age.cat)1", "factor(age.cat)2", "factor(age.cat)3", 
       "factor(age.cat)4", "factor(age.cat)5", "factor(age.cat)6",
       "factor(age.cat)7", 
       
       
       "factor(noinHH_cat)1",
       "factor(noinHH_cat)2", "factor(noinHH_cat)3",
       "factor(noinHH_cat)4", "factor(noinHH_cat)5+",
       
       "factor(occupation)1", "factor(occupation)2",
       "factor(occupation)3", "factor(occupation)4", "factor(occupation)5", "factor(occupation)7",
       "factor(occupation)8", "factor(occupation)9", "factor(occupation)10", "factor(occupation)11",
       "factor(occupation)12", "factor(occupation)13", "factor(occupation)14", "factor(occupation)15",
       "factor(occupation)16", "factor(occupation)17", "factor(occupation)18", "factor(occupation)19", 
       "factor(occupation)20", "factor(occupation)21", "factor(occupation)22", "factor(occupation)23", 
       
       "movepref_workschool_freq_cat0", "movepref_workschool_freq_cat1", 
       
       "factor(workplace_location)1",
       "factor(workplace_location)2", "factor(workplace_location)3", "factor(workplace_location)4",
       
       "factor(telework_freq)0", "factor(telework_freq)1", "factor(telework_freq)2", "factor(telework_freq)3", "factor(telework_freq)4",
       
       "No Contact at Home", "home_wkday", "No Contact at Other Home", "otherhome_wkday",
       
       "No Contact at School", "school_wkday", "No Contact at Restaurant", "restaurant_wkday", 
       
       "No Contact at Bar", "bar_wkday",
       
       "factor(mask.cat)1", 
       "factor(mask.cat)2", "factor(mask.cat)3", 
       "factor(mask.cat)4", "factor(mask.cat)5", 
       
       "factor(handwashing_cat)0",
       "factor(handwashing_cat)1", "factor(handwashing_cat)2",
       "factor(handwashing_cat)3","factor(handwashing_cat)4", "factor(handwashing_cat)5", 
       "factor(handwashing_cat)6+",
       
       "factor(vax.cat)0", "factor(vax.cat)1", "factor(vax.cat)2", "factor(vax.cat)3+", "factor(vax.cat)NA",
       
       "factor(covidview)1",  "factor(covidview)2", "factor(covidview)3", "factor(covidview)4", "factor(covidview)5", "factor(covidview)6")

total <- total %>% slice(match(x, category)) #reorder the rows for the entire table 
total <- total[, c(1, 10,11, 8,9,12,13, 2:7)] #reorder the columns for the entire table 
total_archive <- total #original version of the "total" data frame




#create a subset of the main dataset that just has the variables you selected in your final reg model
#in the same order as how the rows are aligned in Table 2 so it will work properly in the loop 
subset <- totalctc %>% dplyr::select(
  weekday_trunc, age.cat, noinHH_cat, occupation, movepref_workschool_freq_cat,
  workplace_location, telework_freq, home_wkday, otherhome_wkday, school_wkday, restaurant_wkday, bar_wkday,
  mask.cat, handwashing_cat, vax.cat, covidview 
)

#relevel the subset variables based on how Table 2 is ordered 

#var1
subset$age.cat <- factor(subset$age.cat, 
                         levels = c("1", "2", "3", "4", "5", "6", "7"))
# #double check to make sure the reference point has changed
# #the first level is the reference 
levels(subset$age.cat)

#var2
subset$noinHH_cat <- factor(subset$noinHH_cat, 
                            levels = c("1", "2", "3", "4", "5+"))

levels(subset$noinHH_cat)

#var3
subset$occupation <- factor(subset$occupation, 
                            levels = c("1", "2", "3", "4",
                                       "5", "6", "7", "8", 
                                       "9", "10", "11", "12", 
                                       "13", "14", "15", "16",
                                       "17", "18", "19", "20", 
                                       "21", "22", "23"
                            ))

levels(subset$occupation)

#var4
subset$workplace_location <- factor(subset$workplace_location, 
                                    levels = c("1", "2", "3", "4"))
levels(subset$workplace_location)

#var5
subset$handwashing_cat <- factor(subset$handwashing_cat, 
                                 levels = c("0", "1", "2", "3", "4", "5", "6+"))

levels(subset$handwashing_cat)

#var6
table(subset$vax.cat)
subset$vax.cat <- factor(subset$vax.cat, 
                         levels = c("0", "1", "2", "3+", "NA"))
levels(subset$vax.cat)

table(subset$covidview)
table(subset$telework_freq)

#start the loop from here to enter all the nubers in Table 2 
counter <- 1   # initial row
for (i in 2:length(subset)) {
  #table2_stats[[i]] 
  extract_subset <- subset %>%
    group_by(subset[[i]]) %>%
    summarize(n = n(), #gives the total count of each subcategory 
              mean = round(mean(weekday_trunc),2), 
              median = format(round(median(weekday_trunc),
                                    digits = 2), nsmall = 2), 
              #nsmall = round to 2 digits after decimal point 
              #format the median so there will be two 0's after decimal point  
              q25 = format(round(quantile(weekday_trunc, probs = 0.25)), nsmall = 2),
              q75 = format(round(quantile(weekday_trunc, probs = 0.75)), nsmall = 2)) %>%
    mutate(perc = round(prop.table(n)*100, 2))  #adds new column with percent of the n in each subcategory 
  n_rows <- dim(extract_subset)[1]    # number of rows in the data extracted from subset
  total[counter:(counter+n_rows-1), c(2,4,5,6,7,3)] <- extract_subset[,c(2,3,4,5,6,7)] 
  # inject data in correct columns and rows. rows are updated dynamically
  counter <- counter+n_rows  #update the counter as you go through each column "i"
}

#check to make sure the codes are correct in the loop 
testhh <- totalctc %>% group_by(noinHH_cat) %>% 
  summarize(n = n(), 
            mean = round(mean(weekday_trunc),2), #round to 2 digits after decimal point 
            median = format(round(median(weekday_trunc),
                                  digits = 2), nsmall = 2), 
            q25 = format(round(quantile(weekday_trunc, probs = 0.25)), nsmall = 2),
            q75 = format(round(quantile(weekday_trunc, probs = 0.75)), nsmall = 2)
  )
testhh 

testcovidview <- totalctc %>% group_by(covidview) %>% 
  summarize(n = n(), 
            mean = round(mean(weekday_trunc),2), #round to 2 digits after decimal point 
            median = format(round(median(weekday_trunc),
                                  digits = 2), nsmall = 2), 
            q25 = format(round(quantile(weekday_trunc, probs = 0.25)), nsmall = 2),
            q75 = format(round(quantile(weekday_trunc, probs = 0.75)), nsmall = 2)
  )
testcovidview

#or can check with the below codes using tapply 
tapply(totalctc$weekday_trunc, totalctc$noinHH_cat, mean)
tapply(totalctc$weekday_trunc, totalctc$noinHH_cat, median)
tapply(totalctc$weekday_trunc, totalctc$noinHH_cat, quantile)

table(subset$age.cat)
prop.table(table(subset$age.cat))
tapply(subset$weekday_trunc, subset$age.cat, mean)
tapply(subset$weekday_trunc, subset$age.cat, median)
tapply(subset$weekday_trunc, subset$age.cat, quantile)

table(subset$covidview)
prop.table(table(subset$covidview))
tapply(subset$weekday_trunc, subset$covidview, mean)
tapply(subset$weekday_trunc, subset$covidview, median)
tapply(subset$weekday_trunc, subset$covidview, quantile)

table(subset$handwashing_cat)
prop.table(table(subset$handwashing_cat))
tapply(subset$weekday_trunc, subset$handwashing_cat, mean)
tapply(subset$weekday_trunc, subset$handwashing_cat, median)
tapply(subset$weekday_trunc, subset$handwashing_cat, quantile)


#and indeed...the codes inside the loop are correct!

#now replace the category names in Table 2 so they are comprehensible!

setwd("~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/CSV")
varnames <- read.csv("table2_colnames.csv", header = FALSE)
y <- varnames$V1 #put all of the variable names as a list 
total$category <- as.character(y)

print(total)


#save the table as as csv file 
path  <- "~/Dropbox/LSHTM PhD/Tomoka PhD Project Shared/JP Fukuoka Osaka Survey/Paper_Plots"
write.csv(total, file.path(path, "Table2_weibull_coef_weekday_forward_feb23_revised.csv"), 
          row.names=F, fileEncoding = "UTF-8")


