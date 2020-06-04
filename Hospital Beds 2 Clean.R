#####Hospital Beds 2 - clean#######
# RBH Beds = 723
 
#At this point I think you need to install all these packages, bu that's because I've tried 
#so many different ways of modelling this

setwd("~/HOSPITAL BEDS")

library(dplyr)
library(readxl)
library(ggplot2)
library(GGally)
library(foreach)
library(forecast)
library(deSolve)
library(tidyverse)
library(lubridate)
library(scales)
library(stringr)
library(sjmisc)
library(stringdist)
library(simmer)
library(simmer.plot)
library(expm)
library(tidyr)
library(timetk)
library(tsbox)
library(sweep)
library(tidyquant)
library(plotly)

set.seed(25042020) 

los_ref <- read_excel("length of stay last 6 months.xlsx")
number_of_admissions_by_month <- read_excel("number of admissions by month.xlsx", 
                                            col_types = c("date", "text", "text", 
                                                          "text", "text", "text", "text", "numeric"))

#####dropped Day Cases, dropped NULL, dropped LOS==0 days (i.e not overnight stay)

##admissions_daily <- read.excel("admissions_daily.xlsx")
##admissions_daily <- admissions_daily %>% filter(diagnosis_1 != "NULL",
#                                                `Admission Type` != "DC",
#                                                LOS != 0)
#^used in model.making r script rather than here


#####Midnight bed occupancy

midnight_beds <- read_excel("midnight_beds.xlsx")

##########

## Attributing mean length of stay to each of the admissions and summing a 'bed-days' metric

mean_los_icd<- admissions_daily %>%
  group_by(`ICD10 Short`) %>%
  summarise(mean_los_icd = mean(LOS)
  )
average_los <- merge(admissions_daily, mean_los_icd, by.x = "ICD10 Short")
average_los$bed_days <- c(average_los$mean_los_icd*average_los$`Count of Admissions`)

bed_days_daily<- average_los %>%
  group_by(`Admission Date`) %>%
  summarise(sum_bed_days = sum(bed_days)
  )

##plotting bed days

bed_days_daily$`Admission Date` <- as.Date(bed_days_daily$`Admission Date`, tz = "UTC")
bed_days_dailyts <- ts(bed_days_daily, start = decimal_date(as.Date(2018-10-01)))
m_totalbd = auto.arima(bed_days_dailyts[,2])
f_totalbd = forecast(m_totalbd, h=28)    #h = number of days into the 'future' to forecast
plot(f_totalbd, xlab = "days since 2018-10-11", ylab = "admissions x mean_LOS for that admission = bed days from that admission", main = "Forecast of incoming Bed Days")
as.numeric(f_totalbd$mean)

#plotting admissions per day

total_admissions_daily<- average_los %>%
  group_by(`Admission Date`) %>%
  summarise(total_admissions = sum(`Count of Admissions`)
  )

total_admissions_daily$`Admission Date` <- as.Date(total_admissions_daily$`Admission Date`)
total_admissions_dailyts <- ts(total_admissions_daily, start = decimal_date(as.Date(2018-10-01)))
total_admissions_dailyts_train <- subset(total_admissions_dailyts, start = 2018-10-01, end = 2020-03-01)
m_totaladmissionstrain = auto.arima(total_admissions_dailyts_train[,2])

f_totaladmissions = forecast(m_totaladmissions, h=60)  
plot(f_totaladmissions, xlab = "days since 2018-10-11", ylab = "admissions per day", main = "Forecast of admissions per day (non-day case)")

##### moving onto specalty specific forecasting
##### REMEMBER TO FORMAT AS.DATE

##########################
##### Bed occupancy ######
##########################

#adding in 0 values for bed occupancy so NA values don't mess up and making a complementary wide dataset

midnight_beds_wide <- spread(midnight_beds, Discharge_Main_Specialty_Desc, "Occupied Beds")
midnight_beds_wide[is.na(midnight_beds_wide)] <- 0

#collating all departments which have very few overnight stays to improve presentation
#then dropping them from the reformed long dataset

midnight_beds_wide_OTHER <- midnight_beds_wide %>% mutate(OTHER = ANAESTHETICS + `CHILD AND ADOLESCENT PSYCH` + `CLINICAL ONCOLOGY` + DERMATOLOGY + `GENITO-URINARY MEDICINE` + `MEDICAL ONCOLOGY` + `MIDWIFE EPISODE` + NEUROLOGY + RADIOLOGY)
drop <- c("ANAESTHETICS","CHILD AND ADOLESCENT PSYCH","CLINICAL ONCOLOGY","DERMATOLOGY","GENITO-URINARY MEDICINE","MEDICAL ONCOLOGY","MIDWIFE EPISODE","NEUROLOGY","RADIOLOGY")
midnight_beds_wide_OTHER = midnight_beds_wide_OTHER[,!(names(midnight_beds_wide_OTHER) %in% drop)]

midnight_beds<- gather(midnight_beds_wide_OTHER, Discharge_Main_Specialty_Desc, "Occupied Beds", "ACCIDENT AND EMERGENCY":"OTHER", factor_key = T)

#formating the dates

midnight_beds$Date_Value <- as.Date(midnight_beds$Date_Value)
midnight_beds_wide$Date_Value <- as.Date(midnight_beds_wide$Date_Value)
ggplot(data = midnight_beds, 
       aes(x = `Date_Value`, y = `Occupied Beds`, fill = Discharge_Main_Specialty_Desc)) +
       geom_bar(stat = 'identity') 

#group fun and forecasting
midnight_beds_dept <- midnight_beds %>%
  group_by(Discharge_Main_Specialty_Desc)

#Training for pre-covid
#comment out the below line for everything to run faster

midnight_beds_train <- midnight_beds %>% filter(Date_Value < "2020-03-01")

midnight_beds_train_list <- midnight_beds_train %>%
  group_by(Discharge_Main_Specialty_Desc) %>%
  nest()
midnight_beds_train_list_ts <- midnight_beds_train_list %>%
  mutate(data_train.ts = map(.x = data,
                       .f = tk_ts,
                       start = decimal_date(as.Date("2018-10-01")),
                       freq = 12))

midnight_beds_train_list_ts_arima <- midnight_beds_train_list_ts %>%
  mutate(fit.arima = map(data_train.ts, auto.arima))
midnight_beds_train_list_forecast <- midnight_beds_train_list_ts_arima %>%
  mutate(forecast.arima = map(fit.arima, forecast, h=90))

midnight_beds_train_list_forecast_tidy <- midnight_beds_train_list_forecast %>%
  mutate(sweep = map(forecast.arima, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)

midnight_beds_train_list_forecast_tidy$`Occupied Beds`[midnight_beds_train_list_forecast_tidy$`Occupied Beds`<0] <- 0
midnight_beds_train_list_forecast_tidy$lo.80[midnight_beds_train_list_forecast_tidy$lo.80 <0] <- 0
midnight_beds_train_list_forecast_tidy$lo.95 [midnight_beds_train_list_forecast_tidy$lo.95 <0] <- 0



midnight_beds_train_list_forecast_tidy %>%
  ggplot(aes(x = index, y = `Occupied Beds`, color = key, group = Discharge_Main_Specialty_Desc)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Hospital Beds by Discharge Specialty",
       subtitle = "Auto-ARIMA Model Forecasts",
       x = "Date", y = "Beds") +
  scale_x_date(date_breaks = "3 month", date_labels = "%M-%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ Discharge_Main_Specialty_Desc, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################
#forecasts including COVID period (up to present)
################################################


midnight_beds_list <- midnight_beds %>%
  group_by(Discharge_Main_Specialty_Desc) %>%
  nest()
midnight_beds_list_ts_overlay <- midnight_beds_list %>%
  mutate(data_train.ts = map(.x = data,
                             .f = tk_ts,
                             start = as.Date("2018-10-01"),
                             
                             freq = 7))

midnight_beds_list_ts_overlay_arima <- midnight_beds_list_ts_overlay %>%
  mutate(fit.arima = map(data_train.ts, auto.arima))
midnight_beds_list_for_overlay_forecast <- midnight_beds_list_ts_overlay_arima %>%
  mutate(forecast.arima = map(fit.arima, forecast, h=14))
midnight_beds_list_forecast <- midnight_beds_list_ts_overlay_arima %>%
  mutate(forecast.arima = map(fit.arima, forecast, h=60))

midnight_beds_list_overlay_forecast_tidy <- midnight_beds_list_for_overlay_forecast %>%
  mutate(sweep = map(forecast.arima, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)

midnight_beds_list_overlay_forecast_tidy$`Occupied Beds`[midnight_beds_list_overlay_forecast_tidy$`Occupied Beds`<0] <- 0
midnight_beds_list_overlay_forecast_tidy$lo.80[midnight_beds_list_overlay_forecast_tidy$lo.80 <0] <- 0
midnight_beds_list_overlay_forecast_tidy$lo.95 [midnight_beds_list_overlay_forecast_tidy$lo.95 <0] <- 0

ggplot(midnight_beds_list_overlay_forecast_tidy, aes(x = index, y = `Occupied Beds`, color = key, group = Discharge_Main_Specialty_Desc)) +
  geom_ribbon(data = midnight_beds_train_list_forecast_tidy,aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(data = midnight_beds_train_list_forecast_tidy,aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  geom_line(data = midnight_beds_train_list_forecast_tidy, aes(x = index, y = `Occupied Beds`, color = key, group = Discharge_Main_Specialty_Desc))+
  labs(title = "Hospital Beds by Discharge Specialty",
       subtitle = "Auto-ARIMA Model Forecasts",
       x = "Date", y = "Beds") +
  scale_x_date(date_breaks = "4 month", date_labels = "%y-%m") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ Discharge_Main_Specialty_Desc, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


  
###############################################################################################
## This produces an overlay plot of forecasts pre-COVID and real data post- 01/03/2020 ########

midnight_beds_list_overlay_forecast_tidy %>%
ggplot(aes(x = index, y = `Occupied Beds`, color = key, group = Discharge_Main_Specialty_Desc)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  geom_line(data = midnight_beds_train_list_forecast_tidy, aes(x = index, y = `Occupied Beds`, color = key, group = Discharge_Main_Specialty_Desc))+
  labs(title = "Hospital Beds by Discharge Specialty",
       subtitle = "Auto-ARIMA Model Forecasts",
       x = "Date", y = "Beds") +
  scale_x_date(date_breaks = "4 month", date_labels = "%y-%m") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ Discharge_Main_Specialty_Desc, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##ELM FORECAST


#midnight_beds_list_overlay_elm_forecast_tidy %>%
 # ggplot(aes(x = index, y = `Occupied Beds`, color = key, group = Discharge_Main_Specialty_Desc)) +
#
 # geom_line() +
  #labs(title = "Hospital Beds by Discharge Specialty",
   #    subtitle = "Auto-ARIMA Model Forecasts",
    #   x = "Date", y = "Beds") +
#  scale_x_date(date_breaks = "4 month", date_labels = "%y-%m") +
#  scale_color_tq() +
#  scale_fill_tq() +
#  facet_wrap(~ Discharge_Main_Specialty_Desc, scales = "free_y", ncol = 3) +
#  theme_tq() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################
######## Run outputs of model on weekly mean data ##############
################################################################






#####################################################################
##starting on building an in/out model on admissions_daily###########
#####################################################################



#Building paper model




#simulate with previous year
#??neural net
# NARX MODEL???????

  