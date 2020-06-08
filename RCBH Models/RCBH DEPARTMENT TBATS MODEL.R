#ARIMA MODEL
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
midnight_beds <- read_excel("midnight_beds.xlsx")

##########################
##### Bed occupancy ######
##########################

#adding in 0 values for bed occupancy so NA values don't mess up and making a complementary wide dataset

midnight_beds_wide <-
  spread(midnight_beds, Discharge_Main_Specialty_Desc, "Occupied Beds")
midnight_beds_wide[is.na(midnight_beds_wide)] <- 0

#collating all departments which have very few overnight stays to improve presentation
#then dropping them from the reformed long dataset

midnight_beds_wide_OTHER <-
  midnight_beds_wide %>% mutate(
    OTHER = ANAESTHETICS + `CHILD AND ADOLESCENT PSYCH` + `CLINICAL ONCOLOGY` + DERMATOLOGY + `GENITO-URINARY MEDICINE` + `MEDICAL ONCOLOGY` + `MIDWIFE EPISODE` + NEUROLOGY + RADIOLOGY
  )
drop <-
  c(
    "ANAESTHETICS",
    "CHILD AND ADOLESCENT PSYCH",
    "CLINICAL ONCOLOGY",
    "DERMATOLOGY",
    "GENITO-URINARY MEDICINE",
    "MEDICAL ONCOLOGY",
    "MIDWIFE EPISODE",
    "NEUROLOGY",
    "RADIOLOGY"
  )
midnight_beds_wide_OTHER = midnight_beds_wide_OTHER[,!(names(midnight_beds_wide_OTHER) %in% drop)]

midnight_beds <-
  gather(
    midnight_beds_wide_OTHER,
    Discharge_Main_Specialty_Desc,
    "Occupied Beds",
    "ACCIDENT AND EMERGENCY":"OTHER",
    factor_key = T
  )

#formating the dates

midnight_beds$Date_Value <- as.Date(midnight_beds$Date_Value)
midnight_beds_wide$Date_Value <-
  as.Date(midnight_beds_wide$Date_Value)
ggplot(data = midnight_beds,
       aes(x = `Date_Value`, y = `Occupied Beds`, fill = Discharge_Main_Specialty_Desc)) +
  geom_bar(stat = 'identity')


# Setting data as grouped list with multiple seasonality ------------------


midnight_beds_tbats_list <- midnight_beds %>%
  select(-Date_Value) %>%
  group_by(Discharge_Main_Specialty_Desc) %>%
  nest()

midnight_beds_tbats_7 <- midnight_beds_tbats_list %>%
  mutate(data_train_7.ts = map(
    .x = data,
    .f = msts,
    start = midnight_beds$Date_Value[1],
    seasonal.periods = c(7, 365.25)
  ))

# Generating Model Fit (TBATS) and Tidying --------------------------------------------


midnight_beds_tbats_fit <-
  midnight_beds_tbats_7 %>%
  mutate(fit.tbats = map(data_train_7.ts, tbats))

midnight_beds_tbats_forecast <-
  midnight_beds_tbats_fit %>%
  mutate(forecast.tbats = map(fit.tbats, forecast, h = 60))

midnight_beds_tbats_forecast_tidy <-
  midnight_beds_tbats_forecast %>%
  mutate(sweep = map(
    forecast.tbats,
    sw_sweep,
    fitted = FALSE,
    timetk_idx = FALSE
  )) %>%
  unnest(sweep)

midnight_beds_tbats_forecast_tidy$date_value <-
  rep(seq.Date(
    from = midnight_beds$Date_Value[1],
    by = "1 day",
    length.out = nrow(midnight_beds_tbats_forecast_tidy)/n_groups(midnight_beds_tbats_forecast_tidy)
  ), times = n_groups(midnight_beds_tbats_forecast_tidy))

midnight_beds_tbats_forecast_tidy$`Occupied Beds`[midnight_beds_tbats_forecast_tidy$`Occupied Beds` <
                                                        0] <-
  0
midnight_beds_tbats_forecast_tidy$lo.80[midnight_beds_tbats_forecast_tidy$lo.80 <
                                              0] <- 0
midnight_beds_tbats_forecast_tidy$lo.95 [midnight_beds_tbats_forecast_tidy$lo.95 <
                                               0] <- 0

# Plotting Model Outputs (showing 01/20 onwards) --------------------------


midnight_beds_tbats_forecast_tidy %>%
  ggplot(
    aes(
      x = date_value ,
      y = `Occupied Beds`,
      color = key,
      group = Discharge_Main_Specialty_Desc
    )
  ) +
  geom_ribbon(
    aes(ymin = lo.95, ymax = hi.95),
    fill = "#D5DBFF",
    color = NA,
    size = 0
  ) +
  geom_ribbon(
    aes(ymin = lo.80, ymax = hi.80, fill = key),
    fill = "#596DD5",
    color = NA,
    size = 0,
    alpha = 0.8
  ) +
  geom_line() +
  labs(
    title = "Hospital Beds by Discharge Specialty",
    subtitle = "Trigonometric, Box-Cox transform, ARMA errors, Trend, and Seasonal components Model",
    x = "Date",
    y = "Beds"
  ) +
  scale_x_date(
    date_breaks = "3 month",
    date_labels = "%Y-%m",
    limit = c(as.Date("2020-01-01"), as.Date("2020-07-01"))
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap( ~ Discharge_Main_Specialty_Desc,
              scales = "free_y",
              ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
