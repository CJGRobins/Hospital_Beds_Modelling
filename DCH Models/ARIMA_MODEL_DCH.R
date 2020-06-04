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
midnight_beds_dch <- read_excel("midnight_beds_dch.xlsx")

##########################
##### Bed occupancy ######
##########################

# Clearing and Wrangling Database + Aggregating Smaller Specialties -------



midnight_beds_dch <- midnight_beds_dch %>%
  mutate(
    `Census Date` = as.Date(`Census Date`),
    `Occupied Beds` = as.numeric(`Occupied Beds`)
  )

midnight_beds_dch_wide <-
  spread(midnight_beds_dch, Specialty, "Occupied Beds")

midnight_beds_dch_wide <- midnight_beds_dch_wide %>%
  replace(is.na(.), 0) %>%
  mutate(
    `OTHER SURGERY` = `GENERAL SURGERY` + `BREAST SURGERY` + `COLORECTAL SURGERY` + `VASCULAR SURGERY` + `MAXILLO-FACIAL SURGERY`
  ) %>%
  mutate(
    `ALL PAEDIATRICS` = `PAEDIATRIC OPHTHALMOLOGY` + `PAEDIATRIC TRAUMA & ORTH` + `PAEDIATRIC SURGERY` + `PAEDIATRIC ONCOLOGY` + `PAEDIATRICS UROLOGY` + `PAEDIATRIC NEUROLOGY` + `PAEDIATRIC ENT` + `PAEDIATRIC DIABETES` + `PAEDIATRIC GASTROENTEROLOGY` +
      `PAEDIATRIC RESPIRATORY` + `PAEDIATRICS NEPHROLOGY` +  PAEDIATRICS + `PAEDIATRIC NEURODISABILITY`
  ) %>%
  mutate(
    `OBSTETRICS AND GYNAECOLOGY` = OBSTETRICS + GYNAECOLOGY + `MATERNITY ANTE-NATAL OUT` + `MIDWIFE LED EPISODE`
  ) %>%
  mutate(`TRAUMA AND OTHOPAEDICS` = ORTHOPAEDICS + TRAUMA) %>%
  mutate(
    `OTHER MEDICAL` = ANAESTHETICS + `CLINICAL ONCOLOGY` + E.N.T + `MEDICAL ONCOLOGY` + NEUROLOGY + OPHTHALMOLOGY + RADIOLOGY + DIABETES
  )

midnight_beds_dch_wide$DAILY_TOTAL <-
  rowSums(midnight_beds_dch_wide[, 2:ncol(midnight_beds_dch_wide)])

drop_dch <-
  c(
    "ANAESTHETICS",
    "BREAST SURGERY",
    "CLINICAL ONCOLOGY",
    "COLORECTAL SURGERY",
    "E.N.T",
    "GYNAECOLOGY",
    "MATERNITY ANTE-NATAL OUT",
    "MAXILLO-FACIAL SURGERY",
    "MEDICAL ONCOLOGY",
    "MIDWIFE LED EPISODE",
    "NEUROLOGY",
    "OBSTETRICS",
    "OPHTHALMOLOGY",
    "ORTHOPAEDICS",
    "TRAUMA",
    "PAEDIATRIC DIABETES",
    "PAEDIATRIC ENT",
    "PAEDIATRIC GASTROENTEROLOGY",
    "PAEDIATRIC NEURODISABILITY",
    "PAEDIATRIC NEUROLOGY",
    "PAEDIATRIC ONCOLOGY",
    "PAEDIATRIC OPHTHALMOLOGY",
    "PAEDIATRIC RESPIRATORY",
    "PAEDIATRIC SURGERY",
    "PAEDIATRIC TRAUMA & ORTH",
    "PAEDIATRICS",
    "PAEDIATRICS NEPHROLOGY",
    "PAEDIATRICS UROLOGY",
    "RADIOLOGY",
    "VASCULAR SURGERY",
    "DIABETES"
  )
midnight_beds_dch_wide = midnight_beds_dch_wide[,!(names(midnight_beds_dch_wide) %in% drop_dch)]

midnight_beds_dch <-
  gather(
    midnight_beds_dch_wide,
    Discharge_Main_Specialty_Desc,
    "Occupied Beds",
    "ACCIDENT & EMERGENCY":DAILY_TOTAL,
    factor_key = T
  )

midnight_beds_dch_graph <-
  gather(
    midnight_beds_dch_wide,
    Discharge_Main_Specialty_Desc,
    "Occupied Beds",
    "ACCIDENT & EMERGENCY":"OTHER MEDICAL",
    factor_key = T
  )


# Date Formatting ---------------------------------------------------------

midnight_beds_dch$`Census Date` <-
  as.Date(midnight_beds_dch$`Census Date`)
midnight_beds_dch_wide$`Census Date` <-
  as.Date(midnight_beds_dch_wide$`Census Date`)
midnight_beds_dch_graph$`Census Date` <-
  as.Date(midnight_beds_dch_graph$`Census Date`)

# Basic Stacked Bar of Overall Occupancy ----------------------------------

ggplot(data = midnight_beds_dch_graph,
       aes(x = `Census Date`, y = `Occupied Beds`, fill = Discharge_Main_Specialty_Desc)) +
  geom_bar(stat = 'identity')


# Setting Data For Use In Model -------------------------------------------


midnight_beds_dch_arima_list <- midnight_beds_dch %>%
  group_by(Discharge_Main_Specialty_Desc) %>%
  nest()

midnight_beds_dch_arima_7 <- midnight_beds_dch_arima_list %>%
  mutate(data_train_7.ts = map(
    .x = data,
    .f = tk_ts,
    start = as.Date("2015-01-01"),
    frequency = 7
  ))

# Generating Model Fit (Auto-ARIMA) and Tidying --------------------------------------------


midnight_beds_dch_arima_fit <-
  midnight_beds_dch_arima_7 %>%
  mutate(fit.arima = map(data_train_7.ts, auto.arima))

midnight_beds_dch_arima_forecast <-
  midnight_beds_dch_arima_fit %>%
  mutate(forecast.arima = map(fit.arima, forecast, h = 60))

midnight_beds_dch_arima_forecast_tidy <-
  midnight_beds_dch_arima_forecast %>%
  mutate(sweep = map(
    forecast.arima,
    sw_sweep,
    fitted = FALSE,
    timetk_idx = FALSE
  )) %>%
  unnest(sweep)

midnight_beds_dch_arima_forecast_tidy$date_value <-
  rep(seq.Date(
    from = as.Date("2015-01-01"),
    by = "1 day",
    length.out = 2038
  ), times = 18)

midnight_beds_dch_arima_forecast_tidy$`Occupied Beds`[midnight_beds_dch_arima_forecast_tidy$`Occupied Beds` <
                                                        0] <-
  0
midnight_beds_dch_arima_forecast_tidy$lo.80[midnight_beds_dch_arima_forecast_tidy$lo.80 <
                                              0] <- 0
midnight_beds_dch_arima_forecast_tidy$lo.95 [midnight_beds_dch_arima_forecast_tidy$lo.95 <
                                               0] <- 0

# Plotting Model Outputs (showing 01/20 onwards) --------------------------


midnight_beds_dch_arima_forecast_tidy %>%
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
    subtitle = "Auto-Arima Model",
    x = "Date",
    y = "Beds"
  ) +
  scale_x_date(breaks = "1 month", limits = c(as.Date("2020-01-01"), as.Date("2020-07-01"))) +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap( ~ Discharge_Main_Specialty_Desc,
              scales = "free_y",
              ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
