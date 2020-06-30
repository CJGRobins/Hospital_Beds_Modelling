#without taking into account admission type
#Making dataset for machine learning


library(nnfor)
library(tidyr)
library(dplyr)
library(readxl)
library(mltools)
library(data.table)
library(lubridate)
library(ggplot2)
library(forecast)
library(readr)
library(readxl)
library(foreach)
library(forecast)
library(deSolve)
library(tidyverse)
library(scales)
library(stringr)
library(sjmisc)
library(stringdist)
library(simmer)
library(simmer.plot)
library(expm)
library(timetk)
library(tsbox)
library(sweep)
library(tidyquant)
library(plotly)
admissions_daily <- read_csv(
  "admissions_daily.csv",
  col_types = cols(
    `Admission Date` = col_date(format = "%d/%m/%Y"),
    `Discharge Date` = col_date(format = "%d/%m/%Y")
  )
)
#View(admissions_daily)
# allowing the totalling to accurately reflect current occupancy
admissions_daily <- admissions_daily %>%
  filter(!is.na(`Admission Date`),
         !is.na(`Admission Hour`),
         !is.na(Discharge_Main_Specialty_Desc))

admissions_daily$diagnosis_1 <- as.character(admissions_daily$diagnosis_1)
admissions_daily$`ICD10 Short` <- as.character(admissions_daily$`ICD10 Short`)
admissions_daily$Discharge_Main_Specialty_Desc <- as.character(admissions_daily$Discharge_Main_Specialty_Desc)

admissions_daily$diagnosis_1[is.na(admissions_daily$diagnosis_1)] <- "To Be Coded"
admissions_daily$`ICD10 Short`[is.na(admissions_daily$`ICD10 Short`)] <- "To Be Coded"


##fixing data for patients currently in hospital
admissions_daily$`Discharge Hour`[is.na(admissions_daily$`Discharge Hour`)] <- 14
admissions_daily$`Discharge Date` <- as.Date(admissions_daily$`Discharge Date`)
admissions_daily$`Discharge Date`[is.na(admissions_daily$`Discharge Date`)] <- as.Date("2099-01-01")

#admissions_daily <- admissions_daily %>%
# filter(`Admission Type` == "NE")

#admissions_daily$Discharge_Main_Specialty_Desc <- ifelse(admissions_daily$diagnosis_1 == "U071", admissions_daily$diagnosis_1, admissions_daily$Discharge_Main_Specialty_Desc)
#admissions_daily <- admissions_daily %>% 
#  mutate(Discharge_Main_Specialty_Desc = recode(Discharge_Main_Specialty_Desc, U071 = "COVID"))
admissions_daily$`Admission Date`<- as.Date(admissions_daily$`Admission Date`)
admissions_daily$`Discharge Date` <- as.Date(admissions_daily$`Discharge Date`)

admissions_daily$`Admission Quarter` <- ifelse(admissions_daily$`Admission Hour`<=5,1 ,
                                               ifelse(admissions_daily$`Admission Hour`>5 & admissions_daily$`Admission Hour` <=11, 2, 
                                                      ifelse(admissions_daily$`Admission Hour`>11 & admissions_daily$`Admission Hour`<=17,3, 4)))
#1 = 12-6am, 2 = 6-12 am, 3 = 12-6pm, 4 = 6-12midnight
admissions_daily$`Admission Quarter` <- as.numeric(admissions_daily$`Admission Quarter`)
admissions_daily$`Discharge Quarter` <- ifelse(admissions_daily$`Discharge Hour`<=5, 1 ,
                                               ifelse(admissions_daily$`Discharge Hour`>5 & admissions_daily$`Discharge Hour` <=11, 2, 
                                                      ifelse(admissions_daily$`Discharge Hour`>11 & admissions_daily$`Discharge Hour`<=17,3, 4)))
admissions_daily$`Discharge Quarter` <- as.numeric(admissions_daily$`Discharge Quarter`)
admissions_daily$dummy_var <- 1
admissions_daily$`Admission Date` <- as.Date(admissions_daily$`Admission Date`)
admissions_daily$`Discharge Date` <- as.Date(admissions_daily$`Discharge Date`)

#create a list of ever represented specialty
other_list <- c("CHEMICAL PATHOLOGY", 
                "CHILD AND ADOLESCENT PSYCH", 
                "CLINICAL GENETICS", 
                "CLINICAL IMMUN AND ALLERGY", 
                "EAR NOSE AND THROAT",
                "ENDOCRINOLOGY",
                "GENERAL PRACTICE",
                "NEPHROLOGY",
                "NEUROLOGY",
                "OBSTETRICS",
                "ORAL SURGERY",
                "PAEDIATRIC NEUROLOGY",
                "PAEDIATRICS",
                "RADIOLOGY", 
                "GENITO-URINARY MEDICINE",
                "ANAESTHETICS",
                "RHEUMATOLOGY",
                "MIDWIFE EPISODE"
)


foreach(i = other_list) %do% {
  admissions_daily$Discharge_Main_Specialty_Desc <-
    ifelse(
      admissions_daily$Discharge_Main_Specialty_Desc == i,
      "OTHER",
      admissions_daily$Discharge_Main_Specialty_Desc
    )
}
admissions_daily$Discharge_Main_Specialty_Desc <- ifelse(admissions_daily$Discharge_Main_Specialty_Desc == "CLINICAL ONCOLOGY", "ONCOLOGY", admissions_daily$Discharge_Main_Specialty_Desc)
admissions_daily$Discharge_Main_Specialty_Desc <- ifelse(admissions_daily$Discharge_Main_Specialty_Desc == "MEDICAL ONCOLOGY", "ONCOLOGY", admissions_daily$Discharge_Main_Specialty_Desc)

Specialty_list <- unique(admissions_daily$Discharge_Main_Specialty_Desc)


admissions_daily_subset <- subset(admissions_daily, select = c(`Admission Date`, `Admission Quarter`, `Discharge Date`, `Discharge Quarter`, Discharge_Main_Specialty_Desc, dummy_var))
inflow <- admissions_daily_subset %>%
  group_by(`Admission Date`, `Admission Quarter`)
inflow <- subset(inflow, select = -c(`Discharge Date`, `Discharge Quarter`))  

inflow_spec <- inflow %>%
  group_by(`Admission Date`, `Admission Quarter`, Discharge_Main_Specialty_Desc) %>%
  summarise(Count = sum(dummy_var)) 

inflow_wide <- spread(inflow_spec, Discharge_Main_Specialty_Desc, Count)
inflow_wide[is.na(inflow_wide)] <- 0
inflow_wide$total <- rowSums(inflow_wide[,3:ncol(inflow_wide)])
inflow_wide[is.na(inflow_wide)] <- 0



inflow_wide <- inflow_wide %>% dplyr::rename_all(function(x) paste0("in_", x))
inflow_wide_merge <- inflow_wide %>% 
  rename(Date_value = `in_Admission Date`) %>%
  rename(Day_quarter = `in_Admission Quarter`)


######################################################################################
outflow <- admissions_daily_subset %>%
  group_by(`Admission Date`)
outflow <- subset(outflow, select = -c(`Admission Date`, `Admission Quarter`))  

outflow_spec <- outflow %>%
  group_by(`Discharge Date`, `Discharge Quarter`, Discharge_Main_Specialty_Desc) %>%
  summarise(Count = sum(dummy_var)) 

outflow_wide <- spread(outflow_spec, Discharge_Main_Specialty_Desc, Count)
outflow_wide[is.na(outflow_wide)] <- 0
outflow_wide$total <- rowSums(outflow_wide[,3:ncol(outflow_wide)])
outflow_wide[is.na(outflow_wide)] <- 0
outflow_wide[,3:ncol(outflow_wide)] <- lapply(outflow_wide[,3:ncol(outflow_wide)], function(x) -1 * x)

outflow_wide <- outflow_wide %>% dplyr::rename_all(function(x) paste0("out_", x))
outflow_wide_merge <- outflow_wide %>% 
  rename(Date_value = `out_Discharge Date`)%>%
  rename(Day_quarter = `out_Discharge Quarter`)
###############################

in_out_depts <- merge(inflow_wide_merge, outflow_wide_merge, by=c("Date_value", "Day_quarter"), all = TRUE)
in_out_depts[is.na(in_out_depts)] <-0
in_out_depts$D_O_W <- weekdays(in_out_depts$Date_value)
in_out_depts$D_O_W <- as.factor(in_out_depts$D_O_W)
###################

nth_week<- function(dates = NULL,
                    count_weeks_in = c("month","year"),
                    begin_week_on = "Sunday"){
  
  require(lubridate)
  
  count_weeks_in<- tolower(count_weeks_in[1])
  
  # day_names and day_index are for beginning the week on a day other than Sunday
  # (this vector ordering matters, so careful about changing it)
  day_names<- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  
  # index integer of first match
  day_index<- pmatch(tolower(begin_week_on),
                     tolower(day_names))[1]
  
  
  ### Calculate week index of each day
  
  if (!is.na(pmatch(count_weeks_in, "year"))) {
    
    # For year:
    # sum the day of year, index for day of week at start of year, and constant 5 
    #  then integer divide quantity by 7   
    # (explicit on package so lubridate and data.table don't fight)
    n_week<- (5 + 
                lubridate::yday(dates) + 
                lubridate::wday(floor_date(dates, 'year'), 
                                week_start = day_index)
    ) %/% 7
    
  } else {
    
    # For month:
    # same algorithm as above, but for month rather than year
    n_week<- (5 + 
                lubridate::day(dates) + 
                lubridate::wday(floor_date(dates, 'month'), 
                                week_start = day_index)
    ) %/% 7
    
  }
  
  # naming very helpful for review
  names(n_week)<- paste0(lubridate::wday(dates,T), '-', dates)
  
  n_week
  
}

library(collapse)

in_out_depts$W_O_M <- nth_week(in_out_depts$Date_value)
in_out_depts$W_O_M <- as.factor(in_out_depts$W_O_M)
in_out_depts$MONTH <- lubridate::month(in_out_depts$Date_value, label = TRUE, abbr = TRUE)
in_out_depts$MONTH <- factor(in_out_depts$MONTH, ordered = FALSE)
in_out_depts$Day_quarter <- recode(in_out_depts$Day_quarter , `1` = "12PM-6AM", `2` = "6AM-12AM", `3` = "12AM-6PM", `4` = "6PM-12PM")
in_out_depts$Day_quarter <- factor(in_out_depts$Day_quarter)

in_out_depts$daily_total <- in_out_depts$in_total + in_out_depts$out_total
in_out_depts$total <- cumsum(in_out_depts$daily_total)

in_out_depts$`daily_GENERAL SURGERY` <- in_out_depts$`in_GENERAL SURGERY` + in_out_depts$`out_GENERAL SURGERY`
in_out_depts$`GENERAL SURGERY` <- cumsum(in_out_depts$`daily_GENERAL SURGERY`)

in_out_depts$`daily_GERIATRIC MEDICINE` <- in_out_depts$`in_GERIATRIC MEDICINE` + in_out_depts$`out_GERIATRIC MEDICINE`
in_out_depts$`GERIATRIC MEDICINE` <- cumsum(in_out_depts$`daily_GERIATRIC MEDICINE`)

in_out_depts$`daily_THORACIC MEDICINE` <- in_out_depts$`in_THORACIC MEDICINE` + in_out_depts$`out_THORACIC MEDICINE`
in_out_depts$`THORACIC MEDICINE` <- cumsum(in_out_depts$`daily_THORACIC MEDICINE`)

in_out_depts$`daily_ACCIDENT AND EMERGENCY` <- in_out_depts$`in_ACCIDENT AND EMERGENCY` + in_out_depts$`out_ACCIDENT AND EMERGENCY`
in_out_depts$`ACCIDENT AND EMERGENCY` <- cumsum(in_out_depts$`daily_ACCIDENT AND EMERGENCY`)

in_out_depts$daily_UROLOGY <- in_out_depts$in_UROLOGY + in_out_depts$out_UROLOGY
in_out_depts$UROLOGY <- cumsum(in_out_depts$daily_UROLOGY)

in_out_depts$`daily_GENERAL MEDICINE` <- in_out_depts$`in_GENERAL MEDICINE` + in_out_depts$`out_GENERAL MEDICINE`
in_out_depts$`GENERAL MEDICINE` <- cumsum(in_out_depts$`daily_GENERAL MEDICINE`)

in_out_depts$daily_OTHER <- in_out_depts$in_OTHER + in_out_depts$out_OTHER
in_out_depts$OTHER <- cumsum(in_out_depts$daily_OTHER)

in_out_depts$daily_CARDIOLOGY <- in_out_depts$in_CARDIOLOGY + in_out_depts$out_CARDIOLOGY
in_out_depts$CARDIOLOGY <- cumsum(in_out_depts$daily_CARDIOLOGY)

in_out_depts$daily_OPHTHALMOLOGY <- in_out_depts$in_OPHTHALMOLOGY + in_out_depts$out_OPHTHALMOLOGY
in_out_depts$OPHTHALMOLOGY <- cumsum(in_out_depts$daily_OPHTHALMOLOGY)

in_out_depts$daily_GYNAECOLOGY <- in_out_depts$in_GYNAECOLOGY + in_out_depts$out_GYNAECOLOGY
in_out_depts$GYNAECOLOGY <- cumsum(in_out_depts$daily_GYNAECOLOGY)

in_out_depts$daily_RHEUMATOLOGY <- in_out_depts$in_RHEUMATOLOGY + in_out_depts$out_RHEUMATOLOGY
in_out_depts$RHEUMATOLOGY <- cumsum(in_out_depts$daily_RHEUMATOLOGY)

in_out_depts$daily_DERMATOLOGY <- in_out_depts$in_DERMATOLOGY + in_out_depts$out_DERMATOLOGY
in_out_depts$DERMATOLOGY <- cumsum(in_out_depts$daily_DERMATOLOGY)

in_out_depts$daily_GASTROENTEROLOGY <- in_out_depts$in_GASTROENTEROLOGY + in_out_depts$out_GASTROENTEROLOGY
in_out_depts$GASTROENTEROLOGY <- cumsum(in_out_depts$daily_GASTROENTEROLOGY)

in_out_depts$daily_REHABILITATION <- in_out_depts$in_REHABILITATION + in_out_depts$out_REHABILITATION
in_out_depts$REHABILITATION <- cumsum(in_out_depts$daily_REHABILITATION)

in_out_depts$daily_ANAESTHETICS <- in_out_depts$in_ANAESTHETICS + in_out_depts$out_ANAESTHETICS
in_out_depts$ANAESTHETICS <- cumsum(in_out_depts$daily_ANAESTHETICS)

#in_out_depts$daily_COVID <- in_out_depts$in_COVID + in_out_depts$out_COVID
#in_out_depts$COVID <- cumsum(in_out_depts$daily_COVID)

in_out_depts$daily_ONCOLOGY <- in_out_depts$in_ONCOLOGY + in_out_depts$out_ONCOLOGY
in_out_depts$ONCOLOGY <- cumsum(in_out_depts$daily_ONCOLOGY)

in_out_depts$`daily_MIDWIFE EPISODE` <- in_out_depts$`in_MIDWIFE EPISODE` + in_out_depts$`out_MIDWIFE EPISODE`
in_out_depts$`MIDWIFE EPISODE` <- cumsum(in_out_depts$`daily_MIDWIFE EPISODE`)

in_out_depts$`daily_HAEMATOLOGY (CLINICAL)` <- in_out_depts$`in_HAEMATOLOGY (CLINICAL)` + in_out_depts$`out_HAEMATOLOGY (CLINICAL)`
in_out_depts$`HAEMATOLOGY (CLINICAL)` <- cumsum(in_out_depts$`daily_HAEMATOLOGY (CLINICAL)`)

in_out_depts$`daily_TRAUMA AND ORTHOPAEDICS` <- in_out_depts$`in_TRAUMA AND ORTHOPAEDICS` + in_out_depts$`out_TRAUMA AND ORTHOPAEDICS`
in_out_depts$`TRAUMA AND ORTHOPAEDICS` <- cumsum(in_out_depts$`daily_TRAUMA AND ORTHOPAEDICS`)

in_out_depts$`daily_PALLIATIVE MEDICINE` <- in_out_depts$`in_PALLIATIVE MEDICINE` + in_out_depts$`out_PALLIATIVE MEDICINE`
in_out_depts$`PALLIATIVE MEDICINE` <- cumsum(in_out_depts$`daily_PALLIATIVE MEDICINE`)

in_out_depts$`daily_GENITO-URINARY MEDICINE` <- in_out_depts$`in_GENITO-URINARY MEDICINE` + in_out_depts$`out_GENITO-URINARY MEDICINE`
in_out_depts$`GENITO-URINARY MEDICINE` <- cumsum(in_out_depts$`daily_GENITO-URINARY MEDICINE`)


#REMOVE THE LAST LINE IN THE DATASET (PLACEHOLDER FOR PTS STILL IN HOSPITAL)

in_out_depts <- slice(in_out_depts, 1:(n()-1))

#CHANGE THE "TO" DATE TO THE LAST REAL OBSERVATION IN YOUR DATASET 
#NOT THE DUMMY DATE FOR PTS IN HOSPITAL

in_out_depts$new_date <- seq.POSIXt(from = as.POSIXct(in_out_depts$Date_value[1]),
                                    length.out = nrow(in_out_depts),
                                    by = "6 hours")
library(directlabels)

#PLOT OF COVID V TOTAL EMERGENCY ADMISSIONS

ggplot(in_out_depts) +
  geom_line(aes(y=total, x=new_date, colour= "All Occupancy")) +
  xlab("Date") +
  ylab("Occupied Beds") +
  scale_x_datetime(date_breaks = "4 weeks", date_labels = "%Y-%m-%d", limit=c(as.POSIXct("2020-01-01 00:00", tz="UTC"),as.POSIXct("2020-05-18 12:00", tz="UTC"))) +
  geom_line(aes(x = new_date, y = COVID, colour= "U071 Code Patients")) +
  geom_hline(yintercept = 12, linetype="dashed", color= "red3") +
  scale_color_manual(values = c(
    'All Occupancy' = 'forestgreen',
    'U071 Code Patients' = 'darkorchid3')) +
  labs(color = 'Occupancy Type')




in_out_depts_oh <- one_hot(as.data.table(in_out_depts), cols = "auto", dropCols = TRUE)

#write.csv(in_out_depts_oh, "hospital_in_out.csv")

library(timetk)
library(sweep)

time_and_total <- subset(in_out_depts_oh, select = c(new_date, total))
spec_occupancy <- subset(in_out_depts_oh, select = c(Specialty_list))
total_ts <- cbind(time_and_total, spec_occupancy)

total_ts <- slice(total_ts, 240:n())


total_ts_dept <-
  gather(
    total_ts,
    Specialty,
    "Occupied Beds",
    total:ncol(total_ts),
    factor_key = T
  )
# Setting Data For Use In Model -------------------------------------------


total_ts_dept_tbats_list <- total_ts_dept %>%
  select(-new_date) %>%
  group_by(Specialty) %>%
  nest()

total_ts_dept_tbats_7 <- total_ts_dept_tbats_list %>%
  mutate(data_train_7_msts = map(
    .x = data,
    .f = msts,
    start = total_ts_dept$new_date[1],
    seasonal.periods = c(28, 1461)
  ))

# Generating Model Fit (TBATS) and Tidying --------------------------------------------


total_ts_dept_tbats_fit <-
  total_ts_dept_tbats_7 %>%
  mutate(fit.tbats = map(data_train_7_msts, tbats))

total_ts_dept_tbats_forecast <-
  total_ts_dept_tbats_fit %>%
  mutate(forecast.tbats = map(fit.tbats, forecast, h = 60))

total_ts_dept_tbats_forecast_tidy <-
  total_ts_dept_tbats_forecast %>%
  mutate(sweep = map(
    forecast.tbats,
    sw_sweep,
    fitted = FALSE,
    timetk_idx = FALSE
  )) %>%
  unnest(sweep)

total_ts_dept_tbats_forecast_tidy$date_value <-
  rep(seq.POSIXt(
    from = total_ts_dept$new_date[1],
    by = "6 hours",
    length.out = nrow(total_ts_dept_tbats_forecast_tidy)/n_groups(total_ts_dept_tbats_forecast_tidy)
  ), times = n_groups(total_ts_dept_tbats_forecast_tidy))

total_ts_dept_tbats_forecast_tidy$`Occupied Beds`[total_ts_dept_tbats_forecast_tidy$`Occupied Beds` <
                                                      0] <-
  0
total_ts_dept_tbats_forecast_tidy$lo.80[total_ts_dept_tbats_forecast_tidy$lo.80 <
                                            0] <- 0
total_ts_dept_tbats_forecast_tidy$lo.95 [total_ts_dept_tbats_forecast_tidy$lo.95 <
                                             0] <- 0

total_ts_dept_tbats_forecast_tidy_export <- total_ts_dept_tbats_forecast_tidy %>%
  select(date_value, Specialty, key, `Occupied Beds`, lo.80, lo.95, hi.80, hi.95)

write.csv(total_ts_dept_tbats_forecast_tidy_export, "all_6h_tbats.csv")


total_ts_dept_tbats_forecast_tidy %>%
  ggplot(
    aes(
      x = date_value,
      y = `Occupied Beds`,
      color = key,
      group = Specialty
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
    subtitle = "TBATS Model",
    x = "Date",
    y = "Beds"
  ) +
  scale_x_datetime(breaks = "1 month", limits = c(as.POSIXct("2020-01-01", tz = "UTC"), as.POSIXct("2020-07-01", tz = "UTC"))) +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap( ~ Specialty,
              scales = "free_y",
              ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


