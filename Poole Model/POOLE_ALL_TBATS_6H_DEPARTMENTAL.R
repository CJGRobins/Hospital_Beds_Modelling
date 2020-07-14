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

admissions_daily_poole_all <- read_csv(
  "admissions_daily_poole.csv",
  col_types = cols(
    `Admission Date` = col_date(format = "%d/%m/%Y"),
    `Discharge Date` = col_date(format = "%d/%m/%Y")
  )
)
#View(admissions_daily_poole_all)
# allowing the totalling to accurately reflect current occupancy
admissions_daily_poole_all <- admissions_daily_poole %>%
  filter(
    !is.na(`Admission Date`),!is.na(`Admission Hour`),!is.na(`Main Discharge Specialty`)
  )

#admissions_daily_poole_all <- admissions_daily_poole_all %>%
 # filter(`Admission Type` == "NE")
admissions_daily_poole_all$diagnosis_1 <-
  as.character(admissions_daily_poole_all$diagnosis_1)
admissions_daily_poole_all$`ICD10 Short` <-
  as.character(admissions_daily_poole_all$`ICD10 Short`)
admissions_daily_poole_all$`Main Discharge Specialty` <-
  as.character(admissions_daily_poole_all$`Main Discharge Specialty`)

admissions_daily_poole_all$diagnosis_1[is.na(admissions_daily_poole_all$diagnosis_1)] <-
  "To Be Coded"
admissions_daily_poole_all$`ICD10 Short`[is.na(admissions_daily_poole_all$`ICD10 Short`)] <-
  "To Be Coded"


##fixing data for patients currently in hospital
admissions_daily_poole_all$`Discharge Hour`[is.na(admissions_daily_poole_all$`Discharge Hour`)] <-
  14
admissions_daily_poole_all$`Discharge Date` <-
  as.Date(admissions_daily_poole_all$`Discharge Date`)
admissions_daily_poole_all$`Discharge Date`[is.na(admissions_daily_poole_all$`Discharge Date`)] <-
  as.Date("2099-01-01")


admissions_daily_poole_all$`Admission Date` <-
  as.Date(admissions_daily_poole_all$`Admission Date`)
admissions_daily_poole_all$`Discharge Date` <-
  as.Date(admissions_daily_poole_all$`Discharge Date`)

admissions_daily_poole_all$`Admission Quarter` <-
  ifelse(
    admissions_daily_poole_all$`Admission Hour` <= 5,
    1 ,
    ifelse(
      admissions_daily_poole_all$`Admission Hour` > 5 &
        admissions_daily_poole_all$`Admission Hour` <= 11,
      2,
      ifelse(
        admissions_daily_poole_all$`Admission Hour` > 11 &
          admissions_daily_poole_all$`Admission Hour` <= 17,
        3,
        4
      )
    )
  )
#1 = 12-6am, 2 = 6-12 am, 3 = 12-6pm, 4 = 6-12midnight
admissions_daily_poole_all$`Admission Quarter` <-
  as.numeric(admissions_daily_poole_all$`Admission Quarter`)
admissions_daily_poole_all$`Discharge Quarter` <-
  ifelse(
    admissions_daily_poole_all$`Discharge Hour` <= 5,
    1 ,
    ifelse(
      admissions_daily_poole_all$`Discharge Hour` > 5 &
        admissions_daily_poole_all$`Discharge Hour` <= 11,
      2,
      ifelse(
        admissions_daily_poole_all$`Discharge Hour` > 11 &
          admissions_daily_poole_all$`Discharge Hour` <= 17,
        3,
        4
      )
    )
  )
admissions_daily_poole_all$`Discharge Quarter` <-
  as.numeric(admissions_daily_poole_all$`Discharge Quarter`)

admissions_daily_poole_all$dummy_var <- 1

admissions_daily_poole_all$`Admission Date` <-
  as.Date(admissions_daily_poole_all$`Admission Date`)
admissions_daily_poole_all$`Discharge Date` <-
  as.Date(admissions_daily_poole_all$`Discharge Date`)

#create a list of everY represented specialty
other_list_poole_all <- c("BONE MARROW TRANSPLANTATION",
                      "NEUROSURGERY",
                      "RESTORATIVE DENTISTRY",
                      "SPEECH AND LANGUAGE THERAPY",
                      "UROLOGY",
                      "ANAESTHETICS",
                      "COLORECTAL SURGERY",
                      "UPPER GASTROINTESTINAL SURGERY",
                      "PAIN MANAGEMENT",
                      "BREAST SURGERY",
                      "SPECIALIST REHABILITION SERV",
                      "DERMATOLOGY",
                      "ADULT CYSTIC FIBROSIS",
                      "RHEUMATOLOGY", 
                      "ORAL AND MAXILLOFACIAL SURGERY",              
                      "DIABETIC MEDICINE",
                      "REHABILITATION",                   
                      "ORAL SURGERY",     
                      "TRANSIENT ISCHAEMIC ATTACK",
                      "NEUROLOGY",
                      "THORACIC MEDICINE",
                      "GASTROENTEROLOGY",
                      "PALLIATIVE MEDICINE",
                      "HAEMATOLOGY (CLINICAL)", 
                      "INTERVENTIONAL RADIOLOGY",
                      "OPHTHALMOLOGY"
)

foreach(i = other_list_poole_all) %do% {
  admissions_daily_poole_all$`Main Discharge Specialty` <-
    ifelse(
      admissions_daily_poole_all$`Main Discharge Specialty` == i,
      "OTHER",
      admissions_daily_poole_all$`Main Discharge Specialty`
    )
}
admissions_daily_poole_all$`Main Discharge Specialty` <-
  ifelse(
    admissions_daily_poole_all$`Main Discharge Specialty` == "WELL BABIES",
    "MATERNITY AND NEONATOLOGY",
    admissions_daily_poole_all$`Main Discharge Specialty`
  )
admissions_daily_poole_all$`Main Discharge Specialty` <-
  ifelse(
    admissions_daily_poole_all$`Main Discharge Specialty` == "NEONATOLOGY",
    "MATERNITY AND NEONATOLOGY",
    admissions_daily_poole_all$`Main Discharge Specialty`
  )
admissions_daily_poole_all$`Main Discharge Specialty` <-
  ifelse(
    admissions_daily_poole_all$`Main Discharge Specialty` == "OBSTETRICS",
    "OBSTETRICS AND GYNAECOLOGY",
    admissions_daily_poole_all$`Main Discharge Specialty`
  )
admissions_daily_poole_all$`Main Discharge Specialty` <-
  ifelse(
    admissions_daily_poole_all$`Main Discharge Specialty` == "GYNAECOLOGY",
    "OBSTETRICS AND GYNAECOLOGY",
    admissions_daily_poole_all$`Main Discharge Specialty`
  )
admissions_daily_poole_all$`Main Discharge Specialty` <-
  ifelse(
    admissions_daily_poole_all$`Main Discharge Specialty` == "CLINICAL ONCOLOGY",
    "ONCOLOGY",
    admissions_daily_poole_all$`Main Discharge Specialty`
  )
admissions_daily_poole_all$`Main Discharge Specialty` <-
  ifelse(
    admissions_daily_poole_all$`Main Discharge Specialty` == "MEDICAL ONCOLOGY",
    "ONCOLOGY",
    admissions_daily_poole_all$`Main Discharge Specialty`
  )
admissions_daily_poole_all$`Main Discharge Specialty` <-
  ifelse(
    admissions_daily_poole_all$`Main Discharge Specialty` == "GYNAECOLOGY ONCOLOGY",
    "ONCOLOGY",
    admissions_daily_poole_all$`Main Discharge Specialty`
  )
paeds_list <- c(
  "PAEDIATRIC ONCOLOGY",
  "PAEDIATRIC DIABETES",
  "PAEDIATRIC CARDIOLOGY",
  "PAEDIATRIC SURGERY",
  "PAEDIATRIC EPILEPSY",
  "PAEDIATRIC DERMATOLOGY",
  "PAEDIATRIC NEUROLOGY",
  "PAEDIATRIC CYSTIC FIBROSIS"
)
foreach(i = paeds_list) %do% {
  admissions_daily_poole_all$`Main Discharge Specialty` <-
    ifelse(
      admissions_daily_poole_all$`Main Discharge Specialty` == i,
      "PAEDIATRICS",
      admissions_daily_poole_all$`Main Discharge Specialty`
    )
}


Specialty_list_poole_all <-
  unique(admissions_daily_poole_all$`Main Discharge Specialty`)


admissions_daily_poole_all_subset <-
  subset(
    admissions_daily_poole_all,
    select = c(
      `Admission Date`,
      `Admission Quarter`,
      `Discharge Date`,
      `Discharge Quarter`,
      `Main Discharge Specialty`,
      dummy_var
    )
  )
inflow_poole_all <- admissions_daily_poole_all_subset %>%
  group_by(`Admission Date`, `Admission Quarter`)
inflow_poole_all <-
  subset(inflow_poole_all, select = -c(`Discharge Date`, `Discharge Quarter`))

inflow_poole_all_spec <- inflow_poole_all %>%
  group_by(`Admission Date`,
           `Admission Quarter`,
           `Main Discharge Specialty`) %>%
  summarise(Count = sum(dummy_var))

inflow_poole_all_wide <-
  spread(inflow_poole_all_spec, `Main Discharge Specialty`, Count)
inflow_poole_all_wide[is.na(inflow_poole_all_wide)] <- 0
inflow_poole_all_wide$total <- rowSums(inflow_poole_all_wide[, 3:ncol(inflow_poole_all_wide)])
inflow_poole_all_wide[is.na(inflow_poole_all_wide)] <- 0



inflow_poole_all_wide <-
  inflow_poole_all_wide %>% dplyr::rename_all(function(x)
    paste0("in_", x))
inflow_poole_all_wide_merge <- inflow_poole_all_wide %>%
  rename(Date_value = `in_Admission Date`) %>%
  rename(Day_quarter = `in_Admission Quarter`)


######################################################################################
outflow_poole_all <- admissions_daily_poole_all_subset %>%
  group_by(`Admission Date`)
outflow_poole_all <-
  subset(outflow_poole_all, select = -c(`Admission Date`, `Admission Quarter`))

outflow_poole_all_spec <- outflow_poole_all %>%
  group_by(`Discharge Date`,
           `Discharge Quarter`,
           `Main Discharge Specialty`) %>%
  summarise(Count = sum(dummy_var))

outflow_poole_all_wide <-
  spread(outflow_poole_all_spec, `Main Discharge Specialty`, Count)
outflow_poole_all_wide[is.na(outflow_poole_all_wide)] <- 0
outflow_poole_all_wide$total <- rowSums(outflow_poole_all_wide[, 3:ncol(outflow_poole_all_wide)])
outflow_poole_all_wide[is.na(outflow_poole_all_wide)] <- 0
outflow_poole_all_wide[, 3:ncol(outflow_poole_all_wide)] <-
  lapply(outflow_poole_all_wide[, 3:ncol(outflow_poole_all_wide)], function(x)
    - 1 * x)

outflow_poole_all_wide <-
  outflow_poole_all_wide %>% dplyr::rename_all(function(x)
    paste0("out_", x))
outflow_poole_all_wide_merge <- outflow_poole_all_wide %>%
  rename(Date_value = `out_Discharge Date`) %>%
  rename(Day_quarter = `out_Discharge Quarter`)
###############################

in_out_depts_poole_all <-
  merge(
    inflow_poole_all_wide_merge,
    outflow_poole_all_wide_merge,
    by = c("Date_value", "Day_quarter"),
    all = TRUE
  )
in_out_depts_poole_all[is.na(in_out_depts_poole_all)] <- 0
in_out_depts_poole_all$D_O_W <- weekdays(in_out_depts_poole_all$Date_value)
in_out_depts_poole_all$D_O_W <- as.factor(in_out_depts_poole_all$D_O_W)
###################

nth_week <- function(dates = NULL,
                     count_weeks_in = c("month", "year"),
                     begin_week_on = "Sunday") {
  require(lubridate)
  
  count_weeks_in <- tolower(count_weeks_in[1])
  
  # day_names and day_index are for beginning the week on a day other than Sunday
  # (this vector ordering matters, so careful about changing it)
  day_names <-
    c("Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday")
  
  # index integer of first match
  day_index <- pmatch(tolower(begin_week_on),
                      tolower(day_names))[1]
  
  
  ### Calculate week index of each day
  
  if (!is.na(pmatch(count_weeks_in, "year"))) {
    # For year:
    # sum the day of year, index for day of week at start of year, and constant 5
    #  then integer divide quantity by 7
    # (explicit on package so lubridate and data.table don't fight)
    n_week <- (5 +
                 lubridate::yday(dates) +
                 lubridate::wday(floor_date(dates, 'year'),
                                 week_start = day_index)) %/% 7
    
  } else {
    # For month:
    # same algorithm as above, but for month rather than year
    n_week <- (5 +
                 lubridate::day(dates) +
                 lubridate::wday(floor_date(dates, 'month'),
                                 week_start = day_index)) %/% 7
    
  }
  
  # naming very helpful for review
  names(n_week) <- paste0(lubridate::wday(dates, T), '-', dates)
  
  n_week
  
}

library(collapse)

in_out_depts_poole_all$W_O_M <- nth_week(in_out_depts_poole_all$Date_value)
in_out_depts_poole_all$W_O_M <- as.factor(in_out_depts_poole_all$W_O_M)
in_out_depts_poole_all$MONTH <-
  lubridate::month(in_out_depts_poole_all$Date_value, label = TRUE, abbr = TRUE)
in_out_depts_poole_all$MONTH <- factor(in_out_depts_poole_all$MONTH, ordered = FALSE)
in_out_depts_poole_all$Day_quarter <-
  recode(
    in_out_depts_poole_all$Day_quarter ,
    `1` = "12PM-6AM",
    `2` = "6AM-12AM",
    `3` = "12AM-6PM",
    `4` = "6PM-12PM"
  )
in_out_depts_poole_all$Day_quarter <- factor(in_out_depts_poole_all$Day_quarter)

in_out_depts_poole_all$daily_total <-
  in_out_depts_poole_all$in_total + in_out_depts_poole_all$out_total
in_out_depts_poole_all$total <- cumsum(in_out_depts_poole_all$daily_total)

in_out_depts_poole_all$`daily_GENERAL SURGERY` <-
  in_out_depts_poole_all$`in_GENERAL SURGERY` + in_out_depts_poole_all$`out_GENERAL SURGERY`
in_out_depts_poole_all$`GENERAL SURGERY` <-
  cumsum(in_out_depts_poole_all$`daily_GENERAL SURGERY`)

in_out_depts_poole_all$`daily_GERIATRIC MEDICINE` <-
  in_out_depts_poole_all$`in_GERIATRIC MEDICINE` + in_out_depts_poole_all$`out_GERIATRIC MEDICINE`
in_out_depts_poole_all$`GERIATRIC MEDICINE` <-
  cumsum(in_out_depts_poole_all$`daily_GERIATRIC MEDICINE`)


in_out_depts_poole_all$`daily_ACCIDENT AND EMERGENCY` <-
  in_out_depts_poole_all$`in_ACCIDENT AND EMERGENCY` + in_out_depts_poole_all$`out_ACCIDENT AND EMERGENCY`
in_out_depts_poole_all$`ACCIDENT AND EMERGENCY` <-
  cumsum(in_out_depts_poole_all$`daily_ACCIDENT AND EMERGENCY`)


in_out_depts_poole_all$`daily_GENERAL MEDICINE` <-
  in_out_depts_poole_all$`in_GENERAL MEDICINE` + in_out_depts_poole_all$`out_GENERAL MEDICINE`
in_out_depts_poole_all$`GENERAL MEDICINE` <-
  cumsum(in_out_depts_poole_all$`daily_GENERAL MEDICINE`)

in_out_depts_poole_all$daily_OTHER <-
  in_out_depts_poole_all$in_OTHER + in_out_depts_poole_all$out_OTHER
in_out_depts_poole_all$OTHER <- cumsum(in_out_depts_poole_all$daily_OTHER)

in_out_depts_poole_all$daily_CARDIOLOGY <-
  in_out_depts_poole_all$in_CARDIOLOGY + in_out_depts_poole_all$out_CARDIOLOGY
in_out_depts_poole_all$CARDIOLOGY <- cumsum(in_out_depts_poole_all$daily_CARDIOLOGY)

in_out_depts_poole_all$`daily_OBSTETRICS AND GYNAECOLOGY` <-
  in_out_depts_poole_all$`in_OBSTETRICS AND GYNAECOLOGY` + in_out_depts_poole_all$`out_OBSTETRICS AND GYNAECOLOGY`
in_out_depts_poole_all$`OBSTETRICS AND GYNAECOLOGY` <- cumsum(in_out_depts_poole_all$`daily_OBSTETRICS AND GYNAECOLOGY`)

in_out_depts_poole_all$daily_ONCOLOGY <-
  in_out_depts_poole_all$in_ONCOLOGY + in_out_depts_poole_all$out_ONCOLOGY
in_out_depts_poole_all$ONCOLOGY <- cumsum(in_out_depts_poole_all$daily_ONCOLOGY)

in_out_depts_poole_all$`daily_TRAUMA AND ORTHOPAEDICS` <-
  in_out_depts_poole_all$`in_TRAUMA AND ORTHOPAEDICS` + in_out_depts_poole_all$`out_TRAUMA AND ORTHOPAEDICS`
in_out_depts_poole_all$`TRAUMA AND ORTHOPAEDICS` <-
  cumsum(in_out_depts_poole_all$`daily_TRAUMA AND ORTHOPAEDICS`)

in_out_depts_poole_all$`daily_ACUTE INTERNAL MEDICINE` <-
  in_out_depts_poole_all$`in_ACUTE INTERNAL MEDICINE` + in_out_depts_poole_all$`out_ACUTE INTERNAL MEDICINE`
in_out_depts_poole_all$`ACUTE INTERNAL MEDICINE` <-
  cumsum(in_out_depts_poole_all$`daily_ACUTE INTERNAL MEDICINE`)

in_out_depts_poole_all$`daily_EAR, NOSE AND THROAT` <-
  in_out_depts_poole_all$`in_EAR, NOSE AND THROAT` + in_out_depts_poole_all$`out_EAR, NOSE AND THROAT`
in_out_depts_poole_all$`EAR, NOSE AND THROAT` <-
  cumsum(in_out_depts_poole_all$`daily_EAR, NOSE AND THROAT`)

in_out_depts_poole_all$`daily_MATERNITY AND NEONATOLOGY` <-
  in_out_depts_poole_all$`in_MATERNITY AND NEONATOLOGY` + in_out_depts_poole_all$`out_MATERNITY AND NEONATOLOGY`
in_out_depts_poole_all$`MATERNITY AND NEONATOLOGY` <-
  cumsum(in_out_depts_poole_all$`daily_MATERNITY AND NEONATOLOGY`)

in_out_depts_poole_all$`daily_MAXILLO-FACIAL SURGERY` <-
  in_out_depts_poole_all$`in_MAXILLO-FACIAL SURGERY` + in_out_depts_poole_all$`out_MAXILLO-FACIAL SURGERY`
in_out_depts_poole_all$`MAXILLO-FACIAL SURGERY` <-
  cumsum(in_out_depts_poole_all$`daily_MAXILLO-FACIAL SURGERY`)

in_out_depts_poole_all$`daily_STROKE MEDICINE` <-
  in_out_depts_poole_all$`in_STROKE MEDICINE` + in_out_depts_poole_all$`out_STROKE MEDICINE`
in_out_depts_poole_all$`STROKE MEDICINE` <-
  cumsum(in_out_depts_poole_all$`daily_STROKE MEDICINE`)

in_out_depts_poole_all$daily_PAEDIATRICS <-
  in_out_depts_poole_all$in_PAEDIATRICS + in_out_depts_poole_all$out_PAEDIATRICS
in_out_depts_poole_all$PAEDIATRICS <-
  cumsum(in_out_depts_poole_all$daily_PAEDIATRICS)


#REMOVE THE LAST LINE IN THE DATASET (PLACEHOLDER FOR PTS STILL IN HOSPITAL)

in_out_depts_poole_all <- slice(in_out_depts_poole_all, 1:(n() - 1))

#CHANGE THE "TO" DATE TO THE LAST REAL OBSERVATION IN YOUR DATASET
#NOT THE DUMMY DATE FOR PTS IN HOSPITAL

in_out_depts_poole_all$new_date <-
  seq.POSIXt(
    from = as.POSIXct(in_out_depts_poole_all$Date_value[1]),
    length.out = nrow(in_out_depts_poole_all),
    by = "6 hours"
  )
library(directlabels)


#PLOT OF COVID V TOTAL EMERGENCY ADMISSIONS

ggplot(in_out_depts_poole_all) +
  geom_line(aes(y = total, x = new_date, colour = "All Occupancy")) +
  xlab("Date") +
  ylab("Occupied Beds") +
  scale_x_datetime(
    date_breaks = "4 weeks",
    date_labels = "%Y-%m-%d",
    limit = c(
      as.POSIXct("2020-01-01 00:00", tz = "UTC"),
      as.POSIXct("2020-05-18 12:00", tz = "UTC")
    )
  ) +
  geom_line(aes(x = new_date, y = COVID, colour = "U071 Code Patients")) +
  geom_hline(yintercept = 12,
             linetype = "dashed",
             color = "red3") +
  scale_color_manual(values = c('All Occupancy' = 'forestgreen',
                                'U071 Code Patients' = 'darkorchid3')) +
  labs(color = 'Occupancy Type')




in_out_depts_poole_all_oh <-
  one_hot(as.data.table(in_out_depts_poole_all),
          cols = "auto",
          dropCols = TRUE)

#write.csv(in_out_depts_poole_all_oh, "hospital_in_out.csv")

library(timetk)
library(sweep)

time_and_total_poole_all <-
  subset(in_out_depts_poole_all_oh, select = c(new_date, total))
spec_occupancy_poole_all <-
  subset(in_out_depts_poole_all_oh, select = c(Specialty_list_poole_all))
total_ts_poole_all <- cbind(time_and_total_poole_all, spec_occupancy_poole_all)

total_ts_poole_all <- slice(total_ts_poole_all, 240:n())

total_ts_poole_all_dept <-
  gather(total_ts_poole_all,
         Specialty,
         "Occupied Beds",
         total:ncol(total_ts_poole_all),
         factor_key = T)

total_ts_poole_all_dept_spec <-
  total_ts_poole_all_dept %>% filter(Specialty != "total")

ggplot(data = total_ts_poole_all_dept_spec,
       aes(x = `new_date`, y = `Occupied Beds`, fill = Specialty)) +
  geom_area(stat = 'identity') +
  scale_x_datetime(
    date_breaks = "3 months",
    date_labels = "%Y-%m-%d",
    limit = c(
      as.POSIXct("2019-01-01 00:00", tz = "UTC"),
      as.POSIXct("2020-07-06 12:00", tz = "UTC")
    ) 
  )+ xlab("Date")

# Setting Data For Use In Model -------------------------------------------


total_ts_poole_all_dept_tbats_list <- total_ts_poole_all_dept %>%
  select(-new_date) %>%
  group_by(Specialty) %>%
  nest()

total_ts_poole_all_dept_tbats_7 <- total_ts_poole_all_dept_tbats_list %>%
  mutate(
    data_train_poole_all_7_msts = map(
      .x = data,
      .f = msts,
      start = total_ts_poole_all_dept$new_date[1],
      seasonal.periods = c(28, 1461)
    )
  )

# Generating Model Fit (TBATS) and Tidying --------------------------------------------


total_ts_poole_all_dept_tbats_fit <-
  total_ts_poole_all_dept_tbats_7 %>%
  mutate(fit.tbats = map(data_train_poole_all_7_msts, tbats))

total_ts_poole_all_dept_tbats_forecast <-
  total_ts_poole_all_dept_tbats_fit %>%
  mutate(forecast.tbats = map(fit.tbats, forecast, h = 120))

total_ts_poole_all_dept_tbats_forecast_tidy <-
  total_ts_poole_all_dept_tbats_forecast %>%
  mutate(sweep = map(
    forecast.tbats,
    sw_sweep,
    fitted = FALSE,
    timetk_idx = FALSE
  )) %>%
  unnest(sweep)

total_ts_poole_all_dept_tbats_forecast_tidy$date_value <-
  rep(
    seq.POSIXt(
      from = total_ts_poole_all_dept$new_date[1],
      by = "6 hours",
      length.out = nrow(total_ts_poole_all_dept_tbats_forecast_tidy) / n_groups(total_ts_poole_all_dept_tbats_forecast_tidy)
    ),
    times = n_groups(total_ts_poole_all_dept_tbats_forecast_tidy)
  )

total_ts_poole_all_dept_tbats_forecast_tidy$`Occupied Beds`[total_ts_poole_all_dept_tbats_forecast_tidy$`Occupied Beds` <
                                                             0] <-
  0
total_ts_poole_all_dept_tbats_forecast_tidy$lo.80[total_ts_poole_all_dept_tbats_forecast_tidy$lo.80 <
                                                   0] <- 0
total_ts_poole_all_dept_tbats_forecast_tidy$lo.95 [total_ts_poole_all_dept_tbats_forecast_tidy$lo.95 <
                                                    0] <- 0

total_ts_poole_all_dept_tbats_forecast_export <-
  total_ts_poole_all_dept_tbats_forecast_tidy %>%
  select(date_value,
         Specialty,
         key,
         `Occupied Beds`,
         lo.80,
         lo.95,
         hi.80,
         hi.95)

write.csv(total_ts_poole_all_dept_tbats_forecast_export, "poole_all_6h_tbats.csv")

total_ts_poole_all_dept_tbats_forecast_tidy %>%
  ggplot(aes(
    x = date_value,
    y = `Occupied Beds`,
    color = key,
    group = Specialty
  )) +
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
    title = "poole_all Hospital Beds by Discharge Specialty",
    subtitle = "Auto-Arima Model",
    x = "Date",
    y = "Beds"
  ) +
  scale_x_datetime(breaks = "1 month",
                   limits = c(
                     as.POSIXct("2020-01-01", tz = "UTC"),
                     as.POSIXct("2020-08-01", tz = "UTC")
                   )) +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ Specialty,
             scales = "free_y",
             ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
