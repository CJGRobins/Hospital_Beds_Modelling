#Making dataset for machine learning
#NE ONLY DCH
setwd("~/HOSPITAL BEDS")

library(foreach)
library(nnfor)
library(tidyr)
library(dplyr)
library(readxl)
library(mltools)
library(data.table)
library(lubridate)
library(ggplot2)

#admissions_daily_dch <- read_excel("admissions_daily_dch.xlsx")
#View(admissions_daily_dch_ne)

#cleaning data and adding 10 diagnoses as categories ----------------------------------------------------------

##fixing data for patients currently in hospital
admissions_daily_dch_ne <- admissions_daily_dch %>%
  filter(`Admission Type` == "NE",
         LOS != 0)
admissions_daily_dch_ne$`Discharge Date`[is.na(admissions_daily_dch_ne$`Discharge Date`)] <-
  as.Date("2050-01-01")


admissions_daily_dch_ne$Specialty <-
  ifelse(
    admissions_daily_dch_ne$`Diagnosis Code` == "U071",
    admissions_daily_dch_ne$`Diagnosis Code`,
    admissions_daily_dch_ne$Specialty
  )
admissions_daily_dch_ne <- admissions_daily_dch_ne %>%
  mutate(Specialty = recode(Specialty, U071 = "COVID"))

top10_desc <-
  c(
    "Pneumonia, organism unspecified",
    "Pain in throat and chest",
    "Abdominal and pelvic pain",
    "Other disorders of urinary system",
    "Viral infection of unspecified site",
    "Maternal care for other known or suspected fetal problems",
    "Other chronic obstructive pulmonary disease",
    "Fracture of femur",
    "Unspecified acute lower respiratory infection",
    "Acute myocardial infarction"
  )

foreach(i = top10_desc) %do% {
  admissions_daily_dch_ne$Specialty <-
    ifelse(
      admissions_daily_dch_ne$`Short Diagnosis Description` == i,
      admissions_daily_dch_ne$`Short Diagnosis Description`,
      admissions_daily_dch_ne$Specialty
    )
}

admissions_daily_dch_ne$`Admission Date` <-
  as.Date(admissions_daily_dch_ne$`Admission Date`)
admissions_daily_dch_ne$`Discharge Date` <-
  as.Date(admissions_daily_dch_ne$`Discharge Date`)

admissions_daily_dch_ne$dummy_var <- 1


# Subsetting data ---------------------------------------------------------

admissions_daily_dch_ne_subset <-
  subset(
    admissions_daily_dch_ne,
    select = c(`Admission Date`, `Discharge Date`, Specialty, dummy_var)
  )
inflow_dch <- admissions_daily_dch_ne_subset %>%
  group_by(`Admission Date`)


# Making inflow and outflow dataframes and merging them -------------------


inflow_dch <- subset(inflow_dch, select = -c(`Discharge Date`))
inflow_dch_spec <- inflow_dch %>%
  group_by(`Admission Date`, Specialty) %>%
  summarise(Count = sum(dummy_var))

inflow_dch_wide <- spread(inflow_dch_spec, Specialty, Count)
inflow_dch_wide[is.na(inflow_dch_wide)] <- 0
inflow_dch_wide$total <- rowSums(inflow_dch_wide[, 2:ncol(inflow_dch_wide)])
inflow_dch_wide[is.na(inflow_dch_wide)] <- 0

inflow_dch_wide <-
  inflow_dch_wide %>% dplyr::rename_all(function(x)
    paste0("in_", x))
inflow_dch_wide_merge <- inflow_dch_wide %>%
  rename(Date_value = `in_Admission Date`)

outflow_dch <- admissions_daily_dch_ne_subset %>%
  group_by(`Admission Date`)
outflow_dch <- subset(outflow_dch, select = -c(`Admission Date`))

outflow_dch_spec <- outflow_dch %>%
  group_by(`Discharge Date`, Specialty) %>%
  summarise(Count = sum(dummy_var))

outflow_dch_wide <- spread(outflow_dch_spec, Specialty, Count)
outflow_dch_wide[is.na(outflow_dch_wide)] <- 0
outflow_dch_wide$total <- rowSums(outflow_dch_wide[, 2:ncol(outflow_dch_wide)])
outflow_dch_wide[is.na(outflow_dch_wide)] <- 0
outflow_dch_wide[, 2:ncol(outflow_dch_wide)] <-
  lapply(outflow_dch_wide[, 2:ncol(outflow_dch_wide)], function(x)
    - 1 * x)

outflow_dch_wide <-
  outflow_dch_wide %>% dplyr::rename_all(function(x)
    paste0("out_", x))
outflow_dch_wide_merge <- outflow_dch_wide %>%
  rename(Date_value = `out_Discharge Date`)

in_out_depts_dch <-
  merge(
    inflow_dch_wide_merge,
    outflow_dch_wide_merge,
    by = c("Date_value"),
    all = TRUE
  )
in_out_depts_dch[is.na(in_out_depts_dch)] <- 0
in_out_depts_dch$D_O_W <- weekdays(in_out_depts_dch$Date_value)
in_out_depts_dch$D_O_W <- as.factor(in_out_depts_dch$D_O_W)


# N week in the month function --------------------------------------------


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


# Creating factors for OHE ------------------------------------------------

in_out_depts_dch$W_O_M <- nth_week(in_out_depts_dch$Date_value)
in_out_depts_dch$W_O_M <- as.factor(in_out_depts_dch$W_O_M)
in_out_depts_dch$MONTH <-
  lubridate::month(in_out_depts_dch$Date_value,
                   label = TRUE,
                   abbr = TRUE)
in_out_depts_dch$MONTH <-
  factor(in_out_depts_dch$MONTH, ordered = FALSE)

in_out_depts_dch$total <-
  in_out_depts_dch$in_total + in_out_depts_dch$out_total
in_out_depts_dch$running_total <- cumsum(in_out_depts_dch$total)

in_out_depts_dch$daily_COVID <-
  in_out_depts_dch$in_COVID + in_out_depts_dch$out_COVID
in_out_depts_dch$COVID <- cumsum(in_out_depts_dch$daily_COVID)

# Creating bed occupancy for desired diagnoses ----------------------------


#top 10 diagnoses summing for time series exporting
#couldn't work out how to 'foreach' these, please don't be upset with how ugly this code is

in_out_depts_dch$`daily_Pneumonia, organism unspecified` <-
  in_out_depts_dch$`in_Pneumonia, organism unspecified` + in_out_depts_dch$`out_Pneumonia, organism unspecified`
in_out_depts_dch$`Pneumonia, organism unspecified` <-
  cumsum(in_out_depts_dch$`daily_Pneumonia, organism unspecified`)

in_out_depts_dch$`daily_Pain in throat and chest` <-
  in_out_depts_dch$`in_Pain in throat and chest` + in_out_depts_dch$`out_Pain in throat and chest`
in_out_depts_dch$`Pain in throat and chest` <-
  cumsum(in_out_depts_dch$`daily_Pain in throat and chest`)

in_out_depts_dch$`daily_Abdominal and pelvic pain` <-
  in_out_depts_dch$`in_Abdominal and pelvic pain` + in_out_depts_dch$`out_Abdominal and pelvic pain`
in_out_depts_dch$`Abdominal and pelvic pain` <-
  cumsum(in_out_depts_dch$`daily_Abdominal and pelvic pain`)

in_out_depts_dch$`daily_Other disorders of urinary system` <-
  in_out_depts_dch$`in_Other disorders of urinary system` + in_out_depts_dch$`out_Other disorders of urinary system`
in_out_depts_dch$`Other disorders of urinary system` <-
  cumsum(in_out_depts_dch$`daily_Other disorders of urinary system`)

in_out_depts_dch$`daily_Viral infection of unspecified site` <-
  in_out_depts_dch$`in_Viral infection of unspecified site` + in_out_depts_dch$`out_Viral infection of unspecified site`
in_out_depts_dch$`Viral infection of unspecified site` <-
  cumsum(in_out_depts_dch$`daily_Viral infection of unspecified site`)

in_out_depts_dch$`daily_Maternal care for other known or suspected fetal problems` <-
  in_out_depts_dch$`in_Maternal care for other known or suspected fetal problems` + in_out_depts_dch$`out_Maternal care for other known or suspected fetal problems`
in_out_depts_dch$`Maternal care for other known or suspected fetal problems` <-
  cumsum(in_out_depts_dch$`daily_Maternal care for other known or suspected fetal problems`)

in_out_depts_dch$`daily_Other chronic obstructive pulmonary disease` <-
  in_out_depts_dch$`in_Other chronic obstructive pulmonary disease` + in_out_depts_dch$`out_Other chronic obstructive pulmonary disease`
in_out_depts_dch$`Other chronic obstructive pulmonary disease` <-
  cumsum(in_out_depts_dch$`daily_Other chronic obstructive pulmonary disease`)

in_out_depts_dch$`daily_Fracture of femur` <-
  in_out_depts_dch$`in_Fracture of femur` + in_out_depts_dch$`out_Fracture of femur`
in_out_depts_dch$`Fracture of femur` <-
  cumsum(in_out_depts_dch$`daily_Fracture of femur`)

in_out_depts_dch$`daily_Unspecified acute lower respiratory infection` <-
  in_out_depts_dch$`in_Unspecified acute lower respiratory infection` + in_out_depts_dch$`out_Unspecified acute lower respiratory infection`
in_out_depts_dch$`Unspecified acute lower respiratory infection` <-
  cumsum(in_out_depts_dch$`daily_Unspecified acute lower respiratory infection`)

in_out_depts_dch$`daily_Acute myocardial infarction` <-
  in_out_depts_dch$`in_Acute myocardial infarction` + in_out_depts_dch$`out_Acute myocardial infarction`
in_out_depts_dch$`Acute myocardial infarction` <-
  cumsum(in_out_depts_dch$`daily_Acute myocardial infarction`)

#REMOVE THE LAST LINE IN THE DATASET (PLACEHOLDER FOR PTS STILL IN HOSPITAL)

in_out_depts_dch <- slice(in_out_depts_dch, 1:(n() - 1))

#CHANGE THE "TO" DATE TO THE LAST REAL OBSERVATION IN YOUR DATASET
#NOT THE DUMMY DATE FOR PTS IN HOSPITAL


#PLOT OF COVID V TOTAL EMERGENCY ADMISSIONS

# Plotting and time series setting ----------------------------------------


ggplot(in_out_depts_dch) +
  #geom_line(aes(y = running_total, x = Date_value, colour = "All Emergency Admissions")) +
  xlab("Date") +
  ylab("Occupied Beds") +
  geom_line(aes(x = Date_value, y = COVID, colour = "U071 Code Patients")) +
  geom_line(aes(x = Date_value, y = `Pneumonia, organism unspecified`, colour = "Pneumonia")) +
  #geom_smooth(aes(x = Date_value, y = `Pneumonia, organism unspecified`, method = "loess")) +
  geom_hline(yintercept = 4,
             linetype = "dashed",
             color = "red3") +
  scale_color_manual(
    values = c(
      'All Emergency Admissions' = 'forestgreen',
      'U071 Code Patients' = 'darkorchid3',
      'Pneumonia' = 'blue'
    )
  ) +
  labs(color = 'Occupancy Type') +
  scale_x_date(date_breaks = "4 months", date_labels = "%Y-%m-%d", limits = c(as.Date("2017-06-01"), as.Date("2020-05-27")))

#PLOT OF COID V A/E BED OCCUPANCY

in_out_depts_dch_oh <-
  one_hot(as.data.table(in_out_depts_dch),
          cols = "auto",
          dropCols = TRUE)


#write.csv(in_out_depts_dch_oh, "dch_hospital_in_out.csv")

total_dch_ne_ts_wide <- subset(in_out_depts_dch_oh, select = c(Date_value, 
                                      `Pneumonia, organism unspecified`,                          
                                      `Pain in throat and chest`,                         
                                      `Abdominal and pelvic pain`,                        
                                      `Other disorders of urinary system`,                       
                                      `Viral infection of unspecified site`,                      
                                      `Maternal care for other known or suspected fetal problems`,
                                      `Other chronic obstructive pulmonary disease`,
                                      `Fracture of femur`,
                                      `Unspecified acute lower respiratory infection`,
                                      `Acute myocardial infarction`, 
                                       running_total))
total_dch_ne_ts_wide <- slice(total_dch_ne_ts_wide, 60:n())

total_dch_ne_ts <-
  gather(
    total_dch_ne_ts_wide,
    Diagnosis_Group,
    "Occupied Beds",
    "Pneumonia, organism unspecified":running_total,
    factor_key = T
  )
# Setting Data For Use In Model -------------------------------------------


total_dch_ne_ts_tbats_list <- total_dch_ne_ts %>%
  select(-Date_value) %>%
  group_by(Diagnosis_Group) %>%
  nest()

total_dch_ne_ts_tbats_7 <- total_dch_ne_ts_tbats_list %>%
  mutate(data_train_7.ts = map(
    .x = data,
    .f = msts,
    start = total_dch_ne_ts$Date_value[1],
    seasonal.periods = c(7, 365.25)
  ))

# Generating Model Fit (TBATS) and Tidying --------------------------------------------


total_dch_ne_ts_tbats_fit <-
  total_dch_ne_ts_tbats_7 %>%
  mutate(fit.tbats = map(data_train_7.ts, tbats))

total_dch_ne_ts_tbats_forecast <-
  total_dch_ne_ts_tbats_fit %>%
  mutate(forecast.tbats = map(fit.tbats, forecast, h = 60))

total_dch_ne_ts_tbats_forecast_tidy <-
  total_dch_ne_ts_tbats_forecast %>%
  mutate(sweep = map(
    forecast.tbats,
    sw_sweep,
    fitted = FALSE,
    timetk_idx = FALSE
  )) %>%
  unnest(sweep)

total_dch_ne_ts_tbats_forecast_tidy$date_value <-
  rep(seq.Date(
    from = total_dch_ne_ts$Date_value[1],
    by = "1 day",
    length.out = nrow(total_dch_ne_ts_tbats_forecast_tidy)/n_groups(total_dch_ne_ts_tbats_forecast_tidy)
  ), times = n_groups(total_dch_ne_ts_tbats_forecast_tidy))

total_dch_ne_ts_tbats_forecast_tidy$`Occupied Beds`[total_dch_ne_ts_tbats_forecast_tidy$`Occupied Beds` <
                                                        0] <-
  0
total_dch_ne_ts_tbats_forecast_tidy$lo.80[total_dch_ne_ts_tbats_forecast_tidy$lo.80 <
                                              0] <- 0
total_dch_ne_ts_tbats_forecast_tidy$lo.95 [total_dch_ne_ts_tbats_forecast_tidy$lo.95 <
                                               0] <- 0

# Plotting Model Outputs (showing 01/20 onwards) --------------------------


total_dch_ne_ts_tbats_forecast_tidy %>%
  ggplot(
    aes(
      x = date_value,
      y = `Occupied Beds`,
      color = key,
      group = Diagnosis_Group
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
  scale_x_date(breaks = "1 month", limits = c(as.Date("2020-01-01"), as.Date("2020-07-01"))) +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap( ~ Diagnosis_Group,
              scales = "free_y",
              ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))