#without taking into account admission type
#Making dataset for machine learning
setwd("~/HOSPITAL BEDS")


library(nnfor)
library(tidyr)
library(dplyr)
library(readxl)
library(mltools)
library(data.table)
library(lubridate)
library(ggplot2)
library(sweep)

#admissions_daily <- read_excel("admissions_daily.xlsx")

admissions_daily <- admissions_daily %>%
  filter(!is.na(`Admission Date`),
         !is.na(`Admission Hour`),
         !is.na(Discharge_Main_Specialty_Desc))

# allowing the totalling to accurately reflect current occupancy

##fixing data for patients currently in hospital
admissions_daily_ne <- admissions_daily %>%
  filter(`Admission Type` == "NE")
admissions_daily_ne$`Discharge Hour`[is.na(admissions_daily_ne$`Discharge Hour`)] <- 14
admissions_daily_ne$`Discharge Date`[is.na(admissions_daily_ne$`Discharge Date`)] <- as.Date("2050-01-01")



#admissions_daily_ne <- admissions_daily_ne[complete.cases(admissions_daily_ne), ]
admissions_daily_ne$Discharge_Main_Specialty_Desc <- ifelse(admissions_daily_ne$diagnosis_1 == "U071", admissions_daily_ne$diagnosis_1, admissions_daily_ne$Discharge_Main_Specialty_Desc)
admissions_daily_ne <- admissions_daily_ne %>% 
  mutate(Discharge_Main_Specialty_Desc = recode(Discharge_Main_Specialty_Desc, U071 = "COVID"))
admissions_daily_ne$`Admission Date`<- as.Date(admissions_daily_ne$`Admission Date`)
admissions_daily_ne$`Discharge Date` <- as.Date(admissions_daily_ne$`Discharge Date`)

admissions_daily_ne$`Admission Quarter` <- ifelse(admissions_daily_ne$`Admission Hour`<=5,1 ,
                                               ifelse(admissions_daily_ne$`Admission Hour`>5 & admissions_daily_ne$`Admission Hour` <=11, 2, 
                                                      ifelse(admissions_daily_ne$`Admission Hour`>11 & admissions_daily_ne$`Admission Hour`<=17,3, 4)))
#1 = 12-6am, 2 = 6-12 am, 3 = 12-6pm, 4 = 6-12midnight
as.numeric(admissions_daily_ne$`Admission Quarter`)
admissions_daily_ne$`Discharge Quarter` <- ifelse(admissions_daily_ne$`Discharge Hour`<=5, 1 ,
                                               ifelse(admissions_daily_ne$`Discharge Hour`>5 & admissions_daily_ne$`Discharge Hour` <=11, 2, 
                                                      ifelse(admissions_daily_ne$`Discharge Hour`>11 & admissions_daily_ne$`Discharge Hour`<=17,3, 4)))
as.numeric(admissions_daily_ne$`Discharge Quarter`)
admissions_daily_ne$dummy_var <- 1
admissions_daily_ne$`Admission Date` <- as.Date(admissions_daily_ne$`Admission Date`)
admissions_daily_ne$`Discharge Date` <- as.Date(admissions_daily_ne$`Discharge Date`)


admissions_daily_ne_subset <- subset(admissions_daily_ne, select = c(`Admission Date`, `Admission Quarter`, `Discharge Date`, `Discharge Quarter`, Discharge_Main_Specialty_Desc, dummy_var))
inflow_ne <- admissions_daily_ne_subset %>%
  group_by(`Admission Date`, `Admission Quarter`)
inflow_ne <- subset(inflow_ne, select = -c(`Discharge Date`, `Discharge Quarter`))  

inflow_ne_spec <- inflow_ne %>%
  group_by(`Admission Date`, `Admission Quarter`, Discharge_Main_Specialty_Desc) %>%
  summarise(Count = sum(dummy_var)) 

inflow_ne_wide <- spread(inflow_ne_spec, Discharge_Main_Specialty_Desc, Count)
inflow_ne_wide[is.na(inflow_ne_wide)] <- 0
inflow_ne_wide$total <- rowSums(inflow_ne_wide[,3:ncol(inflow_ne_wide)])
inflow_ne_wide[is.na(inflow_ne_wide)] <- 0



inflow_ne_wide <- inflow_ne_wide %>% dplyr::rename_all(function(x) paste0("in_", x))
inflow_ne_wide_merge <- inflow_ne_wide %>% 
  rename(Date_value = `in_Admission Date`) %>%
  rename(Day_quarter = `in_Admission Quarter`)


######################################################################################
outflow_ne <- admissions_daily_ne_subset %>%
  group_by(`Admission Date`)
outflow_ne <- subset(outflow_ne, select = -c(`Admission Date`, `Admission Quarter`))  

outflow_ne_spec <- outflow_ne %>%
  group_by(`Discharge Date`, `Discharge Quarter`, Discharge_Main_Specialty_Desc) %>%
  summarise(Count = sum(dummy_var)) 

outflow_ne_wide <- spread(outflow_ne_spec, Discharge_Main_Specialty_Desc, Count)
outflow_ne_wide[is.na(outflow_ne_wide)] <- 0
outflow_ne_wide$total <- rowSums(outflow_ne_wide[,3:ncol(outflow_ne_wide)])
outflow_ne_wide[is.na(outflow_ne_wide)] <- 0
outflow_ne_wide[,3:ncol(outflow_ne_wide)] <- lapply(outflow_ne_wide[,3:ncol(outflow_ne_wide)], function(x) -1 * x)

outflow_ne_wide <- outflow_ne_wide %>% dplyr::rename_all(function(x) paste0("out_", x))
outflow_ne_wide_merge <- outflow_ne_wide %>% 
  rename(Date_value = `out_Discharge Date`)%>%
  rename(Day_quarter = `out_Discharge Quarter`)
###############################

in_out_depts_ne <- merge(inflow_ne_wide_merge, outflow_ne_wide_merge, by=c("Date_value", "Day_quarter"), all = TRUE)
in_out_depts_ne[is.na(in_out_depts_ne)] <-0
in_out_depts_ne$D_O_W <- weekdays(in_out_depts_ne$Date_value)
in_out_depts_ne$D_O_W <- as.factor(in_out_depts_ne$D_O_W)
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

in_out_depts_ne$W_O_M <- nth_week(in_out_depts_ne$Date_value)
in_out_depts_ne$W_O_M <- as.factor(in_out_depts_ne$W_O_M)
in_out_depts_ne$MONTH <- lubridate::month(in_out_depts_ne$Date_value, label = TRUE, abbr = TRUE)
in_out_depts_ne$MONTH <- factor(in_out_depts_ne$MONTH, ordered = FALSE)
in_out_depts_ne$Day_quarter <- recode(in_out_depts_ne$Day_quarter , `1` = "12PM-6AM", `2` = "6AM-12AM", `3` = "12AM-6PM", `4` = "6PM-12PM")
in_out_depts_ne$Day_quarter <- factor(in_out_depts_ne$Day_quarter)

in_out_depts_ne$total <- in_out_depts_ne$in_total + in_out_depts_ne$out_total
in_out_depts_ne$running_total <- cumsum(in_out_depts_ne$total)

in_out_depts_ne$COVID <- in_out_depts_ne$in_COVID + in_out_depts_ne$out_COVID
in_out_depts_ne$sum_COVID <- cumsum(in_out_depts_ne$COVID)

in_out_depts_ne$CARDIO <- in_out_depts_ne$in_CARDIOLOGY + in_out_depts_ne$out_CARDIOLOGY
in_out_depts_ne$sum_CARDIO <- cumsum(in_out_depts_ne$CARDIO)

in_out_depts_ne$AE <- in_out_depts_ne$`in_ACCIDENT AND EMERGENCY` + in_out_depts_ne$`out_ACCIDENT AND EMERGENCY`
in_out_depts_ne$sum_AE <- cumsum(in_out_depts_ne$AE)



#REMOVE THE LAST LINE IN THE DATASET (PLACEHOLDER FOR PTS STILL IN HOSPITAL)

in_out_depts_ne <- slice(in_out_depts_ne, 1:(n()-1))

#CHANGE THE "TO" DATE TO THE LAST REAL OBSERVATION IN YOUR DATASET 
#NOT THE DUMMY DATE FOR PTS IN HOSPITAL

in_out_depts_ne$new_date <- seq.POSIXt(from = as.POSIXct(in_out_depts_ne$Date_value[1]),
                                       length.out = nrow(in_out_depts_ne),
                                       by = "6 hours")
library(directlabels)
 
#PLOT OF COVID V TOTAL EMERGENCY ADMISSIONS

ggplot(in_out_depts_ne) +
  geom_line(aes(y=running_total, x=new_date, colour= "All Emergency Admissions")) +
  xlab("Date") +
  ylab("Occupied Beds") +
  scale_x_datetime(date_breaks = "4 weeks", date_labels = "%Y-%m-%d", limit=c(as.POSIXct("2020-01-01 00:00", tz="UTC"),as.POSIXct("2020-05-18 12:00", tz="UTC"))) +
  geom_line(aes(x = new_date, y = sum_COVID, colour= "U071 Code Patients")) +
  geom_hline(yintercept = 12, linetype="dashed", color= "red3") +
  scale_color_manual(values = c(
    'All Emergency Admissions' = 'forestgreen',
    'U071 Code Patients' = 'darkorchid3')) +
  labs(color = 'Occupancy Type')

#PLOT OF COID V A/E BED OCCUPANCY

ggplot(in_out_depts_ne) +
  geom_line(aes(y=sum_AE, x=new_date, colour= "A+E Occupancy")) +
  xlab("Date") +
  ylab("Occupied Beds") +
  scale_x_datetime(date_breaks = "4 weeks", date_labels = "%Y-%m-%d", limit=c(as.POSIXct("2020-01-01 00:00", tz="UTC"),as.POSIXct("2020-05-18 12:00", tz="UTC"))) +
  geom_line(aes(x = new_date, y = sum_COVID, colour= "U071 Code Patients")) +
  geom_hline(yintercept = 12, linetype="dashed", color= "red3") +
  scale_color_manual(values = c(
    'A+E Occupancy' = 'forestgreen',
    'U071 Code Patients' = 'darkorchid3')) +
  labs(color = 'Occupancy Type')

ggplot(in_out_depts_ne) +
  geom_line(aes(y=sum_CARDIO, x=new_date, colour= "Cardiology Occupancy")) +
  xlab("Date") +
  ylab("Occupied Beds") +
  scale_x_datetime(date_breaks = "4 weeks", date_labels = "%Y-%m-%d", limit=c(as.POSIXct("2020-01-01 00:00", tz="UTC"),as.POSIXct("2020-05-18 12:00", tz="UTC"))) +
  geom_line(aes(x = new_date, y = sum_COVID, colour= "U071 Code Patients")) +
  scale_color_manual(values = c(
    'Cardiology Occupancy' = 'forestgreen',
    'U071 Code Patients' = 'darkorchid3')) +
  labs(color = 'Occupancy Type') 

in_out_depts_ne_oh <- one_hot(as.data.table(in_out_depts_ne), cols = "auto", dropCols = TRUE)



#write.csv(in_out_depts_ne_oh, "hospital_in_out.csv")

total_ne_ts <- subset(in_out_depts_ne_oh, select = c(new_date, running_total))
total_ne_ts <- slice(total_ne_ts, 240:n())
total_ne_ts_2 <-subset(total_ne_ts, select = c(running_total))

#simple total emergency admissions

total_ne_msts <- msts(total_ne_ts_2, seasonal.periods = c(28, 1461))
total_ne_fit <- tbats(total_ne_msts)
total_ne_forecast <- forecast(total_ne_fit, h=240)
total_ne_forecast_tidy <- sw_sweep(total_ne_forecast)
total_ne_forecast_tidy$date_value <-
  seq.POSIXt(
    from = total_ne_ts$new_date[1],
    by = "6 hours",
    length.out = nrow(total_ne_forecast_tidy)
  )

total_ne_forecast_tidy %>%
ggplot(
  aes(
    x = date_value ,
    y = running_total,
    color = key
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
    title = "RCBH Emergency Classified Admissions Forecast, 6 Hourly Definition",
    subtitle = "Trigonometric, Box-Cox transform, ARMA errors, Trend, and Seasonal components Model",
    x = "Date",
    y = "Beds"
  ) +
  scale_x_datetime(breaks = "1 month", limits = c(as.POSIXct("2020-01-01 00:00", tz="UTC"), as.POSIXct("2020-07-01 00:00", tz = "UTC"))) 
  
