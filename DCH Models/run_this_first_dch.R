#Loading in base datasets and setting wd
setwd("~/HOSPITAL BEDS")

library(tidyr)
library(dplyr)
library(readxl)
library(data.table)
library(ggplot2)
library(foreach)
library(forecast)
library(tidyverse)
library(lubridate)
library(expm)
library(tidyr)
library(timetk)
library(tsbox)
library(sweep)
library(tidyquant)
library(plotly)

admissions_daily_dch <- read_excel("admissions_daily_dch.xlsx")
midnight_beds_dch <- read_excel("midnight_beds_dch.xlsx")
