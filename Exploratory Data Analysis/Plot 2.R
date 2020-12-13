#clear work space
rm(list=ls())

#install pacman to check for required packages
if (!require('pacman')) install.packages('pacman'); library(pacman) 

# load (install if required) packages from CRAN
p_load("dplyr", "lattice", "lubridate", "tidyverse",  "data.table")


#Check if a directory exists, and if not, Create one
#Directory is GCD (Getting and Cleaning Data)
if(!file.exists("Exploratory Data Analysis/data")){
  dir.create("Exploratory Data Analysis/data")
}


#unzip data folder
zipfile <- "./Exploratory Data Analysis/data.zip"
unzip(zipfile, exdir = "./Exploratory Data Analysis", unzip = "internal")
list.files("./Exploratory Data Analysis/data")

#read data into r
hpc <- read.table("./Exploratory Data Analysis/household_power_consumption.txt",
                  sep = ";", header = TRUE, na.strings = "?")

Sys.setlocale("LC_TIME", "English")

hpc_feb <- hpc %>% 
  mutate(Date = as.Date(dmy(Date))) %>% 
  mutate(interval = interval(dmy("01-02-2007"), dmy("02-02-2007"))) %>% 
  mutate(in_int = Date %within% interval) %>% 
  filter(in_int == TRUE) %>% 
  mutate(d_time = paste(Date, Time, sep=" "),
         d_time = as.POSIXct(d_time), 
         weekday = lubridate::wday(Date, label=TRUE, abbr=FALSE), 
         Global_active_power = as.numeric(Global_active_power),
         Global_reactive_power = as.numeric(Global_reactive_power),
         Voltage = as.numeric(Voltage),
         Global_intensity = as.numeric(Global_intensity),
         Sub_metering_1 = as.numeric(Sub_metering_1),
         Sub_metering_2 = as.numeric(Sub_metering_2),
         Sub_metering_3 = as.numeric(Sub_metering_3)) %>%
  select(Date, Time, d_time, everything())



#Plot 2: Global Active Power (y) by weekday (x)

plot(Global_active_power ~ d_time, hpc_feb,
     type = "l",
     xlab = "",
     ylab = "Global Active Power (kilowatts)")


