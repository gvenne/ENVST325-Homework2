#activity 2
#install packages
install.packages("dplyr")
install.packages("lubridate")

#load library
library(dplyr)
library(lubridate)

#read in data
streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

#site names
siteInfo$names

exampleDate <- c("2021-01-10 05:23:30")
#parse date with year, month, day hour:minute:second
ymd_hms(exampleDate)

#parse date with timezone so that it is always in NY time
#will account for daylight savings. Time local to NY
ymd_hms(exampleDate, tz="America/New_York")

#eastern standard time (note this doesn't work during daylight savings)
ymd_hms(exampleDate, tz="EST")

streamH$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

#date with Jan 31, 2021
exampleDate <- c("2021-01-31")
#parse date with year, month, day hour:minute:second
ymd(exampleDate)

#years
year(streamH$dateF )

leap_year(streamH$dateF )

yday(streamH$dateF )

decimal_date(streamH$dateF )

#subset stream height to just include peace river
peaceH <- streamH[streamH$siteID == 2295637, ]

#subset stream height to just include peace river
peaceH <- streamH[streamH$siteID != 2295637, ]

#plot date vs height with both lines and filled in points
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date",
     ylab = "Stage height (ft)")

#plot date vs height with squares
plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=0, xlab="Date",
     ylab = "Stage height (ft)")


#preview streamH table 
str(streamH)

#print siteInfo
siteInfo

height.ave <- floods %>% # data frame with pipe
  group_by(names) %>% # group data frame by unique names
  summarise(mean.height = mean(gheight.ft)) # next summarize using mean

height.ave

height.day <- floods %>% # data frame with pipe
  group_by(names, doy) %>% # group data frame by unique names and doy
  summarise(mean.height = mean(gheight.ft), max.height = max(gheight.ft))

`rlang::last_error()`

max.cat <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= major.ft) #observations with height more than or equal to the major flood height

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = n()) # count number of observations for each name

max.cat <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(n.major = ((n()*15)/60)/24)

#Prompt 1
# join site info and stream heights into a new data frame floods
floods <- full_join(streamH, # left table
                    siteInfo, # right table
                    by="siteID") # common identifier
head(floods)

mean(floods$gheight.ft)

#Prompt 2
#Parse the date for the Floods data frame.

floods$dateF <- ymd_hm(streamH$datetime, tz="America/New_York")

#Prompt 3
#What was the earliest date that each river reached the flood stage?
Earlier <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(min.date = min(dateF), flood.level = mean(flood.ft), max.ht = max(gheight.ft))

#Homework 2 part A
#how to use the select function
data <- select(floods, "dateF", "gheight.ft")
#Example 1
VariableA <- select(floods, "names", "gheight.ft", "datetime")
#Example 2
VariableB <- select(floods, "gheight.ft":"dateF")
#Example 3
VariableC <- select(floods, -datetime)
#Example 4
VariableD <- select(floods, gheight.ft:dateF, -moderate.ft)
#Example 5 
VariableE <- select(floods, contains("e"), -agency)
#Example 6
VariableF <- select(floods,(!(gheight.ft:dateF)))
#remove everything in na omit, but not get rid of specific rows

#histogram function
Fishheating_creek = floods[1:2208,]
hist(Fishheating_creek$gheight.ft, main = "Fishheating Creek Water Height", 
     xlab = "water height (ft)", 
     col = "cyan", 
     border = "magenta")

Santa.Fe.River <- floods[4266:6473,]
hist(Santa.Fe.River$gheight.ft, main = "Santa Fe River", 
     xlab = "Height (ft)", 
     col = "tomato2", 
     border = "blue",
     ylim = c(0,200))

Fishheating <- floods[floods$gheight.ft >= floods$flood.ft,]

#mutate function
#can be used to add and delete columns at the same time
Floods_Mutated <- mutate(floods,
                         stage_meters = gheight.ft * 0.3048, 
                         percent_flood = (gheight.ft / major.ft) * 100)

floods_dont_keep <- mutate(floods, .keep = c("unused"), 
                           stage_meters = gheight.ft * 0.3048)

#ifelse function
floods$RiverLocationNA <- ifelse(floods$names == "SANTA FE RIVER NEAR FORT WHITE", 
                                 NA, "Not missing")

#Homework 2 part B(2)
#Question 1
Fishheating_creek_data <- floods[1:2208,]
Peaceriver_data <- floods[2209:4265,]
Santaferiver_data <- floods[4266:6473,]
Withlacoochee_data <- floods[6474:8681,]

#plot fishheating creek
plot(Fishheating_creek_data$dateF, Fishheating_creek_data$gheight.ft, 
     ylab = "Height (ft)",
     main = "Fishheating Creek", 
     xlab = "Date", 
     col = "tomato2",
     bg = "tomato2")

#plot Peace river 
plot(Peaceriver_data$dateF, Peaceriver_data$gheight.ft,
     ylab = "Height (ft)",
     main = "Peacer River", 
     xlab = "Date", 
     col = "blue",
     bg = "blue")

#plot Santa Fe river
plot(Santaferiver_data$dateF, Santaferiver_data$gheight.ft,
     ylab = "Height (ft)",
     main = "Santa Fe River", 
     xlab = "Date", 
     col = "cyan3",
     bg = "cyan3")

#plot Withlacoochee
plot(Withlacoochee_data$dateF, Withlacoochee_data$gheight.ft, 
     ylab = "Height (ft)",
     main = "Withlacoochee River", 
     xlab = "Date", 
     col = "hotpink1",
     bg = "hotpink1")

#Question 2
#What was the earliest date that each river reached the flood stage?
Earlier <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= flood.ft) %>%
  summarise(min.date = min(dateF), flood.level = mean(flood.ft), max.ht = max(gheight.ft))

moderate <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= moderate.ft) %>%
  summarise(min.date = min(dateF), flood.level = mean(flood.ft), max.ht = max(gheight.ft))

Major <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(min.date = min(dateF), flood.level = mean(flood.ft), max.ht = max(gheight.ft))

#Question 3
Majorflood <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>%
  summarise(min.date = min(dateF), flood.level = mean(major.ft), max.ht = max(gheight.ft))
