library(dplyr)
library(ggplot2)
library(lubridate)
library(pracma)
library(scales) # to access breaks/formatting functions
library(stringr) # To use the str_pad function

ABIA <- read.csv("files/ABIA.csv")
glimpse(ABIA)
ABIA[, 'Departure_flight'] = ifelse(ABIA$Origin == 'AUS',1,0)


qplot(ABIA$TaxiOut[ABIA$Departure_flight == 1],geom = 'histogram',
      ylim = c(0,30000),xlim = c(0,200))
qplot(ABIA$TaxiOut[ABIA$Departure_flight == 0],geom = 'histogram',
      ylim = c(0,30000),xlim = c(0,200))

qplot(ABIA$TaxiIn[ABIA$Departure_flight == 1],geom = 'histogram',
      ylim = c(0,30000),xlim = c(0,100))
qplot(ABIA$TaxiIn[ABIA$Departure_flight == 0],geom = 'histogram',
      ylim = c(0,30000),xlim = c(0,100))


# Question 1 and 3:
# What is the best time of day to fly to minimize delays?
# What is the best time of year to fly to minimize delays?
qplot(ABIA$CRSDepTime[ABIA$DepDelay > 0], geom = 'histogram', binwidth = 10)


# Failed Time buckets
# ABIA$CRSDepTime_Bucket <- cut(ABIA$CRSDepTime, breaks = c(0, 030, 100, 130, 200,
#                                                      230, 300, 330, 400, 430, 
#                                                      500, 530, 600, 630, 700,
#                                                      730, 800, 830, 900, 930,
#                                                      1000, 1030, 1100, 1130,
#                                                      1200, 1230, 1300, 1330,
#                                                      1400, 1430, 1500, 1530,
#                                                      1600, 1630, 1700, 1730,
#                                                      1800, 1830, 1900, 1930,
#                                                      2000, 2030, 2100, 2130,
#                                                      2200, 2230, 2300, 2330,
#                                                      2400), 
#                          include.lowest = TRUE, right = T )


options(scipen = 999)

# Unable to plot using lubridate
# ABIA$CRSDepTime_TF <- hm(paste0(substr(ABIA$CRSDepTime,1,2),":",
#                                 substr(ABIA$CRSDepTime,3,4)),
#                          tz = "Central Time")[-dim(ABIA)[1] - 1]

# Creating R-type datetime variable; specifically POSIXct (UNIX time format)
ABIA$CRSDepTime_TF <- as.POSIXct(
  strptime(paste0(substr(str_pad(ABIA$CRSDepTime, 4, pad = "0"),1,2),":",
                  substr(str_pad(ABIA$CRSDepTime, 4, pad = "0"),3,4)), "%H:%M",
           tz = "UTC"))

# Plotting number of flights over scheduled departure time
ggplot(aes(x = CRSDepTime_TF), data = select(ABIA, FlightNum, CRSDepTime_TF)) +
  geom_histogram(binwidth = 1800) + 
  scale_x_datetime(breaks = date_breaks("30 min"),
                   labels = date_format("%H:%M")) +
  xlab("CRS Departure Time") + 
  ylab("Count of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


# as.POSIXct(CRSDepTime_TF, origin = "1970-01-01"), 

# Subsetting for only-delayed aircraft
Delays_across_TOD <- ABIA  %>% 
  select(CRSDepTime_TF, DepTime, DepDelay, FlightNum) %>%
  filter(DepDelay > 0)
ggplot(aes(x = CRSDepTime_TF), data = select(ABIA, FlightNum, CRSDepTime_TF)) +
  geom_histogram(binwidth = 900) + 
  scale_x_datetime(breaks = date_breaks("30 min"),
                   labels = date_format("%H:%M")) +
  xlab("CRS Departure Time") + 
  ylab("Count of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

# Plotting number of delayed flights over scheduled departure time
ggplot(aes(x = CRSDepTime_TF), data = select(Delays_across_TOD, FlightNum, CRSDepTime_TF)) +
  geom_histogram(binwidth = 1800) + 
  scale_x_datetime(breaks = date_breaks("30 min"),
                   labels = date_format("%H:%M")) +
  xlab("CRS Departure Time") + 
  ylab("Count of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

# Creating bins: 15-min bins till 3 hourus and 5-hour bins after that
Delays_across_TOD$DelayBin <- cut(Delays_across_TOD$DepDelay, 
                                  c(seq(0,180,15), 300, 600, 1000), include.lowest = T)

Delays_TOD_cnts <- Delays_across_TOD %>% 
  group_by(DelayBin) %>%
  summarise(count_delay = count(DelayBin))

ggplot(aes(x = CRSDepTime_TF, colour = DelayBin), data = Delays_across_TOD) +
  geom_freqpoly(binwidth = 900) +
  scale_x_datetime(breaks = date_breaks("30 min"),
                   labels = date_format("%H:%M")) +
  xlab("CRS Departure Time") + 
  ylab("Count of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


# -----------------------------------------------------------------------------


# Plotting number of flights over scheduled departure time


ABIA$Month_C <- as.factor(ABIA$Month)
ggplot(aes(x = Month), data = select(ABIA, FlightNum, CRSDepTime_TF, Month)) +
  geom_bar() + 
  scale_x_continuous(c(1,12),
                     labels = seq(1:12))

xlab("CRS Departure Time") + 
  ylab("Count of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


# as.POSIXct(CRSDepTime_TF, origin = "1970-01-01"), 

# Subsetting for only-delayed aircraft
Delays_across_TOD <- ABIA  %>% 
  select(CRSDepTime_TF, DepTime, DepDelay, FlightNum) %>%
  filter(DepDelay > 0)
ggplot(aes(x = CRSDepTime_TF), data = select(ABIA, FlightNum, CRSDepTime_TF)) +
  geom_histogram(binwidth = 900) + 
  scale_x_datetime(breaks = date_breaks("30 min"),
                   labels = date_format("%H:%M")) +
  xlab("CRS Departure Time") + 
  ylab("Count of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

# Plotting number of delayed flights over scheduled departure time
ggplot(aes(x = CRSDepTime_TF), data = select(Delays_across_TOD, FlightNum, CRSDepTime_TF)) +
  geom_histogram(binwidth = 1800) + 
  scale_x_datetime(breaks = date_breaks("30 min"),
                   labels = date_format("%H:%M")) +
  xlab("CRS Departure Time") + 
  ylab("Count of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

# Creating bins: 15-min bins till 3 hourus and 5-hour bins after that
Delays_across_TOD$DelayBin <- cut(Delays_across_TOD$DepDelay, 
                                  c(seq(0,180,15), 300, 600, 1000), include.lowest = T)

Delays_TOD_cnts <- Delays_across_TOD %>% 
  group_by(DelayBin) %>%
  summarise(count_delay = count(DelayBin))

ggplot(aes(x = CRSDepTime_TF, colour = DelayBin), data = Delays_across_TOD) +
  geom_freqpoly(binwidth = 900) +
  scale_x_datetime(breaks = date_breaks("30 min"),
                   labels = date_format("%H:%M")) +
  xlab("CRS Departure Time") + 
  ylab("Count of Flights") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

