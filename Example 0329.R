library(lubridate)

#1. 
date = as.POSIXct("02-20-2018", 
                  format = "%m-%d-%Y")

#2. 
date = mdy("02-20-2018")
date = dmy("20-02-2018")

ymd("2018-02-20")
ymd("2018-20-02")
ymd("2018.02/20")
ymd("20180220")
ymd("180220")


mdy_hms("02-20-2018 1:12:33", tz = "EST")
hm("1:34")

#3. 
#library(readr)
snow = read.csv("snowdates.csv")

library(ggplot2)

snow$date = ymd(snow$date)
ggplot(snow, aes(x = date, y = deaths)) + 
  geom_point()


library(readr)
snow = read_csv("snowdates.csv")

library(ggplot2)

#snow$date = ymd(snow$date)
ggplot(snow, aes(x = date, y = deaths)) + 
  geom_point()


### 4
library(lubridate)
date = now()

minute(date)
day(date)
year(date)
week(date) # week of the year
wday(date) # day of the week
wday(date, label = T, abbr = F)


# Date Arithmetic

date - years(1)
date + minutes(3)
date + days(5)

date = c("2018/2/20", "2017-01-25")
date = ymd(date)
date
mean(day(date))

#5. 

start_2011 = ymd_hms("2011-01-01 00:00:00")
start_2011
start_2011 + minutes(1)
start_2011 + hours(1)
start_2011 + days(365)
start_2011 + years(1)

#6. 
start_2012 = ymd_hms("2012-01-01 00:00:00")
start_2012
start_2012 + days(365)
start_2012 + years(1)

#10 
# Hint: Start from Jan 1, 2018

# start with the first day of novmeber
start = dmy("01-11-2018")

# find the weekday of Nov 1
wday(start,  label = T)

## add the weeks left
start + weeks(4)

## Thanksgiving for 2019
start = dmy("1.11.2019")
wday(start, label = T)
start + weeks(4) - days(1)


#7 

load("collisiondata.rda")

library(dplyr)

collision = collision %>%
  mutate(timestamp= paste(Collision.date, Collision.Time))

#7. 
pedestrians = collision %>%
  filter(Involved.With == "Pedestrian")

# convert the timestamp to date using lubridate
pedestrians$timestamp = dmy_hms(pedestrians$timestamp)

library(ggplot2)


ggplot(pedestrians, 
       aes(x = wday(timestamp, label = TRUE))) +
  geom_bar() + 
  xlab("")

#8. 

ggplot(pedestrians, 
       aes(x = month(timestamp, label = TRUE))) +
  geom_bar() + 
  xlab("")

#9. 

hmdata = pedestrians %>% 
  mutate(day = wday(timestamp, label = T),
         hour = hour(timestamp)) %>%
  group_by(day, hour) %>%
  summarise(count = n()) 


ggplot(hmdata, aes(x = day, 
                   y = factor(hour), 
                   fill = count)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white",
                      high = "darkred") + 
  xlab("") + 
  ylab("Hour of the Day") 


### 11. 

apple = read.csv("AAPL.csv")
google = read.csv("GOOG.csv")
amazon = read.csv("AMZN.csv")

## change the dates to date objects 

apple$Date = ymd(apple$Date)
google$Date = ymd(google$Date)
amazon$Date = ymd(amazon$Date)

## filter the data to after Jan 1, 2005

apple = apple %>%
  filter(Date > dmy("1-1-2005"))

google = google %>%
  filter(Date > dmy("1-1-2005"))

amazon = amazon %>%
  filter(Date > dmy("1-1-2005"))


ggplot(apple, aes(x = Date, y = Adj.Close)) + 
  geom_line()


install.packages("quantmod")
library(quantmod)


