# 2019_04_23_ch5_R for data science_5 Data transformation before 5.6
# basic is the best strategic
# reference: 

seq(1,10)
(y<- seq(1,10, length.out=5))


library(nycflights13)
library(tidyverse)

flights
view(flights)
# Tibbles are data frames, but slightly tweaked to work better in the tidyverse.

# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).


# filter
(filter(flights, month ==1, day ==1))

jan1 <- filter(flights, month ==1, day ==1)
jan1

(filter(flights, month == 1))

(filter(flights, month ==1 | month ==2))

(nov_dec <- filter(flights, month %in% c(1,2)))

x = NA
is.na(x)


df <- tibble(x = c(1,NA))
filter(df, x==1)


# arrange

arrange(flights, year, month, day)

arrange(flights, year)


# 5.4 Select columns with select()

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

colnames((flights))
rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())

# 5.5 Add new variables with mutate()

flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, gain = dep_delay - arr_delay, speed = distance / air_time * 60)

transmute(flights, gain = dep_delay - arr_delay,
          hours = air_time /60,
          gain_per_hour = gain / hours
          )

transmute(flights,
          dep_time,
          hour = dep_time %/%100,
          minute = dep_time %% 100
          )

(x <- 1:10)

log(x)
lead(x)
lag(x)


cumsum(x)
cummean(x)

y<-c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))


# 5.6 Grouped summaries with summarise()

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# summarise() is not terribly useful unless we pair it with group_by()

# https://r4ds.had.co.nz/transform.html
