# 2019_04_30_ch5_R for data science_5 Data transformation before 5.6
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

by_day <- group_by(flights, year, month, day)
by_day
head(by_day,1)
colnames(by_day)
summary(by_day$dep_delay)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count =n(),
                   dist =mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)

                                     )
delay <- filter(delay, count > 20, dest !="HNL")

ggplot(data = delay, mapping = aes(x=dist, y=delay)) + geom_point(aes(size = count), alpha =1/3) + geom_smooth(se=FALSE)

delays <- flights %>% 
  group_by(dest) %>%
  summarise(
    count =n(),
    dist = mean(distance, na.rm =TRUE),
    delay = mean(arr_delay, na.rm=TRUE)
  ) %>%
  filter(count >20, dest != "HNL")

#Behind the scenes, x %>% f(y) turns into f(x, y), and x %>% f(y) %>% g(z) turns into g(f(x, y), z)

# 5.6.2 Missing values

flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay))

flights %>%
  group_by(year, month, day) %>%
  summarise(mean = mean(dep_delay, na.rm = TRUE))

# missing value as a canceled flight

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  group_by(month, day) %>%
  summarise(mean = mean(dep_delay))

# 5.6.3 Counts

delays <- not_cancelled %>% group_by(tailnum) %>% summarise(delay = mean(arr_delay))
ggplot(data = delays, mapping =aes(x = delay)) + geom_freqpoly(binwidth =5)

delays <- not_cancelled %>%
        group_by(tailnum) %>%
        summarise(
          delay = mean(arr_delay, na.rm = TRUE),
          n=n()
        )

ggplot(data = delays, mapping = aes(x=n, y = delay)) + geom_point(alpha =1/10)                                                            
delays %>%
  filter(n>25) %>%
  ggplot(mapping = aes(x =n, y = delay)) +
  geom_point(alpha = 1/10)

# convert to a tibble so it prints nicely

batting <- as_tibble(Lahman::Batting)

head(batting,3)

batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm =TRUE),
    ab = sum(AB, na.rm = TRUE)
    )

batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x =ab, y = ba)) +
  geom_point() +
  geom_smooth(se=FALSE)

batters %>%
  arrange(desc(ba))

# 5.6.4 Useful summary functions

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )

not_cancelled %>%
  group_by(year) %>%
  summarise(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r=min_rank(desc(dep_time))) %>%
  filter(r %in% range(r))


# 5.6.5 Grouping by multiple variables
# https://r4ds.had.co.nz/transform.html#summarise-funs

daily <- group_by(flights, year, month, day)
head(daily,1)

(per_day <- summarise(daily, flights =n()))

(per_month <- summarise(per_day, flights = sum(flights)))

(per_year <- summarise(per_month, flights =sum(flights)))

daily %>%
  ungroup() %>%
  summarise


# 5.7 Grouped mutates (and filters)

flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) <10)

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n() >365)

popular_dests

popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)


