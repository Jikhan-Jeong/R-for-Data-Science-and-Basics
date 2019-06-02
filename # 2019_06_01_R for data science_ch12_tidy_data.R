# 2019_06_01_r_for_ds_ch12_Tidy_data
# name : Jikhan Jeong
# reference : https://r4ds.had.co.nz/tidy-data.html

library(tidyverse)
table1

# Put each dataset in a tibble.
# Put each variable in a column.

# two main advantages of tidy________________________________________________________________

table1 %>%
  mutate(rate = cases / population *10000) # making new variable with mutate tool


# compute cases per yaer_____________________________________________________________________

table1 %>%
  count(year, wt = cases)


# visualise changes over time________________________________________________________________

library(ggplot2)

ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))



# 12.3 Spreading and gathering________________________________________________________________

# gather() makes wide tables narrower and longer; spread() makes long tables shorter and wider.

# 12.3.1 gatering_____________________________________________________________________________

table4a

table4a %>%
  gather('1999','2000', key ="year", value ="cases")

tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

tidy4b

left_join(tidy4a, tidy4b)

# 12.3.2 spreading___________________________________________________________________________

table2

table2 %>%
  spread(key = type, value =count) # type = {case, population}



# 12.4 separating and uniting

# 12.4.1 sparate

table3

table3 %>%
  separate(rate, into =c("cases", "population")) # rate = {cases, population}


table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/") # values are charaters

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE) # convert : chr -> int


table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

# 12.4.2 Unite
#  combines multiple columns into a single column

table5 

table5 %>%
  unite(new, century, year) # new is hypened = century_year = 19_99

table5 %>% 
  unite(new, century, year, sep = "") # 1999


# 12.5 Missing values

# 2 possible of missing values
# Explicitly, i.e. flagged with NA.
# Implicitly, i.e. simply not present in the data.

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

# The return for the fourth quarter of 2015 is explicitly missing, 
# because the cell where its value should be instead contains NA.

# The return for the first quarter of 2016 is implicitly missing,
# because it simply does not appear in the dataset.

stocks

stocks %>%
  spread(year, return)  # spread by year 2015 2016 and the value is return


stocks %>%
  spread(year, return) %>%
  gather(year, return, '2015':'2016', na.rm = TRUE)

stocks %>%
  complete(year, qtr) # column year, qtr unique set

# complete() takes a set of columns, and finds all unique combinations. 
# It then ensures the original dataset contains all those values, filling in explicit NAs where necessary.

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment

treatment %>%
  fill(person) # fill -> NA replaced by the most recent one

# 12.6 Case Study

who
summary(who)
dim(who)


who1 <- who %>%
  gather(new_sp_m014:newrel_f65, key ="key", value = "cases", na.rm = TRUE)

who1 %>%
  count(key)

who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who2


who3 <- who2 %>%
  separate(key, c("new","type","gender"), sep ="_")

who3


who3 %>%
  count(new)

who4 <- who3 %>%
  select(-new, -iso2, -iso3)

who4


who5 <- who4 %>%
  separate(gender, c("sex", "age"), sep = 1)

who5
