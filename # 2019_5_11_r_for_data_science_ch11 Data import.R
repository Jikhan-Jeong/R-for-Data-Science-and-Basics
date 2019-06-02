# 2019_5_11_r_for_data_science_ch11 Data import
# name : jikan jeong
# reference: https://r4ds.had.co.nz/data-import.html

library(tidyverse)

data<- read_csv("C:/python/a_python/2019_nber_nlp/stata/data.csv")
head(data,1)

# read_csv() uses the first line of the data for the column names, which is a very common convention
read_csv("1,2,3\n 4,5,6")

# without col names
read_csv("1,2,3\n 4,5,6", col_names=FALSE)


read_csv("1,2,3\n 4,5,6", col_names= c("x", "y", "z"))


# 11.3 Parsing a vector # parsers 파싱하다 = 문법적 해부, 컴파일하다 = 기계어 번역 

str(parse_logical(c("TRUE","FALSE","NA")))

str(parse_integer(c("1","2","3")))

str(parse_date(c("2010-01-01","1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na= ".")

parse_double("1.23")

parse_double("1,23", locale = locale(decimal_mark = ","))

# parse_number() addresses the second problem: it ignores non-numeric characters 

parse_number("it cost $123.45")

# Factor

fruit <- c("apple", "grape")

parse_factor(c("apple", "grape", "banan"), levels = fruit)

# time

parse_datetime("2010-01-01T2010")

parse_datetime("2010-01-01")

parse_datetime("20100101")

parse_date("2010-10-01")

library(hms)

parse_time("01:10 am")

parse_time("20:10:01")


parse_date("01/02/15","%m/%d/%y")

head(data,1)

data <-tibble(data)
head(data)

data[["date"]]

parse_date(data[[2]], "%y/%m/%d")


# stragegy

guess_parser("2010-10-01")

# write a csv

write_csv(data, "data2.csv")

