# 2019_5_7_r_for_data_science_ch10 Tibbles
# name : jikan jeong
# reference: https://r4ds.had.co.nz/tibbles.html

library(tidyverse)
class(iris)
as_tibble(iris)

tibble(x=1:5, y=1, z=x+7)

tb <-tibble(";)" ="ses", 'z' ="sse", '20'='number)')
tb            

library(lubridate)

tibble(
  a = lubridate:: now() + runif(1e3) * 86400,
  b = lubridate:: today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)
  
nycflights13::flights %>%
  print(n=2, width=Inf)


nycflights13::flights %>%
  View()

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

df

colnames(df)

# Extract by name
df$x

df[["x"]]

# Extract by position
df[[1]]

# To use these in a pipe, you¡¯ll need to use the special placeholder .

df %>% .$x 

df_dataframe <- as.data.frame(df)
class(df_dataframe)




