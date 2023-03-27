library(tidyverse)

load('data.rds')

ear |> summary()

ear |> 
  ggplot(aes(x=infections)) +
  geom_histogram(binwidth = 5)

ear |> 
  mutate(
    freq = infections / persons
  ) |> 
  ggplot(aes(x=freq)) +
  geom_histogram(binwidth=.2)

ear |> 
  ggplot(aes(x=persons)) +
  geom_histogram(binwidth=5)
