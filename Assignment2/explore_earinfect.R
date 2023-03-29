library(tidyverse)

# loads dataframes 'ear' and 'cloth'
load('data.rds')


ear |> 
  ggplot(aes(x=infections/persons, fill=location)) +
  geom_histogram()
