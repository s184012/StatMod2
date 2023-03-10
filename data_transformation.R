rm(list=ls())
library(caret)


# Load Data ---------------------------------------------------------------

raw = read.csv("dioxin.csv", header = TRUE, sep = ",")
print(sapply(raw, typeof))
raw <- as.data.frame(unclass(raw),stringsAsFactors = TRUE)
raw$TIME = as.factor(raw$TIME)

print(sapply(raw, typeof))


# Transform data ----------------------------------------------------------
raw$PRSEK[15:16] = c("L","L")

data = raw |> 
  replace_na(list(PRSEK = "L")) |> 
  filter(!if_any(everything(), function(x) is.na(x)))

sum(is.na(data))
save(data, raw, file='data.rds')
