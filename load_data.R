library(readr)
# Load clothing.csv -------------------------------------------------------

cloth = read_csv('clothing.csv', col_types = "idddffdf")
cloth <- cloth |> 
  rename(id = ...1)

# Load earinfect.txt ------------------------------------------------------

ear <- read_delim('earinfect.txt', delim = " ", col_types = "ffffii")

save(cloth, ear, file = "data.rds")
