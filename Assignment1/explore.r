library('tidyverse')
dat <- read.table("dioxin.csv", sep=',', head=TRUE)

alpha <- 0.05


#DIOX

#BLOCK EFFECTS
    #PLANT
    #TIME
    #LAB

#Active variables


dat %>%
    filter(
        TIME == 1
    ) %>%
    ggplot(aes(x=DIOX)) +
    geom_density() +
    facet_grid(cols = vars(PLANT), rows = vars(LAB))

