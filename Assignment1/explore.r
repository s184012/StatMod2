library('tidyverse')
library('MASS')
dat <- read.table("dioxin.csv", sep=',', head=TRUE, stringsAsFactors = TRUE)
dat$TIME <- as.factor(dat$TIME)


#dat <- dat %>%
 # group_by(PLANT) #%>%
#  mutate(DIOX=(DIOX - mean(DIOX, na.rm = T)) / sd(DIOX, na.rm=T))





alpha <- 0.05

#DIOX

#BLOCK EFFECTS
    #PLANT
    #TIME
    #LAB

#Active variables
scaleFUN <- function(x) sprintf("%.2f", x)




factor(x=c("KARA", "RENO_N", "RENO_S"), levels = levels(dat$PLANT))

dat |> 
  filter(if_any(everything(), is.na))

dat %>%
    filter(
        #TIME =
    ) %>%
    ggplot(aes(x=DIOX, color=PRSEK)) +
    geom_freqpoly(position = 'stack', bins=5) +
    facet_wrap(rows = vars(LAB), cols = vars(PLANT), scale="free")
    scale_x_continuous(trans="log", labels = scaleFUN)

dat %>% 
  ggplot(aes(x=DIOX)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(trans="log", labels = scaleFUN)

dat |> 
  ggplot(aes(x=DIOX, color=PRSEK)) +
  geom_histogram(bins=10) +
  facet_wrap(vars(Oxygen=OXYGEN), ncol=1, ) +
  scale_x_continuous(trans="log")

dat 

dat %>%
  filter(
    LAB=="KK",
  ) %>%
  ggplot(aes(x=DIOX, fill=PRSEK)) +
  geom_histogram(alpha=.4) 
                     
bc <- boxcox(lm(data = dat, DIOX ~1))

bc$x[which.max(bc$y)]
