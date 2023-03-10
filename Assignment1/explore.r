library('MASS')
library('tidyverse')
library(GGally)
library(patchwork)
data <- read.table("dioxin.csv", sep=',', head=TRUE, stringsAsFactors = TRUE)
data$TIME <- as.factor(data$TIME)


#dat <- dat %>%
 # group_by(PLANT) #%>%
#  mutate(DIOX=(DIOX - mean(DIOX, na.rm = T)) / sd(DIOX, na.rm=T))

data |> 
  ggplot(aes(x = LOAD, y=PRSEK, fill=DIOX)) +
  geom_tile()

data |> 
  select(where(\(x) any(is.na(x))))

data |> 
  select(c(PRSEK, QRAT)) |> 
  group_by(PRSEK) |> 
  summarise(
    mean_qrat=mean(QRAT),
    median_qrat = median(QRAT)
  )

data |>
  ggplot(aes(x=PRSEK, y=QRAT)) +
  geom_boxplot()
  
data |>
  replace_na(list(PRSEK = 'L')) |> 
  mutate(
    CAT = paste(
      'OX', OXYGEN, 'PRSEK', PRSEK, 'LOAD', LOAD, sep = '_'
    )
  ) |> 
  ggplot(aes(x=DIOX, color=OXYGEN)) +
  geom_freqpoly()
    
ox_pr <- data |> 
  replace_na(list(PRSEK = 'L')) |> 
  ggplot(aes(x=OXYGEN, y=PRSEK, fill=DIOX)) +
  geom_tile()+
  scale_fill_continuous(trans = 'log',type = 'viridis')

ox_lo <- data |> 
  replace_na(list(PRSEK = 'L')) |> 
  ggplot(aes(x=LOAD, y=OXYGEN, fill=DIOX)) +
  geom_tile()+
  scale_fill_continuous(trans = 'log',type = 'viridis')

pr_lo <- data |> 
  replace_na(list(PRSEK = 'L')) |> 
  ggplot(aes(x=LOAD, y=PRSEK, fill=DIOX)) +
  geom_tile()+
  scale_fill_continuous(trans = 'log',type = 'viridis')

ox_pr + ox_lo + pr_lo + plot_layout(guides='collect')

|> lm_with_cor <- function(data, mapping, ..., method = "pearson") {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  cor <- cor(x, y, method = method, use = "na.or.complete")
  ggally_smooth_lm(data, mapping, ...) +
    ggplot2::geom_label(
      data = data.frame(
        x = min(x, na.rm = TRUE),
        y = max(y, na.rm = TRUE),
        lab = round(cor, digits = 3)
      ),
      mapping = ggplot2::aes(x = x, y = y, label = lab),
      hjust = 0, vjust = 1,
      size = 5, fontface = "bold",
      inherit.aes = FALSE # do not inherit anything from the ...
    )
}

data |>
  replace_na(list(PRSEK = 'L')) |> 
  mutate(DIOX = log(DIOX)) |> 
  ggduo(
    columnsX = c("O2COR", "NEFFEKT", "QRAT"), 
    columnsY = c("DIOX", "QROEG", "TOVN", "TROEG", "POVN", "CO2", "CO", "SO2", "HCL", "H2O"),
    types = list(continuous = lm_with_cor)
  )
  
O2COR_passive <- data |> 
  replace_na(list(PRSEK = 'L')) |> 
  mutate(DIOX = log(DIOX)) |> 
  ggduo(
    columnsX = c("O2COR"), 
    columnsY = c("CO2", "SO2", "H2O", "CO"),
    types = list(continuous = lm_with_cor)
  )

O2COR_passive

NEFFEKT_passive <- data |> 
  replace_na(list(PRSEK = 'L')) |> 
  mutate(DIOX = log(DIOX)) |> 
  ggduo(
    columnsX = c("NEFFEKT"), 
    columnsY = c("TROEG", "CO2", "SO2", "H2O", "CO"),
    types = list(continuous = lm_with_cor)
  )

NEFFEKT_passive
#DIOX

#BLOCK EFFECTS
    #PLANT
    #TIME
    #LAB

QRAT_passive<- data |> 
  replace_na(list(PRSEK = 'L')) |> 
  mutate(DIOX = log(DIOX)) |> 
  ggduo(
    columnsX = c("QRAT"), 
    columnsY = c("QROEG", "TOVN", "CO"),
    types = list(continuous = lm_with_cor)
  )
QRAT_passive

wrap_elements(ggmatrix_gtable(O2COR_passive)) + wrap_elements(ggmatrix_gtable(NEFFEKT_passive)) + wrap_elements(ggmatrix_gtable(QRAT_passive))

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
