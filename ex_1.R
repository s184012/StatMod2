library(tidyverse)
library(kableExtra)
library(patchwork)
library(scales)

load('data.rds')

save_table = function(table, name) {
  cat(
    kbl(table, 'latex', booktabs=T), 
    file=paste('tables/', name, '.tex', sep="")
  )
}

save_fig = function(name, figure = last_plot()) {
  ggsave(plot = figure, file = paste(name, '.pdf', sep=''), path = 'figs', width=10*0.8, height = 6*0.8)
}
# Missing Values ----------------------------------------------------------

raw |>
  filter(if_any(everything(), is.na))

prsek_missing <- raw |> 
  filter(is.na(PRSEK)) |> 
  select(PLANT, LAB, TIME, PRSEK, O2, O2COR) |> 
  show() |> 
  save_table("prsek_missing_values")

prsek_missing_summary <- raw |> 
  select(PLANT, PRSEK, O2, O2COR) |> 
  filter(PLANT == 'KARA') |> 
  group_by(PRSEK) |> 
  summarise(
    mean_O2 = mean(O2),
    mean_O2COR = mean(O2COR)
  ) |> 
  print() |> 
  save_table('prsek_missing_summary')

passive_missing <- raw |> 
  select(PLANT, LAB, TIME, CO, SO2) |> 
  filter(is.na(CO) | is.na(SO2)) |> 
  show() |> 
  save_table('passive_missing')
  

# Summary Stats -----------------------------------------------------------

plant_lab_summary <- data |> 
  select(PLANT, LAB, DIOX) |> 
  group_by(PLANT, LAB) |> 
  summarise(
    Min = min(DIOX),
    Q25 = quantile(DIOX, .25),
    Mean = mean(DIOX),
    Median = median(DIOX),
    Q75 = quantile(DIOX, .75),
    Max = max(DIOX),
    Std.Error = sd(DIOX)
  ) |> 
  show()

prediction_summary = data |> 
  select(DIOX) |> 
  summarise(across(where(is.numeric), .fns = 
                     list(Min = min,
                          Median = median,
                          Mean = mean,
                          Std.Error = sd,
                          Q25 = ~quantile(., 0.25),
                          Q75 = ~quantile(., 0.75),
                          Max = max))) %>%
  pivot_longer(everything(), names_sep='_', names_to=c('Variable', '.value')) |> 
  show() |> 
  save_table('prediction_summary')

active_summary = data |> 
  select(O2, O2COR, NEFFEKT, QRAT) |> 
  summarise(across(where(is.numeric), .fns = 
                     list(Min = min,
                          Median = median,
                          Mean = mean,
                          Std.Error = sd,
                          Q25 = ~quantile(., 0.25),
                          Q75 = ~quantile(., 0.75),
                          Max = max))) %>%
  pivot_longer(everything(), names_sep='_', names_to=c('Variable', '.value')) |> 
  show() |> 
  save_table('active_summary')

passive_summary <- data |> 
  select(-OBSERV, -DIOX, -PLANT, -TIME, -LAB, -O2, -O2COR, -NEFFEKT, -QRAT) |> 
  summarise(across(where(is.numeric), .fns = 
                     list(Min = min,
                          Q25 = ~quantile(., 0.25),
                          Median = median,
                          Mean = mean,
                          Q75 = ~quantile(., 0.75),
                          Max = max,
                          Std.Error = sd
                          )
                   )
            ) %>%
  pivot_longer(everything(), names_sep='_', names_to=c('Variable', '.value')) |> 
  show() 
  save_table('passive_summary')

# Prediction Distribution Plots ------------------------------------------------------

# histogram
pred_hist <- data |> 
  ggplot(aes(x=DIOX)) +
  geom_histogram(binwidth = 100) +
  labs(
    title = "Distribution of DIOX",
    y = "Count"
  )

# qq-plot
pred_qq <- data |> 
  ggplot(aes(sample = DIOX))+
  geom_qq() +
  geom_qq_line() +
  labs(
    y="DIOX",
    x=NULL
  )

# log histogram
log_pred_hist <- data |> 
  ggplot(aes(x=DIOX)) +
  geom_histogram(bins=15) +
  scale_x_continuous(
    trans='log10', 
    labels=label_log(),
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  labs(
    title = "Distribution of log(DIOX)",
    y='Count'
  )
  
log_pred_hist  

# Log qq
log_pred_qq <- data |> 
  ggplot(aes(sample = log10(DIOX)))+
  geom_qq() +
  geom_qq_line() +
  labs(
    x=NULL,
    y='log(DIOX)'
  )

pred_dist <- pred_hist / pred_qq
save_fig('prediction_dist', figure = pred_dist)
non_trans <- pred_hist / pred_qq
trans <- log_pred_hist / log_pred_qq
non_trans | trans 
ggsave(file = 'prediction_and_log_dist.pdf', path = 'figs', width=10*0.8, height = 6*0.8)


trans = MASS::boxcox(data$DIOX ~ 1)
trans$x[which.max(trans$y)]

# DIOX emissions for different plants
diox_plant_dist <- data |> 
  ggplot(aes(x = PLANT, y=DIOX)) +
  geom_boxplot() + 
  scale_y_continuous(
    trans='log10', 
    labels=label_log(),
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  labs(
    title = "Distribution of DIOX across different plants"
  )
diox_plant_dist
save_fig('diox_plant_dist', diox_plant_dist)

diox_time_dist <- data |> 
  filter(PLANT=='RENO_N') |> 
  ggplot(aes(x = TIME, y=DIOX)) +
  geom_boxplot() + 
  scale_y_continuous(
    trans='log10', 
    labels=label_log(),
    breaks = trans_breaks("log10", function(x) 10^x)
  )
diox_time_dist
save_fig('diox_time_dist', diox_time_dist)

data |> 
  ggplot(aes(x = LAB, y=DIOX)) +
  geom_boxplot() + 
  scale_y_continuous(
    trans='log10', 
    labels=label_log(),
    breaks = trans_breaks("log10", function(x) 10^x)
  )



  

# Within correlation ------------------------------------------------------
active <- data |> 
  select(O2, O2COR, NEFFEKT, QRAT)

passive <- data |> 
  select(QROEG:H2O)

passive |> ggpairs()

co_distribution <- passive |> 
  ggplot(aes(x=CO)) +
  geom_histogram(binwidth = 15) +
  labs(
    title = "CO distribution",
    y="Count"
  )

co_boxplot <- passive |> 
  ggplot(aes(y=CO)) +
  geom_boxplot()
co_boxplot

hcl_distribution <- passive |> 
  ggplot(aes(x=HCL)) +
  geom_histogram(binwidth = 150) +
  labs(
    title = "HCL Distribution",
    y = "Count"
  )

hcl_boxplot <- passive |> 
  ggplot(aes(y=HCL)) +
  geom_boxplot()
hcl_boxplot

qroeg_vs_tovn <- passive |> 
  ggplot(aes(x=QROEG, y=TOVN)) +
  geom_point() +
  geom_smooth(method='lm', se = F)
qroeg_vs_tovn

qroeg_vs_troeg <- passive |> 
  ggplot(aes(x=QROEG, y=TROEG)) +
  geom_point() +
  geom_smooth(method='lm', se = F)
qroeg_vs_troeg

co2_vs_h2o <- passive |> 
  ggplot(aes(x=CO2, y=H2O)) +
  geom_point() +
  geom_smooth(method='lm', se = F)

passive_corr <- (qroeg_vs_tovn | qroeg_vs_troeg | co2_vs_h2o) + plot_annotation(title = "Correlations between passive variables")
save_fig('passive_corr', passive_corr)

passive_outlier <- co_distribution / co_boxplot | hcl_distribution / hcl_boxplot
passive_outlier
save_fig('passive outliers', passive_outlier)
# Between correlations ----------------------------------------------------

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
    columnsY = c("CO2", "SO2", "H2O"),
    types = list(continuous = lm_with_cor)
  )

O2COR_passive

NEFFEKT_passive <- data |> 
  replace_na(list(PRSEK = 'L')) |> 
  mutate(DIOX = log(DIOX)) |> 
  ggduo(
    columnsX = c("NEFFEKT"), 
    columnsY = c("TROEG", "CO2", "SO2", "H2O"),
    types = list(continuous = lm_with_cor)
  )

NEFFEKT_passive

QRAT_passive<- data |> 
  replace_na(list(PRSEK = 'L')) |> 
  mutate(DIOX = log(DIOX)) |> 
  ggduo(
    columnsX = c("QRAT"), 
    columnsY = c("QROEG", "TOVN"),
    types = list(continuous = lm_with_cor)
  )
QRAT_passive

active_passive_corr <- wrap_elements(ggmatrix_gtable(O2COR_passive)) + 
  wrap_elements(ggmatrix_gtable(NEFFEKT_passive)) + 
  wrap_elements(ggmatrix_gtable(QRAT_passive)) +
  plot_annotation(title="Corralations between active and passive variables")
active_passive_corr
save_fig('active_passive_corr', active_passive_corr)
