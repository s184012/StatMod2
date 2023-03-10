library(tidyverse)
library(kableExtra)
library(patchwork)
library(scales)
library(janitor)
library(GGally)

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

all_missing <- raw |>
  filter(if_any(everything(), is.na)) |> 
  t() |>
  row_to_names(1)
save_table(all_missing, name = "all_missing")

prsek_missing <- raw |> 
  filter(is.na(PRSEK)) |> 
  select(PLANT, LAB, TIME, PRSEK, O2, O2COR) |> 
  save_table("prsek_missing_values")

prsek_missing_summary <- raw |> 
  select(LAB, PLANT, PRSEK, O2, O2COR) |>
  filter(PLANT == 'KARA') |>
  group_by(PRSEK) |>
  summarise(
    mean_O2 = mean(O2),
    mean_O2COR = mean(O2COR)
  ) |>
save_table('prsek_missing_summary')


passive_missing <- raw |> 
  select(PLANT, LAB, TIME, CO, SO2) |> 
  filter(is.na(CO) | is.na(SO2))
save_table(passive_missing, 'passive_missing')
  

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
  )

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
  mutate(
    across(where(is.numeric), function(x) round(x, 2))
  )
save_table(prediction_summary, 'prediction_summary')

active_summary = data |> 
  select(O2, O2COR, NEFFEKT, QRAT) |> 
  summarise(across(where(is.numeric), .fns = 
                     list(Min = min,
                          Q25 = ~quantile(., 0.25),
                          Median = median,
                          Mean = mean,
                          Q75 = ~quantile(., 0.75),
                          Max = max,
                          Std.Error = sd
                          ))) %>%
  pivot_longer(everything(), names_sep='_', names_to=c('Variable', '.value')) |> 
  mutate(
    across(where(is.numeric), function(x) round(x, 2))
  ) 
save_table(active_summary, 'active_summary')

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
  mutate(
    across(where(is.numeric), function(x) round(x, 2))
  ) 
passive_summary  
save_table(passive_summary, 'passive_summary')



# Block Distribution ------------------------------------------------------

plant_lab_dist <- data |> 
  ggplot(aes(x=PLANT, fill=LAB)) +
  geom_bar() +
  scale_fill_brewer(type = "qual", palette = 3)
  
plant_time_dist <- data |> 
  ggplot(aes(x=PLANT, fill=TIME)) +
  geom_bar() +
  scale_fill_brewer(type = "qual", palette = 3)
  
lab_active_dist <- data |> 
  ggplot(aes(x=LAB, fill=interaction(OXYGEN, PRSEK, LOAD, sep=", "))) +
  geom_bar(position="fill") +
  scale_y_continuous(labels = label_percent()) +
  labs(
    y="% of total",
    fill = "OXYGEN, PRSEK, LOAD"
  ) +
  scale_fill_brewer(type = "qual", palette = 3)

block_dist <- plant_time_dist / plant_lab_dist | lab_active_dist
block_dist
save_fig('block_dist', block_dist)


# Active Distribution -----------------------------------------------------


# histogram
data |> 
  ggplot(aes(x=O2COR)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Distribution of DIOX",
    y = "Count"
  )

# Passive Distribution ----------------------------------------------------

passive_distribution <- data |> 
  pivot_longer(
    QROEG:H2O,
    names_to = 'Variable',
    values_to = 'Value'
  ) |> 
  ggplot(aes(x=Value)) +
  geom_density() +
  facet_wrap(vars(Variable), scales = 'free') +
  labs(
    x=NULL,
    y='Density',
    title="Distribution of passive variables"
  )
passive_distribution

save_fig('passive_distribution', passive_distribution)
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
pred_dist
save_fig('prediction_dist', figure = pred_dist)
non_trans <- pred_hist / pred_qq
trans <- log_pred_hist / log_pred_qq
non_trans | trans 
ggsave(file = 'prediction_and_log_dist.pdf', path = 'figs', width=10*0.8, height = 6*0.8)

par(mfrow=c(1,1))
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

data |> 
  ggplot(aes(x = PLANT, y=DIOX, color=LAB)) +
  geom_boxplot() + 
  scale_y_continuous(
    trans='log10', 
    labels=label_log(),
    breaks = trans_breaks("log10", function(x) 10^x)
  )

data |> 
  pivot_longer(
    cols = c(OXYGEN, PRSEK, LOAD),
    names_to = 'Variable',
    values_to = 'Level'
  ) |> 
  ggplot(aes(x = Level, y=DIOX, color=LAB)) +
  geom_boxplot() + 
  scale_y_continuous(
    trans='log10', 
    labels=label_log(),
    breaks = trans_breaks("log10", function(x) 10^x)
  ) +
  facet_wrap(facets = vars(Variable))




  

# Within correlation ------------------------------------------------------
active <- data |> 
  select(O2, O2COR, NEFFEKT, QRAT)

passive <- data |> 
  select(QROEG:H2O)


active_distribution <- active |> ggpairs()
save_fig('active_distribution', active_distribution)

co_distribution <- passive |> 
  ggplot(aes(x=CO)) +
  geom_histogram(binwidth = 15) +
  labs(
    title = "CO distribution",
    y="Count"
  )

co_boxplot <- passive |> 
  ggplot(aes(x=CO)) +
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
  ggplot(aes(x=HCL)) +
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
passive_corr
save_fig('passive_corr', passive_corr)

passive_outlier <- co_distribution / co_boxplot | hcl_distribution / hcl_boxplot
passive_outlier
save_fig('passive outliers', passive_outlier)
# Between correlations ----------------------------------------------------
lm_with_cor <- function(data, mapping, ..., method = "pearson") {
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


# Interactions ------------------------------------------------------------

possible_plant_o2_interaction <- data |> 
  ggplot(aes(x = O2COR, y=DIOX, color=PLANT)) + 
  geom_point() +
  geom_smooth(method='lm') +
  scale_y_continuous(trans='log10')
possible_plant_o2_interaction
save_fig('possible_plant_o2_interaction', possible_plant_o2_interaction)

possible_plant_qrat_interaction <- data |> 
  ggplot(aes(x = QRAT, y=DIOX, color=PLANT)) + 
  geom_point() +
  geom_smooth(method='lm') +
  scale_y_continuous(trans='log10')
possible_plant_qrat_interaction
save_fig('possible_plant_qrat_interaction', possible_plant_qrat_interaction)

data |> 
  filter(PLANT == 'RENO_N') |> 
  ggplot(aes(x = QRAT, y=DIOX, color=LOAD)) + 
  geom_point() +
  geom_smooth(method='lm') +
  scale_y_continuous(trans='log10')
possible_plant_qrat_interaction
save_fig('possible_plant_qrat_interaction', possible_plant_qrat_interaction)

passive_plant_interactions <- data |> 
  pivot_longer(
    cols=QROEG:H2O,
    names_to = 'Variable',
    values_to = 'Meassurement'
  ) |> 
  ggplot(aes(x=Meassurement, y=DIOX, color=PLANT)) +
  geom_point() +
  geom_smooth(method='lm') +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(Variable), scales = 'free') +
  labs(
    title = 'Interaction between passive variables and PLANT',
    x = NULL
  )
passive_plant_interactions
save_fig('passive_plant_interactions', passive_plant_interactions)

active_plant_interactions <- data |> 
  pivot_longer(
    cols=O2COR:QRAT,
    names_to = 'Variable',
    values_to = 'Meassurement'
  ) |> 
  ggplot(aes(x=Meassurement, y=DIOX, color=PLANT)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(Variable), scales = 'free') +
  labs(
    title = 'Interaction between active variables and PLANT',
    x = NULL
  )
active_plant_interactions
save_fig('active_plant_interactions', active_plant_interactions)

data |> 
  pivot_longer(
    cols=O2COR:QRAT,
    names_to = 'Variable',
    values_to = 'Meassurement'
  ) |> 
  ggplot(aes(x=Meassurement, y=DIOX, color=PLANT)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  scale_y_continuous(trans='log10') +
  facet_wrap(facets = vars(Variable, PLANT), scales = 'free') +
  labs(
    title = 'Interaction between active variables and PLANT',
    x = NULL
  )

data |> 
  pivot_longer(
    cols=c(CO, HCL),
    names_to = 'Variable',
    values_to = 'Meassurement'
  ) |> 
  ggplot(aes(x=Meassurement, y=DIOX)) +
  geom_point() +
  geom_smooth(method='lm', formula = y~x^2, se=F) +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='sqrt') +
  facet_wrap(facets = vars(Variable), scales = 'free') +
  labs(
    title = 'Interaction between passive variables and PLANT',
    x = NULL
  )
  



data |> 
  mutate(DIOX = log(DIOX)) |> 
  ggduo(columnsY = "DIOX", columnsX = 9:12)
