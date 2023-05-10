rm(list=ls())
library(tidyverse)
library(ggfortify)
library(patchwork)
load('data.rds')

save_fig = function(figure = last_plot(), name = NULL, show_plot=T) {
  if (is.null(name)) {
    fig <- enquo(figure)
    name = as_label(fig)
  }
  ggsave(plot = figure, file = paste0(name, '.pdf'), path = 'figs', width=10*0.8, height = 6*0.8)
  return(figure)
}

glimpse(cloth)
cloth |> 
  filter(day != 4) |> 
  mutate(
    day = paste("Day", day),
    sex = str_to_title(sex)
  ) |> 
  ggplot(aes(time2, clo)) +
  geom_line(aes(group=subjId)) +
  facet_grid(rows=vars(day), cols=vars(sex)) +
  scale_x_continuous(breaks = 1:6, labels = 1:6, minor_breaks = NULL) +
  labs(
    title = "Variation between subjects each day",
    x = "Time point of meassurement",
    y= "Clo"
  ) -> var_between_subjects_plot 

save_fig(var_between_subjects_plot)

cloth |> 
  ggplot(aes(tInOp, tOut, color=clo)) +
  geom_point(color='black', size = 1.5) +
  geom_point(size=1) +
  scale_color_distiller(type='div', palette = 1) +
  labs(
    title="Clo interaction with inside and outside temperature",
    y = "Outside Temperature",
    x = "Inside Operating Temperature",
    color = "Clo"
    ) -> 
  temperature_interaction_with_clo
save_fig(temperature_interaction_with_clo)

cloth |> 
  filter(day != 4) |> 
  mutate(
    day = paste('Day', day)
  ) |> 
  ggplot(aes(day, clo)) +
  geom_boxplot() +
  labs(
    title = "Distribution of clo each day",
    x=NULL,
    y="Clo"
  ) -> 
  day_variation

exploration_plot <- var_between_subjects_plot | (temperature_interaction_with_clo / day_variation)
save_fig(exploration_plot)
