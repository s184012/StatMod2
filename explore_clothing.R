library(tidyverse)

data = read_csv('clothing.csv', col_types = "idddffdf")
data <- data |> 
  rename(id = ...1)

data |> glimpse()
data |> summary()

multiple_data_pr_day_pr_person <- data |> 
  filter(subjId == 11)

no_missing_values <- data |> 
  summarise(
    across(everything(), ~ sum(is.na(.x)))
  )

how_many_subjects <- data |> 
  select(subjId) |> 
  n_distinct()
how_many_subjects

how_many_men_vs_female <- data |> 
  ggplot(aes(x=sex)) +
  geom_bar()
how_many_men_vs_female

observations_pr_subject <- data |> 
  mutate(
    subjId = fct_reorder(subjId, subjId, .fun = 'length')
  ) |> 
  ggplot(aes(x=subjId)) +
  geom_bar()
observations_pr_subject

observations_pr_day <- data |> 
  ggplot(aes(x=day, fill=sex)) +
  geom_bar()
observations_pr_day

distribution_time_interval <- data |> 
  ggplot(aes(x=time)) +
  geom_histogram(binwidth=0.02)
distribution_time_interval

distribution_time_of_day <- data |> 
  group_by(subjId, day) |> 
  mutate(
    time_of_day = cumsum(time)
  ) |> 
  ggplot(aes(x=time_of_day)) +
  geom_histogram(binwidth = .05)
distribution_time_of_day

data |> 
  ggplot(aes(x=log(clo))) +
  # geom_histogram(binwidth = .3) +
  geom_density(aes(fill=sex), alpha=.5)

data |> 
  ggplot(aes(sample=clo)) +
  geom_qq() +
  geom_qq_line()
                 
more_variance_in_women_clothing <- data |> 
  ggplot(aes(y=clo, fill=sex, x = day, )) +
  geom_violin(position = 'dodge')
more_variance_in_women_clothing

women_dress_according_to_wheather <- data |> 
  ggplot(aes(x = tInOp, y=tOut, color=clo)) +
  geom_point(size=4, alpha=.5, position = "jitter") +
  facet_wrap(vars(sex)) +
  scale_color_distiller(type='div', palette = 5)
women_dress_according_to_wheather

mean_variance_pr_person <- data |> 
  group_by(sex, subjId) |> 
  summarise(
    clo_sd = sd(clo),
    .groups = 'drop_last'
  ) |> 
  summarise(
    mean_clo_sd = mean(clo_sd)
  )
mean_variance_pr_person

development_in_clo_pr_person_pr_day <- data |> 
  group_by(subjId, day, .groups="drop") |> 
  mutate(time = cumsum(time)) |> 
  group_by(subjId, day, .groups="drop") |> # count observations pr. person pr. day
  mutate(n = n()) |> 
  filter(n > 2) |> # only include subjects with more than two observations that day.
  ggplot(aes(x=time, y=clo)) +
  geom_line(aes(group=subjId)) +
  facet_grid(rows = vars(sex), cols=vars(day))
development_in_clo_pr_person_pr_day

temperature_is_independent_of_subject_and_day <- data |> 
  group_by(subjId, day, .groups="drop") |> 
  mutate(time=cumsum(time)) |> 
  ggplot(aes(x=time)) +
  geom_line(aes(y = tOut, group=subjId)) +
  facet_wrap(vars(day))
temperature_is_independent_of_subject_and_day


