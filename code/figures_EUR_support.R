
# RNG ---------------------------------------------------------------------

seed_proj <- 100
set.seed(seed_proj)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("ggrepel")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(ggrepel)

source(here("code", "functions", "recoding.R")) # recoding scheme
source(here("code", "functions", "lolliplot.R")) # custom ggplot2 plot

# Data ----------------------------------------------------------------

EUR_OS_support <-
  readRDS(here("data", "preprocessed", "rds", "cluster_6.rds")) 

questions <- unique(EUR_OS_support$question)

# Question 1, lollipop graph ----------------------------------------------------------------

question1 <- questions[1]

# EUR
lollipop_cluster6_question1 <-
  EUR_OS_support %>% 
  filter(question == question1) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_EUR_support_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 70,
    title = "Expected EUR support - EUR",
    title_size = 22
  )

lollipop_cluster6_question1

# save to file
ggsave(
  filename = "figure_EUR_support_EUR.png",
  plot = lollipop_cluster6_question1,
  device = "png",
  path = here("img", "support", "EUR"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_support$School)) {
  
  temp_figure_school <- 
    EUR_OS_support %>% 
    filter(
      question == question1 &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!Likert_EUR_support_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 90,
      title = paste0("Expected EUR support - ", i),
      title_size = 22
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_EUR_support_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "support", "EUR"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

question2 <- questions[3]

# EUR
lollipop_cluster6_question2 <-
  EUR_OS_support %>% 
  filter(question == question2) %>% 
  mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = reorder(response, perc), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 50,
    title = "Desired Open Science support - EUR",
    title_size = 22
  )

lollipop_cluster6_question2

# save to file
ggsave(
  filename = "figure_desired_support_EUR.png",
  plot = lollipop_cluster6_question2,
  device = "png",
  path = here("img", "support", "desired"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_support$School)) {
  
  temp_figure_school <- 
    EUR_OS_support %>% 
    filter(
      question == question2 &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>% 
    count(question, response) %>%
    mutate(
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = reorder(response, perc), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 70,
      title = paste0("Desired Open Science support - ", i),
      title_size = 22
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_desired_support_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "support", "desired"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------

question3 <- questions[2]

# EUR
lollipop_cluster6_question3 <-
  EUR_OS_support %>% 
  filter(question == question3) %>% 
  mutate(response = replace_na(response, "I don't know/prefer not to answer")) %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = reorder(response, perc), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 60,
    title = "FAIR support - EUR",
    title_size = 22
  )

lollipop_cluster6_question3

# save to file
ggsave(
  filename = "figure_FAIR_support_EUR.png",
  plot = lollipop_cluster6_question3,
  device = "png",
  path = here("img", "support", "FAIR"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_support$School)) {
  
  temp_figure_school <- 
    EUR_OS_support %>% 
    filter(
      question == question3 &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don't know/prefer not to answer")) %>% 
    count(question, response) %>%
    mutate(
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = reorder(response, perc), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 100,
      title = paste0("FAIR support - ", i),
      title_size = 22
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_FAIR_support_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "support", "FAIR"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
