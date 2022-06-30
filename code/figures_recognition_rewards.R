
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

EUR_OS_recognition_rewards <-
  readRDS(here("data", "preprocessed", "rds", "cluster_7.rds")) 

questions <- unique(EUR_OS_recognition_rewards$question)

# Question 1, lollipop graph ----------------------------------------------------------------

question1 <- questions[1]

# EUR
lollipop_cluster7_question1 <-
  EUR_OS_recognition_rewards %>% 
  filter(question == question1) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_feeling_recognized_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 70,
    title = "Feeling recognized for Open Science - EUR",
    title_size = 22
  )

lollipop_cluster7_question1

# save to file
ggsave(
  filename = "figure_feeling_recognized_EUR.png",
  plot = lollipop_cluster7_question1,
  device = "png",
  path = here("img", "recognition_rewards", "feeling_recognized"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_recognition_rewards$School)) {
  
  temp_figure_school <- 
    EUR_OS_recognition_rewards %>% 
    filter(
      question == question1 &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!Likert_feeling_recognized_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 60,
      title = paste0("Feeling recognized for Open Science - ", i),
      title_size = 22
    )
    
  # save to file
  ggsave(
    filename = paste0("figure_feeling_recognized_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "recognition_rewards", "feeling_recognized"),
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
lollipop_cluster7_question2 <-
  EUR_OS_recognition_rewards %>% 
  filter(question == question2) %>% 
  mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>%
  mutate(response = fct_recode(response, !!!Likert_current_recognition_convert)) %>%
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
    title = "Current recognition for Open Science - EUR",
    title_size = 22
  )

lollipop_cluster7_question2

# save to file
ggsave(
  filename = "figure_current_recognition_rewards_EUR.png",
  plot = lollipop_cluster7_question2,
  device = "png",
  path = here("img", "recognition_rewards", "current_recognition"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_recognition_rewards$School)) {
  
  temp_figure_school <- 
    EUR_OS_recognition_rewards %>% 
    filter(
      question == question2 &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>%
    mutate(response = fct_recode(response, !!!Likert_current_recognition_convert)) %>%
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
      y_max_limit = 90,
      title = paste0("Current recognition for Open Science - ", i),
      title_size = 22
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_current_recognition_rewards_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "recognition_rewards", "current_recognition"),
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
lollipop_cluster7_question3 <-
  EUR_OS_recognition_rewards %>% 
  filter(question == question3) %>% 
  mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>%
  mutate(response = fct_recode(response, !!!Likert_expected_recognition_convert)) %>%
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
    title = "Expected recognition for Open Science - EUR",
    title_size = 22
  )

lollipop_cluster7_question3

# save to file
ggsave(
  filename = "figure_expected_recognition_rewards_EUR.png",
  plot = lollipop_cluster7_question3,
  device = "png",
  path = here("img", "recognition_rewards", "expected_recognition"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_recognition_rewards$School)) {
  
  temp_figure_school <- 
    EUR_OS_recognition_rewards %>% 
    filter(
      question == question3 &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>%
    mutate(response = fct_recode(response, !!!Likert_expected_recognition_convert)) %>%
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
      title = paste0("Expected recognition for Open Science - ", i),
      title_size = 20
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_expected_recognition_rewards_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "recognition_rewards", "expected_recognition"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
