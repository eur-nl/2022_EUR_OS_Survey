
# RNG ---------------------------------------------------------------------

seed_proj <- 100
set.seed(seed_proj)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("ggrepel")
# install.packages("patchwork")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(ggrepel)
library(patchwork)

source(here("code", "functions", "recoding.R")) # recoding scheme
source(here("code", "functions", "theme_custom.R")) # custom ggplot2 theme

# Data ----------------------------------------------------------------

EUR_OS_recognition_rewards <-
  readRDS(here("data", "preprocessed", "rds", "cluster_7.rds")) 

# "Do you feel recognized and rewarded by EUR (e.g. in the R&O cycle or appraisal conversation) for the Open Science activities you undertake?"
EUR_OS_recognition_rewards_Q1 <- 
  EUR_OS_recognition_rewards %>% 
  filter(question == "Do you feel recognized and rewarded by EUR (e.g. in the R&O cycle or appraisal conversation) for the Open Science activities you undertake?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_feeling_recognized_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "In what way were you recognized and rewarded?"
EUR_OS_recognition_rewards_Q2 <-
  EUR_OS_recognition_rewards %>% 
  filter(question == "In what way were you recognized and rewarded?") %>% 
  mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>%
  mutate(response = fct_recode(response, !!!Likert_current_recognition_convert)) %>%
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "In what way do you expect to be recognized and rewarded?"
EUR_OS_recognition_rewards_Q3 <- 
  EUR_OS_recognition_rewards %>% 
  filter(question == "In what way do you expect to be recognized and rewarded?") %>% 
  mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>%
  mutate(response = fct_recode(response, !!!Likert_expected_recognition_convert)) %>%
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# Question 1, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster7_question1 <-
  EUR_OS_recognition_rewards_Q1 %>%
  ggplot(aes(x = fct_rev(response), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 65, 10),
    limits = c(0, 65)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  labs(
    title = "Feeling recognized for Open Science - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster7_question1

# save to file
ggsave(
  filename = "figure_feeling_recognized_EUR.png",
  plot = lollipop_cluster7_question1,
  device = "png",
  path = here("img", "recognition_rewards", "feeling_recognized"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_recognition_rewards$School)) {
  
  temp_figure_school <- 
    EUR_OS_recognition_rewards %>% 
    filter(
      question == "Do you feel recognized and rewarded by EUR (e.g. in the R&O cycle or appraisal conversation) for the Open Science activities you undertake?" &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!Likert_feeling_recognized_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 60, 10),
      limits = c(0, 60)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Feeling recognized for Open Science - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_feeling_recognized_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "recognition_rewards", "feeling_recognized"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster7_question2 <-
  EUR_OS_recognition_rewards_Q2 %>%
  ggplot(aes(x = reorder(response, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 60, 10),
    limits = c(0, 60)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  labs(
    title = "Current recognition for Open Science - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster7_question2

# save to file
ggsave(
  filename = "figure_current_recognition_rewards_EUR.png",
  plot = lollipop_cluster7_question2,
  device = "png",
  path = here("img", "recognition_rewards", "current_recognition"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_recognition_rewards$School)) {
  
  temp_figure_school <- 
    EUR_OS_recognition_rewards %>% 
    filter(
      question == "In what way were you recognized and rewarded?" &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>%
    mutate(response = fct_recode(response, !!!Likert_current_recognition_convert)) %>%
    count(question, response) %>%
    mutate(
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    ggplot(aes(x = reorder(response, perc), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 90, 10),
      limits = c(0, 90)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Current recognition for Open Science - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_current_recognition_rewards_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "recognition_rewards", "current_recognition"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster7_question3 <-
  EUR_OS_recognition_rewards_Q3 %>%
  ggplot(aes(x = reorder(response, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    limits = c(0, 50)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  labs(
    title = "Expected recognition for Open Science - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster7_question3

# save to file
ggsave(
  filename = "figure_expected_recognition_rewards_EUR.png",
  plot = lollipop_cluster7_question3,
  device = "png",
  path = here("img", "recognition_rewards", "expected_recognition"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_recognition_rewards$School)) {
  
  temp_figure_school <- 
    EUR_OS_recognition_rewards %>% 
    filter(
      question == "In what way do you expect to be recognized and rewarded?" &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>%
    mutate(response = fct_recode(response, !!!Likert_expected_recognition_convert)) %>%
    count(question, response) %>%
    mutate(
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    ggplot(aes(x = reorder(response, perc), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 70, 10),
      limits = c(0, 70)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Expected recognition for Open Science - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 20, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_expected_recognition_rewards_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "recognition_rewards", "expected_recognition"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
