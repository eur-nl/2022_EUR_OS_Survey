
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

EUR_OS_support <-
  readRDS(here("data", "preprocessed", "rds", "cluster_6.rds")) 

# "Do you expect EUR to support you in learning open science practices?"
EUR_OS_support_Q1 <- 
  EUR_OS_support %>% 
  filter(question == "Do you expect EUR to support you in learning open science practices?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_EUR_support_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# "Which of the following open science practices would you like EUR to provide information or support for?"
EUR_OS_support_Q2 <- 
  EUR_OS_support %>% 
  filter(question == "Which of the following open science practices would you like EUR to provide information or support for?") %>% 
  mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "What support services provided at EUR have you used to make your data FAIR?"
EUR_OS_support_Q3 <- 
  EUR_OS_support %>% 
  filter(question == "What support services provided at EUR have you used to make your data FAIR?") %>% 
  mutate(response = replace_na(response, "I don't know/prefer not to answer")) %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# Question 1, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster6_question1 <-
  EUR_OS_support_Q1 %>%
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
    title = "Expected EUR support - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster6_question1

# save to file
ggsave(
  filename = "figure_EUR_support_EUR.png",
  plot = lollipop_cluster6_question1,
  device = "png",
  path = here("img", "support", "EUR"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_support$School)) {
  
  temp_figure_school <- 
    EUR_OS_support %>% 
    filter(
      question == "Do you expect EUR to support you in learning open science practices?" &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!Likert_EUR_support_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 90, 10),
      limits = c(0, 90)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Expected EUR support - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_EUR_support_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "support", "EUR"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster6_question2 <-
  EUR_OS_support_Q2 %>%
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
    title = "Desired Open Science support - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster6_question2

# save to file
ggsave(
  filename = "figure_desired_support_EUR.png",
  plot = lollipop_cluster6_question2,
  device = "png",
  path = here("img", "support", "desired"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_support$School)) {
  
  temp_figure_school <- 
    EUR_OS_support %>% 
    filter(
      question == "Which of the following open science practices would you like EUR to provide information or support for?" &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>% 
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
      title = paste0("Desired Open Science support - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_desired_support_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "support", "desired"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster6_question3 <-
  EUR_OS_support_Q3 %>%
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
    title = "FAIR support - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster6_question3

# save to file
ggsave(
  filename = "figure_FAIR_support_EUR.png",
  plot = lollipop_cluster6_question3,
  device = "png",
  path = here("img", "support", "FAIR"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_support$School)) {
  
  temp_figure_school <- 
    EUR_OS_support %>% 
    filter(
      question == "What support services provided at EUR have you used to make your data FAIR?" &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don't know/prefer not to answer")) %>% 
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
      breaks = seq(0, 100, 10),
      limits = c(0, 100)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("FAIR support - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_FAIR_support_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "support", "FAIR"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
