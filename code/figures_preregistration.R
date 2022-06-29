
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

EUR_OS_preregistration <-
  readRDS(here("data", "preprocessed", "rds", "cluster_3.rds")) 

# "In your opinion, how important is preregistration for your work?"
EUR_OS_preregistration_Q1 <- 
  EUR_OS_preregistration %>% 
  filter(question == "In your opinion, how important is preregistration for your work?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!preregistration_Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# "What is your experience with study preregistration?"
EUR_OS_preregistration_Q2 <- 
  EUR_OS_preregistration %>% 
  filter(question == "What is your experience with study preregistration?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!preregistration_Likert_experience_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with?"
EUR_OS_preregistration_Q3 <- 
  EUR_OS_preregistration %>% 
  filter(question == "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with?") %>% 
  mutate(response = fct_recode(response, !!!preregistration_Likert_concerns_convert)) %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# Question 1, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster3_question1 <-
  EUR_OS_preregistration_Q1 %>%
  ggplot(aes(x = fct_rev(response), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 30, 10),
    limits = c(0, 30)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  labs(
    title = "Importance of Preregistration - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster3_question1

# save to file
ggsave(
  filename = "figure_importance_preregistration_EUR.png",
  plot = lollipop_cluster3_question1,
  device = "png",
  path = here("img", "preregistration", "importance"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_preregistration$School)) {
  
  temp_figure_school <- 
    EUR_OS_preregistration %>% 
    filter(
      question == "In your opinion, how important is preregistration for your work?" &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!preregistration_Likert_importance_convert),
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
      title = paste0("Importance of Preregistration - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_importance_preregistration_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "preregistration", "importance"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster3_question2 <-
  EUR_OS_preregistration_Q2 %>%
  ggplot(aes(x = fct_rev(response), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 40)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  labs(
    title = "Experience with Preregistration - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster3_question2

# save to file
ggsave(
  filename = "figure_experience_preregistration_EUR.png",
  plot = lollipop_cluster3_question2,
  device = "png",
  path = here("img", "preregistration", "experience"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_preregistration$School)) {
  
  temp_figure_school <- 
    EUR_OS_preregistration %>% 
    filter(
      question == "What is your experience with study preregistration?" &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!preregistration_Likert_experience_convert),
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
      title = paste0("Experience with Preregistration - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_preregistration_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "preregistration", "experience"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster3_question3 <-
  EUR_OS_preregistration_Q3 %>%
  ggplot(aes(x = reorder(response, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 25, 10),
    limits = c(0, 25)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  labs(
    title = "Concerns around Preregistration - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster3_question3

# save to file
ggsave(
  filename = "figure_concerns_preregistration_EUR.png",
  plot = lollipop_cluster3_question3,
  device = "png",
  path = here("img", "preregistration", "concerns"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_preregistration$School)) {
  
  temp_figure_school <- 
    EUR_OS_preregistration %>% 
    filter(
      question == "The following are possible concerns that researchers could have about preregistering their studies. Which of these concerns would you agree with?" &
        School == i
    ) %>% 
    mutate(response = fct_recode(response, !!!preregistration_Likert_concerns_convert)) %>% 
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
      breaks = seq(0, 50, 10),
      limits = c(0, 50)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Concerns around Preregistration - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_concerns_preregistration_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "preregistration", "concerns"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
