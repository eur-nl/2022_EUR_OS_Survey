
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

source(here("code", "functions", "theme_custom.R")) # custom ggplot2 theme

Likert_importance_convert <- c(
  "1" = "Extremely important",
  "2" = "Very important",
  "3" = "Moderately important",
  "4" = "Slightly important",
  "5" = "Not at all important",
  "6" = "I don’t know/prefer not to answer"
)

Likert_experience_others_convert <- c(
  "1" = "I regularly use open educational resources developed by others",
  "2" = "I have some experience with open educational resources developed by others, but do not use them regularly",
  "3" = "I am aware of open educational resources developed by others, but have not used them",
  "4" = "Until now, I hadn't heard of open educational resources",
  "5" = "I don’t know/prefer not to answer"
)

Likert_experience_own_convert <- c(
  "1" = "I regularly share open educational resources",
  "2" = "I have some experience with open educational resources, but do not share mine regularly",
  "3" = "I am aware of open educational resources, but have not shared my own",
  "4" = "Until now, I hadn't heard of open educational resources",
  "5" = "I don’t know/prefer not to answer"
)

Likert_concerns_convert <- c(
  "Other" = "Other_I don't know where to share educational resources I have created",
  "Other" = "Other_my data is often highly sensitive and hard to anonymise. sharing would violate ethical principles"
)

# Data ----------------------------------------------------------------

EUR_OS_open_education <-
  readRDS(here("data", "preprocessed", "rds", "cluster_4.rds")) 

# "In your opinion, how important are open educational resources for your work?"
EUR_OS_open_education_Q1 <- 
  EUR_OS_open_education %>% 
  filter(question == "In your opinion, how important are open educational resources for your work?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# "What is your experience with using open educational resources developed by others?"
EUR_OS_open_education_Q2 <- 
  EUR_OS_open_education %>% 
  filter(question == "What is your experience with using open educational resources developed by others?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_experience_others_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "What is your experience with openly sharing educational resources that you developed?"
EUR_OS_open_education_Q3 <- 
  EUR_OS_open_education %>% 
  filter(question == "What is your experience with openly sharing educational resources that you developed?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_experience_own_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with?"
EUR_OS_open_education_Q4 <- 
  EUR_OS_open_education %>% 
  filter(question == "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with?") %>% 
  mutate(response = fct_recode(response, !!!Likert_concerns_convert)) %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# Question 1, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster4_question1 <-
  EUR_OS_open_education_Q1 %>%
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
    title = "Importance of Open Education - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster4_question1

# save to file
ggsave(
  filename = "figure_importance_open_education_EUR.png",
  plot = lollipop_cluster4_question1,
  device = "png",
  path = here("img", "open_education", "importance"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_education$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_education %>% 
    filter(
      question == "In your opinion, how important are open educational resources for your work?" &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!Likert_importance_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 50, 10),
      limits = c(0, 50)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Importance of Open Education - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
 
  # save to file
  ggsave(
    filename = paste0("figure_importance_open_education_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_education", "importance"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster4_question2 <-
  EUR_OS_open_education_Q2 %>%
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
    title = "Experience with others' Open Education - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom+
  theme(plot.title = element_text(size = 18, hjust = .5))

lollipop_cluster4_question2

# save to file
ggsave(
  filename = "figure_experience_others_open_education_EUR.png",
  plot = lollipop_cluster4_question2,
  device = "png",
  path = here("img", "open_education", "experience_others"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_education$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_education %>% 
    filter(
      question == "What is your experience with using open educational resources developed by others?" &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!Likert_experience_others_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 50, 10),
      limits = c(0, 50)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Experience with others' Open Education - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 18, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_others_open_education_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_education", "experience_others"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster4_question3 <-
  EUR_OS_open_education_Q3 %>%
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
    title = "Experience with own Open Education - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom +
  theme(plot.title = element_text(size = 18, hjust = .5))

lollipop_cluster4_question3

# save to file
ggsave(
  filename = "figure_experience_own_open_education_EUR.png",
  plot = lollipop_cluster4_question3,
  device = "png",
  path = here("img", "open_education", "experience_own"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_education$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_education %>% 
    filter(
      question == "What is your experience with openly sharing educational resources that you developed?" &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!Likert_experience_own_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    drop_na() %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 50, 10),
      limits = c(0, 50)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Experience with own Open Education - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 18, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_own_open_education_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_education", "experience_own"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 4, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster4_question4 <-
  EUR_OS_open_education_Q4 %>%
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
    title = "Concerns around Open Education - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom +
  theme(plot.title = element_text(size = 18, hjust = .5))

lollipop_cluster4_question4

# save to file
ggsave(
  filename = "figure_concerns_open_education_EUR.png",
  plot = lollipop_cluster4_question4,
  device = "png",
  path = here("img", "open_education", "concerns"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_education$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_education %>% 
    filter(
      question == "The following are possible concerns that researchers could have about making educational resources developed by them openly available. Which of these concerns would you agree with?" &
        School == i
    ) %>% 
    mutate(response = fct_recode(response, !!!Likert_concerns_convert)) %>% 
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
      breaks = seq(0, 35, 10),
      limits = c(0, 35)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Concerns around Open Education - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 18, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_concerns_open_education_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_education", "concerns"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
