
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

EUR_OS_open_data_materials_code <-
  readRDS(here("data", "preprocessed", "rds", "cluster_2.rds")) 

# "In your opinion, how important are open data, materials, and/or code for your work?"
EUR_OS_open_data_materials_code_Q1 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "In your opinion, how important are open data, materials, and/or code for your work?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_data_materials_code_Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# "What is your experience with using open data, materials, and/or code developed by others?"
EUR_OS_open_data_materials_code_Q2 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "What is your experience with using open data, materials, and/or code developed by others?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_data_materials_code_Likert_experience_others_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "What is your experience with openly sharing data, materials, and/or code that you developed?"
EUR_OS_open_data_materials_code_Q3 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "What is your experience with openly sharing data, materials, and/or code that you developed?") %>% 
  mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_data_materials_code_Likert_experience_own_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "Are you familiar with the FAIR principles for data and code?"
EUR_OS_open_data_materials_code_Q4 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "Are you familiar with the FAIR principles for data and code?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_data_materials_code_Likert_FAIR_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?"
EUR_OS_open_data_materials_code_Q5 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?") %>% 
  mutate(response = fct_recode(response, !!!open_data_materials_code_Likert_concerns_convert)) %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# Question 1, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster2_question1 <-
  EUR_OS_open_data_materials_code_Q1 %>%
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
    title = "Importance of Open Data/Materials/Code - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom +
  theme(plot.title = element_text(size = 20, hjust = .5))

lollipop_cluster2_question1

# save to file
ggsave(
  filename = "figure_importance_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question1,
  device = "png",
  path = here("img", "open_data_materials_code", "importance"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == "In your opinion, how important are open data, materials, and/or code for your work?" &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_importance_convert),
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
      title = paste0("Importance of Open Data/Materials/Code - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 20, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_importance_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "importance"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster2_question2 <-
  EUR_OS_open_data_materials_code_Q2 %>%
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
    title = "Experience with others' Open Data/Materials/Code - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom +
  theme(plot.title = element_text(size = 16, hjust = .5))

lollipop_cluster2_question2

# save to file
ggsave(
  filename = "figure_experience_others_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question2,
  device = "png",
  path = here("img", "open_data_materials_code", "experience_others"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == "What is your experience with using open data, materials, and/or code developed by others?" &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_experience_others_convert),
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
      title = paste0("Experience with others' Open Data/Materials/Code - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 16, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_others_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "experience_others"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster2_question3 <-
  EUR_OS_open_data_materials_code_Q3 %>%
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
    title = "Experience with own Open Data/Materials/Code - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom +
  theme(plot.title = element_text(size = 16, hjust = .5))

lollipop_cluster2_question3

# save to file
ggsave(
  filename = "figure_experience_own_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question3,
  device = "png",
  path = here("img", "open_data_materials_code", "experience_own"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == "What is your experience with openly sharing data, materials, and/or code that you developed?" &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_experience_own_convert),
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
      title = paste0("Experience with own Open Data/Materials/Code - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 16, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_own_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "experience_own"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 4, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster2_question4 <-
  EUR_OS_open_data_materials_code_Q4 %>%
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
    title = "FAIR principles - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster2_question4

# save to file
ggsave(
  filename = "figure_FAIR_EUR.png",
  plot = lollipop_cluster2_question4,
  device = "png",
  path = here("img", "open_data_materials_code", "FAIR"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == "Are you familiar with the FAIR principles for data and code?" &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_FAIR_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    drop_na() %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 70, 10),
      limits = c(0, 70)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("FAIR principles - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_FAIR_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "FAIR"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 5, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster2_question5 <-
  EUR_OS_open_data_materials_code_Q5 %>%
  ggplot(aes(x = reorder(response, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 40)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  labs(
    title = "Concerns around Open Data/Materials/Code - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom +
  theme(plot.title = element_text(size = 16, hjust = .5))

lollipop_cluster2_question5

# save to file
ggsave(
  filename = "figure_concerns_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question5,
  device = "png",
  path = here("img", "open_data_materials_code", "concerns"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?" &
        School == i
    ) %>% 
    mutate(response = fct_recode(response, !!!open_data_materials_code_Likert_concerns_convert)) %>% 
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
      title = paste0("Concerns around Open Data/Materials/Code - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 16, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_concerns_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "concerns"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
