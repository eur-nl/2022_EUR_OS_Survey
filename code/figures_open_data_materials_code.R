
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
  "1" = "I regularly use open data, materials, and/or code developed by others",
  "2" = "I have some experience with open data, materials, and/or code developed by others, but do not use them regularly",
  "3" = "I am aware of open data, materials, and/or code developed by others, but have not used them",
  "4" = "Until now, I hadn't heard of open data, materials, and/or code",
  "5" = "I don’t know/prefer not to answer"
)

Likert_experience_own_convert <- c(
  "1" = "I regularly share open data, materials, and/or code",
  "2" = "I have some experience with open data, materials, and/or code, but do not share mine regularly",
  "3" = "I am aware of open data, materials, and/or code, but have not shared my own",
  "4" = "Until now, I hadn't heard of open data, materials, and/or code",
  "5" = "I don’t know/prefer not to answer"
)

Likert_FAIR_convert <- c(
  "1" = "I regularly follow the FAIR principles",
  "2" = "I have some experience with the FAIR principles, but do not follow them regularly",
  "3" = "I am aware of the FAIR principles, but have not followed them",
  "4" = "Until now, I hadn't heard of the FAIR principles",
  "5" = "I don’t know/prefer not to answer"
)

Likert_concerns_convert <- c(
  "Other" = "Other_I do not 'own' the data and I'm not allowed to share the data",
  "Other" = "Other_I do not research in which this is relevant.",
  "Other" = "Other_Sharing data is not always a good idea when for example one works with vulnerable groups and informants might be at risk of stigma, prosecution, etc.",
  "Other" = "Other_The data I use comes from a third party which doesn't allow me to share the data."
)

# Data ----------------------------------------------------------------

EUR_OS_open_data_materials_code <-
  readRDS(here("data", "preprocessed", "rds", "cluster_2.rds")) 

# "In your opinion, how important are open data, materials, and/or code for your work?"
EUR_OS_open_data_materials_code_Q1 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "In your opinion, how important are open data, materials, and/or code for your work?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# "What is your experience with using open data, materials, and/or code developed by others?"
EUR_OS_open_data_materials_code_Q2 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "What is your experience with using open data, materials, and/or code developed by others?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_experience_others_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "What is your experience with openly sharing data, materials, and/or code that you developed?"
EUR_OS_open_data_materials_code_Q3 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "What is your experience with openly sharing data, materials, and/or code that you developed?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_experience_own_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  drop_na()

# "Are you familiar with the FAIR principles for data and code?"
EUR_OS_open_data_materials_code_Q4 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "Are you familiar with the FAIR principles for data and code?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_FAIR_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?"
EUR_OS_open_data_materials_code_Q5 <- 
  EUR_OS_open_data_materials_code %>% 
  filter(question == "The following are possible concerns that researchers could have about making data, materials, and/or code developed by them openly available. Which of these concerns would you agree with?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_recode(response, !!!Likert_concerns_convert),
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
      response = fct_relevel(response, !!!Likert_importance_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 80, 10),
      limits = c(0, 80)
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
    title = "Experience with Open Data/Materials/Code - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom +
  theme(plot.title = element_text(size = 18, hjust = .5))

lollipop_cluster2_question2

# save to file
ggsave(
  filename = "figure_experience_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question2,
  device = "png",
  path = here("img", "open_data_materials_code", "experience"),
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
      response = fct_relevel(response, !!!Likert_experience_others_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 80, 10),
      limits = c(0, 80)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Experience with Open Data/Materials/Code - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom +
    theme(plot.title = element_text(size = 18, hjust = .5))
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "experience"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}


# FROM HERE ----------------------------------------------------------------

















# Question 3, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster1_question3 <-
  EUR_OS_open_access_Q3 %>%
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
    title = "Concerns around Open Access - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster1_question3

# save to file
ggsave(
  filename = "figure_concerns_open_access_EUR.png",
  plot = lollipop_cluster1_question3,
  device = "png",
  path = here("img", "open_access", "concerns"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_access$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_access %>% 
    filter(
      question == "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?" &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!Likert_concerns_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>%  
    ggplot(aes(x = reorder(response, perc), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 80, 10),
      limits = c(0, 80)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Experience with Open Access - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_concerns_open_access_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_access", "concerns"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 4, free text ----------------------------------------------------------------

# this question does not have figures
# the extraction of the responses will be included 
# in the report, and representative responses quoted

# "Is there anything you want to share with us regarding your experiences with Open Access?"
# FREE TEXT
EUR_OS_open_access_Q4 <-
  EUR_OS_open_access %>% 
  filter(question == "Is there anything you want to share with us regarding your experiences with Open Access?") %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )



# END ----------------------------------------------------------------
