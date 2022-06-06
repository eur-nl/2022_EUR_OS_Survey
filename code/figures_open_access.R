
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

Likert_experience_convert <- c(
  "1" = "I have extensive experience with Open Access publishing",
  "2" = "I have some experience with Open Access publishing",
  "3" = "I am aware of Open Access publishing but have not done it",
  "4" = "Until now, I was unaware of Open Access publishing",
  "5" = "I don’t know/prefer not to answer"
)

Likert_concerns_convert <- c(
  "1" = "Green OA (self-archiving): Some journals might not publish findings that are uploaded to a pre-publication archive",
  "2" = "Green OA (self-archiving): Other people might copy my research and publish it before I do",
  "3" = "Green OA (self-archiving): Non-peer-reviewed findings might add noise to the literature",
  "4" = "Green OA (self-archiving): Making my work available pre-publication might reduce the number of citations to the ultimately published work",
  "5" = "Green OA (self-archiving): Availability of the pre-publication manuscript might highlight differences (e.g., errors in analysis, revisions to hypotheses) between the original conception of the research and the ultimately published work",
  "6" = "Gold OA: Open Access journals might have lower quality articles",
  "7" = "Gold OA: Open Access journals might not provide rigorous peer-review",
  "8" = "Gold OA: I might not have enough funding to pay Article Processing Charges for Open Access journals",
  "9" = "Gold OA: The impact of an Open Access publication might be low (e.g., few downloads and citations, low public engagement)",
  "10" = "I do not share any of these concerns",
  "11" = "I don’t know/prefer not to answer"
)


# TO DO: REORDER FACTOR LEVELS FOR QUESTION 2  ----------------------------------------------------------------


# Data ----------------------------------------------------------------

EUR_OS_open_access <-
  readRDS(here("data", "preprocessed", "rds", "cluster_1.rds")) 

# "In your opinion, how important is Open Access for your work?"
EUR_OS_open_access_Q1 <- 
  EUR_OS_open_access %>% 
  filter(question == "In your opinion, how important is Open Access for your work?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  mutate(response = str_wrap(response, width = 40))

# "What is your experience with Open Access?"
EUR_OS_open_access_Q2 <- 
  EUR_OS_open_access %>% 
  filter(question == "What is your experience with Open Access?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, Likert_experience_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  mutate(response = str_wrap(response, width = 40))

# "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?"
EUR_OS_open_access_Q3 <- 
  EUR_OS_open_access %>% 
  filter(question == "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, Likert_concerns_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  mutate(response = str_wrap(response, width = 40))

# "Is there anything you want to share with us regarding your experiences with Open Access?"
# FREE TEXT
EUR_OS_open_access_Q3 <-
  EUR_OS_open_access %>% 
  filter(question == "Is there anything you want to share with us regarding your experiences with Open Access?") %>% 
  count(question, response) %>%
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  mutate(response = str_wrap(response, width = 40))

# Question 1, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster1_question1 <-
  EUR_OS_open_access_Q1 %>%
  ggplot(aes(x = fct_rev(response), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 40)
  ) +
  labs(
    title = "Importance of Open Access - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster1_question1

# save to file
ggsave(
  filename = "figure_importance_open_access_EUR.png",
  plot = lollipop_cluster1_question1,
  device = "png",
  path = here("img"),
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
      question == "In your opinion, how important is Open Access for your work?" &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, Likert_importance_convert),
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
    labs(
      title = paste0("Importance of Open Access - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_importance_open_access_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

# EUR
lollipop_cluster1_question2 <-
  EUR_OS_open_access_Q2 %>%
  ggplot(aes(x = fct_rev(response), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    limits = c(0, 50)
  ) +
  labs(
    title = "Experience with Open Access - EUR",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster1_question2

# save to file
ggsave(
  filename = "figure_experience_open_access_EUR.png",
  plot = lollipop_cluster1_question2,
  device = "png",
  path = here("img"),
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
      question == "What is your experience with Open Access?" &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, Likert_experience_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>%  
    mutate(response = str_wrap(response, width = 40)) %>% 
    ggplot(aes(x = fct_rev(response), y = perc)) +
    geom_point(size = 6, color = "#0C8066") +
    geom_segment(aes(x = response, xend = response, y = 0, yend = perc), color = "#012328") +
    geom_label_repel(aes(response, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
    scale_y_continuous(
      breaks = seq(0, 80, 10),
      limits = c(0, 80)
    ) +
    labs(
      title = paste0("Experience with Open Access - ", i),
      x = ""
    ) +
    coord_flip() +
    theme_custom
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_open_access_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------









# END ----------------------------------------------------------------
