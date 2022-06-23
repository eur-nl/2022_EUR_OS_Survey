
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
  "Some journals might not publish findings that are uploaded to a pre-publication archive" = "Green OA (self-archiving): Some journals might not publish findings that are uploaded to a pre-publication archive",
  "Other people might copy my research and publish it before I do" = "Green OA (self-archiving): Other people might copy my research and publish it before I do",
  "Non-peer-reviewed findings might add noise to the literature" = "Green OA (self-archiving): Non-peer-reviewed findings might add noise to the literature",
  "Making my work available pre-publication might reduce the number of citations to the ultimately published work" = "Green OA (self-archiving): Making my work available pre-publication might reduce the number of citations to the ultimately published work",
  "Availability of the pre-publication manuscript might highlight differences (e.g., errors in analysis, revisions to hypotheses) between the original conception of the research and the ultimately published work" = "Green OA (self-archiving): Availability of the pre-publication manuscript might highlight differences (e.g., errors in analysis, revisions to hypotheses) between the original conception of the research and the ultimately published work",
  "Open Access journals might have lower quality articles" = "Gold OA: Open Access journals might have lower quality articles",
  "Open Access journals might not provide rigorous peer-review" = "Gold OA: Open Access journals might not provide rigorous peer-review",
  "I might not have enough funding to pay Article Processing Charges for Open Access journals" = "Gold OA: I might not have enough funding to pay Article Processing Charges for Open Access journals",
  "The impact of an Open Access publication might be low (e.g., few downloads and citations, low public engagement)" = "Gold OA: The impact of an Open Access publication might be low (e.g., few downloads and citations, low public engagement)",
  "I do not share any of these concerns" = "I do not share any of these concerns",
  "I don’t know/prefer not to answer" = "I don’t know/prefer not to answer",
  "Other" = "Other_For me the most important issue is where to find publications. And if this place/website is clear that publications can be \"googeld\" easily."
)

# Data ----------------------------------------------------------------

EUR_OS_open_access <-
  readRDS(here("data", "preprocessed", "rds", "cluster_1.rds")) 

# "In your opinion, how important is Open Access for your work?"
EUR_OS_open_access_Q1 <- 
  EUR_OS_open_access %>% 
  filter(question == "In your opinion, how important is Open Access for your work?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

# "What is your experience with Open Access?"
EUR_OS_open_access_Q2 <- 
  EUR_OS_open_access %>% 
  filter(question == "What is your experience with Open Access?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!Likert_experience_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

# "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?"
EUR_OS_open_access_Q3 <- 
  EUR_OS_open_access %>% 
  filter(question == "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?") %>% 
  count(question, response) %>%
  mutate(
    response = fct_recode(response, !!!Likert_concerns_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) 

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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
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
  path = here("img", "open_access", "importance"),
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
    path = here("img", "open_access", "importance"),
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
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
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
  path = here("img", "open_access", "experience"),
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
      response = fct_relevel(response, !!!Likert_experience_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>%  
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
    path = here("img", "open_access", "experience"),
    scale = 3,
    width = 8,
    height = 8,
    units = "cm",
    dpi = 600
  )
  
}

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
      breaks = seq(0, 50, 10),
      limits = c(0, 50)
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
    labs(
      title = paste0("Concerns around Open Access - ", i),
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
