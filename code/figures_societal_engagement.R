
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

EUR_OS_societal_engagement <-
  readRDS(here("data", "preprocessed", "rds", "cluster_5.rds")) 

questions <- unique(EUR_OS_societal_engagement$question)

# Question 1, lollipop graph ----------------------------------------------------------------

question1 <- questions[1]

# EUR
lollipop_cluster5_question1 <-
  EUR_OS_societal_engagement %>% 
  filter(question == question1) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!societal_engagement_Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Importance of Societal Engagement - EUR",
    title_size = 18
  )
  
lollipop_cluster5_question1

# save to file
ggsave(
  filename = "figure_importance_societal_engagement_EUR.png",
  plot = lollipop_cluster5_question1,
  device = "png",
  path = here("img", "societal_engagement", "importance"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_societal_engagement$School)) {
  
  temp_figure_school <- 
    EUR_OS_societal_engagement %>% 
    filter(
      question == question1 &
        School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!societal_engagement_Likert_importance_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Importance of Societal Engagement - ", i),
      title_size = 18
    )
    
  # save to file
  ggsave(
    filename = paste0("figure_importance_societal_engagement_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "societal_engagement", "importance"),
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
lollipop_cluster5_question2 <-
  EUR_OS_societal_engagement %>% 
  filter(question == question2) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!societal_engagement_Likert_experience_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Experience with Societal Engagement - EUR",
    title_size = 18
  )

lollipop_cluster5_question2

# save to file
ggsave(
  filename = "figure_experience_societal_engagement_EUR.png",
  plot = lollipop_cluster5_question2,
  device = "png",
  path = here("img", "societal_engagement", "experience"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_societal_engagement$School)) {
  
  temp_figure_school <- 
    EUR_OS_societal_engagement %>% 
    filter(
      question == question2 &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!societal_engagement_Likert_experience_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>%  
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 60,
      title = paste0("Experience with Societal Engagement - ", i),
      title_size = 18
    )
    
  # save to file
  ggsave(
    filename = paste0("figure_experience_societal_engagement_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "societal_engagement", "experience"),
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
lollipop_cluster5_question3 <-
  EUR_OS_societal_engagement %>% 
  filter(question == question3) %>% 
  mutate(response = fct_recode(response, !!!societal_engagement_Likert_concerns_convert)) %>% 
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
    y_max_limit = 40,
    title = "Concerns around Societal Engagement - EUR",
    title_size = 22
  )
  
lollipop_cluster5_question3

# save to file
ggsave(
  filename = "figure_concerns_societal_engagement_EUR.png",
  plot = lollipop_cluster5_question3,
  device = "png",
  path = here("img", "societal_engagement", "concerns"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_societal_engagement$School)) {
  
  temp_figure_school <- 
    EUR_OS_societal_engagement %>% 
    filter(
      question == question3 &
        School == i
    ) %>% 
    mutate(response = fct_recode(response, !!!societal_engagement_Likert_concerns_convert)) %>% 
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
      title = paste0("Concerns around Societal Engagement - ", i),
      title_size = 20
    )
    
  # save to file
  ggsave(
    filename = paste0("figure_concerns_societal_engagement_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "societal_engagement", "concerns"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
