
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

EUR_OS_open_access <-
  readRDS(here("data", "preprocessed", "rds", "cluster_1.rds")) 

questions <- unique(EUR_OS_open_access$question)

# Question 1, lollipop graph ----------------------------------------------------------------

question1 <- questions[1]

# EUR
lollipop_cluster1_question1 <-
  EUR_OS_open_access %>% 
  filter(question == question1) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_access_Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Importance of Open Access - EUR",
    title_size = 22
  )

lollipop_cluster1_question1

# save to file
ggsave(
  filename = "figure_importance_open_access_EUR.png",
  plot = lollipop_cluster1_question1,
  device = "png",
  path = here("img", "open_access", "importance"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_access$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_access %>% 
    filter(
      question == question1 &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_access_Likert_importance_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response),  
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Importance of Open Access - ", i),
      title_size = 22
    )
    
  # save to file
  ggsave(
    filename = paste0("figure_importance_open_access_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_access", "importance"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

question2 <- questions[4]

# EUR
lollipop_cluster1_question2 <-
  EUR_OS_open_access %>% 
  filter(question == question2) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_access_Likert_experience_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 50,
    title = "Experience with Open Access - EUR",
    title_size = 22
  )

lollipop_cluster1_question2

# save to file
ggsave(
  filename = "figure_experience_open_access_EUR.png",
  plot = lollipop_cluster1_question2,
  device = "png",
  path = here("img", "open_access", "experience"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_access$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_access %>% 
    filter(
      question == question2 &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_access_Likert_experience_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>%  
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 80,
      title = paste0("Experience with Open Access - ", i),
      title_size = 22
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_open_access_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_access", "experience"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------

question3 <- questions[3]

# EUR
lollipop_cluster1_question3 <-
  EUR_OS_open_access %>% 
  filter(question == question3) %>% 
  count(question, response) %>%
  mutate(
    response = fct_recode(response, !!!open_access_Likert_concerns_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = reorder(response, perc), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Concerns around Open Access - EUR",
    title_size = 22
  )

lollipop_cluster1_question3

# save to file
ggsave(
  filename = "figure_concerns_open_access_EUR.png",
  plot = lollipop_cluster1_question3,
  device = "png",
  path = here("img", "open_access", "concerns"),
  scale = 6,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_access$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_access %>% 
    filter(
      question == question3 &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_recode(response, !!!open_access_Likert_concerns_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>%  
    lolliplot(
      data = ., 
      x = reorder(response, perc), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Concerns around Open Access - ", i),
      title_size = 22
    )

  # save to file
  ggsave(
    filename = paste0("figure_concerns_open_access_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_access", "concerns"),
    scale = 6,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
