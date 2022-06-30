
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
library(patchwork)

source(here("code", "functions", "recoding.R")) # recoding scheme
source(here("code", "functions", "lolliplot.R")) # custom ggplot2 plot

# Data ----------------------------------------------------------------

EUR_OS_open_education <-
  readRDS(here("data", "preprocessed", "rds", "cluster_4.rds")) 

questions <- unique(EUR_OS_open_education$question)

# Question 1, lollipop graph ----------------------------------------------------------------

question1 <- questions[1]

# EUR
lollipop_cluster4_question1 <-
  EUR_OS_open_education %>%
  filter(question == question1) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_education_Likert_importance_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 30,
    title = "Importance of Open Education - EUR",
    title_size = 22
  )

lollipop_cluster4_question1

# save to file
ggsave(
  filename = "figure_importance_open_education_EUR.png",
  plot = lollipop_cluster4_question1,
  device = "png",
  path = here("img", "open_education", "importance"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_education$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_education %>% 
    filter(
      question == question1 &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_education_Likert_importance_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Importance of Open Education - ", i),
      title_size = 22
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_importance_open_education_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_education", "importance"),
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
lollipop_cluster4_question2 <-
  EUR_OS_open_education %>%
  filter(question == question2) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_education_Likert_experience_others_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Experience with others' Open Education - EUR",
    title_size = 18
  )
  
lollipop_cluster4_question2

# save to file
ggsave(
  filename = "figure_experience_others_open_education_EUR.png",
  plot = lollipop_cluster4_question2,
  device = "png",
  path = here("img", "open_education", "experience_others"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_education$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_education %>% 
    filter(
      question == question2 &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_education_Likert_experience_others_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Experience with others' Open Education - ", i),
      title_size = 18
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_others_open_education_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_education", "experience_others"),
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
lollipop_cluster4_question3 <-
  EUR_OS_open_education %>%
  filter(question == question3) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_education_Likert_experience_own_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Experience with own Open Education - EUR",
    title_size = 18
  )

lollipop_cluster4_question3

# save to file
ggsave(
  filename = "figure_experience_own_open_education_EUR.png",
  plot = lollipop_cluster4_question3,
  device = "png",
  path = here("img", "open_education", "experience_own"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_education$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_education %>% 
    filter(
      question == question3 &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_education_Likert_experience_own_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>%
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Experience with own Open Education - ", i),
      title_size = 18
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_experience_own_open_education_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_education", "experience_own"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 4, lollipop graph ----------------------------------------------------------------

question4 <- questions[2]

# EUR
lollipop_cluster4_question4 <-
  EUR_OS_open_education %>%
  filter(question == question4) %>% 
  mutate(response = fct_recode(response, !!!open_education_Likert_concerns_convert)) %>% 
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
    y_max_limit = 20,
    title = "Concerns around Preregistration - EUR",
    title_size = 22
  )

lollipop_cluster4_question4

# save to file
ggsave(
  filename = "figure_concerns_open_education_EUR.png",
  plot = lollipop_cluster4_question4,
  device = "png",
  path = here("img", "open_education", "concerns"),
  scale = 4,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_education$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_education %>% 
    filter(
      question == question4 &
        School == i
    ) %>% 
    mutate(response = fct_recode(response, !!!open_education_Likert_concerns_convert)) %>% 
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
      y_max_limit = 35,
      title = paste0("Concerns around Open Education - ", i),
      title_size = 22
    )

  # save to file
  ggsave(
    filename = paste0("figure_concerns_open_education_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_education", "concerns"),
    scale = 4,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
