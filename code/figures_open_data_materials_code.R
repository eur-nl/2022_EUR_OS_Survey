
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

EUR_OS_open_data_materials_code <-
  readRDS(here("data", "preprocessed", "rds", "cluster_2.rds")) 

questions <- unique(EUR_OS_open_data_materials_code$question)

# Question 1, lollipop graph ----------------------------------------------------------------

question1 <- questions[2]

# EUR
lollipop_cluster2_question1 <-
  EUR_OS_open_data_materials_code %>%
  filter(question == question1) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_importance_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Importance of Open Data/Materials/Code - EUR",
    title_size = 20
  )

lollipop_cluster2_question1

# save to file
ggsave(
  filename = "figure_importance_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question1,
  device = "png",
  path = here("img", "open_data_materials_code", "importance"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == question1 &
      School == i
      ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_importance_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Importance of Open Data/Materials/Code - ", i),
      title_size = 20
    )
  
  # save to file
  ggsave(
    filename = paste0("figure_importance_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "importance"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 2, lollipop graph ----------------------------------------------------------------

question2 <- questions[5]

# EUR
lollipop_cluster2_question2 <-
  EUR_OS_open_data_materials_code %>%
  filter(question == question2) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_data_materials_code_Likert_experience_others_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Experience with others' Open Data/Materials/Code - EUR",
    title_size = 16
  )
  
lollipop_cluster2_question2

# save to file
ggsave(
  filename = "figure_experience_others_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question2,
  device = "png",
  path = here("img", "open_data_materials_code", "experience_others"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == question2 &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_experience_others_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Experience with others' Open Data/Materials/Code - ", i),
      title_size = 16
    )

  # save to file
  ggsave(
    filename = paste0("figure_experience_others_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "experience_others"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 3, lollipop graph ----------------------------------------------------------------

question3 <- questions[4]

# EUR
lollipop_cluster2_question3 <-
  EUR_OS_open_data_materials_code %>%
  filter(question == question3) %>%
  mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>% 
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_data_materials_code_Likert_experience_own_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Experience with own Open Data/Materials/Code - EUR",
    title_size = 16
  )

lollipop_cluster2_question3

# save to file
ggsave(
  filename = "figure_experience_own_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question3,
  device = "png",
  path = here("img", "open_data_materials_code", "experience_own"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == question3 &
        School == i
    ) %>% 
    mutate(response = replace_na(response, "I don’t know/prefer not to answer")) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_experience_own_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>%
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 50,
      title = paste0("Experience with own Open Data/Materials/Code - ", i),
      title_size = 16
    )

  # save to file
  ggsave(
    filename = paste0("figure_experience_own_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "experience_own"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 4, lollipop graph ----------------------------------------------------------------

question4 <- questions[1]

# EUR
lollipop_cluster2_question4 <-
  EUR_OS_open_data_materials_code %>%
  filter(question == question4) %>%
  count(question, response) %>%
  mutate(
    response = fct_relevel(response, !!!open_data_materials_code_Likert_FAIR_convert),
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>% 
  lolliplot(
    data = ., 
    x = fct_rev(response), 
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "FAIR principles - EUR",
    title_size = 22
  )

lollipop_cluster2_question4

# save to file
ggsave(
  filename = "figure_FAIR_EUR.png",
  plot = lollipop_cluster2_question4,
  device = "png",
  path = here("img", "open_data_materials_code", "FAIR"),
  scale = 3,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == question4 &
        School == i
    ) %>% 
    count(question, response) %>%
    mutate(
      response = fct_relevel(response, !!!open_data_materials_code_Likert_FAIR_convert),
      perc = round(n / sum(n) * 100, 2),
      lab_perc = paste(perc, "%", sep = "")
    ) %>% 
    lolliplot(
      data = ., 
      x = fct_rev(response), 
      y = perc, 
      labels = lab_perc, 
      y_max_limit = 70,
      title = paste0("FAIR principles - ", i),
      title_size = 22
    )
    
  # save to file
  ggsave(
    filename = paste0("figure_FAIR_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "FAIR"),
    scale = 3,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# Question 5, lollipop graph ----------------------------------------------------------------

question5 <- questions[3]

# EUR
lollipop_cluster2_question5 <-
  EUR_OS_open_data_materials_code %>%
  filter(question == question5) %>%
  mutate(response = fct_recode(response, !!!open_data_materials_code_Likert_concerns_convert)) %>%
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
    title = "Concerns around Open Data/Materials/Code - EUR",
    title_size = 22
  )

lollipop_cluster2_question5

# save to file
ggsave(
  filename = "figure_concerns_open_data_materials_code_EUR.png",
  plot = lollipop_cluster2_question5,
  device = "png",
  path = here("img", "open_data_materials_code", "concerns"),
  scale = 6,
  width = 8,
  height = 4,
  units = "cm",
  dpi = 600
)

# separate graph for each school
# NOTE: "Other" will not be reported
for(i in levels(EUR_OS_open_data_materials_code$School)) {
  
  temp_figure_school <- 
    EUR_OS_open_data_materials_code %>% 
    filter(
      question == question5 &
        School == i
    ) %>% 
    mutate(response = fct_recode(response, !!!open_data_materials_code_Likert_concerns_convert)) %>% 
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
      y_max_limit = 50,
      title = paste0("Concerns around Open Data/Materials/Code - ", i),
      title_size = 22
    )

  # save to file
  ggsave(
    filename = paste0("figure_concerns_open_data_materials_code_", i, ".png"),
    plot = temp_figure_school,
    device = "png",
    path = here("img", "open_data_materials_code", "concerns"),
    scale = 6,
    width = 8,
    height = 4,
    units = "cm",
    dpi = 600
  )
  
}

# END ----------------------------------------------------------------
