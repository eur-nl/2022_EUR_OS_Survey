
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

# Data ----------------------------------------------------------------

cluster <-
  read_csv(
    here("data", "preproc", "cluster_0.csv"),
    show_col_types = FALSE
  ) %>% 
  mutate(
    question = as_factor(question),
    item = as_factor(item)
  )

# extract questions
questions <- levels(cluster$question)

# Question 1, lollipop graph ----------------------------------------------------------------

num_question <- 1

data_cluster0_question1 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels() %>%
  mutate(
    item = recode_factor( # recode item values too long to be displayed on axis
      item,
      "Erasmus School of Health Policy & Management (ESHPM)" = "ESHPM",
      "Erasmus School of History, Culture and Communication (ESHCC)" = "ESHCC",
      "Erasmus School of Law (ESL)" = "ESL",
      "Erasmus School of Philosophy (ESPHIL)" = "ESPhil",
      "Erasmus School of Social and Behavioural Sciences (ESSB)" = "ESSB",
      "International Institute of Social Studies (ISS)" = "ISS"
    )
  )

lollipop_cluster0_question1 <-
  data_cluster0_question1 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(item, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 40)
  ) +
  labs(
    title = "School",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster0_question1

# Question 2, lollipop graph ----------------------------------------------------------------

num_question <- 2

data_cluster0_question2 <-
  cluster %>%
  filter(question == questions[num_question]) %>%
  droplevels()

lollipop_cluster0_question2 <-
  data_cluster0_question2 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(item, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 50, 10),
    limits = c(0, 50)
  ) +
  labs(
    title = "Position",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster0_question2

# Merge in one figure ----------------------------------------------------------------

lollipop_figure1 <-
  lollipop_cluster0_question1 / lollipop_cluster0_question2 +
  plot_annotation(
    title = "Respondent Characteristics"
  ) &
  theme(plot.title = element_text(size = 26, hjust = .5))

lollipop_figure1

# save to file
ggsave(
  filename = "figure1.png",
  plot = lollipop_figure1,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# END ----------------------------------------------------------------
