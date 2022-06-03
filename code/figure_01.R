
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
  readRDS(here("data", "preprocessed", "rds", "cluster_0.rds")) %>% 
  filter(question != "Department") %>% 
  droplevels()
  
# extract questions
questions <- unique(cluster$question)

# Question 1, lollipop graph ----------------------------------------------------------------

num_question <- 1

data_cluster0_question1 <-
  cluster %>%
  filter(
    Finished == TRUE & # only completed surveys
    question == questions[num_question]
    ) %>%
  droplevels() %>% 
  pivot_wider(
    id_cols = participant,
    names_from = question,
    values_from = item
  ) %>%
  count(School) %>% 
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

lollipop_cluster0_question1 <-
  data_cluster0_question1 %>%
  ggplot(aes(x = reorder(School, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = School, xend = School, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(School, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
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
  filter(
    Finished == TRUE & # only completed surveys
      question == questions[num_question]
  ) %>%
  droplevels() %>% 
  pivot_wider(
    id_cols = participant,
    names_from = question,
    values_from = item
  ) %>%
  count(Position) %>% 
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  )

lollipop_cluster0_question2 <-
  data_cluster0_question2 %>%
  ggplot(aes(x = reorder(Position, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = Position, xend = Position, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(Position, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 40, 10),
    limits = c(0, 40)
  ) +
  labs(
    title = "Position",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster0_question2

# Merge in one figure ----------------------------------------------------------------

lollipop_figure01 <-
  lollipop_cluster0_question1 / lollipop_cluster0_question2 +
  theme(plot.title = element_text(size = 26, hjust = .5))

lollipop_figure01

# save to file
ggsave(
  filename = "figure01.png",
  plot = lollipop_figure01,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# END ----------------------------------------------------------------
