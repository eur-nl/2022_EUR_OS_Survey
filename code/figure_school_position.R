
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

source(here("code", "functions", "lolliplot.R")) # custom ggplot2 plot

# Data ----------------------------------------------------------------

cluster <-
  readRDS(here("data", "preprocessed", "rds", "cluster_0.rds")) %>% 
  filter(Finished == TRUE) %>% # only completed surveys
  select(-c(Finished, Department)) 

# School, lollipop graph ----------------------------------------------------------------

lollipop_cluster0_question1 <-
  cluster %>%
  count(School) %>% 
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  lolliplot(
    data = ., 
    x = reorder(School, perc),
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "School",
    title_size = 22
  )

lollipop_cluster0_question1

# Position, lollipop graph ----------------------------------------------------------------

lollipop_cluster0_question2 <-
  cluster %>%
  count(Position) %>% 
  mutate(
    perc = round(n / sum(n) * 100, 2),
    lab_perc = paste(perc, "%", sep = "")
  ) %>%
  lolliplot(
    data = ., 
    x = reorder(Position, perc),
    y = perc, 
    labels = lab_perc, 
    y_max_limit = 40,
    title = "Position",
    title_size = 22
  )

lollipop_cluster0_question2

# Merge in one figure ----------------------------------------------------------------

lollipop_figure_school_position <-
  lollipop_cluster0_question1 / lollipop_cluster0_question2 

lollipop_figure_school_position

# save to file
ggsave(
  filename = "figure_school_position.png",
  plot = lollipop_figure_school_position,
  device = "png",
  path = here("img"),
  scale = 3,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600
)

# END ----------------------------------------------------------------
