
# RNG ---------------------------------------------------------------------

seed_proj <- 52118 # A1Z26 cipher (https://www.boxentriq.com/code-breaking/letters-to-numbers)
set.seed(seed_proj)

# Install packages --------------------------------------------------------

# install.packages("here")
# install.packages("tidyverse")
# install.packages("ggrepel")

# Load packages --------------------------------------------------------

library(here)
library(tidyverse)
library(ggrepel)

source(here("code", "functions", "theme_custom.R")) # custom ggplot2 theme

# Data ----------------------------------------------------------------

data_cluster0_question3 <-
  read_csv(
    here("data", "preproc", "cluster_0.csv"),
    show_col_types = FALSE
  ) %>% 
  mutate(
    question = as_factor(question),
    item = as_factor(item)
  ) %>% 
  filter(question == "Department") %>% 
  droplevels() %>% 
  mutate(item = as_factor(replace_na(as.character(item), "N/A")))
  
# Question 3, lollipop graph ----------------------------------------------------------------

lollipop_cluster0_question3 <-
  data_cluster0_question3 %>%
  ggplot(aes(x = reorder(item, perc), y = perc)) +
  geom_point(size = 6, color = "#0C8066") +
  geom_segment(aes(x = item, xend = item, y = 0, yend = perc), color = "#012328") +
  geom_label_repel(aes(item, perc, label = lab_perc), size = 4, nudge_y = 4, segment.alpha = 0, fill = "white", color = "#171C54") +
  scale_y_continuous(
    breaks = seq(0, 80, 10),
    limits = c(0, 80)
  ) +
  labs(
    title = "Department",
    x = ""
  ) +
  coord_flip() +
  theme_custom

lollipop_cluster0_question3

# Merge in one figure ----------------------------------------------------------------

lollipop_figure1 <-
  lollipop_cluster0_question1 / lollipop_cluster0_question2 / lollipop_cluster0_question3 +
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
