
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
         



# Data ----------------------------------------------------------------

EUR_OS_open_access <-
  readRDS(here("data", "preprocessed", "rds", "cluster_1.rds")) 

# "In your opinion, how important is Open Access for your work?"
EUR_OS_open_access_Q1 <- 
  EUR_OS_open_access %>% 
  filter(question %in% c(
    "School", 
    "Position", 
    "In your opinion, how important is Open Access for your work?")
  ) %>%
  mutate(item = fct_recode(item, !!!Likert_importance_convert)) %>% 
  droplevels() %>% 
  pivot_wider(
    id_cols = participant,
    names_from = question,
    values_from = item
  )

# "What is your experience with Open Access?"
EUR_OS_open_access_Q2 <- 
  EUR_OS_open_access %>% 
  filter(question %in% c(
    "School", 
    "Position", 
    "What is your experience with Open Access?")
  ) %>%
  mutate(item = fct_recode(item, !!!Likert_experience_convert)) %>% 
  droplevels() %>%
   pivot_wider(
    id_cols = participant,
    names_from = question,
    values_from = item
  )

# "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?"
EUR_OS_open_access_Q3 <- 
  EUR_OS_open_access %>% 
  filter(question %in% c(
    "School", 
    "Position", 
    "The following are possible concerns that researchers could have about Open Access publishing. Which of these concerns would you agree with?")
  ) %>%
  mutate(item = fct_recode(item, !!!Likert_concerns_convert)) %>% 
  droplevels() %>%
  pivot_wider(
    id_cols = participant,
    names_from = question,
    values_from = item
  )




  
  
  
  
  
  
[6] "Is there anything you want to share with us regarding your experiences with Open Access?" 



mutate(item = as_factor(replace_na(as.character(item), "N/A")))


# "In your opinion, how important is Open Access for your work?" ----------------------------------------------------------------









%>% 
  count(School, Position, "In your opinion, how important is Open Access for your work?") %>%
  select(-`"In your opinion, how important is Open Access for your work?"`)

# 
# %>% 
#   spread(`"In your opinion, how important is Open Access for your work?"`, n, fill = 0)


  








df <- structure(list(User = c("user1", "user2", "user3", "user4", "user5", 
                              "user6", "user7", "user8", "user9", "user10", "user11", "user12", 
                              "user13", "user14"), 
                     Q1 = c(0, 3, 5, 0, 6, 5, 1, 4, 6, 4, 5, 
                                                          0, 0, 0), 
                     Q2 = c(0, 6, 4, 0, 4, 5, 0, 4, 6, 5, 5, 5, 0, 4), 
                     Q3 = c(0, 
                                                                                                                             4, 5, 3, 4, 5, 0, 4, 4, 5, 5, 0, 0, 0), 
                     Q4 = c(5L, 6L, 6L, 7L, 
                                                                                                                                                                            6L, 6L, 6L, 4L, 3L, 4L, 6L, 5L, 3L, 6L), 
                     Q5 = c(7L, 5L, 6L, 7L, 
                                                                                                                                                                                                                            5L, 5L, 7L, 4L, 4L, 6L, 6L, 6L, 6L, 6L), 
                     Q6 = c(6, 5, 7, 7, 7, 
                                                                                                                                                                                                                                                                            6, 7, 6, 0, 5, 4, 7, 3, 6), 
                     Q7 = c(6L, 7L, 7L, 7L, 6L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                               4L, 5L, 6L, 6L, 6L, 7L, 6L), 
                     Q8 = c(1, 4, 6, 3, 4, 6, 3, 4, 4, 
                                                                                                                                                                                                                                                                                                                                                   5, 4, 0, 1, 3), 
                     Q9 = c(3, 3, 4, 0, 4, 5, 1, 4, 3, 5, 7, 4, 0, 
                                                                                                                                                                                                                                                                                                                                                                          0), 
                     Q10 = c(3, 3, 3, 0, 5, 5, 3, 4, 5, 6, 7, 5, 0, 4), 
                     Q11 = c(3, 
                                                                                                                                                                                                                                                                                                                                                                                                                                         4, 5, 0, 4, 5, 5, 4, 3, 5, 6, 4, 0, 0), 
                     Q12 = c(6, 6, 5, 0, 4, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         0, 7, 4, 5, 3, 0, 6, 5, 5)), 
                .Names = c("User", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12"), row.names = c(NA, -14L), 
                class = "data.frame")

df %>% 
  gather(question, response, Q1:Q12) %>% 
  count(question, response) %>% 
  spread(response, n, fill = 0)

                  
                         
                         
                         




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
