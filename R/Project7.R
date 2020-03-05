#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stringr)
library(psych)
#Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
  mutate(timeStart = ymd_hms(timeStart), timeEnd = ymd_hms(timeEnd)) %>%
  mutate(condition = factor(condition, levels = c("A", "B", "C"), labels = c("Condition A", "Condition B", "Control"))) %>%
  mutate(gender = factor(gender, levels = c("M", "F"), labels = c("Male", "Female"))) %>%
  filter(q6 == 1) %>%
  subset(select = -q6)

#Visualization
pairs.panels(week7_tbl[,5:13], method = "pearson", density = TRUE)
ggplot(week7_tbl, aes(timeStart, q1)) + 
  geom_point() + 
  labs(x = "Date of Experiment", y = "Q1 Score")

ggplot(week7_tbl, aes(q1, q2, color = gender)) +
  geom_jitter()
ggplot(week7_tbl, aes(q1, q2)) + 
  geom_jitter() + 
  facet_grid(. ~gender) + 
  labs(x = "Score on Q1", y = "Score on Q2")

ggplot(week7_tbl, aes(gender, difftime(timeEnd, timeStart, units = "secs"))) + 
  geom_boxplot() + 
  labs(x = "Gender", y = "Time Elapsed (secs)")


ggplot(week7_tbl, aes(q5, q7, color = condition)) + 
  geom_jitter() + 
  stat_smooth(method = lm, se = FALSE) + 
  labs(x = "Score on Q5", y = "Score on Q7", color = "Experiment Condition") + 
  theme(panel.background = element_blank(), legend.position = "bottom", legend.background = element_rect(fill = scales::alpha('grey', 0.125)))

