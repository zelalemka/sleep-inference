df <- read.table("Sleep_Efficiency.csv", header = T, sep = ",")

library(leaps)
library(corrplot)
library(Hmisc)
library(purrr)
library(tidyr)
library(ggplot2)
library(dplyr)

## show the number of subjects
nrow(df)
## show the number of male subject
nrow(filter(df, Gender == "Male"))
## show the number of subject who smoke 
nrow(filter(df, Smoking.status == "Yes")) 
## show the number of subject who consume alcohol in the 24 hours prior to bedtime
nrow(filter(df, Alcohol.consumption == 0))

## use percentage as our response variables
df$Sleep.efficiency <- df$Sleep.efficiency * 100

## exclude ID, Bedtime, Wakeup.time
df <- df %>% select(-c("ID", "Bedtime", "Wakeup.time"))

## create a correlation matrix for continuous variables
df_num <- df %>% select(-c("Gender", "Smoking.status"))
corrplot(cor(df_num, use = "pairwise.complete.obs"))
cor(df_num, use = "pairwise.complete.obs")

## exclude Light.sleep.percentage, and Awakenings
df <- df %>% select(-c("Light.sleep.percentage", "Awakenings"))

## create histograms for continuous variables
df_num %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

## create boxplots for y ~ categorical variables
boxplot(Sleep.efficiency ~ Gender, data = df, ylab =
          "sleep efficiency(percentage)")
boxplot(Sleep.efficiency ~ Smoking.status, data = df, xlab = "smoking status", ylab ="sleep efficiency(percentage)")
boxplot(Deep.sleep.percentage ~ Gender, data = df, xlab = "Gender", ylab =
          "Deep sleep percentage")
boxplot(Deep.sleep.percentage ~ Alcohol.consumption, data = df, xlab = "Alcohol consumptions", ylab = "Deep sleep percentage")


## select a model without interaction
lm <- lm(Sleep.efficiency ~ Gender + Age + Sleep.duration + REM.sleep.percentage + Deep.sleep.percentage + Caffeine.consumption + Alcohol.consumption + Smoking.status + Exercise.frequency, data = df)
summary(lm)
plot(lm)
s <- regsubsets(Sleep.efficiency ~., data = df, method = "exhaustive", really.big=T)
ss <- summary(s)
ss$which
ss$adjr2
ss$cp
ss$rsq

lm_best <- lm(Sleep.efficiency ~ Age + REM.sleep.percentage + Deep.sleep.percentage + Caffeine.consumption + Alcohol.consumption + Smoking.status + Exercise.frequency, data = df)
summary(lm_best)

## add 6 interactions to the best model
lm_int <- lm(Sleep.efficiency ~  Age + Gender + REM.sleep.percentage + Deep.sleep.percentage + Caffeine.consumption + Alcohol.consumption + Smoking.status + Exercise.frequency + Smoking.status * Exercise.frequency + Gender * Caffeine.consumption + Gender * Alcohol.consumption + Gender * Smoking.status + Gender * Exercise.frequency + Alcohol.consumption*Smoking.status, data = df)
summary(lm_int)
plot(lm_int)


## identify influential points based on Cook's D (and no influential points found)
cooksD <- cooks.distance(lm_int)
influential <- cooksD[(cooksD > 1)]
influential
