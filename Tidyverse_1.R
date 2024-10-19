install.packages("tidyverse")
install.packages("ggpubr")
library(tidyverse)
install.packages("readr")
library(ggplot2)
getwd()
library(dplyr)
library(ggpubr)



tidy_titanic <- read.csv("C:/Projects/Tidyverse/Tydyverse.R/data/titanic (1).csv")
head(tidy_titanic)

filter(tidy_titanic, Age>12)

colnames(tidy_titanic) <- tolower(colnames(tidy_titanic))
colnames(tidy_titanic)


class(tidy_titanic)
tidy_titanic <- tidy_titanic %>% filter(age > 12)
tidy_titanic

tidy_titanic <- tidy_titanic %>%
  filter(age > 12) %>%
  select(survived, sex)

tidy_titanic

# The group_by verb

tidy_titanic <- tidy_titanic %>%
  filter(age > 12) %>%
  select(survived, sex) %>%
  group_by(sex, survived)

tidy_titanic 

tidy_titanic <- tidy_titanic %>%
  filter(age > 12) %>%
  select(survived, sex) %>%
  group_by(sex, survived) %>%
  summarise(num_sex_survive = n(), .groups = 'drop')

tidy_titanic




tidy_titanic <- tidy_titanic %>%
  select(survived, sex) %>%
  group_by(sex, survived) %>%
  summarise(num_sex_survive = n()) %>%
  group_by(sex) %>%
  mutate(pct_sex_survive = num_sex_survive/sum(num_sex_survive) * 100)


tidy_titanic

tidy_titanic <- tidy_titanic %>%
  select(survived, sex) %>%
  group_by(sex, survived) %>%
  summarise(num_sex_survive = n()) %>%
  mutate(pct_sex_survive = num_sex_survive/sum(num_sex_survive) * 100) %>%
  filter(survived == 1)
  
tidy_titanic

ggplot(data = tidy_titanic )

figure <- ggplot(data = tidy_titanic, mapping = aes(x = sex, y = num_sex_survive, fill = sex))+
  geom_bar(stat = "identity") +
  ylab("Percentage Surviving")+
  labs(title = "Effect of Sex on Survival aboard the Titanic")+
  theme_classic()+
  theme_classic(base_size = 17) 
  xlab(NULL)+
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))+
  scale_x_discrete(labels = c("Female", "Male"))
  
figure
ggsave(plot = figure, filename = "my_fig.png", width = 7, height = 4.5)





tidy_titanic_new <- read.csv("C:/Projects/Tidyverse/Tydyverse.R/data/titanic (1).csv")


head(tidy_titanic_new)

colnames(tidy_titanic_new)

ggplot(data = tidy_titanic_new, mapping = aes(x = Survived, y = Fare)) +
  geom_point()  # or any other geom layer you prefer


ggplot(data = tidy_titanic_new, mapping = aes(x = factor(Survived, labels = c("Died", "Survived")), y = Fare)) +
  geom_boxplot() +
  xlab("Survival Status") +
  ylab("Fare Paid") +
  ggtitle("Fare vs. Survival Status")

#read_csv("https://gist.githubusercontent.com/michhar/2dfd2de0d4f8727f873422c5d959fff5/raw/fa71405126017e6a37bea592440b4bee94bf7b9e/titanic.csv") %>%  
tidy_titanic_new %>%
  filter(is.numeric(age) & Fare > 0) %>%  # Changed Age to age
  mutate(Age_bracket = case_when(
    age > 60 ~ "Senior",  # Changed Age to age
    age < 16 ~ "Child",   # Changed Age to age
    TRUE ~ "Adult"
  )) %>%
  ggplot(mapping = aes(x = factor(Survived, labels = c("Died", "Survived")), y = Fare)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ Age_bracket) +
  stat_compare_means(method = "t.test", label = "p.signif")







