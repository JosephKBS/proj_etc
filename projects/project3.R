# Assignment4
# 

#----------fetch--------------------------
df_ind <- read.csv("./individuals.csv")
df_sites <- read.csv("./sites.csv")

df_ind_pop <- read.csv("./individuals.csv")
df_sites_pop <- read.csv("./sites.csv")

head(df_ind)
head(df_sites)

# -------------sample---------------
library(ggplot2)
library(dplyr)

df_ind_treat <- df_ind %>%
  filter(Treatment == TRUE)
df_ind_control <- df_ind %>%
  filter(Treatment == FALSE)

mean(df_ind_treat$Income) - mean(df_ind_treat$Mean.income)
mean(df_ind_control$Income) - mean(df_ind_control$Mean.income)

#----population -----------
df_pop_treat <- df_ind_pop %>%
  filter(Treatment == TRUE)
df_pop_control <- df_ind_pop %>%
  filter(Treatment == FALSE)

mean(df_pop_treat$Income) - mean(df_pop_treat$Mean.income)
mean(df_pop_control$Income) - mean(df_pop_control$Mean.income)


#----------facet------------------

# faceting depending on other factor variable
ggplot(df_ind, aes(x=Mean.income, y=Income, colour=factor(Treatment)))+
  geom_point( )+
  stat_smooth(method="lm",se=FALSE) + 
   labs(title = "Mean income to Income (Regional)") +
  facet_grid(rows = vars(Region))

ggplot(df_ind, aes(x=Mean.income, y=Income, colour=factor(Treatment)))+
  geom_point( )+
  stat_smooth(method="lm",se=FALSE) + 
   labs(title = "Mean income to Income (Urban)") +
  facet_grid(rows = vars(Urban))

ggplot(df_ind, aes(x=Cost.to.approach.site, y= Income-Mean.income , colour=factor(Treatment))) +
  geom_point( )+
  stat_smooth(method="lm",se=FALSE) +
   labs(title = "Cost to approach site to Income")
