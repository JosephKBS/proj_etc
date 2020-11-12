# Assignment4
# 

# install.packages("arsenal")
# install.packages("kableExtra")
library(tidyverse)
library(arsenal)
library(ggpubr)
require(moonBook)
library(ggiraph)
require(plyr)
require(rgl)
library(kableExtra)

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

df_delta <- df_ind %>%    filter(Treatment == TRUE) %>%
  mutate(change = Income-Mean.income)
#head(df_delta) # individuals who received treatment
#-control
df_control <- df_ind %>%
  mutate(change = Income-Mean.income) %>%
  filter(Treatment == FALSE) %>%  arrange(change)
df_control_up <- df_control %>%  filter(change >= 0) %>%arrange(change)
df_control_down <- df_control %>% filter(change < 0) %>%arrange(change)

df_control_down_ext <- df_control %>%
  filter(change < 2*mean(df_ind$change)) %>%  # mean income change in control group*2 
  arrange(change)

#treatment
df_normal <- as.data.frame(df_delta) %>%  arrange(change)
df_up <- as.data.frame(df_delta) %>%  filter(change >= 0) %>%  arrange(change)
df_down <- as.data.frame(df_delta) %>%  filter(change < 0) %>%  arrange(change)
df_down_ext <- as.data.frame(df_delta) %>%
  filter(change < 2*mean(df_ind$change) ) %>% # mean income change in treatment group*2 
  arrange(change)

mean(df_ind_treat$Income) - mean(df_ind_treat$Mean.income)
mean(df_ind_control$Income) - mean(df_ind_control$Mean.income)

#----population -----------
df_pop_treat <- df_ind_pop %>%
  filter(Treatment == TRUE)
df_pop_control <- df_ind_pop %>%
  filter(Treatment == FALSE)

mean(df_pop_treat$Income) - mean(df_pop_treat$Mean.income)
mean(df_pop_control$Income) - mean(df_pop_control$Mean.income)

df_ind_treat <- df_ind %>% filter(Treatment == TRUE)
df_ind_control <- df_ind %>%  filter(Treatment == FALSE)
treat_income_change  <- mean(df_ind_treat$Income) - mean(df_ind_treat$Mean.income) 
control_income_change <-(mean(df_ind_control$Income) - mean(df_ind_control$Mean.income) )
# paste("Difference in mean income level of two group is :",  "USD", round(treat_income_change - control_income_change,digits = 2 )*1000 )
Difference_in_income <- paste("Difference in mean income level of two group is :",  "USD", 
                              round(treat_income_change - control_income_change,digits = 2 )*1000 )


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


# faceting depending on other factor variable
plot7a <-ggplot(df_ind, aes(x=Mean.income, y=Income, colour=factor(Treatment)))+
  geom_point( )+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Mean income to Income (Regional)") +
  facet_grid(rows = vars(Region))

# df_ind$Region <- as.factor(df_ind$Region)
# plot7a <-ggplot(subset(df_ind, !is.na(Region)), 
#        aes(x = Mean.income, y = change, color = Region)) +
#         stat_smooth(method = "lm", formula = y ~ x,
#               se = FALSE, fullrange = TRUE) + geom_point()

plot7b <- ggplot(df_ind, aes(x=Mean.income, y=Income, 
                             colour=factor(Treatment)))+
  geom_point( )+
  stat_smooth(method="lm",se=FALSE) + 
  labs(title = "Mean income to Income (Urban)") +
  facet_grid(rows = vars(Urban))

figure3 <- ggarrange(plot7a, plot7b,  ncol = 2, nrow = 1) 
figure3 <- annotate_figure( figure3 , 
                            top = text_grob("[Figure 3] Income changes in regional/Urban ") )

plot7c <- ggplot(df_ind, aes(x=Cost.to.approach.site, y= Income-Mean.income , colour=factor(Treatment))) +
  geom_point( )+
  stat_smooth(method="lm",se=FALSE) +
  labs(title = "Cost to approach site to Income")
