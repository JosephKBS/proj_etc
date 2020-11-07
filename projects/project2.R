# ---------------------------------------------------------------------------
# Assignment1
# sampling 100sites from the chosen locations and it is the data of it
# --------------------------------------------------------------------------


#----fetch data-------------------------------
df <- read.csv("./data/sites_sent_invitations.csv")
head(df)
colnames(df)
names(df)

#----simple plotting graph-------------------------------
library(ggplot2)
library(ggthemes)
library(dplyr)

df_region <- df %>% 
  group_by(Region)  %>%
  select(Mean.income)

par(mfrow=c(1,2))
boxplot(Unemployment.rate ~ Region, data=df, col="orange", border="brown")
boxplot(Mean.income ~ Region, data=df, col="orange", border="brown")

boxplot(High.school.degree.rate ~ Region, data=df, col="orange", border="brown")
#----simple summary-------------------------------
#------- creating simple summary dataframe--------------------
 library(grid)
 library(tidyverse)
 library(ggridges)

p1 <- ggplot(df, aes(x=Mean.income, y = as.factor(Region), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Median Income ($)", option = "D") +
  xlim(45, 60) +
  theme_classic() +
  labs(title = 'Comparing Median Income level of four regions', 
       #subtitle="Changes in Global Monthly Mean Temperature",
       #   caption="Source: NOAA National Climate Research Unit"
       x="Median Income Level", y=" Region ") + 
  theme(
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

p1


#-------second plot-------------------------
p1 <- ggplot(df, aes(x=Cost.to.run.study, y = as.factor(Region), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Cost ($)", option = "C") +
  #xlim(45, 60) +
  theme_classic() +
  labs(title = 'Comparing Cost to run study level of four regions', 
       #subtitle="Changes in Global Monthly Mean Temperature",
       #   caption="Source: NOAA National Climate Research Unit"
       x="Cost ($)", y=" Region ") + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

p1
#-------third plot-------------------------
p1 <- ggplot(df, aes(x=Unemployment.rate, y = as.factor(Region), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Rate (%)", option = "E") +
  #xlim(45, 60) +
  theme_classic() +
  labs(title = 'Comparing Unemployment rate of four regions', 
       #subtitle="Changes in Global Monthly Mean Temperature",
       #   caption="Source: NOAA National Climate Research Unit"
       x="Unemployment Rate (%)", y=" Region ") + 
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

p1

#---------------------------------------