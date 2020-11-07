# ---------------------------------------------------------------------------
# Assignment1
# sampling 100sites from the chosen locations and it is the data of it
# --------------------------------------------------------------------------


#----fetch data-------------------------------
df <- read.csv("./data/sites_sent_invitations.csv")
head(df)
colnames(df)
names(df)

#-----correlation chart-----------------------------------
df1 <- dff # total population
df2 <- df  # 200 sampling

for (i in 1:209){
  df1$Urban[i] = df1$Urban[i]*1 
  df1$Other.program.at.site[i] = df1$Other.program.at.site[i]*1
}
for (i in 1:209){
  df2$Urban[i] = df2$Urban[i]*1
}

df1$Urban <- as.numeric(df1$Urban)
df2$Urban <- as.numeric(df2$Urban)
# df1$Other.program.at.site <- as.numeric(df1$Other.program.at.site)
head(df1)
str(df1) # check
str(df2) # check


# selecting only numeric columns
num.cols <- sapply(df1, is.numeric) 
num.cols2 <- sapply(df2, is.numeric)

cor.data <- cor(df1[ , num.cols])
cor.data2 <- cor(df2[ , num.cols2])
head(cor.data)



#----box plot -------------------------------
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




#-------first plot-------------------------
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
