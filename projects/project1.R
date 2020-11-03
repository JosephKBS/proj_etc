# ---------------------------------------------------------------------------
# Latinos education and achievement analysis 
# Data source : U.S. citizens were collected in 2000 by the U.S. Census Bureau.
# --------------------------------------------------------------------------


#----fetch data-------------------------------
df <- read.csv("./data/LatinoEd.csv")
head(df)

df$English <- as.factor(df$English)
df$Mex <- as.factor(df$Mex)

#----simple plotting graph-------------------------------
p<- ggplot(df, aes(x=Achieve, fill=English)) +
  geom_density(alpha=0.4) + 
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_fill_discrete( labels = c("fluent", "non-fluent")) + 
  labs(title="Educational achievement of Latino (English)" , x="Age of Immigration")

p
#----simple summary-------------------------------

eng_x <- (subset(df, English==0))
eng_o <- (subset(df, English==1))
summary(eng_o$Achieve)
summary(eng_x$Achieve)

#----box plot-------------------------------
box <-  ggboxplot(df, x = "English", 
                  y = "Achieve",
                  color = "English", 
                  palette = "jco",
                  title = "Boxplot and summary of data")
box

#------- creating simple summary dataframe--------------------
eng_o_achievement <-data.frame(matrix(nrow = 4 , ncol = 2))
for (i in 1:4){
  eng_o_achievement[1,1] <- mean(eng_o$Achieve)
  eng_o_achievement[2,1] <- median(eng_o$Achieve) 
  eng_o_achievement[3,1] <- var(eng_o$Achieve)
  eng_o_achievement[4,1] <- sd(eng_o$Achieve)
  
  eng_o_achievement[1,2] <- mean(eng_x$Achieve)
  eng_o_achievement[2,2] <- median(eng_x$Achieve) 
  eng_o_achievement[3,2] <- var(eng_x$Achieve)
  eng_o_achievement[4,2] <- sd(eng_x$Achieve) 
}
colnames(eng_o_achievement) <- c("fluent_eng","non_fluent_eng")
rownames(eng_o_achievement) <- c("mean", "median", "variance", "stand_dev" )

head(eng_o_achievement)