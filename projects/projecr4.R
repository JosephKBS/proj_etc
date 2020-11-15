# ---------------------------------------------------------------------------
# Latinos education and achievement analysis 
# Proj grouping
# Data source : U.S. citizens were collected in 2000 by the U.S. Census Bureau.
# --------------------------------------------------------------------------

# fetch data 
library(dplyr)
require(ggplot2)
df <- read.csv("./data/LatinoEd.csv")
df_new <- df %>% 
  mutate(grouping = English)
  
 # grouping 
  for (i in 1:150){
  if (df_new$English[i]=="1" & df_new$Mex[i]=="1"){
    df_new$grouping[i] <- "g1"
  } else if (df_new$English[i]=="1" & df_new$Mex[i]=="0"){
    df_new$grouping[i] <- "g2"
  } else if (df_new$English[i]=="0" & df_new$Mex[i]=="1"){
    df_new$grouping[i] <- "g3"
  } else if (df_new$English[i]=="0" & df_new$Mex[i]=="0"){
    df_new$grouping[i] <- "g4"
  }
 }
  
mycolors<- c("#FFDB6D", "#C4961A", "#F4EDCA",  "#D16103")

# boxplot
p0 <- ggplot(df_new, aes(x=factor(grouping), y=Achieve, color=factor(grouping) )) + 
  geom_point() + geom_boxplot()

# density plot
p1 <- ggplot(df_new, aes(x=Achieve, fill=grouping)) + 
      geom_density(alpha=.3) + 
      scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#E69F00")) + 
      #scale_fill_discrete( labels = c("fluent", "non-fluent")) +
      labs( x="Achievement") + 
      theme_minimal()
      
      
library(ggridges)
library(ggplot2)
library(ggpubr)

p2 <- ggplot(df_new, aes(x = Achieve, y = grouping, fill = grouping)) +
  geom_density_ridges(scale = 3, size=.1) +
  theme_ridges() + 
  theme(legend.position = "none") +
        theme_minimal() + 

   
   

# plotting options --------------------------------------
# draw_plot(plot, x = 0, y = 0, width = 1, height = 1)

# library("cowplot")
# ggdraw() +
#   draw_plot(p0, x = 0, y = 0, width = .5, height = 1) +
#   draw_plot(p1, x = .5, y = 0, width = 1, height = .5) +
#   draw_plot(p2, x = 0.5, y = 0.5, width = 1, height = 1) +
#   draw_plot_label(label = c("A", "B", "C"), 
#                   size = 5,
#                   x = c(0, 0.5, 0), 
#                   y = c(1, 1, 0.5))

# ggarrange(p0, p1, p2 ,
#           labels = c("A: boxplot", "B: density plots", "C:density plot with ridges"),
#           ncol = 2, nrow = 2)
