#-----
# site selection and dataframe creation
#-----

head(df)
head(df_pop)
str(df)


# --------table 1 ------------------------
library(dplyr)
library(kableExtra)
library(ggplot2)
table <- data.frame(matrix(NA, ncol = 6, nrow = 6))
table2 <- data.frame(matrix(NA, ncol = 2, nrow = 4))

rownames(table) <- c("mean","median","sd","max", "min","length")
colnames(table) <- c("Unemploy", "Highschool","Income","Comfort","Cost.approach","Cost.study")


for (i in 1:6){
  table[1,i] <- round( mean(df[,4+i]), digits = 2)
  table[2,i] <- median((df[,4+i]) )
  table[3,i] <- round( sd((df[,4+i]) ), digits = 2)
  table[4,i] <- max((df[,4+i]) )
  table[5,i] <- min((df[,4+i]) )
  table[6,i] <- round( length((df[,4+i]) ), digits = 0)
}
table <- kbl(table, booktabs = T, align = "c", 
             caption = "Summary of the samples: numeric variables") %>% 
  kable_styling(full_width = F) %>%
  kable_paper() %>%
  footnote(general = 
             "Unemploy: Unemployment rate, Highschool: High school degree rate,
           Income: Mean income level, Comfort: possibility of accepting an invitation,
           Cost.approach: Cost to approach the sites, Cost.study: Cost to run the study"
           ,threeparttable = T)

# --------table 2 ------------------------
table2 <- data.frame(matrix(NA, ncol = 3, nrow = 5))
rownames(table2) <- c("South", "West","Northcentral","Northeast","Sum" )
colnames(table2) <- c("Urban","non-Urban", "Sum")

region<- c("South", "West","Northcentral","Northeast" )


urban <- df %>% filter(Urban==T)
non_urban <- df %>% filter(Urban==F)

for (i in 1:4){
  for (j in 1:3){
  table2[i,1] <- length( urban$Region[urban$Region==region[i]] ) 
  table2[i,2] <- length( non_urban$Region[non_urban$Region==region[i]] )
  table2[i,3] <- table2[i,1] + table2[i,2]
  table2[5,j] <- sum( table2[1:4,j] )
  }
}

table2 <- kbl(table2, booktabs = T, align = "c", 
             caption = "Summary of the samples: non-numeric variables") %>% 
  kable_styling(full_width = F) %>%
  kable_paper() %>%
  footnote(general = " ", threeparttable = T)

#-------graph-------------------------
# ggplot(df, aes(x=Region)) + geom_bar()

ggplot(df, aes(x=Region, y= length(factor(Region)), fill=factor(Urban) )) +
  geom_bar( position="stack", stat="identity") + labs(title="Size of Region/Urban sites in the sample") +
  theme_classic()  +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank()
        ) 

#-------plot----------------------------------
table
table2
