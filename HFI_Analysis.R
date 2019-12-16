library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(rworldmap)
library(ggmap)
library(maps)
library(rworldmap)
library(leaflet)
library(dplyr)
library(stringr)
library(lubridate)
library(ggrepel)

setwd("D:/719/Project/Data Analysis")
hfi_raw <- read.csv("hfi_2018.csv")

dim(hfi_raw)
str(hfi_raw)
summary(hfi_raw)

#Checking columns which have higher percentage of blank or NA values
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(hfi_raw,2,pMiss)

#Removing columns with more than 15% of missing values
drop_col = c("pf_rol_procedural","pf_rol_civil","pf_rol_criminal","pf_ss_women_inheritance_daughters","pf_ss_women_inheritance_widows","pf_religion_estop_establish","pf_religion_estop_operate","pf_association_association","pf_association_assembly","pf_association_political_establish","pf_association_political_operate","pf_association_political","pf_association_prof_establish","pf_association_prof_operate","pf_association_prof","pf_association_sport_establish","pf_association_sport_operate","pf_association_sport","pf_association","pf_expression_cable","pf_expression_newspapers","pf_expression_internet","pf_identity_legal","pf_identity_parental_marriage","pf_identity_parental_divorce","pf_identity_divorce")
hfi <- hfi_raw[,!names(hfi_raw) %in% drop_col]

length(unique(hfi$countries)) #checking how many unique countries

#Removing rows with NA values in hfi_scores column
a <- complete.cases(hfi[,"hf_score"])
hfi <- hfi[a,]

which(is.na(hfi$hf_score))

#HFI score of 10 means more freedom
hist(hfi$hf_rank, col = "grey", main = "Histogram of Human Freedom Index rank")

#Distribution of HFI scores
hist(hfi$hf_score, col = "#82D9D9", main = "Distribution of Human Freedom Index scores across all countries - 2008 to 2016")

hist(hfi$ef_score, col = "darkcyan", main = "Distribution of Economic Freedom scores across all countries - 2008 to 2016")

View(hfi)

hfi_2016 <- hfi[hfi$year == 2016,]
hfi_2016 <- droplevels(hfi_2016)

hist(hfi_2016$pf_rol, col = "darkcyan", main = "Distribution of Rule of Law across the world in 2016")

hist(hfi_2016$pf_ss, col = "darkcyan", main = "Distribution of Security & Safety across the world in 2016")

hist(hfi_2016$pf_identity, col = "darkcyan", main = "Distribution of Identity and Relationships across the world in 2016")

par(mfrow=c(1,2))
d1 <- density(hfi_2016$pf_score) #density of hf scores
plot(d1,main = "Distribution of Personal Freedom Index scores in 2016")
polygon(d1,col = "#03A6A6")

d2 <- density(hfi_2016$ef_score) #density of hf scores
plot(d2,main = "Distribution of Economic Freedom Index scores in 2016")
polygon(d2,col = "#03A6A6")

#######################################################################

hfi_cat <- hfi_2016
hfi_cat$hf_score <- ifelse(hfi_cat$hf_score>=1 & hfi_cat$hf_score<=6 ,"Low (1-6)", ifelse(hfi_cat$hf_score>6 & hfi_cat$hf_score<=8,"Mid (6-8)","High (>8)"))
hfi_cat <- droplevels(hfi_cat)

barplot(table(hfi_cat$hf_score),ylim = c(0,120),col = "#F2887E", main = "Distribution of Human Freedom scores in 2016" )

#countries with high HFI Index in 2016
hfi_2016 <- hfi[hfi$year == 2016,]

high_hfi <- hfi[hfi$hf_score > 8,]
high_hfi <- droplevels(high_hfi)

###########################################################################
#Top and Bottom Countries with HFI Scores

hfi_ord <- hfi_2016[with(hfi_2016, order(-`hf_score`)), ]
View(hfi_ord)

top_10 <- hfi_ord[1:10,]
top_10 <- droplevels(top_10)
bot_10 <- hfi_ord[153:162,]

df <- rbind(top_10,bot_10)
View(df)
df <- droplevels(df)
rownames(df) <- NULL

df$rz <- round((df$hf_score - mean(df$hf_score))/sd(df$hf_score), 2)  # compute normalized mpg
df$rating <- ifelse(df$rz < 0,"Low","High")
df <- df[order(df$rz),]

# Diverging Barcharts Personal Freedom
ggplot(df, aes(x=reorder(df$countries,df$rz), y=df$rz, label=df$rz)) + 
  geom_bar(stat='identity', aes(fill=df$rating), width=.5)  +
  scale_fill_manual(name="Personal Freedom Score", 
                    labels = c("High", "Low"), 
                    values = c("High"="#03A6A6", "Low"="#F2887E")) + 
  labs(title= "Countries with PF_Scores") + coord_flip()


plot1 <- ggplot(data=top_10, aes(x=reorder(top_10$countries,top_10$hf_score), y=top_10$hf_score),fill = "#3BA60A")  + geom_bar(stat="identity", fill = "#3BA60A")+ ggtitle('10 Most Free Countries') + xlab("HFI Scores") + ylab("Countries") + coord_cartesian(ylim=c(5,8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1), panel.background = element_rect(fill = "white"),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + coord_flip()

plot2 <- ggplot(data=bot_10, aes(x=reorder(bot_10$countries,-bot_10$hf_score), y=bot_10$hf_score),fill = "#F2E963")  + geom_bar(stat="identity", fill = "#F2E963")+ ggtitle('10 Most Repressive Countries') + xlab("HFI Scores") + ylab("Countries") + coord_cartesian(ylim=c(5,8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1), panel.background = element_rect(fill = "white"),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + coord_flip()

grid.arrange(plot1, plot2, ncol=2)

############################################################################

num.cols <- 7
colors <- colorRampPalette(c("#F2EFC4","#F2E963","#B6D936","#3BA60A"))
mycolors <- colors(num.cols)
colors1 <- colorRampPalette(c("#F2EFC4","#F2E963","#B6D936","#3BA60A","#5BD9C2"))
mycolors1 <- colors1(num.cols)
pie(rep(1,num.cols),col = mycolors1)

#Correlation Matrix
colnames(hfi)
corr_df <- hfi[,c("pf_rol","pf_ss","pf_ss_women","pf_movement","pf_religion","pf_expression","pf_identity","ef_government","ef_legal","ef_money","ef_trade","ef_regulation","pf_score","ef_score","hf_score")]
View(corr_df)

#Removing rows with NA values
a <- complete.cases(corr_df[,"pf_ss_women"])
corr_df <- corr_df[a,]
a <- complete.cases(corr_df[,"pf_religion"])
corr_df <- corr_df[a,]
a <- complete.cases(corr_df[,"ef_money"])
corr_df <- corr_df[a,]
a <- complete.cases(corr_df[,"ef_trade"])
corr_df <- corr_df[a,]

corr_matrix <- cor(corr_df)
corr_matrix <- round(corr_matrix,4)
corrplot(corr_matrix, method = "pie",col = mycolors,tl.col = "black",tl.srt = 45 )
corrplot(corr_matrix, method = "pie",col = mycolors1,tl.col = "black",tl.srt = 45 )

###########################################################################

#Map
View(hfi)
mapData <- hfi[['countries']]
mapData <- as.data.frame(mapData)
colnames(mapData) <- 'Countries'
mapData$hfi <- hfi$hf_score
View(mapData)

#Map
mapDevice('x11')
hf_df <- joinCountryData2Map(mapData, joinCode = "NAME",nameJoinColumn = "Countries")
mapCountryData(hf_df,nameColumnToPlot = "hfi", catMethod = "fixedWidth", colourPalette = mycolors)
mapCountryData(hf_df,nameColumnToPlot = "hfi", catMethod = "fixedWidth", colourPalette = mycolors1)

############################################################################
############################################################################
#NEW COLORS
########################################################################################################################################################

plot1 <- ggplot(data=top_10, aes(x=reorder(top_10$countries,top_10$hf_score), y=top_10$hf_score),fill = "#03A6A6")  + geom_bar(stat="identity", fill = "#03A6A6")+ ggtitle('10 Most Free Countries') + xlab("HFI Scores") + ylab("Countries") + coord_cartesian(ylim=c(5,8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1), panel.background = element_rect(fill = "white"),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + coord_flip()

plot2 <- ggplot(data=bot_10, aes(x=reorder(bot_10$countries,-bot_10$hf_score), y=bot_10$hf_score),fill = "#F2887E")  + geom_bar(stat="identity", fill = "#F2887E")+ ggtitle('10 Most Repressive Countries') + xlab("HFI Scores") + ylab("Countries") + coord_cartesian(ylim=c(5,8)) + theme(axis.text.x = element_text(angle = 0, hjust = 1), panel.background = element_rect(fill = "white"),panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + coord_flip()

grid.arrange(plot1, plot2, ncol=2)

############################################################################
#Creating my color palette
num.cols <- 7
colors <- colorRampPalette(c("#F2F2F2","#F2887E","#F2C7AE","#82D9D9","#03A6A6"))
mycolors <- colors(num.cols)
pie(rep(1,num.cols),col = mycolors)

#Correlation Matrix
colnames(hfi)
corr_df <- hfi[,c("pf_rol","pf_ss","pf_ss_women","pf_movement","pf_religion","pf_expression","pf_identity","ef_government","ef_legal","ef_money","ef_trade","ef_regulation","pf_score","ef_score","hf_score")]
View(corr_df)

#Removing rows with NA values
a <- complete.cases(corr_df[,"pf_ss_women"])
corr_df <- corr_df[a,]
a <- complete.cases(corr_df[,"pf_religion"])
corr_df <- corr_df[a,]
a <- complete.cases(corr_df[,"ef_money"])
corr_df <- corr_df[a,]
a <- complete.cases(corr_df[,"ef_trade"])
corr_df <- corr_df[a,]

corr_matrix <- cor(corr_df)
corr_matrix <- round(corr_matrix,4)
corrplot(corr_matrix, method = "pie",col = mycolors,tl.col = "black",tl.srt = 45 )

###########################################################################

colors1 <- colorRampPalette(c("#F2887E","#F2C7AE","#82D9D9","#03A6A6"))
mycolors1 <- colors1(num.cols)

#Map
View(hfi)
mapData <- hfi[['countries']]
mapData <- as.data.frame(mapData)
colnames(mapData) <- 'Countries'
mapData$hfi <- hfi$hf_score
View(mapData)

#Map
mapDevice('x11')
hf_df <- joinCountryData2Map(mapData, joinCode = "NAME",nameJoinColumn = "Countries")
mapCountryData(hf_df,nameColumnToPlot = "hfi", catMethod = "fixedWidth", colourPalette = mycolors1)

###########################################################################

#Personal Freedom Score vs Economic Freedom Score

ggplot(data=hfi_2016, aes(x = hfi_2016$pf_score, y=hfi_2016$ef_score, label = hfi_2016$countries ))+ 
  geom_point(shape=18, size = 3, color="#03A6A6") + geom_text_repel(aes(label=ifelse(hfi_2016$hf_score > 8.56,as.character(hfi_2016$countries),(ifelse(hfi_2016$hf_score < 4.5,as.character(hfi_2016$countries),'')))),hjust=0,vjust=0,color = "black") + 
  theme(panel.border = element_blank(), panel.background = element_rect(fill = "#F2F2F2"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "#F2F2F2")) + ggtitle('Personal Freedom & Economic Freedom 2016') + xlab("Personal Freedom") + ylab("Economic Freedom")

#Personal Freedom Laws
summary(hfi_2016$pf_score)

pf <- ggplot(data=hfi_2016, aes(x = hfi_2016$pf_ss, y=hfi_2016$pf_expression, label = hfi_2016$countries ))+ 
  geom_point(size = hfi_2016$pf_rol, color="#03A6A6") + geom_text_repel(aes(label=ifelse(hfi_2016$pf_score > 9.29,as.character(hfi_2016$countries),(ifelse(hfi_2016$pf_score < 4.0,as.character(hfi_2016$countries),'')))),hjust=0,vjust=0,color = "black") + 
  theme(panel.border = element_blank(), panel.background = element_rect(fill = "#F2F2F2"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "#F2F2F2")) + ggtitle('Personal Freedom Meters') + xlab("Safety & Security") + ylab("Freedom of Expression")

#Economic Freedom Laws

ef <- ggplot(data=hfi_2016, aes(x = hfi_2016$ef_money, y=hfi_2016$ef_trade, label = hfi_2016$countries ))+ 
  geom_point(size = hfi_2016$ef_legal, color="#03A6A6") + geom_text_repel(aes(label=ifelse(hfi_2016$ef_score > 8.05,as.character(hfi_2016$countries),(ifelse(hfi_2016$ef_score < 5.04,as.character(hfi_2016$countries),'')))),hjust=0,vjust=0,color = "black") + 
  theme(panel.border = element_blank(), panel.background = element_rect(fill = "#F2F2F2"),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "#F2F2F2")) + ggtitle('Economic Freedom Parameters') + xlab("Economic Money") + ylab("Economic Trade")

grid.arrange(pf, ef, ncol=2)
