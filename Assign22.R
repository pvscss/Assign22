library(factoextra)

#Checking the Structure
str(epi_r)

epi_r<-epi_r1

# Imputing the values
epi_r$title<-as.numeric(as.factor(epi_r$title))
epi_r$protein[is.na(epi_r$protein)]<-mean(epi_r$protein,na.rm = T)
epi_r$fat[is.na(epi_r$fat)]<-mean(epi_r$fat,na.rm = T)
epi_r$sodium[is.na(epi_r$sodium)]<-mean(epi_r$sodium,na.rm = T)
epi_r$calories[is.na(epi_r$calories)]<-mean(epi_r$calories,na.rm = T)
epi_r<-na.omit(epi_r)

library(cluster)
library(factoextra)

set.seed(123)

rec<-sample(1:15,15)

rec_df1<-epi_r[rec, ]

head(rec_df1,3)

rec_df1_scaled<-scale(rec_df1)
head(rec_df1_scaled,3)

fviz_nbclust(rec_df1,kmeans,method = 'wss')+geom_vline(xintercept=4,linetype=5,col='red')

set.seed(123)
km.res<-kmeans(rec_df1,4,nstart = 25)
km.res

km.res$totss
km.res$betweenss
