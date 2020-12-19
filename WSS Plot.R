install.packages("nbClust") # To determine no. of clusters. Has 30 functions
library(ggmap)
library(tidyverse) # To use tibble() functions
library(dplyr) # used for Data Manipulation. ex: paste(), arrange(), select(), rename()
library(factoextra) # package for extracting and visualizing the results

getwd()
setwd("C:\\Users\\rishi\\Desktop")
data = read.csv("C:\\Users\\rishi\\Desktop\\TKDSalesRegion4.csv")
head(data)

########################### To Determine Optimum no. of clusters required ################################

data.matrix(data) # converting dataframe to matrix
head(data)
test1 = scale(data.matrix(data)[-1]) # scaling every value in the matrix
head(test1)

wssplot = function(test1,nc=20,seed=123){
  wss = (nrow(test1)-1)*sum(apply(test1,2,var)) # applying variance func to full data. This will be helpful in plotting WSS as we can determine the variation among the data. 
  for (i in 2:nc){  # for every cluster
    set.seed(seed)  
    wss[i]=sum(kmeans(test1,centers=i)$withinss)} #kmeans for every cluster
  plot(x=1:nc, y=wss, type="b",xlab="no. of clusters", ylab="within groups sums of squares(wss)")
}

wssplot(test1, nc=20)

######################### To perform k-means #############################################################

a=kmeans(data[,4:7],4)
a
a$cluster
data$cluster = as.factor(a$cluster)
head(data)

######################### To plot a scatter map plot ###############################################

location_df = paste(data$City ,",", data$State)
location_df = tibble(location_df)  # see the difference between dataframe and tibble

locations_geo_df = geocode(location_df$location_df)
# need Google API to use geocode()


