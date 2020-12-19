
################################################
#    INSTALL AND LOAD PACKAGES
#################################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

library(tidyverse)
library(dplyr)
library(ggcorrplot) 

################################################
#    Data Exploration & Cleaning
################################################

netflix = read.csv(file.choose(), na.strings="", stringsAsFactors = FALSE) # converting empty cells to NA
dim(netflix) #Dimension of dataset
str(netflix) #structure of the dataset
glimpse(netflix) #understand the basic characteristics and get a glimpse of the data
netflix = netflix[,c(1:10)] # removing last 3 columns which are empty (X1, X2 & X3)

# Look at a summary of the data
summary(netflix)

# Changing Column Names
names(netflix)[2]="Date" 
names(netflix)[3]="Day"

head(netflix)
tail(netflix)

select(netflix, Gender) %>% 
  unique%>%
  nrow

unique(netflix$Time.of.Day)
unique(netflix$Completed)
unique(netflix$Gender)
unique(netflix$User.ID)

netflix = na.omit(netflix)  # removing NA

#Buisness Question:  Predict which shows people are watching
################################################
#    Interesting Analysis 
################################################

filter<-(netflix) %>% # 1. Gives the maximum number of hours watched shows
  group_by(Show) %>%
  summarize(netflix_hrs = sum(Time.Watched/60)) %>%
  arrange(desc(netflix_hrs))
View(filter_1)

filter_1<-(netflix) %>% # 2. Gives the watching patterns between gender adn day of the week
  group_by(Day, Gender) %>%
  summarize(netflix_hrs = sum(Time.Watched/60)) %>%
  arrange(desc(netflix_hrs))

table(duplicated(netflix$User.ID)) # 3. Gives if same person is contributing to the watching hours

table(netflix$Day) # 4. Correlation Analysis of shows
table(netflix$Show)
table(netflix$Gender)


# Friends
friends = netflix[netflix$Show=="Friends",]
table(friends$Season)
table(friends$Episode)
table(friends$Time.Watched)
table(friends$Gender)

f <- data.frame(as.numeric(friends$Season),   
                as.numeric(friends$Episode),
                as.numeric(friends$Time.Watched))
f <- cor(f)  
colnames(f)=c("Seaons","Episodes","Times_watched")
rownames(f)=c("Seaons","Episodes","Times_watched")
ggcorrplot(f,title="Correlation plot for 'Friends'" ,type = "lower", lab = TRUE)


# American Horror story
horror = netflix[netflix$Show=="American Horror Story",]
table(horror$Season)
table(horror$Episode)
table(horror$Time.Watched)
table(horror$Gender)

h <- data.frame(as.numeric(horror$Season),   
                as.numeric(horror$Episode),
                as.numeric(horror$Time.Watched))
h <- cor(h)  
colnames(h)=c("Seaons","Episodes","Times_watched")
rownames(h)=c("Seaons","Episodes","Times_watched")
ggcorrplot(h, title="Correlation plot for 'American Horror story'", type = "lower", lab = TRUE)


# Orange Is The New Black
orange = netflix[netflix$Show=="Orange Is The New Black",]
table(orange$Season)
table(orange$Episode)
table(orange$Time.Watched)
table(orange$Gender)

o <- data.frame(as.numeric(orange$Season),   
                as.numeric(orange$Episode),
                as.numeric(orange$Time.Watched))
o <- cor(o)  
colnames(o)=c("Seaons","Episodes","Times_watched")
rownames(o)=c("Seaons","Episodes","Times_watched")
ggcorrplot(o,title="Correlation plot for 'Orange Is The New Black'",type = "lower", lab = TRUE)


# Stranger Things
stranger_things = netflix[netflix$Show=="Stranger Things",]
table(stranger_things$Season)
table(stranger_things$Episode)
table(stranger_things$Time.Watched)
table(stranger_things$Gender)

s <- data.frame(as.numeric(stranger_things$Season),   
                as.numeric(stranger_things$Episode),
                as.numeric(stranger_things$Time.Watched))
s <- cor(s)  
colnames(s)=c("Seaons","Episodes","Times_watched")
rownames(s)=c("Seaons","Episodes","Times_watched")
ggcorrplot(s,title="Correlation plot for 'Stranger Things'",type = "lower", lab = TRUE)




