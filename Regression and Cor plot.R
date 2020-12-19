trees
head(trees)
summary(trees)

#Straight line regression graph
regression_graph = lm(Girth ~ Volume,data=trees)
plot(trees$Girth ~ trees$Volume, pch = 16, col = 59,
     main="Regression Line (Girth vs Volume)",col.main=34, col.lab="Blue",
     xlab="Volume",ylab="Girth")
abline(regression_graph,col="red")

#Histogram plot
h=hist(trees$Height, main="Height of Trees",xlab="Height",
       breaks=c(60:90),ylim=c(0,6),col="darkmagenta",border="green",)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex=0.8)

#Density plot
p <- density(trees$Volume,bw = 3)
plot(p, main= "Density plot for Volume",)
polygon(p, col="red", border="blue")
rug(jitter(trees$Volume))

#Box plot
boxplot(trees$Volume,
        col="orange", col.main="darkmagenta", col.lab="Blue" ,col.axis=34,
        main="BoxPlot for Volume of the tress", ylab="Volume")


#Normal Probability Plot
qqnorm(trees$Height,
       col="darkgreen", col.main="darkorchid3",col.lab="darkgreen",col.axis=34,
       main="Normal probability plot", xlab="Height",
       pch=16,font=2, cex=1,font.lab=2)



################# Part 2  #####################

#Oddbooks

oddbooks = read.csv("oddbooks.csv")
summary(oddbooks)
cor_ob=cor(oddbooks) 
library(ggcorrplot)
options(digits = 3, width = 300)
ggcorrplot(cor_ob,hc.order = TRUE, type = "lower",outline.col = "white",
           ggtheme = theme_gray,lab = TRUE) + ggtitle("oddbooks")
logbooks <- log(oddbooks)
logbooks_reg<-lm(weight~breadth + height ,data=logbooks)
summary(logbooks_reg)

#Rubber

install.packages("MASS")
library(MASS)
summary(Rubber)
cor_rb=cor(Rubber) 
library(ggplot2)
library(ggcorrplot)
ggcorrplot(cor_rb,hc.order = TRUE, type = "lower",outline.col = "white",
           ggtheme = theme_gray,lab = TRUE) + ggtitle("Rubber")
logbooks_rb <- log(Rubber)
logbooks_reg_rb<-lm(hard~loss + tens ,data=logbooks_rb)
summary(logbooks_reg_rb)
