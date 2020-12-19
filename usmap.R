install.packages("usmap")
library(usmap)
library(ggplot2)

plot_usmap(regions = "states")
plot_usmap(regions = "states") + labs(title = "US States") 

a = plot_usmap()
b = plot_usmap("counties")

statepop

############## Without Labels ##########################################

c = plot_usmap(data = statepop, values = "pop_2015")

d = plot_usmap(data = statepop, values = "pop_2015") + 
    scale_fill_continuous(low = "white", high = "red")

e = plot_usmap(data = statepop, values = "pop_2015") +
    scale_fill_continuous(low = "white", high = "red", guide = FALSE)

f = plot_usmap(data = statepop, values = "pop_2015") +
    scale_fill_continuous(low = "white", high = "red", guide = FALSE) +
    scale_x_continuous(expand = c(1, 1))

g = plot_usmap(data = statepop, values = "pop_2015") +
    scale_fill_continuous(low = "white", high = "red", guide = FALSE) +
    scale_x_continuous(expand = c(1, 1)) + 
    scale_y_continuous(expand = c(1, 1))

################ With Labels ##########################################

plot_usmap(data = statepop, values = "pop_2015", labels = TRUE)

# - https://cran.r-project.org/web/packages/usmap/readme/README.html

