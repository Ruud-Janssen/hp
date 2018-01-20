install.packages("freqparcoord")
install.packages("MASS")

library(freqparcoord)
library(MASS)
data("mtcars")
freqparcoord(mtcars,m=5,dispcols=1:4,k=7)





nums <- sapply(data, is.numeric)

parcoord( data[,nums], col=rainbow(length(mtcars[,1])), var.label=TRUE)

freqparcoord(data,m=30,dispcols=nums,k=30)


nums <- sapply(data, is.numeric)


round(1.5)