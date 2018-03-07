install.packages("lattice")
library(lattice)

install.packages("ggplot2")
library(ggplot2)

sunspot.month<-datasets::sunspot.month

plot(sunspot.month, main="Plot without Packages")

xyplot(sunspot.month~time, sunspot.month, grid = TRUE, main="Plot using Lattice")

ggplot(sunspot.month, aes(time, sunspot.month)) + 
  geom_point(data=sunspot.month, aes(time, sunspot.month), colour = 'red', size = .5) + 
  ggtitle("Plot using Ggplot2") +
  theme(plot.title = element_text(hjust = 0.5))
