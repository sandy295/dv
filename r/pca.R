data("iris")
str(iris)
set.seed(111)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- iris[ind==1,]
testing <- iris[ind==2,]
install.packages("psych")
library(psych)
pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)
pc <- prcomp(training[,-5],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
print(pc)
pairs.panels(pc$x,
             gap=0,
             bg = c("red", "yellow", "blue")[training$Species],
             pch=21)