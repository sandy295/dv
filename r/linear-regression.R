data("trees")
# Viewing the trees dataset 
View(trees)

round(cor(trees),2)

install.packages("ggplot2") 
library(ggplot2)
ggplot(trees,aes(x = Girth , y = Volume)) + geom_point() +geom_smooth(method = "lm") # Creating a scatter plot of Volume vs. Height with a linear regression line
ggplot(trees,aes(x = Height , y = Volume)) + geom_point() +geom_smooth(method = "lm") # Building a linear regression model
model <- lm(Volume ~ Girth + Height,data = trees) # Summarizing the linear regression model summary(model)
trees_new <- data.frame( Girth = c(12, 13, 14),
                         Height = c(70, 72, 75)
)

predictions <- predict(model, newdata = trees_new) 
print(predictions)
