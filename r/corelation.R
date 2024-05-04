install.packages("ggpubr")
install.packages("corrplot")
data(mtcars)
library("ggpubr")
require("corrplot")
my_data <- mtcars
ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
res <- cor.test(my_data$wt, my_data$mpg, 
                method = "pearson")
mat <- cor(my_data)
print(mat)
corrplot(mat, method="circle")
corrplot(mat, method="number")

