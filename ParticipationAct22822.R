##2/28/22 Practice w Linear Regression 
#Gabrielle Sorresso 

##subset for iris virginica 
flower <- iris[iris$Species == "virginica",]

#linear model relating petal length to spel length 
fit <- lm(flower$Petal.Length~flower$Sepal.Length)

#view results 
summary(fit)

##create a scatter plot 
plot(flower$Sepal.Length, flower$Petal.Length, 
     main ="Iris Virginica",
     xlab ="Sepal Length",
     ylab ="Petal Length",
     col ="purple", pch = 16)

##plot the residuals, which are stored in regression summary 
plot(flower$Sepal.Length, summary(fit)$residuals,
     main = "Residual Plot",
     xlab = "Sepal Length",
     ylab = "Residuals",
     col = "purple", 
     pch = 16)
#add horizontal line at 0 for reference 
abline(h=0, 
       lty = "dashed")

#histogram of residuals 
hist(summary(fit)$residuals, 
     main = "Hiistigram of residuals", 
     xlab = "Residuals", 
     col = "purple")

#shapiro wilks test 
shapiro.test(summary(fit)$residuals)

##qq plot 
qqnorm(summary(fit)$residuals, pch = 16)

qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm, 
       probs = c(0.25, 0.75), qtype = 7, pch = 16)