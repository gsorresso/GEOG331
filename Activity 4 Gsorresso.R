#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop that produces a regression table for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris  sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

#create a subset for iris versicolor 
flower <- iris[iris$Species == "versicolor",]

##create x and y variable list
variables <- list(flower$Sepal.Length , flower$Sepal.Width, flower$Petal.Length, 
                  flower$Petal.Width, flower$Sepal.Length, flower$Petal.Length)

xvar <- list(flower$Sepal.Length, flower$Petal.Length, flower$Sepal.Length)
yvar <- list(flower$Sepal.Width, flower$Petal.Width, flower$Petal.Length)

##make empty results list to hold regression results from for loop
regResults <- list()

##get regression results for each of 3 requested by unlisting xvar and yvar lists and then running regression 
for(i in 1:3){
  xvar1 = unlist(xvar[i])
  yvar1 = unlist(yvar[i])
  regResults[[i]] <- lm(xvar1 ~ yvar1)
  
}

##look at regression results 
regResults

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#create new data frame equal to old iris df 
IrisWMaxHieght <- iris
#use full join to add height to new df by species 
IrisWMaxHeight <- full_join(IrisWMaxHeight, height, by ='Species')


#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width, xlab = "Sepal Length", 
     ylab = "Sepal Width", main = "Sepal Length against Sepal Width",
     col = "royalblue3")


install.packages("ggplot2")
library(ggplot2)
#3a. now make the same plot in ggplot
IrisPlot <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, ))  
IrisPlot + geom_point() + ggtitle("Sepal Length against Sepal Width")

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
IrisPlot + theme_classic()  + geom_point() + ggtitle("Sepal Length against Sepal Width")

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
IrisPlotColor <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species,size = Petal.Length ))  
IrisPlotColor + theme_classic()  + geom_point() + ggtitle("Sepal Length against Sepal Width, with sizing by Petal Length")



#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################