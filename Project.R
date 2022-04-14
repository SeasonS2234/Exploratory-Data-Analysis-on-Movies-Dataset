

#Movie Recommendation System 

#a Importing the dataset 
library(dplyr)
movie<-read.csv("D:\\DM\\PROJECT\\movie.csv")
View(movie)
str(movie)

summary(movie)


#Data Pre Processing
#b Handling the missing data
movie$Budget.INR. = ifelse(is.na(movie$Budget.INR.),ave(movie$Budget.INR., FUN = function(x) mean(x, na.rm = 'TRUE')),movie$Budget.INR.)

movie$Revenue.INR. = ifelse(is.na(movie$Revenue.INR.),ave(movie$Revenue.INR., FUN = function(x) mean(x, na.rm = 'TRUE')),movie$Revenue.INR.)

#Designing the histogram
hist(movie$Number.of.Screens, col = "red")

#Designing All the Plots
library(ggplot2)
#9
#a Display bar chart 
ggplot(data=movie,aes(Genre))+geom_bar()


#b Design Histogram 
ggplot(data = movie, aes(Budget.INR.))+geom_histogram(bins = 10)


#c Design Scatter Plot
ggplot(data =movie, aes(x = Budget.INR., y = Revenue.INR., col = Genre))+geom_point()


#d Design Box Plot
ggplot(data =movie, aes(fill = Genre, x = Budget.INR., y = Revenue.INR.))+geom_boxplot(notch=TRUE)


#Factoring target feature
movie$Whether.Remake = factor(movie$Whether.Remake, 
                              levels = c('No','Yes'), 
                              labels = c(0,1))

#Transforming data into class variable
Remake_class<-ifelse(movie$Whether.Remake=="1", "Remaked", "Not Remaked");
movie <- data.frame(movie, Remake_class)
View(movie)

#Removing Whether.Remake from Data Frame
movie <- movie[,-3]

#Splitting the Dataset 
library(caTools)
set.seed(2)
id <- sample(2, nrow(movie), prob=c(0.7, 0.3), replace=TRUE)
print(id)

#Training and Testing
movie_train <- movie[id==1,]
movie_test <- movie[id==2,]
print(movie_train)


#Building the model
library(e1071)
model <- naiveBayes(as.factor(Remake_class)~., movie_train)
print(model)

pmodel <- predict(model, movie_test)

#Building the confusion matrix
table(pmodel, movie_test$Remake_class)







