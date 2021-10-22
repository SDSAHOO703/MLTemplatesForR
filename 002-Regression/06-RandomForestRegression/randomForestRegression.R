#Random Forest Regression 

#Importing the dataset
ds = read.csv('Position_Salaries.csv')
dataset = ds[2:3]   #First column is redundant 


# Installing necessary packages
# install.packages('caTools')
library(caTools)
set.seed(123)


#Fitting Random Forest Regression To The Dataset
#install.packages('randomForest')
library(randomForest)
set.seed(1234)
rf_reg = randomForest(x = dataset[1],
                      y = dataset$Salary,
                      ntree = 500)


#Predicting A New Result With Random Forest Regression
X_test = 6.5 
y_pred = predict(rf_reg, 
                 data.frame(Level = X_test))


#Visualizing Random Forest Regression Results (higher 
#resolution)
#install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), 
             max(dataset$Level),
             0.01)
ggplot() +
  geom_point(aes(x = dataset$Level,
                 y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid,
                y = predict(rf_reg, 
                            data.frame(Level = x_grid))),
            colour = 'blue') + 
  ggtitle('Truth or Bluff (Random Forest Regression)') + 
  xlab('Position Level') +
  ylab('Salary') 
