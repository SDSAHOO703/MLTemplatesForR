#Decision Tree Regression 

#Importing the dataset
ds = read.csv('Position_Salaries.csv')
dataset = ds[2:3]   #First column is redundant 


# Installing necessary packages
# # install.packages('caTools')
# library(caTools)
set.seed(123)


#Fitting Decision Tree Regression To The Dataset
#install.packages('rpart')
library(rpart)
dt_reg = rpart(formula = Salary ~ . ,
               data = dataset,
               control = rpart.control(minsplit = 1)
              )


#Predicting A New Result With Decision Tree Regression
X_test = 6.5 
y_pred = predict(dt_reg, 
                 data.frame(Level = X_test)
                )


#Visualizing Decision Tree Regression Results (higher 
#resolution)
#install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), 
             max(dataset$Level),
             0.01
            )
ggplot() +
  geom_point(aes(x = dataset$Level,
                 y = dataset$Salary
                ),
             colour = 'red'
            ) +
  geom_line(aes(x = x_grid,
                y = predict(dt_reg, 
                            data.frame(Level = x_grid)
                           )
               ),
            colour = 'blue'
           ) + 
  ggtitle('Truth or Bluff (Decision Tree Regression)') + 
  xlab('Position Level') +
  ylab('Salary') 
