#Support Vector Regression 

#Importing the dataset
ds = read.csv('Position_Salaries.csv')
dataset = ds[2:3]   #First column is redundant 


# Installing necessary packages
# install.packages('caTools')
library(caTools)
set.seed(123)


#Fitting Support Vector Regression To The Dataset
# install.packages('e1071')
library(e1071)
svr_reg = svm(formula = Salary ~ . ,
              data = dataset
             )


#Predicting A New Result With Support Vector Regression
X_test = 6.5 
y_pred = predict(svr_reg, 
                 data.frame(Level = X_test)
                )


#Visualizing Support Vector Regression Results
#install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), 
             max(dataset$Level),
             0.1
            )
ggplot() +
  geom_point(aes(x = dataset$Level,
                 y = dataset$Salary
                ),
             colour = 'red'
            ) +
  geom_line(aes(x = x_grid,
                y = predict(svr_reg, 
                            data.frame(Level = x_grid)
                           )
               ),
            colour = 'blue'
           ) + 
  ggtitle('Truth or Bluff (Support Vector Regression)') + 
  xlab('Position Level') +
  ylab('Salary') 
