#Polynomial Regression 

#Importing the dataset
ds = read.csv('Position_Salaries.csv')
dataset = ds[2:3]   #First column is redundant 


# Installing necessary packages
install.packages('caTools')
library(caTools)
set.seed(123)


#Fitting Linear Regression To The Dataset
lin_reg = lm(formula = Salary ~ . ,
             data = dataset
            )


#Predicting A New Result With Linear Regression
y_pred_6_5_lr = predict(lin_reg,
                        data.frame(Level = 6.5)
                       )


#Visualizing Linear Regression Results
install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level,
                 y = dataset$Salary
                ),
             colour = 'red'
            ) +
  geom_line(aes(x = dataset$Level,
                y = predict(lin_reg, newdata = dataset)
               ),
            colour = 'blue'
           ) + 
  ggtitle('Truth or Bluff (Linear Regression)') + 
  xlab('Position Level') +
  ylab('Salary') 


#Fitting Polynomial Regression To The Dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ . ,
              data = dataset
             )


#Predicting A New Result With Polynomial Regression
y_pred_6_5_pr = predict(poly_reg,
                        data.frame(Level = 6.5, 
                                   Level2 = 6.5^2,
                                   Level3 = 6.5^3,
                                   Level4 = 6.5^4
                                  )
                       )


#Visualizing Polynomial Regression Results
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
                y = predict(poly_reg, 
                            data.frame(Level = x_grid,
                                       Level2 = x_grid^2,
                                       Level3 = x_grid^3,
                                       Level4 = x_grid^4
                                      )
                           )
               ),
            colour = 'blue'
           ) + 
  ggtitle('Truth or Bluff (Polynomial Regression)') + 
  xlab('Position Level') +
  ylab('Salary') 
