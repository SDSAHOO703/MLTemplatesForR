# Naive Bayes Classification

# Importing the dataset
# dataset = read.csv('DATASET_NAME.csv')
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]


# Encoding categorical data
dataset$Purchased = factor(dataset$Purchased,
                           levels = c(0,1)) 


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 3/4)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])


# Fitting Naive Bayes (Classification) Model into the
# training_set
# install.packages('e1071')   # For Naive Bayes
library(e1071)   # For Naive Bayes
classifier = naiveBayes(x = training_set[-3], 
                        y = training_set[, 3])    


# Predicting the results
y_pred = predict(classifier,
                 newdata = test_set[-3])


# Fetching the observations from test_set
y_test = test_set$Purchased


# Creating the Confusion Matrix 
cm = table(y_test, y_pred)


# Calculating the accuracy of the model
accuracy_vector = ifelse(y_test == y_pred,
                         1,
                         0)
accuracy_score = sum(accuracy_vector) / length(accuracy_vector)
accuracy_score_percent = 100.00 * accuracy_score



# Visualizing Training Set Results
# install.library('Rfast')
library(Rfast)
set = training_set
X1 = seq(min(set[,1]) - 1,
         max(set[,1]) + 1,
         by = 0.01)
X2 = seq(min(set[,2]) - 1,
         max(set[,2]) + 1,
         by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age',
                       'EstimatedSalary')
prob_set = predict(classifier,
                   type = 'raw',
                   newdata = grid_set)
prob_set_num = as.numeric(as.character(prob_set))
y_grid = ifelse(prob_set_num > 0.5,
                1,
                0)
plot(set[, -3],
     main = 'Naive Bayes (Training Set)',
     xlab = 'Age (Scaled)', ylab = 'Estimated Salary (Scaled)',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, 
        matrix(as.numeric(y_grid), length(X1), length(X2)),
        add = TRUE)
points(grid_set, 
       pch = '.',
       col = ifelse(y_grid == 1,
                    'springgreen3',
                    'tomato'))
points(set,
       pch = 21,
       bg = ifelse(set[,3] == 1,
                   'green4',
                   'red3'))


# Visualizing Test Set Results
library(Rfast)
set = test_set
X1 = seq(min(set[,1]) - 1,
         max(set[,1]) + 1,
         by = 0.01)
X2 = seq(min(set[,2]) - 1,
         max(set[,2]) + 1,
         by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age',
                       'EstimatedSalary')
prob_set = predict(classifier,
                   type = 'raw',
                   newdata = grid_set)
prob_set_num = as.numeric(as.character(prob_set))
y_grid = ifelse(prob_set_num > 0.5,
                1,
                0)
plot(set[, -3],
     main = 'Naive Bayes (Test Set)',
     xlab = 'Age (Scaled)', ylab = 'Estimated Salary (Scaled)',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, 
        matrix(as.numeric(y_grid), length(X1), length(X2)),
        add = TRUE)
points(grid_set, 
       pch = '.',
       col = ifelse(y_grid == 1,
                    'springgreen3',
                    'tomato'))
points(set,
       pch = 21,
       bg = ifelse(set[,3] == 1,
                   'green4',
                   'red3'))
