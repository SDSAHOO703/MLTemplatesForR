# K-Nearest-Neighbours Regression

# Importing the dataset
# dataset = read.csv('DATASET_NAME.csv')
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]


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


# Fitting K-Nearest-Neighbour (Classification) Model into the
# training_set and Predicting the results
# install.packages('Rfast')
library(Rfast)
y_pred = knn(x = as.matrix(training_set[, -3]),
             xnew = as.matrix(test_set[, -3]),
             y = as.numeric(training_set[, 3]),
             k = 5,
             type = "C")


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
y_grid = knn(x = as.matrix(training_set[, -3]),
             xnew = as.matrix(test_set[, -3]),
             y = as.numeric(training_set[, 3]),
             k = 5,
             type = "C")
plot(set[, -3],
     main = 'K-Nearest-Neighbour (Training Set)',
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
y_grid = knn(x = as.matrix(training_set[, -3]),
             xnew = as.matrix(test_set[, -3]),
             y = as.numeric(training_set[, 3]),
             k = 5,
             type = "C")
plot(set[, -3],
     main = 'K-Nearest-Neighbour (Test Set)',
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
