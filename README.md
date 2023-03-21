# Unsupervised-and-Supervised-ML
Three short examples of supervised and unsupervised machine learning using lm(), glm(), and kmeans()

1.	Using the Auto data set from the ISLR package, the supervised machine learning algorithm lm() is used for simple and multiple linear regression with mpg as the response variable and horsepower and weight as the predictors. Diagnostic plots were used to describe the regression fit.
2.	Using the Auto data set from the ISLR package, the supervised machine learning algorithm glm() is used for logistic regression to predict whether a given car has high or low gas mileage. A binary categorical variable mpg01 that contains a 1 if mpg contains a value > its median, and a 0 if mpg contains a value <= its median. The glm() algorithm is used to train the model with mpg01 as the response variable. After calculating the test error rate, the model is optimized using the ROC curve approach.
3.	Using the iris data set, the unsupervised machine learning algorithm kmeans() is used to find clusters within the Sepal.Length and Sepal.Width. The number of centroids is set to 3.
