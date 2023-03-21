# ---------------------------------------------------------------
# 1. Linear and Multiple Regression
# ---------------------------------------------------------------

#load data set
library(ISLR)
data(Auto)

#correlation analysis
pairs(Auto[,c(1:8)])
cor(Auto[,c(1:8)], use="complete.obs", method="pearson") 

library(corrplot)
par(xpd=TRUE)
corrplot(cor(na.omit(Auto[,c(1:8)])), 
         method="ellipse", 
         type="upper",
         cl.pos="n",
         tl.pos="diag",
         mar = c(2, 0, 4, 0))
corrplot(cor(na.omit(Auto[,c(1:8)])),
         method="number",
         type="lower",
         tl.pos="n",
         cl.pos="n",
         add=TRUE)

#cylinders, displacement, horsepower and weight show strong positive correlation within the variables
#mpg shows strong negative correlation with the mentioned variables above
#acceleration, year and origin show positive correlation with mpg and negative with cylinders, displacement, horsepower and weight
#cylinders, year and origin are discrete values


#simple linear regression mpg ~ horsepower
#linear model
lm1 <- lm(Auto$mpg ~ Auto$horsepower)
summary(lm1) #strong, significant and negative relationship between horsepower and mpg
lm1$coeff
#(Intercept) Auto$horsepower 
#39.9358610      -0.1578447
#R-squared: 0.6059

#scatterplot
plot(Auto$horsepower, Auto$mpg,pch=19,col="blue")
abline(lm1, col = "red", lwd = 3)
#lines(Auto$horsepower, lm1$fitted, col = "red",lwd = 3) #regression line

#residuals plot
plot(Auto$horsepower,lm1$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=3) #horizontal line: good model


#simple linear regression mpg ~ weight
#linear model
lm2 <- lm(Auto$mpg ~ Auto$weight)
summary(lm2) #strong, significant and negative relationship between weight and mpg
lm2$coeff
#(Intercept)  Auto$weight 
#46.216524549 -0.007647343
#R-squared: 0.6926

#scatterplot
plot(Auto$weight, Auto$mpg,pch=19,col="blue")
abline(lm2, col = "red", lwd = 3)
#lines(Auto$weight, lm2$fitted, col = "red",lwd = 3) #regression line

#residuals plot
plot(Auto$weight,lm2$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=3) #horizontal line: good model


#multiple linear regression mpg ~ horsepower, weight
lm3 <- lm(Auto$mpg ~ Auto$horsepower + Auto$weight)
summary(lm3) #strong, significant and negative relationship between horsepower + weight and mpg
lm3$coeff
#(Intercept) Auto$horsepower     Auto$weight 
#45.640210840    -0.047302863    -0.005794157 
#R-squared: 0.7064


#plots
plot(lm3)
#Residuals vs. Fitted: good model as almost horizontal line
#Normal Q-Q: overall good as residuals are close to straight line, check outliers 321, 325, 382
#Scale-Location: good model as almost horizontal line
#Residuals vs. Leverage: influential observations 316, 324 and 325 which should be checked

#alternative approach for plots
#predicted/fitted vs. residual plot: good distribution
plot(lm3$fitted, lm3$residuals, col="blue",pch=19)
abline(c(0,0),col="red",lwd=3) #horizontal line: good model

#plot by index (row of data set): good as it shows no trend
plot(lm3$residuals, col="blue",pch=19)
abline(c(0,0),col="red",lwd=3) #horizontal line: good model

#QQ plot for residuals: overall good as residuals are close to straight line, theoretical quantiles 2:3 show outliers
rs <- residuals(lm3)
qqnorm(rs, col="blue",pch=19)
qqline(rs, col="red",lwd=3)

#predicted/fitted vs actual values: strong correlation of 0.84, the other variables also influence a little bit mpg
#predict the output of Auto set
predict3 <- predict(lm3, newdata = Auto)
cor(predict3, Auto$mpg)
#[1] 0.8404613      #high correlation 
plot(Auto$mpg, predict3, col="blue",pch=19)
abline(c(0,1), col = "red", lwd=3) #diagonal line: good model

#predicted/fitted values vs. standardized residuals: scale location plot: good distribution
plot(lm3$fitted, sqrt(abs(lm3$residuals)), col="blue",pch=19)
abline(h = median(sqrt(abs(lm3$residuals))), col = "red", lwd = 3)


#predict mpg with horsepower = 98 and weight = 2500
lm3$coeff
#(Intercept) Auto$horsepower     Auto$weight 
#45.640210840    -0.047302863    -0.005794157

predict_mpg <- lm3$coeff[1] + 98 * lm3$coeff[2] + 2500 * lm3$coeff[3]
predict_mpg
#(Intercept) 
#26.51914

# ---------------------------------------------------------------
# 2. Logistic Regression
# ---------------------------------------------------------------

#load data set
library(ISLR)
data(Auto)

#create the mpg01 variable using ifelse statement
Auto_new <- Auto #copy data frame
median(Auto$mpg) #22.75
Auto_new$mpg01 <- ifelse(Auto_new$mpg > median(Auto_new$mpg), 1, 0) #add new column
head(Auto_new, 10)

#correlation analysis
cor(Auto_new[,c(1:8, 10)], use="complete.obs", method="pearson")

#             mpg01
#mpg           0.8369392
#cylinders    -0.7591939
#displacement -0.7534766
#horsepower   -0.6670526
#weight       -0.7577566
#acceleration  0.3468215
#year          0.4299042
#origin        0.5136984
#mpg01         1.0000000

library(corrplot)
par(xpd=TRUE)  #because corrplot() use of margins is quirky
corrplot(cor(na.omit(Auto_new[,c(1:8,10)])), 
         method="ellipse", 
         type="upper",
         cl.pos="n",
         tl.pos="diag",
         mar = c(2, 0, 4, 0))
corrplot(cor(na.omit(Auto_new[,c(1:8, 10)])),
         method="number",
         type="lower",
         tl.pos="n",
         cl.pos="n",
         add=TRUE)

#mpg01 has positive correlation with mpg, acceleration, year and origin
#mpg01 has negative correlation with cylinders, displacement, horsepower and weight
#mpg does not make sense to choose as a predictor as the mpg01 value originates from it
#cylinders, displacement, horsepower and weight have r-values over .60 and will be chosen as predictors


#create training and test set
#create a random sample index
set.seed(1)

#split data set into training set and test set
n <- nrow(Auto_new)  #Number of observations = 392
ntrain <- round(n*0.70)    #70% for training set
tindex <- sample(n, ntrain) #create an index

Auto_train <- Auto_new[tindex,]  #create training set
Auto_test <- Auto_new[-tindex,]  #create test set


#train glm algorithm
formula <- mpg01 ~ cylinders + displacement + horsepower + weight
glm1 <- glm(formula, data = Auto_train, family = "binomial")
summary(glm1) #horsepower and weight show significant, negative correlation


#get predicted probabilities
prob <- predict.glm(glm1, newdata = Auto_test, type="response")
prob

#create vector of predicted probability
glm_pred <- ifelse(prob > 0.5, 1, 0)   #pick arbitrary cut-off of 50%
glm_pred         # Vector has predictions

#calculate actual vs. prediction test error rate
Auto_test$isnotequal <- ifelse (glm_pred != as.numeric(Auto_test$mpg01), 1, 0)
mean(Auto_test$isnotequal)
#mean(glm_pred != as.numeric(Auto_test$mpg01))

# Test error rate: 9.32% using four predictors: cylinders, displacement, horsepower, weight
#[1] 0.09322034


#Optimizing model wit ROC curve approach
#defining function
calc_ROC <- function(probabilities, known_truth, model.name=NULL)
{
  outcome <- as.numeric(factor(known_truth))-1
  pos <- sum(outcome) # total known positives
  neg <- sum(1-outcome) # total known negatives
  pos_probs <- outcome*probabilities # probabilities for known positives
  neg_probs <- (1-outcome)*probabilities # probabilities for known negatives
  true_pos <- sapply(probabilities,
                     function(x) sum(pos_probs>=x)/pos) # true pos. rate
  false_pos <- sapply(probabilities,
                      function(x) sum(neg_probs>=x)/neg)
  if (is.null(model.name))
    result <- data.frame(true_pos, false_pos)
  else
    result <- data.frame(true_pos, false_pos, model.name)
  result %>% arrange(false_pos, true_pos)
}


library(dplyr)
library(ggplot2)

#glm models and ROC curves
#1
glm1 <- glm(mpg01 ~ cylinders,
            data = Auto_new,
            family = binomial)
 
ROC1 <- calc_ROC(probabilities = glm1$fitted.values, 
                 known_truth = Auto_new$mpg01,      
                 model.name = "Cylinders")            

#2
glm1 <- glm(mpg01 ~ displacement,
            data = Auto_new,
            family = binomial)

ROC2 <- calc_ROC(probabilities = glm1$fitted.values, 
                 known_truth = Auto_new$mpg01,      
                 model.name = "Displacement")

#3
glm1 <- glm(mpg01 ~ horsepower,
            data = Auto_new,
            family = binomial)

ROC3 <- calc_ROC(probabilities = glm1$fitted.values, 
                 known_truth = Auto_new$mpg01,      
                 model.name = "Horsepower")

#4
glm1 <- glm(mpg01 ~ weight,
            data = Auto_new,
            family = binomial)

ROC4 <- calc_ROC(probabilities = glm1$fitted.values, 
                 known_truth = Auto_new$mpg01,      
                 model.name = "Weight")

#5
glm1 <- glm(mpg01 ~ horsepower + weight,
            data = Auto_new,
            family = binomial)

ROC5 <- calc_ROC(probabilities = glm1$fitted.values, 
                 known_truth = Auto_new$mpg01,      
                 model.name = "Horsepower + Weight combined")

#6
glm1 <- glm(mpg01 ~ cylinders + displacement + horsepower + weight,
            data = Auto_new,
            family = binomial)

ROC6 <- calc_ROC(probabilities = glm1$fitted.values, 
                 known_truth = Auto_new$mpg01,      
                 model.name = "all combined")

#display ROC curve
ggplot(data=NULL, aes(x=false_pos, y=true_pos)) +
  geom_line(data=ROC1, aes(color=model.name)) +
  geom_line(data=ROC2, aes(color=model.name)) +
  geom_line(data=ROC3, aes(color=model.name)) +
  geom_line(data=ROC4, aes(color=model.name)) +
  geom_line(data=ROC5, aes(color=model.name)) +
  geom_line(data=ROC6, aes(color=model.name))

#calculate the area under the ROC curve (AUC) for the first 
ROCs <- rbind(ROC1, ROC2, ROC3, ROC4, ROC5, ROC6) #combine all ROCs into one big table

#calculate AUCs
ROCs %>% group_by(model.name) %>% 
  mutate(delta=false_pos-lag(false_pos)) %>%
  summarize(AUC=sum(delta*true_pos, na.rm=T)) %>%
  arrange(desc(AUC))
#model.name                     AUC
#<chr>                        <dbl>
#  1 Cylinders                    0.970
#2 all combined                 0.958
#3 Horsepower + Weight combined 0.954
#4 Weight                       0.948
#5 Displacement                 0.942
#6 Horsepower                   0.928

#cylinders has the best AUC

#adjusted model only with cylinders as predictor
#train glm algorithm
formula <- mpg01 ~ cylinders
glm1 <- glm(formula, data = Auto_train, family = "binomial")
summary(glm1)


#get predicted probabilities
prob <- predict.glm(glm1, newdata = Auto_test, type="response")
prob

#create vector of predicted probability
glm_pred <- ifelse(prob > 0.5, 1, 0)   #pick arbitrary cut-off of 50%
glm_pred         # Vector has predictions

#calculate actual vs. prediction test error rate
Auto_test$isnotequal <- ifelse (glm_pred != as.numeric(Auto_test$mpg01), 1, 0)
mean(Auto_test$isnotequal)
#mean(glm_pred != as.numeric(Auto_test$mpg01))

# Test error rate: 11.02% using one predictors (cylinders) is not better!
#[1] 0.1101695

#-> model with all four predictors is best approach with a test error rate of 9.32!
#-> higher percentage rate for the split in training and test set of > 0.80 achieve even better error rate, but number of observations (59 obs.) in the test set is then very small

# ---------------------------------------------------------------
# 3. K-Means Clustering
# ---------------------------------------------------------------

#load data set
data(iris)

#create data frame with Sepal.Length and Sepal.Width
df_iris <- data.frame(sepal.length = iris$Sepal.Length,
                        sepal.width = iris$Sepal.Width)

set.seed(314)
kc <- kmeans(x = df_iris, centers = 3, iter.max=20)

#clusters
kc$cluster
#[1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
#[41] 3 3 3 3 3 3 3 3 3 3 1 1 1 2 1 2 1 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 1 1 1 1 2 2
#[81] 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 1 1 1 2 1 1 1 1 1 1 2 2 1 1 1 1 2
#[121] 1 2 1 2 1 1 2 2 1 1 1 1 1 2 2 1 1 1 2 1 1 1 2 1 1 1 2 1 1 2

#centers
kc$centers
#sepal.length sepal.width
#1     6.812766    3.074468
#2     5.773585    2.692453
#3     5.006000    3.428000

#plot
plot(df_iris$sepal.length, 
     df_iris$sepal.width,
     col = kc$cluster,
     pch = 19,
     cex = 1)
points(kc$centers,
       col = 1:3,
       pch = 3,
       cex = 4,
       lwd = 3)

# ---------------------------------------------------------------
