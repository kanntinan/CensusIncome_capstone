# DS_material
# Introduction 
This study aim to classify heart disease or not in Heart Disease UCI data set on kaggle.
This data set contain 303 observations with 13 predict variables and 1 target variable (heart disease or not).

Set up and execute library.
```{r include=FALSE}
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
```
```{r include=FALSE}
library(readr)
library(tidyverse)
library(GGally)
library(ggpubr)
library(caret)
library(rattle)
```

## Import data and Variables review
14 variables in this study include : 
age - in years
sex - (1 = male; 0 = female)
cp - chest pain type
trestbps - resting blood pressure (in mm Hg on admission to the hospital)
chol - serum cholestoral in mg/dl
fbs - (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
restecg - resting electrocardiographic results
thalach - maximum heart rate achieved
exang - exercise induced angina (1 = yes; 0 = no)
oldpeak - ST depression induced by exercise relative to rest
slope - the slope of the peak exercise ST segment
ca - number of major vessels (0-3) colored by flourosopy
thal - (3 = normal, 6 = fixed defect, 7 = reversable defect)
target - 1 or 0 (heart disease or not heart desease)

```{r include=FALSE}
heart <- read_csv("C:/Users/PromptNow/Desktop/heart.csv")
```

```{r echo=FALSE, eval=TRUE,message=FALSE}
glimpse(heart)
```


We found some factor variables are numeric type, then we transform those variables to factor and rename some factor.
```{r include=FALSE}
data <- heart %>% mutate(sex = as.factor(ifelse(sex == 1, "male","female")),
                         cp = as.factor(cp),
                         fbs = as.factor(ifelse(fbs == 1, ">120", "<=120")),
                         restecg = as.factor(restecg),
                         exang = as.factor(ifelse(exang == 1, "yes","no")),
                         slope = as.factor(slope),
                         ca = as.factor(ca),
                         thal = as.factor(thal),
                         target = as.factor(target))
```

```{r echo=FALSE, eval=TRUE,message=FALSE}
glimpse(data)
```

## Exploratory data analysis
Visualizing characteristics for each variables.
In this plot we found : 
 1. most observation are male and look like half of male are heart disease, 
   in the other hand most of female are not heart disease.
 2. most of chest pain type (cp) are type 0 and most of type 0 are not heart disease, 
   in the other hand other chest pain type tend to heart disease.
 3. most observation are fasting blood sugar less than or equal 120 mg/dl and not pattern in heart disease.
 4. most observation have resting electrocardiographic results (restecg) 0 and 1, half of 0 restecg are heart disease and
   1 restecg tend to heart disease.
 5. most observation are not exercise induced angina and tend to heart disease if they not exercise induced angina, 
   in the other hand they tend to not heart disease if exercise induced angina.
 6. most observation have slope of the peak exercise ST segment (sploe) 1 and 2, they tend to heart disease if slope =2 and
   not heart disease if slope = 1
 7. most observation have 0 major vessels that tend to heart disease, if they have more number of major vessels they 
   tend to not heart disease respectively.
 8. most observation are 2 and 3 thal, they tnd to heart disease if thal = 2 and they tend to not heart disease if thal = 3.
 9. target variable that observation are heart disease or not disease are quite similar portion.

```{r}
# Ploting factor variables
fctplots <- list()
for(i in names(data[, sapply(data, class) %in% c("character","factor")])) {
  fctplots[[i]] <- data %>% ggplot(aes_string(x = i, fill = "target")) + geom_bar() + labs(title = i)
}
ggarrange(fctplots[[1]],fctplots[[2]],fctplots[[3]],fctplots[[4]],fctplots[[5]],fctplots[[6]],
          fctplots[[7]],fctplots[[8]],fctplots[[9]],ncol = 3, nrow = 3)

```

```{r}
propplots <- list()
for(i in names(data[, sapply(data, class) %in% c("character","factor")])) {
  propplots[[i]] <- data %>% ggplot(aes_string(x = i, fill = "target")) + geom_bar(position = "fill") + labs(title = i)
}
ggarrange(propplots[[1]],propplots[[2]],propplots[[3]],propplots[[4]],propplots[[5]],propplots[[6]],
          propplots[[7]],propplots[[8]],propplots[[9]],ncol = 3, nrow = 3)

```

In numeric variables we foud a bit skew in each variables except oldpeak.
We try to compair with target variable with boxplot and found age, maximum heart rate achieved and 
ST depression induced by exercise relative to rest have some pattern in heart disease.

```{r echo=FALSE, eval=TRUE,message=FALSE}
# Boxplot show age, maximum heart rate achieved(thalach) and ST depression induced by exercise relative to rest(oldpeak) 
# are some difference pattern of heart disease 
data %>% select(age, trestbps, chol, thalach, oldpeak, target) %>% ggpairs()
```

In detail we found: 
with age around > 55 tend to not heart disease.
with maximum heart rate achieved > 150 tend to heart disease. 
with ST depression induced by exercise relative to rest < 1 highly tend to heart disease.

```{r echo=FALSE, eval=TRUE,message=FALSE}
numplots <- list()
for(i in names(data[, sapply(data, class) == "numeric"])) {
  numplots[[i]] <- data %>% ggplot(aes_string(x = i, color = "target", fill = "target")) + 
    geom_density(alpha = 0.3) + labs(title = i)
}
ggarrange(numplots[[1]],numplots[[2]],numplots[[3]],numplots[[4]],numplots[[5]],ncol = 3, nrow = 2)

```

We try to look at near zero variance variables for variables selection and result show non near zero variables in this data set.
But as we found very few observation in cp = 3, fbs > 120, restecg =2, ca = 4, thal = 0,1 from first bar charts that maybe we need to 
penalty this in analysis part.

```{r echo=FALSE, eval=TRUE,message=FALSE}
# near zero variance variables
nzv <- nearZeroVar(data, saveMetrics= TRUE)
nzv
```

# Analysis part
In analysis part we start with split data to train 70% and test set 30% 
(211 observations and 92 observations respectively)

```{r include=FALSE}
# Split train and test set.
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(data$target, times = 1, p = 0.30, list = FALSE)
train_set <-data %>% slice(-test_index)
test_set <- data %>% slice(test_index) 
```

In this study aim to classify heart disease or not. We choose 5 binary classification mathod include logistic regression,
K-nearest neighbor, decision tree, random forest and support vector machine.

In each model we standardize data by center and scale in preprocess function and use 10-fold cross validation in trainControl function.
Evaluate model by accuracy and F1 score for balance accuracy.

1.Logistic regression.
Generalized linear model that modified linear regression output to probability between 0 and 1 by 
sigmoid function (s(x) = 1 / (1 + e^-x) : where e is natural logarithms) for predict an output to binary values.

Before do this, we need to know what optimal cutoff or threshold that split probability into binary result with highest accuracy.
Result show cutoff 0.6 are return higest accuracy. then, we change cutoff to evaluate model to 0.6

```{r echo=FALSE, eval=TRUE,message=FALSE}
# optimize threshold 
logmodel <- glm(target~., data = train_set, family = binomial)
cutoffs <- seq(0.1,0.9,0.1)
accuracy <- NULL
for (i in seq(along = cutoffs)){
  prediction <- ifelse(logmodel$fitted.values >= cutoffs[i], 1, 0) 
  accuracy <- c(accuracy,length(which(train_set$target ==prediction))/length(prediction)*100)
}
plot(cutoffs, accuracy, pch =19,type='b',
     main ="Logistic Regression", xlab="Cutoff", ylab = "Accuracy %")
```

We got logistic regression accuracy = 0.88043 and F1 score = 0.87912

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
# generalized linear model
set.seed(1, sample.kind="Rounding")
fit_glm<- train(target ~ ., data = train_set, method = "glm", family = "binomial",maxit = 100, preProcess = c("center", "scale"),
                trControl = trainControl(method="cv", number = 10, p = 0.8))
y_hat_glm <- predict(fit_glm, test_set, type = "raw")
acc_glm <- confusionMatrix(table(predict(fit_glm, test_set, type = "prob")[,2] >= 0.6,test_set$target == "1"))$overall["Accuracy"]
f1_glm <- confusionMatrix(table(predict(fit_glm, test_set, type = "prob")[,2] >= 0.6,test_set$target == "1"))$byClass["F1"]

print(acc_glm)
print(f1_glm)
```

2.Logistic regression with regularization.
This method we have 2 hyperparameters alpha and lambda ridge regression.

we tuning elasticnet mixing parameter alpha with 0 < alpha < 1 
that defined as (1-alpha) / 2 * abs(beta2)^2 + alpha * abs(beta1)
while alpha = 1 for lasso regression and 0 for ridge regression.

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
# generalized linear model with regularization
set.seed(1, sample.kind="Rounding")
fit_glmnet<- train(target ~ ., data = train_set, method = "glmnet", family = "binomial", preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(alpha = c(0,1), lambda =  seq(0,0.1,0.02)),
                   trControl = trainControl(method="cv", number = 10, p = 0.8))
y_hat_glmnet <- predict(fit_glmnet, test_set, type = "raw")
acc_glmnet <- confusionMatrix(predict(fit_glmnet, test_set), test_set$target )$overall["Accuracy"]
f1_glmnet <- confusionMatrix(predict(fit_glmnet, test_set), test_set$target )$byClass["F1"]

```

We tuning parameter alpha with 0,1 and parameter lambda with 0 to 0.1 
these 2 alpha are close accuracy at lambda between 0 to 0.02
at lambda higher than 0.02 alpha = 0 tend to increase accuracy and highest accuracy at lambda =0.06 
on the other hand alpha tend to decrease accuracy if lambda are higher than 0.02 

result show alpha = 0 and lambda = 0.06 return higest accuracy.

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
plot(fit_glmnet) 
```

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
print(fit_glmnet$bestTune)
```

We got logistic regression with regularization accuracy = 0.84426 and F1 score = 0.82243
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
print(acc_glmnet)
print(f1_glmnet)
```

3.K-nearest neighbor.
This method define the distance between all observations based on the features and find k closest data points (by mean or mode)
and summarizing the output variable for those k instances.

Before do this, we need to know what optimal k-neighbor that classify target with higest accuracy.
Result show, use 9 nearest neighbor to define target are higest accuracy.

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
# fit by knn ####
set.seed(1, sample.kind="Rounding") 
fit_knn<- train(target ~ ., data = train_set, method = "knn",preProcess = c("center", "scale"),
                tuneGrid = data.frame(k = seq(1,15,2)),
                trControl =  trainControl(method = "repeatedcv", number = 10, repeats = 10))
y_hat_knn <- predict(fit_knn, test_set, type = "raw")
acc_knn <- confusionMatrix(predict(fit_knn, test_set), test_set$target)$overall["Accuracy"]
f1_knn <- confusionMatrix(predict(fit_knn, test_set), test_set$target)$byClass["F1"]

fit_knn$bestTune
```

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
plot(fit_knn)
```

We got K-nearest neighbor accuracy = 0.89130 and F1 score = 0.88095
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
print(acc_knn)
print(f1_knn)
```

4.Decision trees.
This method use the best attribute of the dataset as the root node of the tree and split dicision nodes until find leaf nodes (target).
In each split it use information gain to find the feature that best splits the target class into the purest possible children nodes.

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
set.seed(1, sample.kind="Rounding") 
fit_rpart<- train(target ~ ., data = train_set, method = "rpart",preProcess = c("center", "scale"),
                  trControl = trainControl(method="cv", number = 10, p = 0.8),
                  tuneGrid = data.frame(cp = seq(0,0.1,0.01)))
y_hat_rpart <- predict(fit_rpart, test_set, type = "raw")

acc_rpart <- confusionMatrix(predict(fit_rpart, test_set), test_set$target)$overall["Accuracy"]
f1_rpart <- confusionMatrix(predict(fit_rpart, test_set), test_set$target)$byClass["F1"]

```

We tuning complexity parameter(cp) from 0 to 0.1 and found cp = 0.1 return higest accuracy
and get dicision tree as plot below.
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
plot(fit_rpart) 
```
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
fit_rpart$bestTune 
```
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
fancyRpartPlot(fit_rpart$finalModel)
```

We got dicision tree accuracy = 0.81522 and F1 score = 0.8
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
print(acc_rpart)
print(f1_rpart)
```

5.Random Forest.

Ensemble models that combine many decision trees.
In this analysis we use 100 trees and tuning mtry parameter (number of variables available for splitting at each tree node)
and found that mtry = 3 are return highest accuracy.

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
set.seed(1, sample.kind="Rounding") 
fit_rf <- train(target ~ ., data = train_set, method = "rf",preProcess = c("center", "scale"),
                nTree = 100,
                tuneGrid = data.frame(mtry = seq(3,20,2)),
                trControl =  trainControl(method = "cv", number = 10, p = 0.8))
y_hat_rf <- predict(fit_rf, test_set, type = "raw")

acc_rf <- confusionMatrix(predict(fit_rf, test_set), test_set$target)$overall["Accuracy"]
f1_rf <- confusionMatrix(predict(fit_rf, test_set), test_set$target)$byClass["F1"]

```

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
fit_rf$bestTune 
```

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
plot(fit_rf$finalModel)
```

We got random forest accuracy = 0.89130 and F1 score = 0.88372
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
print(acc_rf)
print(f1_rf)
```

6.Support vector machine.
In this method we tuning cost parameter (c) and found c = 0.05 are return highest accuracy.
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
set.seed(1, sample.kind="Rounding") 
fit_svm <- train(target ~ ., data = train_set, method = "svmLinear",preProcess = c("center", "scale"),
                tuneGrid = data.frame(C = seq(0.01,0.15,0.02)),
                trControl =  trainControl(method = "cv", number = 10, p = 0.8))

y_hat_svm <- predict(fit_svm, test_set, type = "raw")
acc_svm <- confusionMatrix(predict(fit_svm, test_set), test_set$target)$overall["Accuracy"]
f1_svm <- confusionMatrix(predict(fit_svm, test_set), test_set$target)$byClass["F1"]
```

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
plot(fit_svm)
```
```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
fit_svm$bestTune 
```

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
print(acc_svm)
print(f1_svm)
```

```{r echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE}
results <- data_frame(method= c("Logistic Regression","Logistic Regression with regularization","K-nearest neighbor",
                                "Decision tree","Random forest","support vector machine"),
                                        Accuracy = c(acc_glm,acc_glmnet,acc_knn,acc_rpart,acc_rf,acc_svm), 
                                        F1_Score = c(f1_glm,f1_glmnet,f1_knn,f1_rpart,f1_rf,f1_svm))
results %>% knitr::kable()
```




