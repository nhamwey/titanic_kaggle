library(glmnet)

#Define continuous and categorical variables
cont_vars <- c('Age', 'Fare')
factor_vars <- c('Pclass', 'Sex', 'Embarked', 'Valid_Age', 'Fam_Size_Desc', 'Title_grp',
                'Ticket_Room_grp', 'Age_grp', 'Fare_grp')
model_vars <- c(cont_vars, factor_vars)

#Convert factor variables
for(var in factor_vars){
  full[[var]] <- as.factor(full[[var]])
}
for(var in cont_vars){
  full[[var]] <- as.numeric(full[[var]])
}

#Check data structure
str(full[!'Survived' %in% NA, model_vars])

#Extract non-NA rows for modeling
train_test <- full[!is.na(full$Survived),]

#Create train/test split indicator
split_id <- sample(1:2, size = nrow(train_test), replace = TRUE, prob = c(.75, .25))

#Split data into train/test
train <- train_test[split_id == 1,]
test <- train_test[split_id == 2,]

#Convert data into matrix format for glmnet
X_train <- data.matrix(train[, model_vars])
Y_train <- train$Survived
X_test <- data.matrix(test[, model_vars])
Y_test <- test$Survived
X_train_test <- data.matrix(train_test[, model_vars])
Y_train_test <- train_test$Survived

#Fit lasso model
cv.lasso <- cv.glmnet(x = X_train,
                      y = Y_train,
                      family = 'binomial',
                      alpha = 1,
                      type.measure = 'class')

#Plot results of cross-validation
plot(cv.lasso, main = "Lasso")

#Create predictions and observe error-rate on train vs. test
train$predictions <- predict(cv.lasso, X_train, type = 'class')
test$predictions <- predict(cv.lasso, X_test, type = 'class')
1-mean(train$Survived != train$predictions)
1-mean(test$Survived != test$predictions)

#Final lasso model
cv.lasso_final <- cv.glmnet(x = X_train_test,
                            y = Y_train_test,
                            family = 'binomial',
                            alpha = 1,
                            type.measure = 'class')

train_test$predictions <- predict(cv.lasso_final, X_train_test, type = 'class')
1-mean(train_test$Survived != train_test$predictions)

coef(cv.lasso_final, s = "lambda.min")

holdout <- full[is.na(full$Survived), c(model_vars, "PassengerId")]
X_holdout <- data.matrix(holdout[, model_vars])
holdout$prediction <- predict(cv.lasso_final, X_holdout, type = "class")
output <- holdout[,c("PassengerId", "prediction")]
names(output)[2] <- "Survived"
fwrite(output, "Titanic_submission.csv")


