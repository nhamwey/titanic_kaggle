library(gbm)

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
train <- train_test[split_id == 1, c(model_vars, 'Survived')]
test <- train_test[split_id == 2, c(model_vars, 'Survived')]
train_test <- train_test[, c(model_vars, 'Survived')]

#Convert data into matrix format for glmnet
X_train_test <- data.matrix(train_test[, model_vars])
Y_train_test <- train_test$Survived

cv.gbm <- gbm(Survived ~.,
              data = train_test,
              distribution = "bernoulli",
              n.trees = 500,
              cv.folds = 5,
              interaction.depth = 5,
              n.minobsinnode = 25,
              shrinkage = 0.003,
              train.fraction = 0.9)

summary(cv.gbm)
train$predictions <- round(predict(cv.gbm, train, type = "response"), 0)
test$predictions <- round(predict(cv.gbm, test, type = "response"), 0)
1-mean(train$Survived != train$predictions)
1-mean(test$Survived != test$predictions)

holdout <- full[is.na(full$Survived), c(model_vars, "PassengerId")]
holdout$prediction <- round(predict(cv.gbm, holdout, type = "response"), 0)
output <- holdout[,c("PassengerId", "prediction")]
names(output)[2] <- "Survived"
fwrite(output, "Titanic_submission.csv")
