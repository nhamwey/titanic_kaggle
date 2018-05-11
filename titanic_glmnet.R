library(glmnet)

#Define continuous and categorical variables
cont_vars <- c()
factor_vars <- c('Pclass', 'Sex', 'Embarked', 'Valid_Age', 'Fam_Size_Desc', 'Title_grp',
                'Ticket_Room_grp')
model_vars <- c(cont_vars, factor_vars)

#Convert factor variables
for(var in factor_vars){
  full[[var]] <- as.factor(full[[var]])
}

#Check data structure
str(full[!Survived %in% NA, ..model_vars])

#Extract non-NA rows for modeling
train_test <- full[!Survived %in% NA,]

#Create train/test split indicator
split_id <- sample(1:2, size = nrow(train_test), replace = TRUE, prob = c(.75, .25))

#Split data into train/test
train <- train_test[split_id == 1,]
test <- train_test[split_id == 2,]

#Convert data into matrix format for glmnet
X <- data.matrix(train[, ..model_vars])
Y <- train$Survived

#Fit lasso model
cv.lasso <- cv.glmnet(x = X,
                      y = Y,
                      family = 'binomial',
                      alpha = 1,
                      type.measure = 'class')

#Plot results of cross-validation
plot(cv.lasso, main = "Lasso")

#Create predictions and observe error-rate
train$predictions <- predict(cv.lasso, X, type = 'class')
mean(train$Survived != train$predictions)
