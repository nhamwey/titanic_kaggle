library('xgboost')
library(plotly)
library(data.table)
library(dplyr)
library(tidyr)

#Read in data
train <- fread(input = 'C:/Users/ndigi/Documents/R/Titanic/train.csv')
test <- fread(input = 'C:/Users/ndigi/Documents/R/Titanic/test.csv')
gender_thing <- fread(input = 'C:/Users/ndigi/Documents/R/Titanic/gender_submission.csv')

#Combine datasets, create solution column of NA's for test data
test$Survived <- NA
full <- rbind(train, test)

str(full)
lapply(full, function(x) length(unique(x)))

#Count NA values
num_missing <- full %>%
                summarise_all(funs(sum(is.na(.) | . == '')))
num_missing <- gather(num_missing, key = "feature", value = "missing_pct")

# Plot NA Values
xform <- list(categoryorder = "array",
              categoryarray = num_missing$feature[order(num_missing$missing_pct, decreasing = T)])

plot_ly(x = num_missing$missing_pct,
        y =  num_missing$feature,
        type = 'bar',
        orientation = 'h') %>%
        layout(yaxis = xform)

#Impute missing values
full$Embarked[is.na(full$Embarked) | full$Embarked == ''] <- max(full$Embarked)
full$Valid_Age <- ifelse((is.na(full$Age) | full$Age == ''), 'Invalid', 'Valid')
full$Age[is.na(full$Age) | full$Age == ''] <- mean(full$Age, na.rm = T)
full$Fare[is.na(full$Age) | full$Age == ''] <- mean(full$Fare, na.rm = T)

#Plot variables vs. Survival rates
plot_list <- list()
for(var in colnames(full)){
  grouped_surv <- full[!is.na(full$Survived),] %>%
    group_by_at(vars(var)) %>%
    summarise(ave_Survived = mean(Survived, na.rm = T))
  
  plot_list[[var]] <- plot_ly(data = grouped_surv, x = grouped_surv[[var]],
                              y = grouped_surv$ave_Survived, type = 'scatter',
                              marker=list(size=10))
}
plot_list