library(tidyverse)
library(caTools)
library(caret)
library(randomForest)

loan_default_data_set<-read.csv("C:/Users/rodge/Downloads/loan_default_data_set.csv")

dim(loan_default_data_set)

# The data has 20000 rows and 21 columns

library(dplyr)
glimpse(loan_default_data_set)

str(loan_default_data_set)

# all the variables are classified as numeric except for the "rep_education" variable which is classified 
# as character. 

colSums(is.na(loan_default_data_set))

# it looks like only 3 variables contain missing values Percentage of open credit cards with over 50%
# utilization, annual income, and education level

# We could get rid of the rows with missing values, but then we would have to get rid of almost 2000+ rows
# of data. Another option is to imput the missing rows with the mean/mode of that column. So for education
# the one row with missing values we can just input the mode since it's categorical data, and for the pct_card_over_50_uti
# and income columns we can input the column mean into the rows with missing values since it's numeric data.

impute_missing <- function(loan_default_data_set) {
  loan_default_data_set %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), names(sort(table(.), decreasing = TRUE))[1], .)))
}

loan_clean <- impute_missing(loan_default_data_set)

colSums(is.na(loan_clean))

# Now there is no missing values in any of the columns. My code above filled in the missing values with
# the mean and mode of their respected columns. 

sum(duplicated(loan_clean))

# There are no duplicate rows in the data, but there are duplicate data types (numeric)

# if there was duplicate data we could just remove that data from the set. If the data was classified
# wrong we can re classify it. 

library(ggplot2)

ggplot(loan_clean, aes(x = rep_income, y = tot_balance)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Total Balance vs. Income",
       x = "Income ($)",
       y = "Total Balance") +
  theme_minimal()

boxplot(rep_income ~ tot_balance, data = loan_clean, 
        xlab = "Balance", ylab = "Income ($)", 
        main = "Boxplot of Balance by Income", 
        col = "pink")

# looking at both of these plots we can see that there is no correlation (positive or negative) between
# total balance and annual income

table(loan_clean$rep_education)

# looking at the education variable we can see that "other" is underrepresented
# "other" could mean less than highschool. "graduate" is also slightly underrepresented. 

table(loan_clean$Def_ind)

# the data is inbalanced, one way to fix this is to use SMOTE and increase the observations in "1"

hist(loan_clean$rep_income, 
     main = "Histogram of Reported Income", 
     xlab = "Reported Income", 
     col = "pink", 
     breaks = 20)

# This data looks normal, not skewed

default_by_education <- loan_clean %>%
  group_by(rep_education) %>%
  summarise(default_rate = mean(Def_ind, na.rm = TRUE)) %>%
  arrange(desc(default_rate))
    
 print(default_by_education)   
 
 # Using the code above we can see that the education level with the highest defaults is High School
 

loan_clean$Def_ind <- as.factor(loan_clean$Def_ind)

str(loan_clean)

summary(loan_clean)

table(loan_clean$Def_ind)

class_proportions <- prop.table(table(loan_clean$Def_ind))
class_proportions

set.seed(1)

split <- sample.split(loan_clean$Def_ind, SplitRatio = 0.8)

train_data <- subset(loan_clean, split == TRUE)
test_data <- subset(loan_clean, split == FALSE)

dim(train_data)
dim(test_data)

model <- glm(Def_ind ~ rep_education + rep_income + tot_balance + avg_bal_cards + credit_age + 
               credit_age_good_account + credit_card_age + num_acc_30d_past_due_12_months + num_acc_30d_past_due_6_months
             + num_mortgage_currently_past_due + tot_amount_currently_past_due + num_inq_12_month + num_card_inq_24_month +
               num_card_12_month + uti_open_card + pct_over_50_uti + uti_max_credit_line
             + pct_card_over_50_uti + ind_XYZ, 
             data = train_data, 
             family = "binomial")

pred_probs <- predict(model, test_data, type = "response")
pred_probs

pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

pred_classessss <- as.factor(pred_classes)

head(pred_probs)
head(pred_classes)

do.call(rbind, Map(data.frame, predicted_classes=pred_classes, admit=loan_clean$Def_ind))
conf_matrix <- table(Predicted = pred_classes, Actual = test_data$Def_ind)
conf_matrix

# 3569 true negatives, 31 false positives, 321 false negatives, and 79 true positives


accuracy <- (79 + 3569) / (79 + 3569 + 321 + 31)
cat("Accuracy: ", accuracy * 100, "%\n")
# This model is 91.2% accurate. 

precision <- 79 / (79 + 31)
cat("Precision: ", precision * 100, "%\n")
# This model is 71.81% precise. Only correctly predicted 19.75% positive observations. 

recall <- 79 / (79 + 321)
cat("Recall: ", recall * 100, "%\n")
# 19.75% recall 

knn_model <- train(Def_ind ~ ., data=train_data, method='knn', tuneLength=5) # Fit KNN model


pred_knn <- predict(knn_model, test_data)

print(confusionMatrix(pred_knn, test_data$Def_ind))

dt_model <- train(Def_ind ~ ., data=train_data, method='rpart') 

pred_dt <- predict(dt_model, test_data)


print(confusionMatrix(pred_dt, test$Def_ind))

conf_matrix <- table(Predicted = pred_classes, Actual = test$Def_ind)
conf_matrix

roc_curve <- roc(test_data$Def_ind, pred_probs)
plot(roc_curve)
auc(roc_curve)

summary(model)

# We can see the most important features in the data for predicting default status are
# variables with high statistical signifigance which are education (graduate), average balance on cards,
# credit age, num_acc_30d_past_due_12_months, num_inq_12_month, uti_open_card, and ind_XYZ. 

