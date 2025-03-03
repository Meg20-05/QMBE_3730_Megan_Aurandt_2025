library(tidyverse)
library(caTools)

View(admit)

admit$admit <- as.factor(admit$admit)

str(admit)

summary(admit)

table(admit$admit)

# 273 "0's" 127 "1's" the admit/not admitted is not balanced in this data set

class_proportions <- prop.table(table(admit$admit))
class_proportions

summary(admit$gre)

# the summary statistics for gre show a slight right skew in the data since the mean is slightly larger than the median

# 68.25% admitted 31.75% not admitted

set.seed(1)

split <- sample.split(admit$admit, SplitRatio = 0.7)

train_data <- subset(admit, split == TRUE)
test_data <- subset(admit, split == FALSE)

dim(train_data)
dim(test_data)

log_model <- glm(admit ~ gre + gpa + rank, data = train_data, family = binomial) 

summary(log_model)

# the gre is not significant, the gpa is significant at the 0.05 level a 1 unit increase in gpa
# increases admission by 0.9033. rank is significant at the 0.001 level. the higher the rank the decrease
# in admissions. 

pred_probs <- predict(log_model, test_data, type = "response")
pred_probs

pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

pred_classessss <- as.factor(pred_classes)

head(pred_probs)
head(pred_classes)

do.call(rbind, Map(data.frame, predicted_classes=pred_classes, admit=test_data$admit))

conf_matrix <- table(Predicted = pred_classes, Actual = test_data$admit)
conf_matrix

# the model predicted a lot of false negatives

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))

# my model is only 71.67% correct

ggplot(test_data, aes(x = gre, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admitted",
       x = "GRE",
       y = "Admitted or Not (0 = Admitted, 1 = Not Admitted)") +
  scale_color_manual(values = c("red", "blue"), name = "Prediction")

ggplot(test_data, aes(x = gpa, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admitted",
       x = "GPA",
       y = "Admitted or Not (0 = Admitted, 1 = Not Admitted)") +
  scale_color_manual(values = c("red", "blue"), name = "Prediction")

ggplot(test_data, aes(x = rank, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admitted",
       x = "Rank",
       y = "Admitted or Not (0 = Admitted, 1 = Not Admitted)") +
  scale_color_manual(values = c("red", "blue"), name = "Prediction")

# rank is the most important variable when predicting admission status 
