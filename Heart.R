library(dplyr)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(caTools)
library(lattice)
library(caret)
library(pROC)


data <- read.csv(file="E:/Education/Exercise/data R/Heart Disease/heart.csv")
View(data)

data <- data %>% rename(age = ï..age)
colnames(data)
str(data)

heart <- data %>%
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"),
         fbs = if_else(fbs == 1, ">120", "<=120"),
         exang = if_else(exang == 1, "YES" ,"NO"),
         cp = if_else(cp == 1, "ATYPICAL ANGINA",
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL",
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal)) %>% 
  mutate_if(is.character, as.factor) %>%
  select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())
str(heart)
View(heart)

#Observing the Outliers
xtabs(~ target + sex, data = heart)
xtabs(~ target + restecg, data = heart)
boxplot(heart[,10:14])

#Calculating the Proportion
prop.table(table(heart$target))

heart %>% 
  filter(target=="YES") %>% 
  count(age) %>% 
  ggplot(aes(x=age, y=n)) + 
  geom_bar(stat = "identity") +
  labs(title = "Age vs Patient", x="Age", y="Total")
View(heart)

#Splitting train and test
set.seed(150)
split <- sample.split(heart, SplitRatio = 0.75)
train <- subset(heart, split == TRUE)
test <- subset(heart, split == FALSE)
result <- test %>% select(target)
test <- test %>% select(-target)
View(test)

#Modelling Using GLM
train_glm <- glm(target~., data = train, family = "binomial")
summary(train_glm)
test_pred <- predict(train_glm, type = "response", newdata = test)



#Modelling Using Random Forest
rf_model <- randomForest(target~., data=train, importance=TRUE) 
rf_model
test_pred2 <- predict(rf_model, type = "response", newdata = test)
View(test_pred2)



#Confusion Matrix
cf1 <- data.frame(prediction = as.factor(ifelse(test_pred>=0.62, "YES", "NO")), 
                  result = as.factor(ifelse(result==1, "YES", "NO")))
View(cf1)
confusionMatrix(cf1$prediction, cf1$result)
cf2 <- data.frame(prediction = as.factor(ifelse(test_pred2>=0.5, "YES", "NO")), 
                  result = as.factor(ifelse(result==1, "YES", "NO")))
View(cf2)
confusionMatrix(cf2$prediction, cf2$result)

#ROC
par(pty="s")
ROC1 <- data.frame(result = result, predicted = test_pred)
ROC1 <- ROC1 %>% arrange(predicted)
ROC1$rank <- 1:nrow(ROC1)
ggplot(data = ROC1, aes(x = rank, y=predicted))+
  geom_point(aes(color = target))
View(ROC1)


ROC.info <- roc(ROC1$target, ROC1$predicted, plot = TRUE, legacy.axes = TRUE)
ROC.table <- data.frame(tpp=ROC.info$sensitivities*100, fpp= (1-ROC.info$specificities)*100, treshold=ROC.info$thresholds)
ROC.table


ROC2 <- data.frame(result, predicted = test_pred2)
roc(ROC2$target, ROC2$predicted, plot = TRUE)
plot.roc(ROC2$target, ROC2$predicted, legacy.axes = TRUE, add=TRUE, col="green")
