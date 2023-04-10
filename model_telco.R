df=read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
attach(df)
df <- df %>% mutate(seniorCitizen=as.factor(seniorCitizen)) %>% na.omit()
summary(df)
df <- df %>% dplyr::select(-customerID) %>%
  mutate_at(7,~as.factor(case_when(. =="No phone service"~"No",.=="No"~"No",.=="Yes"~"Yes"))) %>%
  mutate_at(c(9:14),~as.factor(case_when(.=="No internet service"~"No", .=="No"~"No", .=="Yes"~"Yes")))
summary(df)
plot_missing(df)#We see that dataset have 11 NA values
df=na.omit(df)
df=df[-18]
library(plyr)
df$Churn <- as.factor(mapvalues(df$Churn,
                                     from=c("No","Yes"),
                                     to=c("0", "1")))
####### Modelling

library(caTools)
set.seed(123)
split = sample.split(df$Churn, SplitRatio = 0.70)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

classifier <- glm(formula = Churn ~ .,
                  family = binomial,
                  data = training_set)
prob_pred = predict(classifier, type = 'response', newdata = test_set[-19])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#Calculating Model accuracy

cm = table(test_set[,19], y_pred)
cm
logit_recall <- (cm[2,2]+cm[1,1])/nrow(test_set)
logit_recall
#Accuracy is 80.37 %