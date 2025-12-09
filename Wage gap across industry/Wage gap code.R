
setwd("C:/Users/Miranda/OneDrive/Documents/Georgia Tech OMSA/FA25/DMSL/Project")
library(ggplot2)
library(dplyr)
library(car)
set.seed(100)

#read and take a sample of the data. only looking at incomes over 0
dataFull <- read.csv("data/usa_00006.csv")
dataFull <- dataFull %>% filter(INCWAGE > 0)
data <- dataFull[sample(nrow(dataFull), 100000), ]


#get industry values written out
data$Industry <- with(data, case_when(
  IND >= 170 & IND <= 490  ~ "Agriculture, Forestry, Fishing and Hunting, and Mining",
  IND >= 500 & IND <= 790  ~ "Construction",
  IND >= 1070 & IND <= 3990 ~ "Manufacturing",
  IND >= 4070 & IND <= 4590 ~ "Wholesale Trade",
  IND >= 4670 & IND <= 5791 ~ "Retail Trade",
  (IND >= 570 & IND <= 690) | (IND >= 6070 & IND <= 6390) ~ "Transportation and Warehousing, and Utilities",
  IND >= 6471 & IND <= 6781 ~ "Information",
  IND >= 6871 & IND <= 7190 ~ "Finance and Insurance, and Real Estate and Rental and Leasing",
  IND >= 7270 & IND <= 7790 ~ "Professional, Scientific, and Management, and Administrative and Waste Management Services",
  IND >= 7860 & IND <= 8470 ~ "Educational Services, and Health Care and Social Assistance",
  IND >= 8561 & IND <= 8690 ~ "Arts, Entertainment, and Recreation, and Accommodation and Food Services",
  IND >= 8770 & IND <= 9290 ~ "Other Services, Except Public Administration",
  IND >= 9370 & IND <= 9590 ~ "Public Administration",
  IND >= 9670 & IND <= 9870 ~ "Military",
  IND == 9920 ~ "Unemployed",
  TRUE ~ NA_character_
))

#get gender values written out
data$SEX <- with(data, case_when(
  SEX == 1 ~ "Male",
  SEX == 2  ~ "Female",
  TRUE ~ NA_character_
))

data$RACE <- as.factor(data$RACE)
data$EDUC <- as.factor(data$EDUC)
data$EMPSTAT <- as.factor(data$EMPSTAT)
data$PWSTATE2 <- as.factor(data$PWSTATE2)
data$Industry <- as.factor(data$Industry)

########EDA
colnames(data)

summary(data$INCWAGE)
boxplot(data$INCWAGE, main="Box Plot of Income", ylab="Income")

#income distribution
ggplot(data, aes(x = INCWAGE)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Income", x = "Income", y = "Count")

#distribution of log income
ggplot(data, aes(x = log(INCWAGE))) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of log(Income)", x = "log(Income)", y = "Count")

#proportion of gender by industry
ggplot(data, aes(x=Industry, fill=SEX)) + geom_bar(position="fill") + ylab("Gender split") + xlab("Industry") +
  ggtitle("Proportion of each gender by industry") + coord_flip()

#count of industry
table(data$Industry)
ggplot(data, aes(x = reorder(Industry, -table(Industry)[Industry]))) +
  geom_bar() + xlab("Industry")+ ylab("Count of Observations") + 
       ggtitle("Count of Observations by Industry") + coord_flip()


plot(data$INCWAGE, data$AGE)
boxplot(INCWAGE ~ SEX, data=data)
boxplot(INCWAGE ~ Industry, data=data)

#########model exploration and assumptions testing
#linear regression
model1 <- lm(log(INCWAGE) ~ Industry*SEX + RACE + EDUC + PWSTATE2, data= data)
#summary(model1)

plot(model1$fitted.values , rstandard(model1), xlab="fitted values", ylab="standardized residuals", main="Fitted values vs Standardized Residuals")
hist(rstandard(model1), main = "Histogram of Residuals", xlab = "Standardized Residuals", ylab = "Frequency")
qqPlot(model1, main = "Normal Q-Q Plot of Residuals", ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")

#variable selection w log link gamma model
library(MASS)
modelFullglm <- glm(INCWAGE ~ SEX + Industry + AGE +  EDUC + RACE + PWSTATE2,
                  family = Gamma(link="log"), data=data)
vif(modelFullglm)

modelFullglm <- glm(INCWAGE ~ SEX * Industry + AGE +  EDUC + RACE + PWSTATE2,
                    family = Gamma(link="log"), data=data)
model_step <- stepAIC(modelFullglm, direction="both", trace=TRUE)
summary(model_step)
colnames(data)

############after model selected- training actual final model
#gamma log: good for positive, continuous, right skweed data. Log link models multiplicative effects. 
# link=log transforms linear predictors into positive predicted values

#splot data
set.seed(100)
traini <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train <- data[traini, ]
test  <- data[-traini, ]

#train "final" model
gammaLog <- glm(INCWAGE ~ SEX * Industry + EDUC + RACE + AGE + PWSTATE2,
                   family = Gamma(link = "log"), data = train)
#assumptions test
plot(fitted(gammaLog), residuals(gammaLog, type="deviance"),
     xlab="Fitted values", ylab="Deviance Residuals",
     main="Residuals vs Fitted")
abline(h=0, col="red")

gammaLogVIF <- glm(INCWAGE ~ SEX + Industry + EDUC + RACE + AGE + PWSTATE2,
                family = Gamma(link = "log"), data = train)
vif(gammaLogVIF)

############evaluate performance
library(randomForest)

lmModel  <- lm(log(INCWAGE) ~ SEX + Industry + EDUC + RACE + AGE + PWSTATE2,
                data = train)
rfModel  <- randomForest(INCWAGE ~ SEX + Industry + EDUC + RACE + AGE,
                data = train, ntree = 100, mtry = 3, na.action = na.omit)

predGLM <- predict(gammaLog, newdata = test, type = "response")
predLM  <- exp(predict(lmModel, newdata = test))
predRF  <- predict(rfModel, newdata = test)

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mae  <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)
r2   <- function(y, yhat) cor(y, yhat, use = "complete.obs")^2

results <- data.frame(
  Model = c("Gamma GLM (log link)", "Linear Regression (log-transformed)", "Random Forest"),
  RMSE = c(rmse(test$INCWAGE, predGLM), rmse(test$INCWAGE, predLM), rmse(test$INCWAGE, predRF)),
  MAE  = c(mae(test$INCWAGE, predGLM), mae(test$INCWAGE, predLM), mae(test$INCWAGE, predRF)),
  R2   = c(r2(test$INCWAGE, predGLM), r2(test$INCWAGE, predLM), r2(test$INCWAGE, predRF))
)


results
mean(test$INCWAGE)
median(test$INCWAGE)

summary(gammaLog)

table(train$SEX)
levels(data$SEX)
levels(train$SEX)
levels(train$Industry)
levels(train$EDUC)
levels(train$RACE)
levels(train$AGE)
levels(train$PWSTATE2)

mean(train$INCWAGE[train$SEX=="Male"]) / mean(train$INCWAGE[train$SEX=="Female"])

########################################
#Plot results: male premium across industry
coefs <- coef(gammaLog)
maleCoef <- coefs["SEXMale"]

interactionNames <- grep("SEXMale:", names(coefs), value = TRUE)

coefs[interactionNames]
industryEffects <- maleCoef + coefs[interactionNames]
industryEffects

pctBaseline <- exp(maleCoef) - 1
pctIndustries <- exp(industryEffects) - 1

industryNames <- sub("SEXMale:Industry", "", interactionNames)

df <- data.frame(
  industry = c("Agriculture, Forestry, Fishing and Hunting, and Mining", industryNames),
  pctMalePremium = c(pctBaseline, pctIndustries)
)


ggplot(df, aes(x = reorder(industry, pctMalePremium), y = pctMalePremium, fill = pctMalePremium)) +
  geom_col() +
  scale_fill_gradient(low = "#a6cee3", high = "#1f78b4") +
  geom_text(aes(label = scales::percent(pctMalePremium, accuracy = 0.1)),
            hjust = -0.1, color = "black") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title= "Percent Difference in Income for Men by Industry",
       x = "Industry",
       y = "Male Income Premium (Percent)",
       fill = "Premium") +
  theme_minimal()

