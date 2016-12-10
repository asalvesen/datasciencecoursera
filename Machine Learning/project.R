## Course Project
library(caret)
train_raw = read.csv("pml-training.csv", na.strings = c("", "#DIV/0!", "NA"))
test_raw = read.csv("pml-testing.csv", na.strings = c("", "#DIV/0!", "NA"))

# You should create a report describing how you built your model,
# how you used cross validation,
# what you think the expected out of sample error is,
# and why you made the choices you did.

### Cross Validation
set.seed(1234)
trainIndex = createDataPartition(train_all$classe, p = 0.60,list=FALSE)
train_raw = train_all[trainIndex,]
cv_raw = train_all[-trainIndex,]

## Exploratory Analysis

summary(train_raw)

##data observations:

train_noNA <- train_raw[ , colSums(is.na(train_raw)) < 100]
# "X" is an id variable, unique for each row
# the time variables should not be included, they wouldnt have any bearing
# on future testing
ggplot(data = train_raw) +
        geom_point(aes(x = cvtd_timestamp, y = classe,
                       color = user_name))

# new_window and num_window?
summary(train$new_window)
summary(train$num_window)

ggplot(data = train_raw) +
        geom_point(aes(x = num_window, y = classe,
                       color = new_window))

ggplot(data = train_raw) +
        geom_bar(aes(x = classe, color = new_window))

### remove these variables
train <- train_noNA[, -c(1:7)]
names(train)

## Pre Processing?
preProc <- preProcess(train[, -53], method = "pca", thresh = 0.80)
train_pp <- predict(preProc, train[, -53])
cv_pp <- predict(preProc, cv_clean[, -53])
test_pp <- predict(preProc, cv_clean[, -53])

ppIndex <- createDataPartition(train_pp$classe, p = 0.80,list=FALSE)
small_train = train_pp[ppIndex,]


fitGLM <- train(classe ~ ., method = "rf", data = train)
warsummary(train_clean)

fitGLM <- train(classe ~ ., method = "glm", data = dumbbell)

ggplot(data = train_clean) +
        geom_point(aes(x = pitch_dumbbell, y = classe))
                       color = classe))

summary(train_clean$min )

sample <- nearZeroVar(train_raw, saveMetrics=TRUE)



fitGBM <- train(classe ~ ., data = train, method = "gbm", verbose = FALSE)

fitRPART <- train(classe ~ ., method = "rpart", data = train)
library(rattle); library(rpart.plot)
fancyRpartPlot(fitRPART$finalModel)

