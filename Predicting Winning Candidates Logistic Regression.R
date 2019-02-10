### LOOK to Transform NTE and SubNTE


# Make Connection and Imprt Data
library(RODBC)
conn <- odbcDriverConnect('driver={SQL Server};server=amtrowsqldev05\\tab_sqa;database=AnalyticsMapping;uid=cognosuser;pwd=Summer1!')
rawdata <- sqlQuery(conn, "select * from dbo.vPredWinCand")
head(rawdata)
names(rawdata)
library(randomForest)
library(party)


# divide data into 80% training and 20% testing
data <- rawdata 
rand <- runif(nrow(data), min = 0, max = 100)
data <- cbind(data,rand)
training <- subset(data, data$rand >= 20)
testing <- subset(data, data$rand < 20)

response_var <- "WinLose"
predictor_vars1 <- setdiff(names(data), list('WinLose','src_sys_cd','CompDistrClass', 'LT', 'RespCnt', 'rand', 'Result', 'req_id', 'SubCnt', 'CompSubClass'))
predictor_vars2 <- setdiff(names(data), list('WinLose','NTE','Sub_NTE', 'src_sys_cd','CompDistrClass', 'LT', 'RespCnt', 'rand', 'Result', 'req_id', 'SubCnt', 'CompSubClass'))
# build the formula that relates respose to predictors
formula <- as.formula(paste(response_var,
                            paste(predictor_vars,collapse=' + '), sep=' ~ '))
formula2 <- as.formula(paste(response_var,
                             paste(predictor_vars2,collapse=' + '), sep=' ~ '))

## check the balance of using the training data
table(data$WinLose)

##train on imbalanced data 
log.reg.imb <- glm(formula2, data=training, family=binomial)

summary(log.reg.imb)

# use the trained model to predict test data on imbalanced data
pred.log.reg.imb <- predict(log.reg.imb, newdata=testing,
                            type="response")

##generate new balanced data by ROSE
data.rose <- ROSE(formula, data=training, seed=123)$data

## check the balance of using the training data
table(data.rose$WinLose)

# train logistic regression on balanced data
log.reg.bal <- glm(formula, data=data.rose, family=binomial)

# use the trained model to predict test data
pred.log.reg.bal <- predict(log.reg.bal, newdata=testing,
                            type="response")
## confusion matrix 
truth <- ifelse(testing$WinLose == '1',TRUE,FALSE)
pred <- ifelse(pred.log.reg.bal>.55,TRUE,FALSE)
confusionmatrix <- table(truth, pred)
confusionmatrix

