# Make Connection and Imprt Data
library(RODBC)
conn <- odbcDriverConnect('driver={SQL Server};server=amtrowsqldev05\\tab_sqa;database=AnalyticsMapping;uid=cognosuser;pwd=Summer1!')
data <- sqlQuery(conn, "select * from dbo.vPredWinCand")
head(data)
names(data)
library(randomForest)
library(party)


# divide data into 90% training and 10% testing
rand <- runif(nrow(data), min = 0, max = 100)
data <- cbind(data,rand)
data$WinLose <- ifelse(data$WinLose == 1,'Win','Lose')
training <- subset(data, data$rand >= 10)
testing <- subset(data, data$rand < 10)

##balance the classes in the training data

training.win <- subset(data, data$WinLose == 'Win')
training.win <- sample(training.win[1:5000,"req_id"], replace = TRUE)
training.win <- training$req_id %in% training.win
training.lose <- subset(data, data$WinLose == "Lose")
training.lose <- sample(training.lose[1:5000,"req_id"], replace = FALSE)
training.lose <- training$req_id %in% training.lose
training.lose <- subssettraining$req_id %in% training.lose
training <- rbind(training.win,training.lose)


response_var <- "WinLose"
predictor_vars <- setdiff(names(data), list('WinLose', 'rand'))

fmla = WLbin ~ labor_type + Subs + NTE + SRate + Sub2NTE + SubmitTime + SubmitRank
TreeModel = ctree(fmla, data = data)
plot(TreeModel, type = "simple")
summary(TreeModel)
TreeModel2 = ctree(fmla, data = cand, 
                  controls = ctree_control(mincriterion = 0.99, minbucket = 500))
plot(TreeModel2, type = "simple")

rfmodel <- randomForest(x=training[,predictor_vars], 
                        y=training[,response_var],
                        ntree=10,
                        nodesize=7,
                        importance=T
)

varImportance <- importance(model)
varImportance[1:10, ]
