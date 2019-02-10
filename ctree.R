# Make Connection and Imprt Data
library(RODBC)
candidates <- read_csv("C:/Users/johd003/Desktop/Git/predicting-win-candidate/candidates.csv", 
                               +     col_types = cols(X1 = col_skip()))
library(randomForest)
library(party)
library(dplyr)

#balance classses
data <- candidates
winners <- data %>% filter(WinLose == 1)
losers <- data %>% filter(WinLose == 0)

losers_reduced <- losers %>% 
  group_by(req_id) %>% 
  mutate(Random = runif(1)) %>%
  arrange(Random) %>%
  mutate(RowNumber = row_number())%>%
  ungroup() %>%
  filter(RowNumber<=2)

cat_cols <- data %>% select_if(is.character) %>% colnames()
all_cols <- data %>% colnames()
drop_vars <- c('req_id','Result')
keep_vars <- setdiff(all_cols,drop_vars)

model_data <- losers_reduced %>% 
  select(one_of(all_cols)) %>%
  union(winners) %>%
  select(one_of(keep_vars))

##change characters to factors 
model_data_cat_cols <- model_data %>% select_if(is.character) %>% colnames()
model_data[model_data_cat_cols] <- lapply(model_data[model_data_cat_cols],factor)

# # divide data into 90% training and 10% testing
# rand <- runif(nrow(data), min = 0, max = 100)
# data <- cbind(data,rand)
# data$WinLose <- ifelse(data$WinLose == 1,'Win','Lose')
# training <- subset(data, data$rand >= 10)
# testing <- subset(data, data$rand < 10)

library(partykit)
TreeModel = ctree(WinLose~., data = model_data)
plot(TreeModel)

TreeModel = ctree(WinLose~., data = model_data %>% filter(LT=='Information Technology'))
plot(TreeModel)

# #print a tree per labor type
# lt_list <- unique(model_data$LT)
# 
# for (i in 1:length(lt_list)) {
#   data_i <- subset(model_data, LT == lt_list[i]) # subselect group
#   TreeModel = ctree(WinLose~., data = data_i)
#   p <- plot(TreeModel)
#   jpeg(paste("plot_", i, ".png", sep = ""), width=600, height=500, res=120) # start export
#   print(p) 
#   dev.off() # finish export
# }


# rfmodel <- randomForest(x=training[,predictor_vars], 
#                         y=training[,response_var],
#                         ntree=10,
#                         nodesize=7,
#                         importance=T
# )
# 
# varImportance <- importance(model)
# varImportance[1:10, ]
