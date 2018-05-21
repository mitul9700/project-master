# Author: Mitul Solanki
# Start_date : 23rd feb 2018   End_date : 3rd march 2018
#loading packages
library(data.table)
library(xgboost)
library(tm)
library(dplyr)
library(plyr)
library(stringr)
library(stringi)
library(quanteda)
library(Matrix)
library(rpart)
library(Metrics)
library(DMwR)
library(randomForest)
library(inTrees)

install.packages("xgboost")
install.packages("rpart")
install.packages("randomForest")
install.packages("tidy")


setwd("/Users/mitulsolanki/edwisor_proj/")
train_data = fread("train.tsv", na.strings = c("", NA, "NULL"))
test_data = fread("test.tsv", na.strings = c("", NA, "NULL"))
train_data = train_data %>%
  filter(price != 0)
train_data$price_log  <- log(train_data$price + 1)

#Feature engineering
#wordcount
train_data$name_length = nchar(train_data$name)
train_data$description_length = nchar(train_data$item_description)
train_data$name_words = str_count(train_data$name, '\\S+')
train_data$description_words = str_count(train_data$item_description, '\\S+')

#splitting categories and handeling missing values
temp_var = as_tibble(str_split(train_data$category_name, "/", n = 3, simplify = TRUE))
names(temp_var) = paste0("category", 1:3)
train_data = bind_cols(train_data, temp_var)
train_data$category_name = NULL
rm(temp_var)
train_data$brand_name[is.na(train_data$brand_name)] = "Not Available"
train_data$category1[is.na(train_data$category1)] = "Not Available" 
train_data$category2[is.na(train_data$category2)] = "Not Available"
train_data$category3[is.na(train_data$category3)] = "Not Available"

test_data$name_length = nchar(test_data$name)
test_data$description_length = nchar(test_data$item_description)
test_data$name_words = str_count(test_data$name, '\\S+')
test_data$description_words = str_count(test_data$item_description, '\\S+')

#splitting categories for test data and handeling missing values
temp_variable = as_tibble(str_split(test_data$category_name, "/", n = 3, simplify = TRUE))
names(temp_variable) = paste0("category", 1:3)
test_data = bind_cols(test_data, temp_variable)
test_data$category_name = NULL
rm(temp_variable)

test_data$brand_name[is.na(test_data$brand_name)] = "Not Available"
test_data$category1[is.na(test_data$category1)] = "Not Available" 
test_data$category2[is.na(test_data$category2)] = "Not Available" 
test_data$category3[is.na(test_data$category3)] = "Not Available"
gc()

nrow_train = nrow(train)
nrow_test = nrow(test)
price = train_data$price_log

names(train_data)[1] = names(test_data)[1] = "item_id"  #renaming column

train_data$price = NULL

final_data = bind_rows(train_data, test_data)    #preparing final dataset

#tokenization , stemming , removing stopwords for item variable
final_data.items.tokens = tokens(final_data$item_description, what = "word",
                          remove_numbers = TRUE, remove_punct = TRUE,
                          remove_symbols = TRUE, remove_hyphens = TRUE)
final_data.items.tokens = tokens_tolower(final_data.items.tokens)
final_data.items.tokens = tokens_select(final_data.items.tokens, stopwords(),
                                 selection = "remove")
final_data.items.tokens = tokens_wordstem(final_data.items.tokens, language = "english")
final_data.items.dfm = dfm(final_data.items.tokens)   #preparing doc freq matrix
final_data.items.dfm.trim = dfm_trim(final_data.items.dfm, min_count = 300)  #trimming it to min count of 300
final_data.items.tfidf = dfm_tfidf(final_data.items.dfm.trim)  #applying tfidf

#tokenization , stemming , removing stopwords for name variable
final_data.names.tokens = tokens(final_data$name, what = "word",
                          remove_numbers = TRUE, remove_punct = TRUE,
                          remove_symbols = TRUE, remove_hyphens = TRUE)

final_data.names.tokens = tokens_tolower(final_data.names.tokens)
final_data.names.tokens = tokens_select(final_data.names.tokens, stopwords(),
                                 selection = "remove")
final_data.names.tokens = tokens_wordstem(final_data.names.tokens, language = "english")
gc()

# bag of words
final_data.names.dfm = dfm(final_data.names.tokens)

# trim
final_data.names.dfm.trim = dfm_trim(final_data.names.dfm, min_count = 100) 
gc()

# apply the TF IDF
final_data.names.tfidf = dfm_tfidf(final_data.names.dfm.trim)

topfeatures((final_data.names.tfidf))

previous_na_action = options('na.action')  #to bypass NA value issue
options(na.action='na.pass')

sparse_matrix = sparse.model.matrix(~item_condition_id + brand_name + shipping + name_length + 
                                      description_length + name_words + description_words + 
                                      category1 + category2 + category3,
                                    data = final_data)
class(final_data.items.tfidf) = class(sparse_matrix)
class(final_data.names.tfidf) = class(sparse_matrix)
new_data = cbind(sparse_matrix, final_data.items.tfidf, final_data.names.tfidf)
options(na.action=previous_na_action$na.action)  #disabling NA bypass condition
rownames(new_data) = NULL

#spliing into tarining and test data
training = new_data[seq_len(nrow(train_data)),]
testing = new_data[seq(from = (nrow(train_data) + 1), to = nrow(new_data)), ]


Label = price             #setting label
test1 = xgb.DMatrix(testing)             #preparing dense matrix to pass to XGBoost
train1 = xgb.DMatrix((training), 
                      label = data.matrix(Label))


#setting up XGBoost parameters and regression used is Linear
xgb_params = list(booster="gbtree",
                  colsample_bytree = 0.7,
                  subsample = 0.7,
                  eta = 0.05,
                  objective= 'reg:linear',
                  max_depth= 5,
                  min_child_weight= 1,
                  eval_metric= "rmse")

set.seed(400)

#training model
xgb_model = xgb.train(params = xgb_params,
                  data = train1,
                  nrounds = 500,
                  watchlist = list(train = train1),
                  print_every_n = 50,
                  early_stopping_rounds = 100)

#prediction
prediction = predict(xgb_model, testing)
prediction = exp(prediction) - 1          #converting price back to exp1
results = data.frame(
  test_id = as.integer(seq_len(nrow(test_data)) - 1),
  price = prediction
)

#writing prediction to a csv file and saving it.
write.csv(results, file = "final_prediction.csv", row.names = FALSE)


