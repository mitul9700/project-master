library(data.table)
library(magrittr)
library(ggplot2)
library(scales)
library(stringr)
library(quanteda)
library(gridExtra)

train_set <- fread('train.tsv', showProgress = FALSE)
ggplot(data = train_set, aes(x = log(price+1))) + 
  geom_histogram(fill = 'blue') +
  labs(title = 'Distribution of log item price + 1')

ggplot(data = train_set, aes(x =price)) + 
  geom_histogram(fill = 'orange') +
  labs(title = 'Distribution of price')

train_set[, .N, by = item_condition_id] %>%
  ggplot(aes(x = as.factor(item_condition_id), y = N/1000)) +
  geom_bar(stat = 'identity', fill = 'red') + 
  labs(x = 'Item condition', y = 'Number of items', title = 'Number of items by condition category')

train_set[, .(.N, median_price = median(price)), by = item_condition_id][order(item_condition_id)]

ggplot(data = train_set, aes(x = as.factor(item_condition_id), y = log(price+1))) + 
  geom_boxplot(fill = 'yellow', color = 'black')

table(train_set$shipping)

ggplot(data=train_set,aes(x = log(price+1), fill = factor(shipping))) + 
  geom_density(adjust = 2, alpha = 0.6) + 
  labs(x = 'Log price', y = '', title = 'Distribution of price by shipping')

train_set[, .(median_price = median(price)), by = brand_name] %>%
  head(30) %>%
  ggplot(aes(x = reorder(brand_name, median_price), y = median_price)) + 
  geom_point(color = 'green') + 
  scale_y_continuous(labels = scales::dollar) + 
  coord_flip() +
  labs(x = '', y = 'Price', title = 'Top 30 most expensive brands')

train_set[, .(median = median(price)), by = category_name][order(median, decreasing = TRUE)][1:30] %>%
  ggplot(aes(x = reorder(category_name, median), y = median)) + 
  geom_point(color = 'green') + 
  coord_flip() + 
  labs(x = '', y = 'price', title = 'price by item category ') + 
  scale_y_continuous(labels = scales::dollar)

sort(table(train_set$category_name), decreasing = TRUE)[1:10]

train_set[, c("level_1_category", "level_2_category") := tstrsplit(train_set$category_name, split = "/", keep = c(1,2))]
head(train_set[, c("level_1_category", "level_2_category")])
table(train_set$level_1_category)

ggplot(data = train_set , aes(x = level_1_category, y = log(price+1))) + 
  geom_boxplot(fill = 'yellow', color = 'black') + 
  coord_flip() + 
  labs(x = '', y = 'Log price', title = 'Boxplot of price by level_1_category')

ggplot(data = train_set,aes(x = level_2_category, y = log(price+1))) + 
  geom_boxplot(fill = 'yellow', color = 'black') + 
  coord_flip() + 
  labs(x = '', y = 'Log price', title = 'Boxplot of price by level_2_cat')