###########
# Load Data
###########

repo <- "http://cran.us.r-project.org"

if(!require(tidyverse)) install.packages("tidyverse", repos = repo)
if(!require(tidytext)) install.packages("tidytext", repos = repo)
if(!require(stringr)) install.packages("stringr", repos = repo)
if(!require(caret)) install.packages("caret", repos = repo)
if(!require(xgboost)) install.packages("xgboost", repos = repo)
if(!require(ranger)) install.packages("ranger", repos = repo)
if(!require(tm)) install.packages("tm", repos = repo)
if(!require(ggplot2)) install.packages("ggplot2", repos = repo)
if(!require(tictoc)) install.packages("tictoc", repos = repo)
if(!require(kableExtra)) install.packages("kableExtra", repos = repo)
if(!require(knitr)) install.packages("knitr", repos = repo)

# Load fake news and true news datasets
fake_news <- read.csv("data/Fake.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
true_news <- read.csv("data/True.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

####################################################### Examine Fake News

# Check the structure of the fake news data
str(fake_news, vec.len = 1)

# Combine title and text and add fake news flag (1)
fake_news <- fake_news %>% 
  mutate(text = paste(title, text, sep = " "), fake = 1) %>% 
  select(text, fake)

# Check first entry of new text variable
fake_news$text[1] %>% 
  knitr::kable(col.names = c("First entry from fake news")) %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:1, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

# Check some of the words in the fake news articles
fake_news %>% 
  unnest_tokens(output = word, input = text) %>% 
  arrange(word) %>% 
  select(word) %>% 
  head(n = 10) %>% 
  knitr::kable() %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:1, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

# Remove stop words and examine the 20 most common words in 
# the fake news articles
fake_news %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  summarize(frequency = n()) %>% 
  arrange(desc(frequency)) %>% 
  head(n = 20) %>% 
  knitr::kable() %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:2, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

####################################################### Examine True News
  
# Check the structure of the true news data
str(true_news, vec.len = 1)

# Combine title and text and add true news flag (0)
true_news <- true_news %>% 
  mutate(text = paste(title, text, sep = " "), fake = 0) %>% 
  select(text, fake)

# Check first entry of new text variable
true_news$text[1] %>% 
  knitr::kable(col.names = c("First entry from true news")) %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:1, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

# Check proportion of articles that contain "????(???? Reuters) - "
true_news %>% 
  filter(str_detect(text, 
                    paste0("[A-Z]\\(Reuters\\) -|[A-Z]\\s\\(Reuters\\) -|",
                           "[A-Z]\\\\[A-Z]\\s\\(Reuters\\) -|\\(Reuters\\) -|",
                           "[A-Z]\\sReuters\\) -|\\([A-Z]\\sReuters\\) -|Reuters"))) %>%
  summarise(perc_w_prefix = n())/nrow(true_news)

# Check some of the words in the true news articles
true_news %>% 
  unnest_tokens(output = word, input = text) %>% 
  arrange(word) %>% 
  select(word) %>% 
  head(n = 10) %>% 
  knitr::kable() %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:1, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

# Remove stop words and examine the 20 most common words in 
# the true news articles. Stop words like "the", "a", "to", etc...
true_news %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  summarize(frequency = n()) %>% 
  arrange(desc(frequency)) %>% 
  head(n = 20) %>% 
  knitr::kable() %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:2, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

####################################################### Combine/Clean Data

# Combine fake and true news datasets, create doc id, 
# convert all text to lowercase, and do some general cleaning
news <- rbind(fake_news, true_news) %>% 
  # create arbitrary document id and convert text to lowercase
  mutate(doc_id = seq(1, nrow(fake_news) + nrow(true_news), 1), 
         text = tolower(text)) %>% 
  # add space between numbers and words
  mutate(text = gsub("(?<=[0-9])(?=[a-z])", " ", text, perl = TRUE)) %>% 
  # strip the "reuters" tags in all its forms from the text
  mutate(text = gsub(paste0("[a-a]\\(reuters\\) -|[a-z]\\s\\(reuters\\) -|",
                            "[a-z]\\\\[a-z]\\s\\(reuters\\) -|\\(reuters\\) -|",
                            "[a-z]\\sreuters\\) -|\\([a-z]\\sreuters\\) -|reuters"), 
                     "", text, perl = TRUE)) %>%
  # remove excess spaces
  mutate(text = gsub("\\s+", " ", text, perl = TRUE)) %>% 
  # remove excess underscores
  mutate(text = gsub("_+", " ", text, perl = TRUE)) %>% 
  select(doc_id, text, fake)

####################################################### Create Data Partitions

# Validation set will be 20% of combined news data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = news$fake, times = 1, p = 0.2, list = FALSE)
edx <- news[-test_index,]
validation <- news[test_index,]

# Create test and train sets from edx set with an 80/20 split
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$fake, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Remove unused data objects
rm(fake_news, true_news, news, test_index)

# Garbage collect to clear out anything 
# lingering in memory
gc()

####################################################### Create DTMs for modeling

# Create tidy token dataframes. This creates one row for each word
# in the text.
edx_tokens <- edx %>% 
  unnest_tokens(output = word, input = text) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% 
  mutate(word = SnowballC::wordStem(word))

validation_tokens <- validation %>% 
  unnest_tokens(output = word, input = text) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% 
  mutate(word = SnowballC::wordStem(word))

train_set_tokens <- train_set %>% 
  unnest_tokens(output = word, input = text) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% 
  mutate(word = SnowballC::wordStem(word))

test_set_tokens <- test_set %>% 
  unnest_tokens(output = word, input = text) %>% 
  filter(!str_detect(word, "^[0-9]*$")) %>%
  anti_join(stop_words) %>% 
  mutate(word = SnowballC::wordStem(word))

# Create Document Term Matrices. DTMs have the doc id in the 
# row and the words in the columns. The value at the intersection
# is the Tf-Idf discussed in the report.
edx_dtm <- edx_tokens %>% 
  count(doc_id, word) %>% 
  cast_dtm(document = doc_id, term = word, value = n,
           weighting = tm::weightTfIdf) %>% 
  removeSparseTerms(sparse = .99)

validation_dtm <- validation_tokens %>% 
  count(doc_id, word) %>% 
  cast_dtm(document = doc_id, term = word, value = n,
           weighting = tm::weightTfIdf) %>% 
  removeSparseTerms(sparse = .99)

train_set_dtm <- train_set_tokens %>% 
  count(doc_id, word) %>% 
  cast_dtm(document = doc_id, term = word, value = n,
           weighting = tm::weightTfIdf) %>% 
  removeSparseTerms(sparse = .99)

test_set_dtm <- test_set_tokens %>% 
  count(doc_id, word) %>% 
  cast_dtm(document = doc_id, term = word, value = n,
           weighting = tm::weightTfIdf) %>% 
  removeSparseTerms(sparse = .99)

# Ensure same terms/words are in both edx and validation document term matrices
edx_dtm <- edx_dtm[, intersect(colnames(edx_dtm), colnames(validation_dtm))]
validation_dtm <- validation_dtm[, intersect(colnames(validation_dtm), colnames(edx_dtm))]

# Ensure same terms/words are in both train and test document term matrices
train_set_dtm <- train_set_dtm[, intersect(colnames(train_set_dtm), colnames(test_set_dtm))]
test_set_dtm <- test_set_dtm[, intersect(colnames(test_set_dtm), colnames(train_set_dtm))]

####################################################### Save some files for later

# Create folder in the wd to save RDS files 
# if it doesn't already exist.
if(dir.exists("RDS") == FALSE){dir.create("RDS")}

# Save some objects to free up memory
saveRDS(edx, file = "RDS/edx.RDS")
saveRDS(validation, file = "RDS/validation.RDS")
saveRDS(edx_dtm, file = "RDS/edx_dtm.RDS")
saveRDS(validation_dtm, file = "RDS/validation_dtm.RDS")
saveRDS(edx_tokens, file = "RDS/edx_tokens.RDS")
saveRDS(validation_tokens, file = "RDS/validation_tokens.RDS")
saveRDS(test_set_tokens, file = "RDS/test_set_tokens.RDS")
saveRDS(train_set_tokens, file = "RDS/train_set_tokens.RDS")
saveRDS(edx_tfidf, file = "RDS/edx_tfidf.RDS")

# Remove some objects to free up memory
rm(edx, validation, edx_dtm, validation_dtm)
rm(edx_tokens, validation_tokens, edx_tfidf)
rm(test_set_tokens, train_set_tokens)

# Garbage collect to clear out anything 
# lingering in memory
gc()

####################################################### Build Models

####################################### Random Forest Model

# Tictoc package used for timing model training
tic()
set.seed(1, sample.kind="Rounding")

# Train Random Forest Model
train_rf <- ranger(fake_flag ~ .,
                   data = data.frame(fake_flag = as.factor(train_set$fake), 
                                     as.matrix(train_set_dtm)),
                   num.trees = 100,
                   classification = TRUE,
                   verbose = FALSE)

toc()

# Make predictions on test set
pred_rf <- predict(train_rf, data = data.frame(as.matrix(test_set_dtm)))

# calculate confusion matrix
cm_rf <- confusionMatrix(pred_rf$predictions, as.factor(test_set$fake))

# Store F1 score
F1_rf <- cm_rf[["byClass"]][["F1"]]

accuracy_results <- tibble(Method = "Random Forest",
                           F1_Score = F1_rf)

accuracy_results %>% knitr::kable() %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:2, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

####################################### Support Vector Machine

# Tictoc package used for timing model training
tic()
set.seed(1, sample.kind="Rounding")
train_svm <- train(x = as.matrix(train_set_dtm),
                   y = as.factor(train_set$fake),
                   method = "svmLinearWeights2")
toc()

# Get training column order to use in the prediction
# I had an issue where the columns not being in the 
# same order was throwing an error.
col_order <- colnames(train_set_dtm)

# Make predictions on test set
pred_svm <- predict(train_svm, as.matrix(test_set_dtm)[, col_order])

# calculate confusion matrix
cm_svm <- confusionMatrix(pred_svm, as.factor(test_set$fake))

# Store F1 score
F1_svm <- cm_svm[["byClass"]][["F1"]]

accuracy_results <- bind_rows(accuracy_results,
                              tibble(Method = "Support Vector Machine",
                                     F1_Score = F1_svm))

accuracy_results %>% knitr::kable() %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:2, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

####################################### Extreme Gradient Boosting

# Tictoc package used for timing model training
tic()
set.seed(1, sample.kind="Rounding")
train_xgb <- xgboost(data = as.matrix(train_set_dtm), 
                     label = train_set$fake, 
                     nrounds = 100,
                     eta = 0.3, 
                     gamma = 0, 
                     max_depth = 6, 
                     min_child_weight = 1, 
                     subsample = 1, 
                     colsample_bytree = 1,
                     eval_metric = "error",
                     objective = "binary:logistic",
                     nthread = 3)
toc()

# Get training column order to use in the prediction
# I had an issue where the columns not being in the 
# same order was throwing an error.
col_order <- colnames(train_set_dtm)

# Make predictions on test set
pred_xgb <- predict(train_xgb, as.matrix(test_set_dtm)[, col_order])

# if predicted value > 0.5 then its a 1 (fake news) otherwise its not fake news
pred_xgb <- ifelse(pred_xgb > 0.5, 1, 0)

# calculate confusion matrix
cm_xgb <- confusionMatrix(factor(pred_xgb), factor(test_set$fake))

# Store F1 score
F1_xgb <- cm_xgb[["byClass"]][["F1"]]

accuracy_results <- bind_rows(accuracy_results,
                              tibble(Method = "XGB",
                                     F1_Score = F1_xgb))

accuracy_results %>% knitr::kable() %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:2, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")

####################################### Extreme Gradient Boosting Final Model

# Removing some data objects to free up memory
rm(test_set, train_set, test_set_dtm, train_set_dtm)

# Loading in the edx and validation sets
edx <- readRDS(file = "RDS/edx.RDS")
validation <- readRDS(file = "RDS/validation.RDS")
edx_dtm <- readRDS(file = "RDS/edx_dtm.RDS")
validation_dtm <- readRDS(file = "RDS/validation_dtm.RDS")

# Tictoc package used for timing model training
tic()
set.seed(1, sample.kind="Rounding")
train_xgb_final <- xgboost(data = as.matrix(edx_dtm), 
                           label = edx$fake, 
                           nrounds = 100,
                           eta = 0.3, 
                           gamma = 0, 
                           max_depth = 6, 
                           min_child_weight = 1, 
                           subsample = 1, 
                           colsample_bytree = 1,
                           eval_metric = "error",
                           objective = "binary:logistic",
                           nthread = 3)
toc()

# Get training column order to use in the prediction
# I had an issue where the columns not being in the 
# same order was throwing an error.
col_order <- colnames(edx_dtm)

# predict on validation set
pred_xgb_final <- predict(train_xgb_final, as.matrix(validation_dtm)[, col_order])

# if predicted value > 0.5 then its a 1 (fake news) otherwise its not fake news
pred_xgb_final <- ifelse(pred_xgb_final > 0.5, 1, 0)

# calculate confusion matrix
cm_xgb_final <- confusionMatrix(factor(pred_xgb_final), factor(validation$fake))

# Store F1 score
F1_xgb_final <- cm_xgb_final[["byClass"]][["F1"]]

accuracy_results <- bind_rows(accuracy_results,
                              tibble(Method = "XGB_final",
                                     F1_Score = F1_xgb_final))

accuracy_results %>% knitr::kable() %>% 
  kable_styling(latex_options=c("striped")) %>% 
  column_spec(1:2, color = "black") %>% 
  row_spec(0, color = "white", background = "#5a5cd6")
