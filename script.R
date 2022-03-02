# =============================================================================.
# Predict classic cars prices
# =============================================================================.
# trying to predict classic cars prices using machine learning
# =============================================================================.

# Initialization ----
rm(list=ls()); gc()
start_time <- Sys.time()

# user input:
# set TRUE if you want to scrape data (takes some time)
# set FALSE if you want to download already scraped data from github
scrape_data <- F

# packages used
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(rvest)
library(dplyr)
library(data.table)
library(caret)
library(randomForest)

# set seed (please uncomment depending on your R Version)
# if using R 3.6 or later:
set.seed(1, sample.kind = "Rounding")
# if using R 3.5 or earlier:
# set.seed(1)

# functions
get_vehicle_information <- function(link){
  page <- read_html(link)
  
  info <- page %>% html_nodes(".vehicle-details-accordion-content") %>% html_table()
  tab_1 <- info[[1]] %>% as.data.table() %>% transpose() %>% setnames(as.character(.[1,])) %>% .[-1,]
  tab_2 <- info[[2]] %>% as.data.table() %>% transpose() %>% setnames(as.character(.[1,])) %>% .[-1,]
  
  price <- page %>% html_nodes(".filter-by-currency-code") %>% html_text()
  
  if(length(price) == 0){
    price <- page %>% html_nodes(".vehicle-ad-price-on-request") %>% html_text() %>% unique()
    if(length(price == 1)){tab_3 <- data.table(price = NA)}
    
  } else {
    price <- price[str_detect(price, "EUR")] %>% gsub(",", "", .) %>% 
      str_extract_all(pattern = "[\\.0-9e-]+", simplify = T) %>% unique() %>%
      as.numeric()
    if(length(price) != 1){stop(link)}
    tab_3 <- data.table(price = price)
  }
  
  tab <- cbind(ds1=tab_1, ds2=tab_2, tab_3)
  return(tab)
}

# objects
data <- data.table()

results <- data.table(
  model = character(),
  RMSE = numeric(),
  MAE = numeric(),
  MAPE = numeric()
)



# =============================================================================.
# 1. Data pre-processing ----
# =============================================================================.

# -----------------------------------------------------------------------------.
# 1.1 Download Data / perform web scraping ----
# -----------------------------------------------------------------------------.

if(scrape_data){
  
  cat("\nScrape data from classic-trader.com...")
  
  # scrape data
  base_link <- "https://www.classic-trader.com/uk/cars/search?fulltext=&pagination%5Bpage%5D="
  
  # get number of pages we want to scrape
  pages <- read_html(paste0(base_link, 1)) %>%
    html_nodes(".pager-text") %>%
    html_text() %>%
    str_extract_all(pattern = "[\\.0-9e-]+", simplify = T) %>%
    as.numeric()
  
  for(page in pages[1]:pages[2]){

    # create link for specific web page
    link <- paste0(base_link, page)
    
    # get all items of page
    vehicle_links <- read_html(link) %>%
      html_nodes(".result-name") %>%
      html_attr("href") %>%
      file.path("https://www.classic-trader.com", .)
    
    # scrape data from links
    vehicles <- sapply(vehicle_links, FUN = get_vehicle_information)
    
    # bind rows to temporary data.table
    temp_dt <- rbindlist(vehicles, fill = T)
    
    # bind temporary data.table to permanent data.table
    data <- rbind(data, temp_dt, fill = T)
    
    # track progress
    cat('\rPage', page, "of", pages[2], "scraped")
    flush.console()
    
  }
  
  cat("\nScraping successfully completed!")
  
  # clean up
  rm(temp_dt, vehicles, vehicle_links, base_link, link, page, pages)
  
} else {
  
  cat("\nDownload dataset via github...")
  
  # download "pre-scraped" data from github
  url <- "https://raw.githubusercontent.com/bt-koch/ML-project-classic-trader/main/scraped-data.csv"
  data <- read.csv(url, check.names = F)
  rm(url)
  
  cat(" download successful!")
}

# clean up
rm(scrape_data)

# notes for github upload ++++++++++++++++++++++++++++++++++++++++++++++++++++++
# save csv for github upload
# write.csv(data, "scraped-data.csv", row.names = F)
# write following commands in terminal (check wd!):
# git commit -m "message" filename
# git push
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# -----------------------------------------------------------------------------.
# 1.2 Data Wrangling ----
# -----------------------------------------------------------------------------.
cat("\nPrepare data...")

# filter relevant data and perform simple manipulations
data <- data %>%
  select(
    ID = `ds2.Vehicle-ID:`,
    manufacturer = ds1.Make,
    model = ds1.Model,
    model_name = `ds1.Model name`,
    year = `ds1.Year of manufacture`,
    mileage = `ds1.Mileage (read)`,
    body_style = `ds1.Body style`,
    power = `ds1.Power (kw/hp)`,
    ccm = `ds1.Cubic capacity (ccm)`,
    cylinders = ds1.Cylinders,
    steering = ds1.Steering,
    transmission = ds1.Transmission,
    drive = ds1.Drive,
    fuel = ds1.Fuel,
    color = `ds1.Exterior colour`,
    condition = `ds1.Condition category`,
    price = price
  ) %>%
  mutate(
    # extract power in kw as integer
    power = as.integer(str_extract(power, pattern = "[\\.0-9e-]+")),
    # extract mileage and convert to km as numeric
    mileage_num = gsub(",", "", mileage),
    mileage_num = as.integer(str_extract(mileage_num, pattern = "[\\.0-9e-]+")),
    mileage_unit = str_extract(mileage, pattern = "[a-zA-Z]+"),
    mileage_unit = ifelse(mileage_unit == "km", 1, 1.60934),
    mileage = mileage_num*mileage_unit,
    # extract cubic capacity (ccm) as integer
    ccm = gsub(",", "", ccm),
    ccm = as.integer(str_extract(ccm, pattern = "[\\.0-9e-]+")),
    # create decade
    decade = (year-1)-(year-1)%%10,
    # year as factor
    year = as.factor(year),
    # decade as factor
    decade = as.factor(decade),
    # number of cylinders as factor
    cylinders = as.factor(cylinders),
    # manufacturer as factor
    manufacturer = as.factor(manufacturer),
    # model as factor
    model = as.factor(model),
    # body style as factor
    body_style = as.factor(body_style),
    # steering as factor
    steering = as.factor(steering),
    # trainsmission as factor
    transmission = as.factor(transmission),
    # drive as factor
    drive = as.factor(drive),
    # fuel as factor
    fuel = as.factor(fuel)
  ) %>%
  select(
    # clean up
    -c(mileage_num, mileage_unit)
  )

data_raw <- copy(data)

# analyze missing data
missing_data <- data %>%
  sapply(function(x) sum(is.na(x))) %>%
  data.frame(na_count = .) %>%
  mutate(na_perc = na_count/nrow(data))

# extract rows with at least one missing value
index <- apply(data, 1, function(x) anyNA(x))
rows_with_na <- data[index,]
rows_without_na <- data[!index,]

# handle nas
data <- data %>%
  mutate(
    # assume condition = "Original" when there is no further information
    condition = ifelse(is.na(condition), "Original", condition),
    condition = as.factor(condition)
  ) %>%
  select(
    # remove columns with too much missing values
    -c(mileage, color)
  )

# extract rows with at least one missing value
index <- apply(data, 1, function(x) anyNA(x))
rows_with_na <- data[index,]
rows_without_na <- data[!index,]

# data without any missing values
data <- copy(rows_without_na)

# clean up
rm(rows_with_na, rows_without_na, index)


# -----------------------------------------------------------------------------.
# 1.3 Create development and validation set ----
# -----------------------------------------------------------------------------.

# create development and validation set
validation_index <- createDataPartition(data$price, times = 1, p = 0.1, list = F)
validation_set <- data[validation_index,]
development_set <- data[-validation_index,]

rm(validation_index)

# =============================================================================.
# 2. Analyze the data ----
# =============================================================================.

# -----------------------------------------------------------------------------.
# 2.1 Prices ----
# -----------------------------------------------------------------------------.

# distribution without any manipulations
dens <- density(development_set$price)
hist(development_set$price, probability = T, breaks = 25, ylim = c(0, max(dens$y)))
lines(dens)
rm(dens)

# frequency of prices in price ranges
seq <- c(
  seq(0, 20000, by = 5000),
  seq(25000, 225000, by = 25000),
  seq(250000, 10^6, by = 250000),
  Inf
)
prices <- cut(development_set$price, breaks = seq)
barplot(table(prices))
rm(seq, prices)

# distribution with logarithmic transformation
hist(log(development_set$price), probability = T, breaks = 25)
lines(density(log(development_set$price)))

# -----------------------------------------------------------------------------.
# 2.2 Manufacturer ----
# -----------------------------------------------------------------------------.

# count number of vehicles per manufacturer
manufac <- table(development_set$manufacturer) %>%
  as.data.frame() %>%
  arrange(desc(Freq))

# get top manufacturers in data set
manufac_top <- manufac[1:15,]$Freq
names(manufac_top) <- manufac[1:15,]$Var1
manufac_top <- sort(manufac_top)

# make barplot of top manufacturers
par(mar = c(5.1, 7, 4.1, 2.1))
barplot(manufac_top, horiz = T, las=1)

# top 53 manufacturers (for filtering later)
manufac_top_53 <- manufac[1:53,]

# clean up
rm(manufac, manufac_top)

# -----------------------------------------------------------------------------.
# 2.3 Year ----
# -----------------------------------------------------------------------------.

# distribution of year
barplot(table(development_set$year))

# -----------------------------------------------------------------------------.
# 2.3 Power ----
# -----------------------------------------------------------------------------.

# no manipulation
dens <- density(development_set$power)
hist(development_set$power, probability = T, breaks = 25, ylim = c(0, max(dens$y)))
lines(dens)

# log transformation
hist(log(development_set$power), probability = T, breaks = 25)
lines(density(log(development_set$power)))

# square root transformation
hist(development_set$power**(1/2), probability = T, breaks = 25)
lines(density(development_set$power**(1/2)))

# reciprocal transformation
hist(1/development_set$power, probability = T, breaks = 25)
lines(density(1/development_set$power))

# box-cox transformation
bxcx <- BoxCoxTrans(development_set$power)
lambda_power <- bxcx$lambda
hist((development_set$power^lambda_power - 1)/lambda_power, probability = T, breaks = 25)
lines(density((development_set$power^lambda_power - 1)/lambda_power))

# clean up
rm(dens, bxcx)

# -----------------------------------------------------------------------------.
# 2.3 ccm ----
# -----------------------------------------------------------------------------.

# no manipulation
dens <- density(development_set$ccm)
hist(development_set$ccm, probability = T, breaks = 25, ylim = c(0, max(dens$y)))
lines(dens)

# log transformation
hist(log(development_set$ccm), probability = T, breaks = 25)
lines(density(log(development_set$ccm)))

# square root transformation
hist(development_set$ccm**(1/2), probability = T, breaks = 25)
lines(density(development_set$ccm**(1/2)))

# reciprocal transformation
hist(1/development_set$ccm, probability = T, breaks = 25)
lines(density(1/development_set$ccm))

# box-cox transformation
bxcx <- BoxCoxTrans(development_set$ccm)
lambda_ccm <- bxcx$lambda
hist((development_set$ccm^lambda_ccm - 1)/lambda_ccm, probability = T, breaks = 25)
lines(density((development_set$ccm^lambda_ccm - 1)/lambda_ccm))

# clean up
rm(dens, bxcx)

# -----------------------------------------------------------------------------.
# 2.4 manipulate features ----
# -----------------------------------------------------------------------------.

# make copy of development set (for EDA in report)
development_set_raw <- copy(development_set)

# manipulate features and further filtering
data <- data %>%
  filter(
    manufacturer %in% manufac_top_53$Var1 &
      as.numeric(as.character(year)) > 1945 &
      ccm >= 400 &
      as.numeric(as.character(cylinders)) >= 2
  ) %>%
  mutate(
    price = log(price),
    power = (power^lambda_power - 1)/lambda_power,
    ccm   = (ccm^lambda_ccm - 1)/lambda_ccm
  ) %>%
  mutate(
    manufacturer = as.factor(as.character(manufacturer))
  ) %>%
  select(
    -c(model, model_name, year)
  )

# split data again (necessary due to manipulations)
validation_index <- createDataPartition(data$price, times = 1, p = 0.1, list = F)
validation_set <- data[validation_index,]
development_set <- data[-validation_index,]

# keep IDs to identify cars with biggest difference when predicting
IDs_validation_set <- validation_set$ID
validation_set$ID  <- NULL

IDs_development_set <- development_set$ID
development_set$ID  <- NULL

rm(validation_index)


# -----------------------------------------------------------------------------.
# 2.5 one-hot encoding ----
# -----------------------------------------------------------------------------.

# perform one-hot encoding
dummy <- dummyVars(" ~ .", data = development_set)
development_set_one_hot <- data.table(predict(dummy, newdata = development_set))

dummy <- dummyVars(" ~ .", data = validation_set)
validation_set_one_hot <- data.table(predict(dummy, newdata = validation_set))
  
# clean up
rm(dummy)

# -----------------------------------------------------------------------------.
# 2.6 create train and test set ----
# -----------------------------------------------------------------------------.

# create train and test set
test_index <- createDataPartition(development_set$price, times = 1, p = 0.1, list = F)
test_set <- development_set[test_index,]
train_set <- development_set[-test_index,]
test_set_one_hot <- development_set_one_hot[test_index,]
train_set_one_hot <- development_set_one_hot[-test_index,]

# clean up
rm(test_index)

cat(" done!") # data preparation finished

# =============================================================================.
# 3. Develop the model ----
# =============================================================================.

# -----------------------------------------------------------------------------.
# 3.1 Linear Regression ----
# -----------------------------------------------------------------------------.
cat("\nTrain linear model...")

# 3.1.1 train ---- 
start_time <- Sys.time()
cat("training lm started:", format(start_time, "%h.%m %H:%M"))

train_lm <- lm(price ~ ., data = train_set)

end_time <- Sys.time()
end_time - start_time

# 3.1.2 analyze ----
summary(train_lm)
# check the default plots -> plot(train_lm, 1) ...

# 3.1.3 predict ----
# problem: when new factor in test set, prediction is not possible
# -> remove observation with new factors
test_set_lm <- copy(test_set)
index <- which(!(test_set_lm$manufacturer %in% train_set$manufacturer))
test_set_lm$manufacturer[index] <- NA

pred_lm <- predict(train_lm, test_set_lm)

# convert back to normal scale
pred_lm   <- exp(pred_lm)
true_vals <- exp(test_set_lm$price)

# 3.1.4 evaluate ----
rmse <- RMSE(pred = pred_lm, obs = true_vals, na.rm = T)
mae  <- MAE(pred = pred_lm, obs = true_vals, na.rm = T)
mape <- mean(abs((true_vals-pred_lm)/true_vals), na.rm = T) * 100

temp <- data.table(
  model = "linear regression",
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

results <- rbind(results, temp)

# clean up
rm(start_time, end_time, index, rmse, mae, mape, temp)


# -----------------------------------------------------------------------------.
# 3.2 Random Forest ----
# -----------------------------------------------------------------------------.
cat("\nTrain random forest...")

# 3.2.1 train ----
start_time <- Sys.time()
cat("training rf started:", format(start_time, "%h.%m %H:%M\n"))

# tune parameters
mtry <- tuneRF(x = train_set[names(train_set) != "price"],
               y = train_set$price,
               ntreeTry=500,
               stepFactor=1.5,
               improve=0.01)

best.m <- mtry[mtry[,2] == min(mtry[,2]), 1]

# train model
train_rf <- randomForest(x = train_set[names(train_set) != "price"],
                         y = train_set$price,
                         mtry = best.m)

hist(treesize(train_rf))

end_time <- Sys.time()
end_time - start_time

# 3.2.2 analyze ----
print(train_rf)
varImp(train_rf)

# 3.2.3 predict ----
pred_rf <- predict(train_rf, test_set)

# convert back to normal scale
pred_rf   <- exp(pred_rf)
true_vals <- exp(test_set$price)

# 3.2.4 evaluate ----
rmse <- RMSE(pred = pred_rf, obs = true_vals, na.rm = T)
mae  <- MAE(pred = pred_rf, obs = true_vals, na.rm = T)
mape <- mean(abs((true_vals-pred_rf)/true_vals), na.rm = T) * 100

temp <- temp <- data.table(
  model = "random forest",
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

results <- rbind(results, temp)

# clean up
rm(start_time, end_time, rmse, mae, mape, temp)


# -----------------------------------------------------------------------------.
# 3.3 K-nearest Neighbors ----
# -----------------------------------------------------------------------------.
cat("\nTrain knn...")

# 3.3.1 train ----
start_time <- Sys.time()
cat("training knn started:", format(start_time, "%h.%m %H:%M"))

ctrl <- trainControl(method="repeatedcv",
                     repeats = 3)

train_knn <- train(price ~ ., data = train_set_one_hot, method = "knn",
                   trControl = ctrl, tuneLength = 20) 

end_time <- Sys.time()
end_time - start_time

# 3.3.2 analyze ----
print(train_knn)
varImp(train_knn)

# 3.3.3 predict ----
pred_knn <- predict(train_knn, test_set_one_hot)

# convert back to normal scale
pred_knn  <- exp(pred_knn)
true_vals <- exp(test_set$price)

# 3.3.4 evaluate ----
rmse <- RMSE(pred = pred_knn, obs = true_vals, na.rm = T)
mae  <- MAE(pred = pred_knn, obs = true_vals, na.rm = T)
mape <- mean(abs((true_vals-pred_knn)/true_vals), na.rm = T) * 100

temp <- temp <- data.table(
  model = "K-nearest neighbors",
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

results <- rbind(results, temp)

# clean up
rm(start_time, end_time, rmse, mae, mape, temp)


# -----------------------------------------------------------------------------.
# 3.4 Ensemble Model (averaging all models) ----
# -----------------------------------------------------------------------------.
cat("\nImplement the ensemble model...")

# 3.4.1 predict (averaging) ----
pred_avg <- data.table(lm = pred_lm, rf = pred_rf, knn = pred_knn)
pred_avg <- rowMeans(pred_avg)

true_vals <- exp(test_set$price)

# 3.4.2 evaluate ----
rmse <- RMSE(pred = pred_avg, obs = true_vals, na.rm = T)
mae  <- MAE(pred = pred_avg, obs = true_vals, na.rm = T)
mape <- mean(abs((true_vals-pred_avg)/true_vals), na.rm = T) * 100

temp <- data.table(
  model = "ensemble model (average)",
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

results <- rbind(results, temp)

# clean up
rm(rmse, mae, mape, temp)

# -----------------------------------------------------------------------------.
# 3.5 Ensemble Model (averaging all models) ----
# -----------------------------------------------------------------------------.
cat("\nImplement the ensemble model (without linear model)...")

# 3.5.1 predict (averaging) ----
pred_avg_2 <- data.table(rf = pred_rf, knn = pred_knn)
pred_avg_2 <- rowMeans(pred_avg_2)

true_vals <- exp(test_set$price)

# 3.4.2 evaluate ----
rmse <- RMSE(pred = pred_avg_2, obs = true_vals, na.rm = T)
mae  <- MAE(pred = pred_avg_2, obs = true_vals, na.rm = T)
mape <- mean(abs((true_vals-pred_avg_2)/true_vals), na.rm = T) * 100

temp <- data.table(
  model = "ensemble model (average, without linear model)",
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

results <- rbind(results, temp)

# clean up
rm(rmse, mae, mape, temp)


# =============================================================================.
# 4. Apply the model ----
# =============================================================================.
cat("\nApply the model...")

# -----------------------------------------------------------------------------.
# 4.1 apply random forest ----
# -----------------------------------------------------------------------------.

# 4.1.1 make predictions ----
pred_final_rf <- predict(train_rf, validation_set)

# convert back to normal scale
pred_final_rf <- exp(pred_final_rf)
true_vals  <- exp(validation_set$price)

# 4.1.2 evaluate model ----
rmse <- RMSE(pred = pred_final_rf, obs = true_vals, na.rm = T)
mae  <- MAE(pred = pred_final_rf, obs = true_vals, na.rm = T)
mape <- mean(abs((true_vals-pred_final_rf)/true_vals), na.rm = T) * 100

temp <- data.table(
  model = "random forest (validation set)",
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

results <- rbind(results, temp)

analyze_results_final_rf <- data.table(
  ID   = IDs_validation_set,
  pred = pred_final_rf,
  true = true_vals,
  diff = abs(round((pred_final_rf-true_vals)/true_vals*100, 2))
)

hist(analyze_results_final_rf$diff, breaks = 50)

# clean up
rm(rmse, mae, mape, temp)

# -----------------------------------------------------------------------------.
# 4.2 apply ensemble model ----
# -----------------------------------------------------------------------------.

# 4.2.1 make predictions ----
pred_final_knn <- predict(train_knn, validation_set_one_hot)

# convert back to normal scale
pred_final_knn <- exp(pred_final_knn)
true_vals  <- exp(validation_set$price)

# averaging
pred_final_avg_2 <- data.table(rf = pred_final_rf, knn = pred_final_knn)
pred_final_avg_2 <- rowMeans(pred_final_avg_2)

# 4.2.2 evaluate model ----
rmse <- RMSE(pred = pred_final_avg_2, obs = true_vals, na.rm = T)
mae  <- MAE(pred = pred_final_avg_2, obs = true_vals, na.rm = T)
mape <- mean(abs((true_vals-pred_final_avg_2)/true_vals), na.rm = T) * 100

temp <- data.table(
  model = "ensemble model without linear model (validation set)",
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

analyze_results_final_avg_2 <- data.table(
  ID   = IDs_validation_set,
  pred = pred_final_avg_2,
  true = true_vals,
  diff = abs(round((pred_final_avg_2-true_vals)/true_vals*100, 2))
)

hist(analyze_results_final_avg_2$diff, breaks = 60)

results <- rbind(results, temp)


# =============================================================================.
# 5. Closing ----
# =============================================================================.
cat("\nSave data for report...")

# save data for report
save(list = c("data", "data_raw", "development_set_raw", "results", "missing_data",
              "train_lm", "train_rf", "train_knn", "lambda_power", "lambda_ccm",
              "mtry", "best.m", "analyze_results_final_rf", "analyze_results_final_avg_2"),
     file = "rmd-input.RData")

# final message and running time
message("\nScript successfully finished")
end_time <- Sys.time()
end_time - start_time
