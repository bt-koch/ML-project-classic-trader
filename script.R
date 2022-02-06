# =============================================================================.
# Predict classic cars prices
# =============================================================================.
# trying to predict classic cars prices using machine learning
# =============================================================================.

# Initialization ----
rm(list=ls()); gc()

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

rm(scrape_data)

# save csv for github upload
# write.csv(data, "scraped-data.csv", row.names = F)
# write following commands in terminal (check wd!):
# git commit -m "message" filename
# git push

# -----------------------------------------------------------------------------.
# 1.2 Data Wrangling ----
# -----------------------------------------------------------------------------.
cat("\nPrepare data...")

saved <- copy(data) # delete when script is finished!

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
    # assume condition = "Original" when there is no further information
    condition = ifelse(is.na(condition), "Original", condition),
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
    fuel = as.factor(fuel),
    # condition as factor
    condition = as.factor(condition)
  ) %>%
  select(
    # clean up
    -c(mileage_num, mileage_unit)
  )

# analyze missing data
missing_data <- data %>%
  sapply(function(x) sum(is.na(x))) %>%
  data.frame(na_count = .) %>%
  mutate(na_perc = na_count/nrow(data))

# extract rows with at least one missing value
index <- apply(data, 1, function(x) anyNA(x))
rows_with_na <- data[index,]
rows_without_na <- data[!index,]

# remove columns with too much missing values
data <- data %>% select(-c(mileage, color))

# extract rows with at least one missing value
index <- apply(data, 1, function(x) anyNA(x))
rows_with_na <- data[index,]
rows_without_na <- data[!index,]

# data without any missing values
data <- copy(rows_without_na)

# save data for report
save(list = c("data"), file = "rmd-input.RData")


# clean up
rm(rows_with_na, rows_without_na, index)


# -----------------------------------------------------------------------------.
# 1.3 Create development and validation set ----
# -----------------------------------------------------------------------------.

data <- data %>% select(-c(ID, model_name))
# data <- data[1:500,]

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

# logarithmic manipulation
hist(log(development_set$price), probability = T, breaks = 25)
lines(density(log(development_set$price)))

# -----------------------------------------------------------------------------.
# 2.2 Manufacturer ----
# -----------------------------------------------------------------------------.

manufac <- table(development_set$manufacturer) %>% as.data.frame() %>% arrange(desc(Freq))

manufac_top <- manufac[1:15,]$Freq
names(manufac_top) <- manufac[1:15,]$Var1
manufac_top <- sort(manufac_top)

par(mar = c(5.1, 7, 4.1, 2.1))
barplot(manufac_top, horiz = T, las=1)

manufac_top_53 <- manufac[1:53,]

rm(manufac, manufac_top)

# -----------------------------------------------------------------------------.
# 2.3 Year ----
# -----------------------------------------------------------------------------.

barplot(table(development_set$year))

# -----------------------------------------------------------------------------.
# 2.3 Power ----
# -----------------------------------------------------------------------------.

# no manipulation
dens <- density(development_set$power)
hist(development_set$power, probability = T, breaks = 25, ylim = c(0, max(dens$y)))
lines(dens)
rm(dens)

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
# 2.x manipulate features  ----
# -----------------------------------------------------------------------------.

development_set <- development_set %>%
  mutate(
    price = log(price),
    power = (power^lambda_power - 1)/lambda_power
  ) %>%
  filter(manufacturer %in% manufac_top_53$Var1) %>%
  select(-c(model, year)) %>%
  mutate(
    manufacturer = as.factor(as.character(manufacturer))
  )

validation_set <- validation_set %>%
  mutate(
    price = log(price),
    power = (power^lambda_power - 1)/lambda_power
  ) %>%
  filter(manufacturer %in% manufac_top_53$Var1) %>%
  select(-c(model, year)) %>%
  mutate(
    manufacturer = as.factor(as.character(manufacturer))
  )

# clean up
rm(lambda_power, lambda_ccm)

# -----------------------------------------------------------------------------.
# 2.x create train and test set ----
# -----------------------------------------------------------------------------.

test_index <- createDataPartition(development_set$price, times = 1, p = 0.1, list = F)
test_set <- development_set[test_index,]
train_set <- development_set[-test_index,]

rm(test_index)

cat(" done!")

# =============================================================================.
# 3. Develop the model ----
# =============================================================================.

# -----------------------------------------------------------------------------.
# 3.1 Linear Regression ----
# -----------------------------------------------------------------------------.
cat("\nTrain linear model...")

# TO DELETE: check https://rpubs.com/j_fachrel/Linear-Regression-In-Used-Car-Price-Prediction <------

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

# index <- which(!(test_set_lm$model %in% train_set$model))
# test_set_lm$model[index] <- NA

pred_lm <- predict(train_lm, test_set_lm)

# 3.1.4 evaluate ----
rmse <- RMSE(pred = pred_lm, obs = test_set_lm$price, na.rm = T)
mae  <- MAE(pred = pred_lm, obs = test_set_lm$price, na.rm = T)
mape <- mean(abs((test_set_lm$price-pred_lm)/test_set_lm$price), na.rm = T) * 100

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

# it takes too long!
# https://stats.stackexchange.com/questions/37370/random-forest-computing-time-in-r

# 3.2.1 train ----
start_time <- Sys.time()
cat("training rf started:", format(start_time, "%h.%m %H:%M"))

# train_rf <- train(price ~ ., data = train_set, method = "rf")
train_rf <- randomForest(x = train_set[names(train_set) != "price"],
                         y = train_set$price)

end_time <- Sys.time()
end_time - start_time

# 3.2.2 analyze ----
print(train_rf)
varImp(train_rf)

# 3.2.3 predict ----
# remove new factors necessary?
pred_rf <- predict(train_rf, test_set)

# 3.2.4 evaluate ----
rmse <- RMSE(pred = pred_rf, obs = test_set$price, na.rm = T)
mae  <- MAE(pred = pred_rf, obs = test_set$price, na.rm = T)
mape <- mean(abs((test_set$price-pred_rf)/test_set$price), na.rm = T) * 100

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

train_knn <- train(price ~ ., data = train_set, method = "knn")

end_time <- Sys.time()
end_time - start_time

# 3.3.2 analyze ----
print(train_knn)
varImp(train_knn)

# 3.3.3 predict ----
pred_knn <- predict(train_knn, test_set)

# 3.3.4 evaluate ----
rmse <- RMSE(pred = pred_knn, obs = test_set$price, na.rm = T)
mae  <- MAE(pred = pred_knn, obs = test_set$price, na.rm = T)
mape <- mean(abs((test_set$price-pred_knn)/test_set$price), na.rm = T) * 100

temp <- temp <- data.table(
  model = "knn",
  RMSE = rmse,
  MAE = mae,
  MAPE = mape
)

results <- rbind(results, temp)

# clean up
rm(start_time, end_time, rmse, mae, mape, temp)

# =============================================================================.
# 4. Apply the model ----
# =============================================================================.
