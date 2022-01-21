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
scrape_data <- T

# packages used
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(rvest)
library(dplyr)
library(data.table)
library(caret)

# functions
get_vehicle_information <- function(link){
  page <- read_html(link)
  
  info <- page %>% html_nodes(".vehicle-details-accordion-content") %>% html_table()
  tab_1 <- info[[1]] %>% as.data.table() %>% transpose() %>% setnames(as.character(.[1,])) %>% .[-1,]
  tab_2 <- info[[2]] %>% as.data.table() %>% transpose() %>% setnames(as.character(.[1,])) %>% .[-1,]
  
  price <- page %>% html_nodes(".filter-by-currency-code") %>% html_text()
  
  if(length(price) == 0){
    price <- page %>% html_nodes(".vehicle-ad-price-on-request") %>% html_text() %>% unique()
    if(length(price == 1)){tab_3 <- data.table(price = -999)}
    
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



# =============================================================================.
# 1. Data pre-processing ----
# =============================================================================.

# -----------------------------------------------------------------------------.
# 1.1 Download Data / perform web scraping ----
# -----------------------------------------------------------------------------.

if(scrape_data){
  
  base_link <- "https://www.classic-trader.com/uk/cars/search?fulltext=&pagination%5Bpage%5D="
  
  pages <- read_html(paste0(base_link, 1)) %>%
    html_nodes(".pager-text") %>%
    html_text() %>%
    str_extract_all(pattern = "[\\.0-9e-]+", simplify = T) %>%
    as.numeric()
  
  for(page in pages[1]:pages[2]){
    # for(page in 30:50){
    
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
  
  # clean up
  rm(temp_dt, vehicles, vehicle_links, base_link, link, page, pages)
  
} else {
  
  # download data "pre-scraped" by me from github
  # url <- "github.com/bt-koch/test/blob/master/scraped_data.rds"
  # temp <- tempfile()
  # download.file(url, temp)
  # dat <- load(temp)
  # unlink(temp)
  
  url <- "https://raw.githubusercontent.com/bt-koch/test/master/scraped-data.csv"
  data <- read.csv(url, check.names = F)
  
}


test <- readRDS("scraped-data.rds")
# temporary:
# saveRDS(data, "scraped-data.rds")
# write.csv(data, "scraped-data.csv", row.names = F)


# -----------------------------------------------------------------------------.
# 1.2 Data Wrangling ----
# -----------------------------------------------------------------------------.
saved <- copy(data)

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
    condition = `ds1.Condition category`
  ) %>%
  mutate(
    power = as.integer(str_extract(power, pattern = "[\\.0-9e-]+")),
    mileage_num = gsub(",", "", mileage),
    mileage_num = as.integer(str_extract(mileage_num, pattern = "[\\.0-9e-]+")),
    mileage_unit = str_extract(mileage, pattern = "[a-zA-Z]+"),
    mileage_unit = ifelse(mileage_unit == "km", 1, 1.60934),
    mileage = mileage_num*mileage_unit,
    ccm = gsub(",", "", ccm),
    ccm = as.integer(str_extract(ccm, pattern = "[\\.0-9e-]+")),
    condition = ifelse(is.na(condition), "normal", condition)
  ) %>%
  select(
    -c(mileage_num, mileage_unit)
  )

# missing data -> group by manufacturer+model and predict whats most expected
# test

# -----------------------------------------------------------------------------.
# 1.3 Create train, test and validation set ----
# -----------------------------------------------------------------------------.

validation_index <- createDataPartition(data$ID, times = 1, p = 0.1, list = F)
validation_set <- data[validation_index,]
development_set <- data[-validation_index,]

# =============================================================================.
# 2. Develop the model ----
# =============================================================================.

# to do

# =============================================================================.
# 3. Apply the model ----
# =============================================================================.
