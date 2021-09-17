###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(tidyverse) #for data tidying
library(dplyr) #for data processing
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths

# Dataset title: Impaired Driving Death Rate, by Age and Gender, 2012 & 2014, All States
# columns except for first two are death rate per 100,000 population
data_location <- here::here("data","raw_data","Active_Bacterial_Core_surveillance__ABCs__Group_B_Streptococcus.csv")

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
rawdata <- read.csv(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#dataset is so small, we can print it to the screen.
#that is often not possible.
print(rawdata)

#summary of raw data
summary(rawdata)

# All data in this variable is the same, but there are two different names. 
## Convert all to group B Streptococcus
rawdata$Bacteria[rawdata$Bacteria == "Group B Streptococcus"] <- "group B Streptococcus"

# Convert variables Units, Topic, ViewBy, and ViewBy2 from Character to Factor
## This will turn them into categorical variables, rather than characters string data
rawdata$Units <- as.factor(rawdata$Units)
rawdata$Topic <- as.factor(rawdata$Topic)
rawdata$ViewBy <- as.factor(rawdata$ViewBy)
rawdata$ViewBy2 <- as.factor(rawdata$ViewBy2)

# Fix factor names in Topic
rawdata$Topic <- fct_recode(rawdata$Topic, `Death Rates` = "Death rates")

summary(rawdata)

# Focus on working with serotypes in this dataset
processeddata <- rawdata %>% 
                 filter(Topic == "Serotypes") %>%
                 filter(Units == "Percent") %>%
                 drop_na()

# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


