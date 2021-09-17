###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
#originally provided code: summary_df = do.call(cbind, lapply(mydata, summary))
#this fails, so provide new solution (albeit more inefficient)

#drop variables that only have one value (e.g. filtered)
mydata_1 <-select(mydata, "Year", "Value", "ViewBy", "ViewBy2")
summary(mydata_1)

#column names
colnames_list <- c("Year", "Value", "Age Category", "Serotype Category")

#row names
rownames_list <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")

#values
Year <- c(1997, 2002, 2008, 2008, 2014, 2019)
Value <- c(0.00, 0.80, 9.90, 11.41, 17.12, 71.60)
ViewBy <- c(184, "character", "character", 184, "character", "character")
ViewBy2 <- c(115, "character", "character", 115, "character", "character")

summary_df <- data.frame(Year, Value, ViewBy, ViewBy2)
rownames(summary_df) <- rownames_list
colnames(summary_df) <- colnames_list

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

#subset data to overall category
mydata_overall <- mydata[mydata$ViewBy == "Overall", ]

#make a scatterplot of overall category by serotype
#also add a linear regression line to it
p1 <- mydata_overall %>% 
  ggplot(aes(x=Year, y=Value, color = factor(ViewBy2))) +
  geom_point() +
  geom_smooth(method='lm')

#look at overall by serotype figure
plot(p1)

#save figure
figure_file1 = here("results","resultfigure1.png")
ggsave(filename = figure_file1, plot=p1) 

#subset data by infants
mydata_infants <- mydata[mydata$ViewBy == "Infants, early-onset disease", ]

#make a scatterplot of infant category by serotype
#also add a linear regression line to it
p2 <- mydata_infants %>% 
  ggplot(aes(x=Year, y=Value, color = factor(ViewBy2))) +
  geom_point() +
  geom_smooth(method='lm')

#look at infants by serotype figure
plot(p2)

#save figure
figure_file2 = here("results","resultfigure2.png")
ggsave(filename = figure_file2, plot=p2) 

#subset data by adults
mydata_adults <- mydata[mydata$ViewBy == "Adults, 18-64 years old", ]

#make a scatterplot of adult category by serotype
#also add a linear regression line to it
p3 <- mydata_adults %>% 
  ggplot(aes(x=Year, y=Value, color = factor(ViewBy2))) +
  geom_point() +
  geom_smooth(method='lm')

#look at adults by serotype figure
plot(p3)

#save figure
figure_file3 = here("results","resultfigure3.png")
ggsave(filename = figure_file3, plot=p3) 

#subset data by older adults
mydata_olderadults <- mydata[mydata$ViewBy == "Adults, ≥ 65 years old", ]

#make a scatterplot of older adult category by serotype
#also add a linear regression line to it
p4 <- mydata_olderadults %>% 
  ggplot(aes(x=Year, y=Value, color = factor(ViewBy2))) +
  geom_point() +
  geom_smooth(method='lm')

#look at adults by serotype figure
plot(p4)

#save figure
figure_file4 = here("results","resultfigure4.png")
ggsave(filename = figure_file4, plot=p4) 

######################################
#Data fitting/statistical analysis
######################################

# fit linear model for overall
lmfit_overall <- lm(Value ~ Year, mydata_overall)  

# place results from fit into a data frame with the tidy function
lmtable_overall <- broom::tidy(lmfit_overall)

#look at fit results
print(lmtable_overall)

# save fit results table  
table_file1 = here("results", "resulttable_overall.rds")
saveRDS(lmtable_overall, file = table_file1)

# fit linear model for infants
lmfit_infants <- lm(Value ~ Year, mydata_infants)  

# place results from fit into a data frame with the tidy function
lmtable_infants <- broom::tidy(lmfit_infants)

#look at fit results
print(lmtable_infants)

# save fit results table  
table_file2 = here("results", "resulttable_infants.rds")
saveRDS(lmtable_infants, file = table_file2)

# fit linear model for adults
lmfit_adults <- lm(Value ~ Year, mydata_adults)  

# place results from fit into a data frame with the tidy function
lmtable_adults <- broom::tidy(lmfit_adults)

#look at fit results
print(lmtable_adults)

# save fit results table  
table_file3 = here("results", "resulttable_adults.rds")
saveRDS(lmtable_adults, file = table_file3)

# fit linear model for older adults
lmfit_olderadults <- lm(Value ~ Year, mydata_olderadults)  

# place results from fit into a data frame with the tidy function
lmtable_olderadults <- broom::tidy(lmfit_olderadults)

#look at fit results
print(lmtable_olderadults)

# save fit results table  
table_file4 = here("results", "resulttable_olderadults.rds")
saveRDS(lmtable_olderadults, file = table_file4)
  