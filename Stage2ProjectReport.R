## -----------------------------------------------------------------------------
library(dplyr)
library(car)
library(ggplot2)
library(rmarkdown)


## -----------------------------------------------------------------------------
dataset_url <- "https://raw.githubusercontent.com/wilkmzhou/STAT306_PROJECT_REPORT_2026/refs/heads/main/mercedes_benz_listings_cleaned.csv" # load dataset from github

benzDataRaw <- read.csv(dataset_url)                                                                                                        # read dataset
head(benzDataRaw, n = 3)


## -----------------------------------------------------------------------------
benzData <- benzDataRaw |> 
    na.omit() |>                                                                                              # remove any NA rows 
    select(-Price_Per_Mile, -Price_Category, -Year, -Mileage_Category, -Vehicle_Name, -Model_Series, -Is_AMG) |> # remove redundant variables
    select(-Trim_Level, -Is_4MATIC)                                                                           # remove uninterested variables
head(benzData, n = 3)


## -----------------------------------------------------------------------------
benzDataInt <- benzData |>
    select(-Body_Type) # filter dataset from categorical variables

pairs(benzDataInt) # scatterplot between each pair of numerical variables
cor(benzDataInt) # correlation between each pair of numerical variables


## -----------------------------------------------------------------------------
benzDataTransf <- benzDataInt |>
    transmute(log_Vehicle_Age = log(Vehicle_Age), log_Mileage_Miles = log(Mileage_Miles), log_Price_USD = log(Price_USD)) # transform variables 

pairs(benzDataTransf)                                                                                                     # find correlation between variables
cor(benzDataTransf)


## -----------------------------------------------------------------------------
options(repr.plot.width = 8, repr.plot.height = 5)
benzDataCat <- benzData |>
    select(-Vehicle_Age, -Mileage_Miles) # filter dataset from numerical variables

benzBodyBoxplot <- benzDataCat |>
    ggplot(aes(x = Body_Type, y = Price_USD)) + 
    geom_boxplot() + 
    ylab("Price in USD") + # y-axis label change 
    xlab("Vehicle Body Type") + # x-axis label change
    ggtitle("Vehicle Price and Body Type Boxplot") + # title
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), text = element_text(size = 12)) # title alignment and text size increase

benzBodyBoxplot


## -----------------------------------------------------------------------------
benzFit1 <- lm(Price_USD ~ ., data = benzData) # fit linear regression model with all variables in filtered dataset

summary(benzFit1) # print linear regression fit statistics


## -----------------------------------------------------------------------------
vifBenz <- vif(benzFit1)
vifBenz


## -----------------------------------------------------------------------------
options(repr.plot.width = 6, repr.plot.height = 5)
plot(fitted(benzFit1), resid(benzFit1), main = "Residual Plot with Fitted Values from Fit1", xlab = "Fitted Values", ylab = "Residuals") # plot fitted values and residuals
abline(0,0)                                                                                                                              # add horizontal line at 0 


## -----------------------------------------------------------------------------
benzFit2 <- lm(log(Price_USD) ~ log(Vehicle_Age) + log(Mileage_Miles) + Body_Type, data = benzData) # fit linear regression with transformed variables

summary(benzFit2)


## -----------------------------------------------------------------------------
plot(fitted(benzFit2), resid(benzFit2), main = "Residual Plot with Fitted Values from Fit1", xlab = "Fitted Values", ylab = "Residuals") # plot fitted values and residuals
abline(0,0)  


## -----------------------------------------------------------------------------
vifBenzFit2 <- vif(benzFit2) # compute vif of fit with transformed variables

vifBenzFit2 


## -----------------------------------------------------------------------------
resFit1 <- resid(benzFit1)
resFit2 <- resid(benzFit2)

qqnorm(resFit2) # display QQ plot of linear regression with transformed variables 
qqline(resFit2)

qqnorm(resFit1) # display QQ plot of linear regression with no transformed variables
qqline(resFit1)


## -----------------------------------------------------------------------------
rmarkdown::convert_ipynb("Stage2ProjectReport.ipynb") 


## -----------------------------------------------------------------------------
knitr::purl("Stage2ProjectReport.Rmd")

