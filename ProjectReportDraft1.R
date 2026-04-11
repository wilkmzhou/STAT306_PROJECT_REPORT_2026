## -----------------------------------------------------------------------------
library(dplyr)
library(car)
library(ggplot2)
library(rmarkdown)


## -----------------------------------------------------------------------------
dataset_url <- "https://raw.githubusercontent.com/wilkmzhou/STAT306_PROJECT_REPORT_2026/refs/heads/main/mercedes_benz_listings_cleaned.csv"

benzDataRaw <- read.csv(dataset_url)
head(benzDataRaw, n = 3)


## -----------------------------------------------------------------------------
benzData <- benzDataRaw |> 
    na.omit() |>                                                                                              # remove any NA rows 
    select(-Price_Per_Mile, -Price_Category, -Year, -Mileage_Category, -Vehicle_Name, -Model_Series, -Is_AMG) # remove redundant variables
head(benzData, n = 3)


## -----------------------------------------------------------------------------
benzDataInt <- benzData |>
    select(-Trim_Level, -Body_Type, -Is_4MATIC) # filter dataset from categorical variables

pairs(benzDataInt) # scatterplot between each pair of numerical variables
cor(benzDataInt) # correlation between each pair of numerical variables


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
options(repr.plot.height = 6, repr.plot.width = 15)
benzDataCat$Trim_Level <- factor(benzDataCat$Trim_Level)
benzTrimBoxplot <- benzDataCat |>
    ggplot(aes(x = Trim_Level, y = Price_USD, fill = Trim_Level)) +
    geom_boxplot() +
    labs(x = "Vehicle Trim Level", y = "Price in USD", fill = "Vehicle Trim Level") + # label change
    ggtitle("Vehicle Trim Level on Price in USD Boxplot") + # add title 
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), text = element_text(size = 12)) + # adjustments to title and text size
    scale_x_discrete(breaks = levels(benzDataCat$Trim_Level)[seq(1, 29, by = 2)]) # show x-axis indices every two categories
benzTrimBoxplot


## -----------------------------------------------------------------------------
options(repr.plot.width = 6, repr.plot.height = 5)

benzDataCat$Is_4MATIC <- if_else(benzDataCat$Is_4MATIC == 1, TRUE, FALSE) 

benz4MATICBoxplot <- benzDataCat |>
    ggplot(aes(x = Is_4MATIC, y = Price_USD)) +
    geom_boxplot() + 
    labs(x = "Is the vehicle 4MATIC?", y = "Price in USD") +
    ggtitle("Effect of Vehicle Being 4MATIC on Price in USD") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), text = element_text(size = 12))  # adjustments to title and text size

benz4MATICBoxplot


## -----------------------------------------------------------------------------
benzFit1 <- lm(Price_USD ~ ., data = benzData) # fit linear regression model with all variables in filtered dataset

summary(benzFit1) # print linear regression fit statistics


## -----------------------------------------------------------------------------
vifBenz <- vif(benzFit1)
vifBenz


## -----------------------------------------------------------------------------
options(repr.plot.width = 6, repr.plot.height = 5)
plot(fitted(benzFit1), resid(benzFit1), main = "Residual Plot with Fitted Values", xlab = "Fitted Values", ylab = "Residuals") # plot fitted values and residuals
abline(0,0)                                                                         # add horizontal line at 0 


## -----------------------------------------------------------------------------
rmarkdown::convert_ipynb("ProjectReportDraft1.ipynb")


## -----------------------------------------------------------------------------
knitr::purl("ProjectReportDraft1.Rmd")

