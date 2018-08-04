---
title: "Crime Statistics Analysis by R"
author: "Kenneth Chen"
date: "7/15/2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, tidy.opts=list(width.cutoff=60),tidy=TRUE)
library(car)
library(PerformanceAnalytics)
library(ggplot2)
library(corrplot)
library(dplyr)
library(stargazer)
```

```{r}
crime = read.csv("crime_v2.csv")
```

# Introduction

Here I would like to explore the crime dataset from North Carolina. I will go step by step from statistical standpoint. This is to develop several viable approaches to answer a simple question "Are there any factors that influence the crime in North Carolina?". The dataset has  **`r nrow(crime)`** observations and **`r ncol(crime)`** variables. Our first approach is to investigate each of the variables and how they relate to the occurrence of crimes in North Carolina in 1987. 

# Exploratory Data Analysis

```{r echo=FALSE, include=FALSE}
# Not displaying the output because it's the same as my writeup
ls.str(pat='crime')
```

I listed all variables and their descriptions here. 

| variable | label |
|--------|-------|
| 1 county    | county identifier     |
| 2 year   | 1987     |
| 3 crmrte   | crimes committed per person     |
| 4 prbarr   | 'probability' of arrest     |
| 5 prbconv    | 'probability' of conviction     |
| 6 prbpris   | 'probability' of prison sentence     |
| 7 avgsen   | avg. sentence, days     |
| 8 polpc    | police per capita     |
| 9 density  | people per sq. mile |
| 10 taxpc   | tax revenue per capita |
| 11 west    | =1 if in western N.C. |
| 12 central | =1 if in central N.C. |
| 13 urban   | =1 if in SMSA |
| 14 pctmin80 | perc. minority, 1980 |
| 15 wcon    | weekly wage, construction |
| 16 wtuc    | weekly wage, trns, util, commun |
| 17 wtrd    | weekly wage, whlelse, retail trade |
| 18 wfir    | weekly wage, fin, ins, real est |
| 19 wser    | weekly wage, service industry |
| 20 wmfg    | weekly wage, manufacturing |
| 21 wfed    | weekly wage, fed employees |
| 22 wsta    | weekly wage, state employees |
| 23 wloc    | weekly wage, local gov emps |
| 24 mix     | offense mix: face-to-face/other |
| 25 pctymle | percent young male |

Out of 25 variables, we just set our dependent variable to be **`crime rates, crmrte`** because we believe this reflects the frequency of crimes in North Carolina. To create our prediction model precisely and present clearly, we developed several objectives and lay our foundational work here.  

## Sanity check and data cleaning

There are 97 observations and 25 variables in the dataset. we need to check if there are any empty values in each variable by applying the `!is.na` function. Interestingly, only one variable `prbconv (probability of conviction)` has full observations, i.e., 97. The rest of the variables have 91 observations out of original 97, which gives us **`91/97 = 0.9381`**.
\hfill\break
```{r}
# Margin = 2 indicates column wise application
apply(!is.na(crime), MARGIN=2,  mean)

# We could also try another command `colSums()`
colSums(is.na(crime))
```

\hfill\break
We also need to check if all 97 observations in `prbconv` is a real value or any of the special characters. As a control, we included other variables as well.
\hfill\break
```{r}
# Checking special characters such as 'a white space' etc 
(apply(crime[1:25], MARGIN=2, FUN=function(x) sum(x %in% c("`", "", "?", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")"))))
```

As you can see, we found that there are **`6`** special characters in `prbconv` variable, which left me with 91 observations from 97. The rest of the variables do not contain special characters. Further check upon `prbconv` shows that the variable contains **`5`** white space and a special character `backtick`, **`**. 

Before we continue the analysis, I removed all empty rows. I also checked if there is any duplicate observations in our sample by `distinct()` function. Since there are variables with probability, I removed any samples with probability greater than 1 or percentage greater than 100 to avoid erroraneous recordings in crime data, which led to remove one additional row that has a `prbarr` greater than 1. The data now has 90 rows. Lastly, I changed the variable type into `numeric` for developing our model. 

\hfill\break
```{r}
# So 97 observations end up at 91 observations. 
crime_full = crime[complete.cases(crime), ]

# Removing duplicate samples in the dataset
crime_unique = distinct(crime_full)

# Changing the data type into 'numeric' for our data analysis
crime_num= as.data.frame(lapply(crime_unique, as.numeric))

# Remove probability or percentage greater than 1 or 100 in prbarr, prbconv, prbpris, pctmin80, pctymle to make it sensible data
crime_cleaned = crime_num[!(crime_num$prbarr > 1 & crime_num$prbconv > 100 &  crime_num$prbpris > 1 & crime_num$pctmin80 > 100 & crime_num$pctymle > 1),]
```

# Independent Variables or Explanatory Variables analysis

I checked if there's any relationship between each explanatory variable by correlation matrix by `cor()` function. To visualize the correlation matrix, I use the corrplot on the results from the `cor()` function. I also checked by histogram and correlation numbers by `chart.Correlation()` function from `PerformanceAnalytics` library. 

I removed `year` and `county` in my explanatory variable because `year` is 1987 and `county` is all ordinal number. 

```{r}
png("corrplot.png", width = 500, height = 500)
corrplot(cor(crime_cleaned[3:25]))
```


```{r}
png("corrmatrix.png", width = 500, height = 500)
chart.Correlation(crime_cleaned[3:25], histogram = TRUE, pch=19)
```

## Positive Correlation 

I found the high correlation on `crmrte` from ten explanatory variables. 

- `density` = 0.73 
- `urban` = 0.62 
- `taxpc` = 0.45
- `wcon` = 0.39 
- `wtrd` = 0.43 
- `wfir` = 0.34 
- `wfmg` = 0.35 
- `wfed` = 0.49 
- `wloc` = 0.36 
- `pctymle` = 0.29 

Intuitively, we can imagine that `density` and `urban` might be correlated because urban area will be higly populated. Evidently `density` and `urban` has `0.82` correlation, which gives us an insight when we develop our prediction model. 

I also observed that `density` has a high correlation with `wcon`, `wtrd`, `wfir`, `wfmg`, `wfed` and `wloc` with `0.45`, `0.59`, `0.55`, `0.44`, `0.59`, `0.46` respectively. It appears that weekly wages of those amenities such as transportation, manufacturing sector, federal employess are also highly correlated with `density` or `urban` explanatory variable. This also makes sense because highly populated area will have more industrial servivces, accounting for `wages` explanatory variables. 

Another interesting finding from such a correlation matrix is `pctymle`, a percent of young male population is not correlated with all the other variables we listed previously. Therefore we can keep an eye on this explanatory variable.  

## Negative Correlation

We also found that there are some variables negatively correlated to our dependent variable `crmrte`. 

- `prbarr`= -0.40 
- `prbconv` = -0.40 
- `west` = -0.35 

Negative relationship between each explanatory variable: 

- `west` and `pctmin80` = -0.63 

# Selection of Key variables

Out of 25 variables, in order to understand the `key determinants` of the crime, we first need to define our two **Key independent variables** to be 

- `density` people per sq.mile 
- `taxpc` tax per capita 

For developing a more accurate model gradually, here is a list of the variable that I will test out sequentially with more clear explantion on classical regression.  

- `polpc` police per capita 
- `pctmin80` percent of minority 
- `pctymle` percent of young male population 

## Correlation matrix between 3 variables (dependent and independents)

```{r}
png("table1.png", width = 500, height = 500)
table1 = cbind(crime_cleaned[3], sqrt(crime_cleaned[9]), crime_cleaned[10])
chart.Correlation(table1, histogram = TRUE, pch=19)
```
\hfill\break 
# Model 1
```{r}
# Since there's an outlier in taxpc at 120, we remove the extreme outlier
crime_89 = crime_cleaned[!(crime_cleaned$taxpc > 100),]

model1 = lm(crmrte ~ sqrt(density) + taxpc, data=crime_89)
summary(model1)
```
```{r}
png("model1.png", width = 500, height = 500)
par(mfrow=c(2,2))
plot(model1)
```

# Checking the violation of assumptions in classical linear regression

## Assumpion I : Expected or sum of the error term is equal to zero. 

$E(\epsilon) = 0$ or $\sum \epsilon = 0$
```{r}
mean(model1$residuals)
sum(model1$residuals)
```

Since the expected value of residuals or sum is infinitesimally small, our assumption is satisfied. But we do not know if our model also satisifes other assumptions. I will explore more from the 4 plots above. 

The first model shows that our residuals are clustered based on the first plot `Residuals Vs Fitted`. QQ plot also shows that the residuals are not normally distributed and there are a few observations such as an observation `25` has a high influence or leverage on our regression model as it goes beyond the Cook's distance. This can also be seen in our prior scatterplot in which we can see that there are a few outliers in `pctymle` variable. As a practice, I continue checking all other assumptions before we modify our base model. 

```{r}
# Checking the assumption in OLS
# Checking if there's any correlation between independent variable and the residuals
cor.test(sqrt(crime_89$density), model1$residuals)
cor.test(crime_89$taxpc, model1$residuals)
```

The correlation test shows that there is no correlation between our explanatory variables and the dependent variable: `crmrte`. Their p-value is 1 and 1 respectively. So we fail to reject our null hypothesis which is no correlation between the explanatory variables and the dependent variable. 

## Assumption 2 : Homoscedasticity 

```{r}
var(sqrt(crime_cleaned$density))
var(crime_cleaned$taxpc)
```

We also see that the variance of the explanatory variable `sqrt(density)` has low variance, which satisfies our homoscedasticity. However the variance of the other variable `pctymle` is high, further confirming our previous observation that there are a few outliers in `pctymle` variable. 

## Assumption 3 : No Multicollinearity between explanatory variables

```{r}
vif(model1)
```

As a general rule of thumb, VIF (Variance Inflation Factor) should be lower than 4. The higher the VIF, the more correlation between each explanatory variables. As I mentioned earlier on, we started off with a selection of variables that do not have a correlation between each other. So our `vif()` test again confirms our choice here and satisfies our assumption. 

## Assumption 4 : Skewness, Kurtosis

```{r}
library(gvlma)
gvlma(model1)
```
We see that there are a few assumptions violated in our model, presumably due to the choice of our explanatory variable in the beginning. 

## More Data Cleaning

```{r}
# Checking if there's any outliers
png("boxplot1.png", width = 500, height = 500)
par(mfrow=c(1,2))
boxout = boxplot(crime_cleaned$crmrte, varwidth = T, outline = T, border = T, plot = T, boxwex = .25)
boxout$out
```

Boxplot shows that there are 6 outliers in crmrte that would have influenced our regression model. 

# Model 2

```{r}
# Using boxplot.stats remove the printout. boxplot() can also be used. 
png("model2.png", width = 500, height = 500)
crime_83 = crime_89[!(crime_89$crmrte %in% boxplot.stats(crime_89$crmrte)$out),]
model2 = lm(crmrte ~ sqrt(density) + taxpc, data=crime_83)
par(mfrow=c(2,2))
plot(model2)
```
You now notice that after removing 6 outliers, there's no observations closer to Cook's distance. This will reduces biases in our regression model. 

## Assumption 5 : Omitted Variable Bias

Since we used `density`, another assumption in classical linear regression is **Omitted Variable Bias**. Density also reflects a population. Out of 24 explanatory variable, I have `pctymle`, percent of young male population which has no correlation with `density`. So we can introduce new variable. 

```{r}
png("table2.png", width = 500, height = 500)
table2 = cbind(crime_83[3], sqrt(crime_83[9]), crime_83[10], crime_83[25])
chart.Correlation(table2, histogram = TRUE, pch=19)
```

# Model 3

```{r}
png("model3.png", width = 500, height = 500)
model3 = lm(crmrte ~ sqrt(density) + taxpc + pctymle, data=crime_83)
summary(model3)
par(mfrow=c(2,2))
plot(model3)
```
You would notice that the distribution of residuals become normal in 1st plot. Scale-location plot also shows a homoscedasticity of the residual variance. There is one observation `30` lying closer to Cook's distance. This sample is also an outlier in `QQ plot`.  

# Model 4
## Adding 3 dummy variables `west`, `central` and `urban`

```{r}
# Remove the outlier by the boxplot
crime_81 = crime_83[!(crime_83$crmrte %in% boxplot.stats(crime_83$crmrte)$out),]

# Adding dummy variables
model4 = lm(crmrte ~ sqrt(density) + taxpc + pctymle + factor(west) + factor(central) + factor(urban), data=crime_81)


summary(model4)
par(mfrow=c(2,2))
png("model4.png", width = 500, height = 500)
plot(model4)

gvlma(model4)
```


We observed that our model becomes more in tune with classical assumptions for linear regression. 

# Model 5

```{r}
# Adding one more explanatory variable
png("model5.png", width = 500, height = 500)
model5 = lm(crmrte ~ sqrt(density) + taxpc + pctymle + pctmin80 + factor(west) + factor(central) + factor(urban), data=crime_81)

summary(model5)
par(mfrow=c(2,2))
plot(model5)

gvlma(model5)
```

# Model 6

```{r}
# Adding one more explanatory variable
png("model6.png", width = 500, height = 500)
model6 = lm(crmrte ~ prbarr + prbconv + polpc + sqrt(density) + taxpc + pctymle + pctmin80 + factor(west) + factor(central) + factor(urban), data=crime_81)

summary(model6)
par(mfrow=c(2,2))
plot(model5)

gvlma(model6)
```

# Akaike Test : Goodness of Fit Vs Parsimony 

As you know by now, we explored a number of explanatory variables in our prediction model. But as we add more variables, $R^2$ also increases but it does not mean that our model becomes better. We need to check other assumptions such as residuals, homoscedasticity and such. One of the test to check the parsimony model is Akaike test, AIC(). 

```{r}
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model5)
AIC(model6)
```

You now realize that our last model gives us the better AIC value while we are not employing all the variables available in our data. 

```{r}
stargazer(model1, model2, model3, model4, model5, model6, type = "text", 
          report = "vc", # Don't report errors, since we haven't covered them
          title = "Linear Models Predicting Crime Rates in NC",
          keep.stat = c("rsq", "adj.rsq", "n"),
          omit.table.layout = "n") # Omit more output related to errors
```


\hfill\break

\newpage
# Testing Model 5

$\widehat{\text{crmrte}} = \beta_0 + \beta_1 \cdot \text{sqrt(density)} + \beta_2 \cdot \text{taxpc} + \beta_3 \cdot \text{pctymle} + \beta_4 \cdot \text{pctmin80} + \beta_5 \cdot \text{west} + \beta_6 \cdot \text{central} + \beta_7 \cdot \text{urban}$  

Where `west`, `central` and `urban` are dummy variables. 

$\widehat{\text{crmrte}} = -0.013 + 0.025 \cdot \text{sqrt(density)} + 0.0001 \cdot \text{taxpc} + 0.113 \cdot \text{pctymle} + 0.0002 \cdot \text{pctmin80} - 0.004 \cdot \text{west} - 0.004 \cdot \text{central} + 0.001 \cdot \text{urban}$  

```{r}
# define the sample size for train and test split
d = sample(x = nrow(crime_81), size=nrow(crime_81)*0.7)
# Splitting into train and test dataset by the d percentage which is 0.7
train = crime_81[d,]
test = crime_81[-d,]
# Train model
model2 = lm(crmrte ~ sqrt(density) + taxpc, data=train)
model3 = lm(crmrte ~ sqrt(density) + taxpc + pctymle, data=train)
model4 = lm(crmrte ~ sqrt(density) + taxpc + pctymle + factor(west) + factor(central) + factor(urban), data=train)
model5 = lm(crmrte ~ sqrt(density) + taxpc + pctymle + pctmin80 + factor(west) + factor(central) + factor(urban), data=train)
model6 = lm(crmrte ~ prbarr + prbconv + polpc + sqrt(density) + taxpc + pctymle + pctmin80 + factor(west) + factor(central) + factor(urban), data=train)

# test model
prediction2 = predict(model2, test)
prediction3 = predict(model3, test)
prediction4 = predict(model4, test)
prediction5 = predict(model5, test)
prediction6 = predict(model6, test)

# Checking the accuracy of the prediction model by test data on the regression line
library(Metrics)
rmse(actual = crime_81$crmrte, predicted = prediction2)
rmse(actual = crime_81$crmrte, predicted = prediction3)
rmse(actual = crime_81$crmrte, predicted = prediction4)
rmse(actual = crime_81$crmrte, predicted = prediction5)
rmse(actual = crime_81$crmrte, predicted = prediction6)
```


# Conclusion

Statisically we proved that the crime rate in North Carolina is highly correlated with the density. We also showed that depending on the location, crime events vary across the regions: `west`, `central` and `urban`. By using our location variables as the dummy variables, we fine-tuned our model. We have statistically indicated the key determinants in investigating the crime in North Carolina. 
