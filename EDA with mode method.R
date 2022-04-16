

df_num <- subset(df2, select = c(basementenclosurecrawlspacetype, censustract,crsdiscount,federalpolicyfee, latitude, longitude, policycost, policycount, reportedzipcode, ratemethod, totalbuildinginsurancecoverage, totalcontentsinsurancecoverage))

head(df_num)


```{r}
# Examine correlation between all numeric variables 
cormat <-cor(df_num, use = "pairwise.complete.obs")
```

```{r}
corrplot(cormat, tl.col = "red", tl.srt = 45, bg = "White",
         title = "\n\n Correlation Plot of Flood Data",
         type = "lower")
dev.copy(jpeg, filename="Correlation Plot of Flood Data.jpg");
dev.off ()
```


```{r}
# Looking for Spearman correlation (checks nonlinear correlation too eg y=x3 ha Spearman correlation of 1)
cormat_nonlin <- cor(df_num, use = "pairwise.complete.obs", method="spearman")
cormat_nonlin
```

```{r}
corrplot(cormat_nonlin, tl.col = "red", tl.srt = 45, bg = "White",
         title = "\n\n Correlation Plot of Flood Data with Spearman Method",
         type = "lower")
dev.copy(jpeg, filename="Correlation Plot of Flood Data using Spearman.jpg");
dev.off ()
```

## Remove Multicollinearity with cutoff 0.6


```{r}
# Multicollinearity
# Check the variables of highly correlated
highly_corelated = findCorrelation(cormat, cutoff = 0.6)
highlyCor_Col = colnames(df_num)[highly_corelated]
highlyCor_Col
```

head(df2)

```{r}
# Now, we remove highly correlated variables from original dataset and create cleaner dataset, the dataset now contains 41 columns
df_clean = df2[, -which(colnames(df2) %in% highlyCor_Col)]
dim(df_clean)
View(df_clean)
```

```{r}
fit = lm(totalinsurancepremiumofthepolicy ~., df_clean)
summary(fit)
```

## from the output, we can remove some insignificant variables

```{r}
fit1 = lm(totalinsurancepremiumofthepolicy ~.-lowerflooronly -longitude -numberoffloorsininsuredbuilding -basementandabove -basementonly -originalconstructiondate -originalnbdate -primaryresidenceindicator -floodzone_highrisk -policytermindicator -postfirmconstructionindicator -mobilehomeortrailer -floodzone_modrisk -floodzone_undetermined -basementonly -morethan1floor, df_clean)
summary(fit1)
```

## Create the final dataset for further analysis (remove policycost as it = premium =admin fee)

```{r}
df_final <- subset(df_clean, select = -c(longitude,numberoffloorsininsuredbuilding,lowerflooronly,basementandabove,basementonly,originalconstructiondate,originalnbdate ,primaryresidenceindicator ,floodzone_highrisk ,policytermindicator ,postfirmconstructionindicator ,mobilehomeortrailer ,floodzone_modrisk ,floodzone_undetermined ,basementonly ,morethan1floor))
View(df_final)
```

```{r}
str(df_final)
```


```{r}
# Select only numerical columns for data analysist (leave data type in df_final)
# df_num includes 17 variables
df_num <- subset(df_final, select = -c( policyeffectivedate, policyterminationdate))
View(df_num)
```

```{r}
# categorical columns
df_cat <- subset(df_num, select = -c(crsdiscount, policycount, totalbuildinginsurancecoverage,totalcontentsinsurancecoverage , totalinsurancepremiumofthepolicy))
lapply(df_cat, unique)
```

1. Stepwise regression

```{r}
scaledData = as.data.frame(scale(df_num[,c(3,6,7,10,11,12)])) # standardize all numerical variables
scaledData <- cbind(df_num[,c(1,2,4,5,8,9,14,15,16)],scaledData,df_num[,13]) # Add categorical columns  back in
colnames(scaledData)[13] <- "totalinsurancepremiumofthepolicy"
```

```{r}
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
lmFit_Step <- train(totalinsurancepremiumofthepolicy ~ ., data = scaledData, "lmStepAIC", scope = 
                      list(lower = totalinsurancepremiumofthepolicy~1, upper = totalinsurancepremiumofthepolicy~.), direction = "backward",trControl=ctrl, na.action=na.exclude)
```



```{r}
mod_Step = lm(totalinsurancepremiumofthepolicy ~ basementenclosurecrawlspacetype+censustract+elevatedbuildingindicator+construction+deductibleamountincontentscoverage+
                elevationdifference+crsdiscount+totalbuildinginsurancecoverage+deductibleamountinbuildingcoverage+totalcontentsinsurancecoverage+
                policycost+lowerlevelcondo+upperlevelcondo,
              data = scaledData)
summary(mod_Step)
```

```{r}
## Variable selections using Lasso
train_con <- trainControl(method = 'cv', number = 5) # using 5 folds cv
lasso <- train(totalinsurancepremiumofthepolicy ~., scaledData,
               method = 'lasso',
               preProc = c('scale', 'center'),
               trControl = train_con,
               na.action=na.exclude)
lasso
```

```{r}
# Get coef
predict(lasso$finalModel, type = "coef", mode = "fraction", s = as.numeric(lasso$bestTune))
# We can further discard variables with coefficients reduced to zero
```

# From the Lasso Regression models, we can discard the variables with coeeficients near to zero, but lookx like we should kep all. # so the totalinsurancepremiumofthepolicy will have 15 variables and 4 date variables

```{r}
#rerun to check if there is still in-significant variables. Look like the dataset is good to go now
fit2 = lm(totalinsurancepremiumofthepolicy ~. , df_num)
summary(fit2)
