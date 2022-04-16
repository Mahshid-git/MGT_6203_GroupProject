install.packages("arrow")
install.packages("rattle")
library(arrow)
library(dplyr)
library(ggplot2)
library(plyr)
library(car)
library(caret)
library(rattle)
library(rpart.plot)
library(rpart)
library(corrplot)
library(tidyr)
policy_df <- read_parquet("nfip_Houston_flood_policies.parquet", as_tibble = TRUE)
head(policy_df)

policy_df$postfirmconstructionindicator=ifelse(policy_df$postfirmconstructionindicator=='Y',1,0)


policy_df$primaryresidenceindicator=ifelse(policy_df$primaryresidenceindicator=='Y',1,0)

policy_df$elevatedbuildingindicator=ifelse(policy_df$elevatedbuildingindicator=='Y',1,0)

policy_df$construction=ifelse(policy_df$construction=='Y',1,0)

policy_df$lowerlevelcondo=ifelse(policy_df$condominiumindicator=='L',1,0)
policy_df$upperlevelcondo=ifelse(policy_df$condominiumindicator=='U',1,0)

policy_df$locationofcontents[is.na(policy_df$locationofcontents)]=0

policy_df$lowerflooronly=ifelse(policy_df$locationofcontents=='Lowest floor only above ground level (No basement/enclosure/crawlspace/subgrade crawlspace)',1,0)
policy_df$upperandlowerfloors=ifelse(policy_df$locationofcontents=='Lowest floor above ground level and higher floors (No basement/enclosure/crawlspace/subgrade crawlspace)',1,0)
policy_df$basementandabove=ifelse(policy_df$locationofcontents=='Basement/Enclosure/Crawlspace/Subgrade Crawlspace and above',1,0)
policy_df$basementonly=ifelse(policy_df$locationofcontents=='Basement/Enclosure/Crawlspace/Subgrade Crawlspace only',1,0)
policy_df$morethan1floor=ifelse(policy_df$locationofcontents=='Above ground level more than one full floor',1,0)
policy_df$mobilehomeortrailer=ifelse(policy_df$locationofcontents==' Manufactured (mobile) home or travel trailer on foundation',1,0)

#sum(is.na(policy_df$locationofcontents))
policy_df$postfirmconstructionindicator=ifelse(policy_df$postfirmconstructionindicator=='Y',1,0)
#primaryresidenceindicator
policy_df$primaryresidenceindicator=ifelse(policy_df$primaryresidenceindicator=='Y',1,0)
```


```{r}
# Continue remove category variables
df = subset(policy_df,select=-c(condominiumindicator, locationofcontents))
#View(df)
```


```{r}
## Type of data in all columns
str(df)
```
```{r}
# Convert all date from numeric to date format
date_cols <- c("originalconstructiondate", "originalnbdate", "policyeffectivedate", "policyterminationdate")
df1 <- df %>%
  mutate_at(vars(all_of(date_cols)), funs(as.Date(., "%Y-%m-%d")))
#str(df1)
```

```{r}
#floodzones
# high risk if starts with A or V
df1$floodzone_highrisk <- ifelse( (startsWith(df1$floodzone, 'A') | (startsWith(df1$floodzone, 'V'))),1,0)
# moderate-to-low risk if starts with B, C or X
df1$floodzone_modrisk <- ifelse( (startsWith(df1$floodzone, 'B') | (startsWith(df1$floodzone, 'C')) | (startsWith(df1$floodzone, 'X'))),1,0)
# possible but undetermined flood hazards: starts with D
df1$floodzone_undetermined <- ifelse( (startsWith(df1$floodzone, 'D')) ,1,0)
```


```{r}
#identify all character columns
df2 <- subset(df1, select = -c(floodzone))
# ratemethod should also be categorical; 1 - Manual 2 - Specific 3 - Alternative 4 - V-Zone Risk Factor Rating Form 5 - Underinsured Condominium Master Policy 6 - Provisional 7 - Preferred Risk Policy (PRPs issued for eligible properties located within a non-Special Flood Hazard Area [non-SFHA]) 8 - Tentative 9 - MPPP Policy 
chars <- sapply(df2, is.character)
#convert all character columns to numeric
df2[ , chars] <- as.data.frame(apply(df2[ , chars], 2, as.numeric))
str(df2)
```

```{r}
# Remove negative value
df2 <- df2[df2$policycost >= 0, ]
df2 <- df2[df2$federalpolicyfee >= 0, ]
df2 <- df2[df2$totalinsurancepremiumofthepolicy >= 0, ]
```

# Examine the response variable (totalinsurancepremiumofthepolicy)

```{r}
summary(df2$totalinsurancepremiumofthepolicy)
```

```{r}
p=ggplot(df2, aes(x = totalinsurancepremiumofthepolicy, fill = cut(x=totalinsurancepremiumofthepolicy, 100))) +
  geom_histogram(show.legend = FALSE,bins=15) +
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70) +
  theme_minimal() +
  labs(x = "Total Insurance Premium", y = "Frequency") +
  ggtitle("Histogram of Total Insurance Premium")
print(p)
dev.copy(jpeg,filename="Histogram of Total Insurance Premium_Original.jpg");
dev.off ()
```

```{r}
summary(df2$totalinsurancepremiumofthepolicy)
ggplot(data = df2, aes(x=totalinsurancepremiumofthepolicy)) + geom_density(alpha = 0.5)+
  labs(x = "Total Insurance Premium", y = "Frequency")+
  ggtitle("Distribution of Insurance Premium")
dev.copy(jpeg,filename="Distribution of Total Insurance Premium_Original.jpg");
dev.off ()
```



```{r}
# As the number of totalinsurancepremiumofthepolicy > 5000 is only 0.1% then we remove all rows with the value higher than 5000 to eleminate error
df2 <- df2[(df2$totalinsurancepremiumofthepolicy < 3000),]
```
```{r}
ggplot(df2, aes(x = totalinsurancepremiumofthepolicy, fill = cut(x=totalinsurancepremiumofthepolicy, 100))) +
  geom_histogram(show.legend = FALSE,bins=15) +
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70) +
  theme_minimal() +
  labs(x = "Total Insurance Premium", y = "Frequency") +
  ggtitle("Histogram of Total Insurance Premium")
dev.copy(jpeg,filename="Histogram of Total Insurance Premium_Final.jpg");
dev.off ()
```


```{r}
View(df2)
```



```{r}
summary(df2$totalinsurancepremiumofthepolicy)
ggplot(data = df2, aes(x=totalinsurancepremiumofthepolicy)) + geom_density(alpha = 0.5)+
  labs(x = "Total Insurance Premium", y = "Frequency")+
  ggtitle("Distribution of Insurance Premium")
dev.copy(jpeg,filename="Distribution of Total Insurance Premium_Final.jpg");
dev.off ()
```
policy_df = subset(policy_df,select=-c(agriculturestructureindicator, propertystate, reportedcity, houseofworshipindicator,nonprofitindicator,regularemergencyprogramindicator,smallbusinessindicatorbuilding,hfiaasurcharge))
#View(policy_df)
```

```{r}
# count NA values in each column
colSums(is.na(policy_df))
```

## Continue fiter some columns with large number of NA

```{r}
# Filter more columns with significant number of NA values
df2 = subset(df2,select=-c(basefloodelevation,cancellationdateoffloodpolicy, countycode, elevationcertificateindicator, lowestadjacentgrade, lowestfloorelevation, obstructiontype))                          
#View(policy_df)



df_num <- subset(df2, select = c(basementenclosurecrawlspacetype, censustract,crsdiscount,federalpolicyfee, latitude, longitude, policycost, policycount, reportedzipcode, ratemethod, totalbuildinginsurancecoverage, totalcontentsinsurancecoverage))
```
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

```{r}
# Now, we remove highly correlated variables from original dataset and create cleaner dataset, the dataset now contains 41 columns
df_clean = df[, -which(colnames(df) %in% highlyCor_Col)]
dim(df_clean)
View(df_clean)
```

```{r}
fit = lm(totalinsurancepremiumofthepolicy ~., df_clean)
summary(fit)
```

## from the output, we can remove some insignificant variables

```{r}
fit1 = lm(totalinsurancepremiumofthepolicy ~. -censustract -deductibleamountinbuildingcoverage -numberoffloorsininsuredbuilding -policytermindicator -postfirmconstructionindicator -reportedzipcode -lowerflooronly -upperlevelcondo -mobilehomeortrailer -floodzone_modrisk -floodzone_undetermined    -upperandlowerfloors -basementonly -morethan1floor, df_clean)
summary(fit1)
```

## Create the final dataset for further analysis (remove policycost as it = premium =admin fee)

```{r}
df_final <- subset(df_clean, select = -c(censustract, deductibleamountinbuildingcoverage, numberoffloorsininsuredbuilding, policytermindicator, postfirmconstructionindicator, reportedzipcode, lowerflooronly, upperlevelcondo, mobilehomeortrailer, floodzone_modrisk, floodzone_undetermined, upperandlowerfloors, basementonly,morethan1floor, elevationdifference, policycost))
View(df_final)
```

```{r}
str(df_final)
```


```{r}
# Select only numerical columns for data analysist (leave data type in df_final)
# df_num includes 17 variables
df_num <- subset(df_final, select = -c(originalconstructiondate, originalnbdate, policyeffectivedate, policyterminationdate))
View(df_num)
```

```{r}
# categorical columns
df_cat <- subset(df_num, select = -c(crsdiscount, latitude, longitude , policycount, totalbuildinginsurancecoverage,totalcontentsinsurancecoverage , totalinsurancepremiumofthepolicy))
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
mod_Step = lm(totalinsurancepremiumofthepolicy ~ basementenclosurecrawlspacetype + construction +   deductibleamountincontentscoverage + elevatedbuildingindicator + occupancytype + policycount + 
                lowerlevelcondo + basementandabove + floodzone_highrisk + 
                crsdiscount + latitude + longitude + totalbuildinginsurancecoverage + 
                totalcontentsinsurancecoverage,
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
