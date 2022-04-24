```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Remove previous data from environment
rm(list = ls())
```

```{r include=FALSE}
library(arrow) # needed to read parquet file
library(plyr)
library(dplyr)
library(car)
library(caret)
library(rattle)
library(rpart.plot)
library(rpart)
library(ggplot2)
library(corrplot)
library(tidyr)
library(Hmisc) #for redundancy analysis
```

# EXAMINE POLICY_CLEAN_0IMPUTE first

```{r}
#file_path <- "../02_CleanData/policy_clean_modeimputed.parquet"
df <- read_parquet("policy_clean_modeimputed.parquet", as_tibble = TRUE)
View(df)
#head(df)
#str(df)
```

```{r}
str(df)
```
```{r}
# more data cleaning
# ratemethod is categorical
df$ratemethod_manual <- ifelse( (df$ratemethod==1),1,0) # manual rate method; others are all grouped together as they follow a procedure
df$ratemethod_prp <- ifelse( (df$ratemethod==7),1,0) # preferred risk policy
df <- subset(df,select=-c(ratemethod)) # drop ratemethod after defining the dummy variable
# 999 in elevationdifference means na; also there are a few outlier:-9986, 9989 that must be typos
df$elevationdifference <- na_if(df$elevationdifference, 999)
df$elevationdifference <- na_if(df$elevationdifference, -9986)
df$elevationdifference <- na_if(df$elevationdifference, 9989)
# occupancy type to dummy
df$occupancytype_single <- ifelse( (df$occupancytype==1),1,0) # single family home
df$occupancytype_2or4 <- ifelse( (df$occupancytype==2),1,0) # 2-4 unit building
df$occupancytype_4more <- ifelse( (df$occupancytype==3 | df$occupancytype==4),1,0) # more than 4 unit building
df$occupancytype_business <- ifelse( (df$occupancytype==6),1,0) # non-residential business building
# drop occupancytype after defining the dummy variable
df <- subset(df, select=-c(occupancytype))
```

```{r}
cols = colnames(df) # all columns
date_list = c() # list of date variables
for (c in cols){
  if (endsWith(c, 'date') == TRUE){
    date_list <- append(c, date_list)
  }
  
}
num_list <- cols[!cols %in% date_list]
ind_list = c() # indicator list
for (c in num_list){
  if (unique(df[c][is.na(df[c])==0]) == list(0,1)){
    ind_list <- append(c, ind_list)
  }
}
ind_list #list of indicator/dummy variables
num_list2 <- num_list[!num_list %in% ind_list] #list of continuous variables
num_list2
```

#1 Explore 

```{r}
# Select continuous variables for correlation examination
# comment
#df_num <- subset(df, select = c(basementenclosurecrawlspacetype, censustract,crsdiscount,deductibleamountinbuildingcoverage, deductibleamountincontentscoverage, federalpolicyfee, latitude, #longitude, policycount, reportedzipcode, ratemethod, totalbuildinginsurancecoverage, totalcontentsinsurancecoverage))
df_num <- subset(df, select = c(basementenclosurecrawlspacetype, censustract,crsdiscount,deductibleamountinbuildingcoverage, deductibleamountincontentscoverage, federalpolicyfee, latitude, longitude, policycount, reportedzipcode, ratemethod_manual, ratemethod_prp, totalbuildinginsurancecoverage, totalcontentsinsurancecoverage))
```


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
# this checks just for linear correlations
```

```{r}
#df_clean = df[, -which(colnames(df) %in% highlyCor_Col)]
#dim(df_clean)
#View(df_clean)
```

```{r}
highly_corelated = findCorrelation(cormat_nonlin, cutoff = 0.6)
highlyCor_Col_nonlin = colnames(df_num)[highly_corelated]
highlyCor_Col_nonlin
```
```{r}
# Now, we remove highly correlated variables from original dataset and create cleaner dataset, the dataset now contains 41 columns
df_clean = df[, -which(colnames(df) %in% highlyCor_Col_nonlin)]
df_clean=df
dim(df_clean)


```



```{r}
var_list <- colnames(df_clean)
var_list <- var_list[!var_list %in% date_list]
df_clean2 <- subset(df_clean, select=(var_list))
colSums(is.na(df_clean2))
# lots of Nas for elevationdifference
```

```{r}
cor(subset(df_clean2, select=c(elevationdifference, totalinsurancepremiumofthepolicy)), use = "pairwise.complete.obs", method="spearman")
# elevatiodifference has a high nonlinear correlation with the target variable (~-0.66) ==> it should not be removed 
```
```{r}
# replace na with mean and see if corrrelation holds
df_clean3 <- df_clean2
df_clean3$elevationdifference[is.na(df_clean3$elevationdifference)] <- mean(df_clean2$elevationdifference, na.rm=TRUE)
cor(subset(df_clean3, select=c(elevationdifference, totalinsurancepremiumofthepolicy)), use = "pairwise.complete.obs", method="spearman")
# The correlation doesn't hold, but the direction holds (still negative) and the value is still high (-0.29)
```

# It doesn't hold, but the direction (still negative) holds and the value is still high (-0.29)

```{r}
sd(df$elevationdifference, na.rm=TRUE)
mean(df$elevationdifference, na.rm=TRUE)
# plotting elevation differences between -100 to 250 (majority of data)
plot(df_clean3$elevationdifference[df_clean3$elevationdifference> -100 & df$elevationdifference<250], df_clean3$totalinsurancepremiumofthepolicy[df_clean3$elevationdifference> -100 & df_clean3$elevationdifference<250])
```


```{r}
# reportedzipcode: 5 digit Postal Zip Code for the insured property reported by WYO partners
# It's not a real number; also we have longitude and latitude ==> need to omit it for modeling
# For now it is kept to be able to do filtering based on zipcode for later
#df_clean3 <- subset(df_clean3, select=-c(reportedzipcode))
# remove rows with na values
row.has.na <- apply(df_clean3, 1, function(x){any(is.na(x))})
sum(row.has.na)
df_clean3 <- df_clean3[!row.has.na,]
nrow(df_clean3)
```
```{r}
temp <- subset(df_clean3, select = -c(reportedzipcode))
fit = lm(totalinsurancepremiumofthepolicy ~., data=temp)
summary(fit)
```


```{r}
# removing variables with NA coefficients
# NA as a coefficient in a regression indicates that the variable in question is linearly related to the other variables.
df_clean3 <- subset(df_clean3, select=-c(policytermindicator,mobilehomeortrailer, floodzone_undetermined, occupancytype_business))
temp <- subset(df_clean3, select = -c(reportedzipcode))
fit = lm(totalinsurancepremiumofthepolicy ~., data=temp)
summary(fit)
# not removing insignificant features yet. Maybe after non-linear transformation they turn significant
```

```{r}
# check log-linear model
fit = lm(log(totalinsurancepremiumofthepolicy+1) ~ ., data=temp)
summary(fit)
```

```{r}
# there is a non-linear trend here.
# let's check if the features need to be transformed too
final_features = colnames(df_clean3)
final_features <- final_features[final_features != "totalinsurancepremiumofthepolicy" & final_features != "reportedzipcode"]
final_features
```

```{r}
# for each feature compare adjusted R2 for log transformation; it will print the features whose log transformation increases adjusted R2
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ basementenclosurecrawlspacetype, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(basementenclosurecrawlspacetype+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("basementenclosurecrawlspacetype")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ construction, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(construction+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("construction")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ deductibleamountinbuildingcoverage, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(deductibleamountinbuildingcoverage+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("deductibleamountinbuildingcoverage")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ deductibleamountincontentscoverage, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(deductibleamountincontentscoverage+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("deductibleamountincontentscoverage")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ elevatedbuildingindicator, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(elevatedbuildingindicator+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("elevatedbuildingindicator")
}
const <- abs(min(df_clean3$elevationdifference))
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ elevationdifference, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(elevationdifference+const+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("elevationdifference+const")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ federalpolicyfee, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(federalpolicyfee+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("federalpolicyfee")
}
const <- abs(min(df_clean3$latitude))
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ latitude, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(latitude+const+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("latitude+const")
}
```

```{r}
# continue with log transformation of features and comparing R2
const <- abs(min(df_clean3$longitude))
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ longitude, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(longitude+const+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("longitude+const")
}
const <- abs(min(df_clean3$numberoffloorsininsuredbuilding))
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ numberoffloorsininsuredbuilding, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(numberoffloorsininsuredbuilding+const+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("numberoffloorsininsuredbuilding")
}
#const <- abs(min(df_clean3$policycount))
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ policycount, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(policycount+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("policycount")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ postfirmconstructionindicator, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(postfirmconstructionindicator+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("postfirmconstructionindicator")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ primaryresidenceindicator, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(primaryresidenceindicator+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("primaryresidenceindicator")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ totalbuildinginsurancecoverage, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(totalbuildinginsurancecoverage+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("totalbuildinginsurancecoverage")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ not_condo, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(not_condo+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("not_condo")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ u_condo, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(u_condo+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("u_condo")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ lowerflooronly, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(lowerflooronly+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("lowerflooronly")
}
```


```{r}
# continue with log transformation of features and comparing R2
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ upperandlowerfloors, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(upperandlowerfloors+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("upperandlowerfloors")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ basementandabove, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(basementandabove+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("basementandabove")
}
#const <- abs(min(df_clean3$policycount))
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ basementonly, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(basementonly+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("basementonly")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ morethan1floor, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(morethan1floor+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("morethan1floor")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ floodzone_highrisk, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(floodzone_highrisk+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("floodzone_highrisk")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ floodzone_modrisk, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(floodzone_modrisk+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("floodzone_modrisk")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ occupancytype_single, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(occupancytype_single+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("occupancytype_single")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ occupancytype_2or4, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(occupancytype_2or4+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("occupancytype_2or4")
}
mod_lin <- lm(log(totalinsurancepremiumofthepolicy+1) ~ occupancytype_4more, 
              data = df_clean3)
mod_log <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(occupancytype_4more+1), 
              data = df_clean3)
if (summary(mod_log)$adj.r.squared > summary(mod_lin)$adj.r.squared) {
  print("occupancytype_4more")
}
```
```{r}
#final_features
```


```{r}
const <- abs(min(df_clean3$numberoffloorsininsuredbuilding))
# log transformation of indicator variables makes no difference on R2
fit <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(basementenclosurecrawlspacetype+1) + 
            construction +
            log(deductibleamountinbuildingcoverage+1) +
            deductibleamountincontentscoverage +
            elevatedbuildingindicator +
            elevationdifference +
            federalpolicyfee +
            latitude +
            longitude +
            log(numberoffloorsininsuredbuilding+const+1) +
            log(policycount+1) +
            postfirmconstructionindicator +
            primaryresidenceindicator +
            totalbuildinginsurancecoverage +
            not_condo +
            (u_condo) + # log made no difference on R2
            (lowerflooronly) + # log made no difference on R2
            (upperandlowerfloors) + # log made no difference on R2
            (basementandabove) + # log made no difference on R2
            basementonly +
            (morethan1floor) + # log made no difference on R2
            (floodzone_highrisk) + # log made no difference on R2
            #floodzone_modrisk + # not significant
            occupancytype_single +
            occupancytype_2or4 +
            (occupancytype_4more), # log made no difference on R2
          data= df_clean3
)
summary(fit)
```
```{r}
# try cubic root transformation
temp <- df_clean3
temp$elevationdifference_trans <- (temp$elevationdifference)^(1/3)
temp$deductibleamountinbuildingcoverage_trans <- (temp$deductibleamountinbuildingcoverage)^(1/3)
temp$deductibleamountincontentscoverage_trans <- (temp$deductibleamountincontentscoverage)^(1/3)
temp$basementenclosurecrawlspacetype_trans <- (temp$basementenclosurecrawlspacetype)^(1/3)
temp$totalbuildinginsurancecoverage_trans <- (temp$totalbuildinginsurancecoverage)^(1/3)
temp$latitude_trans <- (temp$latitude)^(1/3) # no difference
temp$longitude_trans <- (temp$longitude)^(1/3)

const <- abs(min(df_clean3$numberoffloorsininsuredbuilding))
# log transformation of indicator variables makes no difference on R2
fit <- lm(log(totalinsurancepremiumofthepolicy+1) ~ log(basementenclosurecrawlspacetype+1) + 
            construction +
            deductibleamountinbuildingcoverage_trans +
            deductibleamountincontentscoverage_trans +
            elevatedbuildingindicator +
            elevationdifference_trans +
            federalpolicyfee +
            latitude +
            longitude +
            log(numberoffloorsininsuredbuilding+const+1) +
            log(policycount+1) +
            postfirmconstructionindicator +
            primaryresidenceindicator +
            totalbuildinginsurancecoverage_trans +
            not_condo +
            (u_condo) + 
            (lowerflooronly) + 
            (upperandlowerfloors) + 
            (basementandabove) + 
            basementonly +
            (morethan1floor) + 
            (floodzone_highrisk) + 
            occupancytype_single +
            occupancytype_2or4 +
            (occupancytype_4more), 
          data= temp
)
summary(fit)
```


# Redundancies (takes time to run)

```{r}
## Redundancies
#redun <- redun(~.,
#  r2 = 0.6,
#  type = "adjusted",
#  tlinear = FALSE,
#  nk = 3, #changed from 4
#  iterms = TRUE,
#  pc = TRUE,
#  data = subset(temp, select=-c(totalinsurancepremiumofthepolicy)) # all x variables
#)
#print(redun, digits = 2)
```



## None are insignificant variables

```{r}
vif(fit)
```

# Choose a VIF cutoff under which a variable is retained (Zuur et al. 2010) 
# vif>10  multi-collinearity: remove variables with vif > 10
## Create the final dataset for further analysis 


#basementandabove has vif 17.4
```{r}
df_final <- subset(temp, select = -c(not_condo, policycount, u_condo,basementandabove))
const <- abs(min(df_final$numberoffloorsininsuredbuilding))
df_final$totalinsurancepremiumofthepolicy_log <- log(df_final$totalinsurancepremiumofthepolicy+1)
df_final$basementenclosurecrawlspacetype_log <- log(df_final$basementenclosurecrawlspacetype +1)
df_final$numberoffloorsininsuredbuilding_log <- log(df_final$numberoffloorsininsuredbuilding+const+1)
df_final <- subset(df_final, select = c(totalinsurancepremiumofthepolicy_log,
                                        basementenclosurecrawlspacetype_log, 
                                        construction, 
                                        deductibleamountinbuildingcoverage_trans,
                                        deductibleamountincontentscoverage_trans, 
                                        elevatedbuildingindicator, 
                                        elevationdifference_trans,
                                        federalpolicyfee,
                                        latitude,
                                        longitude,
                                        numberoffloorsininsuredbuilding_log,
                                        postfirmconstructionindicator, 
                                        primaryresidenceindicator, 
                                        totalbuildinginsurancecoverage_trans,
                                        lowerflooronly, 
                                        upperandlowerfloors,
                                        basementonly,
                                        morethan1floor,
                                        floodzone_highrisk,
                                        occupancytype_single,
                                        occupancytype_2or4,
                                        occupancytype_4more,
                                        reportedzipcode)) #added for later filtering
temp0 <- subset(df_final, select = -c(reportedzipcode))
fit2 <- lm(totalinsurancepremiumofthepolicy_log~., data=temp0)
summary(fit2)
```

```{r}
# Exporting the dataframe to EDA folder
# df_final_NoNA includes numeric (df_num) and 2 "date" columns
#file_path3 <- "../03_EDA/df_final_modeimpute_nonlin.parquet"
write_parquet(df_final, "df_final_modeimpute_nonlin.parquet")
```

# filter for top 5 zipcodes
```{r}
names(which.max(table(df_final$reportedzipcode))) # "77096"
```

```{r}
temp <- df_final %>% 
  filter(reportedzipcode != "77096")
names(which.max(table(temp$reportedzipcode))) # 77089
temp <- temp %>% 
  filter(reportedzipcode != "77089")
names(which.max(table(temp$reportedzipcode))) # 77084
```

```{r}
temp <- temp %>% 
  filter(reportedzipcode != "77024")
names(which.max(table(temp$reportedzipcode))) # 77024
```
```{r}
temp <- temp %>% 
  filter(reportedzipcode != "77024")
names(which.max(table(temp$reportedzipcode))) # 77062
```


```{r}
df_5_Zipcode <- subset(df_final, reportedzipcode == '77096'| reportedzipcode == '77089' |reportedzipcode == '77084'| reportedzipcode == '77024' |reportedzipcode == '77062')
# Remove zipcode, the dataset includes 300K rows
#df_5_Zipcode <- subset(df_5_Zipcode, select=-c(reportedzipcode))
# remove rows with na values
row.has.na <- apply(df_5_Zipcode, 1, function(x){any(is.na(x))})
sum(row.has.na)
df_5_Zipcode <- df_5_Zipcode[!row.has.na,]
nrow(df_5_Zipcode)  #278261
#file_path4 <- df_final_0impute_5zipcode.parquet"
write_parquet(df_5_Zipcode,"df_final_modeimpute_5zipcode.parquet")
```



# Import data again to avoid run all above commands
```{r}
#file_path <- "../03_EDA/"df_final_modeimpute_nonlin.parquet"
df_num <- read_parquet("df_final_modeimpute_nonlin.parquet", as_tibble = TRUE)
```
# Rerun lm to check significant variables


```{r}
temp0 = subset(df_num, select=-c(reportedzipcode))
fit3 = lm(totalinsurancepremiumofthepolicy_log ~., temp0)
summary(fit3)

#no need to remove any variable at this point```
```{r}
#temp0 = subset(df_num, select=-c(reportedzipcode))
#fit3 = lm(totalinsurancepremiumofthepolicy_log ~., temp0)
#summary(fit3)
```


```{r}
vif(fit3)
```
temp0 = subset(df_num, select=-c(reportedzipcode,lowerflooronly,upperandlowerfloors,longitude))
fit3 = lm(totalinsurancepremiumofthepolicy_log ~., temp0)
summary(fit3)

# The data all good now.


1. Stepwise regression
```{r}
temp0 <- subset(df_num, select=-c(reportedzipcode,lowerflooronly,upperandlowerfloors,longitude))
# remove rows with na values
row.has.na <- apply(temp0, 1, function(x){any(is.na(x))})
sum(row.has.na)
temp0 <- temp0[!row.has.na,]
colnames(temp0)
```


```{r}
scaledData = as.data.frame(scale(temp0[,c(4,5,7,8,9,10,13)])) # standardize all numerical variables
scaledData <- cbind(temp0[,c(2,3,6,11,12,14,15,16,17,18,19)],scaledData,temp0[,1]) # Add categorical columns  back in
colnames(scaledData)#[16] <- "totalinsurancepremiumofthepolicy_log"
```

# this part is computationally demanding
```{r}
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
lmFit_Step <- train(totalinsurancepremiumofthepolicy_log ~ ., data = temp0, "lmStepAIC", scope = 
                      list(lower = totalinsurancepremiumofthepolicy_log~1, upper = totalinsurancepremiumofthepolicy_log~.), direction = "backward",trControl=ctrl, na.action=na.exclude)
```



```{r}
mod_Step = lm(totalinsurancepremiumofthepolicy_log ~ construction + deductibleamountinbuildingcoverage_trans + 
                deductibleamountincontentscoverage_trans + elevatedbuildingindicator + 
                elevationdifference_trans + federalpolicyfee + latitude + 
                numberoffloorsininsuredbuilding_log + postfirmconstructionindicator + 
                primaryresidenceindicator + totalbuildinginsurancecoverage_trans + 
                basementenclosurecrawlspacetype_log+ 
                basementonly + morethan1floor + floodzone_highrisk + occupancytype_single + 
                occupancytype_2or4 + occupancytype_4more,
              data = temp0)
summary(mod_Step)
```
```{r}
# trying scaled data
mod_Step = lm(totalinsurancepremiumofthepolicy_log ~ construction + deductibleamountinbuildingcoverage_trans + 
                deductibleamountincontentscoverage_trans + elevatedbuildingindicator + 
                elevationdifference_trans + federalpolicyfee + latitude + 
                numberoffloorsininsuredbuilding_log + postfirmconstructionindicator + 
                primaryresidenceindicator + totalbuildinginsurancecoverage_trans + 
                basementenclosurecrawlspacetype_log + 
                basementonly + morethan1floor + floodzone_highrisk + occupancytype_single + 
                occupancytype_2or4 + occupancytype_4more,
              data = scaledData)
summary(mod_Step)
```

## LASSO
# computationally demanding
```{r}
## Variable selections using Lasso
train_con <- trainControl(method = 'cv', number = 5) # using 5 folds cv
lasso <- train(totalinsurancepremiumofthepolicy_log ~., temp0,
               method = 'lasso',
               preProc = c('scale', 'center'),
               trControl = train_con
)
summary(lasso)
```

```{r}
# Get coef
predict(lasso$finalModel, type = "coef", mode = "fraction", s = as.numeric(lasso$bestTune))
# We can further discard variables with coefficients reduced to zero
```

# From the Lasso Regression models, we can discard the variables with coeeficients decreasing to zero, in this case we will remove variable "basementonly", "

```{r}
temp1 = subset(temp0,select=-c(basementonly)) 
fit3 = lm(totalinsurancepremiumofthepolicy_log ~ construction + 
            deductibleamountincontentscoverage_trans + elevatedbuildingindicator + 
            elevationdifference_trans + federalpolicyfee + latitude + 
            numberoffloorsininsuredbuilding_log + postfirmconstructionindicator + 
            primaryresidenceindicator + totalbuildinginsurancecoverage_trans + 
            morethan1floor + floodzone_highrisk + occupancytype_single + 
            occupancytype_2or4 + occupancytype_4more,
          data = temp1)
summary(fit3)
#View(df_num)
```

```{r}
for (col in c('lowerflooronly', 'elevatedbuildingindicator', 'numberoffloorsininsuredbuilding', 'postfirmconstructionindicator', 'primaryresidenceindicator', 'upperandlowerfloors', 'basementandabove','morethan1floor')) {
  plot <- ggplot(data = temp1,
                 aes_string(x = col, y = 'totalinsurancepremiumofthepolicy', group = col, fill = col)) + 
    geom_boxplot(show.legend = FALSE) + 
    ggtitle(glue::glue("Boxplot of Total Insurance Premium per {col}"))
  print(plot)
}
```


```{r}
# Examine correlation between all numeric variables 
cormat <-cor(temp1, use = "pairwise.complete.obs")
```


```{r}
corrplot(cormat, tl.col = "red", tl.srt = 45, bg = "White",
         title = "\n\n Correlation Plot of Flood Data",
         type = "lower")
dev.copy(jpeg, filename="Correlation Plot of Flood Data_0impute.jpg");
dev.off ()
```


```{r}
# Exporting the dataframe to Modeling folder
# df_num includes only numeric variables for analysis
#file_path0 <- "../04_Modeling/df_num_imputemode_Final_FeatureSel.parquet"
write_parquet(temp1,"df_num_imputemode_Final_FeatureSel.parquet")
```
