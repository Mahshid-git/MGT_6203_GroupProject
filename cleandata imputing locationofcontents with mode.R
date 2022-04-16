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

val <- unique(policy_df$locationofcontents[!is.na(policy_df$locationofcontents)])                   
my_mode <- val[which.max(tabulate(match(policy_df$locationofcontents, val)))]
policy_df$locationofcontents[is.na(policy_df$locationofcontents)]=my_mode

policy_df$lowerflooronly=ifelse(policy_df$locationofcontents=='Lowest floor only above ground level (No basement/enclosure/crawlspace/subgrade crawlspace)',1,0)
policy_df$upperandlowerfloors=ifelse(policy_df$locationofcontents=='Lowest floor above ground level and higher floors (No basement/enclosure/crawlspace/subgrade crawlspace)',1,0)
policy_df$basementandabove=ifelse(policy_df$locationofcontents=='Basement/Enclosure/Crawlspace/Subgrade Crawlspace and above',1,0)
policy_df$basementonly=ifelse(policy_df$locationofcontents=='Basement/Enclosure/Crawlspace/Subgrade Crawlspace only',1,0)
policy_df$morethan1floor=ifelse(policy_df$locationofcontents=='Above ground level more than one full floor',1,0)
policy_df$mobilehomeortrailer=ifelse(policy_df$locationofcontents==' Manufactured (mobile) home or travel trailer on foundation',1,0)

sum(is.na(policy_df$locationofcontents))
policy_df$postfirmconstructionindicator=ifelse(policy_df$postfirmconstructionindicator=='Y',1,0)
#primaryresidenceindicator
policy_df$primaryresidenceindicator=ifelse(policy_df$primaryresidenceindicator=='Y',1,0)
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
policy_df = subset(policy_df,select=-c(basefloodelevation,cancellationdateoffloodpolicy, countycode, elevationcertificateindicator, lowestadjacentgrade, lowestfloorelevation, obstructiontype))                          
#View(policy_df)
```

```{r}
# Number of NA values in each column
colSums(is.na(policy_df))

```{r}
# Continue remove category variables
df = subset(policy_df,select=-c(condominiumindicator, locationofcontents))
View(df)
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
str(df1)
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
ggplot(df2, aes(x = totalinsurancepremiumofthepolicy, fill = cut(x=totalinsurancepremiumofthepolicy, 100))) +
  geom_histogram(show.legend = FALSE,bins=15) +
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70) +
  theme_minimal() +
  labs(x = "Total Insurance Premium", y = "Frequency") +
  ggtitle("Histogram of Total Insurance Premium")
#print(p)
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

