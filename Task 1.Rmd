---
title: "Quantium Virtual Internship - Retail Strategy and Analytics - Task 1"
author: "Lavany Jadhav"
date: "7/1/2021"
output: pdf_document
 
---

## Loading packages

```{r}
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(dplyr)
```

## Loading data and inspect data
```{r}
transactionData <- fread("D:/Portfolio Projects/Quantium Virtual Internship/Task 1/QVI_transaction_data.csv")
customerData <- fread("D:/Portfolio Projects/Quantium Virtual Internship/Task 1/QVI_purchase_behaviour.csv")
str(transactionData)
```

## Processing the transactionData

Changing date columnin date format
```{r}
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
head(transactionData)
```

Examine Prod_NAME
```{r}
transactionData[, .N,  PROD_NAME]
```

Looks like we are definitely looking at potato chips but how can we check that these are all chips? We can do some basic text analysis by summarizing the individual words in the product name.
```{r}
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')
head(productWords)
```

Removing numbers and extra spaces
```{r}
transactionData$PROD_NAME = substr(transactionData$PROD_NAME,1,nchar(transactionData$PROD_NAME))
transactionData$PROD_NAME = gsub("\\s+"," ",transactionData$PROD_NAME)
head(transactionData)
```

There are salsa products in the dataset but we are only interested in the chips category, so let’s remove these.
```{r}
### For transactionData
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]
head(transactionData)
```

Summarizing the processed table
```{r}
summary(transactionData)
```

## Filtering the outlier

There are two transactions where 200 packets of chips are bought in one transaction and both of these transactions were by the same customer.
```{r}
transactionData[PROD_QTY == 200, ]
```


It looks like this customer has only had the two transactions over the year and is not an ordinary retail customer. The customer might be buying chips for commercial purposes instead. We’ll remove this loyalty card number from further analysis
```{r}
which(grepl(226000, transactionData$LYLTY_CARD_NBR))
```

Filtering the outliers 
```{r}
transactionData = filter(transactionData, LYLTY_CARD_NBR != 226000) 
summary(transactionData)
```

Count the number of transactions by date
```{r}
tab <- table(cut(transactionData$DATE, 'day'))
transactions_by_day <- data.frame(tab)
transactions_by_day <- data.frame(as.Date(transactions_by_day$Var1), transactions_by_day$Freq)
transactions_by_day <- transactions_by_day %>%
  rename( DATE = as.Date.transactions_by_day.Var1., N = transactions_by_day.Freq)
summary(transactions_by_day)
```

Lets plot Transactions by day, to see sales over time.
```{r}
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

ggplot(transactions_by_day, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

We can see that there is an increase in purchases in December and a break in late December. Let’s zoom in on this.
```{r}
ggplot(transactions_by_day[transactions_by_day$DATE >= "2018-12-01" & transactions_by_day$DATE <= "2018-12-31", ],
       aes(x = DATE, y = N)) +  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

We can see that the increase in sales occurs in the lead-up to Christmas and that there are zero sales on Christmas day itself. This is due to shops being closed on Christmas day.
Now that we are satisfied that the data no longer has outliers, we can move on to creating other features such as brand of chips or pack size from PROD_NAME. We will start with pack size.
```{r}
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]
```

The largest size is 380g and the smallest size is 70g - seems sensible! Lets Plot it. Let's plot a histogram of PACK_SIZE since we know that it is a categorical variable and not a continuous variable even though it is numeric.
```{r}
hist(transactionData[,PACK_SIZE])
```

Pack sizes created look reasonable.
Now to create brands, we can use the first word in PROD_NAME to work out the brand name…
```{r}
transactionData$Brand <- gsub("([A-Za-z]+).*", "\\1", transactionData$PROD_NAME)
transactionData[, .N, by = Brand][order(-N)]
```

Some of the brand names look like they are of the same brands - such as RED and RRD, which are both Red Rock Deli chips. Let’s combine these together.
```{r}
transactionData[Brand == "RED", Brand :="RRD"]
transactionData[Brand == "SNBTS", Brand :="SUNBITES"]
transactionData[Brand == "INFZNS", Brand :="INFUZIONS"]
transactionData[Brand == "WW", Brand :="WOOLWORTHS"]
transactionData[Brand == "SMITH", Brand :="SMITHS"]
transactionData[Brand == "NCC", Brand :="NATURAL"]
transactionData[Brand == "DORITO", Brand :="DORITOS"]
transactionData[Brand == "GRAIN", Brand :="GRNWVES"]
transactionData[, .N, by = Brand][order(-N)]
colnames(transactionData)
```

##  Processing customerData
```{r}
str(customerData)
head(customerData)
```

Summary of customerData
```{r}
summary(customerData)
```
```{r}
customerData[, .N, by = LIFESTAGE][order(N)]
customerData[, .N, by = PREMIUM_CUSTOMER][order(N)]
```

As there do not seem to be any issues with the customer data, we can now go ahead and join the transaction and customer data sets together
```{r}
data <- merge(transactionData,customerData, all.x = TRUE)
```

As the number of rows in `data` is the same as that of `transactionData`, we can be sure that no duplicates were created. This is because we created `data` by setting `all.x = TRUE` (in other words, a left join) which means take all the rows in `transactionData` and find rows with matching values in shared columns and then joining the details in these rows to the `x` or the first mentioned table.
Let’s also check if some customers were not matched on by checking for nulls.
```{r}
data[is.null(LIFESTAGE), .N]
data[is.null(PREMIUM_CUSTOMER), .N]
```

## Data analysis on customer segments

Now that the data is ready for analysis, we can define some metrics of interest to the client:
Who spends the most on chips (total sales), describing customers by lifestage and how premium their general purchasing behaviour is
How many customers are in each segment
How many chips are bought per customer by segment
What’s the average chip price by customer segment
We could also ask our data team for more information. Examples are:
The customer’s total spend over the period and total spend for each transaction to understand what proportion of their grocery spend is on chips
Proportion of customers in each customer segment overall to compare against the mix of customers who purchase chips
Let’s start with calculating total sales by LIFESTAGE and PREMIUM_CUSTOMER and plotting the split by these segments to describe which customer segment contribute most to chip sales.
```{r}
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]
#### Create plot
p <- ggplot(data = sales) +
geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
 fill = PREMIUM_CUSTOMER)) +
labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of
 sales") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#### Plotting
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100, '%'))))
```

Sales are coming mainly from Budget - older families, Mainstream - young singles/couples, and Mainstream - retirees Let’s see if the higher sales are due to there being more customers who buy chips.
```{r}
#### Number of Customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]
#### Create Plot
p <- ggplot(data = customers) +
geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE), fill = PREMIUM_CUSTOMER)) +
labs(x = "Lifestage", y = "Premium customer flag", title = "Proportion of customers") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#### Plot and label with proportion of customers
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 , y =
(ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,
'%'))))
```

There are more Mainstream - young singles/couples and Mainstream - retirees who buy chips. This contributes to there being more sales to these customer segments but this is not a major driver for the Budget - Older families segment.
Higher sales may also be driven by more units of chips being bought per customer.
Let’s have a look at this next.
```{r}
#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
#### Create plot
ggplot(data = avg_units, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
geom_bar(position = position_dodge()) +
labs(x = "Lifestage", y = "Avg units per transaction", title = "Units per customer") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

Older families and young families in general buy more chips per customer
Let’s also investigate the average price per unit chips bought for each customer segment as this is also a driver of total sales.
```{r}
#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price <- data[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)), .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
#### Create plot
ggplot(data = avg_price, aes(weight = AVG, x = LIFESTAGE, fill = PREMIUM_CUSTOMER)) +
geom_bar(position = position_dodge()) +
labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
```

Mainstream midage and young singles and couples are more willing to pay more per packet of chips compared to their budget and premium counterparts. This may be due to premium shoppers being more likely to buy healthy snacks and when they buy chips, this is mainly for entertainment purposes rather than their own consumption. This is also supported by there being fewer premium midage and young singles and couples buying chips compared to their mainstream counterparts.

As the difference in average price per unit isn’t large, we can check if this difference is statistically different. To do so, I will perform an independent t-test between mainstream vs premium midage young singles and couplesto see if the difference is significant.
```{r}
#### Perform an independent t‐test between mainstream vs premium and budget midage and
#### young singles and couples
pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
            & PREMIUM_CUSTOMER == "Mainstream", price]
       , data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") 
              & PREMIUM_CUSTOMER != "Mainstream", price]
       , alternative = "greater")
```

The t-test results in a p-value < 2.2e-16, i.e. the unit price for mainstream, young and mid-age singles and couples are significantly higher than that of budget or premium, young and midage singles and couples.


## Deep dive into specific customer segments for insights

We have found quite a few interesting insights that we can dive deeper into.

We might want to target customer segments that contribute the most to sales to retain them or further increase sales. Let’s look at Mainstream - young singles/couples. For instance, let’s find out if they tend to buy a particular brand of chips.
```{r}
#### Deep dive into Mainstream, young singles/couples
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream",]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"),]
#### Brand affinity compared to the rest of the population
quantity_segment1 <- segment1[, sum(PROD_QTY)]
quantity_other <- other[, sum(PROD_QTY)]
quantity_segment1_by_brand <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = Brand]
quantity_other_by_brand <- other[, .(other = sum(PROD_QTY)/quantity_other), by = Brand]
brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]
```

We can see that :
• Mainstream young singles/couples are 23% more likely to purchase Dorito chips compared to the rest of the population.

• Mainstream young singles/couples are 56% less likely to purchase Burger Rings compared to the rest of the population

Let’s also find out if our target segment tends to buy larger packs of chips.
```{r}
quantity_segment1_by_pack <- segment1[, .(targetSegment = sum(PROD_QTY)/quantity_segment1), by = PACK_SIZE]
quantity_other_by_pack <- other[, .(other = sum(PROD_QTY)/quantity_other), by = PACK_SIZE]
pack_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityToPack := targetSegment/other]
pack_proportions[order(-affinityToPack)]
```

Mainstream young singles/couples are more likely to purchase a 270g pack of chips compared to the rest of the population but let’s dive into what brands sell this pack size.
```{r}
data[PACK_SIZE == 270, unique(PROD_NAME)]
```

Twisties are the only brand offering 270g packs and so this may instead be reflecting a higher likelihood of
purchasing Twisties.

## Conclusion

Let’s recap what we’ve found!
Sales have mainly been due to Budget - older families, Mainstream - young singles/couples, and Mainstream
- retirees shoppers. We found that the high spend in chips for mainstream young singles/couples and retirees is due to there being more of them than other buyers. Mainstream, midage and young singles and
couples are also more likely to pay more per packet of chips. This is indicative of impulse buying behaviour.
We’ve also found that Mainstream young singles and couples are 23% more likely to purchase Tyrrells chips
compared to the rest of the population. The Category Manager may want to increase the category’s performance by off-locating some Tyrrells and smaller packs of chips in discretionary space near segments
where young singles and couples frequent more often to increase visibilty and impulse behaviour.
Quantium can help the Category Manager with recommendations of where these segments are and further
help them with measuring the impact of the changed placement. We’ll work on measuring the impact of
trials in the next task and putting all these together in the third task.



