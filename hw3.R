library(tidyverse)

data <- readRDS("TaxBurden_Data.rds")


# 1. Present a line graph showing the average number of packs sold per capita from 1970 to 2018.
q1 <- data %>% filter(Year>=1970 && Year<=2018) %>% group_by(Year) %>% summarise(avgPacksSold = mean(sales_per_capita))
ggplot(q1,aes(x=Year, y=avgPacksSold)) + geom_line()

# 2. Present a bar graph showing the proportion of states with a change in their cigarette tax in each year from 1970 to 1985.
q2 <- data
change <- c()
for(i in 1:nrow(q2)) {
  row <- q2[i,]
  if (i != 1) {
    lastRow <- q2[i-1,]
    if (row$state==lastRow$state){ #&& row$Year==lastRow$Year){
      if (row$Year == lastRow$Year+1){
        #q2[i]$change = ifelse(row$tax_state==lastRow$tax_state,0,1)
        change[i]=ifelse(row$tax_state==lastRow$tax_state,0,1)
      }
    }
  }else{change[i]=0}
}
q2 <- cbind(q2, change)

summary(q2)

q2 <- q2 %>% group_by(Year) %>% summarise(propChange = mean(change,na.rm=TRUE))

ggplot(q2,aes(x=Year, y=propChange)) + geom_line()

# 3. Plot the average tax (in 2012 dollars) on cigarettes from 1970 to 2018.
q3 <- data %>% filter(Year>=1970 && Year<=2018) %>% group_by(Year) %>% summarise(avgTaxDollar = mean(tax_dollar))
ggplot(q3,aes(x=Year, y=avgTaxDollar)) + geom_line()

# 4. Plot the average price of a pack of cigarettes from 1970 to 2018. Over what period did prices appear to increase the most?
q4 <- data %>% filter(Year>=1970 && Year<=2018) %>% group_by(Year) %>% summarise(avgCost = mean(cost_per_pack))
ggplot(q4,aes(x=Year, y=avgCost)) + geom_line()

# 5. Identify the 5 states with the highest increases in cigarette prices (in dollars) over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.
q5 <- data

q5test <- q5 %>% filter(Year==1970 | Year==2018) %>% arrange(state)

states = c()
totChange = c()

for(i in 1:nrow(q5test)) {
  row <- q5test[i,]
  lastRow <- q5test[i-1,]
  if (row$Year==2018){
    states <- append(states, row$state)
    totChange <- append(totChange,row$cost_per_pack - lastRow$cost_per_pack)
  }
}  

q5All <- data.frame(states,totChange)

descQ5 <- q5All %>% arrange(desc(totChange))
topQ5 <- head(descQ5, 5)
topQ5

Q5 <- q5 %>% filter(state=="New York" | state=="District of Columbia" | state=="Conneticut" | state=="Rhode Island" | state=="Massachusetts") %>%
  group_by(Year) %>% summarise (avgSales = mean(sales_per_capita))

ggplot(Q5, aes(x=Year, y=avgSales)) + geom_point()

# 6. Identify the 5 states with the lowest increases in cigarette prices over the time period. Plot the average number of packs sold per capita for those states from 1970 to 2018.
ascQ6 <- q5All %>% arrange(totChange)
lowQ6 <- head(ascQ6, 5)
lowQ6

Q6 <- q5 %>% filter(state=="Missouri" | state=="North Dakota" | state=="Tennessee" | state=="Georgia" | state=="North Carolina") %>%
  group_by(Year) %>% summarise (avgSales = mean(sales_per_capita))

ggplot(Q6, aes(x=Year, y=avgSales)) + geom_point()

# 7. Compare the trends in sales from the 5 states with the highest price increases to those with the lowest price increases.


# ATE
# 1. Focusing only on the time period from 1970 to 1990, regress log sales on log prices to estimate the price elasticity of demand over that period. Interpret your results.

# 2. Again limiting to 1970 to 1990, regress log sales on log prices using the total (federal and state) cigarette tax (in dollars) as an instrument for log prices. Interpret your results and compare your estimates to those without an instrument. Are they different? If so, why?

# 3. Show the first stage and reduced-form results from the instrument.

# 4. Repeat questions 1-3 focusing on the period from 1991 to 2015.

# 5. Compare your elasticity estimates from 1970-1990 versus those from 1991-2015. Are they different? If so, why?






