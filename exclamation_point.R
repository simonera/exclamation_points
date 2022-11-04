library(rtweet)

# get authorization
auth_setup_default()

# import politician's tweets
mydata <- get_timeline("ewarren", n = 3200)

# create exclamation point variable
mydata$excl <- ifelse(grepl("!", mydata$text), 1, 0)

# see frequency distribution of the exclamation point variable
table(mydata$excl)

# compute mean of exclamation point variable
mean(mydata$excl)

# create table
mymatrix <- matrix(c("ewarren", mean(mydata$excl)), ncol = 2, byrow = TRUE)

# create a loop to add other politicians
clist <- c("BernieSanders", "Ilhan", "AOC", "GiorgiaMeloni", "jairbolsonaro",
           "JoeBiden", "HillaryClinton", "matteosalvinimi", "realDonaldTrump")

for (i in clist) {
  mydata <- get_timeline(i, n = 3200)
  mydata$excl <- ifelse(grepl("!", mydata$text), 1, 0)
  mean(mydata$excl)
  mytempmatrix <- matrix(c(i, mean(mydata$excl)), ncol = 2, byrow = TRUE)
  mymatrix <- rbind(mymatrix, mytempmatrix)
}

# he resulting data set is the following
politician <- c("ewarren", "BernieSanders", "Ilhan", "AOC", "GiorgiaMeloni", "jairbolsonaro", "JoeBiden", "HillaryClinton", "matteosalvinimi", "realDonaldTrump")

excl_perc <- c(18.94, 11.73, 20.01, 13.94, 32.25, 20.76, 6.91, 12.00, 13.66, 51.50)

excl_data <- cbind(politician, excl_perc)
excl_data <- as.data.frame(excl_data)
excl_data
excl_data$excl_perc <- as.numeric(excl_data$excl_perc)
summary(excl_data)

# plot
library(ggplot2)
ggplot(excl_data) +
  geom_point(aes(x = excl_perc, y = reorder(politician, excl_perc)), 
             shape="\u0021", size = 8) + 
  labs(title = "% of Exclamation Mark Use in Tweets of Selected Politicians",
       caption = "Collected by Simone Rambotti with rtweets in June 2020", 
       x = "% of !", y = "Selected Politicians")