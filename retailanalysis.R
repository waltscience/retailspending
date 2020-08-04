# As GDP hits historic lows, is there anything we are still spending money on?
# Chris Walter - chriswalter.info/ecosentiment
#
# Data sources - US Census Bureau Monthly Retail Trade, 
#                retrieved on July 30, 2020 from https://www.census.gov/retail/marts/www/timeseries.html

library(stringr)
files <- list.files(path=".", pattern="*.txt", full.names=TRUE, recursive=FALSE) # list files in folder
spending <- data.frame(matrix(ncol = 0, nrow = 348))                  # initializa a data frame 
for (f in 1:length(files)){
  d <- read.fwf(paste(files[f])  , skip = 1,                          # load data file
                widths = c(8, 8, 8, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10)) 
  colnames(d) <- c("YEAR", "JAN", "FEB", "MAR", "APR", "MAY",         # set column names
                   "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")                         
  d <- d[-c(1, 31:68), ]                                              # remove extraneous rows
  rnam <- d[, 1]                                                      # extract rownames
  d <- d[, -1]                                                        # remove year column
  datafix <- function(x){                                             # function to get data to numeric
    as.numeric(as.character(x))
  }
  d <- as.data.frame(sapply(d, datafix))                              # apply numeric function
  rownames(d) <- rnam                                                 # set rownames
  d <- as.data.frame(t(d))                                            # transpose d
  df <- (stack(d[1:ncol(d)]))                                         # stack matrix data to one column
  df <- (df[, -2])                                                    # remove year column
  spending <- cbind(spending, df)                                     # bind spending column to final data frame
  colnames(spending)[f] <- str_sub(files[f], 3, -5)                   # make column name match file name (category)
}
spending$Date <- seq(as.Date("1992/1/1"), as.Date("2020/12/1"), "months") # add dates to final data frame
write.csv(spending, "spending.csv", row.names = FALSE)                # write file to folder

dat <- read.csv("spending.csv", header = TRUE)                        # load cleaned data
dat$Date <- as.Date(dat$Date, "%Y-%m-%d")                             # set date as date
s20p <- subset(dat, Date >= "2019-02-01" & Date < "2020-01-01")       # subset data for 7 months pre-covid sales
s20p$Date <- c(1:11)                                                  # change date to numeric to make x-scale unit in linear regression = month instead of day
s20a <- subset(dat, Date >= "2020-02-01" & Date < "2020-07-01")       # subset data for 1 month pre-covid sales and 4 months of covid sales
slps <- as.data.frame(matrix(ncol = ncol(s20p)))                      # initialize a data frame
colnames(slps) <- colnames(s20p)                                      # copy data frame column names
for (i in 1:(length(s20p[1, ]) - 1)) {                                # loop through columns and fit lm to each one
    slps[1, i] <- lm(s20p[, i] ~ s20p$Date)$coefficients[2]
}
slps <- slps[, -18]                                                   # remove date column
cumulpred <- s20a[1, -18]                                             # initialize a data frame with Feb 2020 sales
for(j in 2:length(s20a[, 1])) {                                       # loop to iteratively add predicted change in sales each month, to the previous month
  cumulpred[j, ] <- cumulpred[j-1, -18] + slps
}
cumulpred <- as.data.frame(t(colSums(cumulpred[c(2:5), ])))
cumulact <- as.data.frame(t(colSums(s20a[c(2:5), -18])))              # sum March through June actual sales to get total cumulative actual sales for four months
chngsal <- ((cumulact - cumulpred) / abs(cumulpred)) * 100            # calculate percent change in sales from projected
chngsal[2, ]  <- cumulact - cumulpred                                 # calculate dollar change in sales from projected
write.csv(t(chngsal), "saleschange.csv", row.names = TRUE)     # write file to folder
