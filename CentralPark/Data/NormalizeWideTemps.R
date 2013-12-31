###################
# Clear memory, load packages, and define global variables
###################
rm(list=ls(all=TRUE))
require(reshape2)
require(lubridate)
pathInput <- "./CentralPark/Data/Input/CentralParkTempWide.txt"
pathOutputCsv <- "./CentralPark/Data/Derived/CentralParkTemp.csv"
# pathOutputRda <- "./CentralPark/Data/Derived/CentralParkTemp.rda"

###################
# Read in the dataset
###################
dsWide <- read.table(pathInput, header=TRUE, sep=" ", stringsAsFactors=FALSE)
dsWide$ANNUAL <- NULL #Drop this variable, because it doesn't fit well in a long format.

###################
# Convert from wide to long (see Winston Chang's recipe 15.19), and clean up the long format.
###################
defaultDayOfMonth <- 15L
dsLong <- reshape2::melt(dsWide, id.vars="YEAR")
dsLong <- plyr::rename(dsLong, replace=c(
    "YEAR" = "Year", 
    "variable" = "Month",
    "value" = "Temp"
))

###################
# Convert the date to something usable by graphs & stats
###################
dsLong <- transform(dsLong, Date=as.Date(lubridate::ymd(paste(Year, Month, defaultDayOfMonth, sep="-"))))
dsLong <- dsLong[order(dsLong$Date), ]
dsLong$ID <- seq_len(nrow(dsLong))
dsLong$Month <- as.integer(lubridate::month(dsLong$Date)) #Replace the letters with a numeric format

dsLong$DaysInMonth <- lubridate::days_in_month(dsLong$Date)
dsLong$DaysInYear <- as.integer(365L + lubridate::leap_year(dsLong$Date))

dsLong$CenturyFullYear <- (dsLong$Year %/% 100L) * 100L
dsLong$CenturyFullID <- (dsLong$Year %/% 100L) - 17L
dsLong$CenturyHalfYear <- (dsLong$Year %/% 50L) * 50L
dsLong$CenturyHalfID <- (dsLong$Year %/% 50L) - 36L
dsLong$CenturyQuarterYear <- (dsLong$Year %/% 25L) * 25L
dsLong$CenturyQuarterID <- (dsLong$Year %/% 25L) - 73L

# sapply(dsLong, class)
###################
# Write to disk
###################
CentralParkTemp <- dsLong

write.csv(CentralParkTemp, file=pathOutputCsv, row.names=FALSE)
# save(CentralParkTemp, file=pathOutputRda, compress="xz")

