# Coursera Data Science Specialization
# Building Data Products Course
# Housing Prices Time Series Forecasting
# John James
# Start Date: May 23, 2015

# 0.0 readData
library(datasets)

################################################################################
##                              READ DATA                                     ##
################################################################################
zhviUrl <- "http://files.zillowstatic.com/research/public/Zip/Zip_Zhvi_AllHomes.csv"

if (!file.exists("data")) {
  dir.create("data")
}

if (!file.exists("./data/zhvi.csv")) {
  download.file(zhviUrl, destfile="./data/zhvi.csv")
  dateDownloaded <- date()
}

zhviRaw <- read.csv("./data/zhvi.csv", header = TRUE)

################################################################################
##                          PREPROCESS DATA                                   ##
################################################################################

# Combine state abbreviations and state names into a data frame.
stateInfo <- data.frame(state.abb, state.name)
colnames(stateInfo) <- c("State", "StateName")

# Merge stateInfo and zhviRaw to include StateName
zhviData <- merge(zhviRaw,stateInfo)


################################################################################
##                               SAVE  DATA                                   ##
################################################################################
write.csv(zhviData,file = "./data/zhviData.csv", row.names = FALSE, na="")
