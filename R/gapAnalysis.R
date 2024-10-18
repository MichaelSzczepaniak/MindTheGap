
source("gradientDescents_m2.R")

###########################################################################
# Reads in the cvs data files the were previously downloaded from:
# http://finance.yahoo.com/
# Creates a list of 30 dataframes: one df per stock
###########################################################################
read.dow30 <- function(gap.trigger=0.01, path.prefix="original_data//") {
  dow30 <- c()
  for(i in 1:30) {

    if(i < 10)
      fid <- paste("0",i,sep="")
    else
      fid <- paste(i,sep="")

    file.name <- paste("dow",fid,".csv",sep="")
    dow.file <- read.csv(file=file.name)
    # index the dataframe so it can be reordered properly
    index <- nrow(dow.file):1
    dow.file <- cbind(i=index, dow.file)
    # data is read with most recent at top, so we need to reorder
    dow.file <- dow.file[order(dow.file$i, decreasing=FALSE),]
    # remove the adjusted close column
    dow.file <- dow.file[,-8]
    # add gap column
    dow.file <- cbind(dow.file, gap.size=0)   # col 8
    dow.file <- cbind(dow.file, gap.frac=0)   # col 9
    dow.file <- cbind(dow.file, is.gap=FALSE) # col 10
    # calc gap and percent gap
    for(j in 6:nrow(dow.file)) {
      dow.file[j,8] <- (dow.file[j,3]-dow.file[j-1,6])
      dow.file[j,9] <- dow.file[j,8]/dow.file[j-1,6]
      dow.file[j,10] <- (abs(dow.file[j,9]) >= gap.trigger)
    }

    dow30 <- c(dow30, list(dow.file))
  }

  save(dow30,file="dow30gap.data")
  return(dow30)
}

