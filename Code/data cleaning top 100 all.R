## Libraries
library(MASS)
library(tidyverse)
library(mgsub)

## Read in Data
rm(list=ls())
setwd("/Users/kayvoncoffey/Desktop/1 Academics/CU Boulder/5 F21/MSBX 5415/project")
datafiles <- list.files(path="/Users/kayvoncoffey/Desktop/1 Academics/CU Boulder/5 F21/MSBX 5415/project/data/top100_all")
top100files <- datafiles[grepl("top100",datafiles)]

df <- data.frame()
for (file in 1:length(datafiles)){
  tempdf <- read.csv(paste("data/top100_all/",datafiles[file],sep=""))
  tempdf$week <- file
  tempdf$rank <- seq.int(nrow(tempdf))
  tempdf <- tempdf[,-1]
  df <- rbind(df, tempdf)}


## Data Cleaning
# transpose artist
names(df)[c(1,2,17)] <- c("track.name","artist","streams")
df_wide <- reshape(df, v.names = "artist", idvar = "track.name", timevar = "artist",
                   direction="wide")
indvars <- grepl("artist.",names(df_wide))

paste_noNA <- function(x, sep="|"){
  gsub(x, "|", sep, toString(x[!is.na(x) & x!="" & x!="NA"]))
}

df_wide$artist_all <- gsub(", NA","", mgsub(apply(X=df_wide[,indvars], 1, 
                                                  FUN = toString),
                            pattern=c("NA, ",", NA"), replacement = c("","")))

df_wide2 <- df_wide[,append(!indvars,TRUE)]

#aggregate(artist_all ~ track.name, data = df_wide2, FUN=length)

# add classifier target
#df_wide2$rank <- seq.int(nrow(df_wide2))
df_wide2$topX <- ifelse(df_wide2$rank <=50,1,0)

## Output Cleaned Data
write.csv(df_wide2, file=paste(getwd(),"/data/top100all_clean.csv",sep=""))


