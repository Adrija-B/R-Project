## Libraries
library(MASS)
library(tidyverse)
library(corrplot)
library(data.table)

#### READ DATA
rm(list=ls())
setwd("/Users/kayvoncoffey/Desktop/1 Academics/CU Boulder/5 F21/MSBX 5415/project")
df <- read.csv(paste(getwd(),"/data/top100all_clean.csv",sep=""))
dt <- setDT(df)
df_og <- read.csv(paste(getwd(),"/data/top100all.csv",sep=""))
dt_og <- setDT(df_og)


### Data Cleaning
dt_og[id=="7sO5G9EABYOXQKNPNiE9NR",
      c("track.name","instrumentalness") := list("Ric Flair Drip (& Metro Boomin)",5.05e-05)]

dt_og[id=="161DnLWsx1i3u1JT05lzqU" & week%in%c(43,45),
   c("acousticness","danceability","energy","liveness","loudness","speechiness","tempo","valence"):=
     list(0.511,0.498,0.590,0.107,-4.721,0.0320,145.867,0.0784)]

dt_og[!(week==39 & rank>100)]

### One Week Wonders vs True Hits
byvars = c("track.name","artist","id","acousticness","danceability","duration_ms",
           "energy","instrumentalness","key","liveness","loudness","mode",
           "speechiness","tempo","time_signature","valence")

dt_agg <- dt_og[, .(num.t200=.N, num.t50=sum(rank<=50),
                    max.streams=max(streams), avg.streams=mean(streams),
                    max.popularity=max(popularity),avg.popularity=mean(popularity)),
                by = byvars]
dt_agg

#solved
#temp <- aggregate(track.name ~ id,data=dt_agg,length)
#temp[temp[,2]>1,]   #7sO5G9EABYOXQKNPNiE9NR  161DnLWsx1i3u1JT05lzqU

#temp <- aggregate(track.name~artist,data=dt_agg,FUN=length)
#df_og[track.name=="Solid (feat. Drake)"]

dev.new();hist(dt_agg$num.t200,
               main="Distribution of Num Weeks Spent in Top 200 Last Year",
               xlab="Number of Weeks",ylab="Frequency")
dev.new();hist(dt_agg[num.t50>0,num.t50],
               main="Distribution of Num Weeks Spent in Top 50 Last Year \n Among Songs that Hit Top 50 At Least Once",
               xlab="Number of Weeks",ylab="Frequency")

dt_agg$top50 <- ifelse(dt_agg$num.t50>0,1,0)
dt_agg$top50_min5 <- ifelse(dt_agg$num.t50>=5,1,0)

### Distributions
numcols <- names(dt_agg)[sapply(dt_agg,is.numeric)]
dev.new();corrplot(cor(dt_agg[,..numcols]),type="lower",main="Pairwise Linear Correlations", mar = c(0, 0, 4, 0))

## Output Cleaned Data
write.csv(dt_agg, file=paste(getwd(),"/data/top100all_clean2.csv",sep=""))

pdf(paste(getwd(),"/output/Num Weeks in Top Distributions.pdf",sep=""))
hist(dt_agg$num.t200,
       main="Distribution of Num Weeks Spent in Top 200 Last Year",
       xlab="Number of Weeks",ylab="Frequency")
hist(dt_agg[num.t50>0,num.t50],
       main="Distribution of Num Weeks Spent in Top 50 Last Year \n Among Songs that Hit Top 50 At Least Once",
       xlab="Number of Weeks",ylab="Frequency")
dev.off()

pdf(paste(getwd(),"/output/Pairwise Linear Cor.pdf",sep=""))
corrplot(cor(dt_agg[,..numcols]),type="lower",main="Pairwise Linear Correlations", mar = c(0, 0, 4, 0))
dev.off()
