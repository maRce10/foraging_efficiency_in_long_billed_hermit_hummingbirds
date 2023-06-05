# Hermit's tracker - data preparation
# date: 27/08/2016

rm(list = ls())
library(plyr)

# Funcs ---------------------- 

# recodes feeders and intervals (A, B, C, OUT -> 1, 2, 3, 0)
numfd <- function (X1) {
  for (i in 1:length(X1$Feeder)) {
    if (X1$Feeder[i] == "A") {
      X1$feeder[i] = 1
    } else {
      if (X1$Feeder[i] == "B") {
        X1$feeder[i] = 2 
      } else {
        if (X1$Feeder[i] == "C") {
          X1$feeder[i] = 3 
        } else {
          X1$feeder[i] = 0
        }
      }
    }
  }
  result <- X1 
  return(result)
} 

# sequentially numbers feeders and intervals
seqfd <- function (Y1) {
  Y1$seqFeeder <- rep(x = seq_along(rle(Y1$feeder)$lengths), times = rle(Y1$feeder)$lengths)
  return(Y1)
} 

# indicates records to delete (external OUTS)
selout <- function (Z1) {
  lastrow <-length(rle(Z1$feeder)$lengths) 
  Z1$sel <-  ifelse (Z1$seqFeeder == lastrow & Z1$Feeder == "OUT" | Z1$seqFeeder == 1 & Z1$Feeder == "OUT",
                     "to delete", "OK")
  return(Z1)
} 

# to subet valid records (no external OUTS)
subsdf<- function (S1) {
  S1 <-S1[S1$sel == "OK", ]
}


# Read data and use the funcs ------------------

# Data 2014

setwd("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog/csv_tracker_2014")

dataframes <- list.files(path=getwd())  
list_dataframes <- llply(dataframes, read.csv, header = T, sep = ";", dec = ",") # list of all data frames

list_dataframes <- llply(list_dataframes, numfd)
list_dataframes <- llply(list_dataframes, seqfd)
list_dataframes <- llply(list_dataframes, selout)
list_dataframes <- llply(list_dataframes, subsdf)

# Read data for birds ID
id14 <- read.csv("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog/id.14.csv", sep = ";")

# Add id to the tracker data
for (i in 1:length(list_dataframes)) {
  list_dataframes[[i]]$fileid <- id14$EntryID[i]
  list_dataframes[[i]]$birdid <- id14$BirdID[i]
  list_dataframes[[i]]$terr <- id14$Territoriality[i]
}

# Convert to data frame
track14 <- do.call(rbind.data.frame, list_dataframes)

track14 <- track14 %>% 
  select(-c(Feeder, sel)) %>%
  mutate(Season = 2014)
 
# # Check whether there is just a single feeder
# checkfeeder14 <- track14 %>% 
#   filter (feeder != 0) %>% 
#   group_by(fileid) %>% 
#   summarise(nfmax = max(feeder),
#             nfmin = min(feeder),
#             comp = nfmax != nfmin) 

# wrong <- which(checkfeeder14$comp == TRUE)
# checkfeeder14[wrong,]

# Save results
setwd("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog")
write.csv(x = track14, file = "track14.csv", row.names = FALSE)


# Data from 2015
# just in case clean up env; save only relevant items (track14, and the funcs) 
setwd("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog/csv_tracker_2015")
dataframes <- list.files(path=getwd())  

list_dataframes <- llply(dataframes, read.csv, header = T, sep = ";", dec = ",") # list of all data frames

list_dataframes <- llply(list_dataframes, numfd)
list_dataframes <- llply(list_dataframes, seqfd)
list_dataframes <- llply(list_dataframes, selout)
list_dataframes <- llply(list_dataframes, subsdf)

# Read data for birds ID
id15 <- read.csv("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog/id.15.csv", sep = ";")

# # To check comaptibility of id file with tracker files
# length(id15$FileID)
# length(dataframes)
# dataframes == paste(id15$FileID, ".csv", sep = "")


# Add id to the tracker data
for (i in 1:length(list_dataframes)) {
  list_dataframes[[i]]$fileid <- id15$FileID[i]
  list_dataframes[[i]]$birdid <- id15$BirdID[i]
  list_dataframes[[i]]$terr <- id15$Territoriality[i]
}

# Convert to data frame
track15 <- do.call(rbind.data.frame, list_dataframes)

library(tidyverse)
track15 <- track15 %>% 
  select(-c(Feeder, sel)) %>%
  mutate(Season = 2015)

# # Checking whether there is just a single feeder
# checkfeeder15 <- track15 %>% 
#   filter (feeder != 0) %>% 
#   group_by(fileid) %>% 
#   summarise(nfmax = max(feeder),
#             nfmin = min(feeder),
#             comp = nfmax != nfmin) 
# 
# wrong <- which(checkfeeder15$comp == TRUE)
# checkfeeder15[wrong,]

# Save the data
setwd("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog")
write.csv(x = track15, file = "track15.csv", row.names = FALSE)


# Both seasons pooled
track <- rbind(track14, track15)

# Save the data
setwd("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog")
write.csv(x = track, file = "track.csv", row.names = FALSE)


# Pers vars -----------
track  <- read.csv("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog/track.csv", sep = ",")
names(track)

# Id base for pers vars data frame
trk_pers <- track %>% select(fileid, birdid, Season, terr) %>% 
  distinct() %>% 
  arrange(fileid)

#   Number of intra-feeding intervals --------
#   (i.e. number of ‘OUTS’ in between; e.g. A-OUT-A-OUT-A = 2)

nout<- function (G1) {
    nouts<-length(which(rle(G1$feeder)$values == 0))
    return(nouts)
  }
  
# Loop to go thru the file -Nouts
ngr <- sort(unique(trk_pers$fileid))
Nouts <- numeric(length = length(ngr))

for (j in 1:length(ngr)){
  track_temp <- track[track$fileid == ngr[j], ]
  Nouts[j] <- nout(track_temp)
}
  
# Add var to data frame
trk_pers$Nouts <- Nouts


# Feeding duration -----------
# - mean and total feeding duration (in seconds)**
# Mean: a simple mean duration of 'INS' intervals. Total: sum of all ‘INS’ durations;  e.g.  A+B+C.* 
# Single frame duration - 0.33 sec; requires temp data set: list_dataframes1 - no internal 'OUTS'.*

# NOTE: Requires subseting original data set (only INs)
trackINonly <- track %>% filter(feeder != 0) # selected data frame

require(stats)

dins <- function (T1){
    tapply(T1$feeder, T1$seqFeeder, length) # need to be run on df without internal 'OUTS'
  }
  
frame <- 0.033 # frame duration in secs



# Loop to go thru the file - Ins_duration
ngr <- sort(unique(trk_pers$fileid))

durIns_mean <-  numeric(length = length(ngr))
durIns_sum <- numeric(length = length(ngr))

for (k in 1:length(ngr)){
  track_temp <- trackINonly[trackINonly$fileid == ngr[k], ]
  durIns_temp <- dins(track_temp)
  durIns_mean [k] <- mean(durIns_temp) * frame
  durIns_sum [k] <- sum(durIns_temp) * frame

  }


# Add var to data frame
trk_pers$durIns_mean <- durIns_mean
trk_pers$durIns_sum <- durIns_sum

# Inter-feeding duration ------  
# Duration of interfeeding intervals  - mean and total  (in seconds)**
# Mean: a simple mean duration of 'OUTS' intervals. Total: sum of all ‘OUTS' in between;  e.g.  A-from OUT1-B-OUT 2-C ->  OUT1+OUT2.*
# Single frame duration - 0.33 sec; requires temp data set - only 'OUTS' records.*

  
# NOTE: Requires subseting original data set (only OUTs)
trackOUTonly <- track %>% filter(feeder == 0) # selected data frame

require(stats)
  
douts <- function (U1){
  tapply(U1$Feeder, U1$seqFeeder, length)
  }
  
frame <- 0.033 # frame duration in secs
  


# Loop to go thru the file - Outs_duration

ngr <- sort(unique(trk_pers$fileid))

durOuts_mean <-  numeric(length = length(ngr))
durOuts_sum <- numeric(length = length(ngr))

for (m in 1:length(ngr)){
  track_temp <- trackOUTonly[trackOUTonly$fileid == ngr[m], ]
  durOuts_temp <- dins(track_temp)
  durOuts_mean [m] <- mean(durOuts_temp) * frame
  durOuts_sum [m] <- sum(durOuts_temp) * frame
  
}

# Add var to data frame
trk_pers$durOuts_mean <- durOuts_mean
trk_pers <-  trk_pers %>% 
  mutate(durOuts_mean = replace_na(durOuts_mean, 0)) # NA (no outs at all) needs to be replaced by 0
trk_pers$durOuts_sum <- durOuts_sum


# Duration of foraging ----- 
  
trk_pers$totFor <- trk_pers$durIns_sum + trk_pers$durOuts_sum


# Arousal -----

# An auxillary function to calculate coeficent of variance
  CV <- function(z){
  sd(z)/mean(z)
  }

  spcalcv<-function(X){ 
  di<-NULL
  for(i in 2:nrow(X))
  {lx<-X$x[i]-X$x[i-1]
  ly<-X$y[i]-X$y[i-1]  
  di[i-1]<-sqrt(lx^2+ly^2)
  }
  return(CV(di))
  }
  
  
# Loop to go thru the file - spacalcv
  
ngr <- sort(unique(trk_pers$fileid))
  
arousal <-  numeric(length = length(ngr))

for (n in 1:length(ngr)){
    track_temp <- track[track$fileid == ngr[n], ]
    arousal [n] <- spcalcv(track_temp)
  
  }
  
# Add var to data frame
trk_pers$arousal <- arousal
  
setwd("C:/Users/Kasia/Dropbox/FF_fear and foraging/Tracker output_cuts from cog")
write.csv(trk_pers, file = "trk_pers.csv", row.names = FALSE) 
