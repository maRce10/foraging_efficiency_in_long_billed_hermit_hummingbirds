rm(list = ls())

# setwd("G:\\LBH_personality stuff\\Video cut 2_LBH 2015_csv")
# lat_df <-read.csv("entry139_2000-01-01T000000_000.csv",header=T,dec=".",sep=',',stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)

lat <- function(x){
  x$code <- as.character(x$code)
  x$NwV1 <- lag(x$code)
  x$NwV2 <- ifelse(x$code == "feeds_start" & x$NwV1 == "first suspens", "first_feed_start", x$code)
  ap <- x$time[x$NwV2 == "appears"]
  ff <- x$time[x$NwV2 == "first_feed_start"]
  lat <- ff-ap
  return(lat)
}

library(plyr)
dataframes <- list.files(path=getwd())  
list_dataframes <- llply(dataframes, read.csv, header = T, sep = ",", dec = ".") # list of all data frames
lat_res <- ldply(list_dataframes, lat)

# Birds ID
setwd("G:\\LBH_personality stuff")
birdid_lat <- read.csv("LBH_rec_video cut 2.csv", header=T,sep=';')
head(birdid_lat)
lat_res <- as.data.frame(cbind(birdid_lat$Idrec_sub, 
                               birdid_lat$Configuration, 
                               birdid_lat$Color.code, 
                               birdid_lat$ID.., 
                               birdid_lat$Comments, 
                               lat_res))

names(lat_res) <- c("RecID", "Conf", "Colcode", "NoID",
                    "Comm", "Latency")
lat_res$ConfSimpl <- substr(lat_res$Conf, 1,3) 
lat_res_Ctr <- lat_res[lat_res$ConfSimpl == "Ctr",]

write.csv(lat_res, file = "latency.csv", row.names = FALSE)

# MODELLING

library (lme4) 
library (lmerTest) 
library (MCMCglmm) 

########### reml ############
model0 <- lm(log(Latency) ~ ConfSimpl, data = lat_res)
model1 <- lmer(log(Latency) ~ ConfSimpl + (1|Colcode), data = lat_res)
model2 <- lmer(log(Latency) ~ ConfSimpl + (1|Colcode) + (1|Conf), data = lat_res)
model3 <- lmer(log(Latency) ~ Colcode + (1|Conf), data = lat_res)

plot(model3)
summary (model3)
anova(model3)

anova(model1, model2)

library(ggplot2)
  ggplot(lat_res, aes(x = Colcode, y = Latency, colour = ConfSimpl)) +
  geom_boxplot()+
  facet_wrap(~ ConfSimpl)

lat_res_id <- lat_res %>% 
  group_by(Colcode, ConfSimpl) %>% 
  dplyr::summarise(meanL = mean(Latency))

lat_res_ids <- lat_res_id %>% spread(Colcode, meanL)
lat_res_ids<-t(lat_res_ids[,2:13])
colnames(lat_res_ids) <- (c("Ctr", "Exp"))
lat_res_ids <- as.data.frame(lat_res_ids)

t.test(lat_res_ids$Ctr, lat_res_ids$Exp)
boxplot(lat_res_ids$Ctr, lat_res_ids$Exp)
ct <- cor.test(lat_res_ids$Ctr, lat_res_ids$Exp)
plot(lat_res_ids$Ctr, lat_res_ids$Exp, 
     xlab = "mean latency in control",
     ylab = "mean latency in experiment",
     main = paste("r = ", round(ct$estimate,2), "/ p = ", round(ct$p.value,3))
)



##############


library(dplyr)
lat_res_Ctr_sel <- lat_res_Ctr %>%
  filter(Colcode == "G" | Colcode ==  "R" | Colcode ==  "WGR" | Colcode ==  "Y")


# lat_res_cons <- lat_res %>%
#   filter(Colcode == "G" | Colcode ==  "R" | Colcode ==  "WGR" | Colcode ==  "Y")
# names(lat_res_cons)
# 
# ff <- lat_res_cons %>%
#   select(ConfSimpl, Colcode, Latency) %>%
#   group_by(Colcode, ConfSimpl) %>% 
#   dplyr::summarise(lat = mean(Latency))
# 
# fff<- ff %>% spread(Colcode, lat) 
# ffff<-t(fff[,2:5])
# plot(ffff[,1],ffff[,2] )
# library(gdata)
# lat_res_Ctr_sel <-drop.levels(lat_res_Ctr_sel)

library(ggplot2)
ggplot(lat_res_Ctr_sel, aes(x = Colcode, y = Latency, colour = ConfSimpl)) +
  geom_point()+
  facet_wrap(~ ConfSimpl)
ggplot(lat_res_Ctr_sel, aes(x = Colcode, y = Latency)) +
    geom_boxplot()

  # ggplot(lat_res, aes(x = Colcode, y = Latency)) +
#   geom_point() +
#   facet_wrap(~ ConfSimpl)

plot(lat_res$Latency, lat_res$Colcode, colour = lat_res$ConfSimpl)

# setwd("D:\\JA\\Colibri\\Personality tests 2015\\Pers.Video cuts 2015")
# birdsid <-read.csv("detailed data.csv",header=T,dec=".",sep=';',stringsAsFactors = FALSE)
# 
# birdsid <- birdsid %>%
# select (Configuration, Color.code, ID..,Video.file.name)

# # Individuals
# cons       <- c(0,0,0,0,0,0,0,0,0,0)
# noncons    <- c(0,1,0,1,0,1,0,1,0,1)
# almcons    <- c(1,1,1,0,1,1,0,1,1,1)
# nonalmcons <- c(1,0,1,1,0,1,1,0,1,1)
# 
# df <- data.frame(cons, noncons, almcons, nonalmcons)
# df_mean <- sapply(df, mean)
# 
# 
# Df_means <- vector("list",1000)
# 
# for (i in 1:1000) {
#   b_cons <- sample(df$cons,5, replace = FALSE)
#   b_noncons <- sample(df$noncons,5, replace = FALSE)
#   b_almcons <- sample(df$almcons,5, replace = FALSE)
#   b_nonalmcons <- sample(df$nonalmcons,5, replace = FALSE)
#   b_df <- data.frame(b_cons, b_noncons, b_almcons, b_nonalmcons)
#   Df_means[i] <- as.data.frame(sapply(b_df, mean))
# }
# 
# Df_means <- ldply(Df_means)
# 
# hist(Df_means$V2)
# abline(v = df_mean[2], col = "red", lwd = 2)
# 
# hist(Df_means$V3)
# abline(v = df_mean[3], col = "red", lwd = 2)
# 
# hist(Df_means$V4)
# abline(v = df_mean[4], col = "red", lwd = 2)
# 
View(lat_res_Ctr)
meanID <- tapply(lat_res_Ctr_sel$Latency, lat_res_Ctr_sel$Colcode, mean)
sdID <- tapply(lat_res_Ctr_sel$Latency, lat_res_Ctr_sel$Colcode, sd)
mean_plus_SD <- meanID + sdID
mean_minus_SD <- meanID - sdID


lat_res_Ctr_sel <- lat_res_Ctr_sel[order(lat_res_Ctr_sel$Colcode),]

for (i in 1: length(unique(lat_res_Ctr_sel$Colcode))){
  lat_res_Ctr_ID <- lat_res_Ctr_sel[1:9,6]
  
  # for (j in 1 : nrow(lat_res_Ctr_ID)){
    # Sc <- lat_res_Ctr_ID$Latency[j]
    # Sc <- ifelse(lat_res_Ctr_ID$Latency <= mean_minus_SD[i] | lat_res_Ctr_ID$Latency >= mean_plus_SD[i], 0, 1)
  }
  # print(Sc)
  print(lat_res_Ctr_ID)
}

lat_res$Colcode[1] 

c<- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
f<- c(1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1)
l<- c(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
m<- c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0)

df1 <- as.data.frame(cbind(c,f,l,m))  

install.packages("vegan")
library(vegan)
sp1 <- specaccum(df1)
sp2 <- specaccum(df1, "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")


# Dla osobnikow, ktore maja >= 9 rekordów (dla controli) wyliczyc srednia i 



