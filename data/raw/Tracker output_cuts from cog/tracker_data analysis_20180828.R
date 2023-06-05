install.packages("factoextra")
install.packages("corrplot")
install.packages("clValid")

library(factoextra)
library(corrplot)
library(FactoMineR)
library(cluster)
library(fpc)
library(clValid)
library(tidyverse)

setwd("C:/Users/KWJ/Dropbox/FF_fear and foraging/Tracker output_cuts from cog")
trk <- read.csv("trk_pers.csv")


# n recors per ind

table(trk$birdid)

# How the birds behaviour changes thru the time``
# id292
n292 <- as.numeric(table(trk$birdid) [6])
ind292 <- trk %>% filter(birdid == "292") %>% arrange(fileid) %>% mutate(Ord = seq(1:n292)) %>% 
  gather(key = "Metric", value = "Value", - c(fileid, birdid, Season, terr, Ord))
ggplot(ind292, aes(x = Ord, y = Value, col = as.character(Season))) + 
  geom_point() + 
  facet_wrap(~Metric, scales = "free", ncol = 7) + 
  geom_smooth() +
  ggtitle(label = "id292")

# id301
n301 <- as.numeric(table(trk$birdid) [7])
ind301 <- trk %>% filter(birdid == "301")  %>% arrange(fileid) %>% mutate(Ord = seq(1:n301)) %>% 
  gather(key = "Metric", value = "Value", - c(fileid, birdid, Season, terr, Ord)) +
  ggtitle(label = "id301")
ggplot(ind301, aes(x = Ord, y = Value, col = as.character(Season))) + 
  geom_point() + 
  facet_wrap(~Metric, scales = "free", ncol = 7) + 
  geom_smooth() +
  ggtitle(label = "id301")


# id312
n312 <- as.numeric(table(trk$birdid) [9])
ind312 <- trk %>% filter(birdid == "312")  %>% arrange(fileid) %>% mutate(Ord = seq(1:n312)) %>% 
  gather(key = "Metric", value = "Value", - c(fileid, birdid, Season, terr, Ord))
ggplot(ind312, aes(x = Ord, y = Value, col = as.character(Season))) + 
  geom_point() + 
  facet_wrap(~Metric, scales = "free", ncol = 7) + 
  geom_smooth() +
  ggtitle(label = "id312")


# id312
n316 <- as.numeric(table(trk$birdid) [11])
ind316 <- trk %>% filter(birdid == "316")  %>% arrange(fileid) %>% mutate(Ord = seq(1:n316)) %>% 
  gather(key = "Metric", value = "Value", - c(fileid, birdid, Season, terr, Ord))
ggplot(ind316, aes(x = Ord, y = Value, col = as.character(Season))) + 
  geom_point() + 
  facet_wrap(~Metric, scales = "free", ncol = 7) + 
  geom_smooth() +
  ggtitle(label = "id316")



# id364
n364 <- as.numeric(table(trk$birdid) [17])
ind364 <- trk %>% filter(birdid == "364")  %>% arrange(fileid) %>% mutate(Ord = seq(1:n364)) %>% 
  gather(key = "Metric", value = "Value", - c(fileid, birdid, Season, terr, Ord))
ggplot(ind364, aes(x = Ord, y = Value, col = as.character(Season))) + 
  geom_point() + 
  facet_wrap(~Metric, scales = "free", ncol = 7) + 
  geom_smooth() +
  ggtitle(label = "id364")


# territorial vs floaters

trk_tf <- trk %>% filter(terr != "Unknown")  
trk_tf <- droplevels(trk_tf)
trk_tf <- gather(data = trk_tf, key = "Metric", value = "Value", - c(fileid, birdid, Season, terr))

ggplot(trk_tf, aes(x = terr, y = Value)) + geom_boxplot() + facet_wrap(~Metric, scales = "free")

# Foraging efficiency
trk_fe <- trk %>% filter(terr != "Unknown")  
trk_fe <- droplevels(trk_fe)



model1 <- lm(durIns_sum ~ durOuts_mean + I(Nouts/totFor) + arousal + terr + as.character(Season), data = trk_fe)
summary(model1)

ggplot(trk_fe, aes(x = durOuts_mean, y = durIns_sum, col = as.character(Season))) + geom_point() + geom_smooth(method = "lm")



model2 <- lm(log(durIns_mean) ~ durOuts_mean + I(Nouts/totFor) + arousal + terr + as.character(Season), data = trk_fe)
summary(model2)

ggplot(trk_fe, aes(x = durOuts_mean, y = log(durIns_mean), col = as.character(Season))) + geom_point() + geom_smooth(method = "lm")


model3 <- lm(durIns_sum ~ durIns_mean + I(Nouts/totFor) + arousal + terr + as.character(Season), data = trk_fe)
summary(model3)

ggplot(trk_fe, aes(x = durIns_sum, y = durIns_mean, col = as.character(Season))) + geom_point() + geom_smooth(method = "lm")


trkcor <-   trk[,5:11]
trkcor <- na.omit(trkcor)
cor.mat <- round(cor(trkcor),2)


############################
# Clustering based on arousal and mean duration of breaks, are they clear behavioural groups?

trkcl <- trk %>%
  group_by(birdid) %>%
  summarise(arousalmean = mean(arousal),
            totintakemean = mean(durIns_sum),
            durOutssummen = mean(durOuts_mean))


# All vars pooled 
trkcl_all <- na.omit(trkcl)
rownames(trkcl_all) <- as.character(trkcl_all$birdid)
trkcl_all$birdid <- NULL

## Data evaluation - Hopkins stat
get_clust_tendency(scale(trkcl_all), n = 2) # indicate on homogeneity

trkcl_d <- hclust(dist(trkcl_all))
plot(trkcl_d, main = "all vars pooled")



# Arousal
trkcl_a <- trkcl %>%
  select(birdid, arousalmean)
ggplot(trkcl_a, aes(x = as.character(birdid), y = arousalmean)) + geom_point(size = 2)

trkcl_a <- na.omit(trkcl_a)
rownames(trkcl_a) <- as.character(trkcl_a$birdid)
trkcl_a$birdid <- NULL

## Data evaluation - Hopkins stat
get_clust_tendency(trkcl_a, n = 3) # indicate on homogeneity

trkcl_d <- hclust(dist(trkcl_a))
plot(trkcl_d, main = "arousal")


# Breaks duration
trkcl_b <- trkcl %>%
  select(birdid, durOutssummen)
ggplot(trkcl_b, aes(x = as.character(birdid), y = durOutssummen)) + geom_point(size = 2)

trkcl_b <- na.omit(trkcl_b)
rownames(trkcl_b) <- as.character(trkcl_b$birdid)
trkcl_b$birdid <- NULL

## Data evaluation - Hopkins stat
get_clust_tendency(trkcl_b, n = 2) # can be clustered

trkcl_d <- hclust(dist(trkcl_b))
plot(trkcl_d, main = "breaks duration")

# Alternative method of clustering
br <- hcut(trkcl_b, k = 2,
      stand = TRUE)

fviz_dend(br)
fviz_silhouette(br,  print.summary = TRUE)

# Validation of the clustering method
val1 <- clValid::clValid(scale(trkcl_b),nClust = 2,
                 clMethods = c("hierarchial", "kmeans"),
                 validation = "internal")


# Total energy intake
trkcl_e <- trkcl %>%
  select(birdid, totintakemean)
ggplot(trkcl_e, aes(x = as.character(birdid), y = totintakemean)) + geom_point(size = 2)

trkcl_e <- na.omit(trkcl_e)
rownames(trkcl_e) <- as.character(trkcl_e$birdid)
trkcl_e$birdid <- NULL

## Data evaluation - Hopkins stat
get_clust_tendency(trkcl_e, n = 2) # can be clustered


trkcl_d <- hclust(dist(trkcl_e))
plot(trkcl_d, main = "total energy intake")

# Alternative method of clustering
tot <- hcut(trkcl_e, k = 2,
           stand = TRUE)

fviz_dend(tot)
fviz_silhouette(tot,  print.summary = TRUE)


# Are individuals consistent? Some different form each other?

pop_durOutsmean <- mean(trk$durOuts_mean)
pop_durInssum <- mean(trk$durIns_sum)
pop_arousal <- mean(trk$arousal, na.rm = TRUE)

ggplot(trk, aes(x = as.character(birdid), y = durOuts_mean)) + geom_point() +
  geom_hline(aes(yintercept = pop_durOutsmean))

# Select in with <3 records
inclinx <- which(table(trk$birdid)>3)
trk_indsel <- trk %>% 
  filter(trk$birdid %in% names(inclinx))

ggplot(trk_indsel, aes(x = as.character(birdid), y = durOuts_mean, col = as.character(Season))) + geom_boxplot() + geom_point() +
  geom_hline(aes(yintercept = pop_durOutsmean)) + ggtitle(label = "Breaks duration")

ggplot(trk_indsel, aes(x = as.character(birdid), y = durIns_sum, col = as.character(Season))) + geom_boxplot() + geom_point() +
  geom_hline(aes(yintercept = pop_durInssum)) + ggtitle(label = "Total intake")

ggplot(trk_indsel, aes(x = as.character(birdid), y = arousal, col = as.character(Season))) + geom_boxplot() + geom_point() +
  geom_hline(aes(yintercept = pop_arousal)) + ggtitle(label = "Arousal")


# PCA
names(trk)
trk_pca <- trk %>% select(c(5:11)) 


library(car)
pca_res <- PCA(trk_pca, graph = TRUE)
summary(pca_res)
fviz_pca_var(pca_res)
fviz_pca_ind(pca_res)
fviz_screeplot(pca_res)
fviz_pca_biplot(pca_res)


# Extract loadings for particular ind
indx <- pca_res$ind$coord

indx_df <- data.frame(pc1 = indx[,1], pc2 = indx[,2])
trkid <- trk %>% select(2)
indx_df <- cbind(indx_df, trkid)

indx_df <- indx_df %>% group_by(birdid) %>% summarise(pc1m = mean(pc1),
                                                      pc2m = mean(pc2))

# Clustering of PCs

rownames(indx_df) <- as.character(indx_df$birdid)
indx_df$birdid <- NULL

## Data evaluation - Hopkins stat
get_clust_tendency(indx_df, n = 2) # can be clustered

pcacl <- hcut(indx_df, k = 2,
            stand = TRUE)


fviz_dend(pcacl)
fviz_silhouette(pcacl,  print.summary = TRUE)

cl1 <- names(which(pcacl$cluster == 1))
cl2 <- names(which(pcacl$cluster == 2))
cluster <- c(rep(1,length(cl1)), rep(2,length(cl2)))
clid <- c(cl1,cl2)

cl_df <- data.frame(cluster = cluster, birdid = clid)

# Needed to be rep 
indx_df <- data.frame(pc1 = indx[,1], pc2 = indx[,2])
trkid <- trk %>% select(2) %>% mutate(birdid = as.factor(birdid))
indx_df <- cbind(indx_df, trkid)

indx_df <- indx_df %>% group_by(birdid) %>% summarise(pc1m = mean(pc1),
                                                      pc2m = mean(pc2))

pca_cl_df <- left_join(indx_df, cl_df)

ggplot(pca_cl_df, aes(x = pc1m, y = pc2m, col = as.factor(cluster))) + geom_point(size = 3)
