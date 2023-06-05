library(tidyverse)
library(lmerTest)
library(lme4)
library(smatr)


# Data preparation --------------------------------------------------------

setwd("C:/Users/KWJ/Dropbox/FF_fear and foraging")

ff <- read.csv("persIDc.csv", sep = ";")
ff <- ff %>% mutate(for_eff = tot_durins/tot_durfor,
                    treat_num = substr(ff$Configuration, 
                                       nchar(as.character(ff$Configuration)), 
                                       nchar(as.character(ff$Configuration))))


# # ffc <- ff %>% filter(treat == "Ctr") # control set
# # ffe <- ff %>% filter(treat != "Ctr") # experimental set
# # 
# # ff_fill <- left_join(ffc, ffe, by = c("ID..", "treat_num") )
# 
# multmut <- function (x) {if_else(is.na(x), mean(x, na.rm = TRUE), as.numeric(x))}
# 
# ff_fillx <- ff_fill %>% 
#   select (-c(Video.cut.x, abs_nflo.x, nflo_chang.x, nouts.x, nouts.x, 
#              treat.x, Configuration.x, Color.code.x, 
#              stan_nflo.x, stan_nflochang.x, stan_nouts.x, stan_nins.x,
#              
#              Video.cut.y, abs_nflo.y, nflo_chang.y, nouts.y, nouts.y, 
#              treat.y, Configuration.y, Color.code.y, 
#              stan_nflo.y, stan_nflochang.y, stan_nouts.y, stan_nins.y))%>% 
#   group_by(ID..) %>% 
#   mutate_all(funs(multmut))
# 
# ff$treat.num <- as.numeric(ff$treat) - 1
# 


setwd("C:/Users/KWJ/Dropbox/FF_fear and foraging/Personality exp_Data/Personality exp_latency")

lat <- read.csv("latency.csv", sep = ",")
lat <- lat %>% mutate(VideoID = substr(RecID, 38, 60)) %>% 
  rename(ID.. = NoID)

ff <- ff %>% mutate(VideoID = substr(Video.cut, 38, 60))

ff_lat <- left_join(ff, lat, by = c("VideoID", "ID.."))


cog <- read.csv("C:/Users/KWJ/Dropbox/FF_fear and foraging/Cognition data subset for Kasia sep-17.csv")

cog_means <- cog %>% group_by(ID) %>% 
  summarise(cog.mean = mean(cog.score)) %>% 
  rename(ID.. = ID)

pers_means_ctrl <- ff_lat %>% 
  filter(ConfSimpl == "Ctr") %>% 
  group_by(ID..) %>%
  summarise(fer.mean = mean(mov_feroc),
            exp.mean = mean(stan_nflo),
            lat.mean = mean(Latency)) 

cog_pers <- left_join(cog_means, pers_means_ctrl, by = "ID..")

library(openxlsx)
write.xlsx(ff, "C:/Users/KWJ/Dropbox/FF_fear and foraging/ff.xlsx",
           append = TRUE)

write.xlsx(ff_lat, "C:/Users/KWJ/Dropbox/FF_fear and foraging/ff_lat.xlsx",
           append = TRUE)

write.xlsx(cog_pers, "C:/Users/KWJ/Dropbox/FF_fear and foraging/cog_pers.xlsx",
           append = TRUE)


# Variables - test --------------------------------------------------------

cm <- ff_lat[, c("stan_nflo", "Latency", "mov_feroc", "for_eff")]
cm <- cm[complete.cases(cm),]
cm_corr <- cor(cm)
corrplot::corrplot.mixed(cm_corr)



# Arousal ---------------------------------------------------

model.arous <- lmer(for_eff ~ treat * mov_feroc/nflo_chang + (1 | ID..), data = ff, REML = FALSE)
summary(model.arous)
names(ff)
x11()
ggplot(aes(y  = for_eff , 
           x = mov_feroc/nflo_chang), 
       data = ff) +
  facet_wrap(~ treat) +
  geom_point() +
  labs(x = "Arousal (coeficient variance of deviations from the feeders/number of feeder changes)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_classic()


# arousal - Ctr vs Exp

fft <- ff %>% group_by(ID.., treat) %>% 
  summarise (mmov_feroc = mean(mov_feroc)) %>% 
  spread(key = treat, value = mmov_feroc, NA) %>% 
  remove_missing() %>% 
  gather(key = treat, value = mmov_feroc, -ID..)

fft <- droplevels(fft)

x11()
ggplot(aes(y  = mmov_feroc , x = treat, group = as.character(ID..)), data = fft) +
  geom_line() +
  labs(x = "treatment", y =  "arousal") +
  theme_classic()




# Explorative behaviour --------------------------------------

model.explr <- lmer(for_eff ~ treat *  stan_nflo + (1 | ID..), data = ff[ff$stan_nflo<0.5,], REML = FALSE)
summary(model.explr)

model.explr_fix <- lm(for_eff ~ treat *  stan_nflo, data = ff)


anova(model.explr, model.explr_fix)


model.explr_ctr <- lmer(for_eff ~ stan_nflo + (1 | ID..), data = ff[ff$treat == "Ctr",], REML = FALSE)
summary(model.explr_ctr)


model.explr_exp <- lmer(for_eff ~ stan_nflo + (1 | ID..), data = ff[ff$treat == "Exp" | ff$stan_nflo < 0.5,], REML = FALSE)
summary(model.explr_exp)

ggplot(aes(y  = for_eff , x = stan_nflo), data = ff) +
  facet_wrap(~ treat, scales = "free_x") + 
  geom_point() +
  labs(x = "Explorative behavior (# visited feeders/total foraging duration)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_classic()


# exploration - ctrl vs exp

fft <- ff %>% group_by(ID.., treat) %>% 
  summarise (mstan_nflo = mean(stan_nflo)) %>% 
  spread(key = treat, value = mstan_nflo, NA) %>% 
  remove_missing() %>% 
  gather(key = treat, value = mstan_nflo, -ID..)

fft <- droplevels(fft)

x11()
ggplot(aes(y  = mstan_nflo , x = treat, group = as.character(ID..)), data = fft) +
  geom_line() +
  labs(x = "treatment", y =  "# visited feeders") +
  theme_classic()

# ### Risk avoidance

# Risk avoidance - latency ------------------------------------------------

model.risk.lat <- lmer(for_eff ~ treat *  Latency + (1 | ID..), data = ff_lat, REML = FALSE)
summary(model.risk.lat)

x11()
ggplot(aes(y  = for_eff , x = Latency), data = ff_lat) +
  facet_wrap(~ treat) + 
  geom_point() +
  labs(x = "Risk avoidance (latency)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_classic()


# risk - ctrl vs exp

fft <- ff_lat %>% group_by(ID.., treat) %>% 
  summarise (Latency = mean(Latency)) %>% 
  spread(key = treat, value = Latency, NA) %>% 
  remove_missing() %>% 
  gather(key = treat, value = Latency, -ID..)

fft <- droplevels(fft)

x11()
ggplot(aes(y  = Latency , x = treat, group = as.character(ID..)), data = fft) +
  geom_line() +
  labs(x = "treatment", y =  "# visited feeders") +
  theme_classic()


# Repitabilly -------------------------------------------------------------


# arousal
model.arous.rep <- lmer(mov_feroc ~ 0 + (1 | ID..), data = ff, REML = TRUE)
sum.ar.rep <- summary(model.arous.rep)
vr <- as.data.frame(sum.ar.rep$varcor)[,4]

among <- vr[1] # variance among groups (individuals)
wth <- vr[2] # variance within groups

among/(among + wth)


# expolation - n flowers
model.flo.rep <- lmer(stan_nflo ~ 0 + (1 | ID..), data = ff, REML = TRUE)
sum.fl.rep <- summary(model.flo.rep)
vr <- as.data.frame(sum.fl.rep$varcor)[,4]

among <- vr[1] # variance among groups (individuals)
wth <- vr[2] # variance within groups

among/(among + wth)


# risk - latency
model.lat.rep <- lmer(Latency ~ 0 + (1 | ID..), data = ff_lat, REML = TRUE)
sum.lat.rep <- summary(model.lat.rep)
vr <- as.data.frame(sum.lat.rep$varcor)[,4]

among <- vr[1] # variance among groups (individuals)
wth <- vr[2] # variance within groups

among/(among + wth)

# another way to calculate repeatability  -control
library(rptR)

rpt(mov_feroc ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 0)
rpt(stan_nflo ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 0)
rpt(Latency ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 0)

rpt(mov_feroc ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Exp",], grname = "ID..", nboot = 100, npermut = 0)
rpt(stan_nflo ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Exp",], grname = "ID..", nboot = 100, npermut = 0)
rpt(Latency ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Exp",], grname = "ID..", nboot = 100, npermut = 0)


# Full model

# ctr
full.model.ctr1 <- lmer(for_eff ~ Latency + stan_nflo + mov_feroc + (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Ctr",], REML = FALSE)
summary(full.model.ctr1)

full.model.ctr0 <- lm(for_eff ~ Latency + stan_nflo + mov_feroc, data = ff_lat[ff_lat$ConfSimpl == "Ctr",])
summary(full.model.ctr0)

anova(full.model.ctr1, full.model.ctr0)

# interactions
full.model.ctr_int <- lmer(for_eff ~ Latency * stan_nflo * mov_feroc + (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Ctr",], REML = FALSE)
summary(full.model.ctr_int)

full.model.exp_int <- lmer(for_eff ~ Latency * stan_nflo * mov_feroc + (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Exp",], REML = FALSE)
summary(full.model.exp_int)

# exp
full.model.exp1 <- lmer(for_eff ~ Latency + stan_nflo + mov_feroc + (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Exp",], REML = FALSE)
summary(full.model.exp1)

full.model.exp0 <- lm(for_eff ~ Latency + stan_nflo + mov_feroc, data = ff_lat[ff_lat$ConfSimpl == "Exp",])
summary(full.model.exp0)

anova(full.model.exp1, full.model.exp0)

# plot
ff_lat <- remove_missing(ff_lat)


# CTRL
# stan_nflo & mov_feroc
library(rgl)
library(car)
library(digest)
ctr_3pl <- ff_lat %>% filter(ConfSimpl == "Exp")
# x <- ctr_3pl %>% select(stan_nflo)
x <- ctr_3pl %>% select(mov_feroc);range(x)
z <- ctr_3pl %>% select(Latency)
# y <- ctr_3pl %>% select(mov_feroc)
y <- ctr_3pl %>% select(for_eff)


x <-  as.matrix((max(x, na.rm = TRUE) - x)/max(x, na.rm = TRUE))
y <-  as.matrix((max(y, na.rm = TRUE) - y)/max(y, na.rm = TRUE))
z <-  as.matrix((max(z, na.rm = TRUE) - z)/max(z, na.rm = TRUE))

scatter3d(x, y, z, 
          grid = T,
          surface = T,
          xlab = "stan_nflo",
          zlab = "Latency",
          ylab = "Foraging efficiency",
          revolutions= 0.1)




