
library(openxlsx)
library(readxl)
library(tidyverse)
library(lmerTest)
library(lme4)
library(smatr)
library(ggpubr)



# Data --------------------------------------------------------
# Data arealdy prepared (see script from Sept 2019)

rm(list = ls())

ff <- read_excel("C:/Users/KWJ/Dropbox/FF_fear and foraging/ff.xlsx")
ff_lat <- read_excel("C:/Users/KWJ/Dropbox/FF_fear and foraging/ff_lat.xlsx")
cog_pers <- read_excel("C:/Users/KWJ/Dropbox/FF_fear and foraging/cog_pers.xlsx")


# Some data tuning
# Standardizing the arousal
ff <- ff %>% mutate(mov_feroc_stand = mov_feroc/(nflo_chang+1))
ff_lat <- ff_lat %>% mutate(mov_feroc_stand = mov_feroc/(nflo_chang+1))

# Variables choice  --------------------------------------------------------

vars <- ff_lat %>% 
  select(Conf, treat, ID.., mean_durins, tot_durins, mean_durouts, tot_durouts, tot_durfor,
         mov_totdist, mov_spead, stan_nflochang,  stan_nouts, stan_nins, stand_totdist,
         for_eff, mov_feroc_stand) %>%
  gather(key = "key", value = "value", -c(Conf, treat, ID..))


ggplot(vars, aes(x = treat, y = value)) + 
  geom_boxplot() +
  facet_wrap(~ key, scales = "free")

# Variables - test --------------------------------------------------------

cm <- ff_lat[, c("stan_nflo", "Latency", "mov_feroc_stand")]
cm <- cm[complete.cases(cm),]
cm_corr <- cor(cm)
corrplot::corrplot.mixed(cm_corr)



modelall_ctr3 <- lmer(for_eff ~ mov_feroc_stand + Latency + stan_nflo  + (1 | ID..), data = ff_lat[ff_lat$treat == "Ctr", ], REML = FALSE)
summary(modelall_ctr3)


modelall_exp3 <- lmer(for_eff ~ mov_feroc_stand + Latency + stan_nflo  + (1 | ID..), data = ff_lat[ff_lat$treat == "Exp", ], REML = FALSE)
summary(modelall_exp3)


forplot <- ggplot(data = ff_lat, aes(x = treat, y = for_eff)) +
  geom_boxplot() +
  labs(x = "", y =  "Foraging efficiency") +
  theme_bw()




foreff_model <- lmer(for_eff ~ treat  + (1 | ID..), data = ff_lat, REML = FALSE); summary(foreff_model)

foreff_model_fix <- lm(for_eff ~ treat, data = ff_lat) 
anova(foreff_model, foreff_model_fix)

foreff_long <- ff_lat %>% group_by(ID.., treat) %>% 
  summarise (mfor_eff = mean(for_eff)) %>% 
  spread(key = treat, value = mfor_eff, NA) %>% 
  remove_missing() %>% 
  gather(key = treat, value = mfor_eff, -ID..)


indplot <- ggplot(data = foreff_long, aes(x = treat, y = mfor_eff)) +
  geom_point(size = 2) + geom_line(aes(group = as.factor(ID..))) + theme_bw() +
  labs(x = "", y = "Average foraging efficiency")

ggplot(data = ff_lat, aes(x = treat, y = for_eff)) +
  facet_wrap(~ ID..) +
  geom_boxplot()


ggarrange(forplot, indplot, labels = c("A", "B"),
          ncol = 2, nrow = 1)
ggsave(filename = "forplots.tiff", plot = last_plot(),dpi = 300)


# Arousal ---------------------------------------------------

model.arous <- lmer(for_eff ~ treat * mov_feroc_stand + (1 | ID..), 
                    data = ff_lat, REML = FALSE)
summary(model.arous)

arousalplot <- ggplot(aes(y  = for_eff , 
           x = mov_feroc_stand), 
       data = ff_lat) +
  facet_wrap(~ treat, scales = "free_x") +
  geom_point() +
  labs(x = "Arousal (coeficient variance of deviations from the feeders/number of feeder changes)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_bw()


# Explorative behaviour --------------------------------------

model.explr <- lmer(for_eff ~ treat *  stan_nflo + (1 | ID..), data = ff_lat, REML = FALSE)
summary(model.explr)

model.explr_fix <- lm(for_eff ~ treat *  stan_nflo, data = ff_lat)
anova(model.explr, model.explr_fix)


explot <- ggplot(aes(y  = for_eff , x = stan_nflo), data = ff_lat) +
  facet_wrap(~ treat, scales = "free_x") + 
  geom_point() +
  labs(x = "Explorative behavior (# visited feeders/total foraging duration)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_bw()



# Risk avoidance ------------------------------------------------

model.risk.lat <- lmer(for_eff ~ treat *  Latency + (1 | ID..), data = ff_lat, REML = FALSE)
summary(model.risk.lat)


riskplot <- ggplot(aes(y  = for_eff , x = Latency), data = ff_lat) +
  facet_wrap(~ treat, scales = "free_x") + 
  geom_point() +
  labs(x = "Risk avoidance (latency)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_bw()


# All behav plots -------------------------------------------------------------

# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")
library(ggpubr)


ggarrange(explot, riskplot, arousalplot, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)


ggsave(filename = "behavplots.tiff", plot = last_plot(),dpi = 300)

# Repitabilly -------------------------------------------------------------
set.seed(1212)

# arousal
model.arous.rep <- lmer(mov_feroc_stand ~ 0 + (1 | ID..), data = ff_lat[ff_lat$treat == "Ctr",], REML = TRUE)
sum.ar.rep <- summary(model.arous.rep)
vr <- as.data.frame(sum.ar.rep$varcor)[,4]

among <- vr[1] # variance among groups (individuals)
wth <- vr[2] # variance within groups

R_arous <- among/(among + wth);R_arous

# CI for the R
ff_simpl <- ff_lat %>% select(mov_feroc_stand, ID..,treat) %>% filter(treat == "Ctr")
ninds <- length(ff_simpl$ID..)
N <- 1000
Rb_arous <- numeric(N)

for (i in 1: N) {
  df_temp <- sample_n(ff_simpl, ninds, replace = TRUE)
  
  model <- lmer(mov_feroc_stand ~ 0 + (1 | ID..), data = df_temp, REML = TRUE)
  sum.model <- summary(model)
  vr <- as.data.frame(sum.model$varcor)[,4]
  
  among <- vr[1] # variance among groups (individuals)
  wth <- vr[2] # variance within groups
  
  Rb_arous[i] <- among/(among + wth)
  
}

R_arous
q1 <- quantile(Rb_arous, 0.025); q1 # 95% CI
q3 <- quantile(Rb_arous, 0.975); q3 # 95% CI

# expolation - n flowers
model.flo.rep <- lmer(stan_nflo ~ 0 + (1 | ID..), data = ff_lat[ff_lat$treat == "Ctr",], REML = TRUE)
sum.fl.rep <- summary(model.flo.rep)
vr <- as.data.frame(sum.fl.rep$varcor)[,4]

among <- vr[1] # variance among groups (individuals)
wth <- vr[2] # variance within groups

R_ex <- among/(among + wth); R_ex


# CI for the R
ff_simpl <- ff_lat %>% select(stan_nflo, ID..,treat) %>% filter(treat == "Ctr")
ninds <- length(ff_simpl$ID..)
N <- 1000
Rb_ex <- numeric(N)

for (i in 1: N) {
  df_temp <- sample_n(ff_simpl, ninds, replace = TRUE)
  
  model <- lmer(stan_nflo ~ 0 + (1 | ID..), data = df_temp, REML = TRUE)
  sum.model <- summary(model)
  vr <- as.data.frame(sum.model$varcor)[,4]
  among <- vr[1] # variance among groups (individuals)
  wth <- vr[2] # variance within groups
  
  Rb_ex[i] <- among/(among + wth)
  
}

q1 <- quantile(Rb_ex, 0.025);q1 # 95% CI
q3 <- quantile(Rb_ex, 0.975);q3 # 95% CI
R_ex

# risk - latency
model.lat.rep <- lmer(Latency ~ 0 + (1 | ID..), data = ff_lat[ff_lat$treat == "Ctr",], REML = TRUE)
sum.lat.rep <- summary(model.lat.rep)
vr <- as.data.frame(sum.lat.rep$varcor)[,4]

among <- vr[1] # variance among groups (individuals)
wth <- vr[2] # variance within groups

R_lat <-  among/(among + wth); R_lat

# CI for the R
ff_simpl <- ff_lat %>% select(Latency, ID..,treat) %>% filter(treat == "Ctr")
ninds <- length(ff_simpl$ID..)
N <- 1000
Rb_lat <- numeric(N)

for (i in 1: N) {
  df_temp <- sample_n(ff_simpl, ninds, replace = TRUE)
  
  model <- lmer(Latency ~ 0 + (1 | ID..), data = df_temp, REML = TRUE)
  sum.model <- summary(model)
  vr <- as.data.frame(sum.model$varcor)[,4]
  among <- vr[1] # variance among groups (individuals)
  wth <- vr[2] # variance within groups
  
  Rb_lat[i] <- among/(among + wth)
  
}

q1 <- quantile(Rb_lat, 0.025);q1 # 95% CI
q3 <- quantile(Rb_lat, 0.975);q3 # 95% CI
R_lat

# another way to calculate repeatability  -control
library(rptR)

rpt(mov_feroc_stand ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 10)

rpt(mov_feroc_stand ~ treat + (1 | ID..), data = ff_lat, grname = "ID..", nboot = 100, npermut = 10)

rpt(stan_nflo ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 10)

rpt(stan_nflo ~ treat + (1 | ID..), data = ff_lat, grname = "ID..", nboot = 100, npermut = 10)

rpt(Latency ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 10)

rpt(Latency ~ treat + (1 | ID..), data = ff_lat, grname = "ID..", nboot = 100, npermut = 10)



rpt(mov_feroc_stand ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Exp",], grname = "ID..", nboot = 100, npermut = 0)


rpt(stan_nflo ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Exp",], grname = "ID..", nboot = 100, npermut = 0)
rpt(Latency ~ (1 | ID..), data = ff_lat[ff_lat$ConfSimpl == "Exp",], grname = "ID..", nboot = 100, npermut = 0)


# Full model

full.model.ID <- lmer(for_eff ~ treat + (1 | ID..), data = ff_lat, REML = FALSE)
summary(full.model.ID)

full.model.ID0 <- lm(for_eff ~ treat, data = ff_lat)
summary(full.model.ID0)



anova(full.model.ID0, full.model.ID)

full.model.ctr1 <- lmer(for_eff ~ Latency * treat * stan_nflo * treat * mov_feroc_stand * treat + (1 | ID..), data = ff_lat, REML = TRUE)
summary(full.model.ctr1)


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


names()

ff_lat %>% 
  filter(!is.na(ConfSimpl)) %>% 
  ggplot() + 
  # geom_boxplot(aes(x = ConfSimpl, y = stan_nflo))
  geom_boxplot(aes(x = ConfSimpl, y = mov_feroc_stand))
  # geom_boxplot(aes(x = ConfSimpl, y = Latency))


model1 <- lmer(data = ff_lat, mov_feroc_stand ~ ConfSimpl + (1 | ID..))
summary(model1)


model2 <- lmer(data = ff_lat, stan_nflo ~ ConfSimpl + (1 | ID..))
summary(model2)

model3 <- lmer(data = ff_lat, Latency ~ ConfSimpl + (1 | ID..))
summary(model3)




# Foraging efficiency - three models - randomization ----------------------


# basic model

# Exploratory behav data
df_basic_sel <- ff_lat %>% select(for_eff, treat, ID.., stan_nflo) %>% 
rename(param = stan_nflo)

# Risk-avoidance behav data
df_basic_sel <- ff_lat %>% select(for_eff, treat, ID.., Latency) %>% 
  rename(param = Latency)

# Arousal behav data
df_basic_sel <- ff_lat %>% select(for_eff, treat, ID.., mov_feroc_stand) %>% rename(param = mov_feroc_stand)


# test for the ID significance

modID <- lmer(data = df_basic_sel, for_eff ~ treat * stan_nflo + (1|ID..), REML = FALSE)
mod0 <- lm(data = df_basic_sel, for_eff ~ treat * stan_nflo)
anova(modID, mod0)

modID <- lmer(data = df_basic_sel, for_eff ~ treat * Latency + (1|ID..), REML = FALSE)
mod0 <- lm(data = df_basic_sel, for_eff ~ treat * Latency)
anova(modID, mod0)

modID <- lmer(data = df_basic_sel, for_eff ~ treat * mov_feroc_stand + (1|ID..), REML = FALSE)
mod0 <- lm(data = df_basic_sel, for_eff ~ treat * mov_feroc_stand)
anova(modID, mod0)


# Basic model for a given parameter
basic_model <- lmer(for_eff ~ treat *  param + (1 | ID..), data = df_basic_sel, REML = FALSE)
  basicmodel_sum <- summary(basic_model)

#real coeficients
real_treatment <- basicmodel_sum$coefficients[2]
real_parameter <- basicmodel_sum$coefficients[3]
real_interaction <- basicmodel_sum$coefficients[4]


# Randomization
set.seed(1313)

N <- 1000

rand_treatment <- numeric()
rand_parameter <- numeric()
rand_interaction <- numeric()

for (i in 1:N) {
  
  df_temp <- df_basic_sel %>% sample_n(size = nrow(df_basic_sel), replace = TRUE)
  
  mod_temp <- lmer(for_eff ~ treat *  param + (1 | ID..), data = df_temp, REML = FALSE)
  mod_sum <- summary(mod_temp)
  
  rand_treatment[i] <- mod_sum$coefficients[2]
  rand_parameter[i] <- mod_sum$coefficients[3]
  rand_interaction[i] <- mod_sum$coefficients[4]
  
}


treatment <- rand_treatment
parameter <- rand_parameter
interaction <- rand_interaction

key <- c(rep("treatment", N),
         rep("parameter", N),
         rep("interaction", N))

val <- c(treatment, parameter, interaction)

df <- data.frame(key, val)



# Randomization - PLOTS ---------------------------------------------------

# parameter

pval_param <- (sum(rand_parameter<0))/N

plot_param <- ggplot(data = df[df$key == "parameter",]) + geom_density(aes(x = val), fill = "lightgrey") +
geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = real_parameter), col = "darkblue", size = 1.2) +
  theme_classic()+
  scale_x_continuous(expand = c(0,0), name = paste0("Estimate, P = ", pval_param, sep = "")) +
  scale_y_continuous(expand = c(0,0), name = "Density")

# Treatment 

pval_treat <- (sum(rand_treatment>0))/N

plot_treat <- ggplot(data = df[df$key == "treatment",], 
                     aes(x = val)) + 
  geom_density(fill = "lightgrey") + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = real_treatment), col = "darkblue", size = 1.2) +
  theme_classic()+
  scale_x_continuous(expand = c(0,0), 
                     name = paste0("Estimate, P = ", pval_treat, sep = "")) +
  scale_y_continuous(expand = c(0,0), name = "Density") 

# Interaction

pval_inter <- (sum(rand_interaction<0))/N

plot_inter <- ggplot(data = df[df$key == "interaction",], 
                     aes(x = val)) + 
  geom_density(fill = "lightgrey") + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = real_interaction), col = "darkblue", size = 1.2) +
  theme_classic()+
  scale_x_continuous(expand = c(0,0),limits = c(-0.05,0.3), name = paste0("Estimate, P = ", pval_inter, sep = "")) +
  scale_y_continuous(expand = c(0,0), name = "Density")

x11()
ggarrange(plot_param, plot_treat, plot_inter, ncol = 3, labels = "AUTO")

# ggsave(filename = "C:/Users/KWJ/Dropbox/FF_fear and foraging/explo_random.jpg", 
#        plot = last_plot(),dpi = 300 )

# ggsave(filename = "C:/Users/KWJ/Dropbox/FF_fear and foraging/risk_random.jpg", 
#        plot = last_plot(),dpi = 300 )

# ggsave(filename = "C:/Users/KWJ/Dropbox/FF_fear and foraging/arousal.jpg", 
#        plot = last_plot(),dpi = 300 )



# behaviours - ctr vs exp -------------------------------------------------
names(ff_lat)
df_behav <- ff_lat %>% 
  select(treat, ID.., for_eff, stan_nflo, Latency, mov_feroc_stand)  %>% 
  gather(key = "param", value = "val", -c(treat, ID..))

prm <- unique(df_behav$param)
N <- 1000
EST <- list()

set.seed(1212)

for(i in 1:length(prm)) {

    df_beh <- df_behav %>% filter(param == prm[i])
  
  #split data into ctr and exp set to sample separately for the two groups
  df_beh_ctr <- df_beh %>% filter(treat == "Ctr")
  df_beh_exp <- df_beh %>% filter(treat == "Exp")
 
  est <- numeric()
  
  for (j in 1:N) {
    
    df_ctr <- sample_n(df_beh_ctr, nrow(df_beh_ctr), replace = TRUE)
    df_exp <- sample_n(df_beh_exp, nrow(df_beh_exp), replace = TRUE)
    df_behv <- rbind(df_ctr, df_exp)
    
    beh_mod <- lmer(data = df_behv, val ~ treat + (1|ID..))
    beh_mod_sum <- summary(beh_mod)
    
    est[j] <- beh_mod_sum$coefficients[2]
    
  }
  
  EST[[i]] <- est
}
  

behav_random <- data.frame(param_rand = rep(prm, each = N)) 
behav_random$estimate <- c(EST[[1]], EST[[2]], EST[[3]],  EST[[4]])

# P value
behav_random %>% group_by(param_rand) %>% 
  mutate(P= if_else(param_rand == "stan_nflo", estimate<0,
                       if_else(param_rand == "Latency",estimate<0,
                               estimate>0))) %>% 
  summarise(sum(P)/N)



x11()
ggplot() + geom_density(data = behav_random, aes(x = estimate)) + facet_wrap(~param_rand, scales = "free")

mod_exp <- lmer(data = df_behav[df_behav$param == "stan_nflo", ], val ~ treat + (1|ID..), REML = FALSE) 
summary(mod_exp)  

mod_risk <- lmer(data = df_behav[df_behav$param == "Latency", ], val ~ treat + (1|ID..), REML = FALSE) 
summary(mod_risk)  

mod_arousal <- lmer(data = df_behav[df_behav$param == "mov_feroc_stand", ], val ~ treat + (1|ID..), REML = FALSE) 
summary(mod_arousal)  

df_behav_ind <- df_behav %>% 
  group_by(param, treat, ID..) %>% 
  summarise(val = mean(val)) %>% 
  ungroup() %>% 
  mutate(param = if_else(param == "mov_feroc_stand", "Arousal",
                 if_else(param == "for_eff", "Foraging efficiency",
                         if_else(param == "stan_nflo", "Exporation", "Risk-avoidance")))) %>% 
  mutate(BirdID = as.factor(ID..) )




df_behav <- df_behav %>% 
  mutate(param = if_else(param == "mov_feroc_stand", "Arousal", 
                         if_else(param == "for_eff", "Foraging efficiency",
                               if_else(param == "stan_nflo", "Exporation", "Risk-avoidance")))) %>% 
  mutate(BirdID = as.factor(ID..) )


df_behav$param_f  <-  factor(df_behav$param, 
                             levels=c('Foraging efficency',
                                      'Exploration',
                                      'Risk-avoidance',
                                      'Arousal'))

df_behav_ind$param_f  <-  factor(df_behav_ind$param, 
                             levels=c('Foraging efficency',
                                      'Exploration',
                                      'Risk-avoidance',
                                      'Arousal'))

x11()
ggplot(data = df_behav, aes(x = treat, y = val)) + 
  geom_violin() +
  geom_point(data = df_behav_ind, aes(x = treat, y = val, col = BirdID)) +
  geom_line(data = df_behav_ind, aes(x = treat, y = val, group = BirdID))  +
  facet_wrap(~param, scales = "free") +
  theme_bw()

dev.off()



# Various  basic stats ----------------------------------------------------

vbs <- read_excel("F:/Dropbox_20190410/LBH pooled data/Cognition-personality/Cognitive experiments 2013-2015.xls", sheet = 3)

vbs <- vbs %>% select(Lek, Date, `Color code`, Time) %>% 
  mutate(Hr = lubridate::hour(Time),
         Seas = lubridate::year(Date),
         Day = lubridate::day(Date),
         Mth = lubridate::month(Date)) %>% select (- c(Time, Date)) %>% rename(Colcode = `Color code`) 
vbs <- vbs %>% filter(Lek %in% c("SUR Cacao", "SUR Sura"),
                      Seas == 2015)

vbs %>% group_by (Mth, Day, Hr, Colcode) %>%  summarise(n = n()) %>% ungroup() %>% summary(n)

vbs %>% group_by (Mth, Day) %>%  summarise(hr_min = min(Hr),
                                           hr_max = max(Hr)) %>% 
  mutate(dur = hr_max - hr_min) %>% ungroup() %>% summary()
