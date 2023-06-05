rm(list = ls())

library(openxlsx)
library(readxl)
library(tidyverse)
library(lmerTest)
library(lme4)
library(smatr)
library(ggpubr)



# Data --------------------------------------------------------
ff <- read_excel("C:/Users/KWJ/Desktop/ff.xlsx")

# Foraging efficency -------------------------------------------------------

# overall pattern - ctrl vs treat

# plot
forplot <- ggplot(data = ff, aes(x = treat, y = for_eff)) +
  geom_boxplot() +
  labs(x = "", y =  "Foraging efficiency") +
  theme_bw();forplot

# test
foreff_model <- lmer(for_eff ~ treat  + (1 | ID..), data = ff, REML = FALSE); summary(foreff_model)

# significance of the random factor (birdID)
foreff_model_fix <- lm(for_eff ~ treat, data = ff) 
anova(foreff_model, foreff_model_fix)

# plot for individuals (means per ind)
foreff_long <- ff %>% group_by(ID.., treat) %>% 
  summarise (mfor_eff = mean(for_eff)) %>% 
  spread(key = treat, value = mfor_eff, NA) %>% 
  remove_missing() %>% 
  gather(key = treat, value = mfor_eff, -ID..)

indplot <- ggplot(data = foreff_long, aes(x = treat, y = mfor_eff)) +
  geom_point(size = 2) + geom_line(aes(group = as.factor(ID..))) + theme_bw() +
  labs(x = "", y = "Average foraging efficiency");indplot

# ggarrange(forplot, indplot, labels = c("A", "B"),
#           ncol = 2, nrow = 1)
# ggsave(filename = "forplots.tiff", plot = last_plot(),dpi = 300)


# Arousal ---------------------------------------------------

# test
model.arous <- lmer(for_eff ~ treat * mov_feroc_stand + (1 | ID..), 
                    data = ff, REML = FALSE); summary(model.arous)

# significance of the random factor
model.arous_fix <- lm(for_eff ~ treat * mov_feroc_stand, data = ff) 
anova(model.arous, model.arous_fix)

# plot
arousalplot <- ggplot(aes(y  = for_eff , 
                          x = mov_feroc_stand), 
                      data = ff) +
  facet_wrap(~ treat, scales = "free_x") +
  geom_point() +
  labs(x = "Arousal (coeficient variance of deviations from the feeders/number of feeder changes)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_bw();arousalplot


# Explorative behaviour --------------------------------------

# test
model.explr <- lmer(for_eff ~ treat *  stan_nflo + (1 | ID..), data = ff, REML = FALSE)
summary(model.explr)

# significance of the random factor
model.explr_fix <- lm(for_eff ~ treat *  stan_nflo, data = ff)
anova(model.explr, model.explr_fix)

# plot
explot <- ggplot(aes(y  = for_eff , x = stan_nflo), data = ff_lat) +
  facet_wrap(~ treat, scales = "free_x") + 
  geom_point() +
  labs(x = "Explorative behavior (# visited feeders/total foraging duration)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_bw();explot


# Risk avoidance ------------------------------------------------

# test
model.risk.lat <- lmer(for_eff ~ treat *  Latency + (1 | ID..), data = ff, REML = FALSE); summary(model.risk.lat)


# significance of the random factor
model.risk.lat_fix <- lm(for_eff ~ treat *  Latency, data = ff)
anova(model.risk.lat, model.risk.lat_fix)

# plot
riskplot <- ggplot(aes(y  = for_eff , x = Latency), data = ff_lat) +
  facet_wrap(~ treat, scales = "free_x") + 
  geom_point() +
  labs(x = "Risk avoidance (latency)", y =  "Foraging efficiency") +
  geom_smooth(method = "lm") +
  theme_bw(); riskplot


# All behav plots
# ggarrange(explot, riskplot, arousalplot, 
#           labels = c("A", "B", "C"),
#           ncol = 1, nrow = 3)
# ggsave(filename = "behavplots.tiff", plot = last_plot(),dpi = 300)

# Repitabilly -------------------------------------------------------------

# only control group considered
library(rptR)

rpt(mov_feroc_stand ~ (1 | ID..), data = ff[ff$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 10)

# rpt(mov_feroc_stand ~ treat + (1 | ID..), data = ff, grname = "ID..", nboot = 100, npermut = 10)

rpt(stan_nflo ~ (1 | ID..), data = ff[ff$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 10)

# rpt(stan_nflo ~ treat + (1 | ID..), data = ff, grname = "ID..", nboot = 100, npermut = 10)

rpt(Latency ~ (1 | ID..), data = ff[ff$ConfSimpl == "Ctr",], grname = "ID..", nboot = 100, npermut = 10)

# rpt(Latency ~ treat + (1 | ID..), data = ff, grname = "ID..", nboot = 100, npermut = 10)



# Testing significance of the model estimates - randomization ----------------------

# basic model

# Each paramter needs to be processed separately

# Exploratory behav data
df_basic_sel <- ff %>% select(for_eff, treat, ID.., stan_nflo) %>% 
  rename(param = stan_nflo)

# Risk-avoidance behav data
df_basic_sel <- ff %>% select(for_eff, treat, ID.., Latency) %>% 
  rename(param = Latency)

# Arousal behav data
df_basic_sel <- ff %>% select(for_eff, treat, ID.., mov_feroc_stand) %>% rename(param = mov_feroc_stand)

##### START: Common part

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



# PLOTs

# parameter

pval_param <- (sum(rand_parameter<0))/N

plot_param <- ggplot(data = df[df$key == "parameter",]) + geom_density(aes(x = val), fill = "lightgrey") +
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = real_parameter), col = "darkblue", size = 1.2) +
  theme_classic()+
  scale_x_continuous(expand = c(0,0), name = paste0("Estimate, P = ", pval_param, sep = "")) +
  scale_y_continuous(expand = c(0,0), name = "Density")

# treatment 

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

# interaction

pval_inter <- (sum(rand_interaction<0))/N

plot_inter <- ggplot(data = df[df$key == "interaction",], 
                     aes(x = val)) + 
  geom_density(fill = "lightgrey") + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") + 
  geom_vline(aes(xintercept = real_interaction), col = "darkblue", size = 1.2) +
  theme_classic()+
  scale_x_continuous(expand = c(0,0),limits = c(-0.05,0.3), name = paste0("Estimate, P = ", pval_inter, sep = "")) +
  scale_y_continuous(expand = c(0,0), name = "Density")


ggarrange(plot_param, plot_treat, plot_inter, ncol = 3, labels = "AUTO")

# ggsave(filename = "C:/Users/KWJ/Dropbox/FF_fear and foraging/explo_random.jpg", 
#        plot = last_plot(),dpi = 300 )

# ggsave(filename = "C:/Users/KWJ/Dropbox/FF_fear and foraging/risk_random.jpg", 
#        plot = last_plot(),dpi = 300 )

# ggsave(filename = "C:/Users/KWJ/Dropbox/FF_fear and foraging/arousal.jpg", 
#        plot = last_plot(),dpi = 300 )



# behaviours comparison - ctr vs exp -------------------------------------------------

# data selection
df_behav <- ff %>% 
  select(treat, ID.., for_eff, stan_nflo, Latency, mov_feroc_stand)  %>% 
  gather(key = "param", value = "val", -c(treat, ID..))

# test for the significance of the estimates in the model
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

ggplot(data = df_behav, aes(x = treat, y = val)) + 
  geom_violin() +
  geom_point(data = df_behav_ind, aes(x = treat, y = val, col = BirdID)) +
  geom_line(data = df_behav_ind, aes(x = treat, y = val, group = BirdID))  +
  facet_wrap(~param, scales = "free") +
  theme_bw()

