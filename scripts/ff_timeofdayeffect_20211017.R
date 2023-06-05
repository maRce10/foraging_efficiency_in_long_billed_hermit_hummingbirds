rm(list = ls())

library(openxlsx)
library(readxl)
library(tidyverse)
library(lmerTest)
library(lme4)
library(ggpubr)

# Day/hour effect -----

# data
ff <- read_excel("C:/Users/KWJ/Dropbox/lbh_foraging_efficiency/data/raw/ff.xlsx")

# only controls
ff_ctrl <- ff %>% 
  filter(treat == "Ctr") %>% 
  mutate(label = str_sub(Video.cut, 47),
         day = str_sub(label, 8,9),
         hour = str_sub(label, 11,12),
         day_hour = paste0(day, "_", hour, sep = ""))


# Examine relevant paramter
model_lat <- lmer(Latency ~ day_hour + (1|ID..), data = ff_ctrl, REML = TRUE)
ggplot(data = ff_ctrl, aes (x = day, y = Latency)) + geom_boxplot()
ggplot(data = ff_ctrl, aes (x = day_hour, y = Latency)) + geom_boxplot()

ggplot(data = ff_ctrl, aes (x = as.numeric(hour), y = for_eff)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm()

names(ff_ctrl)
model_fordur <- lmer(for_eff ~ as.numeric(hour) + (1|ID..), data = ff_ctrl, REML = TRUE)
summary(model_fordur)


ggplot(data = ff_ctrl, aes (x = day, y = tot_durfor)) + geom_boxplot()
ggplot(data = ff_ctrl, aes (x = day_hour, y = tot_durfor)) + geom_boxplot()
ggplot(data = ff_ctrl, aes (x = hour, y = tot_durfor)) + geom_boxplot()

# Basic models - update
foreff_model <- lmer(for_eff ~  treat +  treat_num + (1 | ID..), data = ff, REML = FALSE); summary(foreff_model)

model.arous <- lmer(for_eff ~ treat * mov_feroc_stand + (1 | ID..), 
                    data = ff, REML = FALSE); summary(model.arous)
model.arous <- lmer(for_eff ~ treat * mov_feroc_stand + treat_num  + (1 | ID..), 
                    data = ff, REML = FALSE); summary(model.arous)

model.explr <- lmer(for_eff ~ treat *  stan_nflo + (1 | ID..), data = ff, REML = FALSE)
summary(model.explr)
model.explr <- lmer(for_eff ~ treat *  stan_nflo + treat_num + (1 | ID..), data = ff, REML = FALSE)
summary(model.explr)

model.risk.lat <- lmer(for_eff ~ treat *  Latency + (1 | ID..), data = ff, REML = FALSE); summary(model.risk.lat)
model.risk.lat <- lmer(for_eff ~ treat *  Latency + treat_num + (1 | ID..), data = ff, REML = FALSE); summary(model.risk.lat)



# Ind sequence -----

# Data
ff <- read_excel("C:/Users/KWJ/Dropbox/lbh_foraging_efficiency/data/raw/ff.xlsx")

# only controls
ff_ctrl <- ff %>% 
  filter(treat == "Ctr") %>% 
  mutate(label = str_sub(Video.cut, 47),
         day = as.numeric(str_sub(label, 8,9)),
         hr = str_sub(label, 11,12),
         mn = str_sub(label, 13,15))

# fix time
library(lubridate)
ff_ctrl <- ff_ctrl %>% 
  mutate(hr = as.numeric(str_remove(hr, "[.]")),
         mn = as.numeric(str_remove_all(mn, "[M.]")),
         time = ymd_hms(paste0("2015-06-", day, " ", hr, ":", mn, ":00")))

# select and create valid cols
ff_durexpl <- ff_ctrl %>% 
  select(time, Video.cut,treat,treat_num, Configuration, ID..,
         tot_durfor, for_eff, stan_nflo, Latency) %>%
  distinct() %>% 
  group_by(Configuration, ID..) %>% 
  mutate(Seq = row_number(),
         IDtreat = paste0(ID.., "_", treat_num, sep ="")) %>% 
  ungroup()

x11()
ggplot(data = ff_durexpl, aes(x = Seq, y = for_eff, col = as.character(treat_num))) +
# ggplot(data = ff_durexpl, aes(x = Seq, y = tot_durfor, col = as.character(treat_num))) +
# ggplot(data = ff_durexpl, aes(x = Seq, y = stan_nflo, col = as.character(treat_num))) +
facet_wrap( ~ID..) +
  geom_point() +
  geom_line()

ggplot(data = ff_durexpl, aes(x = Seq, y = for_eff)) +
  facet_wrap( ~IDtreat) +
  geom_point() +
  geom_line()

ff_durexpl %>% 
  filter(ID.. != "292") %>% 
ggplot(x) +
         geom_point(aes(y = for_eff, x = Seq, col = as.factor(ID..))) + 
                      geom_smooth(aes(y = for_eff, x = Seq), method = "lm", col = "darkgrey") +
  labs(y ="Foraging efficiency", 
      x = "Consecutive visits",
      col = "Bird ID") +
  theme_bw()

  # efect of seq - lm

x <- ff_durexpl %>% 
  filter(ID.. %notin% c("292", "364"))  
  model_for_eff <- lmer(for_eff ~ Seq + (1|ID..), data = x, REML = TRUE)
summary(model_for_eff)


View(ff_durexpl)
# selection ind/conf for acf analysis (those with >= 5 per session)
idsel <- ff_durexpl %>% 
  group_by(IDtreat) %>% 
  summarise(n = n()) %>% 
  filter(n >=5)
idsel <- idsel$IDtreat

ff_durexpl_acf_df <- ff_durexpl %>% 
  filter(IDtreat %in% idsel) %>% 
  select(IDtreat, for_eff)

# split data into separate individuals/sessions for acf analysis
split_data <- split(ff_durexpl_acf_df, f = ff_durexpl_acf_df)   
ff_acf_dfs <- ff_durexpl_acf_df %>%
  group_by(IDtreat)
ff_acf_dfs_list <- group_split(ff_acf_dfs)

# acf plots
setwd("C:/Users/KWJ/Dropbox/lbh_foraging_efficiency/output/acf_plots")
for(i in 1:length(ff_acf_dfs_list)){
  ind_treat <- unique(ff_acf_dfs_list[[i]][1])
  a <- acf(ff_acf_dfs_list[[i]][2])
  jpeg(paste0("acf_", ind_treat, ".jpg", sep = ""))
  plot(a, main = ind_treat)
  dev.off()
}



# Frequency of visits

ff_ctrl <- ff %>% 
  filter(treat == "Ctr") %>% 
  mutate(label = str_sub(Video.cut, 47),
         day = as.numeric(str_sub(label, 8,9)),
         hr = str_sub(label, 11,12),
         mn = str_sub(label, 13,15))

freqvisit <- ff_ctrl %>% 
  select(treat_num, ID.., for_eff) %>% 
  distinct() %>% 
  group_by(treat_num, ID..) %>% 
  summarise(n = n()) %>% 
  mutate(dursess = if_else(treat_num %in% c("2", "3", "4"), 0.5,0),
         freq = n/dursess) %>% 
  ungroup()

FreqVisit <-  freqvisit$freq[!is.infinite(freqvisit$freq)]
range(FreqVisit)
mean(FreqVisit)

totnvisit <- ff_ctrl %>% 
  select(ID.., for_eff) %>% 
  distinct() %>% 
  group_by(ID..) %>% 
  summarise(n = n())
  
range(totnvisit$n)
mean(totnvisit$n)



# nvists for exp

ff_exp <- ff %>% 
  filter(treat == "Exp")  %>% 
  select(treat_num, ID.., for_eff) %>% 
  distinct() %>% 
  group_by(ID..) %>% 
  summarise(n = n())

range(ff_exp$n)
mean(ff_exp$n)

length(unique(ff$ID..))
