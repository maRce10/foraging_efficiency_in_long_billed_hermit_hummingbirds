
# Load packages -----------------------------------------------------------


rm(list = ls())

# unload all non-based packages
out <- sapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), function(x) try(detach(x, unload = FALSE, character.only = TRUE), silent = T))

## add 'developer/' to packages to be installed from github
x <- c(
  "viridis",
  "readxl",
  "ggplot2",
  "tidyverse",
  "lmerTest",
  "lme4",
  "smatr",
  "ggpubr",
  "MCMCglmm",
  "corrplot",
  "rptR",
  "pbapply",
  "MuMIn",
  "parallel",
  "kableExtra",
  "ggridges",
  "cowplot"
)


out <- lapply(x, function(y) {
  # get pakage name
  pkg <- strsplit(y, "/")[[1]]
  pkg <- pkg[length(pkg)]
  
  # check if installed, if not then install
  if (!pkg %in% installed.packages()[, "Package"])  {
    if (grepl("/", y))
      devtools::install_github(y, force = TRUE)
    else
      install.packages(y)
  }
  
  # load package
  try(require(pkg, character.only = T), silent = T)
})



# Load data and set parameters --------------------------------------------

cols <- viridis(10, alpha = 0.6)

# function to get posterior estimates within the HPD interval
HPD_mcmc <- function(y, long = TRUE) {
  
  out <- lapply(1:ncol(y), function(x){
    # calculate hpd
    hpd <- HPDinterval(y[, x])
    
    # get sol as vector
    vctr <- y[, x]
    
    # clip vector to hpd range
    hpdmcmc <- vctr[vctr > hpd[1] & vctr < hpd[2]]
    
    return(hpdmcmc)  
  })
  
  # get them together
  hpd.mcmcs <- do.call(cbind, out)  
  
  # change colnames
  colnames(hpd.mcmcs) <- colnames(y)
  
  # put it in long format
  if (long){
    est.df <- lapply(1:ncol(hpd.mcmcs), function(x){
      
      data.frame(predictor = colnames(hpd.mcmcs)[x], effect_size = hpd.mcmcs[, x], stringsAsFactors = FALSE)
    })
    
    # get them together
    hpd.mcmcs <- do.call(rbind, est.df)  
  }
  
  return(hpd.mcmcs)
}

# color for corrplot
col.crrplt <- colorRampPalette(c(cols[1:2], rep("white", 1), cols[6:7]))(100)


# ggplot2 theme
theme_set(theme_classic(base_size = 30, base_family = "Arial"))

foraging_data <- ff <- read_excel("C:/Users/KWJ/Dropbox/FF_fear and foraging/ff.xlsx")

names(foraging_data)[names(foraging_data) == "ID.."] <- "indiv"


# Exploring data ----------------------------------------------------------

vars <- c("stan_nflo", "for_eff", "Latency", "mov_feroc_stand")

# look at data distribution
long_foragin_data <- do.call(rbind, lapply(vars, function(x) data.frame(var = x, value = foraging_data[, names(foraging_data) == x, drop = TRUE])))

ggplot(long_foragin_data, aes(var, value)) + 
  geom_violin(fill = cols[9]) +
  coord_flip() + 
  ggtitle("Raw parameters") +
  labs(x = "Parameter", y = "Raw value")

# log transformed
ggplot(long_foragin_data, aes(var, log(value + 1))) + 
  geom_violin(fill = cols[9]) +
  coord_flip() + 
  ggtitle("Log-transformed parameters") +
  labs(x = "Parameter", y = "Log value")


# log transform variables
foraging_data$arousal <- log(foraging_data$mov_feroc_stand + 1)
foraging_data$exploration <- log(foraging_data$stan_nflo + 1)
foraging_data$risk_avoidance <- log(foraging_data$Latency + 1)
foraging_data$foraging_efficiency <- log(foraging_data$for_eff + 1)

foraging_data$context <- ifelse(foraging_data$treat == "Ctr", "Low risk", "High risk")

foraging_data$context <- factor(foraging_data$context, levels = c("Low risk", "High risk"))

# new target variables
vars <- c("exploration", "risk_avoidance", "arousal")

# correlation matrix
cm <- cor(foraging_data[, vars], use = "pairwise.complete.obs")

# visualize collinearity
corrplot.mixed(cm, upper = "ellipse", lower = "number", tl.pos = "lt", upper.col = col.crrplt, lower.col = col.crrplt, tl.col = "black", tl.cex = 2)


# Repeatability -----------------------------------------------------------

pboptions(type = "none")
# rep movement

rpt_arousal <- rpt(arousal ~ (1 | indiv), data = foraging_data[foraging_data$context == "Low risk",], grname = "indiv", nboot = 100, npermut = 100, parallel = TRUE)

rpt_exploration <- rpt(exploration ~ (1 | indiv), data = foraging_data[foraging_data$context == "Low risk",], grname = "indiv", nboot = 100, npermut = 100, parallel = TRUE)

rpt_risk <- rpt(risk_avoidance ~ (1 | indiv), data = foraging_data[foraging_data$context == "Low risk",], grname = "indiv", nboot = 100, npermut = 100, parallel = TRUE)

rpt_foraging_efficiency <- rpt(foraging_efficiency ~ (1 | indiv), data = foraging_data[foraging_data$context == "Low risk",], grname = "indiv", nboot = 100, npermut = 100, parallel = TRUE)

saveRDS(list(arousal = rpt_arousal, exploration = rpt_exploration, risk_avoidance = rpt_risk, foraging_efficiency = rpt_foraging_efficiency), "Repeatability results.RDS")


rept <- readRDS("Repeatability results.RDS")

reps <- lapply(1:length(rept), function(x){
  
  X <- rept[[x]]
  data.frame(param = names(rept)[x], R = X$R[1,], low.CI = X$CI_emp[1, 1], hi.CI = X$CI_emp[1, 2])
})

reps.df <- do.call(rbind,reps)

ggplot(reps.df, aes(x = param, y = R)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(col = cols[7], size = 5) +
  geom_errorbar(aes(ymin = low.CI, ymax = hi.CI), width=.0, col = cols[7], size = 2) +
  coord_flip()  + labs(y = "Repeatability", x ="Parameters")


# MCMC_glmm ---------------------------------------------------------------


