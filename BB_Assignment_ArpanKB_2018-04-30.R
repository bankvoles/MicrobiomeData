## setwd("~/Documents/REPOSITORY/Metadata_Pipeline/Metadata/R_Playground/Assignment/")
## getwd()

# pkgs <- c("RCurl", "bbmle","blme","brms","gamm4","glmmLasso","glmmML","lme4","glmmTMB","MCMCglmm","robustlmm", 
#           "spaMM","tidyverse","reshape2","plyr", "benchmark","emdbook","devtools", "AICcmodavg","cAIC4", "coda", 
#           "effects", "emmeans","multcomp","arm", "car","Hmisc","HLMdiag","stargazer","texreg","rockchalk","RLRsim",
#           "pbkrtest","lmerTest","broom","dotwhisker","ggplot2","ggstance","GGally","cowplot", "gridExtra","plotrix","dotwhisker","plotMCMC")

# install_pkg <- pkgs[!pkgs %in% installed.packages()]
# 
# for(lib in install_pkg) install.packages(lib, dependencies = T, quiet = T)
# 
# lapply(pkgs, library, character.only = T)

## Loading data
url <- getURL("https://raw.githubusercontent.com/bankvoles/MicrobiomeData/master/BVoles_data_ArpanKB_2018-04-29.txt")
dat <- read.table(textConnection(url), header = T, row.names = 1)

## Exploring the data
head(dat)
nrow(dat)
str(dat)
var <- colnames(dat)
obs <- rownames(dat)
summary(dat)
str(dat)

## Data visualisation

p_lm_l <- ggplot(dat, aes(Sample_type, Lactobacillus, colour = Selection_line)) + geom_point() + 
  geom_smooth(method = "lm", aes(group = Sex)) + theme_classic()
              
p_bx_l <- ggplot(dat, aes(Sample_type, Lactobacillus, fill= Selection_line)) + geom_boxplot() + theme_classic()

p_lm_r <- ggplot(dat, aes(Sample_type, Ruminococcus, colour = Selection_line)) + geom_point() + geom_smooth(method = "lm", aes(group = Sex))

p_bx_r <- ggplot(dat, aes(Sample_type, Ruminococcus, fill = Selection_line)) + geom_boxplot() + theme_classic()

# p_glm <- ggplot(dat, aes()) + geom_point() + geom_smooth()
# 
# p_lm <- ggplot(dat, aes()) + geom_point() + geom_smooth()

p_bx_r
p_lm_l
p_bx_l
p_lm_r

## Statistical modelling
## For Lactobacillus
mod_kt <- kruskal.test(g = dat$Sample_type, x = dat$Lactobacillus)
mod_lm <- lm(Lactobacillus ~ Sample_type, dat)
summary(mod_lm)

## For Ruminococcus
mod1_kt <- kruskal.test(g = dat$Sample_type, x = dat$Ruminococcus)
mod1_lm <- lm(Ruminococcus ~ Sample_type, data = dat)

summary(mod_lm)
summary(mod1_lm)

## Model fit visualisation
par(mfrow = c(4,2))
plot(mod_lm, main = "Lactobacillus")
plot(mod1_lm, main = "Ruminococcus")
par(mfrow = c(1,1))


sessionInfo()
