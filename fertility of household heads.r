##
## Fig 2: Fertility of Mosuo household heads
##
library(AICcmodavg)
library(lme4)
library(MuMIn)
library(tidyverse)

# source("data functions/reproductive success - load data.r")
source("init.r")


####################################################################################
## Data prep
##
mosuo.people.sub = mosuo.people %>% 
  # keep only adult Mosuo household heads
  filter(Age>=15 & EthnicGroup=="Mosuo" & ID %in% mosuo.heads) %>% 

  # get predictors into correct formats
  mutate(income_bin = factor(income01_12),
         WealthRank = factor(WealthRank),
         zhubo1 = factor(zhubo1),
         #TotalHelp_bin = factor(ifelse(TotalHelp==0, 0, 1)),
         VillageID = factor(VillageID)) %>% 
    
  # keep only relevant variables
  dplyr::select(ChildNum, Age, year.of.education, income_bin, livestockno_12, WealthRank, zhubo1, VillageID, Sex)

##
## females
##
mosuo.females = mosuo.people.sub %>% 
  filter(Sex=="f") %>%
  dplyr::select(-Sex) %>% 
  na.omit()

# head(mosuo.females)
nrow(mosuo.females)

##
## males
##
mosuo.males = mosuo.people.sub %>% 
  filter(Sex=="m") %>%
  dplyr::select(-Sex) %>% 
  na.omit()

nrow(mosuo.males)

##
## pre-processing
##
# mean-centre ages
mosuo.females$Age.c = as.numeric(scale(mosuo.females$Age))
mosuo.males$Age.c = as.numeric(scale(mosuo.males$Age))

# age^2
mosuo.females$Age2 = mosuo.females$Age.c^2
mosuo.males$Age2 = mosuo.males$Age.c^2

rm(mosuo.people.sub)


#########################################################################################################
## Plot fertility distributions
##
# this function is the same as rethinking::simplehist() but without requiring the user to install the rethinking package...
plot_kids = function(d, title="") plot(table(d$ChildNum), xlab="Number of children", ylab="Count", main=title)

png(file.path(plots.dir, "fertility distributions.png"), height=10, width=20, units="cm", res=300)
  par(mfrow=c(1,2))
  plot_kids(mosuo.females, "a")
  plot_kids(mosuo.males, "b")
dev.off()

pdf(file.path(plots.dir, "fertility distributions.pdf"), height=5, width=10)
  par(mfrow=c(1,2))
  plot_kids(mosuo.females, "a")
  plot_kids(mosuo.males, "b")
dev.off()

par(mfrow=c(1,1))  # reset 


####################################################################################
## Should we use a linear or quadratic term for age?
##
m.age.1 = glm(ChildNum ~ Age.c, data=mosuo.females, family="poisson")
m.age.2 = glm(ChildNum ~ Age.c + Age2, data=mosuo.females, family="poisson")

aictab(list(m.age.1, m.age.2), modnames=c("age", "age^2"))  # quadratic model fits best (though only has 66% of the weight)

rm(m.age.1, m.age.2)


####################################################################################
## Prepare for model fitting
##
# lists to store all the female (.f) and male (.m) models
m.rs.f = list()
m.rs.m = list()

# names of models
rs_names = c("Null model", "Control", "Control + Zhu")
# rs_names = c("Null model", "Control", "Control + Zhu", "Control + Help", "Control + Help + Zhu")


####################################################################################
## Poisson models for reproductive success - female household heads
##
# null
m.rs.f[[1]] = glmer(ChildNum ~ 1 + (1 | VillageID), family="poisson", data=mosuo.females)

# control (age, age^2, no. sibs, agexsibs, education, wealth)
m.rs.f[[2]] = glmer(ChildNum ~ Age.c + Age2 + year.of.education + income_bin + livestockno_12 + WealthRank + (1 | VillageID), family="poisson", data=mosuo.females)

# control + Zhubo
m.rs.f[[3]] = glmer(ChildNum ~ Age.c + Age2 + year.of.education + income_bin + livestockno_12 + WealthRank + zhubo1 + (1 | VillageID), family="poisson", data=mosuo.females)

# with farm help
# m.rs.f[[4]] = glmer(ChildNum ~ Age.c + Age2 + year.of.education + income_bin + livestockno_12 + WealthRank + TotalHelp_bin + (1 | VillageID), family="poisson", data=mosuo.females)
# m.rs.f[[5]] = glmer(ChildNum ~ Age.c + Age2 + year.of.education + income_bin + livestockno_12 + WealthRank + TotalHelp_bin + zhubo1 + (1 | VillageID), family="poisson", data=mosuo.females)

##
## model selection/averaging
##
best.models = model.sel(m.rs.f)
best.models$Model = rs_names[ as.integer(rownames(best.models))]  # add model names in correct order

best.models %>% 
  dplyr::select(Model, K=df, delta, weight, LL=logLik) %>% 
  write_csv(file.path(results.dir, "Reproductive success - confidence set - females - household heads.csv"))

# averaging
avg.model = model.avg(best.models, subset = delta < 2, revised.var=T)

avg.model.sum.f = summary(avg.model)$coefficients[2,] %>% 
  as_data_frame() %>% 
  rownames_to_column(var="Parameter") %>% 
  bind_cols(confint(avg.model) %>% as_data_frame())

avg.model.sum.f$Parameter <- c("(Intercept)"
                              ,"Age", "Age^2"
                              ,"Education (years)", "Tourist income", "No. livestock"
                              ,"Wealth rank: 2", "Wealth rank: 3", "Wealth rank: 4", "Wealth rank: 5", "Wealth rank: 6"
                              ,"Zhubo?"
)


####################################################################################
## Poisson models for reproductive success - male household heads
##
# null
m.rs.m[[1]] = glmer(ChildNum ~ 1 + (1 | VillageID), family="poisson", data=mosuo.males,
                             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

# control (age, age^2, no. sibs, agexsibs, education, wealth)
m.rs.m[[2]] = glmer(ChildNum ~ Age.c + Age2 + year.of.education + income_bin + livestockno_12 + WealthRank + (1 | VillageID), family="poisson", data=mosuo.males,
                             control=glmerControl(optCtrl=list(maxfun=2e5)))

# control + Zhubo
m.rs.m[[3]] = glmer(ChildNum ~ Age.c + Age2 + year.of.education + income_bin + livestockno_12 + WealthRank + zhubo1 + (1 | VillageID), family="poisson", data=mosuo.males,
                             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

##
## model selection/averaging
##
best.models = model.sel(m.rs.m)
best.models$Model = rs_names[ as.integer(rownames(best.models))]  # add model names in correct order

best.models %>% 
  dplyr::select(Model, K=df, delta, weight, LL=logLik) %>% 
  write_csv(file.path(results.dir, "Reproductive success - confidence set - males - household heads.csv"))

# averaging
avg.model = model.avg(best.models, subset = delta < 2, revised.var=T)

avg.model.sum.m = summary(avg.model)$coefficients[2,] %>% 
  as_data_frame() %>% 
  rownames_to_column(var="Parameter") %>% 
  bind_cols(confint(avg.model) %>% as_data_frame())

avg.model.sum.m$Parameter <- c("(Intercept)"
                               ,"Age", "Age^2"
                               ,"Education (years)", "Tourist income", "No. livestock"
                               ,"Wealth rank: 2", "Wealth rank: 3", "Wealth rank: 4", "Wealth rank: 5", "Wealth rank: 6"
                               ,"Zhubo?"
)


####################################################################################
## Plot model averaged estimates with CIs
## code modified from: https://github.com/seananderson/lobsters-predators/blob/7f90997b309e52a5f4f7470e20091bdf3b26cba5/r/mod.averaging.lmer.R
##
# pick one or the other of these
#png(filename=file.path(plots.dir, "fig 3.png"), width=200, height=100, units="mm", res=300)
pdf(file=file.path(plots.dir, "fig 3.pdf"), width=7.8, height=3.9)

##
## females
##
par(fig=c(0,0.6,0,1), new=TRUE)
num.params = nrow(avg.model.sum.f)
pch <- c(rep(19, 11), rep(21, 11), cex = 0.8)
par(mar = c(4, 9, 1, 1))
plot(1, 1, type = "n", xlim = c(min(avg.model.sum.f$`2.5 %`, na.rm=T), max(avg.model.sum.f$`97.5 %`, na.rm=T)), ylim = c(1, num.params), axes = FALSE, xlab = "", ylab = "")
with(avg.model.sum.f, points(value, 1:num.params, pch = pch))
with(avg.model.sum.f, segments(`2.5 %`, 1:num.params, `97.5 %`, 1:num.params))
abline(v = 0, lty = 2)
axis(1)
axis(2, at = 1:num.params, labels = avg.model.sum.f$Parameter, las = 1)
mtext("a")

##
## males
##
par(fig=c(0.6,1,0,1), new=TRUE)
num.params = nrow(avg.model.sum.m)
pch <- c(rep(19, 11), rep(21, 11), cex = 0.8)
par(mar = c(4, 1, 1, 1))
plot(1, 1, type = "n", xlim = c(min(avg.model.sum.m$`2.5 %`, na.rm=T), max(avg.model.sum.m$`97.5 %`, na.rm=T)), ylim = c(1, num.params), axes = FALSE, xlab = "", ylab = "")
with(avg.model.sum.m, points(value, 1:num.params, pch = pch))
with(avg.model.sum.m, segments(`2.5 %`, 1:num.params, `97.5 %`, 1:num.params))
abline(v = 0, lty = 2)
axis(1)
mtext("b")

dev.off()


####################################################################################
## Table of varying intercepts' variances for each model
##
bind_cols(
  data_frame(Model = rs_names),
  data_frame(Females = round(unlist(lapply(m.rs.f, VarCorr)), 3)),
  data_frame(Males   = round(unlist(lapply(m.rs.m, VarCorr)), 3))
) %>% 
  write_csv(file.path(results.dir, "Reproductive success - variances of varying intercepts.csv"))

# save models
# save.image("fertility models.rdata")
