##
## GEEs predicting farm work for all household dyads within villages
## - for households with Mosuo heads of house only
##
## produces Supplementary Table 2
##
rm(list=ls())

source("init.r")

library(data.table)
library(geepack)
library(MuMIn)
library(arm)  # for standardize()
library(ggplot2)
library(broom)

# load custom function to standardise GEE models
source("https://gist.githubusercontent.com/matthewgthomas/6e4ae7f55a339bd8c036/raw/cff5ba979c673ef6da3533afbb7bb09c1f42ed71/standardize-gee.r")


##########################################################
## Load data
##
# relative rank:
# +ve if alter is lower wealth rank than ego (e.g. alter=5 but ego=1 => relative rank = 4)
# -ve if alter is higher rank than ego (e.g. ego=2 but alter=1 => relative rank = -1)
hh.all = hh.dyads %>% 
  # merge ego covariates
  left_join(hh %>% dplyr::select(Ego.HH = HH, Ego.WealthRank = WealthRank, Ego.Size = Size, Ego.zhu = zhubo1), by="Ego.HH") %>% 
  
  # merge alter covariates
  left_join(hh %>% dplyr::select(Alter.HH = HH, Alter.WealthRank = WealthRank, Alter.Size = Size, Alter.zhu = zhubo1), by="Alter.HH") %>% 
  
  # calculate some dyadic covariates
  mutate(GiftGiven.ind = ifelse(TotalGifts.ind > 0, 1, 0),
         ChildrenInAlter = ifelse(TotalChildrenInAlter > 0, 1, 0),
         RelativeWealthRank = Alter.WealthRank - Ego.WealthRank) %>% 
  
  # keep only columns needed for GEEs
  dplyr::select(Ego.HH, Alter.HH, HelpObserved, GiftGiven.ind, Distance, r, 
                AnySpousesInAlter, ChildrenInAlter, RelativeWealthRank,
                Ego.Size, Alter.Size, Ego.zhu, Alter.zhu) %>%
  
  na.omit() %>% 
  arrange(Ego.HH, Alter.HH)  # make sure data are in correct order for GEE

hh.zhu = hh.all %>% filter(Ego.zhu==1)
hh.non = hh.all %>% filter(Ego.zhu==0)

# summaries
nrow(hh.zhu)  # no. dyads
length(unique(hh.zhu$Ego.HH))  # no. households

nrow(hh.non)  # no. dyads
length(unique(hh.non$Ego.HH))  # no. households


##########################################################
## GEE models predicting whether one household helped another on their farms
## - egos are zhu
##
# intercept-only model
gee.null.z = geeglm(HelpObserved ~ 1, id=Ego.HH, family="binomial", data=hh.zhu, corstr="exchangeable", scale.fix=T)

# control model (distance + size + wealth)
gee.control.z = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size,
                       id=Ego.HH, family="binomial", data=hh.zhu, corstr="exchangeable", scale.fix=T)

# full model
gee.full.z = geeglm(HelpObserved ~ GiftGiven.ind + r + AnySpousesInAlter + ChildrenInAlter +
                      Alter.zhu + r:Alter.zhu +
                      Distance + RelativeWealthRank + Ego.Size + Alter.Size,
                    id=Ego.HH, family="binomial", data=hh.zhu, corstr="exchangeable", scale.fix=T)


##########################################################
## GEE models predicting whether one household helped another on their farms
## - egos are non-zhu
##
# intercept-only model
gee.null.n = geeglm(HelpObserved ~ 1, id=Ego.HH, family="binomial", data=hh.non, corstr="exchangeable", scale.fix=T)

# control model (distance + size + wealth)
gee.control.n = geeglm(HelpObserved ~ Distance + RelativeWealthRank + Ego.Size + Alter.Size,
                       id=Ego.HH, family="binomial", data=hh.non, corstr="exchangeable", scale.fix=T)

# full model
gee.full.n = geeglm(HelpObserved ~ GiftGiven.ind + r + AnySpousesInAlter + ChildrenInAlter +
                      Alter.zhu + r:Alter.zhu +
                      Distance + RelativeWealthRank + Ego.Size + Alter.Size,
                    id=Ego.HH, family="binomial", data=hh.non, corstr="exchangeable", scale.fix=T)


##########################################################
## Model selection - egos are zhu
##
(best.models.zhu = model.sel(gee.null.z, gee.control.z, gee.full.z, rank=QIC))

# gee.full is best-fitting
gee.best = get(row.names(best.models.zhu)[1])
# standardise coefficients
gee.best.z = standardize.gee(gee.best, standardize.y=F, binary.inputs="full")

# save model coefficients
tidy(gee.best.z) %>% 
  bind_cols(confint_tidy(gee.best.z)) %>%  # calculate CIs
  mutate(OR = exp(estimate), OR.low = exp(conf.low), OR.high = exp(conf.high)) %>% 
  write_csv(file.path(results.dir, "GEE - ego zhu.csv"))

# save model selection table
best.models.zhu %>% 
  rownames_to_column(var = "Model") %>% 
  dplyr::select(Model, delta, weight) %>% 
  write_csv(file.path(results.dir, "GEE - ego zhu - model selection.csv"))


##########################################################
## Model selection - egos are non-zhu
##
(best.models.non = model.sel(gee.null.n, gee.control.n, gee.full.n, rank=QIC))

# gee.full is best-fitting
gee.best = get(row.names(best.models.non)[1])
# standardise coefficients
gee.best.z = standardize.gee(gee.best, standardize.y=F, binary.inputs="full")

# save model coefficients
tidy(gee.best.z) %>% 
  bind_cols(confint_tidy(gee.best.z)) %>%  # calculate CIs
  mutate(OR = exp(estimate), OR.low = exp(conf.low), OR.high = exp(conf.high)) %>% 
  write_csv(file.path(results.dir, "GEE - ego non-zhu.csv"))

# save model selection table
best.models.non %>% 
  rownames_to_column(var = "Model") %>% 
  dplyr::select(Model, delta, weight) %>% 
  write_csv(file.path(results.dir, "GEE - ego non-zhu - model selection.csv"))

# Save models
save(hh.zhu, hh.non, 
     gee.null.z, gee.control.z, gee.full.z, best.models.zhu,
     gee.null.n, gee.control.n, gee.full.n, best.models.non,
     file=file.path(models.dir, "gee.RData"))
