##
## Zero-inflated Poissons predicting number of gifts and amount of farm help received
##
## Supplementary Table 1
##
source("init.r")

library(pscl)
library(igraph)
library(MuMIn)


##########################################################################################
## Calculate in-degrees on various networks
##
prepend_ego = function(x) paste0("Ego.", x)  # helper function to add "Ego." to start of column names

nodes.hh = hh %>% 
  rename_all(prepend_ego)

# remove village 6
nodes.hh = subset(nodes.hh, Ego.VillageID != 6)

##
## Individual gifts 
##
dat = subset(hh.dyads, TotalGifts.ind > 0 & Ego.VillageID != 6, select=c(Ego.HH, Alter.HH, TotalGifts.ind))
g.gifts_ind = graph.data.frame(dat, vertices=nodes.hh, directed=T)

# calculate in degree and put into houses dataframe
deg.in  = degree(g.gifts_ind, mode="in")
nodes.hh$gift.deg.in  = deg.in[ as.character(nodes.hh$Ego.HH) ]

##
## Farm help
##
dat = subset(hh.dyads, HelpObserved > 0 & Ego.VillageID != 6, select=c(Ego.HH, Alter.HH))
g.farm = graph.data.frame(dat, vertices=nodes.hh, directed=T)

# calculate in degree and put into houses dataframe
deg.in  = degree(g.farm, mode="in")
nodes.hh$farm.deg.in  = deg.in[ as.character(nodes.hh$Ego.HH) ]


##########################################################################################
## What predicts in-degree for farm work and gifts
##
d.hh = nodes.hh %>% 
  dplyr::select(Ego.HH, farm.deg.in, gift.deg.in, Ego.Size, Ego.WealthRank, Ego.VillageID, Ego.Sex, Ego.zhubo1) %>% 
  distinct() %>% 
  na.omit()

# zero-inflated poissons
# null
m.null = zeroinfl(farm.deg.in ~ 1, data=d.hh)
m.gift.null = zeroinfl(gift.deg.in ~ 1, data=d.hh)

# control
m.control = zeroinfl(farm.deg.in ~ Ego.Size + Ego.WealthRank + Ego.Sex + factor(Ego.VillageID), data=d.hh)
m.gift.control = zeroinfl(gift.deg.in ~ Ego.Size + Ego.WealthRank + Ego.Sex + factor(Ego.VillageID), data=d.hh)

# full
m.zhubo = zeroinfl(farm.deg.in ~ factor(Ego.zhubo1) + Ego.Size + Ego.WealthRank + Ego.Sex + factor(Ego.VillageID), data=d.hh)
m.gift.zhubo = zeroinfl(gift.deg.in ~ factor(Ego.zhubo1) + Ego.Size + Ego.WealthRank + Ego.Sex + factor(Ego.VillageID), data=d.hh)


#################################################################################
## model selection
##
##
## farm work
##
best.models = model.sel(m.null, m.control, m.zhubo)
top.models  = mget(row.names(subset(best.models, delta<2)))
best.avg    = model.avg(top.models, revised.var=T, beta=F)

# summary(best.avg)

# incidence risk ratios with CIs
zip = cbind( data.frame((coef(best.avg, full=T))), (confint(best.avg, full=T, digits=3)) )
names(zip) = c("Farm work B", "Farm work lwr", "Farm work upr")

round(zip, 3)

write.csv(best.models, file="in-degree - candidate models - farm.csv", row.names=T)

##
## gifts
##
best.models = model.sel(m.gift.null, m.gift.control, m.gift.zhubo)
top.models  = mget(row.names(subset(best.models, delta<2)))
best.avg    = model.avg(top.models, revised.var=T, beta=F)

# summary(best.avg)

# incidence risk ratios with CIs
zip = cbind(zip, data.frame((coef(best.avg, full=T))), (confint(best.avg, full=T, digits=3)) )
names(zip)[4:6] = c("Gifts B", "Gifts lwr", "Gifts upr")

write.csv(best.models, file="in-degree - candidate models - gift.csv", row.names=T)

write.csv(zip, file="zip - parameter estimates.csv", row.names=T)
