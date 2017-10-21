#' ---
#' title: Results
#' output: word_document
#' ---

#+ echo=F
source("init.r")

library(igraph)
library(spdep)

####################################################################################
## Number and proportion of zhu houses
##
table(hh$zhubo1)  # no. zhu-labelled houses with Mosuo heads

nrow(subset(hh, zhubo1==1)) / nrow(hh)  * 100  # proportion of zhubo houses


####################################################################################
## Join count stats for spatial autocorrelation of zhubo houses
##
# make social networks for each village
# loop over each village separately, saving each adjacency matrix into variables of the form adj.X.village,
# where X is the network type (individual gifts, household gifts, farm work) and village is village number
village_ids = unique(hh$VillageID)
for (village in village_ids) {
  # get unique households as vertices
  # households = unique(subset(hh, Ego.VillageID==village, select=c(Ego.HH, Ego.VillageID, Ego.zhubo1)))
  households = hh %>% 
    filter(VillageID==village) %>% 
    select(Ego.HH=HH, Ego.VillageID=VillageID, Ego.zhubo1=zhubo1)

  # distance
  dat = subset(hh.dyads, Ego.VillageID==village & !is.na(Distance), select=c(Ego.HH, Alter.HH, Distance))
  g.dist = graph.data.frame(dat, vertices=households, directed=T)        # turn relevant subset into a graph object
  assign( paste("adj", "dist", village, sep="."),
          get.adjacency( g.dist, sparse=F, attr="Distance" ) )           # create adjaceny matrix
  
  # also create list of witch houses for current village
  assign( paste("zhubo", village, sep="."), households)
}
# clean up
rm(dat, households, village, g.dist, village_ids)

# set up factors
zhubo.1$zhubo = as.factor(zhubo.1$Ego.zhubo1)
zhubo.2$zhubo = as.factor(zhubo.2$Ego.zhubo1)
zhubo.5$zhubo = as.factor(zhubo.5$Ego.zhubo1)
zhubo.6$zhubo = as.factor(zhubo.6$Ego.zhubo1)
zhubo.8$zhubo = as.factor(zhubo.8$Ego.zhubo1)
levels(zhubo.1$zhubo) = c("Non-zhubo", "Zhubo")
levels(zhubo.2$zhubo) = c("Non-zhubo", "Zhubo")
levels(zhubo.5$zhubo) = c("Non-zhubo", "Zhubo")
levels(zhubo.6$zhubo) = c("Non-zhubo", "Zhubo")
levels(zhubo.8$zhubo) = c("Non-zhubo", "Zhubo")

# join-counts per village
lw1 = mat2listw(adj.dist.1)
lw2 = mat2listw(adj.dist.2)
lw5 = mat2listw(adj.dist.5)
lw6 = mat2listw(adj.dist.6)
lw8 = mat2listw(adj.dist.8)
jc.1 = joincount.multi(zhubo.1$zhubo, listw=lw1, zero.policy=T)
jc.2 = joincount.multi(zhubo.2$zhubo, listw=lw2, zero.policy=T)
jc.5 = joincount.multi(zhubo.5$zhubo, listw=lw5, zero.policy=T)
jc.6 = joincount.multi(zhubo.6$zhubo, listw=lw6, zero.policy=T)
jc.8 = joincount.multi(zhubo.8$zhubo, listw=lw8, zero.policy=T)

# print results and calculate p-values
jc.1
2*pnorm(-abs(jc.1[,4]))
jc.2
2*pnorm(-abs(jc.2[,4]))
jc.5
2*pnorm(-abs(jc.5[,4]))
jc.6
2*pnorm(-abs(jc.6[,4]))
jc.8
2*pnorm(-abs(jc.8[,4]))
