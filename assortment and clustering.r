##
## Network stats
## -------------
##
## Assortativity on zhubo for all networks
##
source("init.r")

library(igraph)
library(data.table)
library(dplyr)
library(ggplot2)


#################################################################################
## Make social networks for each village
##
# get unique households as vertices
# households = unique(subset(hh.all, select=c(Ego.HH, Ego.VillageID, Ego.zhubo1)))
households = hh %>% 
  dplyr::select(Ego.HH=HH, Ego.VillageID=VillageID, Ego.zhubo1=zhubo1)

nodes.zhubo = households$Ego.zhubo1 + 1

# individual gifts
dat = subset(hh.dyads, TotalGifts.ind > 0, select=c(Ego.HH, Alter.HH))
g.gift = graph.data.frame(dat, vertices=households, directed=T)        # turn relevant subset into a graph object
adj.gift = get.adjacency( g.gift, sparse=F )                           # create adjaceny matrix

# farm working
dat = subset(hh.dyads, HelpObserved > 0, select=c(Ego.HH, Alter.HH, HelpInstances))
dat$weight = dat$HelpInstances
g.farm = graph.data.frame(dat, vertices=households, directed=T)        # turn relevant subset into a graph object
adj.farm = get.adjacency( g.farm, sparse=F )                           # create adjaceny matrix

# partners
dat = subset(hh.dyads, AnySpousesInAlter > 0, select=c(Ego.HH, Alter.HH))
g.spouse = graph.data.frame(dat, vertices=households, directed=F)      # turn relevant subset into a graph object
g.spouse = simplify(g.spouse)                                          # remove multiple edges (>1 edge between a given pair of nodes)
adj.spouse = get.adjacency( g.spouse, sparse=F )                       # create adjaceny matrix

# kids
dat = subset(hh.dyads, TotalChildrenInAlter > 0, select=c(Ego.HH, Alter.HH))
g.kids = graph.data.frame(dat, vertices=households, directed=F)        # turn relevant subset into a graph object
g.kids = simplify(g.kids)                                              # remove multiple edges (>1 edge between a given pair of nodes)
adj.kids = get.adjacency( g.kids, sparse=F )                           # create adjaceny matrix

# distance between households
dat = subset(hh.dyads, Distance > 0 & !is.na(Distance), select=c(Ego.HH, Alter.HH, Distance))  # keep only pairs of households for which we know the distance between them
dat$weight = dat$Distance
#dat = na.omit(dat)                                                  
g.dist = graph.data.frame(dat, vertices=households, directed=F)
g.dist = simplify(g.dist)


#################################################################################
## Assortativity on zhubo
##
round( assortativity.nominal(g.farm,    types=nodes.zhubo, directed=T), 3)
round( assortativity.nominal(g.gift,    types=nodes.zhubo, directed=T), 3)
round( assortativity.nominal(g.spouse,  types=nodes.zhubo, directed=F), 3)
round( assortativity.nominal(g.kids,    types=nodes.zhubo, directed=F), 3)

net_names = c("Farm help", "Gifts", "Partners", "Children")

obs.recip = data.frame(Network = net_names,
                       
                       reciprocity = c(reciprocity(g.farm), 
                                       reciprocity(g.gift), 
                                       reciprocity(g.spouse), 
                                       reciprocity(g.kids)),
                       
                       assortment  = c(assortativity.nominal(g.farm,    types=nodes.zhubo, directed=T),
                                       assortativity.nominal(g.gift,    types=nodes.zhubo, directed=T),
                                       assortativity.nominal(g.spouse,  types=nodes.zhubo, directed=F),
                                       assortativity.nominal(g.kids,    types=nodes.zhubo, directed=F)))



#################################################################################
## simulate random behaviour in networks
##
#' Title
#'
#' @param n.egos 
#' @param n.alters 
#' @param ties A vector containing each node's out-degree
#' @param labels A list of labels, one for each ego (use `nodes.zhubo`)
#' @param n.sims 
#'
#' @return
#' @export
#'
#' @examples
neutral.network.sim = function(n.egos, n.alters, ties, labels, n.sims=1000) {
  require(igraph)
  
  ## debug ###
  # n.egos = sim.gifts.dat$n_ego; n.alters = sim.gifts.dat$n_alter; ties=sim.gifts.dat$ties; n.sims=10
  # labels=nodes.zhubo
  ###
  
  # blank.matrix = matrix(0, n.alters - n.egos, n.alters)  # blank matrix to tag onto end (since iGraph needs symmetrical matrix to generate graphs)
  
  # set up dataframes for results and edges
  recip = data_frame(i=integer(0), reciprocity=numeric(0), assortment=numeric(0))
  nodes = data_frame(id=1:length(labels))
  
  for (i in 1:n.sims) {
    # gifts.rnd = matrix(0, n.egos, n.alters)
    edgelist = data_frame(ego=integer(0), alter=integer(0))
    egos = c()
    alters = c()
    
    # each row (giver) can give at least one, up to `n.ties` gifts/helps
    for (j in 1:n.egos) {
      recipients = sample(1:n.alters, ties[j], replace=F)  # choose ties[j] number of recipients
      # gifts.rnd[j, recipients] = 1  # add an edge
      
      egos = c(egos, 
               rep(j, ifelse(length(recipients) > length(ego), length(recipients), 1)))
      
      alters = c(alters, recipients)
      # edgelist = add_row(edgelist, ego=j, alter=recipients)
    }
    
    edgelist = add_row(edgelist, ego=egos, alter=alters)
    
    # make symmetrical matrix
    # gifts.rnd = rbind(gifts.rnd, blank.matrix)
    # generate the graph from adj matrix
    # g.tmp = graph.adjacency(gifts.rnd, mode="directed", diag=F)
    
    g.tmp = graph.data.frame(edgelist, vertices=nodes, directed=T)

    # save results
    recip = add_row(recip, i=i, reciprocity=reciprocity(g.tmp), assortment=assortativity.nominal(g.tmp, types=labels, directed=T))
  }
  
  return(recip)
}


#################################################################################
## Simulate random networks
##
# helper function to get numbers of egos and alters in a network, as well as a list of each ego's number of ties
# e.g. filter_str="TotalGifts.ind > 0"
net_struct = function(filter_str=""){
  dat = hh.dyads %>% 
    filter_(filter_str) %>% 
    dplyr::select(Ego.HH, Alter.HH)
  
  edges = dat %>% 
    group_by(Ego.HH) %>% 
    dplyr::summarise(ties = n())
  
  nodes = dat %>% 
    summarise(n_ego   = length(unique(Ego.HH)),
              n_alter = length(unique(Alter.HH)))
  
  return(list(
    n_ego = nodes$n_ego,
    n_alter = nodes$n_alter,
    ties = edges$ties
  ))
}

# gifts
sim.gifts.dat = net_struct("TotalGifts.ind > 0")
sim.gifts = neutral.network.sim(sim.gifts.dat$n_ego, sim.gifts.dat$n_alter, sim.gifts.dat$ties, nodes.zhubo)
sim.gifts$Network = "Gifts"

# farm help
sim.farm.dat = net_struct("HelpObserved > 0")
sim.farm = neutral.network.sim(sim.farm.dat$n_ego, sim.farm.dat$n_alter, sim.farm.dat$ties, nodes.zhubo)
sim.farm$Network = "Farm help"

# spouses
sim.spouse.dat = net_struct("AnySpousesInAlter > 0")
sim.spouse = neutral.network.sim(sim.spouse.dat$n_ego, sim.spouse.dat$n_alter, sim.spouse.dat$ties, nodes.zhubo)
sim.spouse$Network = "Partners"

# kids
sim.kids.dat = net_struct("TotalChildrenInAlter > 0")
sim.kids = neutral.network.sim(sim.kids.dat$n_ego, sim.kids.dat$n_alter, sim.kids.dat$ties, nodes.zhubo)
sim.kids$Network = "Children"

# bosh them all together
sim.nets = dplyr::bind_rows(sim.gifts, sim.farm, sim.spouse, sim.kids)

sim.nets$Network = factor(sim.nets$Network, levels=net_names)


#################################################################################
## plot assortativities
##
##
ggplot(sim.nets, aes(x=factor(Network), y=assortment)) + 
  geom_boxplot() + 
  geom_point(data=obs.recip, aes(x=factor(Network), y=assortment), fill="red", colour="red", shape=23, size=3) + 
  
  geom_blank(data=obs.recip) +
  #ylim(c(0, round(max(obs.recip$Value), 1))) +  # extend y-axis so observed reciprocities will appear
  ylab("Simulated/observed reciprocity") +
  xlab("Network") +
  common_theme

ggsave(file.path(plots.dir, "assortment.png"), width=10, height=10, units="cm")
ggsave(file.path(plots.dir, "assortment.pdf"), width=10, height=10, units="cm")


#################################################################################
## Community detection
##
#' Function returning detected communities and a sub-graph containing only nodes that belong to communities containing > 1 node
#'
#' @param g graph, The graph you want to detect clusters for.
#' @param g.fg communities, The output of some_clustering_algorithm(g)
#' @return A list containing the subset of communities and a graph containing the subset of nodes
#' 
reduce_communities = function(g, g.fg) {
  # re-run community detection
  #g.fg  = fastgreedy.community(g)
  #spouse.fg.sub = spouse.fg[ sizes(spouse.fg) > 1 ]  # list of households per cluster (where >1 HH in each cluster)
  
  # get list of households in a cluster by themselves
  solo_houses = unlist( g.fg[ sizes(g.fg) == 1 ] )
  
  # subset cluster memberhips to keep only nodes clusters containing > 1 house
  mem = membership(g.fg)
  mem = mem[ !V(g)$name %in% solo_houses ]
  
  # get communities for each of the remaining houses
  # this line of code is from igraph:::communities -- the output of this is passed to the 'mark.groups' parameter in plot()
  com = igraph:::groups.default(list(membership = mem))
  
  g.sub = induced_subgraph(graph=g, !V(g)$name %in% solo_houses)
  
  return( list( 
    com.reduced = com,
    membership = mem,
    g.sub = g.sub
  ))
}


##
## Plot detected communities for all villages
##
# plotting options
node_size = 4
edge_width = 1

#zhu_colours = c("black", "red")      # zhu houses will be red
zhu_shapes = c("circle", "square")   # zhu houses will be squares
#l <- layout.fruchterman.reingold(all, niter=100)

village_ids = unique(hh.dyads$Ego.VillageID)

for (village in village_ids)
{
  # subset graphs to keep only current village
  g.farm.v2   = induced_subgraph(graph=g.farm,   which(V(g.farm)$Ego.VillageID==village))
  g.gift.v2   = induced_subgraph(graph=g.gift,   which(V(g.gift)$Ego.VillageID==village))
  g.kids.v2   = induced_subgraph(graph=g.kids,   which(V(g.kids)$Ego.VillageID==village))
  g.spouse.v2 = induced_subgraph(graph=g.spouse, which(V(g.spouse)$Ego.VillageID==village))
  
  # get communities with > 1 house in each graph
  reduced.farm   = reduce_communities(g.farm.v2,   cluster_edge_betweenness(g.farm.v2))
  reduced.gift   = reduce_communities(g.gift.v2,   cluster_edge_betweenness(g.gift.v2))
  reduced.kids   = reduce_communities(g.kids.v2,   fastgreedy.community(g.kids.v2))
  reduced.spouse = reduce_communities(g.spouse.v2, fastgreedy.community(g.spouse.v2))

  #png(file=paste0("clusters - village ", village, ".png"), height=30, width=30, units="cm", res=300)
  pdf(file=file.path(plots.dir, paste0("clusters - village ", village, ".pdf")), height=12, width=12)
  par(mfrow=c(2,2))
  
  plot(reduced.farm$g.sub, #margin=-0.1,
       vertex.label=NA, vertex.size=node_size, vertex.shape=zhu_shapes[V(reduced.farm$g.sub)$Ego.zhubo1 + 1],
       #vertex.color=zhu_colours[V(reduced.farm$g.sub)$Ego.zhubo1 + 1], 
       vertex.color=reduced.farm$membership,
       edge.width=edge_width, edge.arrow.size=0.5, edge.arrow.width=0.5  # make the edges a bit prettier
       #mark.groups = reduced.farm$com.reduced, mark.expand = 20
       )
  #title("Farm labour", adj=0)
  title("(a)", adj=0)
  
  plot(reduced.gift$g.sub, #margin=-0.1,
       vertex.label=NA, vertex.size=node_size, vertex.shape=zhu_shapes[V(reduced.gift$g.sub)$Ego.zhubo1 + 1],
       #vertex.color=zhu_colours[V(reduced.gift$g.sub)$Ego.zhubo1 + 1],
       vertex.color=reduced.gift$membership,
       edge.width=edge_width, edge.arrow.size=0.5, edge.arrow.width=0.5  # make the edges a bit prettier
       #mark.groups = reduced.gift$com.reduced, mark.expand = 20
       )
  #title("Gifts", adj=0)
  title("(b)", adj=0)
  
  # library(dplyr)
  # unlist(reduced.gift$com.reduced) %>% as_data_frame() %>% summarise(n())
  
  plot(reduced.spouse$g.sub, #margin=-0.1,
       vertex.label=NA, vertex.size=node_size, vertex.shape=zhu_shapes[V(g.spouse)$Ego.zhubo1 + 1],
       #vertex.color=zhu_colours[V(g.spouse)$Ego.zhubo1 + 1],
       vertex.color=reduced.spouse$membership,
       edge.width=edge_width, edge.arrow.size=0.5, edge.arrow.width=0.5  # make the edges a bit prettier
       #mark.groups = reduced.spouse$com.reduced, mark.expand = 20
       )
  #title("Partners", adj=0)
  title("(c)", adj=0)
  
  plot(reduced.kids$g.sub, #margin=-0.1,
       vertex.label=NA, vertex.size=node_size, vertex.shape=zhu_shapes[V(reduced.kids$g.sub)$Ego.zhubo1 + 1],
       #vertex.color=zhu_colours[V(reduced.kids$g.sub)$Ego.zhubo1 + 1], 
       vertex.color=reduced.kids$membership,
       edge.width=edge_width, edge.arrow.size=0.5, edge.arrow.width=0.5  # make the edges a bit prettier
       #mark.groups = reduced.kids$com.reduced, mark.expand = 20
       )
  #title("Children", adj=0)
  title("(d)", adj=0)
  
  dev.off()
  par(mfrow=c(1,1))
  
  print(paste0("Printed village ", village))
}
