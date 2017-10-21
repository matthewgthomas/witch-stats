##
## Supplementary table 3
##
source("init.r")

hh.all = hh.dyads %>% 
  left_join(hh %>% dplyr::select(Ego.HH=HH,   Ego.zhubo1=zhubo1),   by="Ego.HH") %>% 
  left_join(hh %>% dplyr::select(Alter.HH=HH, Alter.zhubo1=zhubo1), by="Alter.HH")


####################################################################################
## Witch stats
##
#hh = unique(subset(hh.all, select=c(Ego.HH, Ego.zhubo1, Ego.VillageID)))
table(hh$zhubo1)  # no. witch houses with Mosuo heads
num.witch.hh = nrow(subset(hh, zhubo1==1))
num.non_witch.hh = nrow(subset(hh, zhubo1==0))

##
## farm help
##
# how many witch houses (and non-witch) helped other witch houses (and non-witch)?
(witch.help = ftable(xtabs(~ Ego.zhubo1 + Alter.zhubo1,
                           data = subset(hh.all, Ego.HH != Alter.HH & HelpObserved > 0))))
# chi square on farm help
help.chi = chisq.test(witch.help)
help.chi$observed
help.chi$expected

##
## gifts
##
(witch.gifts = ftable(xtabs(~ Ego.zhubo1 + Alter.zhubo1,
                            data = subset(hh.all, Ego.HH != Alter.HH & TotalGifts.ind > 0))))
# chi square on gifts
gifts.chi = chisq.test(witch.gifts)
gifts.chi$observed
gifts.chi$expected

##
## partners
##
(witch.partners = ftable(xtabs(~ Ego.zhubo1 + Alter.zhubo1,
                               data = subset(hh.all, Ego.HH != Alter.HH & AnySpousesInAlter > 0))))
# chi square on partners
partners.chi = chisq.test(witch.partners)
partners.chi$observed
partners.chi$expected

##
## children
##
(witch.kids = ftable(xtabs(~ Ego.zhubo1 + Alter.zhubo1,
                           data = subset(hh.all, Ego.HH != Alter.HH & TotalChildrenInAlter > 0))))
# chi square on kids
kids.chi = chisq.test(witch.kids)
kids.chi$observed
round(kids.chi$expected, 1)
