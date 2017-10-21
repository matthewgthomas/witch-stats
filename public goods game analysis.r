##
## Table 1
##
#dataset muosuo zhubo public good to the house
MZhupg <- read.csv("data/Mosuo_Zhubo_Game for analysis.csv", head=TRUE)

MZhupg$Zhubo1 <- as.factor(MZhupg$Zhubo1)
MZhupg$fPG <- as.factor(MZhupg$HousePG)

wilcox.test(HousePG ~ Zhubo1, data=MZhupg)

table(MZhupg$fPG, MZhupg$Zhubo1)
