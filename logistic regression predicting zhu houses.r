##
## What predicts being a zhu household?
##
## Supplementary Table 4
##
source("init.r")

library(tidyverse)
library(broom)
library(readr)
library(wesanderson)

d.zhu = hh %>% 
  dplyr::select(HH, zhubo1, NumAdults, NumChildren, WealthRank, Sex, Age) %>% 
  distinct() %>% 
  na.omit() %>% 
  mutate(zhubo1 = factor(zhubo1))

# make males the reference category
d.zhu$Sex = relevel(d.zhu$Sex, ref="m")

# fit model
zhu.logit = glm(zhubo1 ~ NumAdults + NumChildren + WealthRank + Sex + Age, 
                data=d.zhu, family=binomial(link = "logit"))

# summarise model and save raw parameter estimates
glance(zhu.logit)

tidy(zhu.logit) %>% 
  bind_cols(confint_tidy(zhu.logit)) %>% 
  write_csv(file.path(results.dir, "predicting zhu label - log-odds.csv"))

# save marginal effects
library(margins)
zhu.logit.margin = margins(zhu.logit, type="response")

summary(zhu.logit.margin) %>% 
  write_csv(file.path(results.dir, "predicting zhu label - marginal effects.csv"))

# how many observations?
nrow(d.zhu)

# predicted probabilities of being labelled, based on wealth and sex of household head
d.zhu.pred = expand.grid(
  WealthRank = 1:6,
  Sex = factor(c("m", "f")),
  NumAdults = mean(d.zhu$NumAdults),
  NumChildren = mean(d.zhu$NumChildren),
  Age = mean(d.zhu$Age)
)

# plot predictions
augment(zhu.logit, newdata=d.zhu.pred, type.predict="response", conf.int=T) %>% 
  mutate(lwr = .fitted - 1.96*.se.fit, upr = .fitted + 1.96*.se.fit) %>%   # calculate CIs

  ggplot(aes(x=WealthRank, y=.fitted)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Sex), alpha=0.3) +
  geom_line(aes(colour=Sex)) +
  
  scale_color_manual(values = wes_palette("Cavalcanti")) + 
  scale_fill_manual(values = wes_palette("Cavalcanti")) + 
  
  ylab("Probability of being labelled") +
  xlab("Wealth rank") +
  theme_classic() +
  theme(legend.position="none")

ggsave(file.path(plots.dir, "predicted zhu labels by sex and wealth rank.png"), height=10, width=10, units="cm")
# ggsave(file.path(plots.dir, "predicted zhu labels by sex and wealth rank.pdf"), height=10, width=10, units="cm")
