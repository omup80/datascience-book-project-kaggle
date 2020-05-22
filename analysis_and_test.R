library(tidyverse)
#setwd("/Users/ecm/teach/data/booknew")
orders = read.csv("orders.csv")
target = read.csv("target.csv")

rfm = orders %>%
  group_by(id) %>%
  summarise(tof=max(t), r = min(t), fitem=n(), ford=n_distinct(ordnum), m=sum(price*qty))
head(rfm)
#summary(rfm)


all = target %>%
  left_join(rfm, by="id")

train=!is.na(all$logtarg)

cor(all[train,2:7])

plot(all[train,2:7])

fit = lm(logtarg ~   tof+ r + fitem + ford+ m, data=all, subset=train)

fit = lm(logtarg ~    r + fitem + ford, data=all, subset=train)
summary(fit)

