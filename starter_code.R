library(tidyverse)
#setwd("/Users/ecm/teach/data/booknew")
orders = read.csv("orders.csv")
target = read.csv("target.csv")

?summarise
rfm = orders %>%
  group_by(id) %>%
  summarise(tof=max(t), r = min(t), fitem=n(), ford=n_distinct(ordnum), m=sum(price*qty))
head(rfm)
summary(rfm)

all = target %>%
  left_join(rfm, by="id")
train=!is.na(all$logtarg)


fit = lm(logtarg ~   r+ fitem + m, data=all, subset=train)
summary(fit)
yhat = predict(fit, all[!train,])
ans = data.frame(id=all$id[!train], logtarg=yhat)
write.csv(ans, "testanswer_simple_regression.csv", row.names=F)


