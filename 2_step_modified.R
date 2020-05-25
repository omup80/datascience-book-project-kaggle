library(tidyverse)
#setwd("/Users/ecm/teach/data/booknew")
orders = read.csv("orders.csv")
target = read.csv("target.csv")






rfmL = orders %>%
  group_by(id) %>%
  summarise(tof=max(t), r = min(t), fitem=n(), ford=n_distinct(ordnum), m=sum(price*qty)) 



rfmR = orders %>%
  group_by(id, category) %>%
  summarise(fitem=n()) %>%
  spread(category, fitem, fill=0)

rfm = rfmL %>%
  inner_join(rfmR, by="id")


head(rfm)
#summary(rfm)
?spread


all = target %>%
  left_join(rfm, by="id")

train=!is.na(all$logtarg)

cor(all[train, 2:7,])
#create rate variable
#create m over ford
all$buy = as.numeric(all$logtarg>0)
#fit.test = lm(logtarg ~ r + log(fitem) + log(1+ford) + `0` + `5` + `7` + 
#                `8` + `12` + `14` + `19` + `20` + `26` + `27` + `30` + `31` + 
#                `35` + `37` + `39` + `40` + `41` + `50` , data = all, subset = train)
#summary(fit.test)
#step(fit.test)

all$fitem = log(all$fitem)
all$ford = log(1+all$ford)
all$m = log(1+ all$m)
rfm = glm(buy ~ . -id-logtarg , binomial, data=all, subset=train)
summary(rfm)
fit.rfm = step(rfm)
#summary(fit.rfm)
phat = predict(fit.rfm, all[!train,], type="resp")

fit.spend = lm(logtarg ~  r + log(fitem) + log(1+ford) + `0` + `5` + `7` + 
                 `8` + `12` + `14` + `19` + `20` + `26` + `27` + `30` + `31` + 
                 `35` + `37` + `39` + `40` + `41` + `50`, data=all, subset=train&buy==1)
spend = lm(logtarg ~ . -id-logtarg, data=all,  subset=train&buy==1)
fit.spend = step(spend)

#plot(fit)
summary(yhat)
summary(fit.spend)
plot(fit.spend)
yhat = phat * predict(fit.spend, all[!train,])

ans = data.frame(id=all$id[!train], logtarg=yhat)
write.csv(ans, "2_step_1.csv", row.names=F)


