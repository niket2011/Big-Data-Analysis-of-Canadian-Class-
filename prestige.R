library(cars)
library(ggplot2)
Prestige
summary(Prestige)
data = Prestige[,c(1:4)]
data
summary(data)

qplot(education, data=data, geom="histogram", binwidth=1) +
  labs(title = "Histogram of Average Years of Education") +
  labs(x ="Average Years of Education") +
  labs(y = "Frequency") +
  scale_y_continuous(breaks=c(1:20), minor_breaks=NULL) +
  scale_x_continuous(breaks=c(5:17), minor_breaks=NULL) +
  geom_vline(xintercept=mean(data$education), show.legend=TRUE, color="red" )

qplot(income, data=data, geom="histogram", binwidth=1000) +
  labs(title = "Histogram of Average Income") +
  labs(x ="Average Income") +
  labs(y = "Frequency") +
  scale_y_continuous(breaks=c(1:20), minor_breaks=NULL) +
  scale_x_continuous(breaks=c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000, 18000, 20000, 22000, 24000, 26000), minor_breaks=NULL) +
  geom_vline(xintercept=mean(data$income), show.legend=TRUE, color="red" )

education.center = scale(data$education, center=TRUE, scale=FALSE)
prestige.center = scale(data$prestige, center=TRUE, scale=FALSE)

model = lm(income~education.center, data=data)
summary(model)

qplot(education.center, income, data = data, main = "Relationship between Income and Education") +
  stat_smooth(method="lm", col="red") +
  scale_y_continuous(breaks = c(1000, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000, 18000, 20000, 25000), minor_breaks = NULL)

plot(model, pch=16, which=1)

newdata=data[,c(4,2)]

plot(newdata, col="red", type="n",main="Income vs Prestige")
segments(x0=newdata$prestige, y0=newdata$income, x1=newdata$prestige, y1=rep(mean(newdata$income), length(newdata$income)),col=rgb(0,0,1,0.3))
abline(h=mean(newdata$income), col="orange")
points(newdata, col=rgb(0,0,1,0.5), pch=20,cex=1)

model1 = lm(income~education+prestige, data=data)
summary(model1)

library(effects)
plot(allEffects(model1))





