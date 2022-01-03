library(tidyverse)
library(readxl)
setwd("C:/Users/sebas/Google Drive/sideprojects/beer")
beer=read_excel("beer.xlsx")


beer=beer%>%mutate(diff=MyRating-GlobalRating)


plot(density(beer$diff))

summary(lm(data=beer, formula = MyRating~GlobalRating))
cor.test(beer$MyRating, beer$GlobalRating)

tapply(beer$diff, beer$Style, summary)

beer%>%group_by(Style)%>%summarise(n=n(), mean=mean(diff), sd=sd(diff, na.rm=T))

count=beer%>%group_by(Style)%>%tally()

ggplot(data=beer, aes(y=MyRating, color=Style, x=Style))+geom_boxplot(width=0.5)+
  geom_text(data = count,
            aes(Style, Inf, label = n), vjust = 1)+
  theme(legend.position = "none")

ggplot(data=beer, aes(x=GlobalRating, y=MyRating))+
  geom_point(aes(color=Style))+
  geom_smooth(method="lm")


ggplot(data=beer, aes(x=IBU, y=MyRating))+geom_point()

