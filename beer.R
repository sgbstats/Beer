library(tidyverse)
library(readxl)
beer=read_excel("beer.xlsx")
write.csv(beer, "beer.csv")


beer=beer%>%mutate(diff=MyRating-GlobalRating,
                   n=row_number())


plot(density(beer$diff))

summary(lm(data=beer, formula = MyRating~GlobalRating))
cor.test(beer$MyRating, beer$GlobalRating)

tapply(beer$diff, beer$Style, summary)

beer%>%group_by(Style)%>%summarise(n=n(), mean=mean(diff), sd=sd(diff, na.rm=T))

count=beer%>%group_by(Style)%>%tally()

ggplot(data=beer, aes(y=MyRating, color=Style, x=Style))+geom_boxplot(width=0.5)+
  geom_text(data = count,
            aes(Style, Inf, label = n), vjust = 1)+
  theme_bw()+
  theme(legend.position = "none")
ggsave("Plots/Ratingbystlye.png", device = "png", height = 5.68, width = 9.4, units = "in")
  

ggplot(data=beer, aes(x=GlobalRating, y=MyRating))+
  geom_point(aes(color=Style))+
  geom_smooth(method="lm", se=F)+
  theme_bw()
ggsave("Plots/Ratingbyglobal.png", device = "png", height = 5.68, width = 9.4, units = "in")

beer %>% ggplot(aes(x=Date, y=n))+
  geom_line()+
  ylab("Cumulative Beers")+
  theme_bw()
ggsave("Plots/Culativebeers.png", device = "png", height = 5.68, width = 9.4, units = "in")

  

