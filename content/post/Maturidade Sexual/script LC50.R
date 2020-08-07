
#Generating random df
Jovens<- rnorm(n = 50, mean = 10, sd = 2)
Adultos<- rnorm(n = 50, mean = 15, sd = 2)
df<- data.frame(LC = c(Jovens, Adultos), Maturidade = c(rep("Jovem", 50), rep("Adulto", 50)), Adulto = c(rep(0,50), rep(1,50)))

#Regressão binomial
model1<- glm(df$Maturidade ~ df$LC,family='binomial')
summary(model1)


#Encontrando o LC50
LC50 = (log(.5/(1-.5)) - coef(model1)[1])/coef(model1)[2]
LC50

#Plotando o gráfico
require(ggplot2)
ggplot(data=df, aes(x=LC, y=Adulto, label = LC50))+
  geom_point()+
  geom_smooth(method = 'glm', method.args = list(family = "binomial"), se=T, col="red")+ 
  geom_segment(lty = 2, aes(x = 0, y = .5, xend = LC50, yend = .5))+
  geom_segment(lty=2,aes(x = LC50, y = 0, xend = LC50, yend = 0.5))+
  geom_point(x = LC50, y = 0.5, size = 3, shape =21, fill = "red" ) +
  geom_text(label = sprintf(LC50, fmt = '%#.2f'), x = LC50, y = 0.5, hjust=1,vjust=1, size = 5) +
  theme_classic() 



 