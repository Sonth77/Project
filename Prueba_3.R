##RECOMENDAMOS MIRARA LA EXPLICACIÓN DEL PROCESO DEL CODIGO EN EL ARCHIVO 
#DE R-MARKDOWN
library(ggplot2)
library(dplyr)
library(openxlsx)
library(gganimate)
library(gifski)
library(nortest)
library(knitr)
install.packages("printr")
install.packages("sf")
install.packages("pacman")
library(pacman)
install.packages("priceR")
install.packages("tables")
install.packages("pander")
###CARGAMOS EL ARCHIVO DEL AREA CULTIVADA COMO AreaTD
###CARGAMOS EL ARCHIVO DEL PRODUCCIÓN COMO Prod
###CARGAMOS EL ARCHIVO DEL EXPORTACION COMO Exp
library(readxl)
#Exp <- read_excel("UNI. NACIONAL/Cuarto semestre/Programación Estadistica/Proyecto/Precios_Areas_Prod.xlsx",
#                  sheet = "Exp")

###########################PRIMERO
Prod<-within(Prod,year<-format(Mes,format="%Y"))
AreaTD1=AreaTD[-24,]
prdT<-Prod%>%select(year,Production)%>%
filter(year>="2002")%>%group_by(year)%>% summarise("production"=mean(Production))
ArT<-AreaTD1 %>% select(`2002`,`2003`,`2004`,`2005`,`2006`,`2007`,`2008`,
`2009`,`2010*`,`2011*`,`2012*`,`2013*`,`2014*`,`2015*`,`2016*`,
`2017*`,`2018*`,`2019*`,`2020*`,`2021*`)%>%
summarise("2002"=sum(`2002`),"2003"=sum(`2003`),"2004"=sum(`2004`),"2005"=sum(`2005`),
"2006"=sum(`2006`),"2007"=sum(`2007`),"2008"=sum(`2008`),"2009"=sum(`2009`),
"2010"=sum(`2010*`),"2011"=sum(`2011*`),"2012"=sum(`2012*`),"2013"=sum(`2013*`),
"2014"=sum(`2014*`),"2015"=sum(`2015*`),"2016"=sum(`2016*`),"2017"=sum(`2017*`),
"2018"=sum(`2018*`),"2019"=sum(`2019*`),"2020"=sum(`2020*`),"2021"=sum(`2021*`))


Art<- data.frame("year"=c(2002:2021), "Area"=c(as.numeric(ArT[1,])))

Art$production=prdT$production
vec<-c(2002:2021)
vec[1:20]="production"
vecA<-c(2002:2021)
vecA[1:20]="area"
p1<-data.frame("year"=Art$year,"Valor"=Art$production, "Tipo"=vec)
p2<-data.frame("year"=Art$year,"Valor"=Art$Area, "Tipo"=vecA)
pT<-rbind(p1,p2)
ggplot(pT,aes(year,Valor, group=Tipo, colour=Tipo))+geom_line()+
geom_point(size=2, shape=21, fill="white")+
theme_minimal()+labs(title="Linear comparison between area and production",y="value")

#########################222222222222222222222222

q1<-qqnorm(Art$Area, plot.it = FALSE)
q2<-qqnorm(Art$production, plot.it = FALSE)

plot(range(q1$x,q2$x), range(q1$y,q2$y), type = "n", las=1,
xlab="Theorical Quantiles", ylab="Sample Quantiles")
title("Dispersion of area and production")
points(q1, pch=19)
points(q2, col='red', pch=19)
qqline(Art$Area, lty="dashed")
qqline(Art$production, col='red',lty="dashed")
legend('right', legend=c('Area','Production'), 
       bty='O', col=c('black','red'),pch=19, title = "Legend",cex=0.75)

lillie.test(Art$Area)$p.value
lillie.test(Art$production)$p.value

x=var.test(Art$Area,Art$production, null.value=1,alternative = "two.sided", conf.level = 0.95)
cat(sprintf("el p valor es: %f",x$p.value))



layout(matrix(c(1:2),nrow=1, byrow=FALSE))
layout.show(2)
hist(q1$x, main = "Histograma de la
distribución normal
para el area", xlab = "Area")
hist(q2$x, main = "Histograma de la
distribución normal
para la producción", xlab = "Production")

############################2.b
q1<-qqnorm(Art$Area*10, plot.it = FALSE)
q2<-qqnorm(Art$production, plot.it = FALSE)

layout(matrix(c(1,1),nrow=1, byrow=FALSE))
plot(range(q1$x,q2$x), range(q1$y,q2$y), type = "n", las=1,
xlab="Theorical Quantiles", ylab="Sample Quantiles")
title("Dispersion of area and production with 
          area values halved")
points(q1, pch=19)
points(q2, col='red', pch=19)
qqline(Art$Area*10, lty="dashed")
qqline(Art$production, col='red',lty="dashed")
legend('topleft', legend=c('Area','Production'),
       bty='o', col=c('black','red'),pch=19, title = "Legend", cex = 0.8)

##############################333333333
prdT1<-Prod%>%select(year,Production)%>%
  filter(year>="2002")%>%group_by(year)%>% summarise("production"=sum(Production))

Art<- data.frame("year"=c(2002:2021), "Area"=c(as.numeric(ArT[1,])))
Art$production=prdT1$production
Art1=Art
Art1<-within(Art1,'Bultos/Hectareas(p/a)'<-Art1$production/Art1$Area)
###ESTIMACION DEL PARAMETRO DE BUTOS/HECTAREA O PRODUCCION/AREA
mean(Art1$`Bultos/Hectareas(p/a)`)

sd(Art1$`Bultos/Hectareas(p/a)`)


ggplot(Art1,aes(y=`Bultos/Hectareas(p/a)`,x= year,color=`Bultos/Hectareas(p/a)`))+geom_boxplot()+
geom_point(position = position_jitter(width=.2), stroke=.9, shape=1, size=1)+
labs(y="Bultos/Hectareas")+ 
  ggtitle("Box plot of production by area between 2002-2021")+
  theme(plot.title=element_text(hjust=0.5))


################ULTIMO_PUNTO
library(readxl)

Exp<-within(Exp,year<-format(MES,format="%Y"))
ExpT<-Exp%>%select(year,`Total Exportaciones`)%>%
filter(year>="2002")%>%group_by(year)%>% summarise("Export"=mean(`Total Exportaciones`))

#Exp<-within(Exp,year<-format(MES,format="%Y"))
#ExpT<-Exp%>%select(year,`Total Exportaciones`)%>%
#filter(year>="2002")%>%group_by(year)%>% summarise("Export"=mean(`Total Exportaciones`))

Art$Export=ExpT$Export


ggplot(Art, aes(Area,Export))+geom_point()+
geom_smooth(method="lm", colour="Red")+
labs(x="Area", y="Export")+
ggtitle("Scatter plot with trend line between exports and area" )+
  theme(plot.title=element_text(hjust=0.5))
cor(x=Art$Area,y=Art$Export)
#recta de minimos cuadrados



