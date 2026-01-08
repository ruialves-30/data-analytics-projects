library(geoR)
library(spmodel)
library(spData)


?soil250
?wrc
head(wrc)
data(soil250)
names(soil250)

wrc_soil <- cbind(wrc, Areia = soil250$AGrossa, Silte = soil250$Silte, Argila = soil250$Argila)
head(wrc_soil)

#Criação dos Datasets Geoestatísticos
?as.geodata
water5 <- as.geodata(wrc_soil, data.col = 4, covar.col = c(3, 12:14))
plot(water5)
summary(lm(water5$data~., data = water5))

water10 <- as.geodata(wrc_soil, data.col = 5, covar.col = c(3, 12:14))
plot(water10)
summary(lm(water10$data~., data = water10))

water100 <- as.geodata(wrc_soil, data.col = 7, covar.col = c(3, 12:14))
plot(water100)
summary(lm(water100$data~., data = water100))

water15300 <- as.geodata(wrc_soil, data.col = 11, covar.col = c(3, 12:14))
plot(water15300)
summary(lm(water15300$data~., data = water100))

#Perguntas Iniciais

#Quantas observações : 250 observações
summary(water5)
summary(water10)
summary(water100)
summary(water15300)

#Quantas covariaveis - 4 (Densidade, Areia, Silte, Argila)
head(wrc_soil)

#Como estão as observações distribuidas?
plot(water5)
points(water5)

plot(water10)
points(water10)

plot(water100)
points(water100)

plot(water15300)
points(water15300)

#Observações correlacionadas no espaço? - Sim

summary(lm(water5$data~., data = water5))
#CoordX, Densidade e Argila

summary(lm(water10$data~., data = water10))
#CoordX, Densidade e Argila

summary(lm(water100$data~., data = water100))
#CoordX, CoordY, Densidade

summary(lm(water15300$data~., data = water100))
#CoordY, Densidade, Silte e Argila

#Objetivos - Inferência sobre parametros do modelo para a retencao de agua a diferentes pressões, avaliar a influencia das diferentes covariaveis.
# Realizar Previsão para novos locais 

#EDA Não Espacial
par(mfrow=c(2,2))

summary(water5$data)
sd(water5$data)
hist(water5$data)

summary(water10$data)
sd(water10$data)
hist(water10$data)

summary(water100$data)
sd(water100$data)
hist(water100$data)

summary(water15300$data)
sd(water15300$data)
hist(water15300$data)


boxplot(water5$data,
        water10$data,
        water100$data,
        water15300$data,
        names = c("5 mca", "10 mca", "100 mca", "15300 mca"),
        col = "lightblue",
        main = "Comparação das Distribuições de Retenção de Água")


#Analise das Quantidades de Sedimento no solo
summary(wrc_soil$Areia)
sd(wrc_soil$Areia)
summary(wrc_soil$Silte)
sd(wrc_soil$Silte)
summary(wrc_soil$Argila)
sd(wrc_soil$Argila)
boxplot(
  wrc_soil$Areia,
  wrc_soil$Silte,
  wrc_soil$Argila,
  names = c("Areia", "Silte", "Argila"),
  col = "brown",
  main = "Boxplots da composição granulométrica do solo"
)

par(mfrow=c(1,3))
hist(wrc_soil$Areia, nclass = 6)
hist(wrc_soil$Silte)
hist(wrc_soil$Argila)

#Analise Covariavel Densidade
summary(wrc_soil$Densidade)
sd(wrc_soil$Densidade)
par(mfrow=c(1,2))
boxplot(wrc_soil$Densidade, main = "Boxplot da Densidade do Solo")
hist(wrc_soil$Densidade, main = "Histograma da Densidade do Solo")

#Análise Espacial
summary(water5)
summary(water10)
summary(water100)
summary(water15300)

#Coordinates summary
#CoordX CoordY
#min      0      0
#max     45    120

#Distance summary
#min      max 
#5.0000 128.1601

plot(water5)
plot(water10)
plot(water100)
plot(water15300)


points(water5, main = "Retencao de Agua a 5mca")
points(water10, main = "Retencao de Agua a 10mca")
points(water100, main = "Retencao de Agua a 100mca")
points(water15300, main = "Retencao de Agua a 15300mca")

#Estimação de superficie de Retenção de Água

library(spatstat)

#janela de visualização
w <- owin(xrange = c(0, 45),
          yrange = c(0, 120))


par(mfrow=c(2,2))
par(mar=rep(0.5, 4))


dados.ppp5 <- ppp(x=water5$coords[,1], y=water5$coords[,2], window = w, marks=water5$data)
aux5 <- Smooth(dados.ppp)
plot(aux, main = "Retencao de Agua a 5mca")
points(water5, add=T)

dados.ppp10 <- ppp(x=water10$coords[,1], y=water10$coords[,2], window = w, marks=water10$data)
aux10 <- Smooth(dados.ppp10)
plot(aux10, main = "Retencao de Agua a 10mca")
points(water10, add=T)

dados.ppp100 <- ppp(x=water100$coords[,1], y=water100$coords[,2], window = w, marks=water100$data)
aux100 <- Smooth(dados.ppp100)
plot(aux100, main = "Retencao de Agua a 100mca")
points(water100, add=T)

dados.ppp15300 <- ppp(x=water15300$coords[,1], y=water15300$coords[,2], window = w, marks=water15300$data)
aux15300 <- Smooth(dados.ppp15300)
plot(aux15300, main = "Retencao de Agua a 15300mca")
points(water5, add=T)

# obter mínimo e máximo global das imagens
zmin <- min(aux5$v, aux10$v, aux100$v, aux15300$v, na.rm=TRUE)
zmax <- max(aux5$v, aux10$v, aux100$v, aux15300$v, na.rm=TRUE)

# gráficos
par(mfrow=c(2,2), mar=rep(0.5,4))

plot(aux5, zlim=c(zmin,zmax), main="Retenção de Água a 5 mca")
points(dados.ppp5, pch=20)

plot(aux10, zlim=c(zmin,zmax), main="Retenção de Água a 10 mca")
points(dados.ppp10, pch=20)

plot(aux100, zlim=c(zmin,zmax), main="Retenção de Água a 100 mca")
points(dados.ppp100, pch=20)

plot(aux15300, zlim=c(zmin,zmax), main="Retenção de Água a 15300 mca")
points(dados.ppp15300, pch=20)

#Modelação

#Verificar tendencias (alfa = 0.05)
summary(lm(water5$data~., data = water5))
#CoordX, Densidade e Argila

summary(lm(water10$data~., data = water10))
#CoordX, Densidade e Argila

summary(lm(water100$data~., data = water100))
#CoordX, CoordY, Densidade

summary(lm(water15300$data~., data = water100))
#CoordY, Densidade, Silte e Argila

#Variavel reposta é retencao de agua - continua
#Averiguar normalidade
shapiro.test(water5$data)

shapiro.test(water10$data)

shapiro.test(water100$data) #normalidade

shapiro.test(water15300$data)

water5
summary(water5)
128*0.6

#Estimação dos variogramas empíricos
?variog
vg_emp <- variog(water5, trend =~ water5$coords[,1] + water5$covariate[,1] + water5$covariate[,4], max.dist = 80)
plot(vg_emp, main = "Variograma empírico - water5")

summary(water10)
vg_emp2 <- variog(water10, trend =~ water10$coords[,1] + water10$covariate[,1] + water10$covariate[,4], max.dist = 80)
plot(vg_emp2, main = "Variograma empírico - water10")

summary(water100)
vg_emp3 <- variog(water100, trend =~ water100$coords[,1] + water100$coords[,2] +  water100$covariate[,1], max.dist = 80)
plot(vg_emp3, main = "Variograma empírico - water100")

summary(water15300)
vg_emp4 <- variog(water15300, trend =~ water15300$coords[,2] + water15300$covariate[,1] +  water15300$covariate[,3] + water15300$covariate[,4], max.dist = 80)
plot(vg_emp4, main = "Variograma empírico - water15300")

#Variogramas Teóricos (Irei utilizar apenas Water100)

#Water5
plot(vg_emp)
mq_esf_1<- variofit(vg_emp, cov.model="spherical", ini.cov.pars=c(1e-4,10), nugget=3e-4)
lines(mq_esf_1, col="#4F94CD")

mq_exp_1<- variofit(vg_emp, cov.model="exponential", ini.cov.pars=c(1e-4,10), nugget=3e-4)
lines(mq_exp_1, col="red")

mv_exp_1 <- likfit(water5, trend =~ water5$coords[,1] + water5$covariate[,1] + water5$covariate[,4], ini.cov.pars=c(1e-4,10), nugget=3e-4, cov.model="exponential")
lines(mv_exp_1, col="green")

mv_esf_1 <- likfit(water5, trend =~ water5$coords[,1] + water5$covariate[,1] + water5$covariate[,4], ini.cov.pars=c(1e-4,10), nugget=3e-4, cov.model="spherical")
lines(mv_esf_1, col="purple")

legend( "bottomright",
  legend = c(
    "MQ – Esférico",
    "MQ – Exponencial",
    "MV – Exponencial",
    "MV – Esférico"
  ),
  col = c("#4F94CD", "red", "green", "purple"),
  lty = 1,
  lwd = 2,
  bty = "n"
)

mq.esf.cv.1 <- xvalid(water5, model = mq_esf_1)
mq.exp.cv.1 <- xvalid(water5, model = mq_exp_1)
mv.esf.cv.1 <- xvalid(water5, model = mv_esf_1)
mv.exp.cv.1 <- xvalid(water5, model = mv_exp_1)

#CV - water5

#mq.esf
mean(mq.esf.cv.1$error) #1.70e-5
mean(mq.esf.cv.1$std.error^2)#1.14

#mq.exp
mean(mq.exp.cv.1$error)#2.39e-5
mean(mq.exp.cv.1$std.error^2)#1.26

#mv.esf
mean(mv.esf.cv.1$error) #1.46e-5
mean(mv.esf.cv.1$std.error^2) #1.03

#mv.exp
mean(mv.exp.cv.1$error)#1.46e-5
mean(mv.exp.cv.1$std.error^2)#inf

#Water10
plot(vg_emp2)
mq_esf_2<- variofit(vg_emp2, cov.model="spherical", ini.cov.pars=c(1e-4,10), nugget=3.5e-4)
lines(mq_esf_2, col="#4F94CD")

mq_exp_2<- variofit(vg_emp2, cov.model="exponential", ini.cov.pars=c(1e-4,10), nugget=3.5e-4)
lines(mq_exp_2, col="red")

mv_exp_2 <- likfit(water10, trend =~ water10$coords[,1] + water10$covariate[,1] + water10$covariate[,4], ini.cov.pars=c(1e-4,10), nugget=3.5e-4, cov.model="exponential")
lines(mv_exp_2, col="green")

mv_esf_2 <- likfit(water10, trend =~ water10$coords[,1] + water10$covariate[,1] + water10$covariate[,4], ini.cov.pars=c(1e-4,10), nugget=3.5e-4, cov.model="spherical")
lines(mv_esf_2, col="purple")

legend( "bottomright",
        legend = c(
          "MQ – Esférico",
          "MQ – Exponencial",
          "MV – Exponencial",
          "MV – Esférico"
        ),
        col = c("#4F94CD", "red", "green", "purple"),
        lty = 1,
        lwd = 2,
        bty = "n"
)

mq.esf.cv.2 <- xvalid(water10, model = mq_esf_2)
mq.exp.cv.2 <- xvalid(water10, model = mq_exp_2)
mv.esf.cv.2 <- xvalid(water10, model = mv_esf_2)
mv.exp.cv.2 <- xvalid(water10, model = mv_exp_2)

#CV - water10

#mq.esf
mean(mq.esf.cv.2$error) #5.78e-6
mean(mq.esf.cv.2$std.error^2)#1.13

#mq.exp
mean(mq.exp.cv.2$error)#8.55e-6
mean(mq.exp.cv.2$std.error^2)#1.20

#mv.esf
mean(mv.esf.cv.2$error) #6.10e-6
mean(mv.esf.cv.2$std.error^2) #1.03

#mv.exp
mean(mv.exp.cv.2$error)#6.10e-6
mean(mv.exp.cv.2$std.error^2)#inf

#Water100
plot(vg_emp3, main = "Variogramas Teoricos")
mq_esf_3<- variofit(vg_emp3, cov.model="spherical", ini.cov.pars=c(0.0001,30), nugget=0.00014)
lines(mq_esf_3, col="#4F94CD")

mq_exp_3<- variofit(vg_emp3, cov.model="exponential", ini.cov.pars=c(0.0001,30), nugget=0.00014)
lines(mq_exp_3, col="red")

mv_exp_3 <- likfit(water100, trend =~ water100$coords[,1] + water100$coords[,2] +  water100$covariate[,1], ini.cov.pars=c(0.0001,30), nugget=0.00014, cov.model="exponential")
lines(mv_exp_3, col="green")

mv_esf_3 <- likfit(water100, trend =~ water100$coords[,1] + water100$coords[,2] +  water100$covariate[,1], ini.cov.pars=c(0.0001,30), nugget=0.00014, cov.model="spherical")
lines(mv_esf_3, col="purple")

legend( "bottomright",
        legend = c(
          "MQ – Esférrrico",
          "MQ – Exponencial",
          "MV – Exponencial",
          "MV – Esférrrico"
        ),
        col = c("#4F94CD", "red", "green", "purple"),
        lty = 1,
        lwd = 2,
        bty = "n"
)

mq.esf.cv.3 <- xvalid(water100, model = mq_esf_3)
mq.exp.cv.3 <- xvalid(water100, model = mq_exp_3)
mv.esf.cv.3 <- xvalid(water100, model = mv_esf_3)
mv.exp.cv.3 <- xvalid(water100, model = mv_exp_3)

#CV - water100

#mq.esf
mean(mq.esf.cv.3$error) #-3.87e-5
mean(mq.esf.cv.3$std.error^2)#1.17

#mq.exp
mean(mq.exp.cv.3$error)#-2.84e-5
mean(mq.exp.cv.3$std.error^2)#1.13

#mv.esf
mean(mv.esf.cv.3$error) #-3.16e-5
mean(mv.esf.cv.3$std.error^2) #1.00(52)

#mv.exp
mean(mv.exp.cv.3$error)#-2.73e-5
mean(mv.exp.cv.3$std.error^2)#1.00(56)
#Modelo escolhido - mv.exp - mv_exp_3

#Water15300
plot(vg_emp4)
mq_esf_4<- variofit(vg_emp4, cov.model="spherical", ini.cov.pars=c(0.00020,15), nugget=0.00020)
lines(mq_esf_4, col="#4F94CD")

mq_exp_4<- variofit(vg_emp4, cov.model="exponential", ini.cov.pars=c(0.00020,15), nugget=0.00020)
lines(mq_exp_4, col="red")

mv_exp_4 <- likfit(water15300,  trend =~ water15300$coords[,2] + water15300$covariate[,1] +  water15300$covariate[,3] + water15300$covariate[,4], ini.cov.pars=c(0.00020,15), nugget=0.00020, cov.model="exponential")
lines(mv_exp_4, col="green")

mv_esf_4 <- likfit(water15300,  trend =~ water15300$coords[,2] + water15300$covariate[,1] +  water15300$covariate[,3] + water15300$covariate[,4], ini.cov.pars=c(0.00020,15), nugget=0.00020, cov.model="spherical")
lines(mv_esf_4, col="purple")

legend( "bottomright",
        legend = c(
          "MQ – Esférico",
          "MQ – Exponencial",
          "MV – Exponencial",
          "MV – Esférico"
        ),
        col = c("#4F94CD", "red", "green", "purple"),
        lty = 1,
        lwd = 2,
        bty = "n"
)

?xvalid
mq.esf.cv.4 <- xvalid(water15300, model = mq_esf_4)
mq.exp.cv.4 <- xvalid(water15300, model = mq_exp_4)
mv.esf.cv.4 <- xvalid(water15300, model = mv_esf_4)
mv.exp.cv.4 <- xvalid(water15300, model = mv_exp_4)

#CV - water15300

#mq.esf
mean(mq.esf.cv.4$error) #-1.00e-5
mean(mq.esf.cv.4$std.error^2)#0.92

#mq.exp
mean(mq.exp.cv.4$error)#-1.40e-5
mean(mq.exp.cv.4$std.error^2)#1.10

#mv.esf
mean(mv.esf.cv.4$error) #-4.68e-6
mean(mv.esf.cv.4$std.error^2) #1.02

#mv.exp
mean(mv.exp.cv.4$error)#-6.57e-6
mean(mv.exp.cv.4$std.error^2)#0.96


#Interpolação Espacial - Onde faz sentido realizar predição?
points(water100, main = "Retencao de Água a 100mca")

#previsao sobre grelha aleatoria - tenho de fazer predicao da densidade por kriging
set.seed(3010)
xx <- runif(150, -5, 50)
yy <- runif(150, 0, 120)
points(cbind(xx,yy), col="red", cex=1)

novos.locs <- cbind(xx,yy)
points(novos.locs, col="red", cex=1)

#predicao da covariavel densidade por kriging

summary(water100)

dens <- cbind(densidade = water100$covariate[,1], X = water100$coords[,1], Y = water100$coords[,2], Areia = water100$covariate[,2], Silte = water100$covariate[,3], Argila = water100$covariate[,4]) 
head(dens)


dens_aux <- as.geodata(dens, coords.col = 2:3, data.col = 1, covar.col = 4:6, covar.names = c("Areia", "Silte", "Argila"))
summary(dens_aux)
plot(dens_aux)

aux_dens_lm <- lm(dens_aux$data ~ ., data = dens_aux) #dependencia em Y
summary(aux_dens_lm)

v_dens <- variog(dens_aux, trend =~ dens_aux$coords[,2], max.dist = 80)
plot(v_dens, main="Variograma dos residuos (apos excluir tendencia)")

#Densidade Water100
plot(v_dens)
mq_esf_dens<- variofit(v_dens, cov.model="spherical", ini.cov.pars=c(0.0025,25), nugget=0.0025)
lines(mq_esf_dens, col="#4F94CD")

mq_exp_dens<- variofit(v_dens, cov.model="exponential", ini.cov.pars=c(0.0025,25), nugget=0.0025)
lines(mq_exp_dens, col="red")

mv_exp_dens <- likfit(dens_aux, trend =~ dens_aux$coords[,2], ini.cov.pars=c(0.0025,25), nugget=0.0025, cov.model="exponential")
lines(mv_exp_dens, col="green")

mv_esf_dens <- likfit(dens_aux, trend =~ dens_aux$coords[,2], ini.cov.pars=c(0.0025,25), nugget=0.0025, cov.model="spherical")
lines(mv_esf_dens, col="purple")

legend( "bottomright",
        legend = c(
          "MQ – Esférico",
          "MQ – Exponencial",
          "MV – Exponencial",
          "MV – Esférico"
        ),
        col = c("#4F94CD", "red", "green", "purple"),
        lty = 1,
        lwd = 2,
        bty = "n"
)

mq.esf.cv.dens <- xvalid(dens_aux, model = mq_esf_dens)
mq.exp.cv.dens <- xvalid(dens_aux, model = mq_exp_dens)
mv.esf.cv.dens <- xvalid(dens_aux, model = mv_esf_dens)
mv.exp.cv.dens <- xvalid(dens_aux, model = mv_exp_dens)

#CV - densidade-water100

#mq.esf
mean(mq.esf.cv.dens$error) #1.93e-5
mean(mq.esf.cv.dens$std.error^2)#0.95

#mq.exp
mean(mq.exp.cv.dens$error)#-6.16e-6
mean(mq.exp.cv.dens$std.error^2)#inf

#mv.esf
mean(mv.esf.cv.dens$error) #-3.14e-6
mean(mv.esf.cv.dens$std.error^2) #1.00(20)

#mv.exp
mean(mv.exp.cv.dens$error)#2.02e-6
mean(mv.exp.cv.dens$std.error^2)#1.00(21)
#Modelo escolhido - mv.exp - mv_exp_dens

#Predição da densidade considerando dependencia em Y
#Kriging com trend externa

?krige.control
#var auxiliar que contem a informação de krigagem 
?krige.control()
KC <- krige.control(type.krige = "ok", trend.d =~ dens_aux$coords[,2],
                    trend.l=~ novos.locs[,2], obj=mv_exp_dens)

kr <- krige.conv(geodata=dens_aux, locations=novos.locs, krige=KC)

points(dens_aux)
krig.ests <- as.geodata(cbind(novos.locs, kr$predict))
points(krig.ests, col=2, add=T)
title(main="Observações e estimativas de kriging para a Densidade")

new_dens <- kr$predict

#Mapas mais usuais a 2D
windows()
contour(kr, filled=TRUE, main="Estimativas de Kriging para a Densidade")
contour(kr, val=sqrt(kr$krige.var),
        filled=TRUE, coords.data=novos.locs, main="Desvio Padrao de Kriging")

#Kriging com trend externa para a wrc

?krige.control
#var auxiliar que contem a informação de krigagem 
?krige.control()
KC2 <- krige.control(type.krige = "ok", trend.d =~ water100$coords[,1] + water100$coords[,2] +  water100$covariate[,1], trend.l =~ novos.locs[,1] + novos.locs[,2] + new_dens, obj = mv_exp_3)

kr2 <- krige.conv(geodata=water100, locations=novos.locs, krige=KC)

names(kr2)

points(water100)
krig.ests.2 <- as.geodata(cbind(novos.locs, kr2$predict))
points(krig.ests.2, col=2, add=T)
title(main="Observações e estimativas de kriging para a Retenção da Água a 100 mca")


#Mapas mais usuais a 2D
contour(kr2, filled=TRUE, main="Estimativas de Kriging (com influencia da Densidade)")
contour(kr2, val=sqrt(kr2$krige.var),
        filled=TRUE, coords.data=water100$coords, main="Desvio Padrao de Kriging")


#Dados Agregados por Área
library(spmodel)
library(spData)
library(spdep)


#Dataset Escolhido
library(SpatialEpi)
library(ggplot2)

data(scotland)
?scotland_sf
?scotland
scotland_sf
class(scotland_sf)
plot(scotland_sf)

#plot
ggplot() +
  geom_sf(data = scotland_sf, aes(fill= cases))

?spautor
scotmod <- spautor(cases ~ 1, data = scotland_sf, spcov_type = "car")
summary(scotmod)
head(scotmod$W)
scotmod$W[1,]


#Testes de Associação Espacial


#Teste de Moran

#usando mat vizinhanca de spautor
listw <- mat2listw(scotmod$W, style = "M", zero.policy=TRUE)
moran.test(scotland_sf$cases, listw)

#usando poly2nb
nb <- poly2nb(scotland_sf)        # vizinhança queen padrão
listw2 <- nb2listw(nb, style="W", zero.policy = TRUE)  # pesos normalizados por linha
moran.test(scotland_sf$cases, listw2)

plot(scotland_sf$county.names, card(nb))
card(nb)
summary(card(nb))

#Perguntas Iniciais

#Quantas observações : 56 observações
summary(scotland_sf)
scotland_sf

#Quantas covariaveis - 1 (AFF - percentagem de população que trabalha em pesca, pecuaria e aricultura), tambem há informação sobre numero de casos expectaveis
head(wrc_soil)

#Como estão as observações distribuidas?
ggplot(scotland_sf) +
  geom_sf(aes(fill = cases)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Distribuição Espacial dos Casos Observados",
       fill = "Casos") +
  theme_minimal()


#Observações correlacionadas no espaço? - Sim
listw <- mat2listw(scotmod$W, style = "M", zero.policy=TRUE)
moran.test(scotland_sf$cases, listw)
#p_value = 0.0018 sob H0: Independencia Espacial


#Objetivos - Realizar Testes de Associação Espacial para confirmar dependência espacial. Ajustar modelos GLM e GLMM espaciais CAR/SAR para o numero de casos de cancro do lábio nos condados da Escócia/SMR.
#Realizar predição espacial, avaliar e comparar modelos

#EDA Não Espacial

summary(scotland_sf$cases)
summary(scotland_sf$expected)
summary(scotland_sf$AFF)

par(mfrow = c(1,2))
hist(scotland_sf$cases,
     main = "Histograma dos Casos Observados",
     xlab = "Número de Casos",
     ylab = "Frequência",
     col = "lightblue",
     border = "white")

# Boxplot de cases
boxplot(scotland_sf$cases,
        main = "Boxplot dos Casos Observados",
        ylab = "Número de Casos",
        col = "lightblue")


SMR <- scotland_sf$cases / scotland_sf$expected
summary(SMR)
hist(SMR)

plot(scotland_sf$AFF, scotland_sf$cases)
cor(scotland_sf$AFF, scotland_sf$cases)

#Analise Espacial
library(ggplot2)

ggplot(scotland_sf) +
  geom_sf(aes(fill = cases)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Distribuição Espacial dos Casos Observados",
       fill = "Casos") +
  theme_minimal()

ggplot(data = scotland_sf) +
  geom_sf(aes(fill = SMR), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "magma",
    name = "SMR",
    na.value = "grey90"
  ) +
  labs(
    title = "SMR na Escócia",
    subtitle = "Razão Padronizada de Mortalidade (SMR) por condado"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggplot(scotland_sf) +
  geom_sf(aes(fill = AFF), colour = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(
    name = "AFF",
    option = "C"
  ) +
  labs(
    title = "Distribuição espacial da proporção AFF",
    subtitle = "Proporção da população em agricultura, pesca e silvicultura"
  ) +
  theme_minimal()

max(SMR)

#Modelação

?spautor
lipcases_mod <- spautor(cases ~ 1, family="poisson", data = scotland_sf, spcov_type = "car")
summary(lipcases_mod)

#Modelo Poisson Não Espacial - Referencia

#casesi ∼ Poisson(μi), log(μi) = log(expectedi) + β0 + β1 AFFi
glm_poisson <- glm(cases ~ AFF,  offset = log(expected),  family = poisson,  data = scotland_sf)
summary(glm_poisson)


#Modelo Poisson Espacial (νi é um efeito aleatório estruturado espacialmente )

#log(μ)=log(expected) + Xβ + νi

?spgautor
glmm_poisson_espacial <- spgautor(cases ~ AFF,  family = "poisson", offset = log(scotland_sf$expected),  data = scotland_sf,  spcov_type = "car")
summary(glmm_poisson_espacial)


#Modelo BYM (Besagd-York-Mollié)

#log(μi) = log(expectedi) + Xi β+ ϕi + νi
glmm_poisson_completo <- spgautor(cases ~ AFF,  family = poisson,  offset = log(scotland_sf$expected),  data = scotland_sf,  spcov_type = "car",  random = ~ (1 | scotland_sf$county))
summary(glmm_poisson_completo)



#Métricas de avaliação dos modelos
library(Metrics)
?RMSE
AIC(glm_poisson)
AIC(glmm_poisson_completo)
AIC(glmm_poisson_espacial)

glances(glmm_poisson_espacial, glmm_poisson_completo)
deviance(glm_poisson) #maior desvio

rmse_glm <- rmse(scotland_sf$cases, scotland_sf_pred_glm) 
rmse_glmm_car <- rmse(scotland_sf$cases, aug_car$.fitted)
rmse_glmm_bym <- rmse(scotland_sf$cases, aug_bym$.fitted)

mae_glm <- mae(scotland_sf$cases, scotland_sf_pred_glm) 
mae_glmm_car <- mae(scotland_sf$cases, aug_car$.fitted)
mae_glmm_bym <- mae(scotland_sf$cases, aug_bym$.fitted)

#risco relativo - casos/casos expectaveis
rr_glm <- scotland_sf_pred_glm / scotland_sf$expected
rr_car <- aug_car$.fitted / scotland_sf$expected
rr_bym <- aug_bym$.fitted / scotland_sf$expected

mae_rr_glm <- mae(SMR, rr_glm)
mae_rr_car <- mae(SMR, rr_car)
mae_rr_bym <- mae(SMR, rr_bym)

#Análise de Resíduos

#Variancia
var(glm_poisson$residuals)
var(aug_car$.std.resid)
var(aug_bym$.std.resid)

#Autocorrelação Resiudos
#H0: Independência Espacial
moran.test(glm_poisson$residuals, listw2) #p_value <0.001 - nao se verifica independencia espacial - residuos correlacionados
moran.test(aug_car$.resid, listw2)
moran.test(aug_bym$.resid, listw2)

cv_car <- loocv(glmm_poisson_espacial)
cv_bym <- loocv(glmm_poisson_completo)

mean(cv_car$MSPE)
mean(cv_bym$MSPE)

#Previsoes

scotland_sf_pred_glm <- predict(
  glm_poisson,
  type = "response"
)
scotland_sf_rr_glm <- scotland_sf_pred_glm / scotland_sf$expected


aug_car <- augment(glmm_poisson_espacial)
head(aug_car)
aug_bym <- augment(glmm_poisson_completo)

summary(scotland_sf_pred_glm)
summary(aug_car$.fitted)
summary(aug_bym$.fitted)


#graficos
scotland_plot <- scotland_sf
scotland_plot$smr_obs <- scotland_plot$cases / scotland_plot$expected
scotland_plot$pred_glm <- predict(
  glm_poisson,
  type = "response"
)
scotland_plot$pred_car <- augment(glmm_poisson_espacial)$.fitted
scotland_plot$pred_bym <- augment(glmm_poisson_completo)$.fitted

scotland_plot$rr_glm <- scotland_plot$pred_glm / scotland_plot$expected
scotland_plot$rr_car <- scotland_plot$pred_car / scotland_plot$expected
scotland_plot$rr_bym <- scotland_plot$pred_bym / scotland_plot$expected

library(tidyr)
library(dplyr)

map_data <- scotland_plot %>%
  select(geometry, smr_obs, rr_glm, rr_car, rr_bym) %>%
  pivot_longer(
    cols = -geometry,
    names_to = "modelo",
    values_to = "risco"
  ) %>%
  mutate(
    modelo = factor(
      modelo,
      levels = c("smr_obs", "rr_glm", "rr_car", "rr_bym"),
      labels = c(
        "SMR observado",
        "GLM Poisson",
        "Poisson espacial (CAR)",
        "BYM"
      )
    )
  )
lims <- range(map_data$risco, na.rm = TRUE)

library(ggplot2)

ggplot(map_data) +
  geom_sf(aes(fill = risco), colour = "grey30", linewidth = 0.2) +
  scale_fill_viridis_c(
    name = "Risco relativo",
    limits = lims,
    option = "C"
  ) +
  facet_wrap(~ modelo, ncol = 2) +
  labs(
    title = "Risco relativo observado e predito",
    subtitle = "Comparação entre modelos (Escócia, 1975–1980)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )
