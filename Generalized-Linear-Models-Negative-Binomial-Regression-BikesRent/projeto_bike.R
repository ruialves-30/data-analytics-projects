setwd("C:/Users/afons/Documents/MECD/MLG/projeto")
day <- read.csv("~/MECD/MLG/projeto/day.csv")
View(day)
day <- subset(day, select = -c(instant, dteday, casual, registered))
##Análise Exploratória
library(ggplot2)
library(readr)
library(dplyr)
library(ggcorrplot)
library(reshape2)
day <- basededay
str(day)
day <- na.omit(day)
sum(is.na(day))
attach(day)
#transformar os normalizados em reais para a analise univariada (summary)
desnormalizar_temp <- function(temp_norm) {
  tmin <- -8
  tmax <- 39
  temp_norm * (tmax - tmin) + tmin
}

desnormalizar_atemp <- function(atemp_norm) {
  atemp_norm * 66 - 16
}

desnormalizar_hum <- function(hum_norm) {
  hum_norm * 100
}

desnormalizar_windspeed <- function(wind_norm) {
  wind_norm * 67
}

day$temp_real       <- desnormalizar_temp(day$temp)
day$atemp_real      <- desnormalizar_atemp(day$atemp)
day$hum_real        <- desnormalizar_hum(day$hum)
day$windspeed_real  <- desnormalizar_windspeed(day$windspeed)

summary(day[, c("temp_real", "atemp_real", "hum_real", "windspeed_real")])
table(season)
table(weathersit)

#Analise Univariada
year_freq <- table(day$yr)
year_labels <- c("2011", "2012")
year_pct <- round(100 * year_freq / sum(year_freq), 1)
labels <- paste0(year_labels, ": ", year_pct, "%")
cores <- c("#B3CDE3", "#FBB4AE")
pie(year_freq,
    labels = labels,
    col = cores,
    main = "Distribuição das Observações por Ano")


mnthfator <- factor(day$mnth, levels = 1:12)
frequencias <- as.data.frame(table(mnthfator))
colnames(frequencias) <- c("mnth", "freq")

ggplot(frequencias, aes(x = mnth, y = freq)) +
  geom_bar(stat = "identity", fill = "#DECBE4", color = "white") +
  geom_text(aes(label = freq), vjust = -0.3, size = 4) +
  labs(
    x = "Mês", y = "Frequência") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

holiday_freq <- table(day$holiday)
holiday_labels <- c("Dia normal", "Feriado")
holiday_pct <- round(100 * holiday_freq / sum(holiday_freq), 1)
labels <- paste0(holiday_labels, ": ", holiday_pct, "%")
cores <- c("#AED6F1", "#F9E79F")
pie(holiday_freq,
    labels = labels,
    col = cores)




day_week <- factor(day$weekday, levels = 0:6,
                     labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab"))
frequencias_weekday <- as.data.frame(table(day_week))
colnames(frequencias_weekday) <- c("weekday", "freq")
ggplot(frequencias_weekday, aes(x = weekday, y = freq)) +
  geom_bar(stat = "identity", fill = "#D8BFAA", color = "white") +
  geom_text(aes(label = freq), vjust = -0.3, size = 4) +
  labs(x = "Dia da Semana", y = "Frequencia") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



workingday_freq <- table(day$workingday)
workingday_labels <- c("Nao útil", "Dia util")
workingday_pct <- round(100 * workingday_freq / sum(workingday_freq), 1)
labels <- paste0(workingday_labels, ": ", workingday_pct, "%")
cores_working <- c("#F4A6A6", "#FADBD8")  
pie(workingday_freq,
    labels = labels,
    col = cores_working,
    cex = 0.7)

table(day$weathersit)


#Analise Bivariada
shapiro.test(day$temp)
shapiro.test(day$atemp)
shapiro.test(day$hum)
shapiro.test(day$windspeed)
shapiro.test(day$cnt)
variaveis_quant <- day[, c("temp", "atemp", "hum", "windspeed", "cnt")]
mat_cor <- round(cor(vars, method = "spearman"), 2)
cor_data <- melt(mat_cor)
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 4.5) +
  scale_fill_gradient2(low = "#D4A373", mid = "white", high = "lightblue", midpoint = 0, limit = c(-1, 1)) +
  labs(title = "Matriz de Correlação com cnt", x = "", y = "") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggplot(day, aes(x = factor(season), y = cnt)) +
  geom_boxplot(fill = "#AED6F1", color = "black") +  
  x = "Estação do Ano", y = "Alugueres Diários"
) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


ggplot(day, aes(x = factor(workingday), y = cnt)) +
  stat_summary(fun = mean, geom = "bar", fill = "#F5B041") +  
  labs(
    title = "Média de Alugueres por Tipo de Dia",
    x = "Dia Útil (0 = não, 1 = sim)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(day, aes(x = factor(weathersit), y = cnt)) +
  geom_violin(fill = "#D2B48C", color = "black") +
  stat_summary(fun = median, geom = "crossbar",
               width = 0.5,        
               size = 3,          
               color = "black",
               fatten = 0) +
  labs(
    x = "Situação Climática (1 = céu limpo, 2 = nevoeiro, 3 = chuva)",
    y = "Alugueres Diários"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#Testes estatísticos
basededay <- subset(basededay, select = -c(instant, dteday, casual, registered))
str(basededay)
summary(base)
basededay$season     <- factor(basededay$season)
basededay$mnth       <- factor(basededay$mnth)
basededay$weekday    <- factor(basededay$weekday)
basededay$weathersit <- factor(basededay$weathersit)
basededay$yr         <- factor(basededay$yr)
basededay$holiday    <- factor(basededay$holiday)
basededay$workingday <- factor(basededay$workingday)
modelo_binom_neg <- glm.nb(cnt ~ atemp + yr + season + weathersit + mnth + windspeed + hum + holiday + temp + workingday,data=basededay)
modelo_temp_holiday <- glm.nb(cnt ~ basededay$temp + basededay$holiday,
                              data = basededay)
summary(modelo_temp_holiday)

kruskal.test(cnt ~ basededay$season, data = basededay)
kruskal.test(cnt ~ basededay$mnth, data = basededay)
kruskal.test(cnt ~ basededay$weekday, data = basededay)
kruskal.test(cnt ~ basededay$weathersit, data = basededay)
kruskal.test(cnt ~ basededay$yr, data = basededay)
kruskal.test(cnt ~ basededay$holiday, data = basededay)
kruskal.test(cnt ~ basededay$workingday, data = basededay)
#Análise do Modelo
day <- subset(day, select = -c(instant, dteday, casual, registered))
str(day)
summary(day)
# Transformar variáveis categóricas (sem adicionar labels)
day$season     <- factor(day$season)
day$mnth       <- factor(day$mnth)
day$weekday    <- factor(day$weekday)
day$weathersit <- factor(day$weathersit)
# Manter variáveis binárias como 0 e 1
day$yr         <- factor(day$yr)
day$holiday    <- factor(day$holiday)
day$workingday <- factor(day$workingday)
str(day)
summary(day)

attach(day)
mean(day$cnt)      # média da variável de contagem
var(day$cnt)       # variância da variável de contagem

#sobredispersão ELEVADA-> BINOMIAL NEGATIVA


#confirmar:
modelopoisson<-glm(cnt~., family=poisson, data = day)

library(performance)
check_overdispersion(modelopoisson) #confirma-se

#######
library(DHARMa)

# Simular os resíduos (sem plot)
sim <- simulateResiduals(fittedModel = modelopoisson, plot = FALSE)

# Obter os resíduos simulados
res <- recalculateResiduals(sim)

# Criar QQ plot manualmente (sem testes)
qqplot(x = sort(runif(length(res$scaledResiduals))), 
       y = sort(res$scaledResiduals),
       xlab = "Expected", ylab = "Observed",
       main = "QQ plot residuals", pch = 17)
abline(0, 1, col = "red")



#############


library(MASS)

# Modelo completo com todas as variáveis
modelo_completo <- glm.nb(cnt ~ ., data = day)



# Ajustar o modelo nulo 
modelo_nulo <- glm.nb(cnt ~ 1, data = day)

# Definir escopo do modelo completo com todas as variáveis de 'day'
completo <- formula(glm.nb(cnt ~ ., data = day))

# Aplicar seleção forward
modelo_forward <- step(modelo_nulo,
                       scope = completo,
                       direction = "forward")


# BACKWARD ELIMINATION
modelo_backward <- step(modelo_completo, direction = "backward")

# STEPWISE (both directions)
modelo_stepwise <- step(modelo_completo, direction = "both")

# Ver os resumos dos modelos finais
summary(modelo_forward)
summary(modelo_backward)
summary(modelo_stepwise)

#  Ver as fórmulas finais selecionadas
formula(modelo_forward)
formula(modelo_backward)
formula(modelo_stepwise)


# Obter AIC e BIC dos três modelos
aic_forward   <- AIC(modelo_forward)
bic_forward   <- BIC(modelo_forward)

aic_backward  <- AIC(modelo_backward)
bic_backward  <- BIC(modelo_backward)

aic_stepwise  <- AIC(modelo_stepwise)
bic_stepwise  <- BIC(modelo_stepwise)

# Criar data frame com os resultados
tabela_resultados <- data.frame(
  Modelo = c("Forward", "Backward", "Stepwise"),
  AIC    = c(aic_forward, aic_backward, aic_stepwise),
  BIC    = c(bic_forward, bic_backward, bic_stepwise)
)

# Visualizar a tabela
print(tabela_resultados)

#Modelo      AIC      BIC
#1  Forward 12155.03 12269.89 
#2 Backward 12155.23 12293.06
#3 Stepwise 12155.23 12293.06

formula(modelo_forward)
#cnt ~ atemp + yr + season + weathersit + mnth + windspeed + hum + holiday + temp + workingday





# Teste de razão de verossimilhança

library(lmtest)
lrtest(modelo_stepwise, modelo_completo)

lrtest(modelo_forward, modelo_completo) #-----> o teste valida a utilização deste modelo

lrtest(modelo_backward, modelo_completo)



##########
vif(modelo_forward)

formula(modelo_forward)

# Reajustar o modelo removendo 'atemp'
modelo_ajustado <- glm.nb(cnt ~ yr + season + weathersit + mnth + windspeed + hum + 
                            holiday + temp + workingday, data = day)

# Ver resumo do novo modelo
summary(modelo_ajustado)

# Calcular os novos VIFs
library(car)
vif(modelo_ajustado)

summary(modelo_ajustado)
AIC(modelo_ajustado)
BIC(modelo_ajustado)


formula(modelo_ajustado)

# Exponencial dos coeficientes
exp(coef(modelo_ajustado))

# Intervalos de confiança (95%) dos coeficientes
exp(confint(modelo_ajustado))



##############

set.seed(3333)  



# Dividir o dataset day em treino (80%) e teste (20%)

indices <- sample(seq_len(nrow(day)), size = 0.8 * nrow(day))
treino <- day[indices, ]
teste  <- day[-indices, ]

# -------------------------
# FORWARD SELECTION
# -------------------------

# Modelo nulo
modelo_nulo <- glm.nb(cnt ~ 1, data = treino)

# Modelo completo (fórmula apenas para escopo)
escopo <- formula(cnt ~ .)

# Aplicar forward
modelo_forward <- step(modelo_nulo,
                       scope = escopo,
                       direction = "forward",trace = 1)

# Previsões e RMSE
pred_forward <- predict(modelo_forward, newdata = teste, type = "response")
rmse_forward <- sqrt(mean((teste$cnt - pred_forward)^2))


# -------------------------
# BACKWARD SELECTION
# -------------------------

# Ajustar modelo backward com os day de treino
modelo_backward <- step(glm.nb(cnt ~ ., data = treino), direction = "backward", trace = 1)

# Previsões e RMSE para backward
pred_backward <- predict(modelo_backward, newdata = teste, type = "response")
rmse_backward <- sqrt(mean((teste$cnt - pred_backward)^2))


# -------------------------
# STEPWISE SELECTION
# -------------------------

# Ajustar modelo stepwise com os day de treino
modelo_stepwise <- step(glm.nb(cnt ~ ., data = treino), direction = "both", trace = 1)

# Previsões e RMSE para stepwise
pred_stepwise <- predict(modelo_stepwise, newdata = teste, type = "response")
rmse_stepwise <- sqrt(mean((teste$cnt - pred_stepwise)^2))


# -------------------------
# Tabela comparativa dos RMSEs
# -------------------------

tabela_rmse <- data.frame(
  Modelo = c("Forward", "Backward", "Stepwise"),
  RMSE   = c(rmse_forward, rmse_backward, rmse_stepwise)
)

print(tabela_rmse)

#Analise Diagnostico
summary(modelo_ajustado)


#O modelo ajusta-se aos day?

731-23#n-p-1
deviance(modelo_ajustado)#deviance está proximo do modelo ajustado, o que é um bom inidicador de qualidade de ajustamento do modelo

1-pchisq(deviance(modelo_ajustado), df.residual(modelo_ajustado)) #p_value = 0.15, nao rejeitamos H0, o modelo ajusta-se bem aos day => confirma o vusto anteriormente

#Comparar com modelo Nulo
anova(modelo_nulo, modelo_ajustado, test = "Chisq")

#Percentagem de Deviance Explicada
100 * (deviance(modelo_nulo)-deviance(modelo_ajustado))/deviance(modelo_nulo)
#1.84% da deviance explicada

#ETPG
ETPG <- sum(residuals(modelo_ajustado, type="pearson")^2)
1-pchisq(ETPG, 731-22) #p_value = 0.9999, o que indica um bom ajustameto dos day

plot(
  fitted(modelo_ajustado),
  residuals(modelo_ajustado, type = "pearson"),
  main = "Resíduos de Pearson vs Valores Ajustados",
  xlab = "Valores Ajustados",
  ylab = "Resíduos de Pearson",
  pch = 20,             # pontos preenchidos
  col = "blue"
)
abline(h = 0, col = "red", lty = 2)  # linha horizontal a 0
#O gráfico dos resíduos de Pearson em função dos valores ajustados revela um padrão descendente, sugerindo a presença de heterocedasticidade ou má especificação do modelo. Apesar de a maioria dos resíduos estar compreendida entre -2 e 2, o padrão sistemático indica que o modelo pode beneficiar da inclusão de termos não lineares ou transformações adicionais.
#

qqnorm(residuals(modelo_ajustado, type = "pearson"))
qqline(residuals(modelo_ajustado, type = "pearson"))

#O gráfico Q-Q mostra que os resíduos seguem razoavelmente uma distribuição normal na zona central, mas apresentam desvios nas caudas, indicando possíveis outliers ou desvios da normalidade. Este comportamento é esperado em modelos de contagem com variância não constante, como o binomial negativo. Ainda assim, a aproximação assintótica parece aceitável para efeitos de inferência.
