library(ggplot2)
library(mvnormtest)
library(MASS)
library(heplots)

#MANIPULAÇÃO INICIAL DOS DADOS 


#tamanho
dim(diamonds)

#reduzir o dataset para 200 observações
set.seed(3030)
indices_amostra <- sample(1:53940, 200)
length(indices_amostra)
length(unique(indices_amostra))

#definir os novos dados amostrais
dados_amostra <- diamonds[indices_amostra,]

#descricao dos dados
dim(dados_amostra)
summary(dados_amostra)

#colocar em matriz
dados_amostra_mat <- cbind(dados_amostra$price, dados_amostra$depth,
                           dados_amostra$table, dados_amostra$price,
                           dados_amostra$x, dados_amostra$y, 
                           dados_amostra$z)


dados_amostra_num <- diamonds[indices_amostra, c("carat", "depth", "table", "price", "x", "y", "z")]

#ANALISE EXPLORATORIA DE DADOS

#Estatísticas discritivas
#carat-peso: peso do diamante
#cut-qualidade: qualidade do corte (fair,good,very good,premium,ideal)
#color-cor: cor do diamante, de D (best) a J (worst)
#clarity-claridade: medidade quao claro o diamante é, (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
#x-comprimento: comprimento em mm (0–10.74)
#y-largura: largura em mm (0–58.9)
#Z-profundidade: profundidade em mm (0–31.8)
#table-tabela: largura do topo do diamante relativo ao ponto mais largo (43–95)
#price-preco: preço em dolares ($326–$18,823)
#depht- percentagem total da profundidade

################
#Vetor das médias
dadosnovos<-dados_amostra[,-c(2,3,4)]
str(dadosnovos)
summary(dadosnovos)
media<-apply(dadosnovos, 2,mean); media
round(media,3)

#matriz variancia-covariancia
s<-cov(dadosnovos)
round(s,3)

#matriz de correlações
R<-cor(dadosnovos);R
round(R,3)

#dispersão
?plot
plot(dadosnovos, col="#FF6961")

#graficos de barras


#para cor:
# Criando uma tabela de frequências
freq <- table(dados_amostra$color)

# Criando o barplot e armazenando as posições das barras
bar_positions <- barplot(freq,
                         xlab = "Categorias",  
                         ylab = "Frequência",  
                         col = c("#0277BD", "#039BE5", "#29B6F6", "#4FC3F7", 
                                 "#81D4FA", "#B3E5FC", "#E0F7FA"), 
                         ylim = c(0, max(freq) * 1.2)) # Ajuste da escala para melhor visualização

# Adicionando os valores acima das barras
text(x = bar_positions, y = freq, labels = freq, pos = 3, cex = 1.2, col = "black")
# Adicionando a legenda
legend("topright",                              # Posição da legenda
       legend = c("D - Pior", "J - Melhor"), # Texto da legenda
       title = "Cor do diamante",                      # Título da legenda
       border = "white")      

#__________________________________


# Criando uma tabela de frequências
freq <- table(dados_amostra$cut)

# Gerando um degradê de cores pastel
num_bars <- length(freq) # Quantidade de categorias
pastel_colors <- colorRampPalette(c("#7FB3D5", "#A9CCE3", "#D6EAF8", "#F0F8FF", "#FFFFFF"))(num_bars)

# Criando o barplot e armazenando as posições das barras
bar_positions <- barplot(freq,  
                         xlab = "Categorias  da qualidade de corte",  
                         ylab = "Frequência",  
                         col = pastel_colors,  # Aplicando o degradê
                         ylim = c(0, max(freq) * 1.2)) # Ajuste da escala

# Adicionando os valores acima das barras
text(x = bar_positions, y = freq, labels = freq, pos = 3, cex = 1.2, col = "black")

#_____________________________
#clarity
# Criando uma tabela de frequências
freq <- table(dados_amostra$clarity)

# Gerando um degradê de azul pastel
num_bars <- length(freq)  # Quantidade de categorias
blue_pastel_colors <- colorRampPalette(c(
  "#0D1F3C", # Azul escuro
  "#1E3B61", # Azul mais claro
  "#2A4C7F", # Azul médio
  "#3A649D", # Azul com um pouco mais de saturação
  "#4A76B9", # Azul brilhante
  "#5A88D3", # Azul suave
  "#6A9AE7", # Azul bem claro
  "#7AB4FF"))(num_bars) # Azul pastel
# Criando o barplot e armazenando as posições das barras
bar_positions <- barplot(freq,  
                         xlab = "Categorias da claridade do diamante",  
                         ylab = "Frequência",  
                         col = blue_pastel_colors,  # Aplicando o degradê azul pastel
                         ylim = c(0, max(freq) * 1.2)) # Ajuste da escala

# Adicionando os valores acima das barras
text(x = bar_positions, y = freq, labels = freq, pos = 3, cex = 1.2, col = "black")
# Adicionando a legenda
legend("topright",                              # Posição da legenda
       legend = c("I1 - Pior", "IF - Melhor"), # Texto da legenda
       title = "Claridade",                      # Título da legenda
       border = "white")      


################

summary(dados_amostra) # fazer uma tabela das estatisticas descritivas
######
#Número de diamantes por faixa de peso e qualidade

library(ggplot2)

options(warnings = -1)

p3 <- ggplot(dados_amostra, aes(x = carat)) +
  geom_freqpoly(aes(color = factor(cut)), bins = 30, size = 0.75) +  # Linhas um pouco mais grossas para mais visibilidade
  theme_minimal() +
  scale_color_manual(values = c("#FF6961","#F7C5A0" ,"#8DA0CB", "#E78AC3", "#A6D854")) + 
  labs(title = "Número de diamantes por faixa de peso e qualidade",
       x = "Unidade de peso",
       y = "Frequências",
       color = "Qualidade do Corte")  # Nome da legenda

print(p3)




#--------------------------

#peso vs peso
#diamantes mais pesados são sempre mais caros?

ggplot(dados_amostra, aes(x = carat, y = price)) +
  geom_point(alpha = 0.3, color = "#0072B2") +
  geom_smooth(method = "lm", color = "#FF6961", se = FALSE) +
  theme_minimal() +
  labs(
    x = "Peso(quilates)",
    y = "Preço(USD)")

#preço vs corte
library(ggplot2)

# Definir cores personalizadas
cores_personalizadas <- c("#FF6961", "#F7C5A0", "#8DA0CB", "#E78AC3", "#A6D854")

ggplot(dados_amostra, aes(x = cut, y = price, fill = cut)) +
  geom_boxplot(outlier.shape = 1,  # Define outliers como círculos vazados
               outlier.size = 0.2,  # Define o tamanho das bolinhas menores
               outlier.stroke = 1) +  # Deixa as bolinhas mais finas
  scale_fill_manual(values = cores_personalizadas) +  # Aplica as cores personalizadas
  theme_minimal() +
  labs(title = "Distribuição do Preço por Qualidade do Corte",
       x = "Qualidade do Corte",
       y = "Preço (USD)")

#preço vs cor

cor_cord<-c("#0277BD", "#039BE5", "#29B6F6", "#4FC3F7", 
            "#81D4FA", "#B3E5FC", "#E0F7FA")
ggplot(dados_amostra, aes(x = color, y = price, fill = color)) +
  geom_boxplot() +
  scale_fill_manual(values =cor_cord)+
  theme_minimal() +
  labs(title = "Distribuição do Preço por Cor do Diamante",
       x = "Cor do Diamante",
       y = "Preço (USD)")

#preço vs claridade
ggplot(dados_amostra, aes(x = clarity, y = price, fill = clarity)) +
  geom_boxplot() +
  scale_fill_manual(values =blue_pastel_colors) +
  theme_minimal() +
  labs(title = "Distribuição do Preço por Claridade",
       x = "Claridade do Diamante",
       y = "Preço (USD)")


#-------------
ggplot(data = dados_amostra) + 
  geom_point(mapping = aes(x = carat, y = price, color = cut)) + 
  facet_wrap(~color) +
  scale_color_manual(values = c("#FF6961", "#F7C5A0", "#8DA0CB", "#E78AC3", "#A6D854")) +  # Aplicando as cores fornecidas
  labs(
    title = "Características que afetam o preço do diamante", 
    subtitle = "Peso, corte e cor",
    x = "Peso ",  # Traduzindo o eixo x
    y = "Preço (USD)",         # Traduzindo o eixo y
    color = "Corte"            # Traduzindo a legenda de cor
  ) +
  theme_minimal()

#---------------

ggplot(data = dados_amostra) + 
  geom_point(mapping = aes(x = carat, y = price, color = cut)) + 
  scale_color_manual(values = c("#FF6961", "#F7C5A0", "#8DA0CB", "#E78AC3", "#A6D854")) +  # Alterado para scale_color_manual
  facet_wrap(~clarity) +
  labs(title = "Características que afetam o preço do diamante", subtitle = "Peso, corte e claridade") +
  theme_minimal() +
  labs(
    x = "Peso",
    y = "Preço (USD)",
    color = "Corte")  # Atualiza a legenda de cores








###################
dados_num <- as.data.frame(dados_amostra_num)
mshapiro.test(t(dados_num)) #variaveis quantitativas

#realizar o teste M de Box
# 1.(cut)
box_cut <- boxM(dados_num, group = dados_amostra$cut)
print(box_cut)

# 2. color)
box_color <- boxM(dados_num, group = dados_amostra$color)
print(box_color)

# 3.(clarity)
box_clarity <- boxM(dados_num, group = dados_amostra$clarity)
print(box_clarity)


#MANOVA
#1. variável cut
modelo_cut <- manova(cbind(carat, depth, price, x, y, z) ~ cut, data = dados_amostra)
summary(modelo_cut, test = "Wilks")
S <- summary(modelo_cut)
E <- S$SS$Residuals
R <- S$SS
H <- R$cut
T <- H + E
gl_grupo <- S[["stats"]][1, 1]  # Graus de liberdade do fator (cut)
gl_residuo <- S[["stats"]][2, 1]  # Graus de liberdade dos resíduos
gl_total <- gl_grupo + gl_residuo 


#2. variável color
modelo_color <- manova(cbind(carat, depth, price, x, y, z) ~ color, data = dados_amostra)
summary(modelo_color, test = "Hotelling-Lawley")
S_color <- summary(modelo_color)
E_color <- S_color$SS$Residuals
R_color <- S_color$SS
H_color <- R_color$color
T_color <- H_color + E_color
gl_grupo_color <- S_color[["stats"]][1, 1] 
gl_residuo_color <- S_color[["stats"]][2, 1]  
gl_total_color <- gl_grupo_color + gl_residuo_color 


#3. variável clarity
modelo_clarity <- manova(cbind(carat, depth, price, x, y, z) ~ clarity, data = dados_amostra)
summary(modelo_clarity, test = "Roy")
S_clarity <- summary(modelo_clarity)
E_clarity <- S_clarity$SS$Residuals
R_clarity <- S_clarity$SS
names(R_clarity)
H_clarity <- R_clarity$clarity
T_clarity <- H_clarity + E_clarity
gl_grupo_clarity <- S_clarity[["stats"]][1, 1] 
gl_residuo_clarity <- S_clarity[["stats"]][2, 1] 
gl_total_clarity <- gl_grupo_clarity + gl_residuo_clarity 



#Análise Discriminante
#1. variável cut
modelo_cut1 <- lda(cut ~ carat + depth + table + price + x + y + z, data = dados_amostra)
modelo_cut1

scores <- predict(modelo_cut1)$x

# Criar o gráfico de dispersão para LD1 e LD2
par(mar = c(5, 4, 4, 10), xpd = TRUE)  

plot(scores[,1], scores[,2], 
     xlab = "LD1", ylab = "LD2",
     bg = c("lightblue", "lightgreen", "salmon", "lightyellow", "violet")[as.numeric(dados_amostra$cut)],
     pch = c(21, 22, 23, 24, 25)[as.numeric(dados_amostra$cut)],
     col = "black", 
     main = "Representação das Funções Discriminantes para Cut")

legend(x = par("usr")[2] + 0.5,  
       y = par("usr")[4] - 0.5,  
       legend = levels(dados_amostra$cut),
       pt.bg = c("lightblue", "lightgreen", "salmon", "lightyellow", "violet"), 
       pch = c(21, 22, 23, 24, 25), 
       col = "black", 
       bty = "n", cex = 1.2)

previsao_cut <- predict(modelo_cut1)
classe_prevista <- previsao_cut$class
table(classe_prevista)
tabela_cut <- table(Classe_Real = dados_amostra$cut, Classe_Prevista = classe_prevista)
accuracy_cut_total <- sum(diag(tabela_cut)) / sum(tabela_cut)
accuracy_cut_classe <- diag(tabela_cut) / rowSums(tabela_cut)

#2. variável color
modelo_color1 <- lda(color ~ carat + depth + table + price + x + y + z, data = dados_amostra)
modelo_color1

scores <- predict(modelo_color1)$x

# Criar o gráfico de dispersão para LD1 e LD2
par(mar = c(5, 4, 4, 9), xpd = TRUE)

plot(scores[,1], scores[,2], 
     xlab = "LD1", ylab = "LD2",
     bg = c("lightblue", "lightgreen", "salmon", "lightyellow", "violet", "orange", "brown")[as.numeric(dados_amostra$color)],
     pch = c(21, 22, 23, 24, 25)[as.numeric(dados_amostra$color)],  
     col = "black", 
     main = "Representação das Funções Discriminantes para Color")

legend(x = par("usr")[2] + 0.1,  
       y = par("usr")[4] - 0.1,  
       legend = levels(dados_amostra$color),
       pt.bg = c("lightblue", "lightgreen", "salmon", "lightyellow", "violet", "orange", "brown"), 
       pch = c(21, 22, 23, 24, 25, 21, 22),  
       col = "black", 
       bty = "n", cex = 1)  

previsao_color <- predict(modelo_color1)
classe_prevista_color <- previsao_color$class
table(classe_prevista_color)
tabela_color <- table(Classe_Real = dados_amostra$color, Classe_Prevista = classe_prevista_color)
accuracy_color_total <- sum(diag(tabela_color)) / sum(tabela_color)
accuracy_color_classe <- diag(tabela_color) / rowSums(tabela_color)


#3. variável clarity
modelo_clarity1 <- lda(clarity ~ carat + depth + table + price + x + y + z, data = dados_amostra)
modelo_clarity1

scores <- predict(modelo_clarity1)$x

# Criar o gráfico de dispersão para LD1 e LD2
par(mar = c(5, 4, 4, 9), xpd = TRUE)

plot(scores[,1], scores[,2], 
     xlab = "LD1", ylab = "LD2",
     bg = c("lightblue", "lightgreen", "salmon", "lightyellow", "violet", "orange", "brown", "gray")[as.numeric(dados_amostra$clarity)],
     pch = c(21, 22, 23, 24, 25, 21, 22, 23)[as.numeric(dados_amostra$clarity)],
     col = "black", 
     main = "Representação das Funções Discriminantes para Clarity")

legend(x = par("usr")[2] + 0.1,  
       y = par("usr")[4] - 0.1, 
       legend = levels(dados_amostra$clarity),
       pt.bg = c("lightblue", "lightgreen", "salmon", "lightyellow", "violet", "orange", "brown", "gray"), 
       pch = c(21, 22, 23, 24, 25, 21, 22, 23), 
       col = "black", 
       bty = "n", cex = 1)

previsao_clarity <- predict(modelo_clarity1)
classe_prevista_clarity<- previsao_clarity$class
table(classe_prevista_clarity)
tabela_clarity <- table(Classe_Real = dados_amostra$clarity, Classe_Prevista = classe_prevista_clarity)
accuracy_clarity_total <- sum(diag(tabela_clarity)) / sum(tabela_clarity)
accuracy_clarity_classe <- diag(tabela_clarity) / rowSums(tabela_clarity)

#ACP
R<-cor(dadosnovos);R
library(psych)
str(dadosnovos)
KMO(R) #estatística de KMO
cortest.bartlett(R,200)



#Calcular os coeficientes da componentes principais: vetores pr´oprios da matriz de covariância

# ACP com base na matriz de correlações
pca <- prcomp(dadosnovos, scale. = TRUE)

# Calcular os valores próprios (autovalores)
eigenvalues <- pca$sdev^2

# Proporção da variância explicada
prop_var <- eigenvalues / sum(eigenvalues)

# Proporção acumulada
cum_var <- cumsum(prop_var)

# Montar a tabela
tabela_pca <- data.frame(
  `Componentes Principais` = paste0("CP", 1:length(eigenvalues)),
  `Valores Próprios` = round(eigenvalues, 3),
  `Proporção de Variância (%)` = round(prop_var * 100, 3),
  `Proporção Acumulada de Variância (%)` = round(cum_var * 100, 3)
)

# Visualizar a tabela
print(tabela_pca)


library(ggplot2)

df <- data.frame(
  CP = factor(1:length(eigenvalues)),
  Eigenvalue = eigenvalues
)

ggplot(df, aes(x = CP, y = Eigenvalue, group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Gráfico do Cotovelo (Scree Plot)",
       x = "Componentes Principais",
       y = "Valores Próprios") +
  theme_minimal()


###pesos e comunalidades



# Número de componentes principais a manter
num_cp <- 2

# 1. Pesos (loadings)
loadings <- pca$rotation[, 1:num_cp]

# 2. Comunalidades (soma dos quadrados dos loadings por variável)
comunalidades <- rowSums(loadings^2)

# 3. Montar a tabela
tabela_final <- as.data.frame(round(loadings, 3))
tabela_final$Inicial <- 1
tabela_final$Comunalidade <- round(comunalidades, 3)
tabela_final$Variável <- rownames(tabela_final)

# 4. Reorganizar colunas
tabela_final <- tabela_final[, c("Variável", paste0("PC", 1:num_cp), "Inicial", "Comunalidade")]

# 5. Ver resultado
print(tabela_final)

# 6. (Opcional) Exportar como LaTeX
library(xtable)
xtable(tabela_final, caption = "Matriz dos Pesos e Comunalidades das Variáveis", label = "tab:pesos")

# Visualizar
print(tabela_final)

# Exportar como LaTeX (opcional)
# install.packages("xtable") se ainda não tiveres
library(xtable)
xtable(tabela_final, caption = "Matriz dos Pesos e Comunalidades", label = "tab:loadings")


######gráficos das cp's e scores#######


# 1. Matriz de loadings (pesos)
loadings <- pca$rotation[, 1:2]

# 2. Identificar a CP dominante por variável (maior valor absoluto)
dominante <- apply(abs(loadings), 1, which.max)

# 3. Atribuir cores consoante a CP dominante
cores <- ifelse(dominante == 1, "forestgreen",  "#0072B2")

# 4. Gráfico de loadings com eixos customizados
plot(loadings[, 1], loadings[, 2],
     xlab = paste0("CP1 (", round(summary(pca)$importance[2,1]*100, 2), "%)"),
     ylab = paste0("CP2 (", round(summary(pca)$importance[2,2]*100, 2), "%)"),
     main = "Gráfico dos Loadings - CP1 vs CP2",
     pch = 19, col = cores, xlim = c(-1, 1), ylim = c(-1, 1),
     axes = FALSE)  # desativa os eixos automáticos

# Linhas de referência
abline(h = 0, lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")

# Eixos personalizados de 0.20 em 0.20
axis(1, at = seq(-1, 1, by = 0.5))  # eixo x
axis(2, at = seq(-1, 1, by = 0.5))  # eixo y
box()  # borda do gráfico

# 5. Adicionar os nomes das variáveis
text(loadings[, 1], loadings[, 2], labels = rownames(loadings),
     pos = 3, cex = 0.8, col = cores)

# 6. Tabela de scores (valores das observações nas CPs)
scores <- pca$x[, 1:2]
library(xtable)
xtable(round(scores, 3))



######biplots#####

library(factoextra)

# Percentagem explicada pelas componentes
eig_vals <- summary(pca)$importance[2, 1:2] * 100


?fviz_pca_biplot
# Biplot personalizado
fviz_pca_biplot(pca,
                axes = c(1, 2),
                repel = TRUE,
                col.ind = "gray40",       # cor das observações
                col.var = "contrib",      # cor das setas por contribuição
                label = "var",
                title = "Biplot - ACP (CP1 vs CP2)") +
  xlab(paste0("PC1 (", round(eig_vals[1], 1), "%)")) +
  ylab(paste0("PC2 (", round(eig_vals[2], 1), "%)")) +
  labs(color = "Contribuição") +     
  theme_minimal()


######Rotação Ortogonal das Componentes Principais (CP)#####

# Extrair loadings (pesos das variáveis nas CPs)
loadings <- pca$rotation[, 1:2]

# Aplicar rotação Varimax
varimax_rot <- varimax(loadings)

# Matriz dos pesos após rotação
loadings_rot <- varimax_rot$loadings
print(loadings_rot)

tabela_pesos <- data.frame(
  Variável = rownames(loadings),
  CP1 = loadings[, 1],
  CP2 = loadings[, 2],
  CP1_rot = loadings_rot[, 1],
  CP2_rot = loadings_rot[, 2]
)

print(tabela_pesos)




#loadings rodados
# 1. Identificar a CP dominante para cada variável (após rotação)
dominante <- apply(abs(loadings_rot), 1, which.max)

# 2. Atribuir cores diferentes conforme a CP dominante
cores <- ifelse(dominante == 1, "forestgreen", "#0072B2")  # verde para CP1, azul para CP2

# 3. Criar o gráfico dos loadings rotacionados com cores
plot(loadings_rot[,1], loadings_rot[,2],
     xlab = "CP1 (rotacionada)", ylab = "CP2 (rotacionada)",
     main = "Gráfico dos Loadings após Rotação Varimax",
     pch = 19, col = cores, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)

# 4. Linhas de referência
abline(h = 0, v = 0, col = "gray60", lty = 2)

# 5. Adicionar os nomes das variáveis
text(loadings_rot[,1], loadings_rot[,2],
     labels = rownames(loadings_rot), col = cores, pos = 3, cex = 0.9)


#biplot


library(ggplot2)
library(ggrepel)

# 1. Obter matriz de rotação
matriz_rot <- varimax_rot$rotmat

# 2. Rotacionar os scores das observações
scores_rot <- as.data.frame(as.matrix(pca$x[, 1:2]) %*% matriz_rot)
colnames(scores_rot) <- c("CP1_rot", "CP2_rot")

# 3. Loadings rotacionados
loadings_rot <- as.data.frame(varimax_rot$loadings[, 1:2])
colnames(loadings_rot) <- c("CP1_rot", "CP2_rot")
loadings_rot$var <- rownames(loadings_rot)

# 4. Cor por componente dominante
loadings_rot$dom_cp <- apply(abs(loadings_rot[, 1:2]), 1, which.max)
loadings_rot$cor <- ifelse(loadings_rot$dom_cp == 1, "#66C2A5", "#8DA0CB")  # verde e azul suaves

# 5. Percentagens de variância explicada (mesmas que pré-rotação)
eig_vals <- summary(pca)$importance[2, 1:2] * 100
escala <- 5  # para setas mais visíveis

# 6. Biplot final
ggplot() +
  geom_point(data = scores_rot, aes(x = CP1_rot, y = CP2_rot), color = "gray60", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  
  geom_segment(data = loadings_rot,
               aes(x = 0, y = 0,
                   xend = CP1_rot * escala,
                   yend = CP2_rot * escala,
                   color = cor),
               arrow = arrow(length = unit(0.25, "cm")),
               linewidth = 1.2) +
  
  geom_text_repel(data = loadings_rot,
                  aes(x = CP1_rot * escala, y = CP2_rot * escala, label = var),
                  color = "black", size = 5, box.padding = 0.4) +
  
  xlab(paste0("CP1 rotacionada (", round(eig_vals[1], 1), "%)")) +
  ylab(paste0("CP2 rotacionada (", round(eig_vals[2], 1), "%)")) +
  labs(title = "Biplot com Loadings e Scores Rotacionados (Varimax)") +
  
  scale_color_identity() +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")





#Análise de Clusters

library(factoextra) # clustering visualization
library(cluster)    # clustering algorithms
library(ggcorrplot)
library(dplyr)

str(dados_amostra)
#definir matriz com variaveis quantitativas
diamonds_quant <- dados_amostra[,-c(2,3,4)]

cor(diamonds_quant)

#Cluster de Variaveis

clust_var = hclust(as.dist(1 - cor(as.matrix(diamonds_quant))),method = "ward.D2")

plot(hclust(as.dist(1 - cor(as.matrix(diamonds_quant))),method = "ward.D2"),cex = 0.6, hang = -1, ylab = "Altura", xlab = "Variáveis", main = "Dendograma das Variáveis do Dataset")  # Hierarchical clustering
?plot.hclust
rect.hclust(hclust(as.dist(1 - cor(as.matrix(diamonds_quant))),method = "ward.D2"),k=2,border=1:2)


fviz_dend(clust_var,k = 2,
          color_labels_by_k =TRUE, rect = TRUE,k_colors = c("#1B9E77", "#D95F02"),ylab = "Altura", xlab = "Variáveis", main = "Dendograma das Variáveis do Dataset")
?fviz_dend

#Clustering Hierarquico

#Aglomerativo

# Fazer o scale das variaveis numericas
?scale
scaled_data = scale(diamonds_quant, center = TRUE, scale = TRUE)
head(scaled_data,10)

# -------------------- Distancia Euclidiana -----------------------------------#
dist_euclidean=dist(scaled_data,method = "euclidean")
fviz_dist(dist_euclidean,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Method Complete
hc1_euclidean=hclust(dist_euclidean,method = "complete")
plot(hc1_euclidean, cex = 0.6, hang = -1,main = "Complete Linkage")


# Method Single X
hc2_euclidean=hclust(dist_euclidean,method = "single")
plot(hc2_euclidean, cex = 0.6, hang = -1)

# Method average X
hc3_euclidean=hclust(dist_euclidean,method = "average")
plot(hc3_euclidean, cex = 0.6, hang = -1)

windows()
par(mfrow=c(2,2))

# Method Ward.D (COOLEST)
hc4_euclidean=hclust(dist_euclidean,method = "ward.D")
plot(hc4_euclidean, cex = 0.6, hang = -1,main="Ward.D Method")

# Method Ward.D2 X
hc5_euclidean=hclust(dist_euclidean,method = "ward.D2")
plot(hc5_euclidean, cex = 0.6, hang = -1,main="Ward.D2 Method")


rect.hclust(hc4_euclidean, k = 3, border = 2:5)




# ------------------- Distancia de Manhattan ----------------------------------#

dist_man=dist(scaled_data,method = "manhattan")
fviz_dist(dist_man,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Method Complete !!!!
hc1_man=hclust(dist_man,method = "complete")
plot(hc1_man, cex = 0.6, hang = -1,main = "Complete Linkage")


# Method Single X
hc2_man=hclust(dist_man,method = "single")
plot(hc2_man, cex = 0.6, hang = -1)

# Method average X
hc3_man=hclust(dist_man,method = "average")
plot(hc3_man, cex = 0.6, hang = -1)

# Method Ward.D X
hc4_man=hclust(dist_man,method = "ward.D")
plot(hc4_man, cex = 0.6, hang = -1,main="Ward.D Method")

# Method Ward.D2 X
hc5_man=hclust(dist_man,method = "ward.D2")
plot(hc5_man, cex = 0.6, hang = -1,main="Ward.D2 Method")

rect.hclust(hc4_euclidean,k=4,border = 2:5)
sub_grp_man=cutree(hc4_euclidean,k=4)
table(sub_grp_man)

table(sub_grp_man,dados_amostra$cut)


# Cluster plot com cores: vermelho, azul, verde
fviz_cluster(list(data = scaled_data, cluster = sub_grp_man), 
             repel = TRUE, 
             labelsize = 0) +
  scale_color_manual(values = c("red", "blue", "green3","magenta")) +
  scale_fill_manual(values = c("red", "blue", "green3","magenta")) +
  theme_minimal()

# Dendrograma com as MESMAS cores e na mesma ordem
fviz_dend(hc4_euclidean, 
          cex = 0.5, 
          lwd = 0.8, 
          k = 2,
          k_colors = c("red", "blue", "green3","magenta"),  # mesma ordem
          rect = TRUE, 
          rect_border = "gray", 
          rect_fill = TRUE)


#criar tabelas sumario dos clusters - metodo aglomerativo
library(compareGroups)
data_Cluster=dados_amostra %>% mutate(Cluster=sub_grp_man)
table_comp=compareGroups(Cluster~.,data = data_Cluster)
createTable(table_comp,show.p.overall = FALSE)
export2latex(createTable(table_comp,show.p.overall = FALSE))

#------------------------ Distancia de Minskosvy-------------------------------#

dist_min=dist(scaled_data,method = "minkowski")
fviz_dist(dist_min,gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


# Method Complete X
hc1_min=hclust(dist_min,method = "complete")
plot(hc1_min, cex = 0.6, hang = -1,main = "Complete Linkage")


# Method Single X
hc2_min=hclust(dist_min,method = "single")
plot(hc2_min, cex = 0.6, hang = -1)

# Method average X
hc3_min=hclust(dist_min,method = "average")
plot(hc3_min, cex = 0.6, hang = -1)

# Method Ward.D
hc4_min=hclust(dist_min,method = "ward.D")
plot(hc4_min, cex = 0.6, hang = -1,main="Ward.D Method")

# Method Ward.D2 X
hc5_min=hclust(dist_min,method = "ward.D2")
plot(hc5_min, cex = 0.6, hang = -1,main="Ward.D2 Method")

sub_grp_min=cutree(hc4_min,k=3)
length(sub_grp_min)
rect.hclust(hc4_min,k=3,border = 2:5)


fviz_cluster(list(data=scaled_data,cluster=sub_grp_min))

table(sub_grp_min,dados_amostra$cut)
table(sub_grp_min,dados_amostra$color)
table(sub_grp_min,dados_amostra$clarity)

# Numero optimo de clusters
?fviz_nbclust()

# Elbow Method
fviz_nbclust(scaled_data, FUN = hcut, method = "wss") #hcut usa euclidiana (3/4 clusters)

# Silhouette Method
fviz_nbclust(scaled_data, FUN = hcut, method = "silhouette") #(2/3/4 clusters)

# Gap Statistic
gap_stat <- clusGap(scaled_data, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# 4 Clusters
sub_grp=cutree(hc4_euclidean,k=4)
table(sub_grp)

#k_means

fviz_nbclust(scaled_data, kmeans, method = "wss")
fviz_nbclust(scaled_data, kmeans, method = "silhouette")
gap_stat <- clusGap(scaled_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


#clusters
?kmeans
K2=kmeans(scaled_data,centers = 2,nstart = 25)
K3=kmeans(scaled_data,centers = 3,nstart = 25)
K4=kmeans(scaled_data,centers = 4,nstart = 25)
K5=kmeans(scaled_data,centers = 5,nstart = 25)
K6=kmeans(scaled_data,centers = 6,nstart = 25)

#Plots dos clusters
?fviz_cluster()
p1 <- fviz_cluster(K2, data = scaled_data, labelsize = 0) + ggtitle("k = 2")+theme_minimal()
p2 <- fviz_cluster(K3,  data = scaled_data, labelsize = 0) + ggtitle("k = 3")+theme_minimal()
p3 <- fviz_cluster(K4,  data = scaled_data, labelsize = 0) + ggtitle("k = 4")+theme_minimal()
p4 <- fviz_cluster(K5,  data = scaled_data, labelsize = 0) + ggtitle("k = 5")+theme_minimal()
p5 <- fviz_cluster(K6,  data = scaled_data, labelsize = 0) + ggtitle("k = 6")+theme_minimal()

# Plots para vários números de Clusters
library(gridExtra)
windows()
grid.arrange(p1, p2, p3, nrow = 3)
grid.arrange(p1)
grid.arrange(p4)
grid.arrange(p5)

#tabelas de sumário para os clusters - kmeans
library(compareGroups)

data_Cluster=dados_amostra %>% mutate(Cluster=K4$cluster)
table_grp=compareGroups(Cluster~.,data = data_Cluster)
createTable(table_grp,show.p.overall = FALSE)
export2latex(createTable(table_grp,show.p.overall = FALSE))




