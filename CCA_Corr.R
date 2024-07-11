
################################################################################
####                     Análise de Correlacao Canonica (CCA)
################################################################################

# Carregar pacotes
library(corrplot) # Mapa de Calor (heatmap)
library(candisc)  # Correlacao
library(ggplot2)  # Graficos
library(ggrepel)  # Graficos
library(ellipse)  # Elipse de confianca
library(metan)    # Multicolinearidade
library(yacca)    # Pacote dependente
library(MVN)      # Normalidade dos dados


# Carregar dados
dados <- read.csv("borboletas.csv", header = T, row.names = 1)
head(dados)

# Definicao das variaveis 
env <- dados[,1:4]  #' Variaveis ambientais (vetor "x")
PFPGi0.40_0.60 <- dados[,5] + dados[,6]   # Criando uma nova variavel a partir da combinacao linear
gen <- cbind(PFPGi0.40_0.60, dados[7:9])  # Variaveis geneticas (vetor "y")


# Correlação por grupos
R <- cor(dados);R
corrplot(R, method = "ellipse") # Podemos usar outra forma de visualizacao

# Vamos criar um objeto para cada conjunto de variaveis
A <- cor(env)
B <- cor(gen)
C <- cor(env,gen)
corrplot(C, method="ellipse")


# Verificando a multicolinearidade - uso do pacote "Metan"
n <- nrow(R)
multicolinearidade <- colindiag(R, n = n)
multicolinearidade

n <- nrow(C)
multicolinearidade <- colindiag(C, n = n)
multicolinearidade


# Verificando a normalidade dos dados - uso do pacote "MVN"
mvn(dados, mvnTest = "hz",
    univariateTest = "SW",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj",
    showOutliers = TRUE, showNewData = TRUE) # Problema de colinearidade



dados2 <- data.frame(env,PFPGi0.40_0.60,gen)
mvn(dados[,-10], mvnTest = "hz",
    univariateTest = "SW",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj",
    showOutliers = TRUE, showNewData = TRUE)


# Padronização de dados
env.std <- scale(env)
gen.std <- scale(gen)


# Correlação canônica (usando o pacote "Candisc")
cc.borboletas <- cancor(env.std,gen.std, set.names=c("Environmental","Genetic"))
summary(cc.borboletas)
coef(cc.borboletas, type="both", standardize=TRUE)
plot(cc.borboletas, smooth=TRUE, xlim = c(-3,2), ylim = c(-3,2))


# O objeto "estrutura" contém as correlações entre as variáveis ambientais
# e as variáveis canônicas U1,..., U4, e entre as variáveis genéticas
# e as variáveis canônicas V1,..., V4. A interpretação dessas
# correlações sao discutidas na página 189 do livro texto (MANLY, NAVARRO, 2017)

cc.borboletas$structure
CCX1 <- cc.borboletas$scores$X[,1]
CCY1 <- cc.borboletas$scores$Y[,1]


# Construindo um gráfico (em ggplot2)
df <- data.frame(CCX1,CCY1)
label <- dados$env
df$label <- label
df


ellipse_data <- ellipse::ellipse(cor(df$CCX1, df$CCY1), scale = c(sd(df$CCX1), sd(df$CCY1)), centre = c(mean(df$CCX1), mean(df$CCY1)))# Calcular os parâmetros da elipse
ellipse_points <- data.frame(x = ellipse_data[, 1], y = ellipse_data[, 2]) # Extrair os pontos da elipse


ggplot(df, aes(x = CCX1, y = CCY1)) +
  geom_point(aes(color = label, fill = label)) +
  geom_text_repel(aes(label = rownames(df)), size = 3) +  # Adiciona os rótulos
  geom_smooth(aes(x = CCX1, y = CCY1), method = "lm", se = FALSE, color = "black", linetype = "dashed", inherit.aes = FALSE) +  # Adiciona uma única reta de regressão
  geom_polygon(data = ellipse_points, aes(x = x, y = y), color = "blue", fill = NA, size = 1.2) +  # Adiciona a elipse
  labs(x = "U1", y = "V1", color = "Grupo", fill = "Grupo") +
  # scale_color_discrete(name = "Grupo") +  # Garante que a legenda seja discreta
  # scale_fill_discrete(name = "Grupo") +   # Garante que a legenda seja discreta
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme(legend.position = "bottom")  # Ajuste a posição da legenda, se necessário


#' ## Uso do pacote "yacca" (Faz o teste de Barttlet automaticamente)
CC.yacca <- cca(env, gen, xscale=TRUE, yscale=TRUE)
summary(CC.yacca)


