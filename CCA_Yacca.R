
################################################################################
####                     Análise de Correlacao Canonica (CCA)
####                     Utilizacao do pacote "yacca"
################################################################################


# Carregar pacotes
library(yacca)    # Pacote dependente

# Carregar dados
dados <- read.csv("borboletas.csv", header = T, row.names = 1)
head(dados)

#' ## Uso do pacote "yacca" (Faz o teste de Barttlet automaticamente)
CC.yacca <- cca(env, gen, xscale=TRUE, yscale=TRUE)
summary(CC.yacca)
plot(CC.yacca)
