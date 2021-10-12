# analise multivariada
# analisar distribuicao de plantas invasoras no cultivo de milho. 
# pacotes 
install.packages('Rtools') 
install.packages('magrittr') 
install.packages('tidyverse') 
library(magrittr)
library(tidyverse)
library(FactoMineR)
library(factoextra)

# dados 
setwd('dados')
ledados <- read.csv('dadosPCAcomMilho.csv', sep=';', dec=',' , header = TRUE, stringsAsFactors=F)
#ledados <- read.csv('dados/dadosPCAsemMilho.csv', sep=';', dec=',' , header = TRUE, stringsAsFactors=F)
#dados_select <- ledados %>% select(-Arab)
#dados_select <- ledados %>% select(-Zema)
dados_select <- ledados 

# Model FactoMiner 

#res_pca <- PCA(data.matrix(ledados) %>% select(-Arab), quali.sup = 1, graph = FALSE)
res_pca <- PCA(dados_select, quali.sup = 1, graph = FALSE)
summary(res_pca)
sweep(res_pca$var$coord, 2, sqrt(res_pca$eig[1:ncol(res_pca$var$coord),1]), FUN = '/')

###### usando baseR 
data.matrix(dados_select) %>% prcomp()
data.matrix(dados_select) %>% princomp()

# Grafico variacao 
fviz_contrib(res_pca, choice = "var", axes = c(1,2))
fviz_contrib(res_pca, choice = "var", addlabels=T, ylim=c(0,30))

# visualizacao 
fviz_pca_biplot(res_pca) 

fviz_pca_biplot(res_pca, col.var = "blue", geom.ind = "point", pointshape = 21, pointsize = 3 , fill.ind = dados_select$tratamento)+
    theme_test() +
    labs(title="Analise Multivariada", fill="Tratamentos")

# formato circular 
fviz_pca_var(res_pca, col.var = "blue")
fviz_pca_var(res_pca, col.var = "blue", geom.ind = "point", pointshape = 21, pointsize = 3 , fill.ind = dados_select$tratamento)+
    theme_test() +
    labs(title="Analise Multivariada", fill="Tratamentos",
    x="PCA1", y="PCA2")

