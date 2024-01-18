# Analise Multivariada
# An�lise de Componentes Principais (PCA)
# Analisar distribuicao de plantas invasoras no cultivo de milho em Sistemas Integrados de Produ��o Agropecu�ria (SIPA). 

# -----------
### pacotes 
# -----------
#install.packages("pacman")
pacman::p_load(magrittr, tidyverse, FactoMineR, corrplot)

# -----------
### Dados 
# -----------
planilha <- read.csv('dadosPCAcomMilho.csv', sep=';', dec=',' , header = TRUE, stringsAsFactors=F)
tabela <- planilha %>% select(-Zema)
#tabela <- tabela %>% select(-Arab)

# -----------
### Padroniza��o dos dados com PCA 
# -----------

res.pca <- PCA(tabela, quali.sup = 1, scale.unit = TRUE, graph = FALSE)

summary(res.pca)
print(res.pca)
sweep(res.pca$var$coord, 2, sqrt(res.pca$eig[1:ncol(res.pca$var$coord),1]), FUN = '/')

# -----------
### Interpreta��o: obten��o dos autovalores/vari�ncias
# -----------

eig.val <- get_eigenvalue(res.pca)
eig.val

figura <- 
    fviz_eig(res.pca, addlabels = TRUE 
             , ylim = c(0, 35), title = "Contribui��o para a Vari�ncia", xlab="Dimens�es", ylab="Vari�ncia (%)")

png("Figura 1-contribuicao-variancia.png")
print(figura)
dev.off()

# -----------
### Gr�fico de correla��o das vari�veis
# -----------

res.var <- get_pca_var(res.pca)
res.var

fviz_pca_var(res.pca, col.var = "cos2", pointsize="cos2",  
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE )

# -----------
### Gr�fico de correla��o das vari�veis. Contribui��o individual. 
# -----------

fviz_pca_ind(res.pca, col.ind = "cos2"
             , pointsize="cos2"
             , gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             , repel = TRUE )

# -----------
### Qualidade da representa��o
# -----------

#figura <- 
    corrplot(res.var$cos2, is.corr=FALSE)

pdf("Figura 10-Qualidade de representacao.pdf")
print(figura)
dev.off()

# -----------
# Total cos2 of variables on Dim.1 and Dim.2
# -----------

fviz_cos2(res.pca, choice = "var", axes = 1:2)

# -----------
### Contribui��o das variaveis para o PCA
# -----------

#figura <- 
    corrplot(res.var$contrib, is.corr=FALSE)

png("Figura 11-Contribuicao das variaveis nos tratamentos.png")
print(figura)
dev.off()

# -----------
# Gr�fico PCA (biplot). Contribui��o individual por tratamento
# -----------

Tratamento = tabela$tratamento

#figura <- 
    fviz_pca_biplot(res.pca, col.var = "blue"
                    , repel = TRUE
                    , legend.title="Tratamentos"
                    ) +
    geom_point(aes(shape=Tratamento, color=Tratamento), size=5) + 
    scale_shape_manual(values=c(0,7,15, 1,13,19, 5,9,18, 2,14,17))+
    theme_classic() +
    labs(title="Analise de Componentes Principais")

png("Figura 2-analise multivariada-PCA.png")
print(figura)
dev.off()


# -----------
### Refer�ncias 
# -----------
# AN�LISE DE COMPONENTES PRINCIPAIS (PCA): C�LCULO E APLICA��O NO R. Dispon�vel em https://operdata.com.br/blog/analise-de-componentes-principais-pca-calculo-e-aplicacao-no-r/. Acesso em 13 out. 2021. 
#
# Principal Coordinates Analysis. 
    # dispon�vel em https://openplantpathology.github.io/OPP_Workshop_Multivariate/2-MV_PCO.html. Acesso em 13 out. 2021.
# 
# How to choose ordination method, such as PCA, CA, PCoA, and NMDS?
#     https://www.researchgate.net/post/How-to-choose-ordination-method-such-as-PCA-CA-PCoA-and-NMDS
