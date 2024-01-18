# -----------
### pacotes 
# -----------

install.packages("languageserver")

install.packages("languageserver", repos = c(
    reditorsupport = "https://reditorsupport.r-universe.dev",
    getOption("repos")
))

remotes::install_github("REditorSupport/languageserver")

??magrittr 

if(!require('pacman')) {
  install.packages('pacman')
  library('pacman')
}
pacman::p_load(tigerstats, lattice)

#--------------------
# 6 Analise de experimentos de um fator em DBC
#--------------------

##### 6.1 Entrada de dados 
getwd()
#setwd('dados')
setwd('C:/dev/github/sipa-pca-2024')
ledados <- read.csv('dados.csv', sep=';', dec=',' , header = TRUE, stringsAsFactors=F)
dbc <- ledados
dbc
str(dbc)
class(dbc)

# graficos #--------------------

?xyplot
xyplot(prod~tratamento, groups=bloco, data=dbc)
// --xyplot(tratamento, groups=bloco, data=dbc)
xyplot(prod~MSPA, groups=tratamento, data=dbc)

#--------------------
# 6.2 Analise de variancia
#--------------------
modelo0 <- aov(prod~tratamento, data=dbc)
class(modelo0)
anova(modelo0)
summary(modelo0)
summary.lm(modelo0)
#
###### checagem grafica

par(mfrow=c(2,2))
plot(modelo0)
layout(1)
#
#--------------------
# teste das pressuposicoes de normalidade de homocedasticidade
shapiro.test(residuals(modelo0))
bartlett.test(residuals(modelo0)~dbc$tratamento)
bartlett.test(residuals(modelo0)~dbc$bloco)
#
#--------------------
###### # 6.3 Teste de medias
#--------------------
# Calculando media e erro padrao por tratamento (A,B,C):
# O jeito mais facil de calcular esses valores eh utilizando a funcao tapply. Podemos tambem utilizar a funcao: function(x) sqrt(var(x)/length(x)) junto com tapply para calcular o erro padrao de cada grupo.

medias <- with(dbc, tapply(dbc$prod,tratamento,mean)) 

mediageral <- mean(dbc$prod)
mediageral 

erro <- with(dbc,tapply(prod,tratamento,function(x) sqrt(var(x)/length(x))))

#Perceba que as medias sao diferentes, como foi demonstrado pela ANOVA:
medias

###### Grafico de barras:
#--------------------
# Podemos utilizar os objetos medias e erro para plotar nosso grafico de barras com barras de erros representando o erro padrao de cada tratamento

# ?barplot
layout(1)
x <- barplot(medias, beside=T, ylim=c(0,12000),ylab="Produtividade (kg/ha)",xlab="Tratamentos")
arrows(x0=x,y0=medias-erro, x1=x,y1=medias+erro, angle=90,length=0.14,code=3)

###### teste de Tukey
# Comparacao entre as medias de todos os tratamentos. 
#--------------------

TukeyHSD(modelo0)

# teste de Scott-Knott
#--------------------
sk <- SK(x=dbc, y=dbc$prod, model="prod~tratamento", which="tratamento")
summary(sk)

#--------------------
# 6.4 Observacoes perdidas
#--------------------
# simulando observacoes perdidas aleatoriamente no conjunto dados

id <- sample(1:nrow(dbc), 3)
id
dbc$prod[id] <- NA
dbc

#--------------------
m1 <- aov(prod~tratamento, data=dbc)
summary(m1)

#--------------------
# o que acontece com os estimadores amostrais das medias?
dbc$ajus <- predict(m1, newdata=dbc)
with(dbc, tapply(prod, tratamento, mean, na.rm=TRUE))

#--------------------
# o que acontece com os estimadores de mínimos quadrados das medias?
with(dbc, tapply(ajus, tratamento, mean))

#--------------------
# como comparar medias? Tukey usando a media harmônica do número de repeticoes (danger)
dbc.cc <- dbc[complete.cases(dbc),]
with(dbc.cc,
    HSD.test(prod, tratamento,
    DFerror=df.residual(m1),
    MSerror=deviance(m1)/df.residual(m1)
    ))

#--------------------
# tratamentos diretos que precisam melhor descricao metodologica (usar com cuidado!)
TukeyHSD(m1)
layout(1)
plot(TukeyHSD(m1))
abline(v=0)
summary(glht(m1, linfct=mcp(tratamento="Tukey")))
#
#------------------------------------------------------------------------------------------
# contraste de medias populacionais marginais, montando os vetores de comparacoes
comp <- outer(levels(dbc$tratamento), levels(dbc$tratamento),
function(x, y) paste(x, y, sep="-"))
comp
comp <- comp[upper.tri(comp)]
comp <- do.call(rbind, strsplit(comp, "-"))
comp
#
#------------------------------------------------------------------------------------------
# montando a matriz de contrastes
cX <- sapply(1:nrow(comp),
function(i){
c.contr <- contrast(m1, type="average",
list(tratamento=comp[i,1], bloco=levels(dbc$bloco)),
list(tratamento=comp[i,2], bloco=levels(dbc$bloco)))
c.contr$X
})
cX
#
#------------------------------------------------------------------------------------------
# fornecendo a matriz para a glht para manutencao do erro tipo I
comP <- glht(m1, linfct=t(cX))
summary(comP)
#
#------------------------------------------------------------------------------------------
# as medias marginais populacionais
do.call(c, sapply(levels(dbc$tratamento),
function(i){
contrast(m1, type="average", list(tratamento=i, bloco=levels(dbc$bloco)))[1]
}))
#
#------------------------------------------------------------------------------------------

