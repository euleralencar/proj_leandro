# Carregando os dados ####

setwd("C:\\Users\\t318769.TRIBUNAL\\Documents\\GitHub\\proj_leandro")
library(xlsx)
data <- read.xlsx("C:\\Users\\t318769.TRIBUNAL\\Documents\\GitHub\\proj_leandro\\reg_leandro.xlsx", 
                  sheetName="dados")
nomevar <- names(data)

head(data)
data <- data[-1]
data <- data[-9]
data <- data[-8]
data <- data[-7]
data <- data[-6]
head(data)

# coeficientes <- rep(NA,length(data)-1) ####
n <- length(data)  #número de variáveis no bando de dados
k <- n-1
r2 <- rep(NA,k)
beta <- rep(NA,k)
intercepto <- rep(NA,k)
pvalor_int <- rep(NA,k)
pvalor_beta <- rep(NA,k)
f <- rep(NA,k)
pvalor_f <- rep(NA,k)

# Regressão levando em conta o nível da variável dependente "txainc" ####

for (i in 1:k){
  fit <- lm(data[,i+1] ~ data[,1], data)
  tab <- summary(fit)
  
    r2[i] <- tab$coefficients[8]
    intercepto[i] <- tab$coefficients[1]
    beta[i]  <- tab$coefficients[2]
    pvalor_int[i]  <- tab$coefficients[7]
    pvalor_beta[i]  <- tab$coefficients[8]
    f[i]  <- tab$fstatistic[1]
    pvalor_f[i]  <- tab$coefficients[8]
}

sumario_reg <- round(cbind(r2, intercepto, beta, pvalor_int, pvalor_beta, f, pvalor_f),5)


#fit1 <- aov(rhdc ~ txainc, data)
#summary(fit1)

# Anova utilizando fatores ####

Fvalor <- rep(NA,k)
PrF <- rep(NA,k)
shapiro <- rep(NA,k)
par(mfrow = c(4,2))

#TabelasAnova <- list(a1="NA",a2="NA",a3="NA",a4="NA")
names(data)[2]

for (i in 1:k){
  datatwo <- data.frame(y=data[,i+1], group=factor(data$txainc))
  fitt <- lm(y ~ group, datatwo)
  tabb <- anova(fitt)
  fitted <- aov(y ~ group, datatwo)
  shapiro[i] <- shapiro.test(resid(fitted))$p.value
  plot(TukeyHSD(fitted,ordered=T))
  boxplot(y~group,data=datatwo, col="lightblue", main=names(data)[i+1])
  
  #TabelasAnova[i] <- tabb  
  Fvalor[i] <- tabb[1,4]
  PrF[i] <- tabb[1,5]
}

sumario_anova <- round(cbind(Fvalor, PrF, shapiro),5)

# Testes para Anova ####
names(data)
datatwo <- data.frame(y=data$iqd, group=factor(data$txainc))
fitt <- lm(y ~ group, datatwo)
tabb <- anova(fitt)
fitted <- aov(y ~ group, datatwo)

# The null-hypothesis of this test is that the population is normally distributed
shapiro <- shapiro.test(resid(fitted)) # Teste para verificar normalidade
#names(shapiro)

#leveneTest(fitt,data=tabela) # Teste de homogeneidade das variãveis
par(mfrow = c(4,2))
diferencas$group <- TukeyHSD(fitted) #Teste de Tukey para verificar quais pares mostram-se diferentes


jpeg('nome.jpg')
par(mfrow = c(2,1))
plot(TukeyHSD(fitted,ordered=T))
boxplot(y~group,data=datatwo, col="lightblue", main="Teste Anova")
dev.off()

# Hipotese:
# Qnt de bosta dá rebustez
# H- Altura
# NF - Número de Folhas
# DC - Diâmetro do Coleto

### How to work with plot
# http://gforge.se/2013/02/exporting-nice-plots-in-r/
