# Modelo Inicial: ####

dados <- read.csv("Social_Network_Ads.csv",h=T)
summary(dados)
#plot(dados[,-1])

n <- dim(dados)[1]
head(dados)
attach(dados)
library(tidyverse)
require(knitr)
a1<- dados
{
  dados=mutate(a1, feminino= Gender == "Female", masculino= Gender == "Male")
  dados$feminino[dados$feminino == "TRUE"] <- "1"
  dados$feminino[dados$feminino == "FALSE"] <- "0"
  
  dados$masculino[dados$masculino == "TRUE"] <- "1"
  dados$masculino[dados$masculino == "FALSE"] <- "0"
  
  d1<- dados[,-c(1,2)]
  attach(d1)
  head(d1)
}
fem<-(factor(feminino))
summary(fem)
204/400 #média

fit <- glm( Purchased ~ Age + feminino +  EstimatedSalary, family=binomial(link= "logit" ), data = d1)
summary(fit)
fit2 <- step(fit)
summary(fit2)

s<- summary(fit2)
#print(kable(s$coefficients))
print(kable(s$coefficients, format="latex"))
#AIC: 283.05 (inicial)


library(hnp) # pacote util para envelope simulado
# note que eh preciso definir o residuo para o envelope. As opcoes sao:
# "deviance", "pearson", "response", "working", "student", or "standard"
hnp(fit2, resid.type="deviance", xlab= "Quantis teóricos", ylab= "Resíduos" ,halfnormal = F)
#hnp(fit2, resid.type="pearson", halfnormal = F)
#hnp(fit2, resid.type="response", halfnormal = F)
#hnp(fit2, resid.type="working", halfnormal = F)
#hnp(fit2, resid.type="student", halfnormal = F)
#hnp(fit2, resid.type="standard", halfnormal = F)

library(fmsb) # para computo do R2 de Nagelkerke (1991)
NagelkerkeR2(fit2)
#0.6277741 (inicial)


cooks.distance(fit2)
limite<-4/(n-fit2$rank )
abline(plot(cooks.distance(fit2),ylab="Distancia de Cook",xlab="Indices"), 
       col="red", h=limite,lty=2)


hatvalues(fit2)
h_bar<-fit2$rank / n
limite<-2*h_bar
abline(plot(hatvalues(fit2),ylab="Alavancagem",xlab="Indices"), 
       col="red", h=limite,lty=2)


r<-residuals(fit2)

plot(r,type='p',pch="+",ylab="Resíduos",xlab="Indices") # plota os residuos do modelo
abline(h=c(-2,0,2),col="red",lty=2) # inclui linhas horizontais no grafico



# Modelo ajustado: ####
dados <- read.csv("Social_Network_Ads.csv",h=T)[-c(307,65,285,208,213,271,147,
                                                   219,202,217,239,32,98,138,255,
                                                   377,299,161),]
n <- dim(dados)[1]
head(dados)
attach(dados)
library(tidyverse)
a1<- dados
{ #Transformando a covariável genero em dummy, para masculino e feminino.
  dados=mutate(a1, feminino= Gender == "Female", masculino= Gender == "Male")
  dados$feminino[dados$feminino == "TRUE"] <- "1"
  dados$feminino[dados$feminino == "FALSE"] <- "0"
  
  dados$masculino[dados$masculino == "TRUE"] <- "1"
  dados$masculino[dados$masculino == "FALSE"] <- "0"
  
  d1<- dados[,-c(1,2)]
  attach(d1)
  head(d1)
}

fit <- glm( Purchased ~ Age + masculino +  EstimatedSalary, family=binomial(link= "logit" ), data = d1)
summary(fit)
fit2 <- step(fit)
summary(fit2)

s<- summary(fit2)
print(kable(s$coefficients))
print(kable(s$coefficients, format="latex"))
#AIC: 283.05 (inicial)
#AIC: 274.14
#AIC: 264.58
#AIC: 255.31
#AIC: 245.47
#AIC: 237.61
#AIC: 230.19
#AIC: 223.17
#AIC: 216.97
#AIC: 210.75
#AIC: 205.2
#AIC: 199.58 #Até aqui eram só muito sig
#AIC: 191.5 dupla retirada
#AIC: 181.32 dupla retirada
#AIC: 175.82 voltou a ser muito sig
#AIC: 170.25
#AIC: 165.14#Super sig


library(hnp) # pacote util para envelope simulado
# note que eh preciso definir o residuo para o envelope. As opcoes sao:
# "deviance", "pearson", "response", "working", "student", or "standard"
hnp(fit2, resid.type="deviance", xlab= "Quantis teóricos", ylab= "Resíduos" ,halfnormal = F)
#hnp(fit2, resid.type="pearson", halfnormal = F)
#hnp(fit2, resid.type="response", halfnormal = F)
#hnp(fit2, resid.type="working", halfnormal = F)
#hnp(fit2, resid.type="student", halfnormal = F)
#hnp(fit2, resid.type="standard", halfnormal = F)

library(fmsb) # para computo do R2 de Nagelkerke (1991)
NagelkerkeR2(fit2)
#0.6277741 (inicial)
#0.6434687
#0.660057
#0.6757889
#0.692227
#0.7049845
#0.7168247
#0.7267569
#0.7363409
#0.7458534
#0.7541872
#0.7625432
#0.7723896
#0.7862947
#0.8021647
#0.8085861

cooks.distance(fit2)
limite<-4/(n-fit2$rank )
abline(plot(cooks.distance(fit2),ylab="Distancia de Cook",xlab="Indices"), 
       col="red", h=limite,lty=2)
max(d[70:90])
#307***, 65***, 285***, 208***,  213***, 271***, 147***, 219***, 202***, 217***
#239***, (32 98)***, (138 255)***, 377 ***, 299***, 161***, 49 86

hatvalues(fit2)
h_bar<-fit2$rank / n
limite<-2*h_bar
abline(plot(hatvalues(fit2),ylab="Alavancagem",xlab="Indices"), 
       col="red", h=limite,lty=2)
#169ns ou muito pouco

r<-residuals(fit2)

plot(r,type='p',pch="+",ylab="Residuos",xlab="Indices") # plota os residuos do modelo
abline(h=c(-2,0,2),col="red",lty=2) # inclui linhas horizontais no grafico
min(r[285:300])
#217 239]

# Predição ####

Chance_compra <- function(idade, salario){
  g <- -27.49 + (0.54 * idade) + (0.0000759 * salario)  # g função de ligação logistica
  E <- exp(g)/ (1 + exp(g)) # E(Y)= exp(g(x))/ 1 + exp(g(x))
  options(digits = 6)
  cat("As chances de compra do individuo são de",E*100,"%")}

idade<- 20 # idade do cliente
salario<- 40000 #salário anual do cliente
Chance_compra(idade,salario)
