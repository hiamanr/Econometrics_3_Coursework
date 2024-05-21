# Trabalho - Metria 3 ----------------------------------------------------------
# Alunos: Hiaman Santos e JFCP2000----------------------------------------------

# Instalando pacotes------------------------------------------------------------
# Tidyverse
#install.packages("tidyverse")
library(tidyverse)

#Instalando pacotes para decompor e dessazonalizar
#install.packages("seasonal")
library(seasonal) #Arima X13-Seats

#install.packages("mFilter")
library(mFilter) #Filtro HP

#Carregando dados disponíveis no pacote URCA
#install.packages("urca")
library("urca")

#install.packages("forecast")
library(forecast)

#install.packages("sandwich")
library(sandwich) #Erros padrão HAC

#install.packages("lmtest")
library(lmtest) #Testes com erros padrão robustos

#install.packages("CADFtest")
library("CADFtest") # testes de raiz unitária

#install.packages("sandwich")
library("sandwich") #Erros padrão HAC

#install.packages("car")
library("car") #Teste de restrições lineares

# Ex.2.-------------------------------------------------------------------------

# Importing dataset with relevant data from OECD and Destatis.
# Building variable vacancy rate as the ratio between job vacancies and EAP

library(readxl)
data <- read_excel("data.xlsx", col_types = c("text", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric"))
View(data)


# Creating new column 
# Vacancy rate

data$vacancy_rate <- 100*data$Vacancies_mill / data$EAP_mill

Vacancy_rate <- ts(data$vacancy_rate, start = c(1991, 1), frequency = 12)
plot(Vacancy_rate)

# Conducting uni root tests:

#Vamos conduzir os testes a 10% de significância

#Começamos pelo modelo mais completo:
teste_1 = CADFtest(Vacancy_rate, type = "trend", 
                   max.lag.y = ceiling(12*(length(Vacancy_rate)/100)^(1/4)), #definição do lag ótimo para teste de Dickey-Fuller
                   criterion = "MAIC") #avaliação da especificação do modelo
summary(teste_1)
print(teste_1)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% (p-valor é de 0.3143 > 0.10)

# Caímos no item 1.2. do slide 16: 
# Se não rejeitamos a hipótese nula, fazemos o teste F da nula (β, γ) = (0, 0),
#usando os valores críticos do slide 11 (Caso 4).

# Teste F-----------------------------------------------------------------------

#Objetivo: detectar se o modelo de fato apresenta tendência linear

print(teste_1$est.model)
linearHypothesis(teste_1$est.model,c("trnd = 0", "L(y, 1) = 0" ))

# A estatística F é de 3.2294. 
# Usando o Caso 4 da tabela na página 11 dos slides, 
#vemos que, a 10% de significância,o valor crítico para 408 observações é 5.36. 
# Como F < 5.47, NÃO rejeitamos a hipótese nula de que 
#a tendência e o coeficiente associado a y_{t-1} são ambos zero.

# Conclusão: modelo não apresenta tendência linear.
# Caímos no caso 1.2.1, do slide 16.
# Nesse caso, vamos para o modelo somente com intercepto.

# Procedimento Sequencial II, slide 17------------------------------------------
# Modelo apenas com intercepto--------------------------------------------------

# Se chegamos a esta etapa, não temos evidências de que haja uma
#tendência linear no modelo. 

teste_2 = CADFtest(Vacancy_rate, type = "drift", max.lag.y = ceiling(12*(length(Vacancy_rate)/100)^(1/4)), 
                   criterion = "MAIC")
print(teste_2)
summary(teste_2)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% 
#(p-valor é de 0.7353 > 0.10)

# Caímos no caso 2.2., do slide 17:
# Se não rejeitamos a hipótese nula, fazemos o teste F da nula (α, γ) = (0, 0),
#usando os valores críticos do slide 11 (Caso 2).

# Teste F-----------------------------------------------------------------------
#Objetivo: detectar se o modelo de fato apresenta intercepto.

print(teste_2$est.model)
linearHypothesis(teste_2$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

# A estatística F é de 0.9921
# Usando o Caso 2 da tabela da página 11 dos slides,
# a 10% de significância, para 408 observações, o valor crítico é 3.79.
# Como F < 3.86, NÃO REJEITAMOS a hipótese nula
# de que o intercepto e o coeficiente associado a y_{t-1} são ambos zero 
#(alfa e gama, respectivamente).

# Caímos no caso 2.2.1, do slide 17:
# Se não rejeitamos a hipótese nula do teste F, concluímos que o modelo não
#apresenta intercepto. Nesse caso, vamos à etapa 3.


# Procedimento Sequencial III---------------------------------------------------
# Modelo sem componentes determinísticos

# Se chegamos a essa etapa, não temos evidências de que haja uma
#tendência linear no modelo nem um intercepto.


# Testamos a nula de H0 : γ = 0 contra a alternativa de que H1 : γ < 0 
#usando a estatística t chapéu e os valores críticos não normais para esse caso 
#(slide 10, caso 1). 

teste_3 = CADFtest(Vacancy_rate, type = "none", 
                   max.lag.y = ceiling(12*(length(Vacancy_rate)/100)^(1/4)), 
                   criterion = "MAIC")
print(teste_3)
summary(teste_3)

# O teste t NÃO rejeita a hipótese nula de H0 : γ = 0, 
#a 10% (p-valor é de 0.8295 > 0.10).

# Caímos no caso 3.2, do slide 18: 
# Se não rejeitamos a nula, concluímos que a série apresenta UMA raiz unitária.

## Componentes determinísticos--------------------------------------------------

# Vamos testar a presença de componentes determinísticos.
# Como os dados apresentam tendência estocástica, 
#trabalhamos com a série diferenciada

trend = 1:(length(Vacancy_rate)-1)
modelo = lm(diff(Vacancy_rate)~trend)

coeftest(modelo, vcov. = vcovHAC)

# O teste t NÃO rejeita a hipótese nula de de que 
#NÃO há tendência linear determinística
#na série em primeira diferença
#a 10% (p-valor é de 0.7953 > 0.10). 





# Ex. 3-------------------------------------------------------------------------

# Building Saez efficiency measure

#Nível de desemprego eficiente: u = raiz(u*v)


u <- mutate(data, u_star = sqrt((data$vacancy_rate*data$Unemployment_rate)))

u_ts <- ts(u$u_star, start = c(1991,1), frequency = 12)

plot(u_ts)


# Repetindo testes para janela até a reforma

janela_testes <- window(u_ts, end = c(2002,12))


#Começamos pelo modelo mais completo:
teste_4 = CADFtest(janela_testes, type = "trend", 
                   max.lag.y = ceiling(12*(length(janela_testes)/100)^(1/4)), #definição do lag ótimo para teste de Dickey-Fuller
                   criterion = "MAIC") #avaliação da especificação do modelo
summary(teste_4)
print(teste_4)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% (p-valor é de 0.9962 > 0.10)

# Caímos no item 1.2. do slide 16: 
# Se não rejeitamos a hipótese nula, fazemos o teste F da nula (β, γ) = (0, 0),
#usando os valores críticos do slide 11 (Caso 4).

# Teste F-----------------------------------------------------------------------

#Objetivo: detectar se o modelo de fato apresenta tendência linear

print(teste_4$est.model)
linearHypothesis(teste_4$est.model,c("trnd = 0", "L(y, 1) = 0" ))

# A estatística F é de 1.0712. 
# Usando o Caso 4 da tabela na página 11 dos slides, 
#vemos que, a 10% de significância,o valor crítico para 144 observações é 5.47. 
# Como F < 5.47, NÃO rejeitamos a hipótese nula de que 
#a tendência e o coeficiente associado a y_{t-1} são ambos zero.

# Conclusão: modelo não apresenta tendência linear.
# Caímos no caso 1.2.1, do slide 16.
# Nesse caso, vamos para o modelo somente com intercepto.

# Procedimento Sequencial II, slide 17------------------------------------------
# Modelo apenas com intercepto--------------------------------------------------

# Se chegamos a esta etapa, não temos evidências de que haja uma
#tendência linear no modelo. 

teste_5 = CADFtest(janela_testes, type = "drift", max.lag.y = ceiling(12*(length(janela_testes)/100)^(1/4)), 
                   criterion = "MAIC")
print(teste_5)
summary(teste_5)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% 
#(p-valor é de 0.605 > 0.10)

# Caímos no caso 2.2., do slide 17:
# Se não rejeitamos a hipótese nula, fazemos o teste F da nula (α, γ) = (0, 0),
#usando os valores críticos do slide 11 (Caso 2).

# Teste F-----------------------------------------------------------------------
#Objetivo: detectar se o modelo de fato apresenta intercepto.

print(teste_5$est.model)
linearHypothesis(teste_5$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

# A estatística F é de 1.7099
# Usando o Caso 2 da tabela da página 11 dos slides,
# a 10% de significância, para 144 observações, o valor crítico é 3.86.
# Como F < 3.86, NÃO REJEITAMOS a hipótese nula
# de que o intercepto e o coeficiente associado a y_{t-1} são ambos zero 
#(alfa e gama, respectivamente).

# Caímos no caso 2.2.1, do slide 17:
# Se não rejeitamos a hipótese nula do teste F, concluímos que o modelo não
#apresenta intercepto. Nesse caso, vamos à etapa 3.


# Procedimento Sequencial III---------------------------------------------------
# Modelo sem componentes determinísticos

# Se chegamos a essa etapa, não temos evidências de que haja uma
#tendência linear no modelo nem um intercepto.


# Testamos a nula de H0 : γ = 0 contra a alternativa de que H1 : γ < 0 
#usando a estatística t chapéu e os valores críticos não normais para esse caso 
#(slide 10, caso 1). 

teste_6 = CADFtest(janela_testes, type = "none", 
                   max.lag.y = ceiling(12*(length(janela_testes)/100)^(1/4)), 
                   criterion = "MAIC")
print(teste_6)
summary(teste_6)

# O teste t NÃO rejeita a hipótese nula de H0 : γ = 0, 
#a 10% (p-valor é de 0.8985 > 0.10).

# Caímos no caso 3.2, do slide 18: 
# Se não rejeitamos a nula, concluímos que a série apresenta UMA raiz unitária.

## Componentes determinísticos--------------------------------------------------

# Vamos testar a presença de componentes determinísticos.
# Como os dados apresentam tendência estocástica, 
#trabalhamos com a série diferenciada

trend = 1:(length(janela_testes)-1)
modelo = lm(diff(janela_testes)~trend)

coeftest(modelo, vcov. = vcovHAC)

# O teste t rejeita a hipótese nula de de que 
#NÃO há tendência linear determinística
#na série em primeira diferença
#a 10% (p-valor é de 0.0.04837 < 0.10). 














#Vamos conduzir os testes a 10% de significância

#Começamos pelo modelo mais completo:
teste_4 = CADFtest(u_ts, type = "trend", 
                   max.lag.y = ceiling(12*(length(u_ts)/100)^(1/4)), #definição do lag ótimo para teste de Dickey-Fuller
                   criterion = "MAIC") #avaliação da especificação do modelo
summary(teste_4)
print(teste_4)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% (p-valor é de 0.2137 > 0.10)

# Caímos no item 1.2. do slide 16: 
# Se não rejeitamos a hipótese nula, fazemos o teste F da nula (β, γ) = (0, 0),
#usando os valores críticos do slide 11 (Caso 4).

# Teste F-----------------------------------------------------------------------

#Objetivo: detectar se o modelo de fato apresenta tendência linear

print(teste_4$est.model)
linearHypothesis(teste_4$est.model,c("trnd = 0", "L(y, 1) = 0" ))

# A estatística F é de 4.2187. 
# Usando o Caso 4 da tabela na página 11 dos slides, 
#vemos que, a 10% de significância,o valor crítico para 408 observações é 5.36. 
# Como F < 5.47, NÃO rejeitamos a hipótese nula de que 
#a tendência e o coeficiente associado a y_{t-1} são ambos zero.

# Conclusão: modelo não apresenta tendência linear.
# Caímos no caso 1.2.1, do slide 16.
# Nesse caso, vamos para o modelo somente com intercepto.

# Procedimento Sequencial II, slide 17------------------------------------------
# Modelo apenas com intercepto--------------------------------------------------

# Se chegamos a esta etapa, não temos evidências de que haja uma
#tendência linear no modelo. 

teste_5 = CADFtest(u_ts, type = "drift", max.lag.y = ceiling(12*(length(u_ts)/100)^(1/4)), 
                   criterion = "MAIC")
print(teste_5)
summary(teste_5)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% 
#(p-valor é de 0.1787 > 0.10)

# Caímos no caso 2.2., do slide 17:
# Se não rejeitamos a hipótese nula, fazemos o teste F da nula (α, γ) = (0, 0),
#usando os valores críticos do slide 11 (Caso 2).

# Teste F-----------------------------------------------------------------------
#Objetivo: detectar se o modelo de fato apresenta intercepto.

print(teste_5$est.model)
linearHypothesis(teste_5$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

# A estatística F é de 2.6021
# Usando o Caso 2 da tabela da página 11 dos slides,
# a 10% de significância, para 408 observações, o valor crítico é 3.79.
# Como F < 3.86, NÃO REJEITAMOS a hipótese nula
# de que o intercepto e o coeficiente associado a y_{t-1} são ambos zero 
#(alfa e gama, respectivamente).

# Caímos no caso 2.2.1, do slide 17:
# Se não rejeitamos a hipótese nula do teste F, concluímos que o modelo não
#apresenta intercepto. Nesse caso, vamos à etapa 3.


# Procedimento Sequencial III---------------------------------------------------
# Modelo sem componentes determinísticos

# Se chegamos a essa etapa, não temos evidências de que haja uma
#tendência linear no modelo nem um intercepto.


# Testamos a nula de H0 : γ = 0 contra a alternativa de que H1 : γ < 0 
#usando a estatística t chapéu e os valores críticos não normais para esse caso 
#(slide 10, caso 1). 

teste_6 = CADFtest(u_ts, type = "none", 
                   max.lag.y = ceiling(12*(length(u_ts)/100)^(1/4)), 
                   criterion = "MAIC")
print(teste_6)
summary(teste_6)

# O teste t NÃO rejeita a hipótese nula de H0 : γ = 0, 
#a 10% (p-valor é de 0.6114 > 0.10).

# Caímos no caso 3.2, do slide 18: 
# Se não rejeitamos a nula, concluímos que a série apresenta UMA raiz unitária.

## Componentes determinísticos--------------------------------------------------

# Vamos testar a presença de componentes determinísticos.
# Como os dados apresentam tendência estocástica, 
#trabalhamos com a série diferenciada

trend = 1:(length(u_ts)-1)
modelo = lm(diff(u_ts)~trend)

coeftest(modelo, vcov. = vcovHAC)

# O teste t rejeita a hipótese nula de de que 
#NÃO há tendência linear determinística
#na série em primeira diferença
#a 10% (p-valor é de 0.0.03642 < 0.10). 


# Box Jenkins

#Dos testes anteriores, sabemos que a série apresenta raiz unitária, e de:
trend = 1:(length(u_ts)-1)
regg = lm(diff(u_ts)~trend)
coeftest(regg, vcov. = vcovHAC)

#Série APRESENTA componentes determinísticos na primeira diferença.

# Dessa forma, trabalharemos com os resíduos do modelo ...

#Coeficinete é significante (p-valor = 0.03642 < 0.10), série tem trend

#Dessa forma, 

trend = 1:(length(u_ts)-1)
modelo = lm(diff(u_ts)~trend)
coeftest(modelo, vcov. = vcovHAC)

residuals_model <- residuals(modelo)

residuals_df <- data.frame(
  residuals = residuals_model
)

residuals_ts <- ts(residuals_model, start = c(1991,1), frequency = 12)

plot(residuals_ts)


#Cria-se a janela temporal:

# Até 2002:
janela_residuals <- window(residuals_ts, end = c(2002,12))


acf((janela_residuals), lag.max = 60)
pacf((janela_residuals), lag.max = 60)


u_ts <- ts(u$u_star, start = c(1991,1), frequency = 12)

janela_u <- window(u_ts, end = c(2002,12))


#Sendo conservador, da inspeção visual da FAC e FACP, incluímos, na parte não sazonal:
pmax = 4
qmax = 3 
spmax = 0 #Verificar !!!!

# Generating table to choose most suitable model:

tabela = arima.est.parallel(janela_u, pmax, qmax, d=1, pseas_max = spmax, include.constant = F, include.trend = T,
                            signif = 0.1, lags.lbox = c(20,30))
#Removendo modelos que não convergiram
tabela = tabela[tabela$Converged,] 

# Verificar qual modelo escolher

# ARIMA(1,1,2), Invertível e estacionário

#Vamos estimá-lo:
modelo = Arima(janela_u, order =c(0,1,3), include.constant = F, include.drift = T)
summary(modelo)

checkresiduals(modelo)

grafico = forecast(modelo, h=4)
plot(grafico)
