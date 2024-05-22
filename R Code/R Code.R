# Coursework - Econometrics 3 --------------------------------------------------
# Authors: Hiaman Santos and João Francisco C. Perez ---------------------------

# Installing packages-----------------------------------------------------------
# Tidyverse
#install.packages("tidyverse")
library(tidyverse)

# Installing packages to decompose and deseasonalize data
#install.packages("seasonal")
library(seasonal) #Arima X13-Seats

#install.packages("mFilter")
library(mFilter) #Filtro HP

#URCA package
#install.packages("urca")
library(urca)

#install.packages("forecast")
library(forecast)

#install.packages("sandwich")
library(sandwich) # HAC standard errors

#install.packages("lmtest")
library(lmtest) #Robust standard errors

#install.packages("CADFtest")
library(CADFtest) #uni root tests

#install.packages("car")
library(car) #Linear restriction errors

#install.packages("readxl")
library(readxl)

#install.packages("ggthemes")
library(ggthemes)

#Setting work directory

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path) %>%
  gsub("\\R Code", "", .)

setwd(file_directory)

getwd()

folder_path <- "Database\\"

file_path <- paste0(folder_path, "data.xlsx")

data <- read_excel("data.xlsx", 
                   col_types = c("text", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric"))


# Checking data classes
class(data$Vacancies_mill)
class(data$EAP_mill)
class(data$Employed_mill)
class(data$Unemployed_mill)
class(data$Unemployment_rate)
class(data$Vacancies)

# Ex.2.-------------------------------------------------------------------------

# Creating new variable vacancy rate

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

teste_2 = CADFtest(Vacancy_rate, type = "drift", max.lag.y = 
                     ceiling(12*(length(Vacancy_rate)/100)^(1/4)), criterion = "MAIC")
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

#Nível de desemprego eficiente: u = sqrt(u*v)

u <- mutate(data, u_star = sqrt((data$vacancy_rate*data$Unemployment_rate)))

u_ts <- ts(u$u_star, start = c(1991,1), frequency = 12)

plot(u_ts)


# Uniroot tests-----------------------------------------------------------------
# Conducting uniroot tests for the period from 1991 to 2002:

#janela_testes <- window(u_ts, end = c(2002,12))


#Começamos pelo modelo mais completo:
#teste_4 = CADFtest(janela_testes, type = "trend", 
                   #max.lag.y = ceiling(12*(length(janela_testes)/100)^(1/4)), #definição do lag ótimo para teste de Dickey-Fuller
                   #criterion = "MAIC") #avaliação da especificação do modelo
#summary(teste_4)
#print(teste_4)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% (p-valor é de 0.9962 > 0.10)

# Caímos no item 1.2. do slide 16: 
# Se não rejeitamos a hipótese nula, fazemos o teste F da nula (β, γ) = (0, 0),
#usando os valores críticos do slide 11 (Caso 4).

# Teste F-----------------------------------------------------------------------

#Objetivo: detectar se o modelo de fato apresenta tendência linear

#print(teste_4$est.model)
#linearHypothesis(teste_4$est.model,c("trnd = 0", "L(y, 1) = 0" ))

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

#teste_5 = CADFtest(janela_testes, type = "drift", 
#max.lag.y = ceiling(12*(length(janela_testes)/100)^(1/4)), criterion = "MAIC")
#print(teste_5)
#summary(teste_5)

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% 
#(p-valor é de 0.605 > 0.10)

# Caímos no caso 2.2., do slide 17:
# Se não rejeitamos a hipótese nula, fazemos o teste F da nula (α, γ) = (0, 0),
#usando os valores críticos do slide 11 (Caso 2).

# Teste F-----------------------------------------------------------------------
#Objetivo: detectar se o modelo de fato apresenta intercepto.

#print(teste_5$est.model)
#linearHypothesis(teste_5$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

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

#teste_6 = CADFtest(janela_testes, type = "none", 
                   #max.lag.y = ceiling(12*(length(janela_testes)/100)^(1/4)), 
                   #criterion = "MAIC")
#print(teste_6)
#summary(teste_6)

# O teste t NÃO rejeita a hipótese nula de H0 : γ = 0, 
#a 10% (p-valor é de 0.8985 > 0.10).

# Caímos no caso 3.2, do slide 18: 
# Se não rejeitamos a nula, concluímos que a série apresenta UMA raiz unitária.

## Componentes determinísticos--------------------------------------------------

# Vamos testar a presença de componentes determinísticos.
# Como os dados apresentam tendência estocástica, 
#trabalhamos com a série diferenciada

#trend = 1:(length(janela_testes)-1)
#modelo = lm(diff(janela_testes)~trend)

#coeftest(modelo, vcov. = vcovHAC)

# O teste t rejeita a hipótese nula de de que 
#NÃO há tendência linear determinística
#na série em primeira diferença
#a 10% (p-valor é de 0.0.04837 < 0.10). 

# Uniroot tests-----------------------------------------------------------------
# Testes com janela para a variável desemprego eficiente

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



# Reportamos que comportamento das séries para resíduos e trend é semelhante. 
# Trend é estatisticamente significnate, todavia o valor do coeficiente é baixo.
# Trend não faz muito sentido econômico.

# Box Jenkins para a série u_ts (em primeira diferença)

#Dos testes anteriores, sabemos que a série apresenta raiz unitária, e de:
trend = 1:(length(u_ts)-1)
regg = lm(diff(u_ts)~trend)
coeftest(regg, vcov. = vcovHAC)

#Série APRESENTA componentes determinísticos na primeira diferença.
#Coeficinete é significante (p-valor = 0.03642 < 0.10), série tem trend
# Uma vez que coeficiente é muito baixo, apesar de estatisticamente significante,
# Trabalharemos com a própria série diferenciada.

#Dessa forma, 

#trend = 1:(length(u_ts)-1)
#modelo = lm(diff(u_ts)~trend)
#coeftest(modelo, vcov. = vcovHAC)

#residuals_model <- residuals(modelo)

#residuals_df <- data.frame(
  #residuals = residuals_model
#)

#residuals_ts <- ts(residuals_model, start = c(1991,1), frequency = 12)

#plot(residuals_ts)


#Cria-se a janela temporal:

# Até 2002:
#janela_residuals <- window(residuals_ts, end = c(2002,12))

#acf((janela_residuals), lag.max = 60)
#pacf((janela_residuals), lag.max = 60)


u_ts <- ts(u$u_star, start = c(1991,1), frequency = 12)

janela_u <- window(u_ts, end = c(2002,12))

acf(window(diff(u_ts), end = c(2002,12)), lag.max=60)

pacf(window(diff(u_ts), end = c(2002,12)), lag.max=60)

#Sendo conservador, da inspeção visual da FAC e FACP, incluímos, na parte não sazonal:
pmax = 4
qmax = 3 
spmax = 0 #Verificar !!!!

# Generating table to choose most suitable model:

tabela = arima.est.parallel(janela_u, pmax, qmax, d=1, include.constant = F, include.trend = F,
                            signif = 0.1, lags.lbox = c(20,30))
#Removendo modelos que não convergiram
tabela = tabela[tabela$Converged,] 

# Verificar qual modelo escolher

# ARIMA(0,1,3), Invertível 

#Vamos estimá-lo:
modelo = Arima(janela_u, order =c(0,1,3), include.constant = F, include.drift = T)
summary(modelo)

checkresiduals(modelo)


#Usaremos agora o modelo estimado até 2002 para fazer previsões contrafactuais
#após a reforma (jan-2003 a dez-2006)

forecast <- forecast(modelo, h = 4*12)
plot(forecast)

#Intervalos de predição para a série em si

forecast_mean  <- ts(forecast$mean, start = c(2003, 1), frequency = 12)
forecast_lower <- ts(forecast$lower[,2], start = c(2003, 1), frequency = 12)
forecast_upper <- ts(forecast$upper[,2], start = c(2003, 1), frequency = 12)

#Construindo intervalos de predição para o efeito \alpha

effect_mean  <- window(u_ts, start = c(2003, 1)) - forecast_mean
effect_lower <- window(u_ts, start = c(2003, 1)) - forecast_upper
effect_upper <- window(u_ts, start = c(2003, 1)) - forecast_lower

plot_data <- u %>% 
  mutate(Date =as.Date(paste(Date, "01", sep = "-"), 
                       format = "%Y-%b-%d")) %>% 
  mutate(forecast_mean_data  = ifelse(Date < as.Date("2003-01-01"),
                              yes = NA,
                              no = forecast_mean),
         forecast_lower_data = ifelse(Date < as.Date("2003-01-01"),
                                 yes = NA,
                                 no = forecast_lower),
         forecast_upper_data = ifelse(Date < as.Date("2003-01-01"),
                                 yes = NA,
                                 no = forecast_upper),
         u_star_data = u_star) %>%
  filter(Date < as.Date("2007-01-01"))


ggplot(data = plot_data, aes(x = Date)) +
  geom_line(aes(y = u_star_data, color = "u*")) +
  geom_line(aes(y = forecast_mean_data,  color = "Forecasting contrafactual"), 
            linetype = "dotted") +
  geom_line(aes(y = forecast_lower_data), #color = "lightblue",
            linetype = "dotted") +
  geom_line(aes(y = forecast_upper_data), #color = "lightblue",
            linetype = "dotted") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y",
               limits = as.Date(c('1991-01-01','2006-11-01'))) +
  theme_stata(scheme = "s1color")
  
  







