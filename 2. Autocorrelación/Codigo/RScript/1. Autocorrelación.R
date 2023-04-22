
setwd("/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/2. Autocorrelación/Datos/")
options(prompt="R> ", digits=4, scipen=999)

library(readxl)
base <- read_excel("Datos.xlsx")


# 1. Modelo MCO ---------
formula <- consumo ~ PBI
ols <- lm(formula, data=base)
summary(ols)

resids <-ols$residuals
data_frame_white<-as.data.frame(cbind(base, resids))


# 2. Pruebas de Autocorrelacion -----------

# 2.1. Test Durbin Watson ---------
library(lmtest)
dwtest(ols,alternative = "two.sided",iterations = 1000)

install.packages("car")
library(car)
durbinWatsonTest(ols,simulate = TRUE,reps = 1000) 
# Se rechaza la H0 de no autocorrelación


# 2.2 Test Breusch-Godfrey ---------
install.packages("lmtest", dependencies = T)
library(lmtest)
bgtest(ols, order=2)


# 3. MCO con errores estándar Newey-West ---------
library("sandwich")
NW_VCOV <- NeweyWest(ols, 
                     lag =1, prewhite = F, 
                     adjust = T)

coeftest(ols, vcov=NW_VCOV)
summary(ols)
# Más detalles en: https://www.econometrics-with-r.org/15-4-hac-standard-errors.html


# 4. MCG Prais-Winsten ---------------
install.packages("prais")
library("prais")
pw <- prais_winsten(formula, data=base, index=base$Año)
summary(pw)
