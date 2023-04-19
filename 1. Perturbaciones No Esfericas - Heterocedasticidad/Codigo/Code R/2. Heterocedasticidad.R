
setwd("/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/2. Heterocedasticidad/Trabajadas")
library(readxl)
options(prompt="R> ", digits=4, scipen=999)

base <- read_excel("base.xlsx")


# 1. Modelo MCO ---------
names(base)
formula <- lnalim ~ lning + mieperho + urbano + costa + sierra
ols <- lm(formula, data = base)
summary(ols)

#install.packages("stargazer")
library("stargazer")
stargazer(ols, title = "modelo estimado", type = "text")

# 2. Pruebas de Heterocedasticidad -----------

# 2.1. Test White (1era Forma)-------

# a. Capturo los residuos
resids <-ols$residuals
data_frame_white<-as.data.frame(cbind(base, resids))

# b. Regresion auxiliar
formula2 <- I(resids^2) ~ lning + mieperho + urbano + costa + sierra +
                  I(lning^2) + (lning*mieperho) + (lning*urbano) + (lning*costa) + (lning*sierra) +
                  I(mieperho^2) + (mieperho*urbano)+(mieperho*costa)+(mieperho*sierra)+
                  (urbano*costa)
  
regresion_auxiliar <- lm(formula2, data = data_frame_white)
sumario<- summary(regresion_auxiliar)
sumario

# c. Validar la hipótesis nula
n <- nrow(data_frame_white)
R_2<- sumario$r.squared
LM_W<- n*R_2
gl <- 15        # Numero de variables explicativas de la ecuación auxiliar
p_value<- 1- pchisq(q = LM_W, df = gl)
VC<- qchisq(p = 0.95, df = gl)
salida_white<- c(LM_W, VC, p_value)
names(salida_white)<-c("LMw", "Valor critico", "p-value")
stargazer(salida_white, title = "Resultados de la prueba de white", type = "text", digits =0)


# 2.2. Test White (2da forma) ------------
#install.packages("lmtest")
library(lmtest)

prueba_white<- bptest(ols, 
                      ~ lning + mieperho + urbano + costa + sierra +
                        I(lning^2) + (lning*mieperho) + (lning*urbano) + (lning*costa) + (lning*sierra) +
                        I(mieperho^2) + (mieperho*urbano)+(mieperho*costa)+(mieperho*sierra)+
                        (urbano*costa),
                        data = base)
print(prueba_white)


# 2.3. Test Breusch-Pagan/Godfrey -------------------
library(lmtest)
bpg <- bptest(formula, studentize = F, data=base)
bpg


# 3. Modelo WLS ---------
ols <- lm(formula, data = base)
summary(ols)
confint(ols, level=0.95)
resids <-ols$residuals
resids2 <- resids^2

wls <- lm(formula, data = base, weights=(1/resids2))
summary(wls)
confint(wls, level=0.95)

ci_ols <- confint(ols, level=0.95)
ci_wls <- confint(wls, level=0.95)
ci_ols
ci_wls
ci_ols[,2] - ci_ols[,1]
ci_wls[,2] - ci_wls[,1]
# Los intervalos de confianza de WLS son más estrechos o pequeños que los de OLS
# Por lo tanto, de acuerdo a la teoría, FGLS con un modelo corregido
# por heterocedasticidad es más eficiente que OLS


# 4. Modelo con errores estándar White -------
install.packages("lmtest")
library("sandwich")
library("lmtest")
coeftest(ols, vcov=vcovHC(ols, "HC1"))





