
rm(list = ls(all = TRUE))
setwd("/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/1. Perturbaciones No Esfericas/Ejemplo/Procesadas")

# 1. Cargar paquetes y data --------
library(haven)
library(foreign)
enaho2020 <- read_dta("clean_enaho_2020.dta")

# Nombre de variables
names(enaho2020)

# Numero de observaciones y variables
dim(enaho2020)

# Resumen Estadistico de variables
summary(enaho2020)

# 2. Modelos con diferentes controles -----
ols_1 <- lm(log(wage) ~ sch,
            data = enaho2020)
ols_2 <- lm(log(wage) ~ sch + sexo,
            data = enaho2020)
ols_3 <- lm(log(wage) ~ sch + sexo + edad,
            data = enaho2020)
ols_4 <- lm(log(wage) ~ sch + sexo + edad + estatus,
            data = enaho2020)

# 3. Presentando en tabla ----
install.packages("memisc")
library("memisc")
table_1 <- mtable("Model 1" = ols_1,
                  "Model 2" = ols_2,
                  "Model 3" = ols_3,
                  "Model 4" = ols_4,
                  summary.stats = c("N", "R-squared"),
                  coef.style = "default")
table_1

library(stargazer)
stargazer(ols_1, ols_2, ols_3, ols_4, type="html", out="ols.html",
          covariate.labels = c("Escolaridad", "Hombre", "Edad", "Con pareja","Constant"),
          dep.var.labels=c("Logaritmo del Salario"),
          title = "Modelo de Regresion Lineal",
          notes = c("Fuente: Elaboracion Propia"))


# 4. Usando matrices ----------
X <- cbind(1, enaho2020$sch,
           enaho2020$sexo,
           enaho2020$edad,
           enaho2020$estatus)
X
y <- log(enaho2020$wage)
y


b_hat <- solve(crossprod(X)) %*% crossprod(X, y) # (X'X)^-1 X'y
b_hat

y_hat <- X %*% b_hat
e_hat <- y - y_hat
sigma2_hat <- crossprod(e_hat) / (length(y) - ncol(X))  # n-K
sigma2_hat

V_hat <- drop(sigma2_hat) * solve(crossprod(X))
se <- sqrt(diag(V_hat))
se
