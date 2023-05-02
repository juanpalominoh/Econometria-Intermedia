# Ecuaciones Simultáneas
  
# Data files: 
#   MROZ.csv

# setup
rm(list = ls()) 
setwd("/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/5. Ecuaciones Simultaneas/Ejercicio/")

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "AER", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}   


# Ecuaciones Simultáneas --------------------------------------------------

# Data de oferta y demanda laboral para mujeres trabajadoras
MROZ <- read.csv("MROZ.csv")

# Solo mujeres que trabajan
MROZ %<>% filter(inlf == 1)

MROZ %>% 
  select(hours, lwage, educ, exper, expersq, age, kidslt6, nwifeinc) %>% 
  stargazer(type = "text")

MROZ %>% 
  select(hours, lwage, educ, exper, expersq, age, kidslt6, nwifeinc) %>% 
  head(10)

# Regresión para hours usando estimación MCO
model1 <- lm(hours ~ lwage + educ + age + kidslt6 + nwifeinc, MROZ)
summary(model1)

# Regresión para hours usando estimación MC2E
# lwage es instrumentalizada por variables de otra ecuación
model2 <- ivreg(hours ~ lwage + educ + age + kidslt6 + nwifeinc | 
                  . - lwage + exper + expersq, data = MROZ)
summary(model2, diagnostics = TRUE)

# Regresion para lwage usando estimación MCO
model3 <- lm(lwage ~ hours + educ + exper + expersq, MROZ)
summary(model3)

# Regresion para lwage usando estimación MC2E
# hours es instrumentalizada por variables de otra ecuación
model4 <- ivreg(lwage ~ hours + educ + exper + expersq | 
                  ~ . - hours + age + kidslt6 + nwifeinc, 
                data = MROZ)
summary(model4, diagnostics = TRUE)


# Evaluando condición de rango ----------------------------------------------

# Evaluar la condición de rango implica estimar la ecuación de forma reducida 
# y probar la significancia de las variables instrumentales

# Ecuación de la forma reducida para lwage, ecuación de identificación para hours
model5 <- lm(lwage ~ educ + age + kidslt6 + nwifeinc + exper + expersq, MROZ)
summary(model5)
linearHypothesis(model5, c("exper = 0", "expersq = 0"))

# Ecuación de la forma reducida para hours, ecuación de identificación para lwage
model6 <- lm(hours ~ educ + age + kidslt6 + nwifeinc + exper + expersq, MROZ)
summary(model6)
linearHypothesis(model6, c("age = 0", "kidslt6 = 0", "nwifeinc = 0"))
