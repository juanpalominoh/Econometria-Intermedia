
setwd("/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/4. Endogeneidad/RScript /")
library(haven)

library("foreign")
library("AER")
library("stargazer")

card <- read_dta("CARD.DTA")
names(card)

ols1 <- lm(log(wage) ~ educ + exper + expersq + black + south + smsa +
             reg661 + reg662 + reg663 + reg664 + reg665 + reg666 +
             reg667 + reg668 + smsa66,
           data=card)

ols2 <- lm(log(wage) ~ educ + exper + expersq + black + south + smsa +
             reg661 + reg662 + reg663 + reg664 + reg665 + reg666 +
             reg667 + reg668 + smsa66 + IQ,
           data=card)

iv1  <- ivreg(log(wage) ~ educ + exper + expersq + black + south + smsa +
                reg661 + reg662 + reg663 + reg664 + reg665 + reg666 +
                reg667 + reg668 + smsa66 |
                exper + expersq + black + south + smsa + reg661 + reg662 + 
                reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + smsa66 + nearc4,
              data=card)

red  <- lm(educ ~ nearc4 + exper + expersq + black + south + smsa + 
             reg661 + reg662 + reg663 + reg664 + reg665 + reg666 +
             reg667 + reg668 + smsa66,
           data=card) 

iq1 <- lm(IQ ~ nearc4,
          data=card)

iq2 <- lm(IQ ~ nearc4 + reg661 + reg662 + reg663 + reg664 + reg665 + reg666 +
            reg667 + reg668 + smsa66,
          data=card)

iv2 <- ivreg(log(wage) ~ educ + exper + expersq + black + south + smsa +
               reg661 + reg662 + reg663 + reg664 + reg665 + reg666 +
               reg667 + reg668 + smsa66 |
               black + south + smsa + reg661 + reg662 + 
               reg663 + reg664 + reg665 + reg666 + reg667 + 
               reg668 + age + I(age^2) + smsa66 + nearc4,
             data=card)

iv3 <- ivreg(log(wage) ~ educ + exper + expersq + black + south + smsa +
               reg661 + reg662 + reg663 + reg664 + reg665 + reg666 +
               reg667 + reg668 + smsa66 |
               exper + expersq + black + south + smsa + reg661 + reg662 + 
               reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + 
               smsa66 + nearc2 + nearc4,
             data=card)

red3 <- lm(educ ~ exper + expersq + black + south + smsa + reg661 + reg662 + 
             reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + smsa66 +
             nearc2 + nearc4, data=card)

stargazer(ols1, ols2, iv1, red, iq1, iq2,
          type="html", 
          out="variable_instrumental.html",
          align = TRUE,
          keep.stat = c("rsq", "adj.rsq", "n"),
          column.sep.width = "0.01pt",
          no.space = TRUE,
          title = "Return to Education",
          covariate.labels=c("Educ", "Nearc4", "Experience", "Experience2",
                             "Black", "South", "SMSA"),
          float.env="table",
          font.size="footnotesize",
          model.names=FALSE,
          table.placement = "t",
          header= FALSE,
          notes.align="l",
          multicolumn= FALSE,
          label="table:iv"
)

stargazer(iv2, iv3, red3,
          type="html", 
          out="variable_instrumental2.html",
          align=TRUE,
          keep.stat=c("rsq", "adj.rsq", "n"),
          column.sep.width = "0.01pt",
          no.space=TRUE,
          title="Return to Education",
          float.env="table",
          font.size="footnotesize",
          model.names=FALSE,
          table.placement = "t",
          header=FALSE,
          notes.align="l",
          multicolumn=FALSE,
          label="table:iv2"
)          


library("car")
linearHypothesis(red3, c("nearc2 = 0", "nearc4 = 0"), test = "F")
