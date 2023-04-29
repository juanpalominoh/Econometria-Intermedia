
global dta 		"/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/4. Endogeneidad/RScript "
global results 	"/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/4. Endogeneidad/Resultados"

use "$dta/CARD.DTA", clear

gen lnwage=log(wage)

* MCO
regress lnwage educ exper expersq black south smsa ///
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66, cformat(%6.3fc) 
outreg2 using "$results/regresiones.xls", bdec(3) sdec(3) stats(coef se) ctitle("log(wage)") noparen addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") excel replace
	
* Agregando el IQ
reg lnwage educ exper expersq black south smsa ///
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66 IQ, cformat(%6.3fc) 
outreg2 using "$results/regresiones.xls", bdec(3) sdec(3) stats(coef se) ctitle("log(wage)") noparen addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") excel
	

* Regresión por variable instrumental
ivreg lnwage exper expersq black south smsa ///
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66 (educ=nearc4), cformat(%6.3fc)
outreg2 using "$results/regresiones.xls", bdec(3) sdec(3) stats(coef se) ctitle("log(wage)") noparen addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") excel

	
* Relacion educ y nearc4
reg educ nearc4 exper expersq black south smsa /// 
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66, cformat(%6.3fc)	
outreg2 using "$results/regresiones.xls", bdec(3) sdec(3) stats(coef se) ctitle("educ") noparen addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") excel


* Instrumento valido
display (0.320/0.088)^2

* IQ sin variables de control
reg IQ nearc4, cformat(%6.3fc)
outreg2 using "$results/regresiones.xls", bdec(3) sdec(3) stats(coef se) ctitle("IQ") noparen addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") excel
 

* IQ con variables de control
reg IQ nearc4 reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66, cformat(%6.3fc) 
outreg2 using "$results/regresiones.xls", bdec(3) sdec(3) stats(coef se) ctitle("IQ") noparen addnote("Sea ***, **, * los niveles de significancia al 1%, 5% y 10%") excel


* Dos etapas
ssc install ivreg2
ssc install ranktest
ivreg2 lnwage exper expersq black south smsa ///
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66 (educ=nearc4), first 
	

* Test de exogeneidad de los instrumentos
*=========================================
quietly ivreg2 lnwage exper expersq black south smsa ///
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66 (educ=nearc4)
	
predict resid, resid

quietly reg resid exper expersq black south smsa ///
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66 nearc4
	
test (nearc4=0)

scalar J=2*r(F)
display J
* Chi-2 (95%) 1 = 3.84. No se puede rechazar la H0 de que los instrumentos no están 
* correlacionados excluidos de esta ecuación. 


* Test de endogeneidad
*=======================
ivreg2 lnwage exper expersq black south smsa ///
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66 (educ=nearc4), first 
estimates store mc2e

reg lnwage educ exper expersq black south smsa ///
    reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 smsa66, cformat(%6.3fc) 
estimates store mco

hausman mc2e mco	
	
