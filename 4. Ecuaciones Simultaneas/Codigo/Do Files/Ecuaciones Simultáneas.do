
clear all
global dta "/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/5. Ecuaciones Simultaneas/Ejercicio/" 

*=========================
* Ecuaciones Simultáneas
*=========================

* Data de oferta y demanda laboral para mujeres trabajadoras
use "$dta/MROZ.dta", clear

* Solo mujeres que trabajan
keep if inlf==1

describe hours lwage educ exper expersq age kidslt6 nwifeinc
summarize hours lwage educ exper expersq age kidslt6 nwifeinc
list hours lwage educ exper expersq age kidslt6 nwifeinc in 1/10

* Regresion para horas usando estimación MCO
eststo mco_of: reg hours lwage educ age kidslt6 nwifeinc

* Regresion para horas usando estimación MC2E
* lwage es instrumentalizada por variables de otra ecuación
eststo mc2e_of: ivreg hours (lwage = exper expersq) educ age kidslt6 nwifeinc, first

* Regresion para lwage usando estimación MCO
eststo mco_de: reg lwage hours educ exper expersq

* Regresion para lwage usando estimación MC2E
* hours es instrumentalizada por variables de otra ecuación
eststo mc2e_de: ivreg lwage (hours = age kidslt6 nwifeinc) educ exper expersq, first

* Tablas
esttab mco_of mc2e_of mco_de mc2e_de using "$dta/resultados.csv", ///
					   replace title("Ecuaciones Simultaneas") ///
					   b(3) se(3) stats(N r2, fmt(0 3) ///
					   labels("Observations" "R2")) ///
					   star(* 0.10 ** 0.05 *** 0.01) ///
					   mtitle("OLS-Oferta" "MC2E-Oferta" "OLS-Demanda" "MC2E-Demanda") ///
		               note("Standard errors in parentheses")	


* Evaluando condición de rango
*===============================

* Evaluar la condición de rango implica estimar la ecuación de forma reducida 
* y probar la significancia de las variables instrumentales

* Ecuación de la forma reducida para lwage, ecuación de identificación para hours
reg lwage educ age kidslt6 nwifeinc exper expersq
test exper expersq

* Ecuación de la forma reducida para hours, ecuación de identificación para lwage
reg hours educ age kidslt6 nwifeinc exper expersq
test age kidslt6 nwifeinc
