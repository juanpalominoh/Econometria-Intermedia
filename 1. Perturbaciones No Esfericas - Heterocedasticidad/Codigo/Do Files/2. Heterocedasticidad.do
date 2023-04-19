
global main  "/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/2. Heterocedasticidad"
global dta   "$main/Datos"
global works "$main/Trabajadas"


*================
**# Test de White
*================

* Modelo 1
* (Primera Forma)
use "$works/base.dta", clear

regress lnalim lning mieperho urbano costa sierra

predict ehat, resid
generate ehat2=ehat^2

reg ehat2 lning mieperho urbano costa sierra ///
	      c.(lning mieperho urbano costa sierra)#c.(lning mieperho urbano costa sierra)
		  
display e(N)*e(r2)   // 107.9904
* chi-square 0.95(15)=25
		  
* 2da forma
reg lnalim lning mieperho urbano costa sierra
whitetst
imtest, white


* Modelo 2
use "$works/base.dta", clear

reg lnalim lning lning2 mieperho urbano costa sierra

predict ehat, resid
gen ehat2=ehat^2

reg ehat2 lning lning2 mieperho urbano costa sierra ///
	      c.(lning lning2 mieperho urbano costa sierra)# ///
		  c.(lning lning2 mieperho urbano costa sierra)
display e(N)*e(r2)   // 126.42628
		  
reg lnalim lning lning2 mieperho urbano costa sierra
whitetst
imtest, white


*==========================
**# Test de Breusch-Pagan
*=========================
use "$works/base.dta", clear

* Paso 1: Estimas la ecuación
reg lnalim lning mieperho urbano costa sierra

* Paso 2: Calculas residuos y residuos al cuadrado
predict ehat, resid
gen ehat2=ehat^2

* Paso 3: Calculas sigma2
gen sigma2=e(rss)/e(N)

* Paso 4: Calculas residuos al cuadrado / sigma2
gen ratio=ehat2/sigma2

* Paso 5: Regresionas la ecuación auxiliar
reg ratio lning mieperho urbano costa sierra

* Paso 6: Hallas el valor a comparar con el chi-cuadrado de tabla
display 0.5*e(mss)
* chi-square 0.95(15)=9.49



use "$works/base.dta", clear

* Paso 1: Estimas la ecuación
reg lnalim lning mieperho urbano costa sierra
estat hettest, rhs


**# Minimos cuadrados ponderados por varianza 
*============================================
use "$works/base.dta", clear

* Paso 1:
reg lnalim lning mieperho urbano costa sierra

* Paso 2: Calculas residuos y residuos al cuadrado
predict ehat, resid
gen ehat2=ehat^2

* Paso 3: Calculas el desvio
gen desvio=ehat2^0.5

vwls lnalim lning mieperho urbano costa sierra, sd(desvio)  // Estimamos MCG ponderados por varianza



**# MCO corregida por White
*============================
reg lnalim lning mieperho urbano costa sierra, robust 

reg lnalim lning mieperho urbano costa sierra


