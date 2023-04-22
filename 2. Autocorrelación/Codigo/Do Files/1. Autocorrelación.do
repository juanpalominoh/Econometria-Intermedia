
global main  "/Users/juanpalomino/Google Drive/Cursos/Econometría Intermedia/2. Autocorrelación"
global dta   "$main/Datos"


import excel "$dta/Datos.xlsx", sheet("Anuales") firstrow clear

tset Año
reg consumo PBI

* Test Durbin Watson
predict residuo, resid
estat dwatson


egen std_res=std(resid)
tsline std_res, xlabel(1950(20)2020)
scatter std_res Año, xlabel(1950(20)2020) graphregion(color(white)) ylabel(, nogrid) yline(0)

corrgram std_res
ac std_res, lags(20) ciopts(lcolor(black)) graphregion(color(white)) // AR(1) (2) (3)


* Test Breusch-Godfrey
regress residuo L(1/2).residuo PBI
display (e(N))*e(r2)

reg consumo PBI
bgodfrey, lag(2)
bgodfrey, lag(2) nomiss0  // Sin corrección de Davidson y MacKinnon


* Estimacion MCG con el método iterativo de Prais Winsten
prais consumo PBI

* Estimacion MCO con errores estándar de Newey West
newey consumo PBI, lag(1)


