/* =============================================================================
                     
					  Real Bussiness Cycles in the World:
					  Empirical Regularities using Stata
					 
					     	       Brian Daza
                         https://briandaza.github.io/
				   
============================================================================= */

/*
Contents:

I. Introduction
II. Data
III. Estimation
IV. Tables
V. Graphics

*/

/* -----------------------------------------------------------------------------
                               I. Introduction
----------------------------------------------------------------------------- */

/* 
El presente código tiene como objetivo responder la sección de ejercicios del 
primer capítulo del libro Open Economy Macroeconomics de Uribe y Schmitt-Grohé.

Si bien las instrucciones de Uribe y Schmitt-Grohé solicitan el análisis de los
ciclos económicos para Corea del Sur y los Estados Unidos, en este código se
aplica el procedimiento para todos los países que tienen al menos 30 observaciones
en la serie de producto per cápita en la base de datos de World Development 
Indicators (Banco Mundial).

Para el cálculo de las estadísticas de ciclos económicos, se utilizan cuatro métodos
alternativos de desestacionalización:
	
	- Desestacionalización log-lineal
	- Desestacionalización log-cuadrática
	- Filtro Hodrik-Prescott (con lambda=100)
	- Filtro Hodrik-Prescott (con lambda=6.25)

A partir de los ciclos económicos calculados mediante los métodos antes descritos, 
se producen tablas resumen con estadísticas de los ciclos económicos (desviaciones 
estándar, correlación con el producto y autocorrelacion serial) y gráficos 
correspondientes al ciclo del producto por cada país en la muestra.

En este código se usan algunos paquetes que no vienen por defecto en la instalación
estándar de Stata. Para instalarlos, basta ejecutar líneas como las siguientes:

ssc install wbopendata
ssc install egenmore
ssc install tabout

Para poder realizar la mayor parte de comandos, que se realizan a traves de 
bucles del tipo "foreach" se ha usado 'macros' o variables auxilares de los tipos
'local' y 'global'. No es posible correr los loops sin haber corrido las líneas 
que generan las macros necesarias; tener ello en cuenta y que las macros del tipo
'local' son temporales y por lo tanto requieren ser creadas junto con la ejecución 
del código que las genera.

*/

* Antes de empezar, indicamos las direcciones donde se ubicarán nuestros outputs:
global name "`c(username)'"
cd "C:\Users\\$name\OneDrive\RBC"
global dir "C:\Users\\$name\OneDrive\RBC"
* Creamos la carpeta de gráficos:
mkdir "$dir\Graficos"
* Y las sub-carpetas por categoría de ingreso
mkdir "$dir\Graficos\Ricos"
mkdir "$dir\Graficos\Emergentes"
mkdir "$dir\Graficos\Pobres"
* Después de correrlas por primera vez, es preferible comentar las cinco líneas 
* de código pues Stata no puede sobreescribir o eliminar carpetas mediante "mkdir"



/* -----------------------------------------------------------------------------
                     II. Importación de la Base de Datos
----------------------------------------------------------------------------- */

* Importamos los indicadores indicados por Uribe y Schmitt-Grohé de manera automática
* a partir de la base de datos World Bank's WDI. Adicionalmente importamos el PBI
* per cápita a precios constantes de 2010 y la población total para clasificar a los países
* de acuerdo a dicha información : 

wbopendata, indicator(ny.gdp.pcap.kn; ne.con.prvt.zs; ne.gdi.totl.zs; ne.con.govt.zs; ne.imp.gnfs.zs; ne.exp.gnfs.zs; ny.gdp.pcap.kd; sp.pop.totl) language(es)  clear long 

* Etiquetamos las variables de acuerdo con su descripción:

label variable ny_gdp_pcap_kn "GDP per capita (constant LCU)"
label variable ne_con_prvt_zs "Households and NPISHs final consumption expenditure (% of GDP)"
label variable ne_gdi_totl_zs "Gross capital formation (% of GDP)"
label variable ne_con_govt_zs "General government final consumption expenditure (% of GDP)"
label variable ne_imp_gnfs_zs "Imports of goods and services (% of GDP)"
label variable ne_exp_gnfs_zs "Exports of goods and services (% of GDP)"

label variable ny_gdp_pcap_kd "GDP per capita (constant 2010 US$)"	// Esta variable la hemos importado para clasificar los países de acuerdo a su nivel de ingreso.
label variable sp_pop_totl "Total population" // Esta variable se ha importado para utilizarla como ponderador en el cálculo de estadísticas.

* Necesitamos calcular las estadísticas sólo para los países que tienen toda la información.
* Generamos una variable que es igual a 1 si tiene todo y que es 'missing' en caso de que alguna de las variables sea missing.
* Luego nos quedamos solamente con las combinaciones país/año, para los que tenemos toda la información (salvo en la variable ny.gdp.pcap.kd y que usaremos sólo como criterios clasificadores):

gen nomissing=( ny_gdp_pcap_kn + ne_con_prvt_zs + ne_gdi_totl_zs + ne_con_govt_zs + ne_imp_gnfs_zs + ne_exp_gnfs_zs )/( ny_gdp_pcap_kn + ne_con_prvt_zs + ne_gdi_totl_zs + ne_con_govt_zs + ne_imp_gnfs_zs + ne_exp_gnfs_zs )

keep if nomissing==1

drop nomissing

* 'Contamos' el número de años que disponemos para cada país:

egen numberyears=count(year), by(countryname)

* Y seleccionamos a los países que tienen más de 30 años de observaciones:

gen countrym30=0
replace countrym30=1 if numberyears>30

* Verificamos que no tengan 'gaps':Si no tienen gaps, el número de observaciones 
*en cada país debería ser igual a uno más la diferencia del año más reciente menos
* el año más antiguo: 

egen minyear=min(year), by(countryname)
egen maxyear=max(year), by(countryname)
gen rangeyear=maxyear-minyear+1

* Quitamos de la muestra a los países con gaps:
gen countrysample=countrym30
replace countrysample=0 if rangeyear!=numberyears

* A continuación, se pueden ver cuales son los países con gaps:
tab countryname if countrysample==0 & countrym30==1

* Generamos un indicador del GDP per cápita a dólares del 2010 promedio con información a partir de 1990 en adelante:

egen y_p=mean(ny_gdp_pcap_kd) if year>=1990, by(countryname)
egen y_P=max(y_p), by(countryname)

* A partir de ello, generamos un indicador del nivel de ingreso:

gen nivelingreso=0
replace nivelingreso=1 if y_P<3000
replace nivelingreso=2 if y_P>=3000 & y_P<25000
replace nivelingreso=3 if y_P>=25000 & y_P!=.

label define niveles 1 "Pobres" 2 "Emergentes" 3 "Ricos" 
label values nivelingreso niveles

* El único país no clasificado perteneciente a la muestra es Syria:
tab countrycode if  countrysample==1 & nivelingreso==0
* Según la data del Banco Mundial, el PBI per cápita en dólares de este país en 2004 fue 1,408.85, y en 2007, 2,058.04
replace nivelingreso=1 if countrycode=="SYR"

* La pertenencia de los países a cada nivel de ingreso es la siguiente:
tab countryname nivelingreso if countrysample==1

* Guardamos
save usabledata, replace

/* -----------------------------------------------------------------------------
                              III. Estimación
----------------------------------------------------------------------------- */

* Usamos la base de datos previamente cargada:
use usabledata, clear

* Codificamos a los nombres de los países:
encode countryname, gen(countryid)

* Para configurar el entorno como un panel de datos:
xtset countryid year
label variable year "Año"

* Generamos las que seán nuestras variables de interés:
gen y=log(ny_gdp_pcap_kn) // GDP per capita
gen c=log(ny_gdp_pcap_kn*ne_con_prvt_zs) // Household final consumption expenditure
gen i=log(ny_gdp_pcap_kn*ne_gdi_totl_zs) //  Gross capital formation
gen g=log(ny_gdp_pcap_kn*ne_con_govt_zs) // General government final consumption expenditure
gen m=log(ny_gdp_pcap_kn*ne_imp_gnfs_zs) // Imports of goods and services
gen x=log(ny_gdp_pcap_kn*ne_exp_gnfs_zs) // Exports of goods and services
gen tb=x-m // Trade balance (pseudo: tb = x - m)
gen tby=tb/y // Trade balance over product
gen gy=g/y // Government expenditure over product

* Tenemos un problema con la variable inversión en dos años para dos países, el Congo y Sierra Leona. 
* El Congo tiene inversión cero en una observacion y Sierra Leona una observación con inversión negativa.
* No es posible calcular logaritmos naturales para ambos valores, por ello, tenemos missing values y el filtro HP no funcionará.
* Como los gaps son sólo de un año, utilizaremos una interpolación lineal simple para esos valores:

replace i = (i[_n-1]+i[_n+1]) if inlist(countrycode,"COD","SLE") & i==.

* Desestacionalización :
* ----------------------

* Generamos una macro con la primera lista de variables :

global macrovar y c i g m x tby gy

* Generaremos dónde imputar los datos finales para cada variable y tipo de desestacionalización:
foreach var of varlist $macrovar{
gen `var'_cl=0 if countrysample==1
gen `var'_trl=0 if countrysample==1
gen `var'_cq=0 if countrysample==1
gen `var'_trq=0 if countrysample==1
gen `var'_chp=0 if countrysample==1
gen `var'_trhp=0 if countrysample==1
gen `var'_chpi=0 if countrysample==1
gen `var'_trhpi=0 if countrysample==1
}

* Y sus respectivas etiquetas:
foreach var of varlist $macrovar{
label variable `var'_cl    "Componente cíclico de `var' (tendencia log-lineal)"
label variable `var'_trl   "Tendencia de `var' (tendencia log-lineal)"
label variable `var'_cq    "Componente cíclico de `var' (tendencia log-cuadrática)"
label variable `var'_trq   "Tendencia de `var' (tendencia log-cuadrática)"
label variable `var'_chp   "Componente cíclico de `var' (filtro HP - Lambda=100)"
label variable `var'_trhp  "Tendencia de `var' (filtro HP - Lambda=100)"
label variable `var'_chpi  "Componente cíclico de `var' (filtro HP - Lambda=6.25)"
label variable `var'_trhpi "Tendencia de `var' (filtro HP - Lambda=6.25)"
}

* Calculamos la tendencia y ciclo de todas las series en $macrovar para cada país:

foreach var of varlist $macrovar {
 levelsof countryname if countrysample==1, local(idpais) 
 foreach j of local idpais {
 
  display "País = `j'"
 
 * Log-linear detrending
 * ---------------------
 reg `var' year if countryname=="`j'"
 predict `var'_tr if countryname=="`j'"
 gen `var'_c = `var' - `var'_tr
 replace `var'_cl=`var'_c if countryname=="`j'"
 replace `var'_trl=`var'_tr if countryname=="`j'"
 drop `var'_c `var'_tr


 * Log-quadratic detrending
 * ------------------------
 gen year2=year^2
 reg `var' year year2 if countryname=="`j'"
 predict `var'_tr if countryname=="`j'"
 gen `var'_c = `var' - `var'_tr
 replace `var'_cq=`var'_c if countryname=="`j'"
 replace `var'_trq=`var'_tr if countryname=="`j'"
 drop `var'_c `var'_tr year2
 

 levelsof countryid if countrysample==1 & countryname=="`j'" , local(ccpp)
 
 * HP filtering with Lambda=100
 * ----------------------------

 tsfilter hp `var'_c = `var' if countryid==`ccpp' , smooth(100) trend(`var'_tr) 
 replace `var'_chp=`var'_c if countryid==`ccpp'
 replace `var'_trhp=`var'_tr if countryid==`ccpp'
 drop `var'_c `var'_tr 
 
 * HP filtering with Lambda=6.25
 * -----------------------------
 
 tsfilter hp `var'_c = `var' if countryid==`ccpp' , smooth(6.25) trend(`var'_tr)
 replace `var'_chpi=`var'_c if countryid==`ccpp'
 replace `var'_trhpi=`var'_tr if countryid==`ccpp'
 drop `var'_c `var'_tr
}
}

* Repetiremos el proceso para la balanza comercial, pero dividiendo el valor de 
* acuerdo a la estimación respectiva del ciclo económico:

gen tb_cl=0 if countrysample==1
gen tb_trl=0 if countrysample==1
gen tb_cq=0 if countrysample==1
gen tb_trq=0 if countrysample==1
gen tb_chp=0 if countrysample==1
gen tb_trhp=0 if countrysample==1
gen tb_chpi=0 if countrysample==1
gen tb_trhpi=0 if countrysample==1

label variable tb_cl    "Componente cíclico de tb (tendencia log-lineal)"
label variable tb_trl   "Tendencia de tb (tendencia log-lineal)"
label variable tb_cq    "Componente cíclico de tb (tendencia log-cuadrática)"
label variable tb_trq   "Tendencia de tb (tendencia log-cuadrática)"
label variable tb_chp   "Componente cíclico de tb (filtro HP - Lambda=100)"
label variable tb_trhp  "Tendencia de tb (filtro HP - Lambda=100)"
label variable tb_chpi  "Componente cíclico de `var' (filtro HP - Lambda=6.25)"
label variable tb_trhpi "Tendencia de tb (filtro HP - Lambda=6.25)"

 foreach j of local idpais {
 
  display "País = `j'"
 
 * Log-linear detrending
 * ---------------------
 gen tb_yc= tb/(exp(y_cl))
 reg tb_yc year if countryname=="`j'"
 predict tb_yc_tr if countryname=="`j'"
 gen tb_yc_c = tb_yc - tb_yc_tr
 replace tb_cl=tb_yc_c if countryname=="`j'"
 replace tb_trl=tb_yc_tr if countryname=="`j'"
 drop tb_yc_c tb_yc_tr tb_yc


 * Log-quadratic detrending
 * ------------------------
 gen tb_yc= tb/(exp(y_cq))
 gen year2=year^2
 reg tb_yc year year2 if countryname=="`j'"
 predict tb_yc_tr if countryname=="`j'"
 gen tb_yc_c = tb_yc - tb_yc_tr
 replace tb_cq=tb_yc_c if countryname=="`j'"
 replace tb_trq=tb_yc_tr if countryname=="`j'"
 drop tb_yc_c tb_yc_tr year2 tb_yc tb_yc
 

 levelsof countryid if countrysample==1 & countryname=="`j'" , local(ccpp)
 
 * HP filtering with Lambda=100
 * ----------------------------
 gen tb_yc= tb/(exp(y_chp))
 tsfilter hp tb_yc_c = tb_yc if countryid==`ccpp' , smooth(100) trend(tb_yc_tr) 
 replace tb_chp=tb_yc_c if countryid==`ccpp'
 replace tb_trhp=tb_yc_tr if countryid==`ccpp'
 drop tb_yc_c tb_yc_tr tb_yc
 
 * HP filtering with Lambda=6.25
 * -----------------------------
 gen tb_yc= tb/(exp(y_chpi))
 tsfilter hp tb_yc_c = tb_yc if countryid==`ccpp' , smooth(6.25) trend(tb_yc_tr)
 replace tb_chpi=tb_yc_c if countryid==`ccpp'
 replace tb_trhpi=tb_yc_tr if countryid==`ccpp'
 drop tb_yc_c tb_yc_tr tb_yc
}

* Grabamos la base de datos procesada:

save processeddata, replace

/* -----------------------------------------------------------------------------
                                 IV. Tablas
----------------------------------------------------------------------------- */

* Generamos la tabla de estadísticos a partir de los ciclos de las series 
* desestacionalizadas previamente:

use processeddata, clear

* Desviaciones estándar:
global tablavar y c i g m x tby gy tb
global filtertypes l q hp hpi
foreach var of varlist $tablavar {
foreach h of global filtertypes {
egen sd`var'_`h'=sd(`var'_c`h') if countrysample==1, by(countryid)
}
}

* Correlaciones con el producto:
global tablavar y c i g m x tby gy tb
global filtertypes l q hp hpi
foreach var of varlist $tablavar {
foreach h of global filtertypes {
egen cy`var'_`h'=corr(`var'_c`h' y_c`h') if countrysample==1, by(countryid)
}
}

* Autocorrelaciones seriales de primer orden:
global tablavar y c i g m x tby gy tb
global filtertypes l q hp hpi
xtset countryid year
foreach var of varlist $tablavar {
foreach h of global filtertypes {
gen lagged`var'_c`h'=L.`var'_c`h'
egen ac`var'_`h'=corr(`var'_c`h' lagged`var'_c`h') if countrysample==1, by(countryid)
}
drop lagged`var'*
}

* "Colapsamos" (resumimos) la base de datos para tener sólo los estadísticos 
* calculados por país y los promedios de las variables y, c, i, g, m, x, tby, gy 
* y tb por país. Retenemos el valor máximo de la variable de población, sp_pop_totl,
* es decir, se considerará la mayor población alcanzada por cada país.

collapse (mean )y c i g m x tby gy tb sd* cy* ac* (max) sp_pop_totl if countrysample==1, by(countryname countryid region regioncode nivelingreso)

* Generamos variable de población redondeada a decenas de millar para ponderación por poblacion:

gen npob=round(sp_pop_totl/10000,1)

* Clasificamos los países por su tamaño:
gen tama=0 
replace tama=1 if npob<2000 // menos de 20 millones
replace tama=2 if npob>=2000 & npob<=8000 // entre 20 y 80 millones
replace tama=3 if npob>8000 // más de 80 millones
label define paistama 1 "Pequeño" 2 "Mediano" 3 "Grande"
label values tama paistama

order countryname countryid region regioncode nivelingreso tama

* Grabamos:
save basestats, replace

* Generamos las tablas:
* ---------------------
use basestats, clear

* Las tablas básicas, en el sentido que contienen la información necesaria, 
* se producen a continuación. La descripción y el agrupamiento adecuado se realizará
* en el editor de texto.

* Tablas por tipo de filtro y nivel de ingreso
* --------------------------------------------
global filtertypes l q hp hpi
foreach h of global filtertypes {
display "            Método de ciclo: `h' "
display " ------------------------------------------"
* Desviaciones estándar
tabstat sd*_`h' [fweight=npob],  by(nivelingreso) format(%4.3f)
* Correlación con el producto: 
tabstat cy*_`h' [fweight=npob],  by(nivelingreso) format(%4.3f)
* Autocorrelación serial de primer orden:
tabstat ac*_`h' [fweight=npob],  by(nivelingreso) format(%4.3f) 
}

* Tablas para Estados Unidos
* --------------------------------------------
global filtertypes l q hp hpi
foreach h of global filtertypes {
display "            Método de ciclo: `h' "
display " ------------------------------------------"
* Desviaciones estándar
tabstat sd*_`h' if countryid==57 [fweight=npob],  by(countryname) format(%4.3f) nototal
* Correlación con el producto: 
tabstat cy*_`h' if countryid==57 [fweight=npob],  by(countryname) format(%4.3f) nototal
* Autocorrelación serial de primer orden:
tabstat ac*_`h' if countryid==57 [fweight=npob],  by(countryname) format(%4.3f) nototal
}

* Tablas por tipo de filtro y tamaño para cada nivel de ingreso
* --------------------------------------------

* Todos los países:
* ----------------
global filtertypes l q hp hpi
foreach h of global filtertypes {
display "            Método de ciclo: `h' "
display " ------------------------------------------"
* Desviaciones estándar
tabstat sd*_`h' [fweight=npob], nototal by(tama) format(%4.3f)
* Correlación con el producto: 
tabstat cy*_`h' [fweight=npob], nototal  by(tama) format(%4.3f)
* Autocorrelación serial de primer orden:
tabstat ac*_`h' [fweight=npob], nototal  by(tama) format(%4.3f) 
}
* Países pobres:
* ----------------
global filtertypes l q hp hpi
foreach h of global filtertypes {
display "            Método de ciclo: `h' "
display " ------------------------------------------"
* Desviaciones estándar
tabstat sd*_`h' if nivelingreso==1 [fweight=npob], nototal  by(tama) format(%4.3f)
* Correlación con el producto: 
tabstat cy*_`h' if nivelingreso==1 [fweight=npob], nototal  by(tama) format(%4.3f)
* Autocorrelación serial de primer orden:
tabstat ac*_`h' if nivelingreso==1 [fweight=npob], nototal  by(tama) format(%4.3f) 
}
* Renta media:
* ----------------
global filtertypes l q hp hpi
foreach h of global filtertypes {
display "            Método de ciclo: `h' "
display " ------------------------------------------"
* Desviaciones estándar
tabstat sd*_`h' if nivelingreso==2 [fweight=npob], nototal  by(tama) format(%4.3f)
* Correlación con el producto: 
tabstat cy*_`h' if nivelingreso==2 [fweight=npob], nototal  by(tama) format(%4.3f)
* Autocorrelación serial de primer orden:
tabstat ac*_`h' if nivelingreso==2 [fweight=npob], nototal  by(tama) format(%4.3f) 
}
* Países ricos:
* ----------------
global filtertypes l q hp hpi
foreach h of global filtertypes {
display "            Método de ciclo: `h' "
display " ------------------------------------------"
* Desviaciones estándar
tabstat sd*_`h' if nivelingreso==3 [fweight=npob], nototal  by(tama) format(%4.3f)
* Correlación con el producto: 
tabstat cy*_`h' if nivelingreso==3 [fweight=npob], nototal  by(tama) format(%4.3f)
* Autocorrelación serial de primer orden:
tabstat ac*_`h' if nivelingreso==3 [fweight=npob], nototal  by(tama) format(%4.3f) 
}

/* -----------------------------------------------------------------------------
                                V. Gráficos
----------------------------------------------------------------------------- */
use processeddata, clear

* Graficamos los países ricos:
* ----------------------------

levelsof countryname if countrysample==1 & nivelingreso==3, local(idpais)
  foreach j of local idpais {
 
 * Generamos la macro global país para guardar los gráficos. No usamos `j' porque 
 * el compilador de latex tiene dificultades con las tildes y los espacios en los nombres de archivo.
 levelsof countrycode if countrysample==1 & countryname=="`j'" , local(code)
 global pais `code'
 
 * Graficamos las tendencias:
 * --------------------------
 
 tsline y_trl y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia Log-Lineal") ylabel(, nogrid) ///
 name(LL, replace) lpattern("dash") legend(off) 
 
 tsline y_trq y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia Log-Cuadrática") ylabel(, nogrid) ///
 name(LQ, replace) lpattern("dash") legend(off) 
 
 tsline y_trhp y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia HP (lambda=100)") ylabel(, nogrid) ///
 name(HP100, replace) lpattern("dash") legend(off) 
 
 tsline y_trhpi y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia HP (lambda=6.25)") ylabel(, nogrid) ///
 name(HP625, replace) lpattern("dash") legend(off) 
 
 graph combine LL LQ HP100 HP625,  title("`j'") ///
 subtitle("Logaritmo natural del PBI per cápita en modena local", size(small) ///
 color(dknavy)) t2title("Serie y Tendencia", size(small)) graphregion(color(white)) /// 
 note("Tendencia: Línea azul punteada") 
 graph export "$dir\Graficos\Ricos\serie$pais.eps", as(eps) replace  
 
  * Graficamos también todos los componentes cíclicos:
 * --------------------------------------------------

 tsline y_cl y_cq y_chp y_chpi if countryname=="`j'", title("`j'") ///
 subtitle("Logaritmo natural del PBI per cápita en moneda local", size(small) color(dknavy)) ///
 t2title("Componente Cíclico", size(small)) ylabel(, nogrid) legend( size(small) ///
 label(1 "Tendencia Log-Lineal") label(2 "Tendencia Log-Cuadrática") ///
 label(3 "Tendencia HP (lambda=100)") label(4 "Tendencia HP (lambda=6.25)")) ///
 yline(0, lstyle(grid)) graphregion(color(white))
 graph export "$dir\Graficos\Ricos\line$pais.eps", as(eps) replace  
 
}

 levelsof countryname if countrysample==1, local(idpais)
 
* Graficamos los países emergentes:
* ---------------------------------

levelsof countryname if countrysample==1 & nivelingreso==2, local(idpais)
  foreach j of local idpais {
 
 * Generamos la macro global país para guardar los gráficos. No usamos `j' porque 
 * el compilador de latex tiene dificultades con las tildes y los espacios en los nombres de archivo.
 
 levelsof countrycode if countrysample==1 & countryname=="`j'" , local(code)
 global pais `code'
  
 * Graficamos las tendencias:
 * --------------------------
 
 tsline y_trl y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia Log-Lineal") ylabel(, nogrid) ///
 name(LL, replace) lpattern("dash") legend(off) 
 
 tsline y_trq y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia Log-Cuadrática") ylabel(, nogrid) ///
 name(LQ, replace) lpattern("dash") legend(off) 
 
 tsline y_trhp y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia HP (lambda=100)") ylabel(, nogrid) ///
 name(HP100, replace) lpattern("dash") legend(off) 
 
 tsline y_trhpi y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia HP (lambda=6.25)") ylabel(, nogrid) ///
 name(HP625, replace) lpattern("dash") legend(off) 
 
 graph combine LL LQ HP100 HP625,  title("`j'") ///
 subtitle("Logaritmo natural del PBI per cápita en modena local", size(small) ///
 color(dknavy)) t2title("Serie y Tendencia", size(small)) graphregion(color(white)) /// 
 note("Tendencia: Línea azul punteada") 
 graph export "$dir\Graficos\Emergentes\serie$pais.eps", as(eps) replace  
 
  * Graficamos también todos los componentes cíclicos:
 * --------------------------------------------------

 tsline y_cl y_cq y_chp y_chpi if countryname=="`j'", title("`j'") ///
 subtitle("Logaritmo natural del PBI per cápita en moneda local", size(small) color(dknavy)) ///
 t2title("Componente Cíclico", size(small))  ylabel(, nogrid) ///
 legend( size(small) label(1 "Tendencia Log-Lineal") label(2 "Tendencia Log-Cuadrática") ///
 label(3 "Tendencia HP (lambda=100)") label(4 "Tendencia HP (lambda=6.25)")) ///
 yline(0, lstyle(grid)) graphregion(color(white))
 graph export "$dir\Graficos\Emergentes\line$pais.eps", as(eps) replace  
 
}

* Graficamos los países pobres:
* -----------------------------

levelsof countryname if countrysample==1 & nivelingreso==1, local(idpais)
  foreach j of local idpais {

  * Generamos la macro global país para guardar los gráficos. No usamos `j' porque 
 * el compilador de latex tiene dificultades con las tildes y los espacios en los nombres de archivo.
 
 levelsof countrycode if countrysample==1 & countryname=="`j'" , local(code)
 global pais `code'

 * Graficamos las tendencias:
 * --------------------------
 
 tsline y_trl y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia Log-Lineal") ylabel(, nogrid) ///
 name(LL, replace) lpattern("dash") legend(off) 
 
 tsline y_trq y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia Log-Cuadrática") ylabel(, nogrid) ///
 name(LQ, replace) lpattern("dash") legend(off) 
 
 tsline y_trhp y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia HP (lambda=100)") ylabel(, nogrid) ///
 name(HP100, replace) lpattern("dash") legend(off) 
 
 tsline y_trhpi y if countryname=="`j'", graphregion(color(white)) ///
 subtitle("Tendencia HP (lambda=6.25)") ylabel(, nogrid) ///
 name(HP625, replace) lpattern("dash") legend(off) 

 graph combine LL LQ HP100 HP625,  title("`j'") ///
 subtitle("Logaritmo natural del PBI per cápita en modena local", size(small) ///
 color(dknavy)) t2title("Serie y Tendencia", size(small)) graphregion(color(white)) /// 
 note("Tendencia: Línea azul punteada") 
 graph export "$dir\Graficos\Pobres\serie$pais.eps", as(eps) replace  
 
 * Graficamos también todos los componentes cíclicos:
 * --------------------------------------------------

 tsline y_cl y_cq y_chp y_chpi if countryname=="`j'", title("`j'") ///
 subtitle("Logaritmo natural del PBI per cápita en moneda local", size(small) color(dknavy)) ///
 t2title("Componente Cíclico", size(small)) ylabel(, nogrid) ///
 legend( size(small) label(1 "Tendencia Log-Lineal") label(2 "Tendencia Log-Cuadrática") ///
 label(3 "Tendencia HP (lambda=100)") label(4 "Tendencia HP (lambda=6.25)")) ///
 yline(0, lstyle(grid)) graphregion(color(white))
 graph export "$dir\Graficos\Pobres\line$pais.eps", as(eps) replace  
 
}


