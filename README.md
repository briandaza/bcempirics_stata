# Ciclos Económicos en el Mundo: Evidencia Empírica

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

Los resultados finales los puedes observar en el _"Reporte de resultados (ES).pdf"_
