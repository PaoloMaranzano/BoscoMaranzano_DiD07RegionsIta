      //PROJECT: DID estimations of 2007 income tax treatment//   
		       //on Italian growth related variables //
		            // using regional level data//
					 
//Data set: Always control for the directory where the file name.dta is resident//
use "H:\Il mio Drive\RedditiComuniItaliani\Papers_PaoloBruno\Regioni\EmpEco_r0\Regions_DID_data_prov_Aug2024.dta"
xtset province_code time

describe
summarize

summarize PROV_rate_of_employment15_64 PROV_GDP_PPS PROV_GDP_GROWTH_RATE REG_GDP REG_GDP_GWROTH_RATE REG_GROSS_FIXED_CAPITAL_Euros

//RESPONSE VARIABLE OF THE PROJECT: RATE OF E,MPLOYMENtT//
	  
//Optical Inferenbce: Visualizing parallel trend/

preserve
collapse (mean) PROV_rate_of_employment15_64, by(province_treated time)
reshape wide PROV_rate_of_employment15_64 , i(time) j(province_treated)
graph twoway connect PROV_rate_of_employment15_64* time if time < 2007
restore

preserve
collapse (mean) PROV_rate_of_employment15_64, by(province_treated time)
reshape wide PROV_rate_of_employment15_64 , i(time) j(province_treated)
graph twoway connect PROV_rate_of_employment15_64* time if time <= 2012
restore
        
 preserve
collapse (mean) PROV_rate_of_employment15_64, by(province_treated time)
reshape wide PROV_rate_of_employment15_64 , i(time) j(province_treated)
graph twoway connect PROV_rate_of_employment15_64* time 
restore

 preserve
 keep if time < 2010
collapse (mean) PROV_rate_of_employment15_64, by(province_treated time)
reshape wide PROV_rate_of_employment15_64 , i(time) j(province_treated)
graph twoway connect PROV_rate_of_employment15_64* time 
restore
  
  //More tests//
  
  xtreg PROV_rate_of_employment15_64 i.province_code##c.time if time < 2007
margins province_code, dydx(time)
  
  //Difficult to interpret//
  
  
  //a) SOME SIMPLE PLOTS//

twoway connected  PROV_rate_of_employment15_64   time  if  province_code <=103, connect(L)
twoway connected  PROV_rate_of_employment15_64   time  if  province_treated == 1, connect(L)
twoway connected  PROV_rate_of_employment15_64   time  if  province_treated == 0, connect(L)
twoway connected  MEAN_RATE_EMPLOY   time  if  province_code <=103, connect(L)

graph box PROV_rate_of_employment15_64, by(TREATMENT  )
graph box PROV_rate_of_employment15_64, by(province_treated  time_treatment ) 
//part of Fig 1//

graph box PROV_rate_of_employment15_64, by(   area_name) 

preserve
keep if time <=2006
graph box PROV_rate_of_employment15_64, by(province_treated  )
restore

preserve
keep if time >= 2007
graph box PROV_rate_of_employment15_64, by(province_treated  )
restore

twoway (kdensity PROV_rate_of_employment15_64 if province_treated == 1 & time <= 2006) (kdensity PROV_rate_of_employment15_64 if province_treated  == 1 & time > 2006) (kdensity PROV_rate_of_employment15_64 if province_treated == 0 & time <= 2006) (kdensity PROV_rate_of_employment15_64 if province_treated  == 0 & time > 2006)

//part of Fig1//


//b) UNITROOT TEST//

xtline PROV_rate_of_employment15_64 , i(province_name) t(time) 									
xtunitroot llc PROV_rate_of_employment15_64 , trend

 //No unit roots in the panel/. Adj t = .8.5 (P-value = 0.0000). Reject Ho of unit root  in favor of the alternative that Rate of Employment is stationary //
 
//AI FINI DEL CONTROLLO GRAFICO DEL PARALLEL TREND BISOGNEREBBE CALCOLARE LE MEDIE ANNUALI PER OGNUNO DEI 17 ANNI del  "sotto"data set DELLE PROVINCE TREATED (prima serie di 17 numeri) E QUELLO DELLE PROVINCE UNTREATED (seconda serie di 17 numeri) E VEDERE IL PLOT DELLE DUE SERIE ASSIEME, DATO L'ANNO 2006 (ULTIMO ANNO PRE TREATMENT) E CONFRONTARE TUTTO CON MEAN_RATE_EMPLOY CHE è LA MEDIA NAZIONALE, TRATTATI E NON. quest'ultima sono riuscito a capire come farla con Stata le due separate no//
//E' importante farlo perchè con modelli mixed Stata non esegue estat trendplots//

 //c: CLUB CONVERGENCE//

logtreg PROV_rate_of_employment15_64, kq(0.3)
 psecta PROV_rate_of_employment15_64, name (province_name) kq(0.3) gen(CLUB_1)
 matrix b=e(bm)
 matrix t=e(tm)
 matrix result1=(b\t)
 matlist result1, border(rows) rowtitle("log(t)") 
 scheckmerge PROV_rate_of_employment15_64,kq(0.333) club(CLUB_1) mdiv
 matrix b=e(bm)
 matrix t=e(tm)
 matrix results2 =(b\t)
 matlist results2, border(rows) rowtitle("log(t)") format(%9.3f) left(4)
 imergeclub PROV_rate_of_employment15_64 , name (province_name) kq(0.333) club(CLUB_1) gen(Final_Club_1) 
 
  // Because the value of the t statistic (calculated as −20.10) is less than −1.65, the null hypothesis of overall convergence is rejected at the 5% level.//
 //11 clubs are estimated. There is no uncoverging province//
  //Each time you run the logtreg recall to eliminate previous CLUB_1 and Final_Club_1, or change names//
 //Club convergence generates 11 clubs passing the t-stat test of P-S. More test and interpretation//
 


 
//***** ONE LEVEL DID TAX MODELS *****//
** Start with a simple ONE LEVEL DID of the effect of income taxes on the rate of employment (male and female, age 15-64, NUTS-3 data). Basically, a TWFE with common treatment period. Eq (4) in the text is defined only by the fixed part//

//Version 1: No cofactors (MOD1)//
xtdidregress ( PROV_rate_of_employment15_64) ( TREATMENT), group( province_code) time(time) aeq
estat trendplots
estat trendplots,omeans
estat trendplots,ltrends
estat ptrends //Fig. 1//
estat granger
estat grangerplot
estat grangerplot, verbose nodraw nleads(1) nlags(0) 
estat vce
estimates store MOD1
//Instrumentally to obtain better plots//
xthdidregress ra ( PROV_rate_of_employment15_64 ) (TREATMENT), group(province_code)  aeq
estat atetplot, sci
estat aggregation, time graph
estat aggregation, dynamic graph //For the lower plot of Fig 1//

 
//Version 2: Provincial cofactors (MOD2)//
//Use Real Provincial GDP and Regional GDP//
xtdidregress ( PROV_rate_of_employment15_64 PROV_GDP_PPS ) ( TREATMENT), group( province_code) time(time) aeq
estat trendplots
estat trendplots,omeans
estat trendplots,ltrends
estat ptrends
estat granger
estat grangerplot
estat grangerplot, verbose nodraw nleads(1) nlags(0)
estat vce
estimates store MOD2

//Version 3. Provincial and Regional cofactors: Real Provincial GDP and Regional GDP + Regional Growth Rate (MOD3)//
xtdidregress ( PROV_rate_of_employment15_64 PROV_GDP_PPS REG_GDP REG_GDP_GWROTH_RATE ) ( TREATMENT), group( province_code) time(time) aeq
estat trendplots
estat trendplots,omeans
estat trendplots,ltrends
estat ptrends
estat granger
estat grangerplot
estat grangerplot, verbose nodraw nleads(1) nlags(0)
estat vce
estimates store MOD3


//Version4//
//Use Real Provincial GDP and Regional GDP + Regional Growth Rate + Mean Employment Rate (a Mundlak variable) (MOD4)//
xtdidregress ( PROV_rate_of_employment15_64 PROV_GDP_PPS REG_GDP REG_GDP_GWROTH_RATE  MEAN_RATE_EMPLOY) (TREATMENT), group( province_code) time(time) aeq
estat trendplots
estat trendplots,omeans
estat trendplots,ltrends
estat ptrends
estat granger
estat grangerplot
estat vce
estimates store MOD4

estimates table MOD1 MOD2  MOD3 MOD4, star(0.01 0.05 0.1)

//ATET (a negative impact of taxes on employment) is statistically significant in both versions. Parallel trends ok, and no-anticipation Ho hypotheses cannot be rejected. Time effects are important.//

//Policy: Tax treatment has a negative statistically significant effect on the employment rate//

//DID estimations of the tax effect on provincial GDP Growth is almost zero and almost not statistically significant. Not surprisingly, regional GDP coefficient is positive and significative. Mind the scale (thousands). coefficient is 5.19*e^-10= 0.000235626. Then We tried various DID versions with and without provincial and regional cofactors. TAB 1 paper and No new estimations//

//Treatment (income tax increase) always statistically significative and negative, even after Mundlak like correction. Absolute value of estimated coefficient is smaller when cofactors are present. Regional Growth not statistically significative. Simultaneity between TREATMENTand cofactors?// 

