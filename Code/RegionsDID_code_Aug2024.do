      //PROJECT: DID estimations of 2007 income tax treatment//   
		       //on Italian growth related variables //
		            // using regional level data//
					 
//Data set: Always control for the directory where the file name.dta is resident//
use "H:\Il mio Drive\RedditiComuniItaliani\Papers_PaoloBruno\Regioni\EmpEco_r0\Regions_DID_data_Aug2024.dta"
xtset Region_Code TIME

// Graphical analysis by region and year //
preserve
collapse (mean) tot_employ, by( Region_Code TIME )
reshape wide tot_employ, i(TIME) j(Region_Code)
graph twoway connect tot_employ* TIME if TIME >=1995
restore

xtreg tot_employ i.Region_Code##c.TIME if TIME <= 2006
margins Region_Code, dydx(TIME)

// The -margins- output will then give you estimated time trends for tot_employ in each group prior to 2006, and you can make a judgment whether they are close enough for practical purposes. If the graph showed parallel curves, but they are not approximately linear, then quantifying it gets more complicated because you have to pick some way to specify the tot_employ vs time relationship in the regression. Frankly, I usually don't bother with this unless forced to by a reviewer, since the specification process can be somewhat arbitrary and represents an unnecessary researcher degree of freedom. Visual assessment of parallel trends from graphs is really OK by itself//





//***** DESCRIPTIVE STATISTICS AND EXPLORATORY ANALYSIS (Table 1) *****//
sum  RegGDPEuros PerCapita_GDP_euros Value_Added_per_Worker_euros GROSS_CAPITAL_Euros tot_employ Self_Employed new_part_iva family_cosumption indirecttaxcollection, detail 

//LINE PLOTS//
xtline PerCapita_GDP_euros, i(Region_Name) t(TIME)
xtline Value_Added_per_Worker_euros, i(Region_Name) t(TIME)
xtline tot_employ, i(Region_Name) t(TIME)
xtline family_cosumption indirecttaxcollection, i(Region_Name) t(TIME)
 
//Plots with two variables//
xtline PerCapita_GDP_euros, i(Region_Name) t(TIME) addplot((line Self_Employed TIME, sort yaxis(2)))
preserve
keep if d1_region == 1 
xtline PerCapita_GDP_euros, i(Region_Name) t(TIME) addplot((line Self_Employed TIME, sort yaxis(2)))
xtline new_part_iva, i(Region_Name) t(TIME) addplot((line Self_Employed TIME, sort yaxis(2)))
restore
 
 
//BOX-PLOTS//
graph box RegGDPEuros, by(d1_region d2_time)
graph box PerCapita_GDP_euros, by(d1_region d2_time)
graph box Value_Added_per_Worker_euros, by(d1_region d2_time)
graph box GROSS_CAPITAL_Euros, by(d1_region d2_time)
graph box tot_employ, by(d1_region d2_time)
graph box new_part_iva, by(d1_region d2_time)
graph box family_cosumption, by(d1_region d2_time)
graph box indirecttaxcollection, by(d1_region d2_time)
graph box RegGDPEuros, by(Area_Name)
graph box PerCapita_GDP_euros, by(Area_Name)
graph box Value_Added_per_Worker_euros, by(Area_Name)
graph box GROSS_CAPITAL_Euros, by(Area_Name)
graph box tot_employ, by(Area_Name)
graph box new_part_iva, by(Area_Name)
graph box family_cosumption, by(Area_Name)
graph box indirecttaxcollection, by(Area_Name)





//***** TIME SERIES PROPERTIES *****//

//PANEL UNIT ROOT TESTS//		  
//Version llc with a common trend option. New_part_iva excluded because unbalanced//
xtunitroot llc RegGDPEuros, trend
xtunitroot llc PerCapita_GDP_euros, trend
xtunitroot llc Value_Added_per_Worker_euros , trend
xtunitroot llc GROSS_CAPITAL_Euros , trend
xtunitroot llc tot_employ , trend
xtunitroot llc family_cosumption , trend
xtunitroot llc indirecttaxcollection, trend

//llc with demean option.Cross-sectional means removed //
xtunitroot llc RegGDPEuros, trend demean
xtunitroot llc PerCapita_GDP_euros, trend demean
xtunitroot llc Value_Added_per_Worker_euros , trend demean
xtunitroot llc GROSS_CAPITAL_Euros , trend demean
xtunitroot llc tot_employ , trend demean
xtunitroot llc family_cosumption , trend demean
xtunitroot llc indirecttaxcollection, trend demean
 
//New VAT Certificates (new_part_iva) has no data from 1995 to 2000. Change time span after controlling for Veneto data (2001 or 2009)//
preserve
keep if TIME >= 2001
xtunitroot llc new_part_iva, trend
xtunitroot llc new_part_iva, trend demean
restore

preserve
keep if TIME <=2007
xtunitroot llc RegGDPEuros, trend demean
xtunitroot llc PerCapita_GDP_euros, trend demean
xtunitroot llc Value_Added_per_Worker_euros , trend demean
xtunitroot llc GROSS_CAPITAL_Euros , trend demean
xtunitroot llc tot_employ , trend demean
xtunitroot llc family_cosumption , trend demean
xtunitroot llc indirecttaxcollection, trend demean

xtunitroot ht RegGDPEuros, trend demean
xtunitroot ht PerCapita_GDP_euros, trend demean
xtunitroot ht Value_Added_per_Worker_euros , trend demean
xtunitroot ht GROSS_CAPITAL_Euros , trend demean
xtunitroot ht tot_employ , trend demean 
xtunitroot ht family_cosumption , trend demean
xtunitroot ht indirecttaxcollection, trend demean

restore



// Unit Roots  Harris-Tzavalis Version//
** Why H-T? Because the Levin–Lin–Chu test requires that the ratio of the number of panels to time periods tend to zero asymptotically, it is not well suited to datasets with a large number of panels and relatively few time periods. Here we use the Harris–Tzavalis test, which assumes that the number of panels tends to infinity while the number of time periods is fixed//
// H-T with common trend Version//
xtunitroot ht RegGDPEuros, trend 
xtunitroot ht PerCapita_GDP_euros, trend
xtunitroot ht Value_Added_per_Worker_euros , trend
xtunitroot ht GROSS_CAPITAL_Euros , trend
xtunitroot ht tot_employ , trend
xtunitroot ht family_cosumption , trend
xtunitroot ht indirecttaxcollection, trend

//H-T with common trend and demean options//
xtunitroot ht RegGDPEuros, trend demean
xtunitroot ht PerCapita_GDP_euros, trend demean
xtunitroot ht Value_Added_per_Worker_euros , trend demean
xtunitroot ht GROSS_CAPITAL_Euros , trend demean
xtunitroot ht tot_employ , trend demean 
xtunitroot ht family_cosumption , trend demean
xtunitroot ht indirecttaxcollection, trend demean
 
//New VAT Certificates have no data from 1995 to 2000//
preserve 
keep if TIME >= 2001
xtunitroot ht new_part_iva , trend
xtunitroot ht new_part_iva , trend demean
restore

//Pesaran CD test; still on Table 3//
xtcdf RegGDPEuros PerCapita_GDP_euros Value_Added_per_Worker_euros GROSS_CAPITAL_Euros tot_employ Self_Employed new_part_iva family_cosumption indirecttaxcollection





//***** TWFE-DID estimations and parallel trend analysis (Table 2 and Figure 1) *****//
** https://www.statalist.org/forums/forum/general-stata-discussion/general/1629338-reproducing-estat-trendplots-linear-trends-model
graph drop ptrend_selfemp ptrend_newvat ptrend_GDPpc ptrend_logcons ptrend_GDPgrw

xtdidregress (Self_Employed log_K log_Val ) (d3_treatment), group(Region_Code) time(TIME) aeq
estat ptrends
estat granger
estat trendplots, title(Self-Employment) xlabel(#5) ytitle(Units) name(ptrend_selfemp) xtitle(" ") line1opts(lcolor(black)) line2opts(lcolor(black) lpattern(dash))
estat grangerplot, baseline(2006)
estimates store Self_Employment

xtdidregress (new_part_iva log_K log_Val) (d3_treatment), group(Region_Code) time(TIME) aeq
estat ptrends
estat granger
estat trendplots, title(New VAT Certificates) xlabel(#5) ytitle(Units) name(ptrend_newvat) xtitle(" ") notitle line1opts(lcolor(black)) line2opts(lcolor(black) lpattern(dash))
estat grangerplot
estimates store new_part_iva

xtdidregress (g log_K log_Val) (d3_treatment), group(Region_Code) time(TIME) aeq
estat ptrends
estat granger
estat trendplots, title(GDP growth per capita) subtitle(HP filtered) xlabel(#5) ytitle(%) name(ptrend_GDPgrw) xtitle(" ") notitle line1opts(lcolor(black)) line2opts(lcolor(black) lpattern(dash))
estat grangerplot
estimates store pc_Growth

xtdidregress (PerCapita_GDP_euros log_K log_Val Initial_Value_GDP_Euros) (d3_treatment), group(Region_Code) time(TIME) aeq
estat ptrends
estat granger
estat trendplots, title(GDP per capita) xlabel(#5) ytitle(€) name(ptrend_GDPpc) xtitle(" ") notitle line1opts(lcolor(black)) line2opts(lcolor(black) lpattern(dash))
estat grangerplot
estimates store PerCapita_GDP_euros

xtdidregress (Log_Consumption) (d3_treatment), group(Region_Code) time(TIME) aeq
estat ptrends
estat granger
estat trendplots, title(log(Family consumption)) xlabel(#5) ytitle(log(€)) name(ptrend_logcons) xtitle(" ") notitle line1opts(lcolor(black)) line2opts(lcolor(black) lpattern(dash))
* legend(rows(1) pos(6))
estat grangerplot
estimates store log_cons

graph combine ptrend_selfemp ptrend_newvat ptrend_GDPgrw ptrend_GDPpc ptrend_logcons, rows(5)

estimates table Total_Employment pc_Growth HP_pc_Growth  PerCapita_GDP_euros Self_Employment  new_part_iva  , star(0.01 0.05 0.1) 





*************************************************************
********** USE HETEROG only to retrive the plots ************
*************************************************************

// Total employment
xthdidregress ra (tot_employ) (d3_treatment), group(Region_Code)  aeq
estat atetplot, sci
estat aggregation, time graph
estat aggregation, dynamic graph
* Exposition to treatment is not statistically significant over time//
xthdidregress twfe (tot_employ) (d3_treatment), group(Region_Code)  aeq
estat atetplot
estat aggregation, time graph
estat aggregation, dynamic graph
* As above

// Self-employment
xthdidregress ra (Self_Employed log_K log_Val) (d3_treatment), group(Region_Code) aeq
estat atetplot, sci subtitle(Self-Employment) xlabel(#5) ytitle(Ths. units) xtitle(" ") legend(rows(3) position(3)) level(95)
name(atetplot_selfemp)
estat aggregation, time graph
estat aggregation, dynamic graph
* Exposition to treatment is statistically significant over time in the short run only
xthdidregress twfe (Self_Employed log_K log_Val) (d3_treatment), group(Region_Code) aeq
estat atetplot
estat aggregation, time graph
estat aggregation, dynamic graph
* As above
 
// New VAT certificates
xthdidregress ra (new_part_iva log_K log_Val) (d3_treatment), group(Region_Code)  aeq
estat atetplot, sci subtitle(New VAT Certificates) xlabel(#5) ytitle(Units) xtitle(" ") legend(rows(3) position(3)) name(atetplot_newvat)
estat aggregation, time graph
estat aggregation, dynamic graph
* Exposition to treatment is statistically significant over time in both the short and long run
xthdidregress twfe (new_part_iva log_K log_Val ) (d3_treatment), group(Region_Code)  aeq
estat atetplot
estat aggregation, time graph
estat aggregation, dynamic graph
* As above

// GDP Growth per capita (HP filter)
xthdidregress ra (g log_K log_Val) (d3_treatment), group(Region_Code)  aeq
estat atetplot, sci subtitle(GDP growth per capita (HP filtered)) xlabel(#5) ytitle(%) xtitle(" ") legend(rows(3) position(3)) name(atetplot_GDPgrw)
estat aggregation, time graph
estat aggregation, dynamic graph
* Exposition to treatment is never statistically significant over time
xthdidregress twfe (g log_K log_Val ) (d3_treatment), group(Region_Code)  aeq
estat atetplot
estat aggregation, time graph
estat aggregation, dynamic graph
* As above

// GDP per capita
xthdidregress ra (PerCapita_GDP_euros log_K log_Val ) (d3_treatment), group(Region_Code)  aeq
estat atetplot, sci subtitle(GDP per capita) xlabel(#5) ytitle(€) xtitle(" ") legend(rows(3) position(3)) name(atetplot_GDPpc)
estat aggregation, time graph
estat aggregation, dynamic graph
* Exposition to treatment is never statistically significant over time
xthdidregress twfe (PerCapita_GDP_euros log_K log_Val ) (d3_treatment), group(Region_Code)  aeq
estat atetplot
estat aggregation, time graph
estat aggregation, dynamic graph
* As above

// log Consumption
xthdidregress ra (Log_Consumption) (d3_treatment), group(Region_Code)  aeq
estat atetplot, sci subtitle(log(Family consumption)) xlabel(#5) ytitle(log(€)) xtitle(" ") legend(rows(3) position(3)) name(atetplot_logcons)
estat aggregation, time graph
estat aggregation, dynamic graph
* Exposition to treatment is never statistically significant over time
xthdidregress twfe (Log_Consumption) (d3_treatment), group(Region_Code)  aeq
estat atetplot
estat aggregation, time graph
estat aggregation, dynamic graph
* As above

graph combine atetplot_selfemp atetplot_newvat atetplot_GDPgrw atetplot_GDPpc atetplot_logcons, rows(3)
 
 


//********************************//
//***** Convergence analysis *****//
//********************************//

/* Log t test and REGIONAL CLUB analysis for the entire period 1995-2021 and for pre and post tretment periods on the full sample */
 
** VARIABLE = TOTAL EMPLOYMENT
//Total period//
logtreg tot_employ, kq(0.3)
//Pre-Treatment//
preserve 
keep if TIME <= 2006
logtreg tot_employ, kq(0.3)
restore
//Post-Treatment//
preserve 
keep if TIME >= 2007
logtreg tot_employ, kq(0.3)
restore
 
** VARIABLE = SELF-EMPLOYMENT
//Total period//
preserve
keep if TIME >=2003
keep if TIME <=2020
logtreg Self_Employed, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if TIME >=2003
keep if TIME <=2006
logtreg Self_Employed, kq(0.3)
restore
//Post-Treatment//
preserve
keep if TIME >=2007
keep if TIME <=2020
logtreg Self_Employed, kq(0.3)
restore

** VARIABLE = New VAT Certificates
//Total period//
preserve
keep if TIME >= 2001
logtreg new_part_iva, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if TIME >= 2001
keep if TIME <= 2006
logtreg new_part_iva, kq(0.3)
restore
//Post-Treatment//
preserve
keep if TIME >=2007
logtreg new_part_iva, kq(0.3)
restore
 
  
** VARIABLE = p-c regional GDP
//Total period// 
logtreg PerCapita_GDP_euros, kq(0.3)
//Pre-Treatment//
preserve
keep if TIME <= 2006
logtreg PerCapita_GDP_euros, kq(0.3)
restore
//Post-Treatment//
preserve
keep if TIME >= 2007
logtreg PerCapita_GDP_euros, kq(0.3)
restore

** VARIABLE = p-c GDP (HP filter)
//Total period// 
logtreg HP_Log_pc_GDP, kq(0.3)
//Pre-Treatment//
preserve
keep if TIME <=2006
logtreg HP_Log_pc_GDP, kq(0.3)
restore
//Post-Treatment//
preserve
keep if TIME >=2007
logtreg HP_Log_pc_GDP, kq(0.3)
restore
 
** VARIABLE = p-c GDP growth
//Total period// 
preserve
keep if TIME >= 1996
logtreg g, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if TIME <= 2006
keep if TIME >= 1996
logtreg g, kq(0.3)
restore
//Post-Treatment//
preserve
keep if TIME >= 2006
logtreg g, kq(0.3)
restore

** VARIABLE = Family Consumption
//Total period//
logtreg family_cosumption, kq(0.3)
//Pre-Treatment//
preserve
keep if TIME <=2006
logtreg family_cosumption, kq(0.3)
restore
//Post-Treatment//
preserve
keep if TIME >=2007
logtreg family_cosumption, kq(0.3)
restore




/* Log t test and REGIONAL CLUB analysis for the entire period 1995-2021 and for pre and post tretment periods on the untretated regions only */
 
** VARIABLE = TOTAL EMPLOYMENT
//Total period//
preserve
keep if d1_region == 0
logtreg tot_employ, kq(0.3)
restore
//Pre-Treatment//
preserve 
keep if d1_region == 0
keep if TIME <= 2006
logtreg tot_employ, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 0 
keep if TIME >= 2007
logtreg tot_employ, kq(0.3)
restore
 
** VARIABLE = SELF-EMPLOYMENT
//Total period//
preserve
keep if d1_region == 0
keep if TIME >=2003
keep if TIME <=2020
logtreg Self_Employed, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 0
keep if TIME >=2003
keep if TIME <=2006
logtreg Self_Employed, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 0
keep if TIME >=2007
keep if TIME <=2020
logtreg Self_Employed, kq(0.3)
restore

** VARIABLE = New VAT Certificates
//Total period//
preserve
keep if d1_region == 0
keep if TIME >= 2001
logtreg new_part_iva, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 0
keep if TIME >= 2001
keep if TIME <= 2006
logtreg new_part_iva, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 0
keep if TIME >=2007
logtreg new_part_iva, kq(0.3)
restore
 
  
** VARIABLE = p-c regional GDP
//Total period//
preserve
keep if d1_region == 0
logtreg PerCapita_GDP_euros, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 0
keep if TIME <= 2006
logtreg PerCapita_GDP_euros, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 0
keep if TIME >= 2007
logtreg PerCapita_GDP_euros, kq(0.3)
restore

** VARIABLE = p-c GDP (HP filter)
//Total period//
preserve
keep if d1_region == 0
logtreg HP_Log_pc_GDP, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 0
keep if TIME <=2006
logtreg HP_Log_pc_GDP, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 0
keep if TIME >=2007
logtreg HP_Log_pc_GDP, kq(0.3)
restore
 
** VARIABLE = p-c GDP growth
//Total period// 
preserve
keep if d1_region == 0
keep if TIME >= 1996
logtreg g, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 0
keep if TIME <= 2006
keep if TIME >= 1996
logtreg g, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 0
keep if TIME >= 2006
logtreg g, kq(0.3)
restore

** VARIABLE = Family Consumption
//Total period//
preserve
keep if d1_region == 0
logtreg family_cosumption, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 0
keep if TIME <=2006
logtreg family_cosumption, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 0
keep if TIME >=2007
logtreg family_cosumption, kq(0.3)
restore
 
 
 
/* Log t test and REGIONAL CLUB analysis for the entire period 1995-2021 and for pre and post tretment periods on the tretated regions only */
 
** VARIABLE = TOTAL EMPLOYMENT
//Total period//
preserve
keep if d1_region == 1
logtreg tot_employ, kq(0.3)
restore
//Pre-Treatment//
preserve 
keep if d1_region == 1
keep if TIME <= 2006
logtreg tot_employ, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 1 
keep if TIME >= 2007
logtreg tot_employ, kq(0.3)
restore
 
** VARIABLE = SELF-EMPLOYMENT
//Total period//
preserve
keep if d1_region == 1
keep if TIME >=2003
keep if TIME <=2020
logtreg Self_Employed, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 1
keep if TIME >=2003
keep if TIME <=2006
logtreg Self_Employed, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 1
keep if TIME >=2007
keep if TIME <=2020
logtreg Self_Employed, kq(0.3)
restore

** VARIABLE = New VAT Certificates
//Total period//
preserve
keep if d1_region == 1
keep if TIME >= 2001
logtreg new_part_iva, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 1
keep if TIME >= 2001
keep if TIME <= 2006
logtreg new_part_iva, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 1
keep if TIME >=2007
logtreg new_part_iva, kq(0.3)
restore
 
  
** VARIABLE = p-c regional GDP
//Total period//
preserve
keep if d1_region == 1
logtreg PerCapita_GDP_euros, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 1
keep if TIME <= 2006
logtreg PerCapita_GDP_euros, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 1
keep if TIME >= 2007
logtreg PerCapita_GDP_euros, kq(0.3)
restore

** VARIABLE = p-c GDP (HP filter)
//Total period//
preserve
keep if d1_region == 1
logtreg HP_Log_pc_GDP, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 1
keep if TIME <=2006
logtreg HP_Log_pc_GDP, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 1
keep if TIME >=2007
logtreg HP_Log_pc_GDP, kq(0.3)
restore
 
** VARIABLE = p-c GDP growth
//Total period// 
preserve
keep if d1_region == 1
keep if TIME >= 1996
logtreg g, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 1
keep if TIME <= 2006
keep if TIME >= 1996
logtreg g, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 1
keep if TIME >= 2006
logtreg g, kq(0.3)
restore

** VARIABLE = Family Consumption
//Total period//
preserve
keep if d1_region == 1
logtreg family_cosumption, kq(0.3)
restore
//Pre-Treatment//
preserve
keep if d1_region == 1
keep if TIME <=2006
logtreg family_cosumption, kq(0.3)
restore
//Post-Treatment//
preserve
keep if d1_region == 1
keep if TIME >=2007
logtreg family_cosumption, kq(0.3)
restore
  
//In all the above estimations: IF t-test of log(t) test is < -1.65, THEN the null of  convergence of the variable is rejectd at 5% level.//

