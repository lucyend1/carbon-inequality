
****Table 1 Impact of EU ETS on carbon footprint
use carbon inequality,clear
est clear
foreach x in     lnenfghg999i lnenfcar999i lnenfgho999i lnknfghg999i lnknfcar999i lnknfgho999i{
reghdfe `x'  ETS    sptinc992j1  gpop gpgdp lntrade , absorb(id year) cl(id)
est store dy`x'
}

esttab dy* using "table 1.rtf",replace ar2 compress nogap ///    
					keep(ETS) b(3) se(3)  ///					
					star(* 0.1 ** 0.05 *** 0.01) 
				

****Table 2 Impact of the EU ETS on carbon footprint of different income groups
use carbon inequality,clear			
est clear
foreach x in   lnlpfghg999i1  lnlpfghg999i10 lnlpfghg999imid40 lnlpfghg999ibot50 {
reghdfe `x'  ETS  sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
est store dy`x'
}

esttab dy* using "table 2.rtf",replace ar2 compress nogap ///    
					keep(ETS) b(3) se(3)  ///					
					star(* 0.1 ** 0.05 *** 0.01) ///
				
			
					
****Table 3 Impact of the EU ETS on carbon inequality
use carbon inequality,clear
est clear
foreach x in   sghg1  sghg10 sghg40 sghg50 {
reghdfe `x'  ETS  sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
est store dy`x'
}

esttab dy* using "table 3.rtf",replace ar2 compress nogap ///    
					keep(ETS) b(3) se(3)  ///					
					star(* 0.1 ** 0.05 *** 0.01) 



					
****Fig.2:Event study

**Fig 2.a
use carbon inequality,clear
reghdfe lnlpfghg999i1 year_period1-year_period4 year_period6-year_period11 sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
mat se=e(V)
forvalues i = 1/4{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i', `i'])
}
forvalues i = 6/11{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i'-1, `i'-1])
}
keep coef* se*
duplicates drop
gen id = 1
reshape long coef se, i(id) j(period)
gen high=coef + 1.96 * se
gen low=coef - 1.96 * se
replace period=period-6
gen scax=-1 if period==0
gen scay=0 if period==0
gen blankx=-6 if period==0
gen blanky=0 if period==0
replace blankx=6 if period==1
replace blanky=0 if period==1
tw (rcap low high period, color("80 140 240")) ///
(sca coef period, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca scay scax, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca blanky blankx, msize(medsmall) color(none) msymbol(D)), ///
ytitle("Effect on top1% personal carbon footprint", size(medsmall)) ///
xtitle("Number of years before and after the EU ETS", size(medsmall)) ///
xscale(titlegap(small)) yscale(titlegap(small)) legend(off) ///
ylabel(, format(%9.2f) labsize(medsmall) angle(0) nogrid) ///
xlabel(-5 "<= -5"-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 ">= 5", labsize(medsmall)) xmtick(-5(1)5) ///
yline(0, lcolor(black) lpattern(dash)) ///
xline(0, lcolor(dkorange) lpattern(dash)) ///
text(0.1 1.5 "EU ETS", size(medsmall)) ///
graphregion(color(white)) plotregion(style(outline))
graph export "F:\Effect on top1% personal carbon footprint.png", as(png) name("Graph") replace

**Fig 2.b
use carbon inequality,clear
reghdfe lnlpfghg999i10 year_period1-year_period4 year_period6-year_period11 sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
mat se=e(V)
forvalues i = 1/4{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i', `i'])
}
forvalues i = 6/11{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i'-1, `i'-1])
}
keep coef* se*
duplicates drop
gen id = 1
reshape long coef se, i(id) j(period)
gen high=coef + 1.96 * se
gen low=coef - 1.96 * se
replace period=period-6
gen scax=-1 if period==0
gen scay=0 if period==0
gen blankx=-6 if period==0
gen blanky=0 if period==0
replace blankx=6 if period==1
replace blanky=0 if period==1
tw (rcap low high period, color("80 140 240")) ///
(sca coef period, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca scay scax, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca blanky blankx, msize(medsmall) color(none) msymbol(D)), ///
ytitle("Effect on top10% personal carbon footprint", size(medsmall)) ///
xtitle("Number of years before and after the EU ETS", size(medsmall)) ///
xscale(titlegap(small)) yscale(titlegap(small)) legend(off) ///
ylabel(, format(%9.2f) labsize(medsmall) angle(0) nogrid) ///
xlabel(-5 "<= -5"-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 ">= 5", labsize(medsmall)) xmtick(-5(1)5) ///
yline(0, lcolor(black) lpattern(dash)) ///
xline(0, lcolor(dkorange) lpattern(dash)) ///
text(0.1 1.5 "EU ETS", size(medsmall)) ///
graphregion(color(white)) plotregion(style(outline))
graph export "F:\Effect on top10% personal carbon footprint.png", as(png) name("Graph") replace

**Fig 2.c
use carbon inequality,clear
reghdfe lnlpfghg999imid40 year_period1-year_period4 year_period6-year_period11 sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
mat se=e(V)
forvalues i = 1/4{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i', `i'])
}
forvalues i = 6/11{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i'-1, `i'-1])
}
keep coef* se*
duplicates drop
gen id = 1
reshape long coef se, i(id) j(period)
gen high=coef + 1.96 * se
gen low=coef - 1.96 * se
replace period=period-6
gen scax=-1 if period==0
gen scay=0 if period==0
gen blankx=-6 if period==0
gen blanky=0 if period==0
replace blankx=6 if period==1
replace blanky=0 if period==1
tw (rcap low high period, color("80 140 240")) ///
(sca coef period, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca scay scax, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca blanky blankx, msize(medsmall) color(none) msymbol(D)), ///
ytitle("Effect on middle40% personal carbon footprint", size(medsmall)) ///
xtitle("Number of years before and after the EU ETS", size(medsmall)) ///
xscale(titlegap(small)) yscale(titlegap(small)) legend(off) ///
ylabel(, format(%9.2f) labsize(medsmall) angle(0) nogrid) ///
xlabel(-5 "<= -5"-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 ">= 5", labsize(medsmall)) xmtick(-5(1)5) ///
yline(0, lcolor(black) lpattern(dash)) ///
xline(0, lcolor(dkorange) lpattern(dash)) ///
text(0.1 1.5 "EU ETS", size(medsmall)) ///
graphregion(color(white)) plotregion(style(outline))
graph export "F:\Effect on middle40% personal carbon footprint.png", as(png) name("Graph") replace

**Fig 2.d
use carbon inequality,clear
reghdfe lnlpfghg999ibot50 year_period1-year_period4 year_period6-year_period11 sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
mat se=e(V)
forvalues i = 1/4{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i', `i'])
}
forvalues i = 6/11{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i'-1, `i'-1])
}
keep coef* se*
duplicates drop
gen id = 1
reshape long coef se, i(id) j(period)
gen high=coef + 1.96 * se
gen low=coef - 1.96 * se
replace period=period-6
gen scax=-1 if period==0
gen scay=0 if period==0
gen blankx=-6 if period==0
gen blanky=0 if period==0
replace blankx=6 if period==1
replace blanky=0 if period==1
tw (rcap low high period, color("80 140 240")) ///
(sca coef period, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca scay scax, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca blanky blankx, msize(medsmall) color(none) msymbol(D)), ///
ytitle("Effect on bottom50% personal carbon footprint", size(medsmall)) ///
xtitle("Number of years before and after the EU ETS", size(medsmall)) ///
xscale(titlegap(small)) yscale(titlegap(small)) legend(off) ///
ylabel(, format(%9.2f) labsize(medsmall) angle(0) nogrid) ///
xlabel(-5 "<= -5"-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 ">= 5", labsize(medsmall)) xmtick(-5(1)5) ///
yline(0, lcolor(black) lpattern(dash)) ///
xline(0, lcolor(dkorange) lpattern(dash)) ///
text(0.1 1.5 "EU ETS", size(medsmall)) ///
graphregion(color(white)) plotregion(style(outline))
graph export "F:\Effect on bottom50% personal carbon footprint.png", as(png) name("Graph") replace

**Fig 2.e
use carbon inequality,clear
reghdfe sghg1 year_period1-year_period4 year_period6-year_period11 sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
mat se=e(V)
forvalues i = 1/4{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i', `i'])
}
forvalues i = 6/11{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i'-1, `i'-1])
}
keep coef* se*
duplicates drop
gen id = 1
reshape long coef se, i(id) j(period)
gen high=coef + 1.96 * se
gen low=coef - 1.96 * se
replace period=period-6
gen scax=-1 if period==0
gen scay=0 if period==0
gen blankx=-6 if period==0
gen blanky=0 if period==0
replace blankx=6 if period==1
replace blanky=0 if period==1
tw (rcap low high period, color("80 140 240")) ///
(sca coef period, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca scay scax, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca blanky blankx, msize(medsmall) color(none) msymbol(D)), ///
ytitle("Effect on the share of top1% carbon footprint", size(medsmall)) ///
xtitle("Number of years before and after the EU ETS", size(medsmall)) ///
xscale(titlegap(small)) yscale(titlegap(small)) legend(off) ///
ylabel(, format(%9.3f) labsize(medsmall) angle(0) nogrid) ///
xlabel(-5 "<= -5"-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 ">= 5", labsize(medsmall)) xmtick(-5(1)5) ///
yline(0, lcolor(black) lpattern(dash)) ///
xline(0, lcolor(dkorange) lpattern(dash)) ///
text(0.0075 1.5 "EU ETS", size(medsmall)) ///
graphregion(color(white)) plotregion(style(outline))
graph export "F:\Effect on the share of top1% carbon footprint.png", as(png) name("Graph") replace

**Fig 2.f
use carbon inequality,clear
reghdfe sghg10 year_period1-year_period4 year_period6-year_period11 sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
mat se=e(V)
forvalues i = 1/4{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i', `i'])
}
forvalues i = 6/11{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i'-1, `i'-1])
}
keep coef* se*
duplicates drop
gen id = 1
reshape long coef se, i(id) j(period)
gen high=coef + 1.96 * se
gen low=coef - 1.96 * se
replace period=period-6
gen scax=-1 if period==0
gen scay=0 if period==0
gen blankx=-6 if period==0
gen blanky=0 if period==0
replace blankx=6 if period==1
replace blanky=0 if period==1
tw (rcap low high period, color("80 140 240")) ///
(sca coef period, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca scay scax, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca blanky blankx, msize(medsmall) color(none) msymbol(D)), ///
ytitle("Effect on the share of top10% carbon footprint", size(medsmall)) ///
xtitle("Number of years before and after the EU ETS", size(medsmall)) ///
xscale(titlegap(small)) yscale(titlegap(small)) legend(off) ///
ylabel(, format(%9.2f) labsize(medsmall) angle(0) nogrid) ///
xlabel(-5 "<= -5"-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 ">= 5", labsize(medsmall)) xmtick(-5(1)5) ///
yline(0, lcolor(black) lpattern(dash)) ///
xline(0, lcolor(dkorange) lpattern(dash)) ///
text(0.01 1.5 "EU ETS", size(medsmall)) ///
graphregion(color(white)) plotregion(style(outline))
graph export "F:\Effect on the share of top10% carbon footprint.png", as(png) name("Graph") replace

**Fig 2.g
use carbon inequality,clear
reghdfe sghg40 year_period1-year_period4 year_period6-year_period11 sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
mat se=e(V)
forvalues i = 1/4{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i', `i'])
}
forvalues i = 6/11{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i'-1, `i'-1])
}
keep coef* se*
duplicates drop
gen id = 1
reshape long coef se, i(id) j(period)
gen high=coef + 1.96 * se
gen low=coef - 1.96 * se
replace period=period-6
gen scax=-1 if period==0
gen scay=0 if period==0
gen blankx=-6 if period==0
gen blanky=0 if period==0
replace blankx=6 if period==1
replace blanky=0 if period==1
tw (rcap low high period, color("80 140 240")) ///
(sca coef period, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca scay scax, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca blanky blankx, msize(medsmall) color(none) msymbol(D)), ///
ytitle("Effect on the share of middle40% carbon footprint", size(medsmall)) ///
xtitle("Number of years before and after the EU ETS", size(medsmall)) ///
xscale(titlegap(small)) yscale(titlegap(small)) legend(off) ///
ylabel(, format(%9.3f) labsize(medsmall) angle(0) nogrid) ///
xlabel(-5 "<= -5"-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 ">= 5", labsize(medsmall)) xmtick(-5(1)5) ///
yline(0, lcolor(black) lpattern(dash)) ///
xline(0, lcolor(dkorange) lpattern(dash)) ///
text(0.005 1.5 "EU ETS", size(medsmall)) ///
graphregion(color(white)) plotregion(style(outline))
graph export "F:\Effect on the share of middle40% carbon footprint.png", as(png) name("Graph") replace

**Fig 2.h
use carbon inequality,clear
reghdfe sghg50 year_period1-year_period4 year_period6-year_period11 sptinc992j1  gpop gpgdp lntrade, absorb(id year) cl(id)
mat se=e(V)
forvalues i = 1/4{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i', `i'])
}
forvalues i = 6/11{
gen coef`i' = _b[year_period`i']
gen se`i' = sqrt(se[`i'-1, `i'-1])
}
keep coef* se*
duplicates drop
gen id = 1
reshape long coef se, i(id) j(period)
gen high=coef + 1.96 * se
gen low=coef - 1.96 * se
replace period=period-6
gen scax=-1 if period==0
gen scay=0 if period==0
gen blankx=-6 if period==0
gen blanky=0 if period==0
replace blankx=6 if period==1
replace blanky=0 if period==1
tw (rcap low high period, color("80 140 240")) ///
(sca coef period, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca scay scax, msize(medsmall) color("80 140 240") msymbol(D)) ///
(sca blanky blankx, msize(medsmall) color(none) msymbol(D)), ///
ytitle("Effect on the share of bottom50% carbon footprint", size(medsmall)) ///
xtitle("Number of years before and after the EU ETS", size(medsmall)) ///
xscale(titlegap(small)) yscale(titlegap(small)) legend(off) ///
ylabel(, format(%9.2f) labsize(medsmall) angle(0) nogrid) ///
xlabel(-5 "<= -5"-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 ">= 5", labsize(medsmall)) xmtick(-5(1)5) ///
yline(0, lcolor(black) lpattern(dash)) ///
xline(0, lcolor(dkorange) lpattern(dash)) ///
text(0.01 1.5 "EU ETS", size(medsmall)) ///
graphregion(color(white)) plotregion(style(outline))
graph export "F:\Effect on the share of bottom50% carbon footprint.png", as(png) name("Graph") replace


****Fig.3:Distribution of estimated coefficients of placebo test

**Fig 3.a
set more off
use ydyl_a,clear
drop if dof==43
save "ydyl_a.dta", replace
forvalues i=1(1)5000 {
use data,clear
save ydyl_data,replace
use ydyl_data,clear
keep countryid year

bysort countryid year: gen temp_code=_n
keep if temp_code==1
drop temp_code
drop if year==1990

save dummy_data,replace
use dummy_data,clear
** create the false dataset
bysort year: keep if _n==1
sample 1, count
keep year
save temp,replace
use dummy_data,clear
merge m:1 year using temp
keep if _merge==3
drop _merge
sample 30, count
keep year countryid
rename year ydyl_year
save ydyl_time,replace

* Generate the false_ydyl_after variable
use ydyl_time
merge 1:m countryid using ydyl_data
gen false_countryid=(_merge==3)
drop _merge
egen tt=mean(ydyl_year)
replace ydyl_year=tt
drop tt
gen false_after=(year-ydyl_year>=0)
gen false_ydyl_after=false_countryid*false_after
save ydyl_data,replace

* Regression

reghdfe lnlpfghg999i1  false_ydyl_after  sptinc992j1  gpop gpgdp lntrade, absorb(countryid year) cl(countryid)

parmest,format (estimate min95 max95 %8.2f p %8.3f) saving("temp.dta", replace)
use "temp.dta", clear
keep if parm=="false_ydyl_after"
append using "ydyl_a.dta"
save "ydyl_a.dta", replace
}
erase "temp.dta"
use "ydyl_a.dta", clear
drop if estimate==.
save "ydyl_a.dta", replace

**Graphs

*Density Plot
dpplot estimate ,graphregion(color(white))  xtitle("Coefficients", size(medsmall)) ytitle("Density Probability", size(medsmall)) color("80 140 240") xline(-0.335) ///
text(2 -0.25 "true effect", size(medsmall)) text(3.5 -0.47 "(a)", size(medsmall))
graph export "F:\fig 3a.png", as(png) name("Graph") replace

**Fig 3.b
set more off
use ydyl_b,clear
drop if dof==43
save "ydyl_b.dta", replace
forvalues i=1(1)5000 {
use data,clear
save ydyl_data,replace
use ydyl_data,clear
keep countryid year

bysort countryid year: gen temp_code=_n
keep if temp_code==1
drop temp_code
drop if year==1990

save dummy_data,replace
use dummy_data,clear
** create the false dataset
bysort year: keep if _n==1
sample 1, count
keep year
save temp,replace
use dummy_data,clear
merge m:1 year using temp
keep if _merge==3
drop _merge
sample 30, count
keep year countryid
rename year ydyl_year
save ydyl_time,replace

* Generate the false_ydyl_after variable
use ydyl_time
merge 1:m countryid using ydyl_data
gen false_countryid=(_merge==3)
drop _merge
egen tt=mean(ydyl_year)
replace ydyl_year=tt
drop tt
gen false_after=(year-ydyl_year>=0)
gen false_ydyl_after=false_countryid*false_after
save ydyl_data,replace

* Regression

reghdfe lnlpfghg999i10  false_ydyl_after  sptinc992j1  gpop gpgdp lntrade, absorb(countryid year) cl(countryid)

parmest,format (estimate min95 max95 %8.2f p %8.3f) saving("temp.dta", replace)
use "temp.dta", clear
keep if parm=="false_ydyl_after"
append using "ydyl_b.dta"
save "ydyl_b.dta", replace
}
erase "temp.dta"
use "ydyl_b.dta", clear
drop if estimate==.
save "ydyl_b.dta", replace

**Graphs

*Density Plot
dpplot estimate ,graphregion(color(white))  xtitle("Coefficients", size(medsmall)) ytitle("Density Probability", size(medsmall)) color("80 140 240") xline(-0.350) ///
text(2 -0.25 "true effect", size(medsmall)) text(3.5 -0.47 "(b)", size(medsmall))
graph export "F:\fig 3b.png", as(png) name("Graph") replace

**Fig 3.c
set more off
use ydyl_c,clear
drop if dof==43
save "ydyl_c.dta", replace
forvalues i=1(1)5000 {
use data,clear
save ydyl_data,replace
use ydyl_data,clear
keep countryid year

bysort countryid year: gen temp_code=_n
keep if temp_code==1
drop temp_code
drop if year==1990

save dummy_data,replace
use dummy_data,clear
** create the false dataset
bysort year: keep if _n==1
sample 1, count
keep year
save temp,replace
use dummy_data,clear
merge m:1 year using temp
keep if _merge==3
drop _merge
sample 30, count
keep year countryid
rename year ydyl_year
save ydyl_time,replace

* Generate the false_ydyl_after variable
use ydyl_time
merge 1:m countryid using ydyl_data
gen false_countryid=(_merge==3)
drop _merge
egen tt=mean(ydyl_year)
replace ydyl_year=tt
drop tt
gen false_after=(year-ydyl_year>=0)
gen false_ydyl_after=false_countryid*false_after
save ydyl_data,replace

* Regression

reghdfe lnlpfghg999imid40  false_ydyl_after  sptinc992j1  gpop gpgdp lntrade, absorb(countryid year) cl(countryid)

parmest,format (estimate min95 max95 %8.2f p %8.3f) saving("temp.dta", replace)
use "temp.dta", clear
keep if parm=="false_ydyl_after"
append using "ydyl_c.dta"
save "ydyl_c.dta", replace
}
erase "temp.dta"
use "ydyl_c.dta", clear
drop if estimate==.
save "ydyl_c.dta", replace

**Graphs

*Density Plot
dpplot estimate ,graphregion(color(white))  xtitle("Coefficients", size(medsmall)) ytitle("Density Probability", size(medsmall)) color("80 140 240") xline(-0.363) ///
text(2 -0.30 "true effect", size(medsmall)) text(3.5 -0.47 "(c)", size(medsmall))
graph export "F:\fig 3c.png", as(png) name("Graph") replace

**Fig 3.d
set more off
use ydyl_d,clear
drop if dof==43
save "ydyl_d.dta", replace
forvalues i=1(1)5000 {
use data,clear
save ydyl_data,replace
use ydyl_data,clear
keep countryid year

bysort countryid year: gen temp_code=_n
keep if temp_code==1
drop temp_code
drop if year==1990

save dummy_data,replace
use dummy_data,clear
** create the false dataset
bysort year: keep if _n==1
sample 1, count
keep year
save temp,replace
use dummy_data,clear
merge m:1 year using temp
keep if _merge==3
drop _merge
sample 30, count
keep year countryid
rename year ydyl_year
save ydyl_time,replace

* Generate the false_ydyl_after variable
use ydyl_time
merge 1:m countryid using ydyl_data
gen false_countryid=(_merge==3)
drop _merge
egen tt=mean(ydyl_year)
replace ydyl_year=tt
drop tt
gen false_after=(year-ydyl_year>=0)
gen false_ydyl_after=false_countryid*false_after
save ydyl_data,replace

* Regression

reghdfe lnlpfghg999ibot50  false_ydyl_after  sptinc992j1  gpop gpgdp lntrade, absorb(countryid year) cl(countryid)

parmest,format (estimate min95 max95 %8.2f p %8.3f) saving("temp.dta", replace)
use "temp.dta", clear
keep if parm=="false_ydyl_after"
append using "ydyl_d.dta"
save "ydyl_d.dta", replace
}
erase "temp.dta"
use "ydyl_d.dta", clear
drop if estimate==.
save "ydyl_d.dta", replace

**Graphs

*Density Plot
dpplot estimate , graphregion(color(white)) xtitle("Coefficients", size(medsmall)) ytitle("Density Probability", size(medsmall)) color("80 140 240") xline(-0.390) ///
text(2 -0.30 "true effect", size(medsmall))  text(3.5 -0.47 "(d)", size(medsmall))
graph export "F:\fig 3d.png", as(png) name("Graph") replace


