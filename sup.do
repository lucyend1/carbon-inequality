use panel_euets3,clear
preserve
	qui {
		gen innovation = CCM
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m1, title("CCM")
		
		replace innovation = BUILD
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m2, title("BUILD")
		
		replace innovation = GHG
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m3, title("GHG")
		
		replace innovation = CCM_ICT
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m4, title("CCM_ICT")
		
		replace innovation = ENE
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m5, title("ENE")

		replace innovation = GOODS
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m6, title("GOODS")
		
		replace innovation = TRA 
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m7, title("TRA")
		
		replace innovation = WAT_WASTE
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m8, title("WAT_WASTE")
		
		
	}
	esttab m1 m2 m3 m4 m5 m6 m7 m8 using "TableS3.rtf", replace cells(b(fmt(3) star) se(fmt(3) par)) starlevels(* 0.10 ** 0.05 *** 0.01) stats(r2_p N Control Country Year, fmt(%9.3f %9.0g) label("R\textsuperscript{2}" "Observations"))  keep(EU_ETS_phase_3)  label  ///
	rename(euets3 "EU_ETS_phase_3")
restore

preserve
	qui {
		gen innovation = exp(lnh_ccm)-1
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m1, title("CCM")
		
		replace innovation = exp(lnh_build)-1
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m2, title("BUILD")
		
		replace innovation = exp(lnh_ghg)-1
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m3, title("GHG")
		
		replace innovation = exp(lnh_ccmict)-1
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m4, title("CCM_ICT")
		
		replace innovation = exp(lnh_ene)-1
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m5, title("ENE")

		replace innovation = exp(lnh_goods)-1
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m6, title("GOODS")
		
		replace innovation = exp(lnh_tra)-1 
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m7, title("TRA")
		
		replace innovation = exp(lnh_wat)-1
		ppmlhdfe innovation  euets3 sptinc992j1 gdppg trade, absorb(id year) cl(incomegroup)
		estadd local  Control "Yes"
		estadd local  Country "Yes"
		estadd local  Year "Yes"
		estimate store m8, title("WAT_WASTE")
		
		
	}
	esttab m1 m2 m3 m4 m5 m6 m7 m8 using "TableS4.rtf", replace cells(b(fmt(3) star) se(fmt(3) par)) starlevels(* 0.10 ** 0.05 *** 0.01) stats(r2_p N Control Country Year, fmt(%9.3f %9.0g) label("R\textsuperscript{2}" "Observations"))  keep(EU_ETS_phase_3)  label  ///
	rename(euets3 "EU_ETS_phase_3")
restore