
use "$GitHub\labs\okwaayeli\data-raw\releases\harmonized_data\harmonized_ag_services_data",clear
merg 1:m Surveyx EaId using "$GitHub\labs\okwaayeli\data-raw\releases\harmonized_data\harmonized_crop_farmer_data"
keep if _merge==3
drop _merge EduWhyNo 
keep if inlist(Surveyx,"GLSS5","GLSS6","GLSS7")
tab ag_services, gen(ag_services)
gen extension0 = extension  > 2
tab extension_compliance,gen(compliance)
tab extension,gen(extensionCat)
compress
saveold "$GitHub\labs\okwaayeli\replications\ag_services\output\tech_inefficiency_ag_services_data",replace ver(12)

use "$GitHub\labs\okwaayeli\replications\ag_services\output\tech_inefficiency_ag_services_data",clear
tab Surveyx
decode CropID,gen(CropIDx)
keep if CropIDx == "Pooled"
qui levelsof CropIDx, local(levels)
tab services0

qui foreach disag in services0 farm_association community_cooperative extension0{
	
mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries DATA

use "$GitHub\labs\okwaayeli\replications\ag_services\output\tech_inefficiency_ag_services_data",clear
decode CropID,gen(CropIDx)
qui levelsof CropIDx, local(levels)

qui foreach crop in `levels'{
  
*loc crop "Pooled"
use "$GitHub\labs\okwaayeli\replications\ag_services\output\tech_inefficiency_ag_services_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "`crop'"
gen disagCat = `disag'
cap{

sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)

mat Means=J(1,8,.)
qui foreach Var of var Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt AgeYr YerEdu HHSizeAE Depend CrpMix extension_distance community_tractors{
preserve
cap{
*loc Var Yield
reg `Var' c.Trend##i.disagCat, vce(cluster Clust) 
qui est store Model
testparm i.disagCat						//mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.disagCat			//trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins disagCat, eydx(Trend) grand coefl post
nlcom ("Trend_disagCat0":_b[Trend:0bn.disagCat]*100) ("Trend_disagCat1":_b[Trend:1.disagCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(disagCat) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_disagCat0 Trend_disagCat1 Trend_Pooled CATDif TrendDif Mean_disagCat0 Mean_disagCat1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(disagCat) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_disagCat0 `sx'_disagCat1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

qui foreach Var of var Female EqipMech Credit OwnLnd EqipIrig Extension{
preserve
cap{
*Overall and regional means 
qui logit `Var' c.Trend##i.disagCat, vce(cluster Clust) 
qui est store Model
testparm i.disagCat						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.disagCat			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins disagCat, eydx(Trend) grand coefl post
nlcom ("Trend_disagCat0":_b[Trend:0bn.disagCat]*100) ("Trend_disagCat1":_b[Trend:1.disagCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(disagCat) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_disagCat0 Trend_disagCat1 Trend_Pooled CATDif TrendDif Mean_disagCat0 Mean_disagCat1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(disagCat) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_disagCat0 `sx'_disagCat1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

tab disagCat,gen(disagCat)
ren (disagCat1 disagCat2) (disagCat0 disagCat1)

qui foreach Var of var disagCat0 disagCat1{
	cap{
	logit `Var' Trend, vce(cluster Clust) 
	margins, eydx(Trend) grand coefl post
	nlcom ("Trend_`Var'":_b[Trend]*100), post
	qui ereturn display
	mat A = r(table)'
	mat A = A[1...,1..8]

	tabstat `Var', stat(mean sem min max sd n) save
	foreach mt in StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = Trend_`Var' Mean_`Var'
	mat roweq A= Female
	mat li A
	mat Means = A\Means

	mat drop A

	qui levelsof Surveyx, local(SurveyList)
	foreach sx in `SurveyList'{
		mat A = J(1,8,.)
		tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) save
		foreach mt in StatTotal{
			mat B = r(`mt')'
			mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
			mat A =A\B
			mat drop B
		}
		mat rownames A = `sx'_miss `sx'_Pooled
		mat roweq A= Female
		mat Means = A\Means	
		mat drop A
	}
}
}

mat colnames Means = Beta SE Tv Pv Min Max SD N
/*
qui putexcel set "Results\Farmer_Age_Productivity_Ghana_Results.xlsx", sheet(Means) modify
qui putexcel A1=matrix(Means),names
mat li Means
*/
qui clear
qui svmat Means, names(col)
qui gen Coef=""
qui gen Equ=""
local Coef : rownames Means
local Equ  : roweq Means
			
qui forvalues i=1/`: word count `Coef'' {
replace Coef =`"`: word `i' of `Coef''"' in `i'
replace Equ  =`"`: word `i' of `Equ''"'  in `i'
}
qui gen CropIDx= "`crop'"
mat drop Means
if `ApID0' > 0 append using `Summaries'
save `Summaries', replace
loc ApID0=`ApID0'+1
}
}		

use `Summaries', clear

export excel CropIDx Equ Coef Beta SE Tv Pv Min Max SD N /*
*/ using "$GitHub\labs\okwaayeli\replications\ag_services\output\ag_services_results-summary-statistics.xlsx", /*
*/ sheet("Means_`disag'") sheetmodify firstrow(variables) 

}

mat drop _all
sca drop _all
use "$GitHub\labs\okwaayeli\replications\ag_services\output\tech_inefficiency_ag_services_data",clear
decode CropID,gen(CropIDx)

loc xlist services0 farm_association community_cooperative extension0 /*
*/ ag_services1 ag_services2 ag_services3 ag_services4 ag_services5 ag_services6 ag_services7 ag_services8 /*
*/ extension_agency_mofa extension_agency_ngo extension_agency_coop  /*
*/ services_ext_planting services_ext_mechanization services_ext_credit  /*
*/ services_ext_irrigation services_ext_husbandry services_ext_agchemicals services_ext_post_harvest  /*
*/ services_asso_employment services_asso_credit services_asso_equipment services_asso_inputs  /*
*/ services_asso_marketing services_asso_records services_asso_labour /*
*/ compliance1 compliance2 compliance3 compliance4 /*
*/ extensionCat1 extensionCat2 extensionCat3 extensionCat4 extensionCat5 extensionCat6 extensionCat7

tabstat `xlist' if CropIDx == "Cassava",by(Surveyx) save
sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)
mat Means=J(1,8,.)

qui foreach Var in `xlist'{
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			*loc Var extraction_any
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			
			qui levelsof Surveyx, local(levels)
			loc usxx `=r(r)'
			*Overall and regional means 
			qui logit `Var' Trend, vce(cluster Clust) 
			margins, eydx(Trend) grand coefl post predict(pr)
			nlcom ("Trend":(_b[Trend])*100), post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1...,1..8]
			if `=r(r)' == 1 mat A = J(1,8,.)
			tabstat `Var' , stat(mean sem min max sd n) by(Surveyx) save
			foreach mt in StatTotal{
				mat B = r(`mt')'
				mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
				mat A =A\B
				mat drop B
			}
			mat rownames A = "`crop'_Trend" "`crop'_GLSS0"
			mat roweq A= `Var'
			mat Means = A\Means	
			mat drop A
		}
		restore
	}
}

mat li Means

mat colnames Means = Beta SE Tv Pv Min Max SD N

qui clear
qui svmat Means, names(col)
qui gen Coef=""
qui gen Variable=""
local Coef : rownames Means
local Variable  : roweq Means
			
qui forvalues i=1/`: word count `Coef'' {
replace Coef =`"`: word `i' of `Coef''"' in `i'
replace Variable  =`"`: word `i' of `Variable''"'  in `i'
}

split Coef, p("_") limit(2)
ren (Coef1 Coef2) (crop mesure)
keep Variable crop mesure Beta SE Tv Pv Min Max SD N
order Variable crop mesure Beta SE Tv Pv Min Max SD N

export excel Variable crop mesure Beta SE Tv Pv Min Max SD N /*
*/ using "$GitHub\labs\okwaayeli\replications\ag_services\output\ag_services_results.xlsx", /*
*/ sheet("services") sheetmodify firstrow(variables) 
