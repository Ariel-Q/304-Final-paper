************
* SCRIPT: create_mortality_derived.do
* PURPOSE: Create derived mortality datasets used by the replication package
* NOTE: This script is provided for documentation purposes only
************

* Preamble (unnecessary when executing run.do)
do "$Driving/scripts/programs/_config.do"

************
* Code begins
************

clear
set more off
tempfile seer_data

cap mkdir "$Driving/data/mortality"
cap mkdir "$Driving/data/mortality/derived"

forval x = 1/12 {
	local birmon_scens "`birmon_scens' birmonth`x' birmonth`x'_Female"
}
	
qui foreach scenario in "none" "mda192" "mda_not192" "mda192_Female" "mda_not192_Female" "mda192_Male" "mda_not192_Male" "Male" "Female" "White" "Nonwhite" "WhiteMale" "WhiteFemale" "NonwhiteMale" "NonwhiteFemale" `birmon_scens' {

	* Scenarios suppressed due to confidentiality
	if inlist("`scenario'","White","Nonwhite","WhiteMale","WhiteFemale","NonwhiteMale","NonwhiteFemale")|substr("`scenario'",1,6)=="birmon" continue
		
	* Main data for analysis
	use "$Driving/processed/mortality_mda_combined8314st.dta", clear
	
	gen birmon = "birmonth" + string(mo_brth)
	
	* Subset the data down according to the heterogeneity specification of interest		
	if      "`scenario'"=="Male"      keep if male==1
	else if "`scenario'"=="Female"    keep if male==0
	else if "`scenario'"=="none"      assert !mi(male)
		
	else if "`scenario'"=="mda192"     keep if mda_months==192
	else if "`scenario'"=="mda_not192" keep if mda_months!=192
		
	else if "`scenario'"=="mda192_Male"     keep if mda_months==192 & male==1
	else if "`scenario'"=="mda_not192_Male" keep if mda_months!=192 & male==1
		
	else if "`scenario'"=="mda192_Female"     keep if mda_months==192 & male==0
	else if "`scenario'"=="mda_not192_Female" keep if mda_months!=192 & male==0

	else if "`scenario'"=="White"     keep if white==1
	else if "`scenario'"=="Nonwhite"  keep if white==0
		
	else if "`scenario'"=="WhiteMale"      keep if white==1 & male==1
	else if "`scenario'"=="WhiteFemale"    keep if white==1 & male==0
	else if "`scenario'"=="NonwhiteMale"   keep if white==0 & male==1
	else if "`scenario'"=="NonwhiteFemale" keep if white==0 & male==0
		
	else if substr("`scenario'",1,8)=="birmonth" & strpos("`scenario'","Female") keep if birmon==subinstr("`scenario'","_Female","",1) & male==0
	else                                                                         keep if birmon=="`scenario'"
	
	* Causes of death
	unab outcomes : cod_*
	compress

	* Keep 4 years of data before and after MDA
	keep if inrange(agemo_mda, -48, 47)
		
	* Collapse over age in months relative to MDA
	collapse (sum) `outcomes' pop, by(agemo_mda) fast
	
	* Save the data
	compress
	local output_fn = lower("`scenario'")
	save "$Driving/data/mortality/derived/mortality_`output_fn'.dta", replace
}	

***
** MVA and poisoning mortality data by 4-year bins
***

* Number of years in the bin
local num_yrs = 4
	
qui forval yr = 1983(`num_yrs')2013 {
qui foreach scenario in "Male" "Female" {
		
	use "$Driving/processed/mortality_mda_combined8314st.dta", clear
	keep if inrange(year,`yr',`yr'+`num_yrs'-1)
	if "`scenario'"=="Male"           keep if male==1
	else if "`scenario'"=="Female"    keep if male==0

	* Causes of death
	local outcomes "cod_MVA cod_sa_poisoning"	
	compress

	* Keep 4 years of data before and after MDA
	keep if inrange(agemo_mda, -48, 47)
		
	* Collapse over age in months relative to MDA
	collapse (sum) `outcomes' pop, by(agemo_mda) fast

	* Save the data
	compress
	local output_fn = lower("`scenario'")
	save "$Driving/data/mortality/derived/mortality_`output_fn'_`yr'.dta", replace	
}
}

***
** Suicide and accident mortality data
***
	
qui foreach scenario in "Male" "Female" {
		
	use "$Driving/processed/mortality_mda_combined8314nt.dta", clear
	if "`scenario'"=="Male"           keep if male==1
	else if "`scenario'"=="Female"    keep if male==0

	* Causes of death
	local outcomes "cod_suicide* cod_acct* cod_sa*"	
	compress

	* Keep 4 years of data before and after MDA
	keep if inrange(agemo_mda, -48, 47)
		
	* Collapse over age in months relative to MDA
	collapse (sum) `outcomes' pop, by(agemo_mda) fast

	* Save the data
	compress
	local output_fn = lower("`scenario'")
	save "$Driving/data/mortality/derived/mortality_sa_`output_fn'.dta", replace		
}

***
** Mortality data with ages extended to 10-29
***

* SEER population data
use "$Driving/processed/intermediate/seer_pop1983_2014st.dta", clear

collapse (sum) pop, by(year male age) fast
save "`seer_data'", replace

* Main data for analysis with ages extended to 10-29
use "$Driving/processed/intermediate/cdc_mortality_data83to14st.dta", clear
gen age = floor(agemo/12)

* Group into ages 10-14, 15-19, 20-24, and 25-29
keep if inrange(age,10,29)
gen agegroup=1
replace agegroup=2 if inrange(age, 15, 19)
replace agegroup=3 if inrange(age, 20, 24)
replace agegroup=4 if inrange(age, 25, 29)
assert !mi(agegroup)
	
* Causes of death
local outcomes "cod_any cod_external cod_internal cod_MVA cod_homicide cod_extother cod_sa cod_sa_firearms cod_sa_poisoning cod_sa_poisoning_subst cod_sa_poisoning_gas cod_sa_drowning cod_sa_other"

* Merge on SEER data
* Collapse over gender and age group by year
collapse (sum) `outcomes', by(year male age agegroup) fast
merge 1:1 year male age using "`seer_data'", assert(using match) keep(match) nogenerate
drop age

* Save the data
compress
save "$Driving/data/mortality/derived/mortality_sex_agegroup_8314.dta", replace		



** EOF
