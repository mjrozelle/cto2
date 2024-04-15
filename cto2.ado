*! cto2.ado - Stata module to import and minimally clean SurveyCTO data
*! Author: Michael Rozelle <michael.rozelle@wur.nl>
*! Version 0.0.2  Modified:  March 2023

// Drop the cto_read program if it already exists
cap program drop cto2
// Define the cto_read program, which takes three arguments
program define cto2, rclass
// instrument, then dataset, then dofile
syntax, ///
	INSTname(string) /// filepath to the Excel survey instrument
	DATAname(string) /// filepath to the .csv data exported from SurveyCTO
	DOfile(string) /// filepath to the import dofile you want to create
	[RESHAPEfile(string) /// filepath to the reshape dofile you want to create
	IDENTIFIERS(namelist) /// survey unique identifier (usually "key")
	AMERICAN /// use if data were exported in MM/DD/YYYY format, otherwise default to DD/MM/YYYY
	SAVEfolder(string) /// filepath to the folder where all datasets should be saved
	FRGETVARS(namelist) /// variables you want to copy downstream to all reshaped datasets
	DEIDVARS(namelist) /// variables that identify respondents and need to be removed for deidentifying the dataset
	DK(integer 1) /// value used to indicate "don't know" responses
	OTHER(integer 1) /// value used to indicate "other (specify)" responses
	REFUSED(integer 1) /// value used to indicate "refused to answer" responses
	RENAME /// use if you have filled in the "new_name" column and wish to rename variables
	REPLACE /// use to replace existing dofiles
	NUMERIC(namelist) /// use to force numeric variables to be read as numeric
	STRING(namelist)] // use to force string variables to be read as string
	
pause on

version 16

// Create a quiet environment to prevent unnecessary output
qui { 
	
preserve 
local original_frame `c(frame)'

cap confirm file "`dofile'"
if !_rc & "`replace'" == "" {
	
	display as error "file `macval(dofile)' already exists."
	display as error "add option {bf:replace} if you wish to overwrite it."
	exit 602
	
}

cap confirm file "`reshapefile'"
if !_rc & "`replace'" == "" & "`reshapefile'" != "" {
	
	display as error "file `macval(reshapefile)' already exists."
	display as error "add option {bf:replace} if you wish to overwrite it."
	exit 602
	
}

if "`identifiers'" == "" {
	
	local identifying_vars
	
}
else {
	
	local identifying_vars `identifiers'
	
}

if "`american'" == "" {
	
	local datestyle DMY
	
}
else {
	
	local datestyle MDY
	
}

if "`reshapefile'" != "" {
	
	local want_reshape = 1
	
}
else {
	
	local want_reshape = 0
	
}

*===============================================================================
* 	Import XLSforms
*===============================================================================

/* We're going to work in two frames - there's the "survey" sheet containing the 
questions, enablement conditions, and so on, as well as the "choices" sheet,
which gives us all the value labels. Rather than open and close a million datasets,
frames let us work on both these levels simultaneously.
*/

*===============================================================================
* 	The Data
*===============================================================================\

// Create a new frame called "rawdata" to hold the raw data
tempname rawdata
frame create `rawdata' 

// Import the raw data from the CSV file specified by the "dataname" variable
frame `rawdata' {
	
	import delimited "`dataname'", clear bindquote(strict) case(preserve) rowrange(:1)
	compress
	
}

// Create a new frame called "qs" to hold the survey questions
tempname qs
frame create `qs' 
cwf `qs' 

// Import the "survey" sheet from the instrument Excel file specified by the "instname" variable
import excel "`instname'", firstrow clear sheet(survey) 

if "`rename'" != "" local rencheck new_name

// Loop over a list of variables and ensure that they exist in the dataset
foreach v in type name calculation relevant repeat_count `rencheck' {
	cap confirm variable `v'
	cap tostring `v', replace
	if (_rc) {
		local missvars `missvars' `v'
		continue
	}
	else {
		local keepvars `keepvars' `v'
	}
}

cap confirm variable label 

if !_rc {
	
	rename label labelEnglishen
	clonevar labelStata = labelEnglishen
	
}
else {
	
	cap tostring labelStata, replace
	
}

// Keep only the variables that exist in the dataset
keep `keepvars' labelEnglishen labelStata

// Display a warning if any variables are missing
if "`missvars'" != "" {
	noisily display as result "You are possibly missing the variable(s): `missvars'!"
}

// Replace any dollar signs in the "labelEnglishen", "labelStata", "repeat_count", and "relevant" variables with pound signs
foreach v of varlist labelEnglishen labelStata repeat_count relevant {
	replace `v' = subinstr(`v', "$", "#", .)
}

// Replace any missing Stata labels with the English labels
replace labelStata = "" if labelStata=="."
replace labelStata = labelEnglishen if missing(labelStata)

*===============================================================================
* 	Choices
*===============================================================================

// Create a new frame called "choices" to hold the value labels
tempname choices
frame create `choices' 
cwf `choices' 

// Import the "choices" sheet from the instrument Excel file specified by the "instname" variable
import excel "`instname'", firstrow clear sheet(choices)

// Rename the "listname" variable to "list_name" for consistency
cap rename listname list_name 

// Keep only the "list_name", "name", and "label" variables
keep list_name name label 

// Remove any empty rows from the dataset
missings dropobs, force 

// Remove any rows where the "name" variable is not a number (i.e. non-standard labeling)
drop if !regexm(name, "^[0-9\-]+$") 

// Create a new variable called "order" to retain the original order of variables in the instrument
gen order = _n 

// Create a clone of the "name" variable for programming purposes
clonevar name1 = name 

// Set up missing values
if `dk' != 1 replace name1 = ".d" if name == "`dk'"
if `refused' != 1 replace name1 = ".r" if name == "`refused'"
if `other' != 1 replace name1 = ".o" if name == "`other'"

// Replace any minus signs in the "name" variable with underscores, to match how SurveyCTO handles value labels
replace name = subinstr(name, "-", "_", 1) 

// Replace any dollar signs in the "label" variable with pound signs, to prevent Stata from parsing them as macros
replace label = subinstr(label, "$", "#", .)

// Remove any double quotes from the "label" variable
replace label = subinstr(label, `"""', "", .)
replace label = subinstr(label, char(10), "", .)

// Remove any spaces from the "list_name" variable
replace list_name = subinstr(list_name, " ", "", .)

// Create a local macro called "brek" containing a line break character
local brek = char(10) 
local tab = char(9)

// Remove any line breaks from the "name" and "name1" variables
foreach var of varlist name name1 {
	replace `var' = subinstr(`var', "`brek'", "", .)
}

// Loop over the unique values of the "list_name" variable and generate a Stata value label for each one
local counter = 1
levelsof list_name, local(list) clean separate( )
foreach listo in `list' {
	local brek = char(10)
	local cmd "label define `listo' /// `brek'"
	levelsof order if list_name == "`listo'", clean local(values)
	local lvs = `r(r)'
	if ustrregexm("`values'", "[^0-9\s]+") == 1 continue
	else {
		local t = 0
		foreach i in `values' {
			local ++t
			local slash "///"
			if `t' ==`lvs' {
				local brek
				local slash 
			}
			levelsof name1 if order==`i', local(vals) clean
			levelsof label if order==`i', local(label) clean
			local cmd `"`cmd'`tab'`vals' "`label'" `slash'`brek'"'
		}
	local lab`counter' "`cmd'"
	local ++counter 
	}
}

// Count the number of value label locals generated by the loop above
local labels_counter = `counter'-1

*===============================================================================
* 	Some Tidying
*===============================================================================

// Switch back to the "questions" frame
cwf `qs' 

// Create a local macro called "brek" containing a line break character
local brek = char(10) 

// Remove any line breaks, dollar signs, and double quotes from the "labelEnglishen", "labelStata", and "relevant" variables
foreach var of varlist labelEnglishen labelStata relevant {
	replace `var' = subinstr(`var', "`brek'", "", .)
	replace `var' = subinstr(`var', "$", "#", .) 
	replace `var' = subinstr(`var', `"""', "", .)
	/* Stata will intepret dollar signs as globals, and as a result, we won't 
	be able to see the questions being referenced for constraints (or even 
	inside the question itself). By changing the dollar sign to a #, we can 
	still see these references in the variable labels and notes */
}

// Replace any full stops in variable names with an empty string
replace name = subinstr(name, ".", "", .) 

replace type = strtrim(stritrim(type))
replace type = subinstr(type, " ", "_", .) ///
	if inlist(type, "begin group", "end group", "begin repeat", "end repeat")

// Split the "type" variable into two variables, "type" and "type2"
split type 
// Note that "type2" will contain the value label name for categorical variables.

// Create a new variable called "order" to retain the original order of variables in the instrument
gen order = _n

*------------------------------------------------------------------
*	Question Types
*------------------------------------------------------------------

gen preloaded=regexm(calculation, "^pulldata") // If the variable is preloaded, we may want to be alerted to that fact
gen note = type=="note" // Notes are useless to us, good to identify them here

local numeric_formulae index area number round count count-if sum sum-if min min-if max max-if distance-between int abs duration
local regex_stubs: subinstr local numeric_formulae " " "|" , all
local regex_pattern "^(?:`regex_stubs')\("
gen numeric_calculate = ustrregexm(calculation, "`regex_pattern'")

local regex_stubs_2 : subinstr local numeric " " "|", all
local regex_pattern "^`regex_stubs_2'$"
gen numeric_force = ustrregexm(name, "`regex_pattern'")

local regex_stubs_3 : subinstr local string " " "|", all
local regex_pattern "^`regex_stubs_3'$"
gen string_force = ustrregexm(name, "`regex_pattern'")

label define question_type_M 1 "String" 2 "Select One" 3 "Select Multiple" ///
	4 "Numeric" 5 "Date" 6 "Datetime" 7 "GPS" ///
	-111 "Group Boundary" -222 "Note" ///
	-333 "Geopoint" -555 "Other" 
gen question_type=.
label values question_type question_type_M
replace question_type = 1 if inlist(type, "text", "deviceid", "image", "geotrace") ///
	| preloaded==1 | (type == "calculate" & numeric_calculate == 0) | string_force == 1
replace question_type = 2 if word(type, 1) == "select_one" & missing(question_type)
replace question_type = 3 if word(type, 1) == "select_multiple"
replace question_type = 4 if (!inlist(type, "date", "text") & missing(question_type)) | numeric_force == 1
replace question_type = 5 if inlist(type, "date", "today")
replace question_type = 6 if inlist(type, "start", "end", "submissiondate")
replace question_type= 7 if type == "geopoint"
replace question_type = -111 if inlist(type, "begin_group", "end_group", ///
	"begin_repeat", "end_repeat")
replace question_type=-222 if note==1
replace question_type = -333 if type == "text audit"
replace question_type=-555 if missing(question_type)

drop numeric_calculate

/* 
The above tries to assign a question type to every row of the instrument. 
If the question type is negative (see value label) the dofile will just skip 
that variable. In the majority of cases this should be benign, and you'll know
pretty quickly if a variable needs attention.
*/

*===============================================================================
* 	Repeat Groups
*===============================================================================

/* 
These next few loops tell us which groups each question belongs to. This helps
 us tell if a question exists within a loop, or a specific block of questions 
 with an enablement condition, etc.
*/

tempname commands repeat_group_long
frame create `commands'
frame `commands': insobs 1

count if type == "begin_repeat"
local repeat_groups = `r(N)'

if `repeat_groups' > 0 {

	foreach jump in begin end { // identify the begin and end repeat sections

		levelsof order if type=="`jump'_repeat", local(`jump') separate( ) // the rows where those sections are in the instrument
		tokenize "``jump''", parse( ) // create numbered macros containing where those rows are
		forvalues num = 1/`repeat_groups' {
			local `jump'_`num' ``num'' // this will be useful in the next loop
		}
		
	}

	forvalues i = 1/`repeat_groups' {
		local taken_`i' = 0 // this local will indicate whether an "end_repeat" has already been assigned to a specific repeat group
	}

	local counter = 0
	forvalues i = `repeat_groups'(-1)1 { // for each begin_repeat

		local ++counter
		
		// short for "current group"
		local cg = (`repeat_groups' + 1) - `counter'

		forvalues j = `repeat_groups'(-1)1 { // for each end_repeat 
		
			local true = (`end_`j'' > `begin_`i'') & `taken_`j'' == 0 // evaluates to 1 if the end_repeat is after the begin_repeat and hasn't already been taken
			if `true' {
				local k = `j' // potential match. But we will try lower values of repeat blocks to check if there is another end_repeat that is closer
			}
			
		}
		
		local taken_`k' = 1 // we have assigned this end_repeat
		local e_`cg' = `end_`k''
		
	}

	frame create `repeat_group_long' int group int start int end strL group_name ///
		strL group_label strL group_condition int group_type int repetitions ///
		strL function strL repeat_var int repeat_var_pos ///
		int repeat_var_nested strL repeat_count

	tempname grouplabels
	forvalues i = 1/`repeat_groups' {	

		frame post `repeat_group_long' (`i') (`begin_`i'') (`e_`i'') ///
			("`=name[`begin_`i'']'") ("`=labelEnglishen[`begin_`i'']'") ///
			("`=relevant[`begin_`i'']'") (2) (.) ///
			("") ("") (.) ///
			(.) ("`=repeat_count[`begin_`i'']'")
		
		label define `grouplabels' `i' "`=labelEnglishen[`begin_`i'']'", modify
		local gname_`i' = labelEnglishen[`begin_`i'']
		
		frame `commands': gen dataset_`i' = "local shape_`i' "

	}

	frame `repeat_group_long' {
		
		replace function = ustrregexs(1) ///
			if ustrregexm(repeat_count, "^(.*)#\{")
		
		replace repeat_var = ustrregexs(1) ///
			if ustrregexm(repeat_count, "#\{(.*)\}")
			
		levelsof group if !missing(repeat_var), clean local(repvar_groups)
		foreach g in `repvar_groups' {
			
			local r = repeat_var[`g']
			frame `qs': levelsof order if name == "`r'", clean local(rownum)
			replace repeat_var_pos = `rownum' in `g'
			
		}
		
	}

	tempname expanded_repeats expanded_repeats_2
	frame copy `repeat_group_long' `expanded_repeats'
	frame `expanded_repeats' {
		
		gen diff = end - start
		expand diff + 1
		sort group
		bysort group: gen order = start[1] + _n - 1
		drop if inlist(order, end, start)
		
		frame copy `expanded_repeats' `expanded_repeats_2'
		
		replace repeat_var_pos = order
		keep repeat_var_pos group 
		bysort repeat_var_pos (group): gen dum = _n
		reshape wide group, i(repeat_var_pos) j(dum)
		
		findregex, re("^group\d+$")
		local groupvars `s(varlist)'
		
	}

	cwf `repeat_group_long'
		
	frlink m:1 repeat_var_pos, frame(`expanded_repeats')
	frget `groupvars', from(`expanded_repeats')
	replace repeat_var_nested = !missing(`expanded_repeats')
	drop `expanded_repeats'
	
	clonevar repeat_group_name = group_name
	clonevar repeat_group_label = group_label
	clonevar lowest_level = group

	egen nested_levels = anycount(`groupvars'), values(1)
	sort group
	forvalues g = 1/`c(N)' {
		
		local start = start[`g']
		local end = end[`g']
		
		frame `qs' {
			
			tempvar ingroup
			gen `ingroup' = inrange(order, `start', `end') & inlist(question_type, 1, 2, 4, 5, 6)
			sum order if `ingroup'
			local repeat_var = name[`r(min)']
			
		}
			
		frame `rawdata' {
				
			findregex, re("^`repeat_var'[\d_]+") sensitive
			local nested_varlist `s(varlist)'
			local max_num = 0
			foreach var in `nested_varlist' {
				
				local num = real(ustrregexra("`var'", ".*_(\d+)$", "$1"))
				if (`num' > `max_num') local max_num = `num'
				
			}
			
		}
		
		replace repetitions = `max_num' in `g'
		
	}

	frame `expanded_repeats_2' {

		forvalues i = 1/`repeat_groups' {
			
			gen repeat_group_`i' = 1 if group == `i'
			local rvars `rvars' repeat_group_`i'
			
		}
		collapse (max) `rvars' lowest_level = group, by(order)
		
	}

	cwf `qs'
	frlink 1:1 order, frame(`expanded_repeats_2')
	frget `rvars' lowest_level, from(`expanded_repeats_2')
	gen repeated = !missing(`expanded_repeats_2')
	replace lowest_level = 0 if missing(lowest_level)
	drop `expanded_repeats_2'

}
else {
	
	gen repeated = 0
	gen lowest_level = 0
	
}

*------------------------------------------------------------------
*	General Question Blocks
*------------------------------------------------------------------

tempname general_groups
frame create `general_groups' int group int start int end strL group_name ///
	strL group_label strL group_condition

foreach jump in begin end {
	count if type=="`jump'_group"
	local q_groups = `r(N)'
	levelsof order if type=="`jump'_group", local(`jump') separate( )
	tokenize "``jump''", parse( )
	forvalues num = `q_groups'(-1)1{
		local `jump'_`num' ``num''
	}
}

gen group = .
gen conditions = relevant // survey logic for the question

if `q_groups' > 0 {

/* 
Now, we're working on defining the enablement conditions for each question.
This takes into account the conditions for the individual question as well as 
those for the question block the question is in.
*/

tempname grouplabels
forvalues i = 1/`q_groups' {
	
	frame post `general_groups' (`i') (`begin_`i'') (`end_`i'') ///
		("`=name[`begin_`i'']'") ("`=labelEnglishen[`begin_`i'']'") ///
		("`=conditions[`begin_`i'']'")
		
	label define `grouplabels' `i' "`=labelEnglishen[`begin_`i'']'", modify
	local gname_`i' = labelEnglishen[`begin_`i'']

}

tempname groups_long
frame copy `general_groups' `groups_long'
frame `groups_long' {
	
	gen diff = end - start
	expand diff + 1
	sort group
	bysort group: gen order = start[1] + _n - 1
	keep order group group_condition
	bysort order (group): gen dum = _n
	reshape wide group group_condition, i(order) j(dum)
	findregex, re("^group\d+$")
	local gvars `s(varlist)'
	egen total_groups = rowtotal(`gvars')
	egen lowest_group = rowmax(`gvars')
	findregex, re("^group_condition\d+$")
	local convars `s(varlist)'
	local getvars `gvars' `convars'
	
}

frlink 1:1 order, frame(`groups_long')
frget total_groups lowest_group `getvars', from(`groups_long')
drop `groups_long'
label values `gvars' `grouplabels'
forvalues i = 1/`q_groups' {
	
	egen group_`i' = anymatch(`gvars'), values(`i')
	label variable group_`i' "question in group `i'"
	
}

}

egen unlock = concat(conditions `convars'), punct("; ")
replace unlock = strtrim(stritrim(unlock))
replace unlock = ustrregexra(unlock, "(; ; )+", "; ")
replace unlock = ustrregexra(unlock, "^; |^ ; ", "")
replace unlock = ustrregexra(unlock, "; $| ;$", "")
replace unlock = ustrregexra(unlock, "(; ){2,}", ";")
replace unlock = ustrregexra(unlock, "(;){2,}", ";")
replace unlock = ustrregexra(unlock, "^(; )+", "")
replace unlock = ustrregexra(unlock, "(; )+$", "")
replace unlock = "" if unlock == ";"
replace unlock = ustrregexra(unlock, ";+$", "")
replace unlock = ustrregexra(unlock, "; +$", "")

clonevar order_1 = order
sort order 
drop if question_type < 0 | missing(name)
replace order = _n

*===============================================================================
* 	Variables
*===============================================================================

compress
sort order
local brek = char(10)
local tab = char(9)
foreach thing in brek tab {
	
	forvalues z = 2/3 {
	
		local `thing'`z' = `z' * "``thing''"
	
	}
	
}
local hbanner = "*" + ("=")* 65
local lbanner = "*" + ("-") * 65

if `repeat_groups' > 0 {
	
	egen number_repeat_groups = rowtotal(`rvars')
	sort order
	frame `repeat_group_long': sort group
	forvalues j = 1/`repeat_groups' {
		
		frame `repeat_group_long': local repeats_`j' = repetitions[`j']
		
	}
	
}

gen within_order = .
clonevar original_name = name 
clonevar desired_varname = name 
if "`rename'" != "" {
	
	replace desired_varname = new_name if !missing(new_name)
	
}

tempname long_qs
frame copy `qs' `long_qs'

levelsof order if question_type == 3, clean local(mult_selects)
foreach i in `mult_selects' {
	
	local name = desired_varname[`i']
	local vallabel = type2[`i']
	
	frame `choices' {
		
		levelsof order if list_name == "`vallabel'"
		local options `r(levels)'
		local levels = `r(r)'
		local expansion = `levels' + 1
		
	}
	
	frame `long_qs' {
		
		expand `expansion' if order == `i'
		bysort order (within_order): replace within_order = _n if order == `i'
	
		local s = 1
		foreach row in `options' {
			
			local ++s
			frame `choices': local value = name[`row']
			frame `choices': local lab = label[`row']
			frame `qs': local labstat = labelStata[`i']
			
			replace desired_varname = desired_varname + "_" + "`value'" if order == `i' & within_order == `s'
			replace labelStata = "#{`name'}: `lab'" if order == `i' & within_order == `s'
		
		}
		
	}
	
}

levelsof order if question_type == 7, clean local(gps)
foreach i in `gps' {
	
	frame `long_qs' { 
		
		expand 4 if order == `i'
		bysort order (within_order): replace within_order = _n if order == `i'
		
		local s = 0
		foreach f in Accuracy Latitude Longitude Altitude {
			
			local ++s
			replace desired_varname = desired_varname + "`f'" if within_order == `s' & order == `i'
			replace labelStata = labelStata + ": `f'" if within_order == `s' & order == `i'
			
		}
	
	}
	
}

cwf `long_qs'
assert !missing(desired_varname)

// solving problem of duplicate names...
bysort desired_varname (order): keep if _n == 1

sort order within_order 
gen new_order = _n

// storing list of previous varnames
clonevar vlist = desired_varname

// actual varlist in raw data
clonevar varlist = desired_varname

clonevar shapelist = desired_varname
gen shapelist_lag = desired_varname
replace varlist = "" if repeated == 1

if `repeat_groups' > 0 {
	
	forvalues r = 1/`repeat_groups' {
		
		if `repeats_`r'' == 0 {
			
			continue
			
		}
		
		replace shapelist = ustrregexra(shapelist_lag, "(\b\w+\b)", "$1_ ") ///
				if repeat_group_`r' == 1
		
		forvalues i = 1/`repeats_`r'' {
			
			replace varlist = varlist + ustrregexra(vlist, "(\b\w+\b)", "$1_`i' ") ///
				if repeat_group_`r' == 1
			
		}
		
		tempname storage
		frame copy `long_qs' `storage' 
		frame `storage' {
			
			keep if lowest_level == `r'
			keep shapelist new_order 
			gen group = 1
			reshape wide shapelist, i(group) j(new_order)
			unab lvars : shapelist*
			egen macro_1 = concat(`lvars'), punct(" ")
			local shapelist = macro_1[1]
			// remove anything longer than 32 characters
			local shapelist = ustrregexra("`shapelist'", "([a-zA-Z][a-zA-Z0-9_]{31,})", "", .)
			
		}

		frame `commands': replace dataset_`r' = dataset_`r' + stritrim("`shapelist'")
		
		replace vlist = varlist if repeat_group_`r' == 1
		replace shapelist = vlist if repeat_group_`r' == 1
		
		replace shapelist_lag = vlist if repeat_group_`r' == 1
		
		replace varlist = ""
		
	} 

	drop varlist shapelist
	
}

gen num_vars = wordcount(vlist)
gen var_stub = cond(num_vars == 1, vlist, "\`var'")

gen label_command =  "cap label variable " + var_stub + `" ""' + labelStata + `"""'
gen format_command = ""

replace format_command = "cap tostring " + var_stub + ", replace`brek'cap replace " + ///
	var_stub + `" = "" if "' + var_stub + `" == ".""' if ///
	question_type == 1 | (question_type == 3 & within_order == 1)
	
replace format_command = "cap destring " + var_stub + ", replace" ///
	if inlist(question_type, 2, 4, 7) | (question_type == 3 & within_order > 1)

replace format_command = "tempvar date`brek'cap clonevar \`date' = " + var_stub + ///
	"`brek'cap replace " + var_stub + `" = """' + ///
	"`brek'if _rc cap replace " + var_stub + `" = ."' + ///
	"`brek'cap destring " + var_stub + ", replace" + ///
	"`brek'cap replace " + var_stub + `" = date(\`date', "`datestyle'", 2030)"' + ///
	`" if !missing(\`date')"' + ///
	+ `"`brek'cap format "' + var_stub + `" %td"' ///
	if question_type == 5

replace format_command = "tempvar date`brek'cap clonevar \`date' = " + var_stub + ///
	"`brek'cap replace " + var_stub + `" = """' + ///
	"`brek'if _rc cap replace " + var_stub + `" = ."' + ///
	"`brek'cap destring " + var_stub + ", replace" + ///
	"`brek'cap replace " + var_stub + `" = clock(\`date', "`datestyle'hms", 2030)"' + ///
	`" if !missing(\`date')"' + ///
	+ `"`brek'cap format "' + var_stub + `" %tc"' ///
	if question_type == 6
	
assert !missing(format_command)

gen values_command = "cap label values " + var_stub + " " + type2 if !missing(type2) ///
	& !inlist(question_type, 3, 7)

gen notes_command = "cap notes " + var_stub + ": " + labelEnglishen + ///
	"`brek'cap notes " + var_stub + ": relevance conditions: " + unlock
	
gen command = "`hbanner'`brek'*`tab2'" + name + "`brek'`hbanner'`brek2'" 
replace command = command + "cap foreach var of varlist " + vlist + ///
	"{`brek2'cap confirm variable " + var_stub + "`brek'if _rc continue`brek2'" if num_vars > 1
replace command = command + label_command + "`brek'" + format_command + ///
	"`brek'" + values_command + "`brek'" + notes_command
	
if "`rename'" != "" {
	
	gen rename_command = `"`brek'cap rename "' + var_stub + `" \`=ustrregexrf(""' + ///
		var_stub + `"", ""' + original_name + `"", ""' + new_name + `"")'"' ///
		if !missing(new_name)
	
	replace command = command + rename_command
	
}

replace command = command + "`brek2'}" if num_vars > 1

forvalues i = 0(1)`repeat_groups' {
	
	levelsof desired_varname if lowest_level == `i', clean local(vlist_`i')
	frame `commands': gen varlist_`i' = "local varlist_`i' `vlist_`i''"
	
}

*===============================================================================
* 	Open File
*===============================================================================

if "`savefolder'" != "" cap mkdir "`savefolder'"

tempname codebook
frame copy `long_qs' `codebook'
cwf `codebook'
findregex, re("^(repetitions_|function_)\d+")
drop `s(varlist)' name num_vars  label_command format_command ///
	values_command notes_command command var_stub shapelist_lag vlist ///
	calculation order note
	
if `repeat_groups' > 0 {
	
	frlink m:1 lowest_level, frame(`repeat_group_long')
	frget repeat_group_name repeat_group_label, from(`repeat_group_long')
	drop `repeat_group_long'
	label variable repeat_group_name "name of repeat group this question belongs to"
	label variable repeat_group_label "label of repeat group this question belongs to"
	
	local rp_vars repeat_group_name repeat_group_label
	
}
else {
	
	gen repeat_group_name = "survey"
	gen repeat_group_label = "Survey-level"
	
}
	
rename desired_varname name
rename labelStata varlabel
rename labelEnglishen full_question
rename type1 original_type
rename type2 vallabel
rename order_1 instrument_position
rename new_order order
rename lowest_level repeat_group

replace repeat_group_name = "survey" if repeat_group == 0
replace repeat_group_label = "Survey-level" if repeat_group == 0

label variable order "order of appearance in instrument"
label variable within_order "order of appearance for variables generated by single line"
label variable name "variable name"
label variable question_type "question type"
label variable varlabel "variable label"
label variable vallabel "value label"
label variable full_question "full question text in instrument"
label variable repeat_group "lowest-level repeat group to which question belongs"
label variable conditions "full set of relevancy conditions for this question"

cap unab extra_vars : group_* repeat_group_*

order order within_order name question_type preloaded varlabel vallabel full_question ///
	conditions repeated repeat_group `rp_vars' ///
	`extra_vars'
	
cap drop __*

if "`savefolder'" != "" save "`savefolder'/survey_metadata.dta", replace

cwf `long_qs'

// Now we're about to write the instructions to a dofile. Buckle up
cap file close myfile
file open myfile using "`dofile'", write text replace

*===============================================================================
* 	Write Variables
*===============================================================================

local version = max(`c(stata_version)', 16)

file write myfile ///
	"/*" ///
	_n "Title: Import Dofile for `macval(instname)'" ///
	_n "Date Created: `c(current_date)'" ///
	_n "Author: `c(username)'" ///
	_n "Note: " ///
	_n "*/" _n(2) ///
	"`hbanner'" ///
	_n "* 	Setup" _n /// 
	"`hbanner'" ///
	_n(2) "clear all" _n "version `version'" _n "set more off" _n "set maxvar 120000" ///
	_n "cap log close" _n "set trace off" _n "set linesize 200" _n(2) ///
	"`hbanner'" ///
	_n "* 	Macros" _n /// 
	"`hbanner'" ///
	_n(2) ///
	"local" _tab `"today = date(c(current_date), "`datestyle'")"' _n ///
	"local" _tab `"todaystr=string(\`today', "%td")"' _n(2) ///
	"`hbanner'" ///
	_n "* 	Import" _n /// 
	"`hbanner'" _n(2) ///
	`"import delimited "`macval(dataname)'", clear bindquote(strict) case(preserve) maxquotedrows(unlimited)"' _n(2) ///
	"`hbanner'" ///
	_n "* 	Labels" _n /// 
	"`hbanner'" _n(2) 
	
forvalues i = 1/`labels_counter' {
	file write myfile `"`lab`i''"' _n(2)
}

file write myfile ///
"`hbanner'" ///
_n "*`tab2'Clean" _n /// 
"`hbanner'" _n(2) 

forvalues i = 1/`c(N)' {
	file write myfile (command[`i']) _n(2)
}

file write myfile ///
	"`hbanner'" ///
	_n "* 	Survey Version" _n /// 
	"`hbanner'" ///
	_n(2) `"destring formdef_version, replace"' _n ///
	`"label variable formdef_version "survey version""' _n(2) ///
	"`hbanner'" ///
	_n "* 	Survey Unique ID" _n /// 
	"`hbanner'" ///
	_n(2) `"cap rename KEY key"' _n ///
		`"label variable key "survey uid""' _n(2) ///
	"`hbanner'" ///
	_n "* 	Submission Date" _n /// 
	"`hbanner'" ///
		_n(2) `"cap rename SubmissionDate submissiondate"' _n ///
		`"tempvar date"' _n ///
		`"clonevar \`date' = submissiondate"' _n ///
		`"cap replace submissiondate = "" "' _n ///
		`"if _rc replace submissiondate = ."' _n /// 
		`"destring submissiondate, replace"' _n /// 
		`"replace submissiondate = clock(\`date', "`datestyle'hms", 2030)"' _n /// 
		`"cap format submissiondate %tc"' _n ///
		`"cap label variable submissiondate "time of survey submission""' _n(2)
		
if `dk' != 1 local mv `dk' = .d 
if "`mv'" != "" local sep \
if `refused' != 1 local mv `mv' `sep' `refused' = .r 
if "`mv'" != "" local sep \
if `other' != 1 local mv `mv' `sep' `other' = .o 

if "`mv'" != "" file write myfile ///
	"`hbanner'" ///
	_n "* 	Replacing Missing Values" _n /// 
	"`hbanner'" _n(2) /// 
	`"mvdecode _all, mv(`mv')"' _n(2) ///
	`"cap drop __*"'
		
if `want_reshape' == 1 {
	
	cap assert `repeat_groups' > 0
	if _rc {
		
		display as error "No repeat groups in survey" 
		break
		
	}
	
	display regexm("`instname'", "[^\\/]+$")
	local file_short = regexs(0)
	
	frame `repeat_group_long' {
		
		levelsof group if missing(group1) & repetitions > 0, clean local(standalone)
		levelsof group if !missing(group1) & repetitions > 0, clean local(nesteds)
		
		cap file close myfile2
		file open myfile2 using "`reshapefile'", write text replace
		file write myfile2 ///
			"/*" ///
			_n "Title: Reshape Dofile for `file_short'" ///
			_n "Date Created: `c(current_date)'" ///
			_n "Author: `c(username)'" ///
			_n "Note: " ///
			_n "*/" _n(3) ///
			"`hbanner'" ///
			_n "* 	Handy Macros" _n /// 
			"`hbanner'" ///
			_n(2) ///
			"`lbanner'" _n ///
			"*	Reshapable Variables" _n ///
			"`lbanner'" _n(2)
			
	}
	
	frame `commands': file write myfile2 (varlist_0) _n(2)
			
	forvalues i = 1/`repeat_groups' {
		
		frame `commands' { 
			
			file write myfile2 (dataset_`i') _n(2)
			file write myfile2 (varlist_`i') _n(2)
		
		}
		
		frame `repeat_group_long' {
			
			local desc_`i' = group_label[`i']
			local name_`i' = group_name[`i']
		
		}
		
	}
	
	frame `repeat_group_long' {
		
		file write myfile2 ///
		"`hbanner'" ///
		_n "* 	Reshaping" _n /// 
		"`hbanner'" ///
		_n(2) "frame rename default survey" _n ///
		`"label data "Survey-level data from `file_short'""' _n(2) ///
		`"local frgetvars `frgetvars'"'
		if "`deidvars'" != "" {
			file write myfile2 _n `"local deidvars `deidvars'"'
		}
	
		foreach g in `standalone' {
			
			file write myfile2 ///
				_n(2) ///
				"`lbanner'" _n ///
				"*	Reshape to level of '`desc_`g'''" _n ///
				"`lbanner'" _n(2) ///
				"local group_name `name_`g''" _n(2) ///
				`"preserve"' _n(2) ///
				`"local regex_stubs = ustrregexra(strtrim(stritrim("\`shape_`g''")), " ", "|", .)"' _n ///
				`"local regex_pattern "^((\`regex_stubs')\d+)$""' _n ///
				`"quietly findregex, re("\`regex_pattern'") sensitive"' _n ///
				`"local reshaped_vars \`s(varlist)'"' _n ///
				`"keep \`reshaped_vars' *key"' _n(2) ///
				"foreach var in \`shape_`g'' {" _n ///
				_tab "cap local X_\`var' : variable label \`var'1" _n ///
				_tab "cap local L_\`var': value label \`var'1" _n /// 
				"}" _n(2) ///
				"reshape long \`shape_`g'', i(key) j(\`group_name'_key)" _n(2) ///
				"foreach var of varlist \`shape_`g'' {" _n ///
				_tab `"label variable \`var' "\`X_\`var''""' _n ///
				_tab `"cap label values \`var' \`L_\`var''"' _n /// 
				"}" _n(2) ///
				"frame put \`shape_`g'' `identifying_vars' \`group_name'_key, into(\`group_name')" _n ///
				`"restore"' _n `"drop \`reshaped_vars'"' _n(2) ///
				"cwf \`group_name'" _n "missings dropobs \`shape_`g'', force sysmiss" _n ///
				`"renvars *_, postsub("_" "")"' _n ///
				"frlink m:1 key, frame(survey)" _n ///
				`"if "\`frgetvars'" != "" {"' _n(2) ///
				_tab `"local toget"' _n ///
				_tab `"foreach var in \`frgetvars' {"' _n(2) ///
				_tab _tab `"frame \`c(frame)': cap confirm variable \`var'"' _n ///
				_tab _tab `"if !_rc continue"' _n(2) ///
				_tab _tab `"frame survey: cap confirm variable \`var'"' _n ///
				_tab _tab `"if !_rc local toget \`toget' \`var'"' _n(2) ///
				_tab `"}"' _n(2) ///
				_tab `"if "\`toget'" != "" frget \`toget', from(survey)"' _n(2) ///
				"}" _n(2) ///
				"isid key \`group_name'_key" _n ///
				`"local added_vars \`toget'"' _n ///
				`"keep \`varlist_`g'' \`added_vars' *key"' _n ///
				`"if "\`added_vars'" != "" order \`added_vars'"' _n ///
				`"label data "`desc_`g''-level data from `file_short'""' _n
				
			if "`deidvars'" != "" {
			
				file write myfile2 _n ///
				`"ds"' _n ///
				`"local currvars \`r(varlist)'"' _n ///
				`"local vars_to_drop : list deidvars & currvars"' _n ///
				`"if "\`vars_to_drop'" != "" drop \`vars_to_drop'"' _n(2)
				
			}
				
			if "`savefolder'" != "" {
				
				file write myfile2 ///
					`"compress"' _n ///
					`"save "`macval(savefolder)'/\`group_name'.dta", replace"' _n
				
			}

			file write myfile2 _n `"cwf survey"' _n
					
			
		}
		
		foreach g in `nesteds' {
	
// 			if `r(r)' >= 3 {
//				
// 				continue
//				
// 			}
// 			else if `r(r)' == 2 {
//				
// 				continue
//				
// 			}
// 			else if `r(r)' == 1 {
				
			local w = group1[`g']
			local row = start[`g']
			
			file write myfile2 ///
				_n(2) ///
				"`lbanner'" _n ///
				"*	Reshape to level of '`desc_`g''' from within '`desc_`w'''" _n ///
				"`lbanner'" _n(2) ///
				"local group_name `name_`g''" _n ///
				"local prev_group_name `name_`w''" _n ///
				"local all_invars" _n(2) ///
				`"preserve"' _n(2) ///
				`"local regex_stubs = ustrregexra(strtrim(stritrim("\`shape_`g''")), " ", "|", .)"' _n ///
				`"local regex_pattern "^((\`regex_stubs')\d+)$""' _n ///
				`"quietly findregex, re("\`regex_pattern'") sensitive"' _n ///
				`"local reshaped_vars \`s(varlist)'"' _n ///
				`"keep \`reshaped_vars' *key"' _n(2) ///
				"foreach var in \`shape_`g'' {" _n ///
				_tab "cap local X_\`var' : variable label \`var'1" _n "}" _n(2) ///
				"reshape long \`shape_`g'', i(key) j(\`group_name'_key)" _n(2) ///
				"foreach var of varlist \`shape_`g'' {" _n ///
				_tab `"cap label variable \`var' "\`X_\`var''""' _n "}" _n(2) ///
				"frame put \`shape_`g'' `identifying_vars' \`group_name'_key, into(\`group_name')" _n ///
				`"restore"' _n ///
				"drop \`reshaped_vars'" _n(2) ///
				"cwf \`group_name'" _n(2) ///
				"// reformat the reshape variables so that they can be reshaped long once more" _n ///
				`"local all_invars = ustrregexra("\`shape_`g''", "(\d+_)\b", "")"' _n ///
				`"local all_invars : list uniq all_invars"' _n ///
				`"local reshapevars = ustrregexra("\`all_invars'", " ", "@_ ")"' _n(2) ///
				"// find value labels" _n ///
				"foreach var in \`all_invars' {" _n ///
				_tab "cap local X_\`var' : variable label \`var'1_" _n ///
				_tab "cap local L_\`var': value label \`var'1" _n ///
				"}" _n(2) ///
				"// an id for each element being reshaped" _n ///
				"bysort key: gen reshape_id = _n" _n(2) ///
				"// reshape to long again" _n ///
				"reshape long \`reshapevars', i(reshape_id key) j(\`prev_group_name'_key)" _n(2) ///
				"// get rid of the spare underscore" _n ///
				`"renvars *__, postsub("_" "")"' _n(2) ///
				"// label the variables" _n ///
				"foreach var of varlist \`all_invars' {" _n ///
				_tab `"cap label variable \`var' "\`X_\`var''""' _n ///
				_tab `"cap label values \`var' \`L_\`var''"' _n ///
				"}" _n(2) ///
				"// if it's missing this, then it shouldn't be in the dataset" _n ///
				`"missings dropobs \`all_invars', force sysmiss"' _n(2) ///
				"// get rid of the spare underscore again" _n ///
				`"renvars *_, postsub("_" "")"' _n(2) ///
				`"frlink m:1 key, frame(survey)"' _n ///
				`"frlink m:1 `name_`w''_key key, frame(`name_`w'')"' _n ///
				`"if "\`frgetvars'" != "" {"' _n(2) ///
				_tab `"local added_vars"' _n ///
				_tab `"foreach frame in survey `name_`w'' {"' _n(2) ///
				_tab _tab `"local toget"' _n ///
				_tab _tab `"foreach var in \`frgetvars' {"' _n(2) ///
				_tab _tab _tab `"frame \`c(frame)': cap confirm variable \`var'"' _n ///
				_tab _tab _tab `"if !_rc continue"' _n(2) ///
				_tab _tab _tab `"frame \`frame': cap confirm variable \`var'"' _n ///
				_tab _tab _tab `"if !_rc local toget \`toget' \`var'"' _n(2) ///
				_tab _tab `"}"' _n(2) ///
				_tab _tab `"local added_vars \`added_vars' \`toget'"' _n ///
				_tab _tab `"if "\`toget'" != "" frget \`toget', from(\`frame')"' _n ///
				_tab _tab `"drop \`frame'"' _n(2) ///
				_tab `"}"' _n(2) ///
				"}" _n(2) ///
				"// check ids are intact" _n ///
				"isid key `name_`g''_key `name_`w''_key" _n(2) ///
				"keep \`varlist_`g'' \`added_vars' *key" _n ///
				`"if "\`added_vars'" != "" order \`added_vars'"' _n ///
				`"label data "`desc_`g''-level data by `desc_`w'' from `file_short'""' _n
					
// 			}
			
			if "`deidvars'" != "" {
				
				file write myfile2 _n ///
				`"ds"' _n ///
				`"local currvars \`r(varlist)'"' _n ///
				`"local vars_to_drop : list deidvars & currvars"' _n ///
				`"if "\`vars_to_drop'" != "" drop \`vars_to_drop'"' _n(2)
				
			}
			
			if "`savefolder'" != "" {
				
				file write myfile2 ///
					`"compress"' _n ///
					`"save "`macval(savefolder)'/\`group_name'.dta", replace"' _n(2)
				
			}

			file write myfile2 `"cwf survey"' _n(2)
			
		}
		
		file write myfile2 ///
			`"keep \`varlist_0' key instanceID formdef_version"' _n
		
		if "`deidvars'" != "" {
			
			file write myfile2 _n ///
			`"ds"' _n ///
			`"local currvars \`r(varlist)'"' _n ///
			`"local vars_to_drop : list deidvars & currvars"' _n ///
			`"if "\`vars_to_drop'" != "" drop \`vars_to_drop'"' _n(2)
			
		}
		
		
		if "`savefolder'" != "" {
				
				file write myfile2 ///
					`"compress"' _n ///
					`"save "`macval(savefolder)'/survey.dta", replace"' _n
				
		}
		
		file close myfile2
		
	}
	
}
	
	
file close myfile

cwf `original_frame'
restore
	
}

end
