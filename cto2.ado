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

missings dropobs, force

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

replace type = strtrim(stritrim(type))
replace type = subinstr(type, " ", "_", .) ///
	if inlist(type, "begin group", "end group", "begin repeat", "end repeat")
	
// Remove any line breaks, dollar signs, and double quotes from the "labelEnglishen", "labelStata", and "relevant" variables
foreach var of varlist labelEnglishen labelStata relevant {
	replace `var' = subinstr(`var', char(10), "", .)
	replace `var' = subinstr(`var', "$", "#", .) 
	replace `var' = subinstr(`var', `"""', "", .)
	/* Stata will intepret dollar signs as globals, and as a result, we won't 
	be able to see the questions being referenced for constraints (or even 
	inside the question itself). By changing the dollar sign to a #, we can 
	still see these references in the variable labels and notes */
}

// Replace any full stops in variable names with an empty string
replace name = subinstr(name, ".", "", .) 

// Split the "type" variable into two variables, "type" and "type2"
split type 
// Note that "type2" will contain the value label name for categorical variables.

*------------------------------------------------------------------
*	Question Types
*------------------------------------------------------------------

gen preloaded = regexm(calculation, "^pulldata")
gen note = type == "note"

local numeric_formulae index area number round count count-if sum ///
	sum-if min min-if max max-if distance-between int abs duration
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
	| preloaded==1 ///
	| (type == "calculate" & numeric_calculate == 0) ///
	| string_force == 1
	
replace question_type = 2 if word(type, 1) == "select_one" ///
	& missing(question_type)

replace question_type = 3 if word(type, 1) == "select_multiple"

replace question_type = 4 if (!inlist(type, "date", "text") & missing(question_type)) ///
	| numeric_force == 1

replace question_type = 5 if inlist(type, "date", "today")

replace question_type = 6 if inlist(type, "start", "end", "submissiondate")

replace question_type= 7 if type == "geopoint"

replace question_type = -111 if inlist(type, "begin_group", "end_group", ///
	"begin_repeat", "end_repeat")
replace question_type = -222 if note==1
replace question_type = -333 if type == "text audit"
replace question_type = -555 if missing(question_type)

drop if question_type < -111

// Create a new variable called "order" to retain the original order of variables in the instrument
gen order = _n
	
*===============================================================================
* 	Make Groups Dataset
*===============================================================================

tempname groups
frame create `groups' int type strL name strL label strL repeat_count ///
	int repetitions int instrument_row int index strL conditions int within ///
	strL example_var int layers_nested

local n_groups = 0
local n_repeats = 0
local grouplist 0
local r_grouplist 0
local group_label 0 "Survey-level"
local repeat_label 0 "Survey-level"

gen group = 0
gen repeat_group = 0

sort order
forvalues i = 1/`c(N)' {
	
	local j = `i' + 1
	
	if type[`i'] == "begin_group" { 
		
		local ++n_groups
		local grouplist = strtrim(stritrim("`grouplist' `n_groups'"))
		local group_label `group_label' `n_groups' "`=labelStata[`i']'"
		
		local type = question_type[`j']
		while `type' == 7 {
			
			++j
			local type = question_type[`j']
			
		}
		
		frame post `groups' (1) (name[`i']) (labelStata[`i']) ///
			(repeat_count[`i']) (.) (`i') (`n_groups') (relevant[`i']) ///
			(`=real(word("`grouplist'", -2))') (name[`j']) (0)
		
	}
		
	else if type[`i'] == "begin_repeat" {
		
		local ++n_repeats
		local r_grouplist = strtrim(stritrim("`r_grouplist' `n_repeats'"))
		local repeat_label `repeat_label' `n_repeats' "`=labelStata[`i']'"
		
		local type = question_type[`j']
		while `type' == 7 {
			
			++j
			local type = question_type[`j']
			
		}
		
		frame post `groups' (2) (name[`i']) (labelStata[`i']) ///
			(repeat_count[`i']) (.) (`i') (`n_repeats') (relevant[`i']) ///
			(`=real(word("`r_grouplist'", -2))') (name[`j']) (0)
		
	}
	
	else if type[`i'] == "end_group" {
		
		local grouplist = strtrim(stritrim("`grouplist'"))
		local grouplist = ///
			substr("`grouplist'", 1, length("`grouplist'") ///
			- strlen(word("`grouplist'", -1)))
		
	}
	
	else if type[`i'] == "end_repeat" {
		
		local r_grouplist = strtrim(stritrim("`r_grouplist'"))
		local r_grouplist = ///
			substr("`r_grouplist'", 1, length("`r_grouplist'") ///
			- length(word("`r_grouplist'", -1)))
		
	}
	
	local current_group = word("`grouplist'", -1)
	local current_repeat = word("`r_grouplist'", -1)
	
	replace group = `current_group' in `i'
	replace repeat_group = `current_repeat' in `i'
	
}


frame `groups' { 

	gen group_uid = _n
	labmask group_uid, values(label)
	
	label define group_type 1 "Standard Group" 2 "Repeat Group"
	label values type group_type
	
}

if `n_repeats' != 0 {
	
	forvalues i = 1/`n_repeats' {
		
		frame `groups' {
			
			levelsof group_uid if type == 2 & index == `i', clean local(row)
			local repeat_var = example_var[`row']
			
			local nest = within[`row']
			local nested = `nest' != 0
			local t = 0
			while `nested' == 1 {
				
				local ++t
				replace layers_nested = `t' in `row'
				
				local repeat_var `repeat_var'_1
				levelsof group_uid if type == 2 & index == `nest', clean local(new_row)
				
				cap gen nest_level_`t' = .
				replace nest_level_`t' = `nest' in `row'
				
				cap gen nest_repeats_`t' = .
				replace nest_repeats_`t' = repetitions[`new_row'] in `row'
				
				local nest = within[`new_row']
				local nested = `nest' != 0
				
			}
			
		} 

		frame `rawdata' {
						
			findregex, re("^`repeat_var'_\d+") sensitive
			local nested_varlist `s(varlist)'
			local max_num = 0
			foreach var in `nested_varlist' {
				
				local num = real(ustrregexra("`var'", ".*_(\d+)$", "$1"))
				if (`num' > `max_num') local max_num = `num'
				
			}
			
		}
			
		frame `groups': replace repetitions = `max_num' in `row'
		
	}
	
	frame `groups' {
	
		sum layers_nested 
		local nest_layers = `r(max)'
		
		if "`nest_layers'" !=  "" forvalues t = 1/`nest_layers' {
	
			local nest_vars `nest_vars' nest_level_`t' nest_repeats_`t'
			local nest_repeat_vars `nest_repeat_vars' nest_repeats_`t'
			
		}
	
	}
	
}


tempname repeat_groups
frame copy `groups' `repeat_groups'
frame `repeat_groups': drop if type == 1

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
* 	Variables
*===============================================================================

cwf `qs'
compress
drop if question_type < 0
sort order

// generate some helpful macros
local brek = char(10)
local tab = char(9)
foreach thing in brek tab {
	
	forvalues z = 2/3 {
	
		local `thing'`z' = `z' * "``thing''"
	
	}
	
}

local hbanner = "*" + ("=")* 65
local lbanner = "*" + ("-") * 65

if `n_repeats' > 0 {
	
	frlink m:1 repeat_group, frame(`repeat_groups' index)
	frget repetitions within layers_nested `nest_vars' ///
		, from(`repeat_groups')
	
}

gen within_order = .
clonevar original_name = name 
clonevar desired_varname = name 
if "`rename'" != "" {
	
	replace desired_varname = new_name if !missing(new_name)
	
}

// expand multiple_selects
gen n = _n 
levelsof order if question_type == 3, clean local(mult_selects)
foreach i in `mult_selects' {
	
	levelsof n if order == `i', clean local(j)
	local name = desired_varname[`j']
	local vallabel = type2[`j']
	
	frame `choices' {
		
		levelsof order if list_name == "`vallabel'"
		local options `r(levels)'
		local levels = `r(r)'
		local expansion = `levels' + 1
		
	}
		
	expand `expansion' if order == `i'
	bysort order (within_order): replace within_order = _n if order == `i'

	local s = 1
	foreach row in `options' {
		
		local ++s
		frame `choices': local value = name[`row']
		frame `choices': local lab = label[`row']
		frame `qs': local labstat = labelStata[`j']
		
		replace desired_varname = desired_varname + "_" + "`value'" if order == `i' & within_order == `s'
		replace name = name + "_" + "`value'" if order == `i' & within_order == `s'
		replace labelStata = "#{`name'}: `lab'" if order == `i' & within_order == `s'
	
	}
	
	sort order within_order
	replace n = _n
	
}

// expand GPS
levelsof order if question_type == 7, clean local(gps)
foreach i in `gps' {
		
	expand 4 if order == `i'
	bysort order (within_order): replace within_order = _n if order == `i'
	
	local s = 0
	foreach f in Accuracy Latitude Longitude Altitude {
		
		local ++s
		replace desired_varname = desired_varname + "`f'" if within_order == `s' & order == `i'
		replace name = name + "`f'" if within_order == `s' & order == `i'
		replace labelStata = labelStata + ": `f'" if within_order == `s' & order == `i'
		
	}
	
}

assert !missing(desired_varname)

// solving problem of duplicate names...
bysort desired_varname (order): keep if _n == 1
sort order within_order 
replace n = _n 

// how this variable appears in the raw data 
gen vlist = name

// how this variable will appear in final data
gen desired_vlist = desired_varname

// the reshaping varlist
gen shape_vlist = ""

if `n_repeats' > 0 {
	
	frame `repeat_groups' {
		
		sort index 
		forvalues r = 1/`c(N)' {
			
			local layers_nested = layers_nested[`r']
			
			forvalues l = `layers_nested'(-1)0 {
				
				frame `qs' { 
				
					tempvar previous_vlist 
					clonevar `previous_vlist' = vlist
					
					tempvar previous_desired_vlist 
					clonevar `previous_desired_vlist' = desired_vlist
					
					replace shape_vlist = ///
						ustrregexra(`previous_desired_vlist', "(\w+)", "$1_") ///
						if repeat_group == `r'
					
					replace vlist = "" if repeat_group == `r'
					replace desired_vlist = "" if repeat_group == `r'
					
				}
				
				if `l' == 0 local repvar repetitions 
				else local repvar nest_repeats_`l'
				local layer_repeats = `repvar'[`r']
				
				forvalues c = 1/`layer_repeats' {
					
					frame `qs' { 
						
						replace vlist = vlist + " " + ustrregexra(`previous_vlist', "(\b\w+\b)", "$1_`c' ") if repeat_group == `r'
						replace desired_vlist = desired_vlist + " " + ustrregexra(`previous_desired_vlist', "(\b\w+\b)", "$1_`c' ") if repeat_group == `r'
						
					}
					
				}
				
			}
			
		}
		
	}
	
}

drop if repetitions == 0

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

gen values_command = "cap label values " + var_stub + " " + type2 if !missing(type2) ///
	& !inlist(question_type, 3, 7)

gen notes_command = "cap notes " + var_stub + ": " + labelEnglishen + ///
	"`brek'cap notes " + var_stub + ": relevance conditions: " + relevant

gen char_command = "" 
replace char_command = char_command + "cap char " + var_stub + "[qtext] " ///
	+ labelEnglishen + "`brek'cap char " + var_stub + "[logic] " + relevant + ///
	"`brek'cap char " + var_stub + "[preloaded] " + cond(preloaded == 1, "1", "0")
	
gen command = "`hbanner'`brek'*`tab2'" + name + "`brek'`hbanner'`brek2'" 
replace command = command + "cap foreach var of varlist " + vlist + ///
	"{`brek2'cap confirm variable " + var_stub + "`brek'if _rc continue`brek2'" if num_vars > 1
replace command = command + label_command + "`brek'" + format_command + ///
	"`brek'" + values_command + "`brek'" + notes_command + "`brek'" + char_command
	
if "`rename'" != "" {
	
	gen rename_command = `"`brek'cap rename "' + var_stub + `" \`=ustrregexrf(""' + ///
		var_stub + `"", "^"' + name + `"", ""' + desired_varname + `"")'"' ///
		if !missing(new_name) & new_name != name
	
	replace command = command + rename_command
	
}

replace command = command + "`brek2'}" if num_vars > 1

forvalues i = 0(1)`n_repeats' {
	
	valuesof desired_varname if repeat_group == `i'
	local varlist_`i' `r(values)'
	local varlist_`i' : list varlist_`i' - deidvars
	
	if `i' == 0 continue
	valuesof shape_vlist if repeat_group == `i'
	local shapelist_`i' `r(values)'
	
}

*===============================================================================
* 	Open File
*===============================================================================

if "`savefolder'" != "" cap mkdir "`savefolder'"

tempname codebook
frame copy `qs' `codebook'
cwf `codebook'

drop __* vlist desired_vlist shape_vlist label_command format_command ///
	values_command notes_command var_stub num_vars string_force numeric_force ///
	note repeat_count type name numeric_calculate
	
label define repeat_label `repeat_label'
label define group_label `group_label'

label values repeat_group within repeat_label
label values group group_label

rename order var_uid	
rename desired_varname name
rename labelStata varlabel
rename labelEnglishen full_question
rename type1 original_type
rename type2 vallabel
rename n order

order order name varlabel 

label variable order "order of appearance in instrument"
label variable within_order "order of appearance for variables generated by single line"
label variable name "variable name"
label variable question_type "question type"
label variable varlabel "variable label"
label variable vallabel "value label"
label variable full_question "full question text in instrument"
label variable repeat_group "lowest-level repeat group to which question belongs"
label variable group "lowest-level group to which question belongs"
label variable relevant "full set of relevancy conditions for this question"
label variable original_type "variable type, as specified in instrument"
label variable preloaded "preloaded variable"
label variable repetitions "number of repeats for repeat group this variable was in"
label variable within "repeat group this repeated variable is nested inside"
label variable command "import dofile command for this variable"
label variable var_uid "variable unique ID"

if "`savefolder'" != "" {
	
	frame `codebook': save "`savefolder'/survey_metadata.dta", replace
	frame `groups': save "`savefolder'/group_metadata.dta", replace
	
}

cwf `qs'

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
	
	cap assert `n_repeats' > 0
	if _rc {
		
		display as error "No repeat groups in survey" 
		break
		
	}
	
	display regexm("`instname'", "[^\\/]+$")
	local file_short = regexs(0)
		
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
	
	file write myfile2 "local varlist_0 `varlist_0'" _n(2)
			
	forvalues i = 1/`n_repeats' {
			
		if repetitions[`i'] != 0 {
			
			file write myfile2 "local shape_`i' `shapelist_`i''" _n(2)
			file write myfile2 "local varlist_`i' `varlist_`i''" _n(2)
			
		}
		
	}
	
	frame `repeat_groups' {
		
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

		forvalues i = 1/`n_repeats' {
			
			if `=repetitions[`i']' == 0 continue
			
			local all_keys key 
			local within 
			local layers_nested = layers_nested[`i']
			local target_desc = label[`i']
			local target_name = name[`i']
			local counter = -1
			local lab_regex = (`layers_nested' * "_[0-9]+") + "_"
			local previous_frames
			
			forvalues k = `layers_nested'(-1)0 {
				
				local ++counter
				
				if `k' == `layers_nested' {
						
					local prev_desc "Survey-Level"
					local prev_name survey
					local prev_key "key"
					local regex `""(_)\b", "_@""'
					
					file write myfile2 ///
						_n(2) ///
						"`hbanner'" _n ///
						"*	Reshape to level of '`target_desc''" _n ///
						"`hbanner'" _n(2) ///
						`"local reshape_vars \`shape_`i''"' _n ///
						"foreach var in \`reshape_vars' {" _n ///
						_tab `"local stub_var = ustrregexra("\`var'", "(`lab_regex')\$", "")"' _n ///
						_tab `"local stub_vars \`stub_var'"' _n ///
						_tab "cap local X_\`stub_var' : variable label \`var'1" _n ///
						_tab "cap local L_\`stub_var': value label \`var'1" _n /// 
						"}" _n(2) ///
						"local stub_vars : list uniq stub_vars"
						
				}
				else {
					
					if `counter' == 1 {
					
						local prev_desc `target_desc'
						local prev_name `target_name'
					
					}
					else {
						
						local u = `k' + 1
						local w = nest_level_`u'[`i']
						local prev_desc = label[`w']
						local prev_name = name[`w']
						
					}
					
					local prev_key `prev_name'_key
					local regex `""_[0-9]+_@ ", "_@ ""'
				
				}
				
				if `counter' == 0 {
						
					local to `i'
					local previous_frames survey
						
				}
				else {
					
					local p = `k' + 1
					local prev_frame = name[`=nest_level_`p'[`i']']
					local to nest_level_`counter'[`i']
					local previous_frames `previous_frames' `prev_frame'
					
				}
				
				local to_desc = label[`to']
				local to_name = name[`to']
				local to_key `to_name'_key
					
				
				file write myfile2 ///
					_n(2) ///
					"`lbanner'" _n ///
					"*	Reshape to level of '`to_desc'' from '`prev_desc''" _n ///
					"`lbanner'" _n(2) ///
					`"local reshape_vars = ustrregexra("\`reshape_vars'", `regex')"' _n ///
					`"local reshape_vars : list uniq reshape_vars"' _n(2)
					
				if `k' == `layers_nested' {
					
					file write myfile2 ///
						`"preserve"' _n(2) ///
						`"local regex_stubs = ustrregexra(strtrim(stritrim("\`shape_`i''")), " ", "|", .)"' _n ///
						`"local regex_pattern "^((\`regex_stubs')\d+)$""' _n ///
						`"quietly findregex, re("\`regex_pattern'") sensitive"' _n ///
						`"local reshaped_vars \`s(varlist)'"' _n ///
						`"keep \`reshaped_vars' `all_keys'"' _n(2)
					
				}
				
				file write myfile2 ///
					"reshape long \`reshape_vars', i(`all_keys') j(`to_key')" _n ///
					`"renvars *_, postsub("_" "")"' _n
					
				local all_keys `all_keys' `to_key'
				
				if `k' == 0 {
					
					file write myfile2 ///
						`"local reshape_vars = ustrregexra("\`reshape_vars'", "_@", "", .)"' _n ///
						"frame put \`reshape_vars' `all_keys', into(`target_name')" _n ///
						"cwf `target_name'" _n ///
						"missings dropobs \`reshape_vars', force sysmiss" _n ///
						"isid `all_keys'" _n(2) ///
						`"foreach var of varlist \`varlist_`i'' {"' _n(2) ///
						_tab `"cap label variable \`var' "\`X_\`var''""' _n ///
						_tab `"cap label values \`var' "\`L_\`var''""' _n(2) ///
						"}" _n(2) ///
						"keep \`varlist_`i'' \`added_vars' `all_keys'" _n(2) ///
						`"frame survey: restore"' _n(2)
						
					file write myfile2 ///
						`"local j = 0"' _n ///
						`"foreach frame in `previous_frames' {"' _n(2) ///
						_tab `"local ++j"' _n ///
						_tab `"if \`j' == 1 local keys key"' _n ///
						_tab `"else local keys \`keys' \`frame'_key"' _n(2) ///
						_tab `"if "\`frgetvars'" != "" {"' _n(2) ///
						_tab _tab `"local toget"' _n ///
						_tab _tab `"foreach var in \`frgetvars' {"' _n(2) ///
						_tab _tab _tab `"cap confirm variable \`var'"' _n ///
						_tab _tab _tab `"if !_rc continue"' _n(2) ///
						_tab _tab _tab `"frame \`frame': cap confirm variable \`var'"' _n ///
						_tab _tab _tab `"if !_rc local toget \`toget' \`var'"' _n(2) ///
						_tab _tab "}" _n(2) ///
						_tab _tab `"if "\`toget'" != "" {"' _n(2) ///
						_tab _tab _tab `"frlink m:1 \`keys', frame(\`frame')"' _n ///
						_tab _tab _tab `"frget \`toget', from(\`frame')"' _n ///
						_tab _tab _tab `"drop \`frame'"' _n(2) ///
						_tab _tab "}" _n(2) _tab "}" _n(2) "}" _n(2)
					
					
					file write myfile2 "compress" _n ///
						`"label data "`target_desc' data from `file_short'""' _n ///
						`"save "`macval(savefolder)'/`target_name'.dta", replace"' _n(2) ///
						"cwf survey" _n ///
						`"drop \`reshaped_vars'"'

						
				}
				
			}
			
		}
		
		file write myfile2 _n(2) ///
			"`hbanner'" ///
			_n "* 	Survey-Level" _n /// 
			"`hbanner'" _n(2) ///
			`"keep \`varlist_0' key submissiondate formdef_version"' _n ///
			"compress" _n ///
			`"label data "survey-level data from `file_short'""' _n ///
			`"save "`macval(savefolder)'/survey.dta", replace"'
		
		file close myfile2
		
	}
	
}
	
file close myfile

cwf `original_frame'
restore
	
}

end
