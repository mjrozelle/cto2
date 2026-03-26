*! cto2.ado - Stata module to import and minimally clean SurveyCTO data
*! Author: Michael Rozelle <michael.rozelle@wur.nl>
*! Version 2.0.0  Modified:  March 2026

// Drop all cto2 programs before (re)defining them
foreach prog in cto2 _cto2_validate _cto2_parse_survey _cto2_parse_groups ///
	_cto2_parse_choices _cto2_expand_vars _cto2_build_commands ///
	_cto2_write_import _cto2_write_reshape {
	cap program drop `prog'
}

*===============================================================================
*	Main Program
*===============================================================================

program define cto2, rclass

syntax, ///
	INSTname(string) /// filepath to the Excel survey instrument
	DATAname(string) /// filepath to the .csv data exported from SurveyCTO
	DOfile(string) /// filepath to the import dofile you want to create
	[RESHAPEfile(string) /// filepath to the reshape dofile you want to create
	IDENTIFIERS(namelist) /// survey unique identifier (usually "key")
	AMERICAN /// use if data were exported in MM/DD/YYYY format
	ISO /// use if data were exported in YYYY-MM-DD format
	SAVEfolder(string) /// filepath to the folder where all datasets should be saved
	FRGETVARS(namelist) /// variables you want to copy downstream to all reshaped datasets
	DEIDVARS(namelist) /// variables that identify respondents and need to be removed
	DK(integer 1) /// value used to indicate "don't know" responses
	OTHER(integer 1) /// value used to indicate "other (specify)" responses
	REFUSED(integer 1) /// value used to indicate "refused to answer" responses
	RENAME /// use if you have filled in the "new_name" column and wish to rename variables
	REPLACE /// use to replace existing dofiles
	NUMERIC(namelist) /// use to force specific variables to be read as numeric
	STRING(namelist) /// use to force specific variables to be read as string
	KEEP(namelist)] // use to "keep" survey-level variables generated outside the immediate survey

version 17

// Determine date style
if "`iso'" != "" & "`american'" != "" {
	display as error "Options {bf:iso} and {bf:american} are mutually exclusive."
	exit 198
}
local datestyle "DMY"
if "`american'" != "" local datestyle "MDY"
if "`iso'" != "" local datestyle "YMD"

local want_reshape = ("`reshapefile'" != "")

// Preserve state
preserve
local original_frame `c(frame)'

// Wrap main body -- cleanup happens on error or success
capture noisily {

	// --- Phase 1: Validate -----------------------------------------------
	noisily display as text "Validating inputs..."

	_cto2_validate, ///
		instname("`instname'") dataname("`dataname'") dofile("`dofile'") ///
		reshapefile("`reshapefile'") savefolder("`savefolder'") ///
		dk(`dk') other(`other') refused(`refused') ///
		`replace' want_reshape(`want_reshape')

	// --- Phase 2: Parse Survey -------------------------------------------
	tempname qs rawdata
	frame create `qs'
	frame create `rawdata'

	// Import raw data header (row 1 only) for variable name detection
	frame `rawdata' {
		import delimited "`dataname'", clear bindquote(strict) ///
			case(preserve) rowrange(:1)
		compress
	}

	noisily display as text "Parsing survey instrument..."

	_cto2_parse_survey, ///
		qsframe(`qs') instname("`instname'") ///
		rename("`rename'") string("`string'") numeric("`numeric'")

	frame `qs' {
		local n_questions = `c(N)'
	}
	noisily display as text "  `n_questions' questions found."

	// --- Phase 3: Parse Groups -------------------------------------------
	tempname groups
	frame create `groups'

	_cto2_parse_groups, ///
		qsframe(`qs') rawframe(`rawdata') groupsframe(`groups')

	frame `groups' {
		count if type == 1
		local n_groups = `r(N)'
		count if type == 2
		local n_repeats = `r(N)'
	}
	noisily display as text ///
		"  `n_groups' groups, `n_repeats' repeat groups."

	// --- Phase 4: Parse Choices ------------------------------------------
	tempname choices
	frame create `choices'

	noisily display as text "Parsing choice lists..."

	_cto2_parse_choices, ///
		choicesframe(`choices') instname("`instname'") ///
		dk(`dk') other(`other') refused(`refused')

	frame `choices' {
		levelsof list_name, local(choice_lists) clean
		local n_lists : word count `choice_lists'
	}
	noisily display as text "  `n_lists' choice lists."

	// --- Phase 5: Expand Variables ---------------------------------------
	noisily display as text "Expanding variables..."

	_cto2_expand_vars, ///
		qsframe(`qs') choicesframe(`choices') ///
		groupsframe(`groups') ///
		rename("`rename'") deidvars("`deidvars'") ///
		n_repeats(`n_repeats')

	// --- Phase 6: Build Commands -----------------------------------------
	noisily display as text "Building cleaning commands..."

	_cto2_build_commands, ///
		qsframe(`qs') datestyle("`datestyle'") ///
		rename("`rename'")

	// --- Phase 7: Save Metadata ------------------------------------------
	if "`savefolder'" != "" {

		cap mkdir "`savefolder'"

		// Save survey metadata
		tempname codebook
		frame copy `qs' `codebook'
		frame `codebook' {

			cap drop vlist desired_vlist shape_vlist label_command ///
				format_command values_command notes_command char_command ///
				var_stub num_vars string_force numeric_force ///
				note repeat_count type name numeric_calculate ///
				is_begin_group is_end_group is_begin_repeat is_end_repeat ///
				group_depth repeat_depth
			cap drop __*

			// Label repeat group values
			if `n_repeats' > 0 {
				frame `groups' {
					local repeat_label 0 "Survey-level"
					forvalues i = 1/`c(N)' {
						if type[`i'] == 2 {
							local idx = index[`i']
							local lab = label[`i']
							local repeat_label `repeat_label' `idx' "`lab'"
						}
					}
				}
				label define repeat_label `repeat_label'
				cap label values repeat_group repeat_label
				cap label values within repeat_label
				cap label variable repetitions ///
					"number of repeats for repeat group this variable was in"
				cap label variable within ///
					"repeat group this repeated variable is nested inside"
			}

			// Label group values
			frame `groups' {
				local group_label 0 "Survey-level"
				forvalues i = 1/`c(N)' {
					if type[`i'] == 1 {
						local idx = index[`i']
						local lab = label[`i']
						local group_label `group_label' `idx' "`lab'"
					}
				}
			}
			label define group_label `group_label'
			cap label values group group_label

			// Rename to standard codebook variable names
			cap rename order var_uid
			cap rename desired_varname name
			cap rename labelStata varlabel
			cap rename labelEnglishen full_question
			cap rename type1 original_type
			cap rename type2 vallabel
			cap rename n order

			order order name varlabel

			label variable order "order of appearance in instrument"
			cap label variable within_order ///
				"order of appearance for variables generated by single line"
			label variable name "variable name"
			label variable question_type "question type"
			label variable varlabel "variable label"
			cap label variable vallabel "value label"
			cap label variable full_question "full question text in instrument"
			label variable repeat_group ///
				"lowest-level repeat group to which question belongs"
			label variable group ///
				"lowest-level group to which question belongs"
			cap label variable relevant ///
				"full set of relevancy conditions for this question"
			cap label variable original_type ///
				"variable type, as specified in instrument"
			cap label variable preloaded "preloaded variable"
			label variable command "import dofile command for this variable"
			cap label variable var_uid "variable unique ID"

			save "`savefolder'/survey_metadata.dta", replace
		}

		frame `groups': save "`savefolder'/group_metadata.dta", replace

		noisily display as text "Metadata saved to `savefolder'/"
	}

	// --- Phase 8: Write Import Dofile ------------------------------------
	noisily display as text "Writing import dofile..."

	_cto2_write_import, ///
		qsframe(`qs') choicesframe(`choices') ///
		dofile("`dofile'") dataname("`dataname'") ///
		instname("`instname'") datestyle("`datestyle'") ///
		dk(`dk') other(`other') refused(`refused') ///
		keep("`keep'")

	noisily display as text "  Created: `dofile'"

	// --- Phase 9: Write Reshape Dofile (conditional) ---------------------
	if `want_reshape' {

		if `n_repeats' == 0 {
			noisily display as error ///
				"No repeat groups found in survey -- skipping reshape dofile."
		}
		else {
			noisily display as text "Writing reshape dofile..."

			_cto2_write_reshape, ///
				qsframe(`qs') groupsframe(`groups') ///
				reshapefile("`reshapefile'") instname("`instname'") ///
				savefolder("`savefolder'") ///
				frgetvars("`frgetvars'") deidvars("`deidvars'") ///
				keep("`keep'") n_repeats(`n_repeats')

			noisily display as text "  Created: `reshapefile'"
		}
	}

} // end capture noisily

// Capture the return code before cleanup
local rc = _rc

// Cleanup: always restore state
cap file close myfile
cap file close myfile2
cwf `original_frame'
restore

// Re-throw any error
if `rc' exit `rc'

noisily display as text _n "{bf:cto2 complete.}"

end

*===============================================================================
*	Phase 1: Validate Inputs
*===============================================================================

program define _cto2_validate

syntax, instname(string) dataname(string) dofile(string) ///
	dk(integer) other(integer) refused(integer) ///
	want_reshape(integer) ///
	[reshapefile(string) savefolder(string) REPLACE]

// Check input files exist
confirm file "`instname'"
confirm file "`dataname'"

// Check output files don't already exist (unless replace specified)
cap confirm file "`dofile'"
if !_rc & "`replace'" == "" {
	display as error "File `dofile' already exists."
	display as error "Add option {bf:replace} if you wish to overwrite it."
	exit 602
}

if "`reshapefile'" != "" {
	cap confirm file "`reshapefile'"
	if !_rc & "`replace'" == "" {
		display as error "File `reshapefile' already exists."
		display as error "Add option {bf:replace} if you wish to overwrite it."
		exit 602
	}
}

// Reshape requires savefolder
if `want_reshape' & "`savefolder'" == "" {
	display as error ///
		"Option {bf:savefolder()} is required when {bf:reshapefile()} is specified."
	exit 198
}

// Verify instrument has required sheets
capture {
	tempname _test
	frame create `_test'
	frame `_test' {
		import excel "`instname'", firstrow clear sheet(survey) cellrange(A1:A2)
	}
	frame drop `_test'
}
if _rc {
	display as error ///
		`"Instrument file does not contain a "survey" sheet: `instname'"'
	exit 601
}

capture {
	tempname _test2
	frame create `_test2'
	frame `_test2' {
		import excel "`instname'", firstrow clear sheet(choices) cellrange(A1:A2)
	}
	frame drop `_test2'
}
if _rc {
	display as error ///
		`"Instrument file does not contain a "choices" sheet: `instname'"'
	exit 601
}

// Check dk/other/refused are distinct when specified
local mv_vals
if `dk' != 1 local mv_vals `mv_vals' `dk'
if `other' != 1 local mv_vals `mv_vals' `other'
if `refused' != 1 local mv_vals `mv_vals' `refused'

local n_mv : word count `mv_vals'
local mv_uniq : list uniq mv_vals
local n_uniq : word count `mv_uniq'

if `n_mv' != `n_uniq' {
	display as error ///
		"Options {bf:dk()}, {bf:other()}, and {bf:refused()} must have distinct values."
	exit 198
}

end

*===============================================================================
*	Phase 2: Parse Survey Sheet
*===============================================================================

program define _cto2_parse_survey

syntax, qsframe(string) instname(string) ///
	[rename(string) string(string) numeric(string)]

cwf `qsframe'

// Import the survey sheet
import excel "`instname'", firstrow clear sheet(survey)

// Drop empty rows (replaces: missings dropobs, force)
egen _n_nonmiss = rownonmiss(_all), strok
drop if _n_nonmiss == 0
drop _n_nonmiss

// Check for rename column if requested
if "`rename'" != "" local rencheck new_name

// Verify required columns exist; accumulate warnings for optional ones
local required type name
local optional calculation relevant repeat_count `rencheck'
local keepvars
local missvars

foreach v in `required' {
	cap confirm variable `v'
	if _rc {
		display as error "Required column '`v'' not found in survey sheet."
		exit 111
	}
	cap tostring `v', replace
	local keepvars `keepvars' `v'
}

foreach v in `optional' {
	cap confirm variable `v'
	if _rc {
		local missvars `missvars' `v'
		continue
	}
	cap tostring `v', replace
	local keepvars `keepvars' `v'
}

// Handle label columns
// Instruments may have: "label" only, "labelEnglishen" only,
// "labelStata" only, both "label" and "labelStata", or none.
cap confirm variable labelEnglishen
local has_labelEn = !_rc
cap confirm variable labelStata
local has_labelSt = !_rc
cap confirm variable label
local has_label = !_rc

if `has_label' & !`has_labelEn' {
	// "label" exists but "labelEnglishen" does not -- rename it
	rename label labelEnglishen
}
else if `has_label' & `has_labelEn' {
	// Both exist -- drop the generic one, keep the specific one
	drop label
}
else if !`has_label' & !`has_labelEn' {
	// Neither exists -- create empty
	gen labelEnglishen = ""
}

cap tostring labelEnglishen, replace

if !`has_labelSt' {
	clonevar labelStata = labelEnglishen
}
else {
	cap tostring labelStata, replace
}

keep `keepvars' labelEnglishen labelStata

if "`missvars'" != "" {
	noisily display as result ///
		"Warning: optional column(s) not found: `missvars'"
}

// Ensure optional columns exist (as empty) even if missing from instrument
foreach v in calculation relevant repeat_count {
	cap confirm variable `v'
	if _rc gen `v' = ""
}

// --- String cleaning (single pass) ----------------------------------
// Dollar signs -> # (prevents Stata macro expansion in labels/notes)
// Line breaks and double quotes removed
foreach v of varlist labelEnglishen labelStata relevant repeat_count calculation {
	replace `v' = subinstr(`v', "$", "#", .)
	replace `v' = subinstr(`v', char(10), "", .)
	replace `v' = subinstr(`v', char(13), "", .)
	replace `v' = subinstr(`v', `"""', "", .)
}

// Fill in missing Stata labels with English labels
replace labelStata = "" if labelStata == "."
replace labelStata = labelEnglishen if missing(labelStata)

// Standardize type strings
replace type = strtrim(stritrim(type))
replace type = subinstr(type, " ", "_", .) ///
	if inlist(type, "begin group", "end group", "begin repeat", "end repeat")

// Remove dots from variable names
replace name = subinstr(name, ".", "", .)
replace name = strtrim(stritrim(name))

// Split type into type1 (field type) and type2 (value label name)
split type
// type2 now contains the choice list name for select_one/select_multiple

// --- Question type classification -----------------------------------
gen preloaded = regexm(calculation, "^pulldata")
gen note = (type == "note")

// Build regex for numeric calculation detection
local numeric_formulae index area number round count count-if sum ///
	sum-if min min-if max max-if distance-between int abs duration
local regex : subinstr local numeric_formulae " " "|", all
local regex_pattern "^(?:`regex')\("
gen numeric_calculate = ustrregexm(calculation, "`regex_pattern'")

// Build regex for force overrides
foreach mac in numeric string {
	if "``mac''" != "" {
		local force_regex : subinstr local `mac' " " "|", all
		gen `mac'_force = ustrregexm(name, "^(?:`force_regex')$")
	}
	else {
		gen `mac'_force = 0
	}
}

label define question_type_M ///
	1 "String" 2 "Select One" 3 "Select Multiple" ///
	4 "Numeric" 5 "Date" 6 "Datetime" 7 "GPS" ///
	-111 "Group Boundary" -222 "Note" -555 "Other"

gen question_type = .
label values question_type question_type_M

// String types
replace question_type = 1 if ///
	inlist(type, "text", "deviceid", "image", "geotrace", "photo", "audit", "text audit") ///
	| preloaded == 1 ///
	| (type == "calculate" & numeric_calculate == 0) ///
	| string_force == 1

// Select one
replace question_type = 2 if word(type, 1) == "select_one" ///
	& missing(question_type)

// Select multiple
replace question_type = 3 if word(type, 1) == "select_multiple"

// Numeric (default for unclassified non-date, non-text types)
replace question_type = 4 if ///
	(!inlist(type, "date", "text") & missing(question_type)) ///
	| numeric_force == 1

// Date
replace question_type = 5 if inlist(type, "date", "today")

// Datetime
replace question_type = 6 if inlist(type, "start", "end", "submissiondate")

// GPS
replace question_type = 7 if type == "geopoint"

// Structural / metadata types
replace question_type = -111 if ///
	inlist(type, "begin_group", "end_group", "begin_repeat", "end_repeat")
replace question_type = -222 if note == 1
replace question_type = -555 if missing(question_type)

// Drop completely unclassifiable types (below -111)
drop if question_type < -111

// Create order variable to preserve instrument ordering
gen order = _n

end

*===============================================================================
*	Phase 3: Parse Group/Repeat Structure
*===============================================================================

program define _cto2_parse_groups

syntax, qsframe(string) rawframe(string) groupsframe(string)

cwf `qsframe'
sort order

// --- Vectorized group assignment ------------------------------------
// Create indicator variables for group boundaries
gen is_begin_group = (type == "begin_group")
gen is_end_group = (type == "end_group")
gen is_begin_repeat = (type == "begin_repeat")
gen is_end_repeat = (type == "end_repeat")

// Running depth via cumulative sums
gen group_depth = sum(is_begin_group - is_end_group)
gen repeat_depth = sum(is_begin_repeat - is_end_repeat)

// Sequential group/repeat IDs (only increment at begin_ rows)
gen group = sum(is_begin_group)
// For questions before the first group, assign 0
replace group = 0 if group_depth == 0 & !is_begin_group

gen repeat_group = sum(is_begin_repeat)
// For questions outside any repeat group, assign 0
replace repeat_group = 0 if repeat_depth == 0 & !is_begin_repeat

// --- Assign group/repeat IDs correctly using stack approach ---------
// The vectorized running sum gives us depth, but we need the ID of the
// *innermost* group at each row. We still need a loop for this, but we
// only loop over begin/end boundary rows (much fewer than total rows).

// Reset -- we'll fill these properly
replace group = 0
replace repeat_group = 0

// Use a loop but track stack via locals (same logic, just cleaner)
local n_groups = 0
local n_repeats = 0
local groupstack 0
local repeatstack 0

forvalues i = 1/`c(N)' {

	if type[`i'] == "begin_group" {
		local ++n_groups
		local groupstack `groupstack' `n_groups'
	}
	else if type[`i'] == "end_group" {
		// Pop last element
		local groupstack : list groupstack - n_groups_pop
		local wc : word count `groupstack'
		if `wc' > 1 {
			local groupstack_new
			forvalues w = 1/`=`wc'-1' {
				local groupstack_new `groupstack_new' `:word `w' of `groupstack''
			}
			local groupstack `groupstack_new'
		}
		else {
			local groupstack 0
		}
	}
	else if type[`i'] == "begin_repeat" {
		local ++n_repeats
		local repeatstack `repeatstack' `n_repeats'
	}
	else if type[`i'] == "end_repeat" {
		local wc : word count `repeatstack'
		if `wc' > 1 {
			local repeatstack_new
			forvalues w = 1/`=`wc'-1' {
				local repeatstack_new `repeatstack_new' `:word `w' of `repeatstack''
			}
			local repeatstack `repeatstack_new'
		}
		else {
			local repeatstack 0
		}
	}

	// Assign current innermost group/repeat
	replace group = real(word("`groupstack'", -1)) in `i'
	replace repeat_group = real(word("`repeatstack'", -1)) in `i'
}

// --- Build groups frame ---------------------------------------------
// Extract begin_group and begin_repeat rows into the groups frame
cwf `groupsframe'

// Define groups frame structure
frame `qsframe' {

	local n_groups = 0
	local n_repeats = 0
	local groupstack_idx 0
	local repeatstack_idx 0

	// Create groups frame with proper structure
	frame `groupsframe' {
		clear
		gen int type = .
		gen strL name = ""
		gen strL label = ""
		gen strL repeat_count = ""
		gen int repetitions = .
		gen int instrument_row = .
		gen int index = .
		gen strL conditions = ""
		gen int within = .
		gen strL example_var = ""
		gen int layers_nested = 0
		gen strL keys = ""
		gen strL cumulative_con = ""
	}

	forvalues i = 1/`c(N)' {

		if type[`i'] == "begin_group" {

			local ++n_groups
			local groupstack_idx `groupstack_idx' `n_groups'

			// Find first real variable after this group boundary
			local j = `i' + 1
			local found_example = 0
			while `j' <= `c(N)' & !`found_example' {
				local qt = question_type[`j']
				if `qt' > 0 & `qt' != 7 {
					local found_example = 1
				}
				else {
					local ++j
				}
			}
			if !`found_example' local j = `i' + 1

			local parent = word("`groupstack_idx'", -2)
			if "`parent'" == "" local parent 0

			frame post `groupsframe' (1) (name[`i']) (labelStata[`i']) ///
				(repeat_count[`i']) (.) (`i') (`n_groups') ///
				(relevant[`i']) (`parent') (name[`j']) (0) ("") ("")

		}
		else if type[`i'] == "begin_repeat" {

			local ++n_repeats
			local repeatstack_idx `repeatstack_idx' `n_repeats'

			// Find first real variable
			local j = `i' + 1
			local found_example = 0
			while `j' <= `c(N)' & !`found_example' {
				local qt = question_type[`j']
				if `qt' > 0 & `qt' != 7 {
					local found_example = 1
				}
				else {
					local ++j
				}
			}
			if !`found_example' local j = `i' + 1

			local parent = word("`repeatstack_idx'", -2)
			if "`parent'" == "" local parent 0

			frame post `groupsframe' (2) (name[`i']) (labelStata[`i']) ///
				(repeat_count[`i']) (.) (`i') (`n_repeats') ///
				(relevant[`i']) (`parent') (name[`j']) (0) ("key") ("")

		}
		else if type[`i'] == "end_group" {
			local wc : word count `groupstack_idx'
			if `wc' > 1 {
				local groupstack_new
				forvalues w = 1/`=`wc'-1' {
					local groupstack_new `groupstack_new' `:word `w' of `groupstack_idx''
				}
				local groupstack_idx `groupstack_new'
			}
			else {
				local groupstack_idx 0
			}
		}
		else if type[`i'] == "end_repeat" {
			local wc : word count `repeatstack_idx'
			if `wc' > 1 {
				local repeatstack_new
				forvalues w = 1/`=`wc'-1' {
					local repeatstack_new `repeatstack_new' `:word `w' of `repeatstack_idx''
				}
				local repeatstack_idx `repeatstack_new'
			}
			else {
				local repeatstack_idx 0
			}
		}
	}
}

// --- Count repetitions from raw data variable names -----------------
cwf `groupsframe'

local n_repeats = 0
count if type == 2
local n_repeats = `r(N)'

if `n_repeats' > 0 {

	forvalues i = 1/`c(N)' {

		if type[`i'] != 2 continue

		local repeat_var = example_var[`i']

		// Walk up nesting to find actual data variable name
		local nest = within[`i']
		local layers = 0
		while `nest' != 0 {
			local ++layers
			replace layers_nested = `layers' in `i'

			local repeat_var `repeat_var'_1

			// Find the parent repeat group row
			levelsof _n if type == 2 & index == `nest', clean local(parent_row)

			// Track nesting info
			cap gen int nest_level_`layers' = .
			replace nest_level_`layers' = `nest' in `i'

			// Add parent key to this group's key chain
			replace keys = keys + " " + name[`parent_row'] + "_key" in `i'

			cap gen int nest_repeats_`layers' = .
			replace nest_repeats_`layers' = repetitions[`parent_row'] in `i'

			local nest = within[`parent_row']
		}

		// Add own key
		replace keys = keys + " " + name[`i'] + "_key" in `i'

		// Count repetitions using ds in raw data frame (replaces findregex)
		frame `rawframe' {
			cap ds `repeat_var'_*
			local matched_vars `r(varlist)'
			local max_num = 0
			foreach var in `matched_vars' {
				// Extract trailing number
				local num = real(ustrregexra("`var'", ".*_(\d+)$", "$1"))
				if !missing(`num') & `num' > `max_num' {
					local max_num = `num'
				}
			}
		}

		replace repetitions = `max_num' in `i'
	}

	// Record max nesting depth
	sum layers_nested, meanonly
	local nest_layers = `r(max)'
}

// --- Build cumulative relevance conditions --------------------------
// For groups (type == 1)
forvalues i = `c(N)'(-1)1 {

	if type[`i'] != 1 continue

	replace cumulative_con = conditions in `i'
	local w = within[`i']
	while `w' != 0 {
		// Find the row for this parent group
		levelsof _n if type == 1 & index == `w', clean local(parent_row)
		if "`parent_row'" != "" {
			local parent_con = conditions[`parent_row']
			if "`parent_con'" != "" {
				replace cumulative_con = "`parent_con'" + ///
					cond(!missing(cumulative_con) & cumulative_con != "", ///
					" & " + cumulative_con, "") in `i'
			}
			local w = within[`parent_row']
		}
		else {
			local w = 0
		}
	}
}

// For repeat groups (type == 2)
forvalues i = `c(N)'(-1)1 {

	if type[`i'] != 2 continue

	replace cumulative_con = conditions in `i'
	local w = within[`i']
	while `w' != 0 {
		levelsof _n if type == 2 & index == `w', clean local(parent_row)
		if "`parent_row'" != "" {
			local parent_con = conditions[`parent_row']
			if "`parent_con'" != "" {
				replace cumulative_con = "`parent_con'" + ///
					cond(!missing(cumulative_con) & cumulative_con != "", ///
					" & " + cumulative_con, "") in `i'
			}
			local w = within[`parent_row']
		}
		else {
			local w = 0
		}
	}
}

// --- Apply group conditions back to questions -----------------------
cwf `qsframe'

// Apply non-repeat group conditions
frame `groupsframe' {
	local g_n = `c(N)'
	forvalues i = 1/`g_n' {
		if type[`i'] == 1 {
			local g_idx = index[`i']
			local g_con = cumulative_con[`i']
			if "`g_con'" != "" {
				frame `qsframe': replace relevant = "`g_con'" + ///
					cond(!missing(relevant) & relevant != "", ///
					" & " + relevant, "") ///
					if group == `g_idx'
			}
		}
	}
}

// Apply repeat group conditions
frame `groupsframe' {
	forvalues i = 1/`g_n' {
		if type[`i'] == 2 {
			local r_idx = index[`i']
			local r_con = cumulative_con[`i']
			if "`r_con'" != "" {
				frame `qsframe': replace relevant = "`r_con'" + ///
					cond(!missing(relevant) & relevant != "", ///
					" & " + relevant, "") ///
					if repeat_group == `r_idx'
			}
		}
	}
}

// --- Build verbose relevance (expand variable references) -----------
cwf `qsframe'
clonevar long_relevant = relevant

forvalues i = 1/`c(N)' {
	local vname = name[`i']
	local varlabel = labelStata[`i']

	count if ustrregexm(relevant, "#\{`vname'\}")
	if `r(N)' == 0 continue

	replace long_relevant = ///
		ustrregexra(long_relevant, "#\{`vname'\}", "'`varlabel''")
}

// Add label for group type in groups frame
cwf `groupsframe'
label define group_type 1 "Standard Group" 2 "Repeat Group", replace
label values type group_type

end

*===============================================================================
*	Phase 4: Parse Choices Sheet
*===============================================================================

program define _cto2_parse_choices

syntax, choicesframe(string) instname(string) ///
	dk(integer) other(integer) refused(integer)

cwf `choicesframe'

// Import choices sheet
import excel "`instname'", firstrow clear sheet(choices)

// Standardize column name
cap rename listname list_name

// Keep essential columns
keep list_name name label

// Drop empty rows (replaces: missings dropobs, force)
egen _n_nonmiss = rownonmiss(_all), strok
drop if _n_nonmiss == 0
drop _n_nonmiss

tostring name, replace

// Keep only numeric choice values (drop non-standard labels)
drop if !regexm(name, "^[0-9\-]+$")

gen order = _n

// Clone name for potential missing value replacement
clonevar name1 = name

// Map special missing value codes
if `dk' != 1 replace name1 = ".d" if name == "`dk'"
if `refused' != 1 replace name1 = ".r" if name == "`refused'"
if `other' != 1 replace name1 = ".o" if name == "`other'"

// SurveyCTO replaces hyphens with underscores in variable names
replace name = subinstr(name, "-", "_", 1)

// Clean strings (single pass)
replace label = subinstr(label, "$", "#", .)
replace label = subinstr(label, `"""', "", .)
replace label = subinstr(label, char(10), "", .)
replace label = subinstr(label, char(13), "", .)

replace list_name = subinstr(list_name, " ", "", .)

// Clean name values
foreach var of varlist name name1 {
	replace `var' = subinstr(`var', char(10), "", .)
	replace `var' = subinstr(`var', char(13), "", .)
}

end

*===============================================================================
*	Phase 5: Expand Variables (select_multiple, GPS)
*===============================================================================

program define _cto2_expand_vars

syntax, qsframe(string) choicesframe(string) groupsframe(string) ///
	n_repeats(integer) [rename(string) deidvars(string)]

cwf `qsframe'
compress
drop if question_type < 0
sort order

gen within_order = .
clonevar original_name = name
clonevar desired_varname = name

if "`rename'" != "" {
	cap confirm variable new_name
	if !_rc {
		replace desired_varname = new_name if !missing(new_name) & new_name != ""
	}
	else {
		noisily display as result ///
			"Warning: {bf:rename} specified but no 'new_name' column found."
	}
}

// --- Expand select_multiple variables -------------------------------
gen n = _n
levelsof order if question_type == 3, clean local(mult_selects)

foreach i in `mult_selects' {

	levelsof n if order == `i', clean local(j)
	local vname = desired_varname[`j']
	local vallabel = type2[`j']

	frame `choicesframe' {
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
		frame `choicesframe': local value = name[`row']
		frame `choicesframe': local lab = label[`row']

		replace desired_varname = desired_varname + "_" + "`value'" ///
			if order == `i' & within_order == `s'
		replace name = name + "_" + "`value'" ///
			if order == `i' & within_order == `s'
		replace labelStata = "#{`vname'}: `lab'" ///
			if order == `i' & within_order == `s'
	}

	sort order within_order
	replace n = _n
}

// --- Expand GPS variables -------------------------------------------
levelsof order if question_type == 7, clean local(gps_vars)

foreach i in `gps_vars' {

	expand 4 if order == `i'
	bysort order (within_order): replace within_order = _n if order == `i'

	local s = 0
	foreach f in Accuracy Latitude Longitude Altitude {
		local ++s
		replace desired_varname = desired_varname + "`f'" ///
			if within_order == `s' & order == `i'
		replace name = name + "`f'" ///
			if within_order == `s' & order == `i'
		replace labelStata = labelStata + ": `f'" ///
			if within_order == `s' & order == `i'
	}
}

assert !missing(desired_varname)

// Remove duplicate variable names (keep first occurrence)
bysort desired_varname (order): keep if _n == 1
sort order within_order
replace n = _n

// --- Build variable lists for reshape -------------------------------
gen vlist = name
gen desired_vlist = desired_varname
gen shape_vlist = ""

if `n_repeats' > 0 {

	// Get repeat group info into questions frame
	frame `groupsframe' {
		// Build a temporary dataset of repeat group info
		tempname rg_info
		frame copy `groupsframe' `rg_info'
		frame `rg_info' {
			keep if type == 2
			keep index repetitions within layers_nested
			rename index repeat_group

			// Also keep nesting variables
			cap ds nest_level_* nest_repeats_*
			if !_rc {
				local nest_vars `r(varlist)'
			}
		}
	}

	frlink m:1 repeat_group, frame(`rg_info')
	frget repetitions within layers_nested, from(`rg_info')
	cap frget `nest_vars', from(`rg_info')
	cap drop `rg_info'

	// Build vlist/desired_vlist with repeat numbering
	frame `groupsframe' {

		count if type == 2
		local n_rg = `r(N)'

		forvalues r = 1/`n_rg' {

			// Only process repeat groups that actually have repetitions
			local reps = repetitions[`=_n' + 0] if type == 2 & index == `r'
			levelsof repetitions if type == 2 & index == `r', clean local(reps)
			levelsof layers_nested if type == 2 & index == `r', clean local(layers)
			if "`layers'" == "" local layers 0

			forvalues l = `layers'(-1)0 {

				frame `qsframe' {

					tempvar prev_vlist prev_desired
					clonevar `prev_vlist' = vlist
					clonevar `prev_desired' = desired_vlist

					// Create shape stubs with trailing underscore
					replace shape_vlist = ///
						ustrregexra(`prev_desired', "(\w+)", "$1_") ///
						if repeat_group == `r'

					replace vlist = "" if repeat_group == `r'
					replace desired_vlist = "" if repeat_group == `r'
				}

				if `l' == 0 {
					local layer_repeats `reps'
				}
				else {
					frame `groupsframe' {
						levelsof nest_repeats_`l' if type == 2 & index == `r', ///
							clean local(layer_repeats)
					}
				}

				if "`layer_repeats'" == "" | "`layer_repeats'" == "." {
					local layer_repeats 0
				}

				forvalues c = 1/`layer_repeats' {
					frame `qsframe' {
						replace vlist = vlist + " " + ///
							ustrregexra(`prev_vlist', "(\b\w+\b)", "$1_`c' ") ///
							if repeat_group == `r'
						replace desired_vlist = desired_vlist + " " + ///
							ustrregexra(`prev_desired', "(\b\w+\b)", "$1_`c' ") ///
							if repeat_group == `r'
					}
				}
			}
		}
	}

	// Drop variables with 0 repetitions
	drop if repetitions == 0 & repeat_group > 0
}

// Build variable count and stub for command generation
gen num_vars = wordcount(vlist)
gen var_stub = cond(num_vars == 1, vlist, "\`var'")

// --- Build per-repeat-group variable lists --------------------------
// These are stored as locals via c_local for the write phases

end

*===============================================================================
*	Phase 6: Build Cleaning Commands
*===============================================================================

program define _cto2_build_commands

syntax, qsframe(string) datestyle(string) [rename(string)]

cwf `qsframe'

local brek = char(10)
local tab = char(9)
local tab2 `tab'`tab'
local hbanner = "*" + ("=") * 65
local lbanner = "*" + ("-") * 65

// --- Variable label command -----------------------------------------
gen label_command = "label variable " + var_stub + `" ""' + labelStata + `"""'

// --- Format/type command --------------------------------------------
gen format_command = ""

// String variables: tostring + clean missing dots
replace format_command = ///
	"tostring " + var_stub + ", replace" + "`brek'" + ///
	`"replace "' + var_stub + `" = "" if "' + var_stub + `" == ".""' ///
	if question_type == 1 | (question_type == 3 & within_order == 1)

// Numeric variables: destring
replace format_command = "destring " + var_stub + ", replace" ///
	if inlist(question_type, 2, 4, 7) ///
	| (question_type == 3 & within_order > 1)

// Date variables
if "`datestyle'" == "YMD" {
	// ISO format -- try YMD first
	replace format_command = ///
		"tempvar date" + "`brek'" + ///
		"clonevar \`date' = " + var_stub + "`brek'" + ///
		`"replace "' + var_stub + `" = """' + "`brek'" + ///
		"if _rc replace " + var_stub + " = ." + "`brek'" + ///
		"destring " + var_stub + ", replace" + "`brek'" + ///
		`"replace "' + var_stub + ///
		`" = date(\`date', "YMD", 2030) if !missing(\`date')"' + "`brek'" + ///
		`"format "' + var_stub + `" %td"' ///
		if question_type == 5
}
else {
	// DMY or MDY
	replace format_command = ///
		"tempvar date" + "`brek'" + ///
		"clonevar \`date' = " + var_stub + "`brek'" + ///
		`"replace "' + var_stub + `" = """' + "`brek'" + ///
		"if _rc replace " + var_stub + " = ." + "`brek'" + ///
		"destring " + var_stub + ", replace" + "`brek'" + ///
		`"replace "' + var_stub + ///
		`" = date(\`date', "`datestyle'", 2030) if !missing(\`date')"' + "`brek'" + ///
		`"format "' + var_stub + `" %td"' ///
		if question_type == 5
}

// Datetime variables
if "`datestyle'" == "YMD" {
	replace format_command = ///
		"tempvar date" + "`brek'" + ///
		"clonevar \`date' = " + var_stub + "`brek'" + ///
		`"replace "' + var_stub + `" = """' + "`brek'" + ///
		"if _rc replace " + var_stub + " = ." + "`brek'" + ///
		"destring " + var_stub + ", replace" + "`brek'" + ///
		`"replace "' + var_stub + ///
		`" = clock(\`date', "YMDhms", 2030) if !missing(\`date')"' + "`brek'" + ///
		`"format "' + var_stub + `" %tc"' ///
		if question_type == 6
}
else {
	replace format_command = ///
		"tempvar date" + "`brek'" + ///
		"clonevar \`date' = " + var_stub + "`brek'" + ///
		`"replace "' + var_stub + `" = """' + "`brek'" + ///
		"if _rc replace " + var_stub + " = ." + "`brek'" + ///
		"destring " + var_stub + ", replace" + "`brek'" + ///
		`"replace "' + var_stub + ///
		`" = clock(\`date', "`datestyle'hms", 2030) if !missing(\`date')"' + "`brek'" + ///
		`"format "' + var_stub + `" %tc"' ///
		if question_type == 6
}

// --- Value label command --------------------------------------------
gen values_command = "label values " + var_stub + " " + type2 ///
	if !missing(type2) & !inlist(question_type, 3, 7)

// --- Notes command --------------------------------------------------
gen notes_command = "notes " + var_stub + ": " + labelEnglishen + ///
	"`brek'" + "notes " + var_stub + ": relevance conditions: " + relevant

// --- Characteristics command ----------------------------------------
gen char_command = ///
	"char " + var_stub + "[qtext] " + labelEnglishen + "`brek'" + ///
	"char " + var_stub + "[logic] " + relevant + "`brek'" + ///
	"char " + var_stub + "[verbose_logic] " + long_relevant + "`brek'" + ///
	"char " + var_stub + "[preloaded] " + cond(preloaded == 1, "1", "0")

// --- Assemble full command ------------------------------------------
// Each variable block: wrapped in cap confirm variable / if !_rc { }
// For multi-value variables (repeat groups), use foreach loop

gen command = ""

// Single-variable commands: wrap in confirm/if block
replace command = ///
	"`lbanner'" + "`brek'" + ///
	"*`tab'" + name + ": " + labelStata + "`brek'" + ///
	"`lbanner'" + "`brek'" + ///
	"cap confirm variable " + var_stub + "`brek'" + ///
	"if !_rc {" + "`brek'" + ///
	"`tab'" + label_command + "`brek'" + ///
	"`tab'" + format_command + "`brek'" + ///
	cond(!missing(values_command), "`tab'" + values_command + "`brek'", "") + ///
	"`tab'" + notes_command + "`brek'" + ///
	"`tab'" + char_command + "`brek'" + ///
	"}" ///
	if num_vars == 1

// Multi-variable commands (repeat group vars): use foreach loop
replace command = ///
	"`lbanner'" + "`brek'" + ///
	"*`tab'" + name + ": " + labelStata + "`brek'" + ///
	"`lbanner'" + "`brek'" + ///
	"foreach var of varlist " + vlist + " {" + "`brek'" + ///
	"`tab'" + "cap confirm variable \`var'" + "`brek'" + ///
	"`tab'" + "if _rc continue" + "`brek'" + ///
	"`tab'" + label_command + "`brek'" + ///
	"`tab'" + format_command + "`brek'" + ///
	cond(!missing(values_command), "`tab'" + values_command + "`brek'", "") + ///
	"`tab'" + notes_command + "`brek'" + ///
	"`tab'" + char_command + "`brek'" + ///
	"}" ///
	if num_vars > 1

// --- Rename commands (if applicable) --------------------------------
if "`rename'" != "" {
	cap confirm variable new_name
	if !_rc {
		gen rename_command = "`brek'" + ///
			`"cap rename "' + var_stub + ///
			`" \`=ustrregexrf(""' + var_stub + ///
			`"", "^"' + name + `"", ""' + desired_varname + `"")'"' ///
			if !missing(new_name) & new_name != name
		replace command = command + rename_command if !missing(rename_command)
	}
}

end

*===============================================================================
*	Phase 7: Write Import Dofile
*===============================================================================

program define _cto2_write_import

syntax, qsframe(string) choicesframe(string) ///
	dofile(string) dataname(string) instname(string) ///
	datestyle(string) dk(integer) other(integer) refused(integer) ///
	[keep(string)]

local brek = char(10)
local tab = char(9)
local tab2 = "`tab'`tab'"
local hbanner = "*" + ("=") * 65
local lbanner = "*" + ("-") * 65
local version = max(`c(stata_version)', 17)

// --- Build value label commands from choices frame ------------------
frame `choicesframe' {

	local labels_counter = 0
	levelsof list_name, local(choice_lists) clean separate( )

	foreach listo in `choice_lists' {

		local cmd `"label define `listo' ///"'
		local cmd `"`cmd'`brek'"'
		levelsof order if list_name == "`listo'", clean local(values)
		local lvs = `r(r)'

		// Skip lists with non-numeric values
		if ustrregexm("`values'", "[^0-9\s]+") == 1 continue

		local t = 0
		foreach row_i in `values' {
			local ++t
			local slash "///"
			local linebreak "`brek'"
			if `t' == `lvs' {
				local linebreak
				local slash
			}
			levelsof name1 if order == `row_i', local(vals) clean
			levelsof label if order == `row_i', local(lab) clean
			local cmd `"`cmd'`tab'`vals' "`lab'" `slash'`linebreak'"'
		}

		local ++labels_counter
		local lab`labels_counter' "`cmd'"
	}
}

// --- Open file and write --------------------------------------------
cap file close myfile
file open myfile using "`dofile'", write text replace

// Header
file write myfile ///
	"/*" ///
	_n "Title: Import Dofile for `macval(instname)'" ///
	_n "Date Created: `c(current_date)'" ///
	_n "Author: `c(username)'" ///
	_n "Generated by: cto2 v2.0" ///
	_n "Note: This dofile was auto-generated. Edit with care." ///
	_n "*/" _n(2)

// Setup
file write myfile ///
	"`hbanner'" ///
	_n "* 	Setup" _n ///
	"`hbanner'" ///
	_n(2) "clear all" _n "version `version'" _n "set more off" ///
	_n "set maxvar 120000" ///
	_n "cap log close" _n "set trace off" _n "set linesize 200" _n(2)

// Macros
file write myfile ///
	"`hbanner'" ///
	_n "* 	Macros" _n ///
	"`hbanner'" ///
	_n(2) ///
	"local" _tab `"today = date(c(current_date), "`datestyle'")"' _n ///
	"local" _tab `"todaystr = string(\`today', "%td")"' _n(2)

// Import
file write myfile ///
	"`hbanner'" ///
	_n "* 	Import" _n ///
	"`hbanner'" _n(2) ///
	`"import delimited "`macval(dataname)'", clear bindquote(strict) "' ///
	`"case(preserve) maxquotedrows(unlimited)"' _n(2)

// Value Labels
file write myfile ///
	"`hbanner'" ///
	_n "* 	Labels" _n ///
	"`hbanner'" _n(2)

forvalues i = 1/`labels_counter' {
	file write myfile `"`lab`i''"' _n(2)
}

// Variable Cleaning
file write myfile ///
	"`hbanner'" ///
	_n "* 	Clean" _n ///
	"`hbanner'" _n(2)

frame `qsframe' {
	forvalues i = 1/`c(N)' {
		file write myfile (command[`i']) _n(2)
	}
}

// System Variables
file write myfile ///
	"`hbanner'" ///
	_n "* 	Survey Version" _n ///
	"`hbanner'" ///
	_n(2) `"destring formdef_version, replace"' _n ///
	`"label variable formdef_version "survey version""' _n(2)

file write myfile ///
	"`hbanner'" ///
	_n "* 	Survey Unique ID" _n ///
	"`hbanner'" ///
	_n(2) `"cap rename KEY key"' _n ///
	`"label variable key "survey uid""' _n(2)

// Submission Date
file write myfile ///
	"`hbanner'" ///
	_n "* 	Submission Date" _n ///
	"`hbanner'" ///
	_n(2) `"cap rename SubmissionDate submissiondate"' _n ///
	`"tempvar date"' _n ///
	`"clonevar \`date' = submissiondate"' _n ///
	`"cap replace submissiondate = "" "' _n ///
	`"if _rc replace submissiondate = ."' _n ///
	`"destring submissiondate, replace"' _n

if "`datestyle'" == "YMD" {
	file write myfile ///
		`"replace submissiondate = clock(\`date', "YMDhms", 2030)"' _n
}
else {
	file write myfile ///
		`"replace submissiondate = clock(\`date', "`datestyle'hms", 2030)"' _n
}

file write myfile ///
	`"cap format submissiondate %tc"' _n ///
	`"cap label variable submissiondate "time of survey submission""' _n(2)

// Missing Values
local mv
local sep
if `dk' != 1 {
	local mv `dk' = .d
	local sep " \ "
}
if `refused' != 1 {
	local mv `mv'`sep'`refused' = .r
	if "`sep'" == "" local sep " \ "
}
if `other' != 1 {
	local mv `mv'`sep'`other' = .o
}

if "`mv'" != "" {
	file write myfile ///
		"`hbanner'" ///
		_n "* 	Replacing Missing Values" _n ///
		"`hbanner'" _n(2) ///
		`"mvdecode _all, mv(`mv')"' _n(2) ///
		`"cap drop __*"'
}

file close myfile

end

*===============================================================================
*	Phase 8: Write Reshape Dofile
*===============================================================================

program define _cto2_write_reshape

syntax, qsframe(string) groupsframe(string) ///
	reshapefile(string) instname(string) ///
	savefolder(string) n_repeats(integer) ///
	[frgetvars(string) deidvars(string) keep(string)]

local brek = char(10)
local tab = char(9)
local tab2 = "`tab'`tab'"
local hbanner = "*" + ("=") * 65
local lbanner = "*" + ("-") * 65

// Extract short instrument filename
local file_short = ustrregexrf("`instname'", "^.*[/\\]", "")

// --- Build variable lists -------------------------------------------
// varlist_0: survey-level variables
// varlist_i: variables for repeat group i
// shapelist_i: reshape stub variables for repeat group i

frame `qsframe' {

	// Survey-level variables
	local varlist_0
	forvalues i = 1/`c(N)' {
		if repeat_group[`i'] == 0 {
			local vn = desired_varname[`i']
			local varlist_0 `varlist_0' `vn'
		}
	}
	if "`deidvars'" != "" {
		local varlist_0 : list varlist_0 - deidvars
	}

	// Per-repeat-group variables
	forvalues rg = 1/`n_repeats' {

		local varlist_`rg'
		local shapelist_`rg'

		forvalues i = 1/`c(N)' {
			if repeat_group[`i'] == `rg' {
				local vn = desired_varname[`i']
				local varlist_`rg' `varlist_`rg'' `vn'

				local sv = shape_vlist[`i']
				if "`sv'" != "" {
					local shapelist_`rg' `shapelist_`rg'' `sv'
				}
			}
		}

		if "`deidvars'" != "" {
			local varlist_`rg' : list varlist_`rg' - deidvars
		}

		// Deduplicate shape list
		local shapelist_`rg' : list uniq shapelist_`rg'
	}
}

// --- Open file and write header -------------------------------------
cap file close myfile2
file open myfile2 using "`reshapefile'", write text replace

file write myfile2 ///
	"/*" ///
	_n "Title: Reshape Dofile for `file_short'" ///
	_n "Date Created: `c(current_date)'" ///
	_n "Author: `c(username)'" ///
	_n "Generated by: cto2 v2.0" ///
	_n "Note: This dofile was auto-generated. Edit with care." ///
	_n "*/" _n(3)

// Variable list macros
file write myfile2 ///
	"`hbanner'" ///
	_n "* 	Handy Macros" _n ///
	"`hbanner'" _n(2) ///
	"`lbanner'" _n ///
	"*	Reshapable Variables" _n ///
	"`lbanner'" _n(2)

file write myfile2 `"local varlist_0 `varlist_0'"' _n(2)

frame `groupsframe' {
	forvalues i = 1/`c(N)' {
		if type[`i'] != 2 continue
		local idx = index[`i']
		local reps = repetitions[`i']
		if `reps' != 0 {
			file write myfile2 "local shape_`idx' `shapelist_`idx''" _n(2)
			file write myfile2 "local varlist_`idx' `varlist_`idx''" _n(2)
		}
	}
}

// --- Reshape section ------------------------------------------------
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

// --- Write reshape commands for each repeat group -------------------
frame `groupsframe' {

	forvalues i = 1/`c(N)' {

		if type[`i'] != 2 continue
		if repetitions[`i'] == 0 continue

		local rg_idx = index[`i']
		local all_keys key
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
					`"local reshape_vars \`shape_`rg_idx''"' _n ///
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
				local regex `""_[0-9]+_@ ?", "_@ ""'
			}

			if `counter' == 0 {
				local to `rg_idx'
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

				// Use ds instead of findregex (dependency removed)
				file write myfile2 ///
					`"preserve"' _n(2) ///
					`"local regex_stubs = ustrregexra(strtrim(stritrim("\`shape_`rg_idx''")), " ", "|", .)"' _n ///
					`"local reshaped_vars"' _n ///
					`"ds *"' _n ///
					`"foreach _v in \`r(varlist)' {"' _n ///
					_tab `"if ustrregexm("\`_v'", "^((\`regex_stubs')\d+)$") {"' _n ///
					_tab _tab `"local reshaped_vars \`reshaped_vars' \`_v'"' _n ///
					_tab "}" _n ///
					"}" _n ///
					`"keep \`reshaped_vars' `all_keys'"' _n(2)
			}

			// Reshape long + rename trailing underscores (Stata 17 syntax)
			file write myfile2 ///
				"reshape long \`reshape_vars', i(`all_keys') j(`to_key')" _n ///
				`"rename *_ *"' _n

			local all_keys `all_keys' `to_key'

			if `k' == 0 {

				// Drop empty obs (replaces missings dropobs)
				file write myfile2 ///
					`"local reshape_vars = ustrregexra("\`reshape_vars'", "_@", "", .)"' _n ///
					"frame put \`reshape_vars' `all_keys', into(`target_name')" _n ///
					"cwf `target_name'" _n ///
					`"egen _n_nonmiss = rownonmiss(\`reshape_vars'), strok"' _n ///
					`"drop if _n_nonmiss == 0"' _n ///
					`"drop _n_nonmiss"' _n ///
					"isid `all_keys'" _n(2) ///
					`"foreach var of varlist \`varlist_`rg_idx'' {"' _n(2) ///
					_tab `"cap label variable \`var' "\`X_\`var''""' _n ///
					_tab `"cap label values \`var' "\`L_\`var''""' _n(2) ///
					"}" _n(2) ///
					"keep \`varlist_`rg_idx'' \`added_vars' `all_keys'" _n(2) ///
					`"frame survey: restore"' _n(2)

				// frget from parent frames
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

				// Save
				file write myfile2 "compress" _n ///
					`"label data "`target_desc' data from `file_short'""' _n ///
					`"save "`macval(savefolder)'/`target_name'.dta", replace"' _n(2) ///
					"cwf survey" _n ///
					`"drop \`reshaped_vars'"'
			}
		}
	}
}

// Survey-level save
file write myfile2 _n(2) ///
	"`hbanner'" ///
	_n "* 	Survey-Level" _n ///
	"`hbanner'" _n(2) ///
	`"keep \`varlist_0' key submissiondate formdef_version `keep'"' _n ///
	"compress" _n ///
	`"label data "survey-level data from `file_short'""' _n ///
	`"save "`macval(savefolder)'/survey.dta", replace"'

file close myfile2

end
