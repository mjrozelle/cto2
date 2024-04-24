local dependencies findregex missings valuesof moremata
local aux findregex missings valuesof mf_mm_diff.hlp

foreach dependency in `dependencies' {
	
	gettoken 1 aux : aux
	
	cap which `1'
	if _rc == 111 {
		
		ssc install `dependency'
		
	}
	
}
