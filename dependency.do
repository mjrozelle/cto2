local dependencies findregex missings
local aux findregex missings

foreach dependency in `dependencies' {
	
	gettoken 1 aux : aux
	
	which `1'
	if _rc == 111 {
		
		ssc install `dependency'
		
	}
	
}
