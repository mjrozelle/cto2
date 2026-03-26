{smcl}
{* *! version 2.10 26Mar2026}{...}
{hline}
{cmd:help cto2}
{hline}

{title:Title}

{p2colset 5 17 19 2}{...}
{p2col :{hi:cto2} {hline 2}}Import and clean SurveyCTO data{p_end}
{p2colreset}{...}

{title:Syntax}

{p 8 17 2}
{cmd:cto2}
{cmd:,}
{opt INSTname:(filepath)}
{opt DATAname:(filepath)}
{opt DOfile:(filepath)}
[{it:options}]

{synoptset 27 tabbed}{...}
{synopthdr}
{synoptline}
{syntab :Required}
{synopt :{opt INSTname:(filepath)}}filepath to XLSform instrument (.xlsx){p_end}
{synopt :{opt DATAname:(filepath)}}filepath to wide-format .csv data exported from SurveyCTO{p_end}
{synopt :{opt DOfile:(filepath)}}filepath to import dofile to be created{p_end}

{syntab :Reshaping}
{synopt :{opt RESHAPEfile:(filepath)}}filepath to reshape dofile; requires {opt SAVEfolder()}{p_end}
{synopt :{opt SAVEfolder:(filepath)}}folder for saving reshaped datasets and metadata{p_end}
{synopt :{opt FRGETVARS(namelist)}}variables to copy from parent frames to all reshaped datasets{p_end}
{synopt :{opt KEEP(namelist)}}extra survey-level variables to retain (e.g. SurveyCTO system vars not in instrument){p_end}

{syntab :Variable formatting}
{synopt :{opt IDENTIFIERS(namelist)}}survey unique identifier(s); usually "key"{p_end}
{synopt :{opt STRING(namelist)}}force named variables to string format{p_end}
{synopt :{opt NUMERIC(namelist)}}force named variables to numeric format{p_end}
{synopt :{opt RENAME}}rename variables using "new_name" column from instrument{p_end}

{syntab :Date format}
{synopt :{opt AMERICAN}}dates exported as MM/DD/YYYY{p_end}
{synopt :{opt ISO}}dates exported as YYYY-MM-DD{p_end}
{synopt :}(default: DD/MM/YYYY){p_end}

{syntab :Missing values}
{synopt :{opt DK(integer)}}code for "don't know" responses; recoded to {cmd:.d}{p_end}
{synopt :{opt OTHER(integer)}}code for "other (specify)" responses; recoded to {cmd:.o}{p_end}
{synopt :{opt REFUSED(integer)}}code for "refused to answer" responses; recoded to {cmd:.r}{p_end}

{syntab :Deidentification}
{synopt :{opt DEIDVARS(namelist)}}variables to drop from all output datasets{p_end}

{syntab :Output control}
{synopt :{opt REPLACE}}overwrite existing dofiles{p_end}
{synopt :{opt CODEBOOK}}export a CSV codebook to {opt SAVEfolder}; requires {opt SAVEfolder()}{p_end}
{synoptline}

{title:Description}

{pstd}
{cmd:cto2} reads a SurveyCTO XLSform instrument (.xlsx) and wide-format CSV data export, then generates Stata dofiles that import, label, format, and optionally reshape the data. It automates:

{p 8 12 2}- Variable labeling from instrument question text{p_end}
{p 8 12 2}- Value labeling from the choices sheet{p_end}
{p 8 12 2}- Type detection and formatting (string, numeric, date, datetime, GPS, select_one, select_multiple){p_end}
{p 8 12 2}- Select_multiple expansion into binary indicator variables{p_end}
{p 8 12 2}- GPS variable expansion (Accuracy, Latitude, Longitude, Altitude){p_end}
{p 8 12 2}- Nested repeat group reshaping from wide to long{p_end}
{p 8 12 2}- Relevance condition and question text storage as variable characteristics{p_end}
{p 8 12 2}- Special missing value recoding (don't know, refused, other){p_end}

{pstd}
The generated dofiles are designed to be inspected and edited before running. This gives you full transparency over the cleaning process.

{title:Options}

{dlgtab:Required}

{phang}
{opt INSTname(filepath)} specifies the path to the Excel (.xlsx) survey instrument. The file must contain a "survey" sheet and a "choices" sheet following the XLSform standard.

{phang}
{opt DATAname(filepath)} specifies the path to the .csv data file exported from SurveyCTO in wide format.

{phang}
{opt DOfile(filepath)} specifies the path where the import dofile will be written.

{dlgtab:Reshaping}

{phang}
{opt RESHAPEfile(filepath)} creates a second dofile that reshapes repeat group data from wide to long format. Requires {opt SAVEfolder()}. Each repeat group produces a separate .dta file; a survey-level dataset is also saved.

{phang}
{opt SAVEfolder(filepath)} specifies the directory for saving reshaped datasets ({it:survey.dta}, {it:repeat_group_name.dta}) and metadata files ({it:survey_metadata.dta}, {it:group_metadata.dta}). Created automatically if it does not exist.

{phang}
{opt FRGETVARS(namelist)} lists survey-level variables to copy into all reshaped repeat group datasets via {cmd:frlink}/{cmd:frget}.

{phang}
{opt KEEP(namelist)} lists additional variables to retain in the survey-level dataset during reshape. Use this for SurveyCTO system variables (e.g. {it:caseid}) that do not appear in the instrument.

{dlgtab:Variable formatting}

{phang}
{opt STRING(namelist)} forces named variables to be treated as string, overriding automatic type detection.

{phang}
{opt NUMERIC(namelist)} forces named variables to be treated as numeric, overriding automatic type detection.

{phang}
{opt RENAME} renames variables using values from the "new_name" column in the survey sheet of the instrument. If the column does not exist, a warning is displayed.

{dlgtab:Date format}

{phang}
{opt AMERICAN} specifies that dates in the data are formatted as MM/DD/YYYY. The default is DD/MM/YYYY.

{phang}
{opt ISO} specifies that dates in the data are formatted as YYYY-MM-DD (ISO 8601). Mutually exclusive with {opt AMERICAN}.

{dlgtab:Missing values}

{phang}
{opt DK(integer)} specifies the integer code used for "don't know" responses. These are recoded to {cmd:.d} (Stata extended missing) in the generated dofile. Default: not recoded.

{phang}
{opt OTHER(integer)} specifies the integer code used for "other (specify)" responses. Recoded to {cmd:.o}. Default: not recoded.

{phang}
{opt REFUSED(integer)} specifies the integer code used for "refused to answer" responses. Recoded to {cmd:.r}. Default: not recoded.

{pstd}
All three codes must be distinct when specified.

{dlgtab:Codebook}

{phang}
{opt CODEBOOK} exports a CSV file ({it:codebook.csv}) to {opt SAVEfolder} containing variable names, labels, types, value label names, and group assignments. Requires {opt SAVEfolder()}.

{title:Output files}

{pstd}
{cmd:cto2} generates the following files:

{p 8 12 2}1. {bf:Import dofile} ({opt DOfile}): imports CSV data, applies value labels, variable labels, type formatting, date conversion, characteristics, and missing value recoding.{p_end}

{p 8 12 2}2. {bf:Reshape dofile} ({opt RESHAPEfile}, optional): reshapes repeat group variables from wide to long, saves separate datasets per repeat group, and maintains frame linkages for cross-level variable access.{p_end}

{p 8 12 2}3. {bf:survey_metadata.dta} ({opt SAVEfolder}, optional): variable-level codebook with question text, type, group, value labels, and cleaning commands.{p_end}

{p 8 12 2}4. {bf:group_metadata.dta} ({opt SAVEfolder}, optional): group and repeat group structure with nesting, relevance conditions, and repetition counts.{p_end}

{title:Requirements}

{pstd}
Stata 17 or later. No external package dependencies.

{title:Examples}

{phang}{cmd:. cto2, instname("survey.xlsx") dataname("data.csv") dofile("import.do")}{p_end}

{phang}{cmd:. cto2, instname("survey.xlsx") dataname("data.csv") dofile("import.do") reshapefile("reshape.do") savefolder("clean_data") replace}{p_end}

{phang}{cmd:. cto2, instname("survey.xlsx") dataname("data.csv") dofile("import.do") dk(-999) refused(-777) other(-555) iso}{p_end}

{title:Author}

{pstd}
Michael Rozelle, Wageningen University and Research, The Netherlands.
Email: {browse "mailto:michael.rozelle@wur.nl":michael.rozelle@wur.nl}
{p_end}
