{smcl}
{* *! version 1.00 11Apr2024}{...}
{hline}
{cmd:help cto2}
{hline}

{title:Title}

{p2colset 5 17 19 2}{...}
{p2col :{hi:cto2} {hline 2}}Enhanced SurveyCTO Data Processing{p_end}
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
{syntab :Necessary}
{synopt :{opt INSTname:string(filepath)}}filepath to Excel survey instrument{p_end}
{synopt :{opt DATAname:string(filepath)}}filepath to .csv SurveyCTO-exported data{p_end}
{synopt :{opt DOfile:string(filepath)}}filepath to import dofile to be created{p_end}

{syntab :Optional}
{synopt :{opt RESHAPEfile:(filepath)}}filepath to create a reshape dofile for long format{p_end}
{synopt :{opt IDENTIFIERS(namelist)}}specify survey unique identifier (usually "key"){p_end}
{synopt :{opt AMERICAN}}read dates in MM/DD/YYYY format{p_end}
{synopt :{opt SAVEfolder:(filepath)}}filepath to the folder for saving datasets{p_end}
{synopt :{opt FRGETVARS(namelist)}}variables to copy to all reshaped datasets{p_end}
{synopt :{opt DEIDVARS(namelist)}}variables to remove for deidentification{p_end}
{synopt :{opt DK(integer)}}value for "don't know" responses{p_end}
{synopt :{opt OTHER(integer)}}value for "other (specify)" responses{p_end}
{synopt :{opt REFUSED(integer)}}value for "refused to answer" responses{p_end}
{synopt :{opt RENAME}}rename variables as specified in "new_name" column of instrument{p_end}

{title:Description}

{pstd}
{cmd:cto2} is a Stata command for streamlined processing of SurveyCTO data, especially useful for surveys designed in Excel. It reads the survey instrument and the SurveyCTO-exported .csv data, then creates a dofile for importing and initially cleaning the data.

{pstd}
It includes advanced features for data management, such as reshaping data to long format, identifying unique survey identifiers, handling different date formats, and specifying folders for saving processed datasets. Additionally, it can copy certain variables to all reshaped datasets, remove identifiers for deidentification, and handle special response values.

{title:Remarks}

{pstd}
{cmd:cto2} enhances data cleaning and preprocessing by automating tedious tasks and reducing the risk of manual errors. Its options allow for flexible data manipulation and organization, fitting various research and analysis needs.

{pstd}
For effective use, it's advised to include a "new_name" column in the Excel instrument if variable renaming is desired. This ensures consistent and clear variable names across the dataset, aiding in straightforward analysis and reporting.

{pstd}
The command also supports detailed handling of special survey response categories like "don't know," "other," and "refused to answer," providing clear data interpretation and analysis.

{pstd}
In summary, {cmd:cto2} offers a comprehensive and efficient approach to SurveyCTO data processing, ensuring accurate and user-friendly dataset preparation for analysis.

{title:Author}

{pstd}
Michael Rozelle, Wageningen University and Research, The Netherlands.
Email: {browse "mailto:michael.rozelle@wur.nl":michael.rozelle@wur.nl}
{p_end}