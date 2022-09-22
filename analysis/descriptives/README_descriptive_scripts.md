# Post-Covid-Events Descriptive Scripts

## Stage2_missing_table1.R

* Reads in input dataset and generates a missing data table.
* Generates a table of minimum and maximum dates for all date variables. 
* Generates descriptive Table 1 and includes suppression control (redacting counts <= 5).

## table_2.R

* Requires: active analyses, end dates table and input dataset.
* Uses the active analyses table and creates a Table of person days of follow up, unexposed person days and event counts. 
* Redacts counts <= 5.

## venn_diagram.R

* Generates a table of event counts by data source (primary care / secondary care /deaths).
* Any counts <= 5 are merged into the highest cell for that outcome (excluding total) so that the total counts are the same, but the results are non-disclosive.
* Table can be outputted and the script "external_venn_script.R" can then be used to generate the venn diagrams. 

## external_venn_script.R

* Reads in the table generated by "venn_diagram.R" and creates a venn diagram for each outcome (row) in the table.