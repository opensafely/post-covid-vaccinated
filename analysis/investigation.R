# Load data --------------------------------------------------------------------

df <- arrow::read_feather(file = "output/input_investigate.feather")

# Describe data ----------------------------------------------------------------

sink("output/describe_input_investigate_studydefinition.txt")
print(Hmisc::describe(df))
sink()