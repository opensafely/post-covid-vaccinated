analyses = "main"
population = "vaccinated"

analyses = "subgroups"
population = "vaccinated"

analyses = c("main", "subgroups")
population = c("vaccinated", "electively_unvaccinated")
for(i in analyses)
{
  for(j in population)
  {
    rmarkdown::render("analysis/compiled_table2_results.Rmd",
                      output_file=paste0("table_2_",i,"_", j),output_dir="output")
  }
}
