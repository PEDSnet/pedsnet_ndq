## Run this script (Source button) to render one HTML per site.
## Outputs land in reporting/output/

library(quarto)

sites     <- c("colorado", "seattle", "stanford", "cchmc", "lurie", "chop")
db_version <- "v61"

dir.create("output", showWarnings = FALSE)

for (s in sites) {
  message("Rendering: ", s)
  out_file <- paste0("pipeline_comp_", s, "_", db_version, ".html")
  quarto_render(
    input          = "og_vs_newpipeline_ndq_comp.qmd",
    execute_params = list(site = s, db_version = db_version),
    output_file    = out_file
  )
  file.rename(out_file, file.path("output", out_file))
}

message("Done. Reports written to: reporting/output/")
