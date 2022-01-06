# Compile and render a report for metadata (annotations) across projects
USERNAME = ${USERNAME} 
PASSWORD = ${PASSWORD}

# Regenerate will calculate a new metadata summary before rendering
updated_report:
  Rscript -e "rmarkdown::render('build_report.Rmd',params=list(update = TRUE, username = ${USERNAME}, password = ${PASSWORD}))"

# Unlike regenerate, render will render report with whatever summary data is available;
# used for testing format and style changes
report: `
  Rscript -e "rmarkdown::render('build_report.Rmd')"
  
# (TO DO) diff generates a diff using the current version vs last commit in git history;
# This facilitates tracking what has been corrected
diff: 
  @echo "Not yet implemented"