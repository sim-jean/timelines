
if (!requireNamespace("renv", quietly = TRUE)){
  install.packages("renv")
  renv::restore()
  shiny::runApp()
}
shiny::runApp("scripts/")
