if(!('renv' %in% installed.packages())){
  install.packages("renv")
}

library(renv)

renv::restore()


if(!("here" %in% installed.packages())){
  install.packages("here")
  install.packages('magrittr')
  install.packages('timevis')
  install.packages("stringr")
  install.packages("readxl")
  install.packages('dplyr')
  install.packages("lubridate")
  install.packages("tidyr")
  install.packages('shiny') 
  install.packages('ggplot2')
  install.packages("shinyWidgets")
  install.packages("webshot", "webshot2")
  library(webshot)

  renv::snapshot()
  
}

library(here)

if(!dir.exists(here("data"))){
  dir.create(here("data"))
  dir.create(here("scripts"))
  dir.create(here("outputs"))
}

