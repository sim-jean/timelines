if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# 2. Restaurer les packages du projet
renv::restore()

# 3. Charger les packages du projet ensuite
library(shiny)
library(timevis)
library(readxl)
library(here)
library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(shinyWidgets)
library(webshot)
library(webshot2)

if(!dir.exists(here("data"))){
  dir.create(here("data"))
  dir.create(here("scripts"))
  dir.create(here("outputs"))
}

