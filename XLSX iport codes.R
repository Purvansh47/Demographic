#XLSX
library(tidyverse)
library(magrittr)
require(readxl)

path="F:/R Shiny/Multiple file import/XLSX"

url_xlsx <- list.files(path, pattern = "*.xlsx", recursive = TRUE)

read_xlsx_files <- function(x){
  df <- read_excel(path = paste(path, x, sep = "/"))
  return(df)
}

for (i in 1:length(url_xlsx)) assign(url_xlsx[i], read_excel(url_xlsx[i]))

conc=Concentration.xlsx
Rand=Randomization.xlsx
Time=Time.xlsx
