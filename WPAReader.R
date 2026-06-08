# Package import
list_of_packages <- c("tidyverse",
                      "pryr",
                      "yarrr",
                      "car",
                      "readxl",
                      "rpanel",
                      "plotly",
                      "maps",
                      "esquisse",
                      "gapminder",
                      "gganimate",
                      "magrittr",
                      "optimx",
                      "numDeriv",
                      "corrplot",
                      "DescTools",
                      "purrr",
                      "dplyr",
                      "data.table",
                      "vcd"
)

# Create a list of all missing packages
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]

# Install all missing packages, continue if any installation fails
if (length(new_packages)) {
  for (pkg in new_packages) {
    tryCatch({
      install.packages(pkg)
    }, warning = function(w) {
      message(sprintf("Warning installing package %s: %s", pkg, w))
    }, error = function(e) {
      message(sprintf("Error installing package %s: %s", pkg, e))
    })
  }
}

# Recursive load of packages, continue if any loading fails
invisible(
  lapply(list_of_packages, function(pkg) {
    tryCatch({
      library(pkg, character.only = TRUE)
    }, warning = function(w) {
      message(sprintf("Warning loading package %s: %s", pkg, w))
    }, error = function(e) {
      message(sprintf("Error loading package %s: %s", pkg, e))
    })
  })
)

if (exists("new_packages")) rm(new_packages)

rm(list = ls(all.names = TRUE))
gc()
#==================####

setwd("~/Desktop/Daten/WPA/")

OZu500FlachH = read_csv2("SrV2023_Einzeldaten_5_Oberzentren_unter 500 TEW_flach_SciUse_v4_H.csv")
OZu500FlachP = read_csv2("SrV2023_Einzeldaten_5_Oberzentren_unter 500 TEW_flach_SciUse_v4_P.csv")
OZu500FlachW = read_csv2("SrV2023_Einzeldaten_5_Oberzentren_unter 500 TEW_flach_SciUse_v4_W.csv")

OZu500HuegeligH = read_csv2("SrV2023_Einzeldaten_6_Oberzentren_unter 500 TEW_hügelig_SciUse_v4_H.csv")
OZu500HuegeligP = read_csv2("SrV2023_Einzeldaten_6_Oberzentren_unter 500 TEW_hügelig_SciUse_v4_P.csv")
OZu500HuegeligW = read_csv2("SrV2023_Einzeldaten_6_Oberzentren_unter 500 TEW_hügelig_SciUse_v4_W.csv")

OZüber500FlachH = read_csv2("SrV2023_Einzeldaten_7_Oberzentren_über 500 TEW_flach_SciUse_v4_H.csv")
OZüber500FlachP = read_csv2("SrV2023_Einzeldaten_7_Oberzentren_über 500 TEW_flach_SciUse_v4_P.csv")
OZüber500FlachW = read_csv2("SrV2023_Einzeldaten_7_Oberzentren_über 500 TEW_flach_SciUse_v4_W.csv")

OZu500FlachH = as_tibble(OZu500FlachH)
OZu500FlachP = as_tibble(OZu500FlachP)
OZu500FlachW = as_tibble(OZu500FlachW)
OZu500HuegeligH = as_tibble(OZu500HuegeligH)
OZu500HuegeligP = as_tibble(OZu500HuegeligP)
OZu500HuegeligW = as_tibble(OZu500HuegeligW)
OZüber500FlachH = as_tibble(OZüber500FlachH)
OZüber500FlachP = as_tibble(OZüber500FlachP)
OZüber500FlachW = as_tibble(OZüber500FlachW)

print("Done!")
