library(tidyverse)
library(glottospace)
library(dplyr)
library(stringr)
library(openxlsx)
library(rgdal)
library(ggplot2)
library(svglite)
library(xtable)
library(here)

function_files <- list.files(path = here("scripts", "functions"))
for (file in function_files) {
  source(here("scripts", "functions", file))
}

images <- here("scripts", "analysis", "images")

# for plots
theme_set(theme_minimal())

cb_palette <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#999999"
)

cbb_palette <- c(
  "#000000",
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7"
)
