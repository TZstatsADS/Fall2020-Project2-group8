library(readr)
Confirmed <- read_csv("../output/Confirmed.csv")
Deaths <- read_csv("../output/Deaths.csv")
date_choices <- names(Confirmed)[-1]
states <- geojsonio::geojson_read("../data/gz_2010_us_040_00_20m.json", what = "sp")
