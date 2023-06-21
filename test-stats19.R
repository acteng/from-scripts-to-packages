
# install.packages("remotes")
remotes::install_cran("stats19")
library(stats19)
library(tidyverse)

crashes_2017 = get_stats19(year = 2017, type = "accident")
head(crashes_2017)

dl_stats19(year = 2017, type = "accident", ask = FALSE, data_dir = ".")
list.files(pattern = "csv")
raw_stats19_data = read_csv("dft-road-casualty-statistics-accident-2017.csv")
raw_stats19_data

library(sf)
#> Linking to GEOS 3.9.1, GDAL 3.3.2, PROJ 7.2.1
wy = filter(police_boundaries, pfa16nm == "West Yorkshire")
crashes_sf = format_sf(crashes_2017)
crashes_wy = crashes_sf[wy, ]
nrow(crashes_sf)
#> [1] 129963
nrow(crashes_wy)
#> [1] 4371

plot(crashes_wy$geometry)

casualties = get_stats19(year = 2017, type = "cas")

sel = casualties$accident_index %in% crashes_wy$accident_index
summary(sel)
casualties_wy = casualties[sel, ]
names(casualties_wy)

cas_types = casualties_wy %>%
  select(accident_index, casualty_type) %>%
  mutate(n = 1) %>%
  group_by(accident_index, casualty_type) %>%
  summarise(n = sum(n)) %>%
  tidyr::spread(casualty_type, n, fill = 0)
cas_types$Total = rowSums(cas_types[-1])
cj = left_join(crashes_wy, cas_types, by = "accident_index")
names(cj)
cj |>
  filter(`Cyclist` > 0) |>
  mapview::mapview()
