library(dplyr) ; library(sf) ; library(httr) ; library(geojsonsf)

source("code/segmentation_code.R")

gis_dat <- pull_arcgis_dat()

links <- st_read("./data/links/ReliabilityScore.shp")
links_df <- links %>% as.data.frame() %>% select(`combo`, `linknumber`)

segments <- gis_dat %>%
  #mutate(fromto = as.numeric(fromto)) %>%
  left_join(links_df, by = c("fromto" = "combo")) %>%
  rowwise() %>% 
  mutate(from = substr(toString(fromto), 1, 6)) %>%
  mutate(to = substr(toString(fromto), 7, 13)) %>%
  mutate(tofrom = as.numeric(paste0(to, from, collapse = ""))) %>%
  mutate(new_fromto = case_when(
    is.na(linknumber) ~ tofrom,
    TRUE ~ as.numeric(fromto)))

final_segments <- segments %>%
  mutate(fromto = toString(new_fromto)) %>%
  select(-linknumber, -new_fromto, -to, -from, -tofrom) %>%
  left_join(links_df, by = c("fromto" = "combo"))

st_write(final_segments, "./data/corrected_segments.geojson", driver = "GeoJSON", delete_dsn = TRUE)

