setwd("~/Desktop/Master/Data Programming/Task3")
library(readr)
library(dplyr)
library(tidyr)
library(glue)
library(scales)
library(htmltools)
library(leaflet)
library(ggplot2)
library(sf)
library(sp)
library(stringr)
library(magrittr)
library(tibble)
library(leaflet.extras2)
library(lubridate)
library(ggtext)
library(ggmap)
library(RColorBrewer)
library(mapproj)


#add provincias shapefile
mun_shp <- read_sf("georef-spain-municipio-millesime.shp")

mun_shp <- mun_shp |> 
  select(mun_code, geometry) |> 
  unique()

mun_shp <- mun_shp %>%
  rename(cod_mun = mun_code)

map_data <- election_data_clean|> 
  select(-cod_mun) |> 
  mutate(cod_mun = glue("{codigo_provincia}{codigo_municipio}")) |> 
  relocate(cod_mun, .before = municipio)

map_data <- map_data |> 
  select(-codigo_ccaa, -codigo_provincia, -codigo_municipio, 
         -numero_mesas, -participacion_1, -participacion_2)

map_data <- left_join(mun_shp, map_data, by = "cod_mun")

#now we create the categories for the 2 types of parties
national_parties <- c("PSOE", "PP", "VOX", "CS", "MÁS PAÍS", "UP")

regional_parties <- election_data_clean |> 
  filter(!(party_acronym %in% national_parties) | party_acronym == "OTHERS") |> 
  pull(party_acronym)

map_data <- map_data |> 
  mutate(party_type = ifelse(party_acronym %in% national_parties, "National", "Regional")) |> 
  select(-party_acronym)


#compute the avg % of votes of each party
map_data <- map_data |> 
  group_by(date, cod_mun, party_type) |> 
  mutate(ttl_votos = sum(votos), 
         pcg_partyvotes = round(ttl_votos / (votos_nulos + votos_blancos + votos_candidaturas) * 100, 2)) |> 
  ungroup()

map_data <- map_data |> 
  select(-censo, -votos_blancos, -votos_nulos, 
         -votos_candidaturas, -votos, -ttl_votos) |> 
  unique()


#We need to have our data in WGS84
st_crs(map_data$geometry) #we do

#we need to do a pivot wider
as.character(map_data$party_type)


#alternative code as the pivot_wider did not recognise my variable party_type
map_data <- map_data |> 
  spread(key = party_type, value = pcg_partyvotes) |> 
  mutate_all(~replace(., is.na(.), 0)) #takes time as well

# Making a winner column
map_data <- mutate(map_data, winner = if_else(National > Regional, "National", "Regional"))

#filtering the dataset for many different maps
map_data_2008 <- map_data |> 
  filter(date == "2008-03-01")

map_data_2011 <- map_data |> 
  filter(date == "2011-11-01")

map_data_2015 <- map_data |> 
  filter(date == "2015-12-01")

map_data_2016 <- map_data |> 
  filter(date == "2016-06-01")

map_data_apr <- map_data |> 
  filter(date == "2019-04-01")

map_data_nov <- map_data |> 
  filter(date == "2019-04-01")


#min_max values 
#map1(2008)
map_data_2008 <- mutate(map_data_2008, margin = abs(National - Regional))
min_max_values1 <- range(map_data_2008$margin, na.rm = TRUE)

#map2(2011)
map_data_2011 <- mutate(map_data_2011, margin = abs(National - Regional))
min_max_values2 <- range(map_data_2011$margin, na.rm = TRUE)

#map3(2015)
map_data_2015 <- mutate(map_data_2015, margin = abs(National - Regional))
min_max_values3 <- range(map_data_2015$margin, na.rm = TRUE)

#map4(2016)
map_data_2016 <- mutate(map_data_2016, margin = abs(National - Regional))
min_max_values4 <- range(map_data_2016$margin, na.rm = TRUE)

#map5(apr_19)
map_data_apr <- mutate(map_data_apr, margin = abs(National - Regional))
min_max_values5 <- range(map_data_apr$margin, na.rm = TRUE)

#map6(nov_19)
map_data_nov <- mutate(map_data_nov, margin = abs(National - Regional))
min_max_values6 <- range(map_data_nov$margin, na.rm = TRUE)


#Now we create the colour palette taking the lightest
#colour the smallest number of the margin between votes

#map1
regional_palette1 <- colorNumeric(palette = "Reds", 
                                  domain=c(min_max_values1[1], min_max_values1[2]))
national_palette1 <- colorNumeric(palette = "Blues", 
                               domain=c(min_max_values1[1], min_max_values1[[2]]))

#map2
regional_palette2 <- colorNumeric(palette = "Reds", 
                                  domain=c(min_max_values2[1], min_max_values2[2]))
national_palette2 <- colorNumeric(palette = "Blues", 
                                  domain=c(min_max_values2[1], min_max_values2[[2]]))

#map3
regional_palette3 <- colorNumeric(palette = "Reds", 
                                  domain=c(min_max_values3[1], min_max_values3[2]))
national_palette3 <- colorNumeric(palette = "Blues", 
                                  domain=c(min_max_values3[1], min_max_values3[[2]]))
#map4
regional_palette4 <- colorNumeric(palette = "Reds", 
                                  domain=c(min_max_values4[1], min_max_values4[2]))
national_palette4 <- colorNumeric(palette = "Blues", 
                                  domain=c(min_max_values4[1], min_max_values4[[2]]))

#map5
regional_palette5 <- colorNumeric(palette = "Reds", 
                                  domain=c(min_max_values5[1], min_max_values5[2]))
national_palette5 <- colorNumeric(palette = "Blues", 
                                  domain=c(min_max_values5[1], min_max_values5[[2]]))

#map6
regional_palette6 <- colorNumeric(palette = "Reds", 
                                  domain=c(min_max_values6[1], min_max_values6[2]))
national_palette6 <- colorNumeric(palette = "Blues", 
                                  domain=c(min_max_values6[1], min_max_values6[[2]]))



#Creating two different data frames for each type of party (for each year)
#2008
regional_df1 <- map_data_2008[map_data_2008$winner == "Regional",]
national_df1 <- map_data_2008[map_data_2008$winner == "National",]
#2011
regional_df2 <- map_data_2011[map_data_2011$winner == "Regional",]
national_df2 <- map_data_2011[map_data_2011$winner == "National",]
#2015
regional_df3 <- map_data_2015[map_data_2015$winner == "Regional",]
national_df3 <- map_data_2015[map_data_2015$winner == "National",]
#2016
regional_df4 <- map_data_2016[map_data_2016$winner == "Regional",]
national_df4 <- map_data_2016[map_data_2016$winner == "National",]
#apr 2019
regional_df5 <- map_data_apr[map_data_apr$winner == "Regional",]
national_df5 <- map_data_apr[map_data_apr$winner == "National",]
#nov 2019
regional_df6 <- map_data_nov[map_data_nov$winner == "Regional",]
national_df6 <- map_data_nov[map_data_nov$winner == "National",]



#Create a popup

#map1
national_popup1 <- glue("<strong>{national_df1$municipio} AREA</strong><br />
                    <strong>Winner: National</strong><br />
                    National: {scales::comma(national_df1$National, accuracy = 1)}<br />
                    Regional: {scales::comma(national_df1$Regional, accuracy = 1)}<br />
                    Margin: {scales::comma(national_df1$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)
regional_popup1 <- glue("<strong>{regional_df1$municipio} AREA</strong><br />
                      <strong>Winner: Regional</strong><br />
                      Regional: {scales::comma(regional_df1$Regional, accuracy = 1)}<br />
                      National: {scales::comma(regional_df1$National, accuracy = 1)}<br />
                      Margin: {scales::comma(regional_df1$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)

#map2
national_popup2 <- glue("<strong>{national_df2$municipio} AREA</strong><br />
                    <strong>Winner: National</strong><br />
                    National: {scales::comma(national_df2$National, accuracy = 1)}<br />
                    Regional: {scales::comma(national_df2$Regional, accuracy = 1)}<br />
                    Margin: {scales::comma(national_df2$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)
regional_popup2 <- glue("<strong>{regional_df2$municipio} AREA</strong><br />
                      <strong>Winner: Regional</strong><br />
                      Regional: {scales::comma(regional_df2$Regional, accuracy = 1)}<br />
                      National: {scales::comma(regional_df2$National, accuracy = 1)}<br />
                      Margin: {scales::comma(regional_df2$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)

#map3
national_popup3 <- glue("<strong>{national_df3$municipio} AREA</strong><br />
                    <strong>Winner: National</strong><br />
                    National: {scales::comma(national_df3$National, accuracy = 1)}<br />
                    Regional: {scales::comma(national_df3$Regional, accuracy = 1)}<br />
                    Margin: {scales::comma(national_df3$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)
regional_popup3 <- glue("<strong>{regional_df3$municipio} AREA</strong><br />
                      <strong>Winner: Regional</strong><br />
                      Regional: {scales::comma(regional_df3$Regional, accuracy = 1)}<br />
                      National: {scales::comma(regional_df3$National, accuracy = 1)}<br />
                      Margin: {scales::comma(regional_df3$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)

#map4
national_popup4 <- glue("<strong>{national_df4$municipio} AREA</strong><br />
                    <strong>Winner: National</strong><br />
                    National: {scales::comma(national_df4$National, accuracy = 1)}<br />
                    Regional: {scales::comma(national_df4$Regional, accuracy = 1)}<br />
                    Margin: {scales::comma(national_df4$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)
regional_popup4 <- glue("<strong>{regional_df4$municipio} AREA</strong><br />
                      <strong>Winner: Regional</strong><br />
                      Regional: {scales::comma(regional_df4$Regional, accuracy = 1)}<br />
                      National: {scales::comma(regional_df4$National, accuracy = 1)}<br />
                      Margin: {scales::comma(regional_df4$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)

#map5
national_popup5 <- glue("<strong>{national_df5$municipio} AREA</strong><br />
                    <strong>Winner: National</strong><br />
                    National: {scales::comma(national_df5$National, accuracy = 1)}<br />
                    Regional: {scales::comma(national_df5$Regional, accuracy = 1)}<br />
                    Margin: {scales::comma(national_df5$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)
regional_popup5 <- glue("<strong>{regional_df5$municipio} AREA</strong><br />
                      <strong>Winner: Regional</strong><br />
                      Regional: {scales::comma(regional_df5$Regional, accuracy = 1)}<br />
                      National: {scales::comma(regional_df5$National, accuracy = 1)}<br />
                      Margin: {scales::comma(regional_df5$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)

#map6
national_popup6 <- glue("<strong>{national_df6$municipio} AREA</strong><br />
                    <strong>Winner: National</strong><br />
                    National: {scales::comma(national_df6$National, accuracy = 1)}<br />
                    Regional: {scales::comma(national_df6$Regional, accuracy = 1)}<br />
                    Margin: {scales::comma(national_df6$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)
regional_popup6 <- glue("<strong>{regional_df6$municipio} AREA</strong><br />
                      <strong>Winner: Regional</strong><br />
                      Regional: {scales::comma(regional_df6$Regional, accuracy = 1)}<br />
                      National: {scales::comma(regional_df6$National, accuracy = 1)}<br />
                      Margin: {scales::comma(regional_df6$margin, accuracy = 1)}")  |>    
  lapply(htmltools::HTML)



#Then we overlay the data
library(mapview)

#map 1
m1 <- mapview(
  national_df1,
  col.regions = national_palette1(national_df1$margin),
  lwd = 1,
  legend = FALSE,
  alpha.regions = 0.7,
  zcol = "margin",
  cex = 0.8,
  labels = national_popup1,
  main = "National Map", 
  map.types = "CartoDB.Positron"
) +
  mapview(
    regional_df1,
    col.regions = regional_palette1(regional_df1$margin),
    lwd = 1,
    legend = FALSE,
    alpha.regions = 0.7,
    zcol = "margin",
    cex = 0.8,
    labels = regional_popup1,
    main = "Regional Map", 
    map.types = "CartoDB.Positron"
  )

#map 2
m2 <-mapview(
  national_df2,
  col.regions = national_palette2(national_df2$margin),
  lwd = 1,
  legend = FALSE,
  alpha.regions = 0.7,
  zcol = "margin",
  cex = 0.8,
  labels = national_popup2,
  main = "National Map", 
  map.types = "CartoDB.Positron"
) +
  mapview(
    regional_df2,
    col.regions = regional_palette2(regional_df2$margin),
    lwd = 1,
    legend = FALSE,
    alpha.regions = 0.7,
    zcol = "margin",
    cex = 0.8,
    labels = regional_popup2,
    main = "Regional Map", 
    map.types = "CartoDB.Positron"
  )

#map 3
m3 <-mapview(
  national_df3,
  col.regions = national_palette3(national_df3$margin),
  lwd = 1,
  legend = FALSE,
  alpha.regions = 0.7,
  zcol = "margin",
  cex = 0.8,
  labels = national_popup3,
  main = "National Map", 
  map.types = "CartoDB.Positron"
) +
  mapview(
    regional_df3,
    col.regions = regional_palette3(regional_df3$margin),
    lwd = 1,
    legend = FALSE,
    alpha.regions = 0.7,
    zcol = "margin",
    cex = 0.8,
    labels = regional_popup3,
    main = "Regional Map", 
    map.types = "CartoDB.Positron"
  )

#map 4
m4 <-mapview(
  national_df4,
  col.regions = national_palette4(national_df4$margin),
  lwd = 1,
  legend = FALSE,
  alpha.regions = 0.7,
  zcol = "margin",
  cex = 0.8,
  labels = national_popup4,
  main = "National Map", 
  map.types = "CartoDB.Positron"
) +
  mapview(
    regional_df4,
    col.regions = regional_palette4(regional_df4$margin),
    lwd = 1,
    legend = FALSE,
    alpha.regions = 0.7,
    zcol = "margin",
    cex = 0.8,
    labels = regional_popup4,
    main = "Regional Map", 
    map.types = "CartoDB.Positron"
  )

#map 5
m5 <-mapview(
  national_df5,
  col.regions = national_palette5(national_df5$margin),
  lwd = 1,
  legend = FALSE,
  alpha.regions = 0.7,
  zcol = "margin",
  cex = 0.8,
  labels = national_popup5,
  main = "National Map", 
  map.types = "CartoDB.Positron"
) +
  mapview(
    regional_df5,
    col.regions = regional_palette5(regional_df5$margin),
    lwd = 1,
    legend = FALSE,
    alpha.regions = 0.7,
    zcol = "margin",
    cex = 0.8,
    labels = regional_popup5,
    main = "Regional Map", 
    map.types = "CartoDB.Positron"
  )

#map 5
m6 <-mapview(
  national_df6,
  col.regions = national_palette6(national_df6$margin),
  lwd = 1,
  legend = FALSE,
  alpha.regions = 0.7,
  zcol = "margin",
  cex = 0.8,
  labels = national_popup6,
  main = "National Map", 
  map.types = "CartoDB.Positron"
) +
  mapview(
    regional_df5,
    col.regions = regional_palette6(regional_df6$margin),
    lwd = 1,
    legend = FALSE,
    alpha.regions = 0.7,
    zcol = "margin",
    cex = 0.8,
    labels = regional_popup6,
    main = "Regional Map", 
    map.types = "CartoDB.Positron"
  )

#now we make the comparisons
m1 | m2
m2 | m3
m3 | m4
m4 | m5
m5 | m6


