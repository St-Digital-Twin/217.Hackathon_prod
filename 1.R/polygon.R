source("1.R/1.SourceAll.R")

location_proha <- read_all_prosha(sheet = c("location"))$dwh_prod

locations <- as.data.table(location_proha$location)

# sf3 <- geojson_sf(paste0(path_211,"/1.Data/LT/Polygons/all_eur.geojson")) %>% select(NUTS_ID,`name` = NUTS_NAME, geometry)
# Encoding(sf3$name) <- "UTF-8"
# 
# sf <- sf3
# df <- data.frame(NUTS_ID = rep(0,length(sf$name)), name = rep(0,length(sf$name)), geometry = rep(0,length(sf$name)))
# for (i in 1 : length(sf$name)) {
# if(length(sf$geometry[i][[1]]) > 1){
#   max_value = -Inf
#   pologenie = c(-5,-5)
#  for (j in 1:length(sf$geometry[i][[1]])) {
#    if(length(sf$geometry[i][[1]][[j]]) == 1){
#      if(length(sf$geometry[i][[1]][[j]][[1]]) > max_value){
#        max_value <- length(sf$geometry[i][[1]][[j]][[1]])
#        pologenie = c(j,-5)
#      }
#    }else{
#      for (k in 1:length(sf$geometry[i][[1]][[j]])) {
#        if(length(sf$geometry[i][[1]][[j]][[k]]) > max_value){
#          max_value <- length(sf$geometry[i][[1]][[j]][[k]])
#          pologenie = c(j,k)
#        }
#      }
#    }
#  }
#   if(pologenie[2] == -5){
#     coords <- data.frame(lat = c(sf$geometry[i][[1]][[pologenie[1]]][[1]][,2]),
#                          lon = c(sf$geometry[i][[1]][[pologenie[1]]][[1]][,1]))
#   }else{
#     coords <- data.frame(lat = c(sf$geometry[i][[1]][[pologenie[1]]][[pologenie[2]]][,2]),
#                          lon = c(sf$geometry[i][[1]][[pologenie[1]]][[pologenie[2]]][,1]))
#   }
# }else{
#   #coords <- data.frame(lat = c(sf$geometry[i][[1]][[1]][[1]][,2]),lon = c(sf$geometry[i][[1]][[1]][[1]][,1]))
#   coords <- data.frame(lat = c(sf$geometry[i][[1]][[1]][,2]),lon = c(sf$geometry[i][[1]][[1]][,1]))
# }
#   df$name[i] <- as.character(sf$name[i])
#   df$geometry[i] <- as.list(encodePolyline(coords))
#   df$osm_id[i] <- sf$osm_id[i]
#   print(paste0(i, " in ", length(sf$name)))
# }
# sf3 <- geojson_sf(paste0(path_211,"/1.Data/LT/Polygons/all_eur.geojson")) %>% select(NUTS_ID, geometry)
# "india"
sf3 <- geojson_sf("3.Data/polygons_input.geojson") %>% select(`name` = NAME, geometry) %>% as.data.table()
sf <- sf3
df <- data.frame(name = rep(0,length(sf$name)), geometry = rep(0,length(sf$name)))
for (i in 1 : length(sf$name)) {
  if(lapply(sf$geometry[i], attributes)[[1]]$class[2] == "MULTIPOLYGON"){
    if(length(sf$geometry[i][[1]]) > 1){
      max_value = -Inf
      pologenie = c(-5,-5)
      for (j in 1:length(sf$geometry[i][[1]])) {
        if(length(sf$geometry[i][[1]][[j]]) == 1){
          if(length(sf$geometry[i][[1]][[j]][[1]]) > max_value){
            max_value <- length(sf$geometry[i][[1]][[j]][[1]])
            pologenie = c(j,-5)
          }
        }else{
          for (k in 1:length(sf$geometry[i][[1]][[j]])) {
            if(length(sf$geometry[i][[1]][[j]][[k]]) > max_value){
              max_value <- length(sf$geometry[i][[1]][[j]][[k]])
              pologenie = c(j,k)
            }
          }
        }
      }
      if(i == 12){
        pologenie = c(2,-5)
      }
      if(pologenie[2] == -5){
        coords <- data.frame(lat = c(sf$geometry[i][[1]][[pologenie[1]]][[1]][,2]),
                             lon = c(sf$geometry[i][[1]][[pologenie[1]]][[1]][,1]))
      }else{
        coords <- data.frame(lat = c(sf$geometry[i][[1]][[pologenie[1]]][[pologenie[2]]][,2]),
                             lon = c(sf$geometry[i][[1]][[pologenie[1]]][[pologenie[2]]][,1]))
      }
    }else{
      coords <- data.frame(lat = c(sf$geometry[i][[1]][[1]][[1]][,2]),lon = c(sf$geometry[i][[1]][[1]][[1]][,1]))
    }
  }else{
    if(length(sf$geometry[i][[1]]) > 1){
      max_value = -Inf
      pologenie = c(-5,-5)
      for (j in 1:length(sf$geometry[i][[1]])) {
        # if(length(sf$geometry[i][[1]][[j]]) == 1){
        if(length(sf$geometry[i][[1]][[j]]) > max_value){
          max_value <- length(sf$geometry[i][[1]][[j]])
          pologenie = c(j,-5)
        }
        # }else{
        #   for (k in 1:length(sf$geometry[i][[1]][[j]])) {
        #     if(length(sf$geometry[i][[1]][[j]][[k]]) > max_value){
        #       max_value <- length(sf$geometry[i][[1]][[j]][[k]])
        #       pologenie = c(j,k)
        #     }
        #   }
        # }
      }
      if(pologenie[2] == -5){
        coords <- data.frame(lat = c(sf$geometry[i][[1]][[pologenie[1]]][,2]),
                             lon = c(sf$geometry[i][[1]][[pologenie[1]]][,1]))
      }else{
        coords <- data.frame(lat = c(sf$geometry[i][[1]][[pologenie[1]]][[pologenie[2]]][,2]),
                             lon = c(sf$geometry[i][[1]][[pologenie[1]]][[pologenie[2]]][,1]))
      }
    }else{
      #coords <- data.frame(lat = c(sf$geometry[i][[1]][[1]][[1]][,2]),lon = c(sf$geometry[i][[1]][[1]][[1]][,1]))
      coords <- data.frame(lat = c(sf$geometry[i][[1]][[1]][,2]),lon = c(sf$geometry[i][[1]][[1]][,1]))
    }
  }
  df$geometry[i] <- as.list(encodePolyline(coords))
  df$name[i] <- sf$name[i]
  print(paste0(i, " in ", length(sf$name)))
}

mapdeck(token = "pk.eyJ1IjoibmlraXRhLWJ1cmFrb3YiLCJhIjoiY2t3MGxnMTBpYjdpbTJ1cXdjeGN2eGJvNCJ9.faoxOUVwQQ4QK46LGBhYzg", 
        style ="mapbox://styles/nikita-burakov/clemrgi7f001l01qta2hrg896") %>%
  add_polygon(
    data = df
    , polyline = "geometry")
qsave(df, "3.Data/polygons_input")



df <- qread(paste0(path_211,"/1.Data/Russia/all_polygons/for_merge_2300"))
data7 <- as.data.table(qread(paste0(path_211, "/1.Data/Russia/rus2924"))) %>%
  .[location %!in% c("0-------","00000000")]

df1 <- copy(df) %>%
  as.data.table() %>% 
  .[,name := str_replace(name, "городской округ|район|ЗАТО","")] %>% 
  # merge(locations[,c("long_name", "id")],by.x = "name",by.y = "long_name",all.x = TRUE) %>%
  stringdist_join(locations[,c("long_name", "id","oktmo")] %>% 
                    .[,long_name := str_replace(long_name, "городской округ|район|ЗАТО","")] %>% 
                    .[!is.na(long_name)], by = c("name" =  "long_name"), mode = "left", 
                  max_dist = 2,method = "lv") %>% 
  # merge(locations[,c("oktmo", "id")],by.x = "oktmo",by.y = "oktmo",all.x = TRUE) %>%
  as.data.table() %>%
  .[`oktmo.y` %in% unique(data7$location)] %>% 
  .[,id := as.character(id)]


df70 <- crossingDT(copy(df) %>%
                    as.data.table() %>% 
                    .[,name := str_to_lower(str_replace(name, "ЗАТО|ЗАТО город|муниципальный район|городской округ|район|ЗАТО",""))] %>% .[,c("name","geometry")],locations[,c("long_name", "id","oktmo")] %>% 
                    .[,long_name := str_to_lower(str_replace(long_name, "ЗАТО|ЗАТО город|муниципальный район|городской округ|район|ЗАТО",""))] %>% 
                    .[!is.na(long_name) & oktmo %in% unique(data7$location),c("long_name", "id")]) %>% 
  .[,dist := stringdist::stringdistmatrix(name,long_name)[1,1],by = c("name","long_name")] %>% 
  .[,dist_min := min(dist),by = c("name")] %>% 
  .[dist == dist_min]

df8 <- copy(df70) %>%
  .[,len := length(id), by = c("name")] %>% 
  .[,id := min(as.numeric(id)), by = c("name")]
  


stringdist::stringdistmatrix(as.character(copy(df) %>%
                   as.data.table() %>% 
                   .[,name := str_replace(name, "городской округ|район|ЗАТО","")] %>% .[,c("name")])[1:10],as.character(locations[,c("long_name", "id","oktmo")] %>% 
                   .[,long_name := str_replace(long_name, "городской округ|район|ЗАТО","")] %>% 
                   .[!is.na(long_name) & oktmo %in% unique(data7$location),c("long_name")])[1:10])

df2 <- copy(df8) %>% 
  distinct(name,.keep_all = TRUE) %>% 
  .[,c("geometry","id")]

qsave(df2, paste0(path_211,"/1.Data/Russia/all_polygons/merge_2300"))
# 
# df1[name == "Cape Verde"]$id <- "197821"
# df1[name == "Chad"]$id <- "197827"
# df1[name == "Cook Islands"]$id <- "197836"
# df1[name == "Côte d'Ivoire"]$id <- "197843"
# df1[name == "Democratic Republic of the Congo"]$id <- "197835"
# df1[name == "East Timor"]$id <- "198004"
# df1[name == "Federated States of Micronesia"]$id <- "197928"
# df1[name == "North Korea"]$id <- "197902"
# df1[name == "São Tomé and Príncipe"]$id <- "197977"
# df1[name == "South Korea"]$id <- "197903"
# df1[name == "Syria"]$id <- "197999"
# df1[name == "Taiwan"]$id <- "198000"
# df1[name == "The Bahamas"]$id <- "197800"
# df1[name == "The Gambia"]$id <- "197865"
# 
# df1 <- distinct_all(df1,.keep_all = TRUE) %>%
#   .[!is.na(id)]
# 
# qsave(df1, paste0(path_211,"/1.Data/Russia/all_polygons/for_merge_st_14"))


# df1 <- copy(df) %>%
#   merge(locations[,c("long_name", "id")],by.x = "name",by.y = "long_name",all.x = TRUE) %>%
#   as.data.table() %>%
#   .[,id := as.character(id)]
# 
# 
# df1[name == "Andaman and Nicobar"]$id <- "200102"
# df1[name == "Dadra and Nagar Haveli"]$id <- "200134"
# df1[name == "Daman and Diu"]$id <- "200109"
# df1[name == "Jammu and Kashmir"]$id <- "200115"
# df1[name == "NCT of Delhi"]$id <- "200110"
# 
# df1 <- distinct_all(df1,.keep_all = TRUE) %>%
#   .[!is.na(id)] %>%
#   .[,level := 2]
# 
# qsave(df1, paste0(path_211,"/1.Data/Russia/all_polygons/for_merge_in_37"))

# df1 <- copy(df) %>%
#   merge(locations[,c("long_name", "id")],by.x = "name",by.y = "long_name",all.x = TRUE) %>%
#   as.data.table() %>%
#   .[,id := as.character(id)]
# 
# df1[name == "Адыгея"]$id <- "164840"
# df1[name == "Бурятия"]$id <- "171567"
# df1[name == "Дагестан"]$id <- "172792"
# df1[name == "Ингушетия"]$id <- "50591"
# df1[name == "Кабардино-Балкария"]$id <- "175954"
# df1[name == "Калмыкия"]$id <- "176864"
# df1[name == "Карачаево-Черкесия"]$id <- "183808"
# df1[name == "Кемеровская область"]$id <- "66151"
# df1[name == "Марий Эл"]$id <- "179629"
# df1[name == "Мордовия"]$id <- "181538"
# df1[name == "Ненецкий автономный округ"]$id <- "153243"
# df1[name == "Санкт-Петербург"]$id <- "85315"
# df1[name == "Северная Осетия - Алания"]$id <- "183354"
# df1[name == "Сумска"]$id <- "177390"
# df1[name == "Татарстан"]$id <- "184164"
# df1[name == "Удмуртия"]$id <- "189674"
# df1[name == "Чеченская республика"]$id <- "192801"
# df1[name == "Чувашия"]$id <- "193643"
# df1[name == "Москва"]$id <- "91182"
# df1[name == "Севастополь"]$id <- "144203"
# df1[name == "Тыва"]$id <- "189230"
# df1[name == "Башкортостан"]$id <- "165199"
# df1[name == "Ненецкий автономный округ"]$id <- "16868"
# 
# df1 <- distinct_all(df1,.keep_all = TRUE) %>%
#   .[!is.na(id)] %>%
#   .[,level := 2]
# 
# qsave(df1, paste0(path_211,"/1.Data/Russia/all_polygons/for_merge_rf_88"))
df_all <- rbind(df,df_all)
# name <- "All_UE_polygon"
# 
# qsave(df, paste0(path_211,"/1.Data/LT/Polygons/",name))

# name <- "LT_polygon_4-5_level"

# location <- as.data.table(qread(paste0(path_211,"/2.Masterdata/LT/LT_region_codes_ISO"))) %>%
#   .[,ad_level := paste0(str_split(region, pattern = test2,simplify = TRUE)[,3])] %>%
#   .[ad_level != "mun.", ad_level := NA] %>%
#   .[is.na(ad_level),ad_level := paste0(str_split(region, pattern = test2,simplify = TRUE)[,2])] %>%
#   .[ad_level == "mun."] %>%
#   .[,region := str_split(region, pattern = test2,simplify = TRUE)[,1]]

# location <- as.data.table(qread(paste0(path_211,"/2.Masterdata/LT/LT_region_codes_ISO"))) %>%
#   .[,ad_level := paste0(str_split(region, pattern = test2,simplify = TRUE)[,2])] %>%
#   .[ad_level == "t.", ad_level := "c."] %>% 
#   .[,short_region := str_split(region, pattern = test2,simplify = TRUE)[,1]] %>% 
#   .[ad_level == "of", ad_level := "2"] %>% 
#   .[ad_level == "county", ad_level := "4"] %>% 
#   .[ad_level %in% c("d.","mun.","c."), ad_level := "5"] %>% 
#   .[ad_level == "Rūda",ad_level := "5"]

# location <- as.data.table(qread(paste0(path_211,"/2.Masterdata/LT/LT_region_codes_ISO")))
# 
# # location <- read.xlsx(paste0(path_211,"/2.Masterdata/LT/LT_region_codes_ISO.xlsx"))
# # 
# # 
# # qsave(location, paste0(path_211,"/2.Masterdata/LT/LT_region_codes_ISO"))
# 
# df2 <- as.data.table(df) %>% 
#   .[,short_name := str_split(name, pattern = " ",simplify = TRUE)[,1]] %>% 
#   .[,ad_level1 := paste0(str_split(name, pattern = " ",simplify = TRUE)[,2])] %>%
#   # .[ad_level1 == "apskritis", ad_level1 := "4"] %>% 
#   # .[ad_level1 == "miesto", ad_level1 := "c."] %>% 
#   # .[ad_level1 == "savivaldybė", ad_level1 := "mun."] %>% 
#   stringdist_join(location[ad_level == 4], by = c("short_name" =  "short_region"), max_dist = 2, mode = "left") %>% 
#   as.data.table() %>% 
#   .[,c("name", "geometry","ISO_code")]
# 
# 
# qsave(rbindlist(list(qread(paste0(path_211,"/1.Data/LT/Polygons/LT_polygon_5_level")),df2)), name)
























library(googlePolylines)

sf3 <- geojson_sf(paste0(path_211,"/1.Data/Russia/all_polygons/admin_level_6.geojson")) %>% select(`name` = name, geometry, `oktmo:user`) %>% as.data.table() %>% 
  .[1]

enc <- encode(sf3)
str(sf3)

mapdeck(token = "pk.eyJ1IjoibmlraXRhLWJ1cmFrb3YiLCJhIjoiY2t3MGxnMTBpYjdpbTJ1cXdjeGN2eGJvNCJ9.faoxOUVwQQ4QK46LGBhYzg", 
        style = "mapbox://styles/nikita-burakov/clemrgi7f001l01qta2hrg896") %>%
  add_polygon(
    data = enc
    , polyline = "geometry")

















