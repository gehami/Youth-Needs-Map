##############
# The following code creates the maps for our need-based analysis. It uses Leaflet, a javascript package ported to R, to create the map.
# This code also relies on the following R codes having been run at some point to create the data which the maps take from:
#   Agregate Yearly Data.R
#   Metric Definitions.R (does not need to be run, just exist)
#   All Data Grabbing.R 
#   Building the 1 2 and 3 year prediction models.R
#   Predicting the future.R
# Once that code has been run, this code can be run from the same directory. 
# This code was created by Albert Gehami (hereafter referred to as "Albert"). Copyright 2019. It is property of the City of San Jose.
# This code is public and can be copied, reproduced, or edited for any purpose so long as Albert is credited.
# 
rm(list=ls()) #clearing environment

######### Standard Functions ##########

#Given any matrix, data.frame, or other 2-dimensional table, returns a sample of the dataset with "rows" observations
sample_dat = function(dat, rows = 10){
  return(dat[sample(1:nrow(dat), rows),])
}
#Given the name of a library, checks if the package is installed. If it is installed, includes/imports it into the environment so you can use it. 
#If it is not installed, installs it and then includes it. 
install_and_load = function(library){
  if(!is.character(library)){
    message("Please type library name as a character value")
    return(NULL)
  }
  if(!require(library, character.only = T)){
    install.packages(library)
    require(library, character.only = T)
  }
  return(NULL)
}

####### Loading Libraries ###########

#we need the github version of leaflet
# if (!require('devtools')) install.packages('devtools')
# devtools::install_github('rstudio/leaflet')
install_and_load('leaflet')
library(stats)
install_and_load('sp')
install_and_load('htmltools')
install_and_load('magrittr')
install_and_load('e1071')

############ Globally used functions #############
#given a vector of numeric values, or something that can be coerced to numeric, returns a vector of the percentile each observation is within the vector.
#Example: if vec == c(1,2,3), then get_percentile(vec) == c(0.3333333, 0.6666667, 1.0000000)
get_percentile = function(vec, compare_vec = NULL){
  if(is.null(compare_vec)){
    return(ecdf(vec)(vec))
  }else{
    new_vec = rep(0, length(vec))
    for(n in seq_along(vec)){
      new_vec[n] = ecdf(c(vec[n], compare_vec))(c(vec[n], compare_vec))[1]
    }
    return(new_vec)
  }
}
#given a vector of numeric values, and the number of bins you want to place values into, returns a vector of 'vec' length where each observation is the quantile of that observation.
#Example: if vec == c(1,2,3), then get_quantile(vec, quantile_bins = 2) = as.factor(c(0, 50, 50)). 
#To have the top observation be marked as the 100%ile, set ret_100_ile == TRUE. 
#To return a factor variable, ret_factor == TRUE, otherwise it will return a numeric vector. 
get_quantile = function(vec, quantile_bins, ret_factor = TRUE, ret_100_ile = FALSE, compare_vec = NULL){
  quantile_bins = round(min(max(quantile_bins, 2), 100)) #ensuring the quantile bins is an integer between 2 and 100
  quant_val = floor(get_percentile(vec, compare_vec)*100 / (100/quantile_bins)) * (100/quantile_bins)
  if(!ret_100_ile){ quant_val[quant_val == 100] = unique(quant_val)[order(-unique(quant_val))][2]}
  if(ret_factor){return(factor(quant_val))}
  return(quant_val)
}

#given a vector, minmax scales it between 0 and 1.
min_max_scale = function(x,...){return((x- min(x, ...)) /(max(x,...)-min(x,...)))}

###### Opening Data ########


big_list = readRDS('RDS files/yearly_need_metric_spdf.rds') #list of each spatialpolygonsdataframe for each year of need metrics. Each element is [[1]] == year range, [[2]] == SpatialPolygonsDataFrame
full_list = readRDS('RDS files/yearly_raw_metric_spdf.rds') #list of raw metrics for each year. Each element is [[1]] == year range, [[2]] == SpatialPolygonsDataFrame
hotspot_17_raw = readRDS('RDS files/hotspots 2017.rds') #SpatialPolygonsDataFrame of the 2017 hotspots. These were drawn by Albert based on this pdf: \\Prns-nch1\#PRNS-NCH1\Contracts\CONTRACT UNIT ONLY\BEST Reuse SSIG\BEST 2016-2019\2017-2018\Hot Spots NEW
hotspot_15_raw = readRDS('RDS files/hotspots 2015.rds') #SpatialPolygonsDataFrame of the 2015 hotspots. These were drawn by Albert based on this pdf: \\Prns-nch1\#PRNS-NCH1\Contracts\CONTRACT UNIT ONLY\BEST Reuse SSIG\BEST 2013-2016\BEST 15-16\Hot Spots
#here you can do a similar thing to get the hotspots for the next year. 
pred_dat = readRDS('RDS files/predicted_metrics_for_2019-2021_rfq.rds') #developed from "Predicting the future.R". A dataframe of the predicted need metrics 
school_bounds = readRDS('RDS files/school_bounds_for_16-18_IRs.rds') #A set of school boundaries. See 'School Boundary Work' Folder to see how this was made, but it isn't pretty, and required a lot of by-hand data cleaning. Best to just take this file as a given.

cd_bounds = readRDS('RDS files/Council_bounds.rds')


########## Pulling the metric names ##########

source('Metric Definitions.R')
all_metrics = get_all_metrics()

gang_presence_cols = c(all_metrics[[1]][[1]], all_metrics[[1]][[3]][,1])
vocational_cols = c(all_metrics[[2]][[1]], all_metrics[[2]][[3]][,1])
parent_cols = c(all_metrics[[3]][[1]], all_metrics[[3]][[3]][,1])
gp_sub_cols = gang_presence_cols


# gang_presence_pals = get_pals(big_list, all_metrics[[1]][[1]])
# vocational_pals = get_pals(big_list, all_metrics[[2]][[1]])
# parent_pals = get_pals(big_list, all_metrics[[3]][[1]])
# gp_sub_pals = gang_presence_pals
# 
# 
# gang_presence_labels = get_labels(big_list, gang_presence_cols)
# vocational_labels = get_labels(big_list, vocational_cols)
# parent_labels = get_labels(big_list, parent_cols)
# gp_sub_labels = get_labels(big_list, gp_sub_cols)



######## Map Constants ##########
##
TILE_OPACITY = .7 #opacity of the fill colors (color showing risk level) on the map
QUANTILE_BINS = 10 #number of quantiles for need. Can be set to NA to see raw need percentages.
# pallete_colors = 'RdYlGn' #color pallette. Google 'Leaflet color pallettes" and "viridis color pallettes" to see what is available
pallete_colors = 'colorspace::terrain_hcl' #color pallette. Google 'Leaflet color pallettes" and "viridis color pallettes" to see what is available
reverse_pal = TRUE #reverses order of colors in pallette
cd_colors = '#1A1423' #color for council district outline
hotspot_15_colors = '#FA7921'  #color for 2015 hotspot outline
hotspot_17_colors = '#1C3144' #color for 2017 hotspot outline. Can easily incorporate another color and for the next hotspot
brightness_perc = 0.5
raw_data_cols = c('total_pop', 'cad_calls', 'white', 'black', 'asian', 'hispanic', 'unemployed', 'below_poverty_line')#, 'no_diploma_18_24')
raw_data_col_names = c('Total Population', 'High Priority Police Calls for Service', 'White', 'Black', 'Asian', 'Latinx', 'Unemployment Rate', 'Below Poverty Line')#, 'Young Adults (18-24) Without Diploma')
label_transparency = 0.8
cad_colors = "#03F"
cad_size_soft_cutoff = 150
cad_size_hard_cutoff = 80

########## Formatting full_list raw data - raw_list ##############
raw_list = full_list


for(n in seq_along(full_list)){
  raw_list[[n]][[2]]@data[,raw_data_cols[-c(1:2)]] = round(full_list[[n]][[2]]@data[,raw_data_cols[-c(1:2)]]*100, 1)
  for(col_name in raw_data_cols[-1]){raw_list[[n]][[2]]@data[,col_name] = as.numeric(raw_list[[n]][[2]]@data[,col_name])}
  raw_list[[n]][[2]] = raw_list[[n]][[2]][which(raw_list[[n]][[2]]@data$GEOID %in% big_list[[n]][[2]]@data$GEOID),][order(big_list[[n]][[2]]@data$GEOID),]
}





############# adding data to hotspots - hotspot_15 and hotspot_17 ###########
#for testing purposes
# plot(hs$geometry, col = 'green')
# plot(test$geometry, add = TRUE)
# plot(int$geometry, add = TRUE, col = 'red')
# 
# big_list_spdf = big_list[[5]][[2]]
# hotspot_spdf = hotspot_17_raw
# 
install_and_load('sf')
install_and_load('tidyverse')
install_and_load('lwgeom')
install_and_load('doBy')

add_metrics_to_hotspots = function(big_list_spdf, hotspot_spdf, gang_presence_cols, vocational_cols, parent_cols){
  test = big_list_spdf %>% st_as_sf()
  hs = st_as_sf(hotspot_spdf)
  install_and_load('sf')
  install_and_load('tidyverse')
  install_and_load('lwgeom')
  int = (st_intersection(test, st_make_valid(hs))) #%>% select(GEOID, hot_spot, hotspot_name, geometry)
  int$area = as.numeric(st_area(int$geometry))
  int = int[order(int$hot_spot),]
  int_data = int
  int_data$geometry = NULL
  install_and_load('doBy')
  int_data_only = int_data[,unique(c(gang_presence_cols, vocational_cols, parent_cols))]
  int_data_only = int_data_only * int_data$area
  # identical(as.numeric(int_data_only$GANG_PRESENCE), as.numeric(int_data$GANG_PRESENCE*int_data$area))#success
  int_data_only = cbind(hot_spot = int_data$hot_spot, int_data_only)
  test_hs = summaryBy(. ~ hot_spot, data = int_data_only, FUN = sum)
  colnames(test_hs) = gsub('\\.sum','', colnames(test_hs))
  total_area = summaryBy(area ~ hot_spot, data = int_data, FUN = sum)
  test_hs[,unique(c(gang_presence_cols, vocational_cols, parent_cols))] = test_hs[,unique(c(gang_presence_cols, vocational_cols, parent_cols))] / total_area$area.sum
  hotspot_spdf@data = merge(hotspot_spdf@data, test_hs, by = 'hot_spot')
  return(hotspot_spdf)
}


hotspot_17_metrics = add_metrics_to_hotspots(big_list[[grep('2017',unlist(lapply(lapply(lapply(big_list, `[[`, 1), as.character), '[[',1)))[1]]][[2]], #this is the 2017 year in the big_list
                                             hotspot_17_raw, gang_presence_cols, vocational_cols, parent_cols)
hotspot_15_metrics = add_metrics_to_hotspots(big_list[[grep('2015',unlist(lapply(lapply(lapply(big_list, `[[`, 1), as.character), '[[',1)))[1]]][[2]], #this is the 2015 year in the big_list
                                             hotspot_15_raw, gang_presence_cols, vocational_cols, parent_cols)

hotspot_15_raw_metrics = add_metrics_to_hotspots(raw_list[[grep('2015',unlist(lapply(lapply(lapply(raw_list, `[[`, 1), as.character), '[[',1)))[1]]][[2]], #this is the 2015 year in the raw_list
                                                 hotspot_15_raw, gang_presence_cols = raw_data_cols, vocational_cols = NULL, parent_cols = NULL)


hotspot_17_raw_metrics = add_metrics_to_hotspots(raw_list[[grep('2017',unlist(lapply(lapply(lapply(raw_list, `[[`, 1), as.character), '[[',1)))[1]]][[2]], #this is the 2017 year in the raw_list
                                                 hotspot_17_raw, gang_presence_cols = raw_data_cols, vocational_cols = NULL, parent_cols = NULL)

hotspot_17 = hotspot_17_metrics
hotspot_17@data = merge(hotspot_17_metrics@data, hotspot_17_raw_metrics@data %>% select(-hotspot_name), by = 'hot_spot')

hotspot_15 = hotspot_15_metrics
hotspot_15@data = merge(hotspot_15_metrics@data, hotspot_15_raw_metrics@data %>% select(-hotspot_name), by = 'hot_spot')

saveRDS(hotspot_17, 'Final_app_deliverable/Map Layers/hotspots 2017.rds')
saveRDS(hotspot_15, 'Final_app_deliverable/Map Layers/hotspots 2015.rds')






########## From a raw_list item, returns a spdf of the calls for service as a centroid point in each tract ########## 
# raw_map = raw_list[[1]][[2]]
install_and_load('rgeos')
make_cad_spdf = function(raw_map){
  cad_centroids = rgeos::gCentroid(raw_map, byid = TRUE)
  cad_spdf = SpatialPointsDataFrame(coords = cad_centroids, data = data.frame(cad_calls = raw_map@data$cad_calls))
  return(cad_spdf)
}



####### Map functions #########
install_and_load('viridis')
install_and_load('dplyr')


#pallettes
get_pals = function(big_list, label_metric_cols, quantile_bins = NA, pallete_colors = 'plasma', reverse_pal = FALSE){
  require(viridis)
  pals = list()
  if(is.na(quantile_bins) | quantile_bins < 2){
    for(n in seq_along((big_list))){
      pals[[n]] = colorNumeric(pallete_colors, domain = big_list[[n]][[2]]@data[,label_metric_cols], reverse = reverse_pal)
    }
  }else{
    for(n in seq_along(big_list)){
      risk_quant = get_quantile(big_list[[n]][[2]]@data[,label_metric_cols], quantile_bins)
      pals[[n]] = colorFactor(pallete_colors, domain = risk_quant, reverse = reverse_pal)
    }
  }
  return(pals)
}

get_pred_pals = function(pred_dat, label_metric_cols, quantile_bins = NA, pallete_colors = 'plasma', reverse_pal = FALSE){
  require(viridis)
  pals = list()
  if(is.numeric(label_metric_cols[1])){label_metric_cols = colnames(pred_dat@data)[label_metric_cols]}
  n = 1
  for(col_name in label_metric_cols){
    if(is.na(quantile_bins) | quantile_bins < 2){
      pals[[n]] = colorNumeric(pallete_colors, domain = pred_dat@data[,col_name], reverse = reverse_pal)
    }else{
      risk_quant = get_quantile(pred_dat@data[,col_name], quantile_bins)
      pals[[n]] = colorFactor(pallete_colors, domain = risk_quant, reverse = reverse_pal)
    }
    n = n + 1
  }
  return(pals)
}

#labels
get_labels = function(big_list, label_metric_cols, quantile_bins = NA, raw_list = NA, raw_data_cols = NA, raw_data_col_names = NA){#, weights = NA){
  labels = list()
  for(n in seq_along((big_list))){
    test = big_list[[n]][[2]]@data
    if(!is.na(quantile_bins)){
      title = sprintf('<b>%s</b><br/><em><b>Risk Factor Score - %s%%ile</b></em><br/><br/><b>Metrics:</b>', test$neib_name, get_quantile(test[,label_metric_cols[1]], quantile_bins))
    }else{    
      title = sprintf('<b>%s</b><br/><em><b>Risk Factor Score - %.2f%%</b></em><br/><br/><b>Metrics:</b>', test$neib_name, test[,label_metric_cols[1]]*100)
    }
    # if(any(!is.na(weights)) & as.numeric(weights[which(weights[,1] == label_metric_cols[2]),2]) == 0){}else{
      label = paste(sep = '<br/>', title, sprintf('%s: %.f%%ile', gsub('_', ' ', label_metric_cols[2]), test[,paste0(label_metric_cols[2], '_percentile')]*100))
    # }
    for(cols in label_metric_cols[3:length(label_metric_cols)]){
      # if(any(!is.na(weights)) & as.numeric(weights[which(weights[,1] == cols),2]) == 0){}else{
      label = paste(sep = '<br/>', label, sprintf('%s: %.f%%ile', gsub('_', ' ', cols), test[,paste0(cols, '_percentile')]*100))
      # }
    }
    if(!is.na(raw_list)[1] & !is.na(raw_data_cols)[1]){
      if(length(raw_data_cols) == length(raw_data_col_names) | is.na(raw_data_col_names[1])){
        raw_test = raw_list[[n]][[2]]@data
        label = paste0(label, '<br/>_______________<br/><b>Raw Metrics:</b>')

        label = paste0(label, '<br/>', sprintf('%s: %s', raw_data_col_names[1], raw_test[,raw_data_cols[1]]))
        label = paste0(label, '<br/>', sprintf('%s: %s', raw_data_col_names[2], raw_test[,raw_data_cols[2]]))
        for(i in seq_along(raw_data_cols)[-c(1:2)]){
          label = paste(sep = '<br/>', label, sprintf('%s: %s%%', raw_data_col_names[i], raw_test[,raw_data_cols[i]]))
        }
        # for(cols in label_metric_cols[3:length(label_metric_cols)]){
        #   label = paste(sep = '<br/>', label, sprintf('%s: %.f%%ile', gsub('_', ' ', cols), test[,paste0(cols, '_percentile')]*100))
        # }
        
      }
      
    }
    labels[[n]] = label
  }
  return(labels)
}

get_pred_labels = function(pred_dat, label_metric_cols, quantile_bins = NA){
  labels = list()
  if(is.numeric(label_metric_cols[1])){label_metric_cols = colnames(pred_dat@data)[label_metric_cols]}
  if(!is.na(quantile_bins)){
    for(n in seq_along(label_metric_cols)){
      title = sprintf('<b>%s</b><br/><em><b>Risk Factor Score - %s%%ile</b></em><br/><br/>Metrics are, on average, 99.3%% accurate.', pred_dat@data$neib_name, get_quantile(pred_dat@data[,label_metric_cols[n]], quantile_bins))
      labels[[n]] = title
    }
  }else{
    for(n in seq_along(label_metric_cols)){
      title = sprintf('<b>%s</b><br/><em><b>Risk Factor Score - %.2f%%</b></em><br/><br/>Metrics are, on average, 99.3%% accurate.', pred_dat@data$neib_name, pred_dat@data[,label_metric_cols[n]]*100)
      labels[[n]] = title
    }
  }
  return(labels)
}


#given a color in hex or in name of color form (string), returns that color brightened by brightness_perc % (can also darken via negative perc)
brighten_color = function(color, brightness_perc = 0.05){
  require(grDevices)
  if(abs(brightness_perc) > 1){
    warning('please use a brightness_perc that is between -1 and 1')
    return(color)
  }
  bright_rbg = round(col2rgb(color) * (1+brightness_perc))
  for(i in 1:length(bright_rbg)){bright_rbg[i] = max(min(bright_rbg[i], 255), 0)}
  bright_hex = rgb(bright_rbg[1], bright_rbg[2], bright_rbg[3], maxColorValue = 255)
  return(bright_hex)
}


######### FUNCTION: given hotspots, makes labels for hotspots ###########
#testing for function
# hotspot = hotspot_15
# big_list_dat = big_list[[grep('2015',unlist(lapply(lapply(lapply(big_list, `[[`, 1), as.character), '[[',1)))[1]]][[2]]@data
# column_names = gang_presence_cols
# quantile_bins = 10
# raw_data_cols = raw_data_cols
# raw_data_col_names = raw_data_col_names

get_hotspot_labels = function(hotspot, big_list_dat, column_names, quantile_bins = NA, raw_data_cols = NA, raw_data_col_names = NA){
    test = hotspot@data
    if(!is.na(quantile_bins)){
      title = sprintf('<b>Hotspot: %s</b><br/><em><b>Risk Factor Score - %s%%ile</b></em><br/><br/><b>Metrics:</b>', test$hotspot_name, get_quantile(test[,column_names[1]], quantile_bins, compare_vec = big_list_dat[,column_names[1]]))
    }else{
      title = sprintf('<b>Hotspot: %s</b><br/><em><b>Risk Factor Score - %.2f%%</b></em><br/><br/><b>Metrics:</b>', test$hotspot_name, round(test[,column_names[1]]*100,2))
    }
    label = paste(sep = '<br/>', title, sprintf('%s: %.f%%ile', gsub('_', ' ', column_names[2]), round(get_percentile(test[,column_names[2]])*100,2)))
    for(cols in column_names[3:length(column_names)]){
      label = paste(sep = '<br/>', label, sprintf('%s: %.f%%ile', gsub('_', ' ', cols), round(get_percentile(test[,cols])*100, 2)))
    }
    if(!is.na(raw_data_cols)){
      if(length(raw_data_cols) == length(raw_data_col_names) | is.na(raw_data_col_names[1])){
        raw_test = raw_list[[n]][[2]]@data
        label = paste0(label, '<br/>_______________<br/><b>Raw Metrics:</b>')
        label = paste0(label, '<br/>', sprintf('%s: %s', raw_data_col_names[1], round(hotspot@data[,raw_data_cols[1]])))
        label = paste0(label, '<br/>', sprintf('%s: %s', raw_data_col_names[2], raw_test[,raw_data_cols[2]]))
        for(i in seq_along(raw_data_cols)[-c(1:2)]){
          label = paste(sep = '<br/>', label, sprintf('%s: %s%%', raw_data_col_names[i], round(hotspot@data[,raw_data_cols[i]], 2)))
        }
        # for(cols in column_names[3:length(column_names)]){
        #   label = paste(sep = '<br/>', label, sprintf('%s: %.f%%ile', gsub('_', ' ', cols), test[,paste0(cols, '_percentile')]*100))
        # }

      }

    }

  return(label)
}




######## FUNCTION : given all the things for map, makes the map ##########
# map = map
# big_list = big_list
# metric_title = all_metrics[[1]][[1]]
# label_metric_cols = gang_presence_cols
# tile_opacity = 0.7
# pred_dat = pred_dat
# pred_title = 'gp_sub'
# hotspot_15 = hotspot_15
# hotspot_17 = hotspot_17
# school_points = school_points
# quantile_bins = 10
# cd_bounds = cd_bounds
# pallete_colors = 'plasma'


require(leaflet)

make_map = function(map, big_list, metric_title, label_metric_cols, hotspot_15, hotspot_17, school_points, cd_bounds, school_icons, html_legend_school_icons,
                    pred_dat = NULL, pred_title = NULL, tile_opacity = 0.7, quantile_bins = NA,
                    pallete_colors = 'plasma', reverse_pal = FALSE, 
                    cd_colors = '#26A54E', hotspot_15_colors = '#B600FF', hotspot_17_colors = '#000000', brightness_perc = 0.05,
                    raw_list = NA, raw_data_cols = NA, raw_data_col_names = NA, council_centroid = NA, hotspot_15_centroid = NA,
                    hotspot_17_centroid = NA, label_transparency = 0.5, cad_colors = "#03F",
                    cad_size_soft_cutoff = 150, cad_size_hard_cutoff = 80){
  
  oldw <- getOption("warn")
  options(warn = 1)
  #making highlight colors
  cd_bright = brighten_color(cd_colors, brightness_perc)
  hs_15_bright = brighten_color(hotspot_15_colors, brightness_perc)
  hs_17_bright = brighten_color(hotspot_17_colors, brightness_perc)
  
  #making the pallete
  pallete_function = get_pals(big_list, metric_title, quantile_bins, pallete_colors, reverse_pal)
  #labels
  popup_labels = get_labels(big_list, label_metric_cols, quantile_bins, raw_list = raw_list, raw_data_cols, raw_data_col_names)
  
  hotspot_15_labels = get_hotspot_labels(hotspot_15, 
                                         big_list_dat = big_list[[grep('2015',unlist(lapply(lapply(lapply(big_list, `[[`, 1), as.character), '[[',1)))[1]]][[2]]@data,
                                         label_metric_cols, quantile_bins, raw_data_cols, raw_data_col_names)
  
  hotspot_17_labels = get_hotspot_labels(hotspot_17, 
                                         big_list_dat = big_list[[grep('2017',unlist(lapply(lapply(lapply(big_list, `[[`, 1), as.character), '[[',1)))[1]]][[2]]@data,
                                         label_metric_cols, quantile_bins, raw_data_cols, raw_data_col_names)
  
  
  #starting the full map
  initial_map <- map %>% addMarkers(group = 'Clear', lng = 10, lat = 10) %>% 
    addMapPane('risk_tiles', zIndex = 410) %>% addMapPane('cad', zIndex = 420) %>%
    addMapPane('hotspots', zIndex = 430) %>% addMapPane('schools', zIndex = 440) 

  for(n in 1 : length(big_list)){
    if(!is.na(quantile_bins)){
      metric_val = get_quantile(big_list[[n]][[2]]@data[,metric_title], quantile_bins)
    }else{
      metric_val = big_list[[n]][[2]]@data[,metric_title]
    }
    initial_map <- initial_map %>% addPolygons(data = big_list[[n]][[2]], weight = 1, opacity = 1, color = 'white', dashArray = '3',
                                               fillColor = ~pallete_function[[n]](metric_val),
                                               fillOpacity = tile_opacity,
                                               popup = lapply(popup_labels[[n]], HTML),
                                               highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                                                   bringToFront = FALSE, dashArray = FALSE),
                                               group = gsub('([[:digit:]]+)(-[[:print:]]+)', '\\1 Actual', as.character(big_list[[n]][[1]][1])),
                                               options = pathOptions(pane = "risk_tiles")
    ) 
    cad_spdf = make_cad_spdf(raw_list[[n]][[2]])
    cad_radius = (cad_spdf$cad_calls/75)^1.5
    cad_radius[cad_radius < cad_size_soft_cutoff] = cad_size_hard_cutoff + as.numeric(min_max_scale(scale(cad_radius[cad_radius < cad_size_soft_cutoff])))*(cad_size_soft_cutoff - cad_size_hard_cutoff)
    initial_map <- initial_map %>% addCircles(data = cad_spdf, stroke = FALSE, fillOpacity = 0.5, 
                                                    radius = cad_radius, 
                                                    group = gsub('([[:digit:]]+)(-[[:print:]]+)', '\\1 Calls for Service', as.character(big_list[[n]][[1]][1])),
                                                    popup = lapply(popup_labels[[n]], HTML),
                                                    fillColor = cad_colors,
                                                    options = pathOptions(pane = 'cad'),
                                                    label = paste0(gsub('([[:digit:]]+)(-[[:print:]]+)', '\\1 Calls for Service: ', as.character(big_list[[n]][[1]][1])),
                                                                   cad_spdf$cad_calls))
  }
  #and the predictive layers
  if(!is.null(pred_dat)){
    pred_cols = grep(pred_title, colnames(pred_dat@data), value = TRUE)
    pred_group_names = gsub('([^0-9]+)([[:digit:]]+)', '\\2 Predicted', pred_cols)
    pred_labels = get_pred_labels(pred_dat, label_metric_cols = pred_cols, quantile_bins)
    pred_pals = get_pred_pals(pred_dat, label_metric_cols = pred_cols, quantile_bins, pallete_colors, reverse_pal)
    
    for(n in seq_along(pred_cols)){
      col_name = pred_cols[n]
      if(!is.na(quantile_bins)){
        metric_val = get_quantile(pred_dat@data[,col_name], quantile_bins)
      }else{
        metric_val = pred_dat@data[,col_name]
      }
      initial_map <- initial_map %>% addPolygons(data = pred_dat, weight = 1, opacity = 1, color = 'white', dashArray = '3',
                                                 fillColor = ~pred_pals[[n]](metric_val),
                                                 fillOpacity = tile_opacity,
                                                 popup = lapply(pred_labels[[n]], HTML),
                                                 highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                                                     bringToFront = FALSE, dashArray = FALSE),
                                                 group = as.character(pred_group_names[n]),
                                                 options = pathOptions(pane = 'risk_tiles')
      )
    }
    
  }
  
  layer_names = NULL
  for(n in seq_along(big_list)){
    layer_names = c(layer_names, gsub('([[:digit:]]+)(-[[:print:]]+)', '\\1 Actual', as.character(big_list[[n]][[1]][1])))
  }
  if(!is.null(pred_dat)){
    layer_names = c(layer_names, pred_group_names)
    # first_pred = pred_group_names[1]
  }
  last_actual = layer_names[grep('Actual', layer_names)[length(grep('Actual', layer_names))]]
  first_actual = layer_names[1]
  if(!is.na(quantile_bins)){legend_val = unique(metric_val)[order(unique(metric_val))]}else{legend_val = seq(0,1,by = 0.01)}
  
  cad_layer_names = gsub('([[:digit:]]+)([[:print:]]*)', '\\1 Calls for Service', grep('Actual', layer_names, value = TRUE))
  
  
  
  ret_map <- initial_map %>% addLegend(pal = pallete_function[[1]], values = legend_val, opacity = 0.7, position = 'bottomleft',
                                       title = 'Overall Risk Factor Score Decile') %>%
    addPolygons(data = hotspot_17, color = hotspot_17_colors, opacity = 1,
                 fillOpacity = 0,
                 weight = 4, group = '2017 MGPTF Hot Spots', 
                 label = paste0('2017 hotspot: ', hotspot_17@data$hotspot_name),
                 popup = hotspot_17_labels,
                 options = pathOptions(pane = 'hotspots'),
                 highlightOptions = highlightOptions(color = hs_17_bright, weight = 4)) %>%
    addPolygons(data = hotspot_15, color = hotspot_15_colors, opacity = 1,
                 fillOpacity = 0,
                 weight = 4, group = '2015 MGPTF Hot Spots', 
                 label = paste0('2015 hotspot: ', hotspot_15@data$hotspot_name),
                 popup = hotspot_15_labels,
                 options = pathOptions(pane = 'hotspots'),
                 highlightOptions = highlightOptions(color = hs_15_bright, weight = 4)) %>%

    addMarkers(data = school_points, group = 'Schools', label = as.character(school_points@data$OBJECTID),
               icon = school_icons, options = pathOptions(pane = 'schools')) %>% 
    addPolylines(data = cd_bounds, group = 'Council Districts', label = lapply(paste0('District: ', cd_bounds@data$DISTRICTS, '<br/>', 'Council Member: ', cd_bounds@data$CO_MEMBER), HTML),
                 color = cd_colors, weight = 4, opacity = 1, options = pathOptions(pane = 'schools'),
                 highlightOptions = highlightOptions(color = cd_bright, weight = 4)) %>%
                 {if(!is.na(council_centroid)) addLabelOnlyMarkers(., lng = council_centroid@coords[,1],
                                                                   lat = council_centroid@coords[,2],
                                                                   label = council_centroid$name, 
                                                                   group = 'Council Districts',
                                                                   labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                                                               style = list(
                                                                                                 "color" = cd_colors,
                                                                                                 "font-family" = "serif",
                                                                                                 # "font-style" = "italic",
                                                                                                 "box-shadow" = paste0("3px 3px rgba(0,0,0,",label_transparency/2,")"),
                                                                                                 "font-size" = "12px",
                                                                                                 "border-color" = paste0("rgba(0,0,0,",label_transparency,")")
                                                                                               )))} %>% #council district labels
    addLegend(colors = cad_colors, labels = 'Toggle layers here', 'topright', opacity = 0) %>%
    addLayersControl(baseGroups = c('Clear', layer_names), overlayGroups = c('2015 MGPTF Hot Spots', '2017 MGPTF Hot Spots', 'Schools', 'Council Districts',
                                                                             cad_layer_names), options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE),
                     position = 'topright') %>%
    addControl(html = html_legend_school_icons, position = "bottomleft") %>%

    hideGroup(c("2015 MGPTF Hot Spots", "2017 MGPTF Hot Spots", 'Schools', 'Council Districts', cad_layer_names)) %>% showGroup(last_actual) %>% hideGroup(first_actual) %>%
    addLegend(colors = hotspot_17_colors, labels = '2017 Hot Spots', 'bottomleft', opacity = 1) %>%
    addLegend(colors = hotspot_15_colors, labels = '2015 Hot Spots', 'bottomleft', opacity = 1) %>%
    addLegend(colors = cd_colors, labels = 'Council Districts', 'bottomleft', opacity = 1) %>%
    addLegend(colors = cad_colors, labels = 'Calls for Service', 'bottomleft', opacity = 1) 
    
    
  
  
  options(warn = oldw)
  return(ret_map)
  
}






######## Quick fixes to data - big_list and raw_list and pred_dat ###########
# Fixing Spartan Keyes neighborhood name and ordering each map properly
for(n in seq_along(big_list)){
  big_list[[n]][[2]]@data$neib_name = as.character(big_list[[n]][[2]]@data$neib_name)
  big_list[[n]][[2]]@data$neib_name = gsub('Spartan Keys', 'Spartan Keyes', big_list[[n]][[2]]@data$neib_name)
  big_list[[n]][[2]] = big_list[[n]][[2]][order(big_list[[n]][[2]]@data$GEOID),]
}



#dropping unincorporated ares from the raw metrics
for(n in seq_along(raw_list)){
  raw_list[[n]][[2]] = raw_list[[n]][[2]][grep('Unincorporated', big_list[[n]][[2]]@data$neib_name, ignore.case = TRUE, invert = TRUE),]
}
#dropping unincorporated areas from the map
for(n in seq_along(big_list)){
  big_list[[n]][[2]] = big_list[[n]][[2]][grep('Unincorporated', big_list[[n]][[2]]@data$neib_name, ignore.case = TRUE, invert = TRUE),]
}


pred_dat = pred_dat[grep('Unincorporated', pred_dat@data$neib_name, ignore.case = TRUE, invert = TRUE),]
pred_dat$neib_name = gsub('Spartan Keys', 'Spartan Keyes', as.character(pred_dat$neib_name))

# test = big_list[[n]][[2]]
# unique(test$GEOID)
# length(test$GEOID)

saveRDS(big_list, 'Final_app_deliverable/big_list_cleaned.rds')
saveRDS(pred_dat, 'Final_app_deliverable/pred_dat_cleaned.rds')
saveRDS(raw_list, 'Final_app_deliverable/raw_list_cleaned.rds')


######### Cleaning school_bounds data and creating the school_points data - school_bounds and school_points #########


#so we should drop all school districts from the school data (anything with 'S.D.' or 'Dist'), and drop schools with duplicate addresses
school_bounds = school_bounds[grep('S\\.D\\.', school_bounds@data$OBJECTID, invert = TRUE),]
school_bounds = school_bounds[grep('Dist', school_bounds@data$OBJECTID, ignore.case = TRUE, invert = TRUE),]
# print(school_bounds@data[order(school_bounds@data$address),])

#you can drop 'Cambrian Middle ', 'Sunol', 'Leadership Public School', 
#we should drop community centers like seven trees as well. 

school_bounds = school_bounds[!school_bounds@data$OBJECTID %in% c('Cambrian Middle ', 'Sunol', 'Leadership Public School',
                                                                  'Seven Trees Community Center', 'Boys and Girls Club'),]

#we need to make a function that 'jitters' the lat and lon of points that are the exact same. 
# Just use the jitter(..., factor = 0.0001) function on both the lat and lon coordinates.
school_bounds@data$lat = jitter(school_bounds@data$lat, amount = 0.001)
school_bounds@data$lon = jitter(school_bounds@data$lon, amount = 0.001)

school_points = sp::SpatialPointsDataFrame(coords = data.frame(lon = school_bounds@data$lon, lat = school_bounds@data$lat),
                                           data = data.frame(OBJECTID = school_bounds@data$OBJECTID))


# saveRDS(school_bounds, 'RDS files/school_bounds_for_pred_need_map.rds')
# saveRDS(school_points, 'RDS files/school_points_for_pred_need_map.rds')

######### Calculating Centroid for council districts - council_centroid ############
library(rgeos)
library(sp)
council_centroid = rgeos::gCentroid(cd_bounds, byid = TRUE) %>% sp::SpatialPointsDataFrame(data = cd_bounds@data) %>%
  sp::spTransform(cd_bounds@proj4string)

council_centroid$name = paste0('District ', council_centroid$DISTRICTS)

saveRDS(council_centroid, file = 'Final_app_deliverable/council_centroid.rds')

######## Calculating hotspot centroids and names - hotspot_17_centroid and hotspot_15_centroid ###########
require(rgeos)
require(sp)
hotspot_17_centroid = gCentroid(hotspot_17, byid = TRUE) %>% SpatialPointsDataFrame(data = data.frame(name = hotspot_17$hotspot_name)) %>%
  spTransform(hotspot_17@proj4string)

hotspot_15_centroid = gCentroid(hotspot_15, byid = TRUE) %>% SpatialPointsDataFrame(data = data.frame(name = hotspot_15$hotspot_name)) %>%
  spTransform(hotspot_15@proj4string)



saveRDS(hotspot_15_centroid, file = 'Final_app_deliverable/hotspot_15_centroid.rds')
saveRDS(hotspot_17_centroid, file = 'Final_app_deliverable/hotspot_17_centroid.rds')





######## School Icons #######

#I uploaded the icon to my site (gehami.com) so we don't have to worry about someone else taking it down. 
school_icons = icons(iconUrl = 'https://gehami.com/wp-content/uploads/2019/02/noun_School_1059859.png',
                     iconWidth = 38, iconHeight = 38,
                     iconAnchorX = 22, iconAnchorY = 22)

html_legend_school_icons = "<img src = 'https://gehami.com/wp-content/uploads/2019/02/noun_School_1059859.png' style='width:40px;height:40px'>School"


##### starter map #############
map <- leaflet() %>% 
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = -121.88, lat = 37.32, zoom = 12) 



######## gp_sub_map ########
map = map
big_list = big_list
metric_title = all_metrics[[1]][[1]]
label_metric_cols = gang_presence_cols
tile_opacity = TILE_OPACITY
pred_dat = pred_dat
pred_title = 'gp_sub'
hotspot_15 = hotspot_15
hotspot_17 = hotspot_17
school_points = school_points
quantile_bins = QUANTILE_BINS
cd_bounds = cd_bounds



gp_sub_map = make_map(map, big_list, metric_title, label_metric_cols, hotspot_15, hotspot_17, school_points, cd_bounds, school_icons, html_legend_school_icons,
                                 pred_dat, pred_title, tile_opacity, quantile_bins, pallete_colors, reverse_pal,
                      cd_colors, hotspot_15_colors, hotspot_17_colors, brightness_perc, raw_list, raw_data_cols, raw_data_col_names,
                      council_centroid, hotspot_15_centroid, hotspot_17_centroid, label_transparency, cad_colors, cad_size_soft_cutoff, cad_size_hard_cutoff)


########saving gp_sub_map as html #########

library(htmlwidgets)
html_gp_sub = gp_sub_map
# html_gp_sub$height = 900
# html_gp_sub$width = 1000
saveWidget(html_gp_sub, file = 'C:\\Users\\albert.gehami\\OneDrive - City of San Jose\\Documents\\PRNS MGPTF Data Story Work\\violence_risk_factors_web_page\\Media\\big-risk-factors-map\\index.html', selfcontained = FALSE)


######### Making a more mobile-friendly gp_sub_map #############


map = map
big_list = big_list
metric_title = all_metrics[[1]][[1]]
label_metric_cols = gang_presence_cols
tile_opacity = TILE_OPACITY
pred_dat = pred_dat
pred_title = 'gp_sub'
hotspot_15 = hotspot_15
hotspot_17 = hotspot_17
school_points = school_points
quantile_bins = QUANTILE_BINS
cd_bounds = cd_bounds


#making the pallete
pallete_function = get_pals(big_list, metric_title, quantile_bins, pallete_colors, reverse_pal)
#labels
popup_labels = get_labels(big_list, label_metric_cols, quantile_bins, raw_list = raw_list, raw_data_cols, raw_data_col_names)

pallete = pallete_function[[length(pallete_function)]]
pop_labs = popup_labels[[length(popup_labels)]]

if(!is.na(quantile_bins)){
  metric_val = get_quantile(big_list[[length(big_list)]][[2]]@data[,metric_title], quantile_bins)
}else{
  metric_val = big_list[[length(big_list)]][[2]]@data[,metric_title]
}

if(!is.na(quantile_bins)){legend_val = unique(metric_val)[order(unique(metric_val))][c(1,length(unique(metric_val)))]}else{legend_val = seq(0,1,by = 0.01)}


#Simplified map for the phone
phone_map <- map %>% addPolygons(data = big_list[[length(big_list)]][[2]], weight = 1, opacity = 1, color = 'white', dashArray = '3',
                                   fillColor = ~pallete(metric_val),
                                   fillOpacity = tile_opacity,
                                   popup = lapply(pop_labs, HTML),
                                   highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                                       bringToFront = FALSE, dashArray = FALSE),
                                   group = gsub('([[:digit:]]+)(-[[:print:]]+)', '\\1 Actual', as.character(big_list[[length(big_list)]][[1]][1]))
                                   ) %>% addLegend(colors = pallete(legend_val), opacity = 0.7, position = 'bottomright',
                                                   title = 'Risk Factor\nScore', labels = c('Low', 'High'))




library(htmlwidgets)
saveWidget(phone_map, file = "C:\\Users\\albert.gehami\\OneDrive - City of San Jose\\Documents\\PRNS MGPTF Data Story Work\\violence_risk_factors_web_page\\Media\\little-risk-factors-map\\index.html", selfcontained = FALSE)



######## Vocational map ########
map = map
big_list = big_list
metric_title = all_metrics[[2]][[1]]
label_metric_cols = vocational_cols
tile_opacity = TILE_OPACITY
pred_dat = pred_dat
pred_title = 'vocational'
hotspot_15 = hotspot_15
hotspot_17 = hotspot_17
school_points = school_points
quantile_bins = QUANTILE_BINS
cd_bounds = cd_bounds


vocational_map = make_map(map, big_list, metric_title, label_metric_cols, hotspot_15, hotspot_17, school_points, cd_bounds, school_icons, html_legend_school_icons,
                          pred_dat, pred_title, tile_opacity, quantile_bins, pallete_colors, reverse_pal,
                          cd_colors, hotspot_15_colors, hotspot_17_colors, brightness_perc, raw_list, raw_data_cols, raw_data_col_names,
                          council_centroid, hotspot_15_centroid, hotspot_17_centroid, label_transparency)



######### Parent map ##########
map = map
big_list = big_list
metric_title = all_metrics[[3]][[1]]
label_metric_cols = parent_cols
tile_opacity = TILE_OPACITY
pred_dat = pred_dat
pred_title = 'parent'
hotspot_15 = hotspot_15
hotspot_17 = hotspot_17
school_points = school_points
quantile_bins = QUANTILE_BINS
cd_bounds = cd_bounds


parent_map = make_map(map, big_list, metric_title, label_metric_cols, hotspot_15, hotspot_17, school_points, cd_bounds, school_icons, html_legend_school_icons,
                      pred_dat, pred_title, tile_opacity, quantile_bins, pallete_colors, reverse_pal,
                      cd_colors, hotspot_15_colors, hotspot_17_colors, brightness_perc, raw_list, raw_data_cols, raw_data_col_names,
                      council_centroid, hotspot_15_centroid, hotspot_17_centroid, label_transparency)


#### Saving the maps #####


saveRDS(gp_sub_map, 'Final_app_deliverable/Map Layers/initial_gp_map.rds')
saveRDS(vocational_map, 'Final_app_deliverable/Map Layers/initial_vocational_map.rds')
saveRDS(parent_map, 'Final_app_deliverable/Map Layers/initial_parent_map.rds')


######### Converting a map into a arcgis shapefile set #########



install_and_load("rgdal")

#given a set of maps, saves the layers as a shape file in a folder. 
save_map = function(big_list, metric_title, label_metric_cols, pred_dat = NULL, pred_title = NULL,
                    hotspot_15 = NULL, hotspot_17 = NULL, school_points = NULL, school_bounds = NULL,
                    quantile_bins = NULL, cd_bounds = NULL, folder_name = NULL){
  if(is.null(folder_name)) folder_name = paste0(metric_title, "_layers")
  #check if folder already exists
  if(dir.exists(folder_name)){
    #if it exists, clears the folders contents
    oldwd = getwd()
    setwd(folder_name)
    file.remove(list.files())
    setwd(oldwd)
  }else{
    #if it doesn't exist, creates the folder.
    # oldwd = getwd()
    dir.create(folder_name)
    # setwd(folder_name)
  }
  
  #saving the standard layers
  for(n in 1 : length(big_list)){
    if(!is.na(quantile_bins)){
      metric_val = get_quantile(big_list[[n]][[2]]@data[,metric_title], quantile_bins)
    }else{
      metric_val = big_list[[n]][[2]]@data[,metric_title]
    }
    #get scores layer for this year
    map_data = big_list[[n]][[2]]
    map_data@data = big_list[[n]][[2]]@data[,c(label_metric_cols, paste0(label_metric_cols, "_percentile"))]
    map_data@data$metric_score = metric_val
    colnames(map_data@data)[length(colnames(map_data@data))] = paste0(metric_title, "_score")
  
    #get raw data layer for this year
    raw_data = raw_list[[n]][[2]]
    
    #writing the layers to the folder
    writeOGR(map_data, dsn = folder_name, layer = paste0(metric_title, '_metrics_', substr(big_list[[n]][[1]][1], 1,4)),
             driver = 'ESRI Shapefile', check_exists = TRUE, overwrite_layer = TRUE)
    writeOGR(raw_data, dsn = folder_name, layer = paste0("Raw_data_", substr(big_list[[n]][[1]][1], 1,4)),
             driver = 'ESRI Shapefile', check_exists = TRUE, overwrite_layer = TRUE)
  }
    
  #and the predictive layers
  if(!is.null(pred_dat)){
    pred_cols = grep(pred_title, colnames(pred_dat@data), value = TRUE)
    pred_group_names = paste0(gsub('([^0-9]+)([[:digit:]]+)', 'Predicted_\\2', pred_cols), "_", metric_title)
    # pred_labels = get_pred_labels(pred_dat, label_metric_cols = pred_cols, quantile_bins)
    # pred_pals = get_pred_pals(pred_dat, label_metric_cols = pred_cols, quantile_bins, pallete_colors, reverse_pal)
    
    
    
    for(n in seq_along(pred_cols)){
      col_name = pred_cols[n]
      if(!is.na(quantile_bins)){
        metric_val = get_quantile(pred_dat@data[,col_name], quantile_bins)
      }else{
        metric_val = pred_dat@data[,col_name]
      }
      pred_map = pred_dat
      pred_map@data = pred_dat@data[,which(colnames(pred_dat@data) %in% c("GEOID", 'neib_name', col_name))]
      colnames(pred_map@data)[colnames(pred_map@data) == col_name] = paste0(pred_group_names[n], "_raw_metric")
      pred_map@data$metric_score = metric_val
      colnames(pred_map@data)[length(colnames(pred_map@data))] = paste0(pred_group_names[n], "_metric")
      writeOGR(pred_map, dsn = folder_name, layer = pred_group_names[n], driver = 'ESRI Shapefile', 
               check_exists = TRUE, overwrite_layer = TRUE)
    }
  }
  #saving all of the auxillary shape files
  tryCatch(writeOGR(hotspot_15, dsn = folder_name, layer = "Hotspot_2015", driver = "ESRI Shapefile",
                    check_exists = TRUE, overwrite_layer = TRUE), 
           error = function(e) print("No 2015 hotspot layer found"))
  tryCatch(writeOGR(hotspot_17, dsn = folder_name, layer = "Hotspot_2017", driver = "ESRI Shapefile",
                    check_exists = TRUE, overwrite_layer = TRUE), 
           error = function(e) print("No 2017 hotspot layer found"))
  tryCatch(writeOGR(school_points, dsn = folder_name, layer = "School_sites", driver = "ESRI Shapefile",
                    check_exists = TRUE, overwrite_layer = TRUE), 
           error = function(e) print("No school sites layer found"))
  tryCatch(writeOGR(school_bounds, dsn = folder_name, layer = "School_boundaries", driver = "ESRI Shapefile",
                    check_exists = TRUE, overwrite_layer = TRUE), 
           error = function(e) print("No school boundaries layer found"))
  tryCatch(writeOGR(cd_bounds, dsn = folder_name, layer = "Council_districs", driver = "ESRI Shapefile",
                    check_exists = TRUE, overwrite_layer = TRUE), 
           error = function(e) print("No Council districs layer found"))
  
  return(NULL)
  
}


############ Saving gang presence arcgis layers #########

map = map
big_list = big_list
metric_title = all_metrics[[1]][[1]]
label_metric_cols = gang_presence_cols
pred_dat = pred_dat
pred_title = 'gp_sub'
hotspot_15 = hotspot_15
hotspot_17 = hotspot_17
school_points = school_points
school_bounds = school_bounds
quantile_bins = QUANTILE_BINS
cd_bounds = cd_bounds
folder_name = "gang_presence_layers"

save_map(big_list, metric_title, label_metric_cols, pred_dat, pred_title, 
         hotspot_15, hotspot_17, school_points, school_bounds, quantile_bins, cd_bounds, folder_name)

############ Saving vocational training arcgis layers #########

map = map
big_list = big_list
metric_title = all_metrics[[2]][[1]]
label_metric_cols = vocational_cols
pred_dat = pred_dat
pred_title = 'vocational'
hotspot_15 = hotspot_15
hotspot_17 = hotspot_17
school_points = school_points
school_bounds = school_bounds
quantile_bins = QUANTILE_BINS
cd_bounds = cd_bounds
folder_name = "vocational_training_layers"

save_map(big_list, metric_title, label_metric_cols, pred_dat, pred_title, 
         hotspot_15, hotspot_17, school_points, school_bounds, quantile_bins, cd_bounds, folder_name)

############ Saving parent awareness arcgis layers #########

map = map
big_list = big_list
metric_title = all_metrics[[3]][[1]]
label_metric_cols = parent_cols
pred_dat = pred_dat
pred_title = 'parent'
hotspot_15 = hotspot_15
hotspot_17 = hotspot_17
school_points = school_points
school_bounds = school_bounds
quantile_bins = QUANTILE_BINS
cd_bounds = cd_bounds
folder_name = "parent_awareness_layers"

save_map(big_list, metric_title, label_metric_cols, pred_dat, pred_title, 
         hotspot_15, hotspot_17, school_points, school_bounds, quantile_bins, cd_bounds, folder_name)



######### DONE #########




