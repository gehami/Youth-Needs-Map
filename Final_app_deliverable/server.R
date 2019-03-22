#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
######### Standard Functions ##########
# setwd('Final_app_deliverable')

sample_dat = function(dat, rows = 10){
  return(dat[sample(1:nrow(dat), rows),])
}

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


########## Opening Data ########

big_list = readRDS('big_list_cleaned.rds')
pred_dat = readRDS('pred_dat_cleaned.rds')
raw_list = readRDS('raw_list_cleaned.rds')

source('Metric Definitions.R')
all_metrics = get_all_metrics()

# initial_gp_sub_map = readRDS('Map Layers/initial_gp_map.rds')
# initial_parent_map = readRDS('Map Layers/initial_parent_map.rds')
# initial_vocational_map = readRDS('Map Layers/initial_vocational_map.rds')

hotspot_17 = readRDS('Map Layers/hotspots 2017.rds')
hotspot_17_centroid = readRDS('hotspot_17_centroid.rds')
hotspot_15 = readRDS('Map Layers/hotspots 2015.rds')
hotspot_15_centroid = readRDS('hotspot_15_centroid.rds')

# cd_bounds = rgdal::readOGR(dsn = "C:\\Users\\albert.gehami\\Desktop\\Predictive Needs Analysis\\Final_app_deliverable\\CITY_COUNCIL_DISTRICTS", 
#                            layer = "CITY_COUNCIL_DISTRICTS") %>% sp::spTransform(big_list[[1]][[2]]@proj4string)
# saveRDS(cd_bounds, 'Council_bounds.rds')

cd_bounds = readRDS('Council_bounds.rds')
council_centroid = readRDS('council_centroid.rds')

school_bounds = readRDS('Map Layers/school_bounds_for_pred_need_map.rds')
school_points = readRDS('Map Layers/school_points_for_pred_need_map.rds')




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





######## Quick fixes to data - big_list ###########
# Fixing Spartan Keyes neighborhood name
for(n in seq_along(big_list)){
  big_list[[n]][[2]]@data$neib_name = as.character(big_list[[n]][[2]]@data$neib_name)
  big_list[[n]][[2]]@data$neib_name = gsub('Spartan Keys', 'Spartan Keyes', big_list[[n]][[2]]@data$neib_name)
}

#dropping unincorporated areas from the map
for(n in seq_along(big_list)){
  big_list[[n]][[2]] = big_list[[n]][[2]][grep('Unincorporated', big_list[[n]][[2]]@data$neib_name, ignore.case = TRUE, invert = TRUE),]
}


######## FUNCTIONS - Given the class_weights (df), and the big_list (list), returns a new_big_list with updated final metric values - get_new_big_list ############
#given a vector, min-max scales it.
min_max_vec = function(vec, ...){
  if(max(vec, ...) == min(vec,...)){
    return(rep(0, length(vec)))
  }
  return((vec - min(vec, ...))/(max(vec,...)-min(vec,...)))
}

get_new_big_list = function(class_weights, big_list, replace_col){
  
  # Testing the function. It seems to work just fine 
  #' class_weights = data.frame(
  #'   matrix(
  #'     data = c(
  #'       'gang_affiliated_crime',           1,
  #'       'child_maltreatment',              0,
  #'       'developmental_trauma',            0,
  #'       'violent_prejudice_victimization', 1,
  #'       'disorder_in_neighborhood',        0,
  #'       'presence_of_illegal_firearms',    0,
  #'       'youth_school_conflicts',          0,
  #'       'youth_with_disability',           1,
  #'       'economic_deprivation',            0,
  #'       'substance_abuse',                 0#, 
  #'       #'social_discrimination',           0.2
  #'     ),
  #'     ncol = 2,
  #'     byrow = TRUE
  #'   ), stringsAsFactors = FALSE
  #' )
  #' 
  rel_cols = class_weights[,1]
  
  # test = as.matrix(big_list[[3]][[2]]@data[20:25,rel_cols])
  # test_mult = test %*% as.matrix(as.numeric(class_weights[,2]))
  # min_max_mult = min_max_vec(test_mult)
  # 
  # print(test)
  # print(test_mult)
  # print(min_max_mult)
  
  for(n in seq_along(big_list)){
    # test = big_list[[n]][[2]]
    # x_cols = test@data[,rel_cols]
    # test_metrics = as.matrix(x_cols) %*% as.matrix(data.frame(weights = as.numeric(class_weights[,2])))
    big_list[[n]][[2]]@data[,replace_col] = min_max_vec(as.matrix(big_list[[n]][[2]]@data[,rel_cols]) %*% 
                                                          as.matrix(as.numeric(class_weights[,2]))) #works
  }
  return(big_list)
  
}





######## Map Constants ##########
START_VALUE = 1
TILE_OPACITY = .7
QUANTILE_BINS = 10
pallete_colors = 'RdYlGn'
reverse_pal = TRUE
cd_colors = '#1A1423'
hotspot_15_colors = '#FA7921' 
hotspot_17_colors = '#1C3144'
brightness_perc = 0.5
raw_data_cols = c('total_pop', 'white', 'black', 'asian', 'hispanic', 'unemployed', 'below_poverty_line')#, 'no_diploma_18_24')
raw_data_col_names = c('Total Population', 'White', 'Black', 'Asian', 'Latinx', 'Unemployment Rate', 'Below Poverty Line')#, 'Young Adults (18-24) Without Diploma')
label_transparency = 0.5


last_actual_year = as.numeric(substr(as.character(big_list[[length(big_list)]][[1]][1]), 1, 4))

####### Map functions #########
install_and_load('viridis')
install_and_load('dplyr')


#pallettes
get_pals = function(big_list, column_name, quantile_bins = NA, pallete_colors = 'plasma', reverse_pal = FALSE){
  require(viridis)
  pals = list()
  if(is.na(quantile_bins) | quantile_bins < 2){
    for(n in seq_along((big_list))){
      pals[[n]] = colorNumeric(pallete_colors, domain = big_list[[n]][[2]]@data[,column_name], reverse = reverse_pal)
    }
  }else{
    for(n in seq_along(big_list)){
      risk_quant = get_quantile(big_list[[n]][[2]]@data[,column_name], quantile_bins)
      pals[[n]] = colorFactor(pallete_colors, domain = risk_quant, reverse = reverse_pal)
    }
  }
  return(pals)
}

get_pred_pals = function(pred_dat, column_names, quantile_bins = NA, pallete_colors = 'plasma', reverse_pal = FALSE){
  require(viridis)
  pals = list()
  if(is.numeric(column_names[1])){column_names = colnames(pred_dat@data)[column_names]}
  n = 1
  for(col_name in column_names){
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
get_labels = function(big_list, column_names, quantile_bins = NA, raw_list = NA, raw_data_cols = NA, raw_data_col_names = NA){
  labels = list()
  for(n in seq_along((big_list))){
    test = big_list[[n]][[2]]@data
    if(!is.na(quantile_bins)){
      title = sprintf('<b>%s</b><br/><em><b>Need Rating - %s%%ile</b></em><br/><br/><b>Metrics:</b>', test$neib_name, get_quantile(test[,column_names[1]], quantile_bins))
    }else{    
      title = sprintf('<b>%s</b><br/><em><b>Need Rating - %.2f%%</b></em><br/><br/><b>Metrics:</b>', test$neib_name, test[,column_names[1]]*100)
    }
    if(length(column_names) > 1){
      label = paste(sep = '<br/>', title, sprintf('%s: %.f%%ile', gsub('_', ' ', column_names[2]), test[,paste0(column_names[2], '_percentile')]*100))
    }
    if(length(column_names) > 2){
      for(cols in column_names[3:length(column_names)]){
        label = paste(sep = '<br/>', label, sprintf('%s: %.f%%ile', gsub('_', ' ', cols), test[,paste0(cols, '_percentile')]*100))
      }
    }
    if(!is.na(raw_list) & !is.na(raw_data_cols)){
      if(length(raw_data_cols) == length(raw_data_col_names) | is.na(raw_data_col_names[1])){
        raw_test = raw_list[[n]][[2]]@data
        label = paste0(label, '<br/>_______________<br/><b>Raw Metrics:</b>')
        label = paste0(label, '<br/>', sprintf('%s: %s', raw_data_col_names[1], raw_test[,raw_data_cols[1]]))
        for(i in seq_along(raw_data_cols)[-1]){
          label = paste(sep = '<br/>', label, sprintf('%s: %s%%', raw_data_col_names[i], raw_test[,raw_data_cols[i]]))
        }
        # for(cols in column_names[3:length(column_names)]){
        #   label = paste(sep = '<br/>', label, sprintf('%s: %.f%%ile', gsub('_', ' ', cols), test[,paste0(cols, '_percentile')]*100))
        # }
        
      }
      
    }
    labels[[n]] = label
  }
  return(labels)
}

get_pred_labels = function(pred_dat, column_names, quantile_bins = NA){
  labels = list()
  if(is.numeric(column_names[1])){column_names = colnames(pred_dat@data)[column_names]}
  if(!is.na(quantile_bins)){
    for(n in seq_along(column_names)){
      title = sprintf('<b>%s</b><br/><em><b>Need Rating - %s%%ile</b></em><br/><br/>Metrics are, on average, 99.3%% accurate.', pred_dat@data$neib_name, get_quantile(pred_dat@data[,column_names[n]], quantile_bins))
      labels[[n]] = title
    }
  }else{
    for(n in seq_along(column_names)){
      title = sprintf('<b>%s</b><br/><em><b>Need Rating - %.2f%%</b></em><br/><br/>Metrics are, on average, 99.3%% accurate.', pred_dat@data$neib_name, pred_dat@data[,column_names[n]]*100)
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
    title = sprintf('<b>Hotspot: %s</b><br/><em><b>Need Rating - %s%%ile</b></em><br/><br/><b>Metrics:</b>', test$hotspot_name, get_quantile(test[,column_names[1]], quantile_bins, compare_vec = big_list_dat[,column_names[1]]))
  }else{
    title = sprintf('<b>Hotspot: %s</b><br/><em><b>Need Rating - %.2f%%</b></em><br/><br/><b>Metrics:</b>', test$hotspot_name, round(test[,column_names[1]]*100,2))
  }
  if(length(column_names) > 1){
    label = paste(sep = '<br/>', title, sprintf('%s: %.f%%ile', gsub('_', ' ', column_names[2]), round(get_percentile(test[,column_names[2]])*100,2)))
  }
  if(length(column_names) > 2){
    for(cols in column_names[3:length(column_names)]){
      label = paste(sep = '<br/>', label, sprintf('%s: %.f%%ile', gsub('_', ' ', cols), round(get_percentile(test[,cols])*100, 2)))
    }
  }
  if(!is.na(raw_data_cols)){
    if(length(raw_data_cols) == length(raw_data_col_names) | is.na(raw_data_col_names[1])){
      raw_test = raw_list[[n]][[2]]@data
      label = paste0(label, '<br/>_______________<br/><b>Raw Metrics:</b>')
      label = paste0(label, '<br/>', sprintf('%s: %s', raw_data_col_names[1], round(hotspot@data[,raw_data_cols[1]])))
      for(i in seq_along(raw_data_cols)[-1]){
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
                    hotspot_17_centroid = NA, label_transparency = 0.5){
  
  oldw <- getOption("warn")
  options(warn = -1)
  
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
  
  
  if(!is.na(quantile_bins)){
    metric_val = get_quantile(big_list[[1]][[2]]@data[,metric_title], quantile_bins)
  }else{
    metric_val = big_list[[1]][[2]]@data[,metric_title]
  }
  
  # htmlEscape(popup_labels[[1]])
  #starting the full map
  initial_map <- map %>% addPolygons(data = big_list[[1]][[2]], weight = 1, opacity = 1, color = 'white', dashArray = '3',
                                     fillColor = ~pallete_function[[1]](metric_val),
                                     fillOpacity = tile_opacity,
                                     popup = lapply(popup_labels[[1]], HTML),
                                     highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                                         bringToFront = FALSE, dashArray = FALSE),
                                     group = gsub('([[:digit:]]+)(-[[:print:]]+)', '\\1 Actual', as.character(big_list[[1]][[1]][1]))
  ) %>% addMapPane('risk_tiles', zIndex = 410) %>% addMapPane('hotspots', zIndex = 425) %>% addMapPane('schools', zIndex = 440)
  
  for(n in 2 : length(big_list)){
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
  }
  #and the predictive layers
  if(!is.null(pred_dat)){
    pred_cols = grep(pred_title, colnames(pred_dat@data), value = TRUE)
    pred_group_names = gsub('([^0-9]+)([[:digit:]]+)', '\\2 Predicted', pred_cols)
    pred_labels = get_pred_labels(pred_dat, column_names = pred_cols, quantile_bins)
    pred_pals = get_pred_pals(pred_dat, column_names = pred_cols, quantile_bins, pallete_colors, reverse_pal)
    
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
  
  ret_map <- initial_map %>% addLegend(pal = pallete_function[[1]], values = legend_val, opacity = 0.7, position = 'bottomright',
                                       title = 'Need Value Decile') %>%
    addPolygons(data = hotspot_17, color = hotspot_17_colors, opacity = 1,
                fillOpacity = 0,
                weight = 4, group = '2017 MGPTF hotspots', 
                label = paste0('2017 hotspot: ', hotspot_17@data$hotspot_name),
                popup = hotspot_17_labels,
                options = pathOptions(pane = 'hotspots'),
                highlightOptions = highlightOptions(color = hs_17_bright, weight = 4)) %>%
    # if(!is.na(hotspot_17_centroid)) {addLabelOnlyMarkers(., lng = hotspot_17_centroid@coords[,1],
    #                                                      lat = hotspot_17_centroid@coords[,2],
    #                                                      label = hotspot_17_centroid$name, 
    #                                                      group = '2017 hotspots',
    #                                                      labelOptions = labelOptions(noHide = T, direction = "bottom",
    #                                                                                  style = list(
    #                                                                                    "color" = 'hotspot_17_colors',
    #                                                                                    'background-color' = paste0("rgba(255,255,255,",label_transparency,")"),
    #                                                                                    "font-family" = "serif",
    #                                                                                    "font-style" = "italic",
    #                                                                                    "box-shadow" = paste0("3px 3px rgba(0,0,0,",label_transparency/2,")"),
  #                                                                                    "font-size" = "10px",
  #                                                                                    "border-color" = paste0("rgba(0,0,0,",label_transparency,")")
  #                                                                                  )))} %>% #hotspot 17 labels
  addPolygons(data = hotspot_15, color = hotspot_15_colors, opacity = 1,
              fillOpacity = 0,
              weight = 4, group = '2015 MGPTF hotspots', 
              label = paste0('2015 hotspot: ', hotspot_15@data$hotspot_name),
              popup = hotspot_15_labels,
              options = pathOptions(pane = 'hotspots'),
              highlightOptions = highlightOptions(color = hs_15_bright, weight = 4)) %>%
    # if(!is.na(hotspot_15_centroid)) {addLabelOnlyMarkers(., lng = hotspot_15_centroid@coords[,1],
    #                                                      lat = hotspot_15_centroid@coords[,2],
    #                                                      label = hotspot_15_centroid$name, 
    #                                                      group = '2015 hotspots',
    #                                                      labelOptions = labelOptions(noHide = T, direction = "bottom",
    #                                                                                  style = list(
    #                                                                                    "color" = 'hotspot_17_colors',
    #                                                                                    'background-color' = paste0("rgba(255,255,255,",label_transparency,")"),
    #                                                                                    "font-family" = "serif",
    #                                                                                    "font-style" = "italic",
    #                                                                                    "box-shadow" = paste0("3px 3px rgba(0,0,0,",label_transparency/2,")"),
  #                                                                                    "font-size" = "10px",
  #                                                                                    "border-color" = paste0("rgba(0,0,0,",label_transparency,")")
  #                                                                                  )))}  %>% #hotspot 15 labels
  
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
    
    addLayersControl(baseGroups = layer_names, overlayGroups = c('2015 MGPTF hotspots', '2017 MGPTF hotspots', 'Schools', 'Council Districts'), options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE),
                     position = 'topright') %>%
    addControl(html = html_legend_school_icons, position = "bottomleft") %>%
    
    hideGroup(c("2015 MGPTF hotspots", "2017 MGPTF hotspots", 'Schools', 'Council Districts')) %>% showGroup(last_actual) %>% hideGroup(first_actual) %>%
    # {if(!is.null(pred_dat)) hideGroup(.,first_pred)} %>%
    addLegend(colors = hotspot_17_colors, labels = '2017 Hotspots', 'bottomleft', opacity = 1) %>%
    addLegend(colors = hotspot_15_colors, labels = '2015 Hotspots', 'bottomleft', opacity = 1) %>%
    addLegend(colors = cd_colors, labels = 'Council Districts', 'bottomleft', opacity = 1)
  
  options(warn = oldw)
  return(ret_map)
  
}






######## Pulls the Metrics ###########

source('Metric Definitions.R')
all_metrics = get_all_metrics()

gang_presence_cols = c(all_metrics[[1]][[1]], all_metrics[[1]][[3]][,1])
vocational_cols = c(all_metrics[[2]][[1]], all_metrics[[2]][[3]][,1])
parent_cols = c(all_metrics[[3]][[1]], all_metrics[[3]][[3]][,1])
gp_sub_cols = gang_presence_cols



########### Base map #########
# install.packages('leaflet')
library(leaflet)
library(htmltools)

map <- leaflet() %>% 
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = -121.88, lat = 37.32, zoom = 12) 



######## School Icons #######

#I uploaded the icon to my site (gehami.com) so we don't have to worry about someone else taking it down. 
school_icons = icons(iconUrl = 'https://gehami.com/wp-content/uploads/2019/02/noun_School_1059859.png',
                     iconWidth = 38, iconHeight = 38,
                     iconAnchorX = 22, iconAnchorY = 22)
html_legend_school_icons = "<img src = 'https://gehami.com/wp-content/uploads/2019/02/noun_School_1059859.png' style='width:40px;height:40px'>School"

###### Initial map - initial_gp_sub_map #######
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


initial_gp_sub_map = make_map(map, big_list, metric_title, label_metric_cols, hotspot_15, hotspot_17, school_points, cd_bounds, school_icons, html_legend_school_icons,
                              pred_dat, pred_title, tile_opacity, quantile_bins, pallete_colors, reverse_pal,
                              cd_colors, hotspot_15_colors, hotspot_17_colors, brightness_perc, raw_list, raw_data_cols, raw_data_col_names,
                              council_centroid, hotspot_15_centroid, hotspot_17_centroid, label_transparency)



####### Server #########


library(shiny)
library(shinyBS)
library(DT)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  
  #initial Need map
  output$need_map = renderLeaflet(initial_gp_sub_map)
  
  #setting datatables
  {
  output$metric_focus = renderText('Top Neighborhoods by Gang Presence Metrics')
    
  dt_2013 = datatable(data.frame(Neighborhood = big_list[[1]][[2]]@data$neib_name,
                                 risk_level= big_list[[1]][[2]]@data$GANG_PRESENCE, 
                                 Risk = get_quantile(big_list[[1]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                  ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                        mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
  output$dt_2013 = DT::renderDataTable(dt_2013) 
  
  dt_2014 = datatable(data.frame(Neighborhood = big_list[[2]][[2]]@data$neib_name,
                                 risk_level= big_list[[2]][[2]]@data$GANG_PRESENCE, 
                                 Risk = get_quantile(big_list[[2]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                  ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                        mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
  output$dt_2014 = DT::renderDataTable(dt_2014)
  
  dt_2015 = datatable(data.frame(Neighborhood = big_list[[3]][[2]]@data$neib_name,
                                 risk_level= big_list[[3]][[2]]@data$GANG_PRESENCE, 
                                 Risk = get_quantile(big_list[[3]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                  ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                        mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
  output$dt_2015 = DT::renderDataTable(dt_2015)
  
  dt_2016 = datatable(data.frame(Neighborhood = big_list[[4]][[2]]@data$neib_name,
                                 risk_level= big_list[[4]][[2]]@data$GANG_PRESENCE, 
                                 Risk = get_quantile(big_list[[4]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                  ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                        mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
  output$dt_2016 = DT::renderDataTable(dt_2016)
  
  dt_2017 = datatable(data.frame(Neighborhood = big_list[[5]][[2]]@data$neib_name,
                                 risk_level= big_list[[5]][[2]]@data$GANG_PRESENCE, 
                                 Risk = get_quantile(big_list[[5]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                  ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                        mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
  output$dt_2017 = DT::renderDataTable(dt_2017)
  
  dt_2018 = datatable(data.frame(Neighborhood = big_list[[6]][[2]]@data$neib_name,
                                 risk_level= big_list[[6]][[2]]@data$GANG_PRESENCE, 
                                 Risk = get_quantile(big_list[[6]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                  ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                        mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
  output$dt_2018 = DT::renderDataTable(dt_2018)
  }
  
  
  #a list of these will update the sliders to match the numbers
  #Gang Presence
  {
    observeEvent(input$gang_affiliated_crime_num,
      updateSliderInput(session, inputId = 'gang_affiliated_crime', value = input$gang_affiliated_crime_num)
    )
    observeEvent(input$graffiti_num,
                 updateSliderInput(session, inputId = 'graffiti', value = input$graffiti_num)
    )
    observeEvent(input$child_maltreatment_num,
      updateSliderInput(session, inputId = 'child_maltreatment', value = input$child_maltreatment_num)
    )
    observeEvent(input$developmental_trauma_num,
      updateSliderInput(session, inputId = 'developmental_trauma', value = input$developmental_trauma_num)
    )
    observeEvent(input$violent_prejudice_victimization_num,
                 updateSliderInput(session, inputId = 'violent_prejudice_victimization', value = input$violent_prejudice_victimization_num)
    )
    observeEvent(input$disorder_in_neighborhood_num,
                 updateSliderInput(session, inputId = 'disorder_in_neighborhood', value = input$disorder_in_neighborhood_num)
    )
    observeEvent(input$presence_of_firearms_num,
                 updateSliderInput(session, inputId = 'presence_of_firearms', value = input$presence_of_firearms_num)
    )
    observeEvent(input$youth_school_conflicts_num,
                 updateSliderInput(session, inputId = 'youth_school_conflicts', value = input$youth_school_conflicts_num)
    )
    observeEvent(input$reported_disability_num,
                 updateSliderInput(session, inputId = 'reported_disability', value = input$reported_disability_num)
    )
    observeEvent(input$economic_deprivation_num,
                 updateSliderInput(session, inputId = 'economic_deprivation', value = input$economic_deprivation_num)
    )
    observeEvent(input$substance_abuse_num,
                 updateSliderInput(session, inputId = 'substance_abuse', value = input$substance_abuse_num)
    )
  }
  #Vocational Training
  {
    observeEvent(input$joblessness_num,
                 updateSliderInput(session, inputId = 'joblessness', value = input$joblessness_num)
    )
    observeEvent(input$poverty_in_community_num,
                 updateSliderInput(session, inputId = 'poverty_in_community', value = input$poverty_in_community_num)
    )
    observeEvent(input$concentrated_disadvantage_num,
                 updateSliderInput(session, inputId = 'concentrated_disadvantage', value = input$concentrated_disadvantage_num)
    )
    observeEvent(input$edu_completion_num,
                 updateSliderInput(session, inputId = 'edu_completion', value = input$edu_completion_num)
    )
    observeEvent(input$illegal_econ_activity_num,
                 updateSliderInput(session, inputId = 'illegal_econ_activity', value = input$illegal_econ_activity_num)
    )
  }
  #Parent Awareness
  {
    observeEvent(input$child_maltreatment_pa_num,
                 updateSliderInput(session, inputId = 'child_maltreatment_pa', value = input$child_maltreatment_pa_num)
    )
    observeEvent(input$prop_single_mothers_num,
                 updateSliderInput(session, inputId = 'prop_single_mothers', value = input$prop_single_mothers_num)
    )
    observeEvent(input$teen_mothers_num,
                 updateSliderInput(session, inputId = 'teen_mothers', value = input$teen_mothers_num)
    )
    observeEvent(input$families_in_poverty_num,
                 updateSliderInput(session, inputId = 'families_in_poverty', value = input$families_in_poverty_num)
    )
    observeEvent(input$families_unaware_of_community_num,
                 updateSliderInput(session, inputId = 'families_unaware_of_community', value = input$families_unaware_of_community_num)
    )
    observeEvent(input$drug_abuse_num,
                 updateSliderInput(session, inputId = 'drug_abuse', value = input$drug_abuse_num)
    )
  }
  
  #reseting the sliders
  #Gang Presence
  observeEvent(input$gp_reset_weights,{
    updateSliderInput(session, inputId = 'gang_affiliated_crime', value = START_VALUE)
    updateSliderInput(session, inputId = 'child_maltreatment', value = START_VALUE)

    updateSliderInput(session, inputId = 'developmental_trauma', value = START_VALUE)
    updateSliderInput(session, inputId = 'violent_prejudice_victimization', value = START_VALUE)
    updateSliderInput(session, inputId = 'disorder_in_neighborhood', value = START_VALUE)

    updateSliderInput(session, inputId = 'presence_of_firearms', value = START_VALUE)
    updateSliderInput(session, inputId = 'youth_school_conflicts', value = START_VALUE)
    updateSliderInput(session, inputId = 'reported_disability', value = START_VALUE)
    updateSliderInput(session, inputId = 'economic_deprivation', value = START_VALUE)
    updateSliderInput(session, inputId = 'substance_abuse', value = START_VALUE)
    updateSliderInput(session, inputId = 'graffiti', value = START_VALUE)
    
    
  })
  #Vocational Training
  observeEvent(input$vocational_reset_weights,{
    updateSliderInput(session, inputId = 'joblessness', value = START_VALUE)
    updateSliderInput(session, inputId = 'poverty_in_community', value = START_VALUE)
    updateSliderInput(session, inputId = 'concentrated_disadvantage', value = START_VALUE)
    updateSliderInput(session, inputId = 'edu_completion', value = START_VALUE)
    updateSliderInput(session, inputId = 'illegal_econ_activity', value = START_VALUE)
    
  })
  #Parent Awareness
  observeEvent(input$parent_reset_weights, {
    updateSliderInput(session, inputId = 'child_maltreatment_pa', value = START_VALUE)
    updateSliderInput(session, inputId = 'prop_single_mothers', value = START_VALUE)
    updateSliderInput(session, inputId = 'teen_mothers', value = START_VALUE)
    updateSliderInput(session, inputId = 'families_in_poverty', value = START_VALUE)
    updateSliderInput(session, inputId = 'families_unaware_of_community', value = START_VALUE)
    updateSliderInput(session, inputId = 'drug_abuse', value = START_VALUE)
    
  })
  
  #updating metrics, data tables, and displaying plot
  observeEvent(input$gang_presence_map,{
    { 
    class_weights = data.frame(
       matrix(
         data = c(
           'gang_affiliated_crime',           input$gang_affiliated_crime,
           'graffiti',                        input$graffiti,
           'child_maltreatment',              input$child_maltreatment,
           'developmental_trauma',            input$developmental_trauma,
           'violent_prejudice_victimization', input$violent_prejudice_victimization,
           'disorder_in_neighborhood',        input$disorder_in_neighborhood,
           'presence_of_illegal_firearms',    input$presence_of_firearms,
           'trouble_at_school',               input$youth_school_conflicts,
           'youth_with_disability',           input$reported_disability,
           'economic_deprivation',            input$economic_deprivation,
           'substance_abuse',                 input$substance_abuse#, 
           #'social_discrimination',           0.2
         ),
         ncol = 2,
         byrow = TRUE
       ), stringsAsFactors = FALSE
     )
     
     
     focus_col = all_metrics[[1]][[1]][1]
     new_big_list = get_new_big_list(class_weights = class_weights, big_list = big_list, replace_col = focus_col)
     

     #columns of focus
     gp_sub_cols = c(all_metrics[[1]][[1]], class_weights[as.numeric(class_weights[,2]) > 0,1])
     #color pallettes
     # gp_sub_pals = get_pals(new_big_list, all_metrics[[1]][[1]])
     #labels
     # gp_sub_labels = get_labels(new_big_list, gp_sub_cols)
    }
     
     map = map
     big_list = new_big_list
     metric_title = all_metrics[[1]][[1]]
     label_metric_cols = gp_sub_cols
     tile_opacity = TILE_OPACITY
     pred_dat = NULL
     pred_title = NULL
     hotspot_15 = hotspot_15
     hotspot_17 = hotspot_17
     school_points = school_points
     quantile_bins = QUANTILE_BINS
     cd_bounds = cd_bounds
     
     gp_sub_map = make_map(map, big_list, metric_title, label_metric_cols, hotspot_15, hotspot_17, school_points, cd_bounds, school_icons, html_legend_school_icons,
                           pred_dat, pred_title, tile_opacity, quantile_bins, pallete_colors, reverse_pal,
                           cd_colors, hotspot_15_colors, hotspot_17_colors, brightness_perc, raw_list, raw_data_cols, raw_data_col_names,
                           council_centroid, hotspot_15_centroid, hotspot_17_centroid, label_transparency)
     

     
     output$need_map = renderLeaflet(gp_sub_map)
     
     #updating datatables
     output$metric_focus = renderText('Top Neighborhoods by Gang Presence Metrics')
     {
       dt_2013 = datatable(data.frame(Neighborhood = big_list[[1]][[2]]@data$neib_name,
                                      risk_level= big_list[[1]][[2]]@data$GANG_PRESENCE, 
                                      Risk = get_quantile(big_list[[1]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                       ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                             mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
       output$dt_2013 = DT::renderDataTable(dt_2013) 
       
       dt_2014 = datatable(data.frame(Neighborhood = big_list[[2]][[2]]@data$neib_name,
                                      risk_level= big_list[[2]][[2]]@data$GANG_PRESENCE, 
                                      Risk = get_quantile(big_list[[2]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                       ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                             mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
       output$dt_2014 = DT::renderDataTable(dt_2014)
       
       dt_2015 = datatable(data.frame(Neighborhood = big_list[[3]][[2]]@data$neib_name,
                                      risk_level= big_list[[3]][[2]]@data$GANG_PRESENCE, 
                                      Risk = get_quantile(big_list[[3]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                       ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                             mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
       output$dt_2015 = DT::renderDataTable(dt_2015)
       
       dt_2016 = datatable(data.frame(Neighborhood = big_list[[4]][[2]]@data$neib_name,
                                      risk_level= big_list[[4]][[2]]@data$GANG_PRESENCE, 
                                      Risk = get_quantile(big_list[[4]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                       ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                             mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
       output$dt_2016 = DT::renderDataTable(dt_2016)
       
       dt_2017 = datatable(data.frame(Neighborhood = big_list[[5]][[2]]@data$neib_name,
                                      risk_level= big_list[[5]][[2]]@data$GANG_PRESENCE, 
                                      Risk = get_quantile(big_list[[5]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                       ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                             mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
       output$dt_2017 = DT::renderDataTable(dt_2017)
       
       dt_2018 = datatable(data.frame(Neighborhood = big_list[[6]][[2]]@data$neib_name,
                                      risk_level= big_list[[6]][[2]]@data$GANG_PRESENCE, 
                                      Risk = get_quantile(big_list[[6]][[2]]@data$GANG_PRESENCE, QUANTILE_BINS, 
                                                                       ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                             mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
       output$dt_2018 = DT::renderDataTable(dt_2018)
     }
     
     
     
     
     # print(class_weights)
     # print(new_big_list[[1]][[2]]@data[1:4,])
     
  })
  observeEvent(input$vocational_training_map, {
    {
    class_weights = data.frame(
      matrix(
        data = c(
          'joblessness',               input$joblessness,
          'poverty_in_community',      input$poverty_in_community,
          'concentrated_disadvantage', input$concentrated_disadvantage,
          'edu_completion',            input$edu_completion,
          'illegal_econ_activity',     input$illegal_econ_activity
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    
      # class_weights = data.frame(
      #   matrix(
      #     data = c(
      #       'joblessness',               1,
      #       'poverty_in_community',      1,
      #       'concentrated_disadvantage', 1,
      #       'edu_completion',            1,
      #       'illegal_econ_activity',     1
      #     ),
      #     ncol = 2,
      #     byrow = TRUE
      #   ), stringsAsFactors = FALSE
      # )
    
    focus_col = all_metrics[[2]][[1]][1]
    new_big_list = get_new_big_list(class_weights = class_weights, big_list = big_list, replace_col = focus_col)
    

    #columns of focus
    vocational_cols = c(all_metrics[[2]][[1]], class_weights[as.numeric(class_weights[,2]) > 0,1])
    #color pallettes
    vocational_pals = get_pals(new_big_list, all_metrics[[2]][[1]])
    #labels
    vocational_labels = get_labels(new_big_list, vocational_cols)
    }
    #putting together map
    map = map
    big_list = new_big_list
    metric_title = all_metrics[[2]][[1]]
    label_metric_cols = vocational_cols
    tile_opacity = TILE_OPACITY
    pred_dat = NULL
    pred_title = NULL
    hotspot_15 = hotspot_15
    hotspot_17 = hotspot_17
    school_points = school_points
    quantile_bins = QUANTILE_BINS
    cd_bounds = cd_bounds
    
    vocational_map = make_map(map, big_list, metric_title, label_metric_cols, hotspot_15, hotspot_17, school_points, cd_bounds, school_icons, html_legend_school_icons,
                              pred_dat, pred_title, tile_opacity, quantile_bins, pallete_colors, reverse_pal,
                              cd_colors, hotspot_15_colors, hotspot_17_colors, brightness_perc, raw_list, raw_data_cols, raw_data_col_names,
                              council_centroid, hotspot_15_centroid, hotspot_17_centroid, label_transparency)

    
    output$need_map = renderLeaflet(vocational_map)
    
    
    
    #updating datatables
    output$metric_focus = renderText('Top Neighborhoods by Vocational Training Metrics')
    {
      dt_2013 = datatable(data.frame(Neighborhood = big_list[[1]][[2]]@data$neib_name,
                                     risk_level= big_list[[1]][[2]]@data$VOCATIONAL_TRAINING, 
                                     Risk = get_quantile(big_list[[1]][[2]]@data$VOCATIONAL_TRAINING, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2013 = DT::renderDataTable(dt_2013) 
      
      dt_2014 = datatable(data.frame(Neighborhood = big_list[[2]][[2]]@data$neib_name,
                                     risk_level= big_list[[2]][[2]]@data$VOCATIONAL_TRAINING, 
                                     Risk = get_quantile(big_list[[2]][[2]]@data$VOCATIONAL_TRAINING, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2014 = DT::renderDataTable(dt_2014)
      
      dt_2015 = datatable(data.frame(Neighborhood = big_list[[3]][[2]]@data$neib_name,
                                     risk_level= big_list[[3]][[2]]@data$VOCATIONAL_TRAINING, 
                                     Risk = get_quantile(big_list[[3]][[2]]@data$VOCATIONAL_TRAINING, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2015 = DT::renderDataTable(dt_2015)
      
      dt_2016 = datatable(data.frame(Neighborhood = big_list[[4]][[2]]@data$neib_name,
                                     risk_level= big_list[[4]][[2]]@data$VOCATIONAL_TRAINING, 
                                     Risk = get_quantile(big_list[[4]][[2]]@data$VOCATIONAL_TRAINING, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2016 = DT::renderDataTable(dt_2016)
      
      dt_2017 = datatable(data.frame(Neighborhood = big_list[[5]][[2]]@data$neib_name,
                                     risk_level= big_list[[5]][[2]]@data$VOCATIONAL_TRAINING, 
                                     Risk = get_quantile(big_list[[5]][[2]]@data$VOCATIONAL_TRAINING, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2017 = DT::renderDataTable(dt_2017)
      
      dt_2018 = datatable(data.frame(Neighborhood = big_list[[6]][[2]]@data$neib_name,
                                     risk_level= big_list[[6]][[2]]@data$VOCATIONAL_TRAINING, 
                                     Risk = get_quantile(big_list[[6]][[2]]@data$VOCATIONAL_TRAINING, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2018 = DT::renderDataTable(dt_2018)
    }
    
    
    
    
    #print(class_weights)#works
  })
  observeEvent(input$parent_awareness_map, {
    {
    class_weights = data.frame(
      matrix(
        data = c(
          'child_maltreatment',                input$child_maltreatment_pa,
          'prop_single_mothers',               input$prop_single_mothers,
          'teen_mothers',                      input$teen_mothers,
          'families_in_poverty',               input$families_in_poverty,
          'families_unaware_of_community',     input$families_unaware_of_community,
          'drug_abuse',                        input$drug_abuse
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    
    focus_col = all_metrics[[3]][[1]][1]
    new_big_list = get_new_big_list(class_weights = class_weights, big_list = big_list, replace_col = focus_col)
    
    
    
    #columns of focus
    parent_cols = c(all_metrics[[3]][[1]], class_weights[as.numeric(class_weights[,2]) > 0,1])
    #color pallettes
    parent_pals = get_pals(new_big_list, all_metrics[[3]][[1]])
    #labels
    parent_labels = get_labels(new_big_list, parent_cols)
    }
    #putting together map
    map = map
    big_list = new_big_list
    metric_title = all_metrics[[3]][[1]]
    label_metric_cols = parent_cols
    tile_opacity = TILE_OPACITY
    pred_dat = NULL
    pred_title = NULL
    hotspot_15 = hotspot_15
    hotspot_17 = hotspot_17
    school_points = school_points
    quantile_bins = QUANTILE_BINS
    cd_bounds = cd_bounds
    
    parent_map = make_map(map, big_list, metric_title, label_metric_cols, hotspot_15, hotspot_17, school_points, cd_bounds, school_icons, html_legend_school_icons,
                          pred_dat, pred_title, tile_opacity, quantile_bins, pallete_colors, reverse_pal,
                          cd_colors, hotspot_15_colors, hotspot_17_colors, brightness_perc, raw_list, raw_data_cols, raw_data_col_names,
                          council_centroid, hotspot_15_centroid, hotspot_17_centroid, label_transparency)
    
  
    output$need_map = renderLeaflet(parent_map)
    
    #updating datatables
    #dt_2013, dt_2014, dt_2015, dt_2016, dt_2017, dt_2018
    output$metric_focus = renderText('Top Neighborhoods by Parent Awareness Metrics')
    {
      dt_2013 = datatable(data.frame(Neighborhood = big_list[[1]][[2]]@data$neib_name,
                                     risk_level= big_list[[1]][[2]]@data$PARENT_AWARENESS, 
                                     Risk = get_quantile(big_list[[1]][[2]]@data$PARENT_AWARENESS, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2013 = DT::renderDataTable(dt_2013) 
      
      dt_2014 = datatable(data.frame(Neighborhood = big_list[[2]][[2]]@data$neib_name,
                                     risk_level= big_list[[2]][[2]]@data$PARENT_AWARENESS, 
                                     Risk = get_quantile(big_list[[2]][[2]]@data$PARENT_AWARENESS, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2014 = DT::renderDataTable(dt_2014)
      
      dt_2015 = datatable(data.frame(Neighborhood = big_list[[3]][[2]]@data$neib_name,
                                     risk_level= big_list[[3]][[2]]@data$PARENT_AWARENESS, 
                                     Risk = get_quantile(big_list[[3]][[2]]@data$PARENT_AWARENESS, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2015 = DT::renderDataTable(dt_2015)
      
      dt_2016 = datatable(data.frame(Neighborhood = big_list[[4]][[2]]@data$neib_name,
                                     risk_level= big_list[[4]][[2]]@data$PARENT_AWARENESS, 
                                     Risk = get_quantile(big_list[[4]][[2]]@data$PARENT_AWARENESS, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2016 = DT::renderDataTable(dt_2016)
      
      dt_2017 = datatable(data.frame(Neighborhood = big_list[[5]][[2]]@data$neib_name,
                                     risk_level= big_list[[5]][[2]]@data$PARENT_AWARENESS, 
                                     Risk = get_quantile(big_list[[5]][[2]]@data$PARENT_AWARENESS, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2017 = DT::renderDataTable(dt_2017)
      
      dt_2018 = datatable(data.frame(Neighborhood = big_list[[6]][[2]]@data$neib_name,
                                     risk_level= big_list[[6]][[2]]@data$PARENT_AWARENESS, 
                                     Risk = get_quantile(big_list[[6]][[2]]@data$PARENT_AWARENESS, QUANTILE_BINS, 
                                                                      ret_factor = FALSE)) %>% dplyr::arrange(-risk_level) %>%
                            mutate(Risk = paste0(round(Risk,4), '%ile')) %>% select(-risk_level), selection = 'single')
      output$dt_2018 = DT::renderDataTable(dt_2018)
    }
    #print(class_weights)#works
  })
  
  observeEvent(input$need_map_groups, print(input$need_map_groups[2]))
  
  data_list_for_display = reactiveVal()
  
  # display_data = reactive({
  # 
  # })
  
  
  
})
