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
                    # pallete_colors = 'plasma', reverse_pal = FALSE, 
                    pallete_colors = 'brewer yellow-green-blue', reverse_pal = FALSE, 
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
