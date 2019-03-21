######### Standard Functions ##########


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


####### Loading Libraries ###########


# install_and_load('tidyverse')
# install_and_load('tidycensus')
# install_and_load('tigris')
# install_and_load('leaflet')
# install_and_load('stringr')
# install_and_load('sf')
install_and_load('sp')
install_and_load('rgeos')


########### Globally-used functions ############
#given the list of possible GEOIDs, and the list of geoids of the column of interest, returns a 2-column dataframe 
#of the counts of how many times each geoid appears (including zero times)
count_tract_appearance = function(geoid_list, subject_geoid){
  return(data.frame(table(c(subject_geoid, geoid_list)) - 1, stringsAsFactors = F) %>% arrange(Var1))
}


########### CONSTANTS #########

START_DATE_FULL = as.Date('2013-01-01')
END_DATE_FULL = as.Date('2016-01-01')
TOP_METRICS = 3 #number of top metrics to highlight for each tract
SUBSTANCE_ABUSE_WEIGHT = 0.15 #when combining gang presence and substance abuse, the weighting of substnace abuse
YEARS_FORWARD = 1
DAYS_FORWARD = 365*YEARS_FORWARD



SQUARE_METER_TO_SQUARE_MILE = 3.86102E-7
INCLUDED_LEVELS = c(1)
ACS_YEAR = 2016

start_week_range = seq.Date(START_DATE_FULL, (END_DATE_FULL-DAYS_FORWARD), by = 'week')
end_week_range = start_week_range + DAYS_FORWARD
start_week_pred_range = end_week_range
end_week_pred_range = start_week_pred_range + DAYS_FORWARD

######## Reading Constant Data #############
#importing the crime data
crime_data_full = readRDS('Crimes only by Census Tract.rds')

ssci_incidents_full = readRDS('SSCI_IR_ma_16-18_for_plotting.rds') 

sj = readRDS('sj_shapefile.rds')


loc_dist_matrix = readRDS('Santa Clara County Tract 1 Neighbor Matrix.rds')

full_spdf_backup = readRDS('Metrics without time dependency.rds')


########
#All the data manipulation below takes around 4 seconds
########
big_dat = NULL


########### Setting up the year and date for each cycle ###########

for(week_ind in seq_along(start_week_range)){
  full_spdf = full_spdf_backup
  
  START_DATE = start_week_range[week_ind]
  END_DATE = end_week_range[week_ind]
  START_TIME = as.POSIXct(as.character(START_DATE))
  END_TIME = as.POSIXct(as.character(END_DATE))
  
  
  ############# Crime Data  - crime_data, crime_by_tract, and full_spdf ############
  install_and_load('dplyr')
  
  
  crime_data = crime_data_full %>% dplyr::filter(rpt_dt > START_TIME, rpt_dt < END_TIME)
  
  
  
  #organizing the crimes of importance here. 
  {crimes = data.frame(
    #crime title
    crime = c(
      #vocational training
      'AUTO THEFT',
      'BURGLARY',
      'NARCOTICS',
      'SEX',
      'GRAND THEFT',
      #Substance Abuse
      'DUI', #DUI with drugs or alc
      '647(F) PC DRUNK IN PUBLIC', #also include NARCOTICS
      '25662(A) BP MINOR POSN ALCOHOL',
      '308BPC MINOR POSN CIG/TOBACCO',
      #Parent Awareness
      'CHILD ABUSE',
      'CHILD MOLEST',
      'CHILD NEGLECT',
      '243(E)(1) PC BATTERY DOMESTIC', '273.5(A) PC DOMESTIC VIOLENCE', #Domestic Violence
      'MISSING JUV', #runaway youth
      #Gang Presence
      'AGG ASSAULT', '261.5PC UNLAW INTERCRSE MINOR',#Developmental Trauma. Also include child molestation above
      #Child Maltreatment. Also include domestic violence, child neglect/abuse, and Runaway youth
      'ROBBERY', 'ASSAULT', '422 PC CRIMINAL THREATS', #Violent and Prejudice Victimization. Also include AGG ASSAULT. 422 is threats or hate crimes.
      '594 PC VANDALISM-GRAFFITI', 'DISTURB PEACE', #Disorder, also include NARCOTICS, and DUI and drunk in public
      'WEAPONS', '211 PC ROBBERY ARMED', '211 PC ROBBERY STRONG-ARM', 'DOMESTIC-211 PC ARMED ROBBERY', #Weapons. This is all Weapons possession and armed robberies.
      'GANG CRIME'
    ),
    #which column the title is from (usually OFFENSE_DESCRIPTION or CATEGORY)
    column = paste0('c_',c(
      #vocational training
      'auto_theft',
      'burglary',
      'narcotics',
      'sex',
      'grand_theft',
      #substance abuse
      'dui', #DUI with drugs or alc
      'drunk_in_public',
      'minor_alcohol',
      'minor_tobacco',
      #Parent Awarenesss
      'child_abuse',
      'child_molest',
      'child_neglect',
      'domestic_battery', 'domestic_violence',
      'missing_juv',
      #Gang Presence
      'agg_assault', 'intercourse_with_minor', #Also include child molestation above
      #Child Maltreatment. Also include domestic violence, child neglect/abuse, and Runaway youth
      'robbery', 'assault', 'threats',  #Violent and Prejudice Victimization. Also include AGG ASSAULT. 422 is threats or hate crimes.
      'graffiti', 'disturbance', #Disorder, also include NARCOTICS and DUI and drunk in public
      'weapons', 'armed_robbery', 'strong_armed_robbery', 'domestic_armed_robbery',
      'gang_crime'
    )), stringsAsFactors = F
  )
  }
  
  #also have to account for gang involvement
  gang_cols = c('STUDY_FLAG', 'GANG_INV')
  gang_labels = c('GI', 'GIJD', 'GINA', 'M', 'R')
  
  crime_data = crime_data[crime_data$OFFENSE_DESCRIPTION %in% crimes[,1] | crime_data$CATEGORY %in% crimes[,1] |
                            crime_data$STUDY_FLAG %in% gang_labels | crime_data$GANG_INV %in% gang_labels,]
  
  crimes_matrix = matrix(0, nrow = nrow(full_spdf@data), ncol = (length(crimes[,1])))
  colnames(crimes_matrix) = c(crimes[,1])
  crime_by_tract = data.frame(GEOID = full_spdf@data$GEOID, crimes_matrix, stringsAsFactors = F) %>% arrange(GEOID)
  colnames(crime_by_tract) = c('GEOID', crimes[,1])
  # geoid_list = crime_by_tract$GEOID
  # subject_geoid = crime_data[crime_data$CATEGORY == 'AGG ASSAULT','geoid']
  
  
  geoid_list = crime_by_tract$GEOID
  for(n in 2:(ncol(crime_by_tract) - 1)){
    crime_by_tract[,n] = count_tract_appearance(geoid_list = geoid_list, 
                                                subject_geoid = crime_data[crime_data$CATEGORY == colnames(crime_by_tract)[n] | 
                                                                             crime_data$OFFENSE_DESCRIPTION == colnames(crime_by_tract)[n],'geoid'])[,2]
  }
  
  crime_by_tract[,ncol(crime_by_tract)] = count_tract_appearance(geoid_list = geoid_list,
                                                                 subject_geoid = crime_data[crime_data$STUDY_FLAG %in% gang_labels | crime_data$GANG_INV %in% gang_labels,'geoid'])[,2]
  
  colnames(crime_by_tract) = c('GEOID', crimes[,2])
  #Sanity check --> the above works, and quickly.
  #print(crime_data[crime_data$geoid == '06085502907',c('CATEGORY', 'OFFENSE_DESCRIPTION')] %>% arrange(CATEGORY, OFFENSE_DESCRIPTION))
  
  
  full_spdf@data = merge(full_spdf@data, crime_by_tract, by = 'GEOID')
  
  
  ############ SSCI Incident Data - incident_counts, full_spdf ##########
  
  school_bounds = readRDS('school_bounds_for_16-18_IRs.rds') %>% sp::spTransform(full_spdf@proj4string)
  ssci_incidents = ssci_incidents_full %>% dplyr::filter(level %in% INCLUDED_LEVELS) %>%
    select(date, school, level) %>%
    filter(date > START_DATE, date < END_DATE) %>% select(-date)
  
  if(nrow(ssci_incidents) > 1){
    
    ssci = stats::aggregate(ssci_incidents$level, by = list(ssci_incidents$school), FUN = sum)
    ssci_bounds = school_bounds[school_bounds$OBJECTID %in% ssci$Group.1,]
    
    intersect_matrix = matrix(data = 0, nrow = nrow(ssci_bounds@data), ncol = nrow(full_spdf@data))
    colnames(intersect_matrix) = full_spdf@data$GEOID
    rownames(intersect_matrix) = ssci_bounds@data$OBJECTID
    
    
    #Let's just attempt to identify the tracts that a single district is in.
    # one_dis = list(school_bounds@polygons[[1]]) %>% sp::SpatialPolygons(proj4string =  full_spdf@proj4string)
    for(school_ind in seq_along(ssci_bounds@polygons)){
      intersect_matrix[school_ind,which(rgeos::gIntersects(school_bounds[school_ind,], full_spdf, byid = TRUE))] = 1
    }
    #Now we have a matrix which marks every tract that is in every school boundary as 1.
    
    incident_counts = data.frame(GEOID = colnames(intersect_matrix), ssci_incidents = as.numeric(colSums(intersect_matrix)))
    
    full_spdf@data = merge(full_spdf@data, incident_counts, by = 'GEOID')
  }else{
    full_spdf@data$ssci_incidents = 0
  }
  
  
  ########### Neibor Matrix - loc_dist_matrix ###########
  # # 1/x_ij where x is number of blocks between block i and j (starting at 1), 0 if more than MAX_BLOCK_DIST away 
  # MAX_LOC_DIST = 1
  # loc_it = 1
  # 
  # #initializing the matrix
  # loc_dist_matrix = matrix(0, nrow = nrow(full_spdf@data), ncol = nrow(full_spdf@data))
  # loc_matrix = rgeos::gTouches(full_spdf, byid = TRUE)
  # 
  # 
  # 
  # #iterates through all blocks of 1 - MAX_BLOCK_DIST away, identifies which iteration it was picked up, and marks that number into matrix
  # #this will likely take hours, so we are going to run it overnight. Let's transfer this r code and the census df to our local machine.
  # for(loc_it_count in loc_it : ncol(loc_dist_matrix)){
  #   layer_locs = loc_it_count
  #   marked_locs = loc_it_count
  #   for(its in 1  : MAX_LOC_DIST){
  #     if(length(layer_locs) > 1){
  #       layer_locs_vec = which(rowSums(loc_matrix[,layer_locs])>0)
  #       
  #     }else{
  #       layer_locs_vec = which(loc_matrix[,layer_locs])
  #     }
  #     layer_locs_vec = layer_locs_vec[which(!(layer_locs_vec %in% marked_locs))]
  #     loc_dist_matrix[layer_locs_vec,loc_it_count] = its
  #     layer_locs = layer_locs_vec
  #     marked_locs = c(marked_locs, layer_locs)
  #   }
  #   # if(loc_it_count %% 50 == 0) print(loc_it_count)
  #   
  # }
  # # saveRDS(block_dist_matrix, 'block_dist_matrix.rds')
  # # saveRDS(block, 'current_block_it.rds')
  # # saveRDS(block_matrix, 'block_matrix_t_f.rds')
  # #checking to make sure the above works - it seems to work.
  # # plot(full_spdf[loc_dist_matrix[,200] > 0 & loc_dist_matrix[,200] < 3,])
  # 
  # colnames(loc_dist_matrix) = full_spdf@data$GEOID
  # rownames(loc_dist_matrix) = full_spdf@data$GEOID
  # 
  # 
  # # loc_dist_matrix = 1/loc_dist_matrix
  # 
  # # loc_dist_matrix[loc_dist_matrix > 1] = 0
  # 
  # saveRDS(loc_dist_matrix, 'Santa Clara County Tract 1 Neighbor Matrix.rds')
  
  ############# That's all the data. #############
  
  
  ############# Handling data operations and dropping certain data - need_metrics_spdf #############
  
  
  
  columns_to_convert_to_percentages_of_total_population = 
    c('unemployed', 'not_in_labor_force', 'below_poverty_line', 
      'no_diploma_18_24_male', 'no_diploma_18_24_female', 'under_18_pop', 'under_18_with_disability', 'single_mothers',
      'no_diploma_25_64', 'family_in_poverty', 'entered_2010_later', 'foreign_born', 'disability_under_18',
      'income_under_poverty_line', 'Poverty_6_and_younger', 'Poverty_6_11', 'Poverty_12_17',
      crimes[,2], 'ssci_incidents')
  
  #calculating ethnic diversity index (edi)
  race_cols = c('white', 'black', 'asian', 
                'hispanic', 'amiakn', 'hawaiipi', 'other_races', 'two_or_more_races')
  
  #calculating diversity index - 
  num_races = length(race_cols)
  total_race_pops = rowSums(full_spdf@data[,race_cols])
  race_fractions = full_spdf@data[,race_cols] / total_race_pops
  race_fractions_reduced = (race_fractions - (1/num_races))^2 #this works element-wise, so it works as we want it to.
  d_8 = sqrt(rowSums(race_fractions_reduced))
  c1 = 100
  c2 = -100*sqrt(num_races*(num_races-1))/(num_races - 1)
  edi = c1 + (c2*d_8)
  
  #works
  # print(edi[1:6])
  # print(full_spdf@data[1:6, race_cols])
  
  #Turning all of the census and crime data into percentages of total population
  full_spdf@data[,columns_to_convert_to_percentages_of_total_population] = 
    full_spdf@data[,columns_to_convert_to_percentages_of_total_population]/full_spdf@data$total_pop
  
  full_spdf@data$edi = as.numeric(scale(edi)) # we are scaling edi because it's the only one with such a clustering around the center
  
  #creating the "youth in poverty" variable
  full_spdf$youth_in_poverty = full_spdf$Poverty_6_and_younger + full_spdf$Poverty_6_11 + full_spdf$Poverty_12_17
  #creating a variable for all 18-24 yo without a high school diploma
  full_spdf$no_diploma_18_24 = full_spdf$no_diploma_18_24_female + full_spdf$no_diploma_18_24_male
  
  
  need_metrics_spdf = full_spdf
  need_metrics_spdf@data = need_metrics_spdf@data[,-which(colnames(need_metrics_spdf@data) %in% c('ALAND', 'overobese_teen',
                                                                                                  'overobese_child', 'OBJECTID',
                                                                                                  'tract', race_cols, 'Poverty_12_17',
                                                                                                  'Poverty_6_11', 'Poverty_6_and_younger',
                                                                                                  'no_diploma_18_24_male',
                                                                                                  'no_diploma_18_24_female',
                                                                                                  'under_18_pop',
                                                                                                  'sampled_pop', 'neibid'))]
  
  
  
  ########## Calculating Concentrated Economic Disadvantage ###########
  
  #relative poverty
  poverty_vec = loc_dist_matrix %*% need_metrics_spdf@data$below_poverty_line
  division_vec = loc_dist_matrix %*% rep(1, nrow(loc_dist_matrix))
  need_metrics_spdf@data$rel_poverty = need_metrics_spdf@data$below_poverty_line/(poverty_vec/division_vec)
  
  #relative unemployment
  unemployment_vec = loc_dist_matrix %*% need_metrics_spdf@data$unemployed
  need_metrics_spdf@data$rel_unemployment = need_metrics_spdf@data$unemployed/(unemployment_vec/division_vec)
  
  
  
  ############# Keeping only San Jose Census Tracts - need_metrics_spdf #########
  
  # sj = tigris::places('CA') %>% sp::spTransform(need_metrics_spdf@proj4string)
  # sj = sj[sj@data$NAME == 'San Jose',]
  
  # saveRDS(sj, 'sj_shapefile.rds')
  
  #identifying all of the tracts which intersect with or are within san jose
  # tract_city_matrix = matrix(data = 0, nrow = nrow(full_spdf@data), ncol = 1)
  # rownames(tract_city_matrix) = need_metrics_spdf@data$GEOID
  # colnames(tract_city_matrix) = seq_along(sj@polygons)
  
  need_metrics_spdf = need_metrics_spdf[which(gIntersects(sj, need_metrics_spdf, byid = TRUE)),]
  
  
  ############# FUNCTION Calculating percentiles for each metric ################
  get_percentile = function(vec){
    return(ecdf(vec)(vec))
  }
  
  
  ######### Min-Max Scaling all of the variables - need_metrics_spdf ##########
  
  #given a vector, min-max scales it.
  min_max_vec = function(vec, ...){
    if(max(vec, ...) == min(vec,...)){
      return(rep(0, length(vec)))
    }
    return((vec - min(vec, ...))/(max(vec,...)-min(vec,...)))
  }
  
  #minmax scaling all of the metrics
  for(n in 4 : ncol(need_metrics_spdf@data)){
    need_metrics_spdf@data[,n] = min_max_vec(need_metrics_spdf@data[,n])
  }
  
  
  
  
  
  ############ Setting up the metrics ##############
  
  #given the formatted dfs above for both the metric-specific weights and the class-specific weights, returns the accurate weight for each metric
  calculate_final_metric_weights = function(metrics_df, class_df){
    for(class_ind in seq_along(class_df[,1])){
      class_metric_weights = metrics_df[metrics_df[,3] == class_df[class_ind,1],2] #gets the weights for all metrics in the class
      revised_weights = (class_metric_weights/length(class_metric_weights)) * class_df[class_ind,2]
      metrics_df[metrics_df[,3] == class_df[class_ind,1],2] = revised_weights
    }
    return(metrics_df)
  }
  #given the full dataset of need metrics and the final metrics weight_df for an ES area, returns a df of the GEOID and its value for that ES
  get_ES_measurement = function(need_metric_df, weight_df, return_weight_scores = FALSE){
    included_metric_cols = need_metric_df[, which(colnames(need_metric_df) %in% weight_df[,1])]
    es_vec = rep(0, nrow(need_metric_df))
    
    class_scores = array(0, dim = c(nrow(need_metric_df), length(unique(weight_df[,3]))))
    colnames(class_scores) = unique(weight_df[,3])
    
    for(class in colnames(class_scores)){
      class_vars = included_metric_cols[weight_df[weight_df[,3] == class,1]]
      class_weights = weight_df[weight_df[,3] == class,2]
      for(n in seq_along(class_vars[1,])){
        class_vars[,n] = class_vars[,n] * class_weights[n]
      }
      class_scores[,class] = rowSums(class_vars)
    }
    es_vec = rowSums(class_scores)
    if(return_weight_scores){
      ret_arr = data.frame(GEOID = need_metric_df$GEOID, es_metric = min_max_vec(as.numeric(scale(es_vec))), class_scores)
    }else{
      ret_arr = data.frame(GEOID = need_metric_df$GEOID, es_metric = min_max_vec(as.numeric(scale(es_vec))))
    }
    return(ret_arr)
  }
  
  get_top_metrics = function(need_metric_df, weight_df, top_metrics_n = 3){
    included_metric_cols = need_metric_df[, which(colnames(need_metric_df) %in% weight_df[,1])]
    top_n_cols = array(NA, dim = c(nrow(need_metric_df), top_metrics_n*2))
    col_order = rep(c(1, (top_metrics_n+1)), top_metrics_n) + rep(c(0:(top_metrics_n - 1)), each = 2) #this orders the columns as metric name, metric val, metric name, metric val...
    colnames(top_n_cols) = c(paste0('metric_', seq_len(top_metrics_n)), 
                             paste0('value_metric_', seq_len(top_metrics_n)))#[col_order]
    for(row_ind in seq_along(included_metric_cols[,1])){
      row = included_metric_cols[row_ind,]
      row = row[order(-row)][seq_len(top_metrics_n)]
      top_n_cols[row_ind,] = c(names(row), as.numeric(row))
      
    }
    ret_top_n = data.frame(GEOID = need_metric_df$GEOID, top_n_cols, stringsAsFactors = FALSE)
    for(n in (top_metrics_n + 2 ): ncol(ret_top_n)){
      ret_top_n[,n] = as.numeric(ret_top_n[,n])
    }
    return(ret_top_n[,c(1, col_order + 1)])
  }
  
  
  # head(need_metrics_spdf@data, 2)
  
  #making the final metrics map
  fin_metrics_spdf = need_metrics_spdf
  fin_metrics_spdf@data = data.frame(GEOID = fin_metrics_spdf@data$GEOID)
  
  
  ##STREET OUTREACH, PERSONAL TRANSFORMATION, AND CASE MANAGEMENT
  {GANG_PRESENCE_METRICS = data.frame(matrix(
    data = c(
      'c_domestic_violence',                      1, 'child_maltreatment',
      'c_domestic_battery',                       1, 'child_maltreatment',
      'c_domestic_armed_robbery',                 1, 'child_maltreatment',
      'c_child_neglect',                          1, 'child_maltreatment',
      'c_child_abuse',                            1, 'child_maltreatment',
      'c_missing_juv',                            1, 'child_maltreatment',
      'c_child_molest',                           1, 'developmental_trauma',
      'c_intercourse_with_minor',                 1, 'developmental_trauma',#this is sex abuse with a youth
      'c_agg_assault',                            1, 'violent_prejudice_victimization',
      'c_robbery',                                1, 'violent_prejudice_victimization',
      'c_assault',                                1, 'violent_prejudice_victimization',
      'c_threats',                                1, 'violent_prejudice_victimization', #hate crimes and threats
      'c_graffiti',                               1, 'disorder_in_neighborhood',
      'c_dui',                                    1, 'disorder_in_neighborhood',
      'c_disturbance',                            1, 'disorder_in_neighborhood', #public nuisance
      'c_drunk_in_public',                        1, 'disorder_in_neighborhood',
      'c_narcotics',                              1, 'disorder_in_neighborhood',
      'c_weapons',                                1, 'presence_of_illegal_firearms',
      'c_armed_robbery',                          1, 'presence_of_illegal_firearms',
      'c_strong_armed_robbery',                   1, 'presence_of_illegal_firearms',
      'c_gang_crime',                             1, 'gang_affiliated_crime',
      'ssci_incidents',                           1, 'youth_school_conflicts',
      'disability_under_18',                      1, 'reported_disability',
      'family_in_poverty',                        1, 'economic_deprivation',
      'youth_in_poverty',                         1, 'economic_deprivation'#,
      #'edi',                                      1, 'social_discrimination'
    ),
    ncol = 3,
    byrow = TRUE
  ), stringsAsFactors = FALSE)
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    # length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = data.frame(
      matrix(
        data = c(
          'child_maltreatment',              1,
          'developmental_trauma',            1,
          'violent_prejudice_victimization', 1,
          'disorder_in_neighborhood',        1,
          'presence_of_firearms',            1,
          'youth_school_conflicts',          1,
          'reported_disability',             1,
          'economic_deprivation',            1, 
          'social_discrimination',           0.2
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', 'gang_presence', colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
  }
  ##VOCATIONAL TRAINING
  {GANG_PRESENCE_METRICS = data.frame(matrix(
    data = c(
      'unemployed',                 1, 'joblessness',
      'below_poverty_line',         1, 'poverty_in_community',
      'rel_unemployment',           1, 'concentrated_disadvantage',
      'rel_poverty',                1, 'concentrated_disadvantage',
      'no_diploma_18_24',           1, 'edu_completion',
      'c_auto_theft',               1, 'illegal_econ_activity',
      'c_grand_theft',              1, 'illegal_econ_activity',
      'c_burglary',                 1, 'illegal_econ_activity',
      'c_robbery',                  1, 'illegal_econ_activity',
      'c_narcotics',                1, 'illegal_econ_activity',
      'c_sex',                      1, 'illegal_econ_activity'
    ),
    ncol = 3,
    byrow = TRUE
  ), stringsAsFactors = FALSE)
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = data.frame(
      matrix(
        data = c(
          'joblessness',               1,
          'poverty_in_community',      1,
          'concentrated_disadvantage', 1,
          'edu_completion',            1,
          'illegal_econ_activity',     1
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', 'vocational_training', colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
    
  }
  ##PARENT AWARENESS
  {GANG_PRESENCE_METRICS = data.frame(matrix(
    data = c(
      'c_domestic_violence',      1, 'child_maltreatment',
      'c_domestic_battery',       1, 'child_maltreatment',
      'c_domestic_armed_robbery', 1, 'child_maltreatment',
      'c_child_neglect',          1, 'child_maltreatment',
      'c_child_abuse',            1, 'child_maltreatment',
      'c_missing_juv',            1, 'child_maltreatment',
      'single_mothers',           1, 'prop_single_mothers',
      'birth_teen',               1, 'teen_mothers',
      'family_in_poverty',        1, 'families_in_poverty',
      'youth_in_poverty',         1, 'families_in_poverty',
      'foreign_born',             1, 'families_unaware_of_community',
      'entered_2010_later',       1, 'families_unaware_of_community',
      'c_dui',                    1, 'drug_abuse',
      'c_drunk_in_public',        1, 'drug_abuse',
      'c_narcotics',              1, 'drug_abuse', 
      'alchohol_dens',            1, 'drug_abuse', 
      'tobacco_dens',             1, 'drug_abuse',
      'weed_dens',                1, 'drug_abuse'
    ),
    ncol = 3,
    byrow = TRUE
  ), stringsAsFactors = FALSE)
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    # length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = data.frame(
      matrix(
        data = c(
          'child_maltreatment',                1,
          'prop_single_mothers',               1,
          'teen_mothers',                      1,
          'families_in_poverty',               1,
          'families_unaware_of_community',     1,
          'drug_abuse',                        1
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', 'parent_awareness', colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
  }
  ##SUBSTANCE ABUSE
  {GANG_PRESENCE_METRICS = data.frame(matrix(
    data = c(
      'disability_under_18',      1, 'youth_with_disabilities',
      'c_minor_tobacco',          1, 'alc_drug_crimes',
      'c_minor_alcohol',          1, 'alc_drug_crimes',
      'c_dui',                    1, 'alc_drug_crimes',
      'c_drunk_in_public',        1, 'alc_drug_crimes',
      'c_narcotics',              5, 'alc_drug_crimes', 
      'alchohol_dens',            1, 'alc_weed_tob_retailers', 
      'tobacco_dens',             1, 'alc_weed_tob_retailers',
      'weed_dens',                1, 'alc_weed_tob_retailers'
    ),
    ncol = 3,
    byrow = TRUE
  ), stringsAsFactors = FALSE)
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    # length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = data.frame(
      matrix(
        data = c(
          'youth_with_disabilities',       1,
          'alc_drug_crimes',               1,
          'alc_weed_tob_retailers',        1
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', 'substance_abuse', colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
    
  }
  ##merging Substance abuse with STREET OUTREACH, PERSONAL TRANSFORMATION, AND CASE MANAGEMENT
  {
    fin_metrics_spdf@data$gp_and_sub = min_max_vec(SUBSTANCE_ABUSE_WEIGHT*fin_metrics_spdf@data$substance_abuse + 
                                                     (1-SUBSTANCE_ABUSE_WEIGHT)*fin_metrics_spdf@data$gang_presence)
    #you just add substance abuse as a percentile to the gang presence stuff
  }
  
  
  ###### Removing duplicate columns from the fin_metrics_spdf - fin_metrics_spdf ###############
  
  colnames(fin_metrics_spdf@data) = gsub('\\.x', '', colnames(fin_metrics_spdf@data))
  fin_metrics_spdf@data = fin_metrics_spdf@data[,grep('\\.', colnames(fin_metrics_spdf@data), invert = TRUE)]
  
  
  ############ appending to big_dat - big_dat ############
  
  if(is.null(big_dat)){
    big_dat = data.frame(fin_metrics_spdf@data, start_date = START_DATE, end_date = END_DATE, stringsAsFactors = FALSE)
  }else{
    big_dat = rbind(big_dat, data.frame(fin_metrics_spdf@data, start_date = START_DATE, end_date = END_DATE, stringsAsFactors = FALSE))
  }
  
  ######## Printing out the progress ##########
  if(week_ind %% 10 == 0){
    print(paste0(week_ind, ' done out of ', length(start_week_range)))
  }
  
}
big_dat_backup = big_dat

############## removing the percentile variables, and changing GEOID to a character var ##############

big_dat = big_dat_backup
big_dat = big_dat[,-grep('_percentile', colnames(big_dat))]
big_dat$GEOID = as.character(big_dat$GEOID)

############# Adding Neighbor avg variables - big_dat ############

x_vars = colnames(big_dat)[which(!colnames(big_dat) %in% c('start_date', 'end_date', 'GEOID'))]

neib_matrix = data.frame(array(0, dim = c(nrow(big_dat), (length(x_vars) + 1))), stringsAsFactors = FALSE)
colnames(neib_matrix) = c('GEOID', x_vars)
neib_matrix$GEOID = big_dat$GEOID

sj_dist_matrix = loc_dist_matrix[rownames(loc_dist_matrix) %in% neib_matrix$GEOID, colnames(loc_dist_matrix) %in% neib_matrix$GEOID]

identical(rownames(sj_dist_matrix), neib_matrix$GEOID[1:(first_geoid_inds[2] - 1)])#check. They are in the same order

#given a vector of length n and the n by n neighbor matrix, returns a vector of n length of the averaged value for each GEOID's neibs on that var
get_neib_average_vec = function(vec, sj_dist_matrix){
  return((vec %*% sj_dist_matrix)/rowSums(sj_dist_matrix)) 
}#checked and works
#given a vector of cn length (where c is an integer) and the n by n neighbor matrix, returns a vector of cn length of the averaged value for each row's neibs on that var
get_full_neib_average_vec = function(vec, sj_dist_matrix){
  ret_vec = rep(0, length(vec))
  next_start_vec_ind = 1
  n = nrow(sj_dist_matrix)
  for(c in seq_len(length(vec)/n)){
    focus_inds = next_start_vec_ind:(next_start_vec_ind+n-1)
    ret_vec[focus_inds] = get_neib_average_vec(vec[focus_inds], sj_dist_matrix)
    next_start_vec_ind = next_start_vec_ind + n
  }
  return(ret_vec)
} #checked and works (loose checking, but yeah seems to work)

for(x_var in x_vars){
  neib_matrix[,x_var] = get_full_neib_average_vec(big_dat[,x_var], sj_dist_matrix)
}

colnames(neib_matrix)[2:ncol(neib_matrix)] = paste0('neib_avg_', x_vars)

if(identical(neib_matrix$GEOID, big_dat$GEOID)){
  big_dat = data.frame(big_dat, neib_matrix[,2:ncol(neib_matrix)])
}else{message('not identical, something is wrong')}

############# making dependent variables ############
dep_dat = NULL

for(week_ind in seq_along(start_week_pred_range)){
  full_spdf = full_spdf_backup
  
  START_DATE = start_week_pred_range[week_ind]
  END_DATE = end_week_pred_range[week_ind]
  START_TIME = as.POSIXct(as.character(START_DATE))
  END_TIME = as.POSIXct(as.character(END_DATE))
  
  
  ############# Crime Data  - crime_data, crime_by_tract, and full_spdf ############
  install_and_load('dplyr')
  
  
  crime_data = crime_data_full %>% dplyr::filter(rpt_dt > START_TIME, rpt_dt < END_TIME)
  
  
  
  #organizing the crimes of importance here. 
  {crimes = data.frame(
    #crime title
    crime = c(
      #vocational training
      'AUTO THEFT',
      'BURGLARY',
      'NARCOTICS',
      'SEX',
      'GRAND THEFT',
      #Substance Abuse
      'DUI', #DUI with drugs or alc
      '647(F) PC DRUNK IN PUBLIC', #also include NARCOTICS
      '25662(A) BP MINOR POSN ALCOHOL',
      '308BPC MINOR POSN CIG/TOBACCO',
      #Parent Awareness
      'CHILD ABUSE',
      'CHILD MOLEST',
      'CHILD NEGLECT',
      '243(E)(1) PC BATTERY DOMESTIC', '273.5(A) PC DOMESTIC VIOLENCE', #Domestic Violence
      'MISSING JUV', #runaway youth
      #Gang Presence
      'AGG ASSAULT', '261.5PC UNLAW INTERCRSE MINOR',#Developmental Trauma. Also include child molestation above
      #Child Maltreatment. Also include domestic violence, child neglect/abuse, and Runaway youth
      'ROBBERY', 'ASSAULT', '422 PC CRIMINAL THREATS', #Violent and Prejudice Victimization. Also include AGG ASSAULT. 422 is threats or hate crimes.
      '594 PC VANDALISM-GRAFFITI', 'DISTURB PEACE', #Disorder, also include NARCOTICS, and DUI and drunk in public
      'WEAPONS', '211 PC ROBBERY ARMED', '211 PC ROBBERY STRONG-ARM', 'DOMESTIC-211 PC ARMED ROBBERY', #Weapons. This is all Weapons possession and armed robberies.
      'GANG CRIME'
    ),
    #which column the title is from (usually OFFENSE_DESCRIPTION or CATEGORY)
    column = paste0('c_',c(
      #vocational training
      'auto_theft',
      'burglary',
      'narcotics',
      'sex',
      'grand_theft',
      #substance abuse
      'dui', #DUI with drugs or alc
      'drunk_in_public',
      'minor_alcohol',
      'minor_tobacco',
      #Parent Awarenesss
      'child_abuse',
      'child_molest',
      'child_neglect',
      'domestic_battery', 'domestic_violence',
      'missing_juv',
      #Gang Presence
      'agg_assault', 'intercourse_with_minor', #Also include child molestation above
      #Child Maltreatment. Also include domestic violence, child neglect/abuse, and Runaway youth
      'robbery', 'assault', 'threats',  #Violent and Prejudice Victimization. Also include AGG ASSAULT. 422 is threats or hate crimes.
      'graffiti', 'disturbance', #Disorder, also include NARCOTICS and DUI and drunk in public
      'weapons', 'armed_robbery', 'strong_armed_robbery', 'domestic_armed_robbery',
      'gang_crime'
    )), stringsAsFactors = F
  )
  }
  
  #also have to account for gang involvement
  gang_cols = c('STUDY_FLAG', 'GANG_INV')
  gang_labels = c('GI', 'GIJD', 'GINA', 'M', 'R')
  
  crime_data = crime_data[crime_data$OFFENSE_DESCRIPTION %in% crimes[,1] | crime_data$CATEGORY %in% crimes[,1] |
                            crime_data$STUDY_FLAG %in% gang_labels | crime_data$GANG_INV %in% gang_labels,]
  
  crimes_matrix = matrix(0, nrow = nrow(full_spdf@data), ncol = (length(crimes[,1])))
  colnames(crimes_matrix) = c(crimes[,1])
  crime_by_tract = data.frame(GEOID = full_spdf@data$GEOID, crimes_matrix, stringsAsFactors = F) %>% arrange(GEOID)
  colnames(crime_by_tract) = c('GEOID', crimes[,1])
  # geoid_list = crime_by_tract$GEOID
  # subject_geoid = crime_data[crime_data$CATEGORY == 'AGG ASSAULT','geoid']
  
  
  geoid_list = crime_by_tract$GEOID
  for(n in 2:(ncol(crime_by_tract) - 1)){
    crime_by_tract[,n] = count_tract_appearance(geoid_list = geoid_list, 
                                                subject_geoid = crime_data[crime_data$CATEGORY == colnames(crime_by_tract)[n] | 
                                                                             crime_data$OFFENSE_DESCRIPTION == colnames(crime_by_tract)[n],'geoid'])[,2]
  }
  
  crime_by_tract[,ncol(crime_by_tract)] = count_tract_appearance(geoid_list = geoid_list,
                                                                 subject_geoid = crime_data[crime_data$STUDY_FLAG %in% gang_labels | crime_data$GANG_INV %in% gang_labels,'geoid'])[,2]
  
  colnames(crime_by_tract) = c('GEOID', crimes[,2])
  #Sanity check --> the above works, and quickly.
  #print(crime_data[crime_data$geoid == '06085502907',c('CATEGORY', 'OFFENSE_DESCRIPTION')] %>% arrange(CATEGORY, OFFENSE_DESCRIPTION))
  
  
  full_spdf@data = merge(full_spdf@data, crime_by_tract, by = 'GEOID')
  
  
  ############ SSCI Incident Data - incident_counts, full_spdf ##########
  
  school_bounds = readRDS('school_bounds_for_16-18_IRs.rds') %>% sp::spTransform(full_spdf@proj4string)
  ssci_incidents = ssci_incidents_full %>% dplyr::filter(level %in% INCLUDED_LEVELS) %>%
    select(date, school, level) %>%
    filter(date > START_DATE, date < END_DATE) %>% select(-date)
  
  if(nrow(ssci_incidents) > 1){
    
    ssci = stats::aggregate(ssci_incidents$level, by = list(ssci_incidents$school), FUN = sum)
    ssci_bounds = school_bounds[school_bounds$OBJECTID %in% ssci$Group.1,]
    
    intersect_matrix = matrix(data = 0, nrow = nrow(ssci_bounds@data), ncol = nrow(full_spdf@data))
    colnames(intersect_matrix) = full_spdf@data$GEOID
    rownames(intersect_matrix) = ssci_bounds@data$OBJECTID
    
    
    #Let's just attempt to identify the tracts that a single district is in.
    # one_dis = list(school_bounds@polygons[[1]]) %>% sp::SpatialPolygons(proj4string =  full_spdf@proj4string)
    for(school_ind in seq_along(ssci_bounds@polygons)){
      intersect_matrix[school_ind,which(rgeos::gIntersects(school_bounds[school_ind,], full_spdf, byid = TRUE))] = 1
    }
    #Now we have a matrix which marks every tract that is in every school boundary as 1.
    
    incident_counts = data.frame(GEOID = colnames(intersect_matrix), ssci_incidents = as.numeric(colSums(intersect_matrix)))
    
    full_spdf@data = merge(full_spdf@data, incident_counts, by = 'GEOID')
  }else{
    full_spdf@data$ssci_incidents = 0
  }
  
  
  ########### Neibor Matrix - loc_dist_matrix ###########
  # # 1/x_ij where x is number of blocks between block i and j (starting at 1), 0 if more than MAX_BLOCK_DIST away 
  # MAX_LOC_DIST = 1
  # loc_it = 1
  # 
  # #initializing the matrix
  # loc_dist_matrix = matrix(0, nrow = nrow(full_spdf@data), ncol = nrow(full_spdf@data))
  # loc_matrix = rgeos::gTouches(full_spdf, byid = TRUE)
  # 
  # 
  # 
  # #iterates through all blocks of 1 - MAX_BLOCK_DIST away, identifies which iteration it was picked up, and marks that number into matrix
  # #this will likely take hours, so we are going to run it overnight. Let's transfer this r code and the census df to our local machine.
  # for(loc_it_count in loc_it : ncol(loc_dist_matrix)){
  #   layer_locs = loc_it_count
  #   marked_locs = loc_it_count
  #   for(its in 1  : MAX_LOC_DIST){
  #     if(length(layer_locs) > 1){
  #       layer_locs_vec = which(rowSums(loc_matrix[,layer_locs])>0)
  #       
  #     }else{
  #       layer_locs_vec = which(loc_matrix[,layer_locs])
  #     }
  #     layer_locs_vec = layer_locs_vec[which(!(layer_locs_vec %in% marked_locs))]
  #     loc_dist_matrix[layer_locs_vec,loc_it_count] = its
  #     layer_locs = layer_locs_vec
  #     marked_locs = c(marked_locs, layer_locs)
  #   }
  #   # if(loc_it_count %% 50 == 0) print(loc_it_count)
  #   
  # }
  # # saveRDS(block_dist_matrix, 'block_dist_matrix.rds')
  # # saveRDS(block, 'current_block_it.rds')
  # # saveRDS(block_matrix, 'block_matrix_t_f.rds')
  # #checking to make sure the above works - it seems to work.
  # # plot(full_spdf[loc_dist_matrix[,200] > 0 & loc_dist_matrix[,200] < 3,])
  # 
  # colnames(loc_dist_matrix) = full_spdf@data$GEOID
  # rownames(loc_dist_matrix) = full_spdf@data$GEOID
  # 
  # 
  # # loc_dist_matrix = 1/loc_dist_matrix
  # 
  # # loc_dist_matrix[loc_dist_matrix > 1] = 0
  # 
  # saveRDS(loc_dist_matrix, 'Santa Clara County Tract 1 Neighbor Matrix.rds')
  
  ############# That's all the data. #############
  
  
  ############# Handling data operations and dropping certain data - need_metrics_spdf #############
  
  
  
  columns_to_convert_to_percentages_of_total_population = 
    c('unemployed', 'not_in_labor_force', 'below_poverty_line', 
      'no_diploma_18_24_male', 'no_diploma_18_24_female', 'under_18_pop', 'under_18_with_disability', 'single_mothers',
      'no_diploma_25_64', 'family_in_poverty', 'entered_2010_later', 'foreign_born', 'disability_under_18',
      'income_under_poverty_line', 'Poverty_6_and_younger', 'Poverty_6_11', 'Poverty_12_17',
      crimes[,2], 'ssci_incidents')
  
  #calculating ethnic diversity index (edi)
  race_cols = c('white', 'black', 'asian', 
                'hispanic', 'amiakn', 'hawaiipi', 'other_races', 'two_or_more_races')
  
  #calculating diversity index - 
  num_races = length(race_cols)
  total_race_pops = rowSums(full_spdf@data[,race_cols])
  race_fractions = full_spdf@data[,race_cols] / total_race_pops
  race_fractions_reduced = (race_fractions - (1/num_races))^2 #this works element-wise, so it works as we want it to.
  d_8 = sqrt(rowSums(race_fractions_reduced))
  c1 = 100
  c2 = -100*sqrt(num_races*(num_races-1))/(num_races - 1)
  edi = c1 + (c2*d_8)
  
  #works
  # print(edi[1:6])
  # print(full_spdf@data[1:6, race_cols])
  
  #Turning all of the census and crime data into percentages of total population
  full_spdf@data[,columns_to_convert_to_percentages_of_total_population] = 
    full_spdf@data[,columns_to_convert_to_percentages_of_total_population]/full_spdf@data$total_pop
  
  full_spdf@data$edi = as.numeric(scale(edi)) # we are scaling edi because it's the only one with such a clustering around the center
  
  #creating the "youth in poverty" variable
  full_spdf$youth_in_poverty = full_spdf$Poverty_6_and_younger + full_spdf$Poverty_6_11 + full_spdf$Poverty_12_17
  #creating a variable for all 18-24 yo without a high school diploma
  full_spdf$no_diploma_18_24 = full_spdf$no_diploma_18_24_female + full_spdf$no_diploma_18_24_male
  
  
  need_metrics_spdf = full_spdf
  need_metrics_spdf@data = need_metrics_spdf@data[,-which(colnames(need_metrics_spdf@data) %in% c('ALAND', 'overobese_teen',
                                                                                                  'overobese_child', 'OBJECTID',
                                                                                                  'tract', race_cols, 'Poverty_12_17',
                                                                                                  'Poverty_6_11', 'Poverty_6_and_younger',
                                                                                                  'no_diploma_18_24_male',
                                                                                                  'no_diploma_18_24_female',
                                                                                                  'under_18_pop',
                                                                                                  'sampled_pop', 'neibid'))]
  
  
  
  ########## Calculating Concentrated Economic Disadvantage ###########
  
  #relative poverty
  poverty_vec = loc_dist_matrix %*% need_metrics_spdf@data$below_poverty_line
  division_vec = loc_dist_matrix %*% rep(1, nrow(loc_dist_matrix))
  need_metrics_spdf@data$rel_poverty = need_metrics_spdf@data$below_poverty_line/(poverty_vec/division_vec)
  
  #relative unemployment
  unemployment_vec = loc_dist_matrix %*% need_metrics_spdf@data$unemployed
  need_metrics_spdf@data$rel_unemployment = need_metrics_spdf@data$unemployed/(unemployment_vec/division_vec)
  
  
  
  ############# Keeping only San Jose Census Tracts - need_metrics_spdf #########
  
  # sj = tigris::places('CA') %>% sp::spTransform(need_metrics_spdf@proj4string)
  # sj = sj[sj@data$NAME == 'San Jose',]
  
  # saveRDS(sj, 'sj_shapefile.rds')
  
  #identifying all of the tracts which intersect with or are within san jose
  # tract_city_matrix = matrix(data = 0, nrow = nrow(full_spdf@data), ncol = 1)
  # rownames(tract_city_matrix) = need_metrics_spdf@data$GEOID
  # colnames(tract_city_matrix) = seq_along(sj@polygons)
  
  need_metrics_spdf = need_metrics_spdf[which(gIntersects(sj, need_metrics_spdf, byid = TRUE)),]
  
  
  ############# FUNCTION Calculating percentiles for each metric ################
  get_percentile = function(vec){
    return(ecdf(vec)(vec))
  }
  
  
  ######### Min-Max Scaling all of the variables - need_metrics_spdf ##########
  
  #given a vector, min-max scales it.
  min_max_vec = function(vec, ...){
    if(max(vec, ...) == min(vec,...)){
      return(rep(0, length(vec)))
    }
    return((vec - min(vec, ...))/(max(vec,...)-min(vec,...)))
  }
  
  #minmax scaling all of the metrics
  for(n in 4 : ncol(need_metrics_spdf@data)){
    need_metrics_spdf@data[,n] = min_max_vec(need_metrics_spdf@data[,n])
  }
  
  
  
  
  
  ############ Setting up the metrics ##############
  
  #given the formatted dfs above for both the metric-specific weights and the class-specific weights, returns the accurate weight for each metric
  calculate_final_metric_weights = function(metrics_df, class_df){
    for(class_ind in seq_along(class_df[,1])){
      class_metric_weights = metrics_df[metrics_df[,3] == class_df[class_ind,1],2] #gets the weights for all metrics in the class
      revised_weights = (class_metric_weights/length(class_metric_weights)) * class_df[class_ind,2]
      metrics_df[metrics_df[,3] == class_df[class_ind,1],2] = revised_weights
    }
    return(metrics_df)
  }
  #given the full dataset of need metrics and the final metrics weight_df for an ES area, returns a df of the GEOID and its value for that ES
  get_ES_measurement = function(need_metric_df, weight_df, return_weight_scores = FALSE){
    included_metric_cols = need_metric_df[, which(colnames(need_metric_df) %in% weight_df[,1])]
    es_vec = rep(0, nrow(need_metric_df))
    
    class_scores = array(0, dim = c(nrow(need_metric_df), length(unique(weight_df[,3]))))
    colnames(class_scores) = unique(weight_df[,3])
    
    for(class in colnames(class_scores)){
      class_vars = included_metric_cols[weight_df[weight_df[,3] == class,1]]
      class_weights = weight_df[weight_df[,3] == class,2]
      for(n in seq_along(class_vars[1,])){
        class_vars[,n] = class_vars[,n] * class_weights[n]
      }
      class_scores[,class] = rowSums(class_vars)
    }
    es_vec = rowSums(class_scores)
    if(return_weight_scores){
      ret_arr = data.frame(GEOID = need_metric_df$GEOID, es_metric = min_max_vec(as.numeric(scale(es_vec))), class_scores)
    }else{
      ret_arr = data.frame(GEOID = need_metric_df$GEOID, es_metric = min_max_vec(as.numeric(scale(es_vec))))
    }
    return(ret_arr)
  }
  
  get_top_metrics = function(need_metric_df, weight_df, top_metrics_n = 3){
    included_metric_cols = need_metric_df[, which(colnames(need_metric_df) %in% weight_df[,1])]
    top_n_cols = array(NA, dim = c(nrow(need_metric_df), top_metrics_n*2))
    col_order = rep(c(1, (top_metrics_n+1)), top_metrics_n) + rep(c(0:(top_metrics_n - 1)), each = 2) #this orders the columns as metric name, metric val, metric name, metric val...
    colnames(top_n_cols) = c(paste0('metric_', seq_len(top_metrics_n)), 
                             paste0('value_metric_', seq_len(top_metrics_n)))#[col_order]
    for(row_ind in seq_along(included_metric_cols[,1])){
      row = included_metric_cols[row_ind,]
      row = row[order(-row)][seq_len(top_metrics_n)]
      top_n_cols[row_ind,] = c(names(row), as.numeric(row))
      
    }
    ret_top_n = data.frame(GEOID = need_metric_df$GEOID, top_n_cols, stringsAsFactors = FALSE)
    for(n in (top_metrics_n + 2 ): ncol(ret_top_n)){
      ret_top_n[,n] = as.numeric(ret_top_n[,n])
    }
    return(ret_top_n[,c(1, col_order + 1)])
  }
  
  
  # head(need_metrics_spdf@data, 2)
  
  #making the final metrics map
  fin_metrics_spdf = need_metrics_spdf
  fin_metrics_spdf@data = data.frame(GEOID = fin_metrics_spdf@data$GEOID)
  
  
  ##STREET OUTREACH, PERSONAL TRANSFORMATION, AND CASE MANAGEMENT
  {GANG_PRESENCE_METRICS = data.frame(matrix(
    data = c(
      'c_domestic_violence',                      1, 'child_maltreatment',
      'c_domestic_battery',                       1, 'child_maltreatment',
      'c_domestic_armed_robbery',                 1, 'child_maltreatment',
      'c_child_neglect',                          1, 'child_maltreatment',
      'c_child_abuse',                            1, 'child_maltreatment',
      'c_missing_juv',                            1, 'child_maltreatment',
      'c_child_molest',                           1, 'developmental_trauma',
      'c_intercourse_with_minor',                 1, 'developmental_trauma',#this is sex abuse with a youth
      'c_agg_assault',                            1, 'violent_prejudice_victimization',
      'c_robbery',                                1, 'violent_prejudice_victimization',
      'c_assault',                                1, 'violent_prejudice_victimization',
      'c_threats',                                1, 'violent_prejudice_victimization', #hate crimes and threats
      'c_graffiti',                               1, 'disorder_in_neighborhood',
      'c_dui',                                    1, 'disorder_in_neighborhood',
      'c_disturbance',                            1, 'disorder_in_neighborhood', #public nuisance
      'c_drunk_in_public',                        1, 'disorder_in_neighborhood',
      'c_narcotics',                              1, 'disorder_in_neighborhood',
      'c_weapons',                                1, 'presence_of_illegal_firearms',
      'c_armed_robbery',                          1, 'presence_of_illegal_firearms',
      'c_strong_armed_robbery',                   1, 'presence_of_illegal_firearms',
      'c_gang_crime',                             1, 'gang_affiliated_crime',
      'ssci_incidents',                           1, 'youth_school_conflicts',
      'disability_under_18',                      1, 'reported_disability',
      'family_in_poverty',                        1, 'economic_deprivation',
      'youth_in_poverty',                         1, 'economic_deprivation'#,
      #'edi',                                      1, 'social_discrimination'
    ),
    ncol = 3,
    byrow = TRUE
  ), stringsAsFactors = FALSE)
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    # length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = data.frame(
      matrix(
        data = c(
          'child_maltreatment',              1,
          'developmental_trauma',            1,
          'violent_prejudice_victimization', 1,
          'disorder_in_neighborhood',        1,
          'presence_of_firearms',            1,
          'youth_school_conflicts',          1,
          'reported_disability',             1,
          'economic_deprivation',            1, 
          'social_discrimination',           0.2
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', 'gang_presence', colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
  }
  ##VOCATIONAL TRAINING
  {GANG_PRESENCE_METRICS = data.frame(matrix(
    data = c(
      'unemployed',                 1, 'joblessness',
      'below_poverty_line',         1, 'poverty_in_community',
      'rel_unemployment',           1, 'concentrated_disadvantage',
      'rel_poverty',                1, 'concentrated_disadvantage',
      'no_diploma_18_24',           1, 'edu_completion',
      'c_auto_theft',               1, 'illegal_econ_activity',
      'c_grand_theft',              1, 'illegal_econ_activity',
      'c_burglary',                 1, 'illegal_econ_activity',
      'c_robbery',                  1, 'illegal_econ_activity',
      'c_narcotics',                1, 'illegal_econ_activity',
      'c_sex',                      1, 'illegal_econ_activity'
    ),
    ncol = 3,
    byrow = TRUE
  ), stringsAsFactors = FALSE)
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = data.frame(
      matrix(
        data = c(
          'joblessness',               1,
          'poverty_in_community',      1,
          'concentrated_disadvantage', 1,
          'edu_completion',            1,
          'illegal_econ_activity',     1
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', 'vocational_training', colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
    
  }
  ##PARENT AWARENESS
  {GANG_PRESENCE_METRICS = data.frame(matrix(
    data = c(
      'c_domestic_violence',      1, 'child_maltreatment',
      'c_domestic_battery',       1, 'child_maltreatment',
      'c_domestic_armed_robbery', 1, 'child_maltreatment',
      'c_child_neglect',          1, 'child_maltreatment',
      'c_child_abuse',            1, 'child_maltreatment',
      'c_missing_juv',            1, 'child_maltreatment',
      'single_mothers',           1, 'prop_single_mothers',
      'birth_teen',               1, 'teen_mothers',
      'family_in_poverty',        1, 'families_in_poverty',
      'youth_in_poverty',         1, 'families_in_poverty',
      'foreign_born',             1, 'families_unaware_of_community',
      'entered_2010_later',       1, 'families_unaware_of_community',
      'c_dui',                    1, 'drug_abuse',
      'c_drunk_in_public',        1, 'drug_abuse',
      'c_narcotics',              1, 'drug_abuse', 
      'alchohol_dens',            1, 'drug_abuse', 
      'tobacco_dens',             1, 'drug_abuse',
      'weed_dens',                1, 'drug_abuse'
    ),
    ncol = 3,
    byrow = TRUE
  ), stringsAsFactors = FALSE)
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    # length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = data.frame(
      matrix(
        data = c(
          'child_maltreatment',                1,
          'prop_single_mothers',               1,
          'teen_mothers',                      1,
          'families_in_poverty',               1,
          'families_unaware_of_community',     1,
          'drug_abuse',                        1
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', 'parent_awareness', colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
  }
  ##SUBSTANCE ABUSE
  {GANG_PRESENCE_METRICS = data.frame(matrix(
    data = c(
      'disability_under_18',      1, 'youth_with_disabilities',
      'c_minor_tobacco',          1, 'alc_drug_crimes',
      'c_minor_alcohol',          1, 'alc_drug_crimes',
      'c_dui',                    1, 'alc_drug_crimes',
      'c_drunk_in_public',        1, 'alc_drug_crimes',
      'c_narcotics',              5, 'alc_drug_crimes', 
      'alchohol_dens',            1, 'alc_weed_tob_retailers', 
      'tobacco_dens',             1, 'alc_weed_tob_retailers',
      'weed_dens',                1, 'alc_weed_tob_retailers'
    ),
    ncol = 3,
    byrow = TRUE
  ), stringsAsFactors = FALSE)
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    # length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = data.frame(
      matrix(
        data = c(
          'youth_with_disabilities',       1,
          'alc_drug_crimes',               1,
          'alc_weed_tob_retailers',        1
        ),
        ncol = 2,
        byrow = TRUE
      ), stringsAsFactors = FALSE
    )
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', 'substance_abuse', colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
    
  }
  ##merging Substance abuse with STREET OUTREACH, PERSONAL TRANSFORMATION, AND CASE MANAGEMENT
  {
    fin_metrics_spdf@data$gp_and_sub = min_max_vec(SUBSTANCE_ABUSE_WEIGHT*fin_metrics_spdf@data$substance_abuse + 
                                                     (1-SUBSTANCE_ABUSE_WEIGHT)*fin_metrics_spdf@data$gang_presence)
    #you just add substance abuse as a percentile to the gang presence stuff
  }
  
  
  ###### Removing duplicate columns from the fin_metrics_spdf - fin_metrics_spdf ###############
  
  colnames(fin_metrics_spdf@data) = gsub('\\.x', '', colnames(fin_metrics_spdf@data))
  fin_metrics_spdf@data = fin_metrics_spdf@data[,grep('\\.', colnames(fin_metrics_spdf@data), invert = TRUE)]
  
  
  ############ appending to dep_dat - dep_dat ############
  
  if(is.null(dep_dat)){
    dep_dat = data.frame(fin_metrics_spdf@data, start_date = START_DATE, end_date = END_DATE, stringsAsFactors = FALSE)
  }else{
    dep_dat = rbind(dep_dat, data.frame(fin_metrics_spdf@data, start_date = START_DATE, end_date = END_DATE, stringsAsFactors = FALSE))
  }
  
  ######## Printing out the progress ##########
  if(week_ind %% 10 == 0){
    print(paste0(week_ind, ' done out of ', length(start_week_range)))
  }
  
}
dep_dat_backup = dep_dat

############## removing the percentile variables, and changing GEOID to a character var ##############

dep_dat = dep_dat_backup
dep_dat = dep_dat[,-grep('_percentile', colnames(dep_dat))]
dep_dat$GEOID = as.character(dep_dat$GEOID)

######### Setting up prediction for gp_sub - gp_sub_dat ###########
if(identical(dep_dat$GEOID, big_dat$GEOID) & #GEOIDs are in the same order
identical((dep_dat$start_date - DAYS_FORWARD), big_dat$start_date)){ #you can actually just cbind them.
  gp_sub_dat = data.frame(big_dat[,-which(colnames(big_dat) %in% c('GEOID', 'start_date', 'end_date'))], dep_gp_sub = dep_dat$gp_and_sub)
}else{message('dep_dat and big_dat did not match up for some reason. Check yourself')}

########### Conducting a random forest prediction for gp_sub ############

install_and_load('randomForest')
install_and_load('tree')
set.seed(95116)
train = sample(1:nrow(gp_sub_dat), nrow(gp_sub_dat)/2)
# tree.gp_sub = tree(dep_gp_sub ~ ., data = gp_sub_dat, subset = train)
# cv.gp_sub = cv.tree(tree.gp_sub)
# plot(cv.gp_sub$size, cv.gp_sub$dev, type = 'b')

gp_sub.rf = randomForest(dep_gp_sub ~ ., data = gp_sub_dat, subset = train, mtry = ceiling(sqrt(ncol(gp_sub_dat))))
yhat.rf = predict(gp_sub.rf, newdata = gp_sub_dat[-train,])
y.test = gp_sub_dat[-train,]$dep_gp_sub
print(mean(abs(yhat.rf - y.test), na.rm = T)) #we are hitting an average accuracy within a %age point
print(mean((yhat.rf - y.test)^2, na.rm = T))

importance(gp_sub.rf)
varImpPlot(gp_sub.rf)

#saving model
saveRDS(gp_sub.rf, 'gp_sub_1_year_rf.rds')



######### Setting up prediction for vocational_training - vocational_dat ###########
if(identical(dep_dat$GEOID, big_dat$GEOID) & #GEOIDs are in the same order
   identical((dep_dat$start_date - DAYS_FORWARD), big_dat$start_date)){ #you can actually just cbind them.
  vocational_dat = data.frame(big_dat[,-which(colnames(big_dat) %in% c('GEOID', 'start_date', 'end_date'))], dep_vocational = dep_dat$vocational_training)
}else{message('dep_dat and big_dat did not match up for some reason. Check yourself')}

########### Conducting a random forest prediction for vocational ############

install_and_load('randomForest')
install_and_load('tree')
set.seed(95116)
train = sample(1:nrow(vocational_dat), nrow(vocational_dat)/2)
# tree.vocational = tree(dep_vocational ~ ., data = vocational_dat, subset = train)
# cv.vocational = cv.tree(tree.vocational)
# plot(cv.vocational$size, cv.vocational$dev, type = 'b')

vocational.rf = randomForest(dep_vocational ~ ., data = vocational_dat, subset = train, mtry = ceiling(sqrt(ncol(vocational_dat))))
yhat.rf = predict(vocational.rf, newdata = vocational_dat[-train,])
y.test = vocational_dat[-train,]$dep_vocational
print(mean(abs(yhat.rf - y.test), na.rm = T)) #we are hitting an average accuracy within  0.1%age points
print(mean((yhat.rf - y.test)^2, na.rm = T))

importance(vocational.rf)
varImpPlot(vocational.rf)

#saving model
saveRDS(vocational.rf, 'vocational_1_year_rf.rds')

######### Setting up prediction for parent_training - parent_dat ###########
if(identical(dep_dat$GEOID, big_dat$GEOID) & #GEOIDs are in the same order
   identical((dep_dat$start_date - DAYS_FORWARD), big_dat$start_date)){ #you can actually just cbind them.
  parent_dat = data.frame(big_dat[,-which(colnames(big_dat) %in% c('GEOID', 'start_date', 'end_date'))], dep_parent = dep_dat$parent_awareness)
}else{message('dep_dat and big_dat did not match up for some reason. Check yourself')}

########### Conducting a random forest prediction for parent ############

install_and_load('randomForest')
install_and_load('tree')
set.seed(95116)
train = sample(1:nrow(parent_dat), nrow(parent_dat)/2)
# tree.parent = tree(dep_parent ~ ., data = parent_dat, subset = train)
# cv.parent = cv.tree(tree.parent)
# plot(cv.parent$size, cv.parent$dev, type = 'b')

parent.rf = randomForest(dep_parent ~ ., data = parent_dat, subset = train, mtry = ceiling(sqrt(ncol(parent_dat))))
yhat.rf = predict(parent.rf, newdata = parent_dat[-train,])
y.test = parent_dat[-train,]$dep_parent
print(mean(abs(yhat.rf - y.test), na.rm = T)) #we are hitting an average accuracy within 0.3%age points
print(mean((yhat.rf - y.test)^2, na.rm = T))

importance(parent.rf)
varImpPlot(parent.rf)

#saving model
saveRDS(parent.rf, 'parent_1_year_rf.rds')




