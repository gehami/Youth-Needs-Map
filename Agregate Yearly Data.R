rm(list=ls()) #clearing environment

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
END_DATE_FULL = as.Date('2018-01-01')
REAL_END_DATE = as.Date('2019-01-01')
ACS_START_YEAR = 2013
ACS_END_YEAR = 2017
TOP_METRICS = 3 #number of top metrics to highlight for each tract
SUBSTANCE_ABUSE_WEIGHT = 0.15 #when combining gang presence and substance abuse, the weighting of substnace abuse



SQUARE_METER_TO_SQUARE_MILE = 3.86102E-7
INCLUDED_LEVELS = c(1)
ACS_YEAR = 2016

start_year_range = seq.Date(START_DATE_FULL, (END_DATE_FULL-365), by = 'year')
end_year_range = start_year_range + 365
start_year_range = c(start_year_range, END_DATE_FULL)
end_year_range = c(end_year_range, REAL_END_DATE)


######## Reading Constant Data #############
#importing the crime data
crime_data_full = readRDS('RDS files/Crimes only by Census Tract.rds')

ssci_incidents_full = readRDS('RDS files/SSCI_IR_ma_16-18_for_plotting.rds') 

sj = readRDS('RDS files/sj_shapefile.rds')

school_bounds = readRDS('RDS files/school_bounds_updated.rds')

grad_rates_spdf = readRDS('RDS files/graduation rates spdf.rds')

loc_dist_matrix = readRDS('RDS files/Santa Clara County Tract 1 Neighbor Matrix.rds')

full_spdf_backup = readRDS('RDS files/Metrics without time dependency.rds')

neib_names_data = readRDS('RDS files/neighborhood names by census tract GEOID.rds')

graf_backup = readRDS('RDS files/Full Graffiti Data.rds')

######### Reading in All census Data - acs_list ############
install_and_load('tidyverse')
install_and_load('tidycensus')


acs_vars =
{data.frame(
  acs_code = c(#Vocational Training
    'B00001_001', #Population sampled
    'B01001_001', #Total Population
    'B27011_014', #Unemployed
    'B10058_007', #Not in Labor Force
    'B06012_002', #Below 100% of Poverty Line
    'B15001_005', 'B15001_046', #Male and Female 18-24 Less than a high school graduate. Add Together
    #Substance Abuse
    'B18135_002', #population under 18
    #Parent Awareness
    'B11001_006', #Single mothers
    'B27019_003', #25-64 without high school graduate
    'B17017_003', #Family households under poverty line
    'B05005_002', #Entered US 2010 or Later
    'B99051_005', #Foreign born
    #Gang Presence
    'B18135_003', #Under 18 with a disability
    'B10059_002', #income under poverty line
    'B17020_003', 'B17020_004', 'B17020_005', #youth below poverty line: 6 and younger, 6-11, and 12-17
    'B01001H_001', 'B01001B_001', 'B01001D_001', 'B01001I_001', 'B01001C_001', 'B01001E_001', 'B01001F_001', 'B01001G_001' #white, black, asian, hispanic, amiakn, hawaiipi, other, two or more
  ),
  var_name = c(#Vocational Training
    'sampled_pop',
    'total_pop',
    'unemployed',
    'not_in_labor_force',
    'below_poverty_line',
    'no_diploma_18_24_male', 'no_diploma_18_24_female',
    #Substance Abuse
    'under_18_pop',
    #Parent Awareness
    'single_mothers',
    'no_diploma_25_64',
    'family_in_poverty',
    'entered_2010_later',
    'foreign_born',
    #Gang Presence
    'disability_under_18',
    'income_under_poverty_line',
    'Poverty_6_and_younger', 'Poverty_6_11', 'Poverty_12_17',
    'white', 'black', 'asian', 'hispanic', 'amiakn', 'hawaiipi', 'other_races', 'two_or_more_races'
  ), stringsAsFactors = F
)}
#if this is the first time running this on the computer
# acs_list = list()
# i = 1
# for(year in ACS_START_YEAR : ACS_END_YEAR){
#   acs_dat <- get_acs(geography = 'tract',
#                      variables = c(acs_vars[1,1]),
#                      state = '006', #state fips code for CA
#                      county = "085", #county fips code for santa clara
#                      geometry = FALSE, year = year)
#   colnames(acs_dat)[colnames(acs_dat) == 'estimate'] = acs_vars[1,2]
#   for(var_i in 2:nrow(acs_vars)){
#     var_name = acs_vars[var_i,1]
#     new_var_table = get_acs(geography = 'tract',
#                             variables = var_name,
#                             state = '006',
#                             county = '085',
#                             geometry = FALSE, year = year)
#     new_var_column = new_var_table$estimate
#     acs_dat = cbind(acs_dat, new_var = new_var_column)
#     colnames(acs_dat)[colnames(acs_dat) == 'new_var'] = acs_vars[var_i, 2]
#   } #individually adding each ACS variable since the get_acs function only seems to be able to handle one variable at a time.
#   acs_dat$variable = NULL
#   acs_dat$moe = NULL
#   acs_list[[i]] = list(year, acs_dat)
#   i = i + 1
# }

# saveRDS(acs_list, 'RDS files/acs_list_2013_2017.rds')
acs_list = readRDS('RDS files/acs_list_2013_2017.rds')


########
#All the data manipulation below takes around 4 seconds
########
big_list = list() #the metrics for the models
full_list = list() #raw data


########### Setting up the year and date for each cycle ###########

for(year_ind in seq_along(start_year_range)){
  full_spdf = full_spdf_backup
  
  START_DATE = start_year_range[year_ind]
  END_DATE = end_year_range[year_ind]
  START_TIME = as.POSIXct(as.character(START_DATE))
  END_TIME = as.POSIXct(as.character(END_DATE))
  YEAR_NUM = min(as.numeric(gsub('([0-9]{4})([[:print:]]+)', '\\1', as.character(START_DATE))), ACS_END_YEAR)
  

  ########### replacing acs data in the full_spdf with the acs data that it should be - full_spdf ############
  install_and_load('purrr')
  replace_acs = acs_list[[which(unlist(purrr::map(acs_list,1)) == YEAR_NUM)]][[2]]
  
  full_spdf@data[,colnames(replace_acs)] = replace_acs
  
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
  
  crime_by_tract$c_total_crime = count_tract_appearance(geoid_list = geoid_list,
                                                        subject_geoid = crime_data[,'geoid'])[,2]
  
  
  colnames(crime_by_tract) = c('GEOID', c(crimes[,2], 'c_total_crime'))
  #Sanity check --> the above works, and quickly.
  #print(crime_data[crime_data$geoid == '06085502907',c('CATEGORY', 'OFFENSE_DESCRIPTION')] %>% arrange(CATEGORY, OFFENSE_DESCRIPTION))
  
  
  full_spdf@data = merge(full_spdf@data, crime_by_tract, by = 'GEOID')
  
  
  ############ SSCI Incident Data - incident_counts, full_spdf ##########
  school_bounds = tryCatch(school_bounds, error = function(e){readRDS('RDS files/school_bounds_updated.rds') %>% sp::spTransform(full_spdf@proj4string)})
  # school_bounds = readRDS('RDS files/school_bounds_updated.rds') %>% sp::spTransform(full_spdf@proj4string)
  ssci_incidents = ssci_incidents_full %>% dplyr::filter(level %in% INCLUDED_LEVELS) %>%
    select(date, school, level) %>%
    filter(date > START_DATE, date < END_DATE) %>% select(-date)
  
  if(nrow(ssci_incidents) > 1){
    #sums up the number of incidents by school for the given year and chosen levels
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
    #We need to conduct some matrix multiplication now to get the total number of incidents that occur at the schools which boundaries are within each tract
    
    ssci_incident_tract_count = t(t(matrix(as.numeric(ssci[,2]))) %*% intersect_matrix)

    incident_counts = data.frame(GEOID = colnames(intersect_matrix), ssci_incidents = as.numeric(ssci_incident_tract_count))
    
    full_spdf@data = merge(full_spdf@data, incident_counts, by = 'GEOID')
  }else{
    full_spdf@data$ssci_incidents = 0
  }
  
  ############ School Graduation Rates Data - full_spdf and grad_count_df ###############
  
  install_and_load('lubridate')
  grad_rates_spdf_cut = grad_rates_spdf[grad_rates_spdf@data$date == (START_DATE - months(12)),]
  grad_rates = grad_rates_spdf_cut@data 
  
  intersect_matrix = matrix(data = 0, nrow = nrow(grad_rates), ncol = nrow(full_spdf@data))
  
  colnames(intersect_matrix) = full_spdf@data$GEOID
  rownames(intersect_matrix) = grad_rates_spdf_cut@data$High.School
  
  for(school_ind in seq_along(grad_rates_spdf_cut@polygons)){
    intersect_matrix[school_ind,which(rgeos::gIntersects(grad_rates_spdf_cut[school_ind,], full_spdf, byid = TRUE))] = 1
  }
  
  grad_count_matrix = grad_rates[,-c(1,2,c(-2,-1,0)+ncol(grad_rates))] %>% sapply(as.numeric)
  
  colnames(grad_count_matrix) = gsub('(\\.)+', '_', colnames(grad_count_matrix))
  
  grad_count_matrix = as.matrix(grad_count_matrix)
  
  grad_count_df = data.frame(t(t(grad_count_matrix) %*% intersect_matrix)) %>% dplyr::mutate(grad_rate = total_graduated/total_enrolled)
  
  grad_count_df$grad_rate[is.na(grad_count_df$grad_rate)] = 1 #decided to call the grad rates of tracts with no enrolled youth as 1. this avoids NA issues.
  
  grad_count_df$GEOID = colnames(intersect_matrix)
  
  grad_count_df$no_grad_rate = 1-grad_count_df$grad_rate
  
  
  
  full_spdf@data = merge(full_spdf@data, grad_count_df, by = 'GEOID')

  ############ Calculating number of graffiti incidents and sqft of graffiti per tract - graf_count and graf_area and full_spdf ########
  
  graf = graf_backup[graf_backup$date < END_DATE & graf_backup$date >= START_DATE,]
  
  
  #count raw number of graffiti incidents
  graf_count = count_tract_appearance(full_spdf@data$GEOID, graf@data$GEOID)#works
  colnames(graf_count) = c('GEOID', 'graf_incidents')
  
  #sum up overall graffiti area by tract
  require(stats)
  graf$sqft[is.na(graf$sqft)] = 0
  graf_agg = data.frame(sqft = graf$sqft, GEOID = graf$GEOID)
  graf_agg = rbind(graf_agg, data.frame(sqft = 0, GEOID = unique(full_spdf$GEOID)))
  graf_area = aggregate(graf_agg$sqft, by = list(GEOID = graf_agg$GEOID), FUN = sum)
  colnames(graf_area) = c('GEOID', 'graf_area')
  
  #merging in data 
  full_spdf = merge(full_spdf, merge(graf_count, graf_area, by = "GEOID"), by = "GEOID")


  ############# That's all the data. #############
  
  
  ############# Handling data operations and dropping certain data - need_metrics_spdf #############
  
  
  
  columns_to_convert_to_percentages_of_total_population = 
    c('unemployed', 'not_in_labor_force', 'below_poverty_line', 
      'no_diploma_18_24_male', 'no_diploma_18_24_female', 'under_18_pop', 'single_mothers',
      'no_diploma_25_64', 'family_in_poverty', 'entered_2010_later', 'foreign_born', 'disability_under_18',
      'income_under_poverty_line', 'Poverty_6_and_younger', 'Poverty_6_11', 'Poverty_12_17',
      colnames(crime_by_tract)[-1], 'ssci_incidents', 'graf_incidents')
  
  #calculating ethnic diversity index (edi)
  race_cols = c('white', 'black', 'asian', 
                'hispanic', 'amiakn', 'hawaiipi', 'other_races', 'two_or_more_races')
  
  columns_to_convert_to_percentage_of_tract_area = c('graf_area')
  
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
  
  #turning applicable columns into a fraction of the total area of tract
    #getting area of each tract
    tract_area = rep(0, length(full_spdf@polygons))
    for(n in 1 : length(tract_area)){tract_area[n] = full_spdf@polygons[[n]]@Polygons[[1]]@area}
  #calculating area ratios.
  full_spdf@data[,columns_to_convert_to_percentage_of_tract_area] = 
    full_spdf@data[,columns_to_convert_to_percentage_of_tract_area]/(tract_area*1000)
  
  
    
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
  
  # saveRDS(sj, 'RDS files/sj_shapefile.rds')
  
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
  
  #useless
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
  
  #pulls the metrics from a single file to keep everything consistent
  source('Metric Definitions.R')
  all_metrics = get_all_metrics()
  
  ##STREET OUTREACH, PERSONAL TRANSFORMATION, AND CASE MANAGEMENT
  {GANG_PRESENCE_METRICS = all_metrics[[1]][[2]]
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    # length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = all_metrics[[1]][[3]]
      
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', all_metrics[[1]][[1]], colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
  }
  ##VOCATIONAL TRAINING
  {GANG_PRESENCE_METRICS = all_metrics[[2]][[2]]
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = all_metrics[[2]][[3]]
      
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', all_metrics[[2]][[1]], colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
    
  }
  ##PARENT AWARENESS
  {GANG_PRESENCE_METRICS = all_metrics[[3]][[2]]
    
    colnames(GANG_PRESENCE_METRICS) = c('metric', 'weight', 'class')
    GANG_PRESENCE_METRICS$weight = as.numeric(GANG_PRESENCE_METRICS$weight)
    # length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good
    
    #weights for classes
    GANG_PRESENCE_CLASS_WEIGHTS = all_metrics[[3]][[3]]
    
    colnames(GANG_PRESENCE_CLASS_WEIGHTS) = c('class', 'class_weight')
    GANG_PRESENCE_CLASS_WEIGHTS$class_weight = as.numeric(GANG_PRESENCE_CLASS_WEIGHTS$class_weight)
    
    final_metrics = calculate_final_metric_weights(metrics_df = GANG_PRESENCE_METRICS, class_df = GANG_PRESENCE_CLASS_WEIGHTS)
    
    es_measurement = get_ES_measurement(need_metric_df = need_metrics_spdf@data, weight_df = final_metrics, return_weight_scores = TRUE)
    colnames(es_measurement) = c('GEOID', all_metrics[[3]][[1]], colnames(es_measurement)[3:ncol(es_measurement)])
    
    es_percentile = es_measurement
    for(col_ind in 2:ncol(es_percentile)){
      es_percentile[,col_ind] = get_percentile(es_measurement[,col_ind])
    }
    colnames(es_percentile)[2:ncol(es_percentile)] = paste0(colnames(es_percentile)[2:ncol(es_percentile)], '_percentile')
    
    
    fin_metrics_spdf@data = merge(fin_metrics_spdf@data, merge(es_measurement, es_percentile, by = 'GEOID'), by = 'GEOID')
    
  }
  ##merging Substance abuse with STREET OUTREACH, PERSONAL TRANSFORMATION, AND CASE MANAGEMENT
  {
    fin_metrics_spdf@data$gp_and_sub = fin_metrics_spdf@data$gang_presence
    #you just add substance abuse as a percentile to the gang presence stuff
  }
  
  
  ###### Removing duplicate columns from the fin_metrics_spdf - fin_metrics_spdf ###############
  
  colnames(fin_metrics_spdf@data) = gsub('\\.x', '', colnames(fin_metrics_spdf@data))
  fin_metrics_spdf@data = fin_metrics_spdf@data[,grep('\\.', colnames(fin_metrics_spdf@data), invert = TRUE)]
  
  ####### Adding in the neighborhood names for each GEOID - fin_metrics_spdf #########
  
  fin_metrics_spdf@data = merge(neib_names_data, fin_metrics_spdf@data, by = 'GEOID')
  
  ######### Set any percentile Variable where it's corresponding min-max var is 0 to 0 #############
  perc_cols = grep('percentile', colnames(fin_metrics_spdf@data), value = TRUE)
  for(col_name in perc_cols){
    min_max_col = fin_metrics_spdf@data[,gsub('_percentile', '', col_name)]
    fin_metrics_spdf@data[min_max_col == 0,col_name] = 0
  }
  
  ############ appending to big_list - big_list ############
  
  big_list[[year_ind]] = list(c(START_DATE, END_DATE), fin_metrics_spdf)
  
  
  ######### appending to full_list - full_list ###########
  
  full_list[[year_ind]] = list(c(START_DATE, END_DATE), full_spdf)
  
  ######## Printing out the progress ##########
  if(year_ind %% 1 == 0){
    print(paste0(year_ind, ' done out of ', length(start_year_range)))
  }

}





########### Editing race cols to be percentages in full_list #########

fin_full_list = full_list

for(n in seq_along(full_list)){
  fin_full_list[[n]][[2]]@data[,race_cols] = full_list[[n]][[2]]@data[,race_cols] / full_list[[n]][[2]]@data$total_pop #getting %age of pop for each race
  fin_full_list[[n]][[2]]@data[,grep('^c_', colnames(full_list[[n]][[2]]@data))] = full_list[[n]][[2]]@data[,grep('^c_', colnames(full_list[[n]][[2]]@data))] * 1000 #crime per 1000 residents
  #when you grab the population for 18-24 kids. This line below will be good. Make sure to name the var .$pop_18_24
  # fin_full_list[[n]][[2]]@data$no_diploma_18_24 = fin_full_list[[n]][[2]]@data$no_diploma_18_24 * (fin_full_list[[n]][[2]]@data$total_pop / fin_full_list[[n]][[2]]@data$pop_18_24)  
}


######### Saving big_list, and one fin_metrics_spdf, need_metrics_spdf, and top_metrics #########

saveRDS(big_list, 'RDS files/yearly_need_metric_spdf.rds')
saveRDS(big_list, 'Final_app_deliverable/yearly_need_metric_spdf.rds')
saveRDS(fin_full_list, 'RDS files/yearly_raw_metric_spdf.rds')
saveRDS(fin_full_list, 'Final_app_deliverable/yearly_raw_metric_spdf.rds')





