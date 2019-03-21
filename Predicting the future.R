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


install_and_load('sp')
install_and_load('rgeos')
install_and_load('randomForest')
install_and_load('tree')

########### Globally-used functions ############
#given the list of possible GEOIDs, and the list of geoids of the column of interest, returns a 2-column dataframe 
#of the counts of how many times each geoid appears (including zero times)
count_tract_appearance = function(geoid_list, subject_geoid){
  return(data.frame(table(c(subject_geoid, geoid_list)) - 1, stringsAsFactors = F) %>% arrange(Var1))
}


########### CONSTANTS #########

START_DATE_FULL = as.Date('2017-06-25')
END_DATE_FULL = as.Date('2018-06-25')
SUBSTANCE_ABUSE_WEIGHT = 0.15 #when combining gang presence and substance abuse, the weighting of substnace abuse

source('Metric Definitions.R')
all_metrics = get_all_metrics()


dep_vars = c(all_metrics[[1]][[1]][1], all_metrics[[3]][[1]][1], all_metrics[[2]][[1]][1])

years_forward = c(1,2,3)
DAYS_IN_YEAR = 365
days_forward = years_forward*DAYS_IN_YEAR


SQUARE_METER_TO_SQUARE_MILE = 3.86102E-7
INCLUDED_LEVELS = c(1)
ACS_YEAR = 2016


######## Reading Constant Data #############
#importing the crime data
crime_data_full = readRDS('RDS files/Crimes only by Census Tract.rds')

ssci_incidents_full = readRDS('RDS files/SSCI_IR_ma_16-18_for_plotting.rds') 

sj = readRDS('RDS files/sj_shapefile.rds')

school_bounds = readRDS('RDS files/school_bounds_updated.rds')

grad_rates_spdf = readRDS('RDS files/graduation rates spdf.rds')

loc_dist_matrix = readRDS('RDS files/Santa Clara County Tract 1 Neighbor Matrix.rds')

full_spdf_backup = readRDS('RDS files/Metrics without time dependency.rds')

neib_names_data = readRDS('RDS files/neighborhood names by census tract GEOID.rds') #from 'Agregate Yearly Data.R'

graf_backup = readRDS('RDS files/Full Graffiti Data.rds')


######### Making Latest Data ##############
########
big_dat = NULL


########### Setting up the year and date for each cycle ###########

full_spdf = full_spdf_backup

START_DATE = START_DATE_FULL
END_DATE = END_DATE_FULL
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

school_bounds = school_bounds %>% sp::spTransform(full_spdf@proj4string)
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
  #We need to conduct some matrix multiplication now to get the total number of incidents that occur at the schools which boundaries are within each tract
  
  ssci_incident_tract_count = t(t(matrix(as.numeric(ssci[,2]))) %*% intersect_matrix)
  
  incident_counts = data.frame(GEOID = colnames(intersect_matrix), ssci_incidents = as.numeric(ssci_incident_tract_count))
  
  full_spdf@data = merge(full_spdf@data, incident_counts, by = 'GEOID')
}else{
  full_spdf@data$ssci_incidents = 0
}



############ School Graduation Rates Data - full_spdf and grad_count_df ###############

install_and_load('lubridate')
grad_rates_spdf_cut = grad_rates_spdf[grad_rates_spdf@data$date == floor_date((END_DATE - months(12)), unit = 'year'),]
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
# # saveRDS(block_dist_matrix, 'RDS files/block_dist_matrix.rds')
# # saveRDS(block, 'RDS files/current_block_it.rds')
# # saveRDS(block_matrix, 'RDS files/block_matrix_t_f.rds')
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
# saveRDS(loc_dist_matrix, 'RDS files/Santa Clara County Tract 1 Neighbor Matrix.rds')

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
    crimes[,2], 'ssci_incidents', 'graf_incidents')

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


big_dat = fin_metrics_spdf@data


big_dat_backup = big_dat

############## removing the percentile variables, and changing GEOID to a character var ##############

big_dat = big_dat_backup
big_dat = big_dat[,grep('_percentile', colnames(big_dat), invert = TRUE)]
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

########## Formatting the data to only have the x vars - x_dat########
# head(big_dat)
x_dat = big_dat[,-which(colnames(big_dat) %in% c('GEOID', 'start_date', 'end_date'))]


####### Reading in the models #########
startwd = getwd()
setwd('models')
parent_1y.rf = readRDS('model_rf_parent_awareness_365_days_forward.rds')
parent_2y.rf = readRDS('model_rf_parent_awareness_730_days_forward.rds')
parent_3y.rf = readRDS('model_rf_parent_awareness_1095_days_forward.rds')
gp_sub_1y.rf = readRDS('model_rf_GANG_PRESENCE_365_days_forward.rds')
gp_sub_2y.rf = readRDS('model_rf_GANG_PRESENCE_730_days_forward.rds')
gp_sub_3y.rf = readRDS('model_rf_GANG_PRESENCE_1095_days_forward.rds')
vocational_1y.rf = readRDS('model_rf_vocational_training_365_days_forward.rds')
vocational_2y.rf = readRDS('model_rf_vocational_training_730_days_forward.rds')
vocational_3y.rf = readRDS('model_rf_vocational_training_1095_days_forward.rds')
setwd(startwd)

######## Setting up predictive spdf - pred_spdf ###########

pred_spdf = fin_metrics_spdf
pred_spdf@data = data.frame(GEOID = fin_metrics_spdf@data$GEOID)


########## Predicting future #############

#Parent Awareness (ES 4 as of 2019-2022 Eligible Service Areas)
parent_pred_1y = predict(parent_1y.rf, newdata = x_dat)
parent_pred_2y = predict(parent_2y.rf, newdata = x_dat)
parent_pred_3y = predict(parent_3y.rf, newdata = x_dat)

#Gang Presence with Substance Abuse (ES 1, 2, and 5 as of 2019-2022 Eligible Service Areas)
gp_sub_pred_1y = predict(gp_sub_1y.rf, newdata = x_dat)
gp_sub_pred_2y = predict(gp_sub_2y.rf, newdata = x_dat)
gp_sub_pred_3y = predict(gp_sub_3y.rf, newdata = x_dat)

#Vocational Training (ES 3 as of 2019-2022 Eligible Service Areas)
vocational_pred_1y = predict(vocational_1y.rf, newdata = x_dat)
vocational_pred_2y = predict(vocational_2y.rf, newdata = x_dat)
vocational_pred_3y = predict(vocational_3y.rf, newdata = x_dat)

########### Merging all predictions into pred_spdf - pred_spdf ###########

pred_spdf@data = data.frame(pred_spdf@data, 
                            parent_2019 = parent_pred_1y, parent_2020 = parent_pred_2y, parent_2021 = parent_pred_3y,
                            gp_sub_2019 = gp_sub_pred_1y, gp_sub_2020 = gp_sub_pred_2y, gp_sub_2021 = gp_sub_pred_3y,
                            vocational_2019 = vocational_pred_1y, vocational_2020 = vocational_pred_2y, 
                            vocational_2021 = vocational_pred_3y)



######### Min-maxing all of the columns in the pred_spdf - pred_spdf ########

for(n in 2:ncol(pred_spdf@data)){pred_spdf@data[,n] = min_max_vec(pred_spdf@data[,n])}


############ Merging the neighbhoorhood name into pred_spdf - pred_spdf ##########

pred_spdf@data = merge(neib_names_data, pred_spdf@data, by = 'GEOID')



####### Saving pred_spdf - 'predicted_metrics_for_2019-2021_rfq.rds' #########

saveRDS(pred_spdf, 'RDS files/predicted_metrics_for_2019-2021_rfq.rds')
