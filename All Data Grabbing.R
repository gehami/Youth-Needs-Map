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
install_and_load('magrittr')

####### Setting api key #########

api_key = 'db0482639afc79ec7ab4dc58dedc3b83824a9a1c'
tryCatch(census_api_key(api_key, install = T), error = function(e){print("it's already there")})
readRenviron("~/.Renviron")

########## Cache options ############

options(tigris_use_cache = TRUE)
CACHE_OPTION = TRUE

########### Globally-used functions ############
#given the list of possible GEOIDs, and the list of geoids of the column of interest, returns a 2-column dataframe 
#of the counts of how many times each geoid appears (including zero times)
count_tract_appearance = function(geoid_list, subject_geoid){
  return(data.frame(table(c(subject_geoid, geoid_list)) - 1, stringsAsFactors = F) %>% arrange(Var1))
}


########### CONSTANTS #########

START_DATE = as.POSIXct('2017-01-01 00:00:00')
END_DATE = as.POSIXct('2018-01-01 00:00:00')
SQUARE_METER_TO_SQUARE_MILE = 3.86102E-7
INCLUDED_LEVELS = c(1)
ACS_YEAR = 2017


######## Reading Constant Data #############
#importing the crime data
crime_data_full = readRDS('RDS files/Crimes only by Census Tract.rds')

ssci_incidents_full = readRDS('RDS files/SSCI_IR_ma_16-18_for_plotting.rds') 

sj = readRDS('RDS files/sj_shapefile.rds')

# acs_spdf = readRDS('RDS files/2016 ACS spdf.rds')

########## ACS DATA - acs_spdf ###########

install_and_load('tidycensus')

# acs_acs5_vars = load_variables(ACS_YEAR, 'acs5', cache = CACHE_OPTION)
# View(acs_acs5_vars)

acs_vars = data.frame(
  acs_code = c(#Vocational Training
               'B00001_001', #Population sampled
               'B01001_001', #Total Population
               'B27011_014', #Unemployed
               'B10058_007', #Not in Labor Force
               'B06012_002', #Below 100% of Poverty Line
               'B15001_005', 'B15001_046', #Male and Female 18-24 Less than a high school graduate. Add Together
               #Substance Abuse
               'B18135_002', #population under 18
               'B18135_003', #youth with a disability
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
               'under_18_with_disability',
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
)

acs_dat <- get_acs(geography = 'tract',
                   variables = c(acs_vars[1,1]),
                   state = '006', #state fips code for CA
                   county = "085", #county fips code for santa clara
                   geometry = TRUE, year = ACS_YEAR) %>% sf::st_transform(crs = '+init=epsg:4326')
colnames(acs_dat)[colnames(acs_dat) == 'estimate'] = acs_vars[1,2]
for(var_i in 2:nrow(acs_vars)){
  var_name = acs_vars[var_i,1]
  new_var_table = get_acs(geography = 'tract',
                          variables = var_name,
                          state = '006',
                          county = '085',
                          geometry = FALSE, year = ACS_YEAR)
  new_var_column = new_var_table$estimate
  acs_dat = cbind(acs_dat, new_var = new_var_column)
  colnames(acs_dat)[colnames(acs_dat) == 'new_var'] = acs_vars[var_i, 2]
} #individually adding each ACS variable since the get_acs function only seems to be able to handle one variable at a time.
acs_dat$variable = NULL
acs_dat$moe = NULL


acs_spdf = sf::as_Spatial(acs_dat)

acs_spdf$tract = gsub('(Census Tract) ([[:digit:]]+)(\\.*)([[:digit:]]*)([[:print:]]*)', '\\2\\3\\4', acs_spdf@data$NAME)

saveRDS(acs_spdf, 'RDS files/2017 ACS spdf.rds')




#### Pulling and merging data from scc health open data portal - scc_dat and full_spdf #######

install_and_load('geojsonio')
install_and_load('sp')
health_and_safety_stats = geojsonio::geojson_read(x = 'https://opendata.arcgis.com/datasets/48616d7b20e243f99388e847e30962d1_0.geojson', what = 'sp')
# econ_and_ed_stats = geojsonio::geojson_read(x = 'https://opendata.arcgis.com/datasets/c93017c1bf7340b6bbbc19ce86cf9717_0.geojson', what = 'sp')
# demographics = geojsonio::geojson_read(x = 'https://opendata.arcgis.com/datasets/eb57d42a2dd74372a45940b77cffc607_0.geojson', what = 'sp')
health_status = geojsonio::geojson_read(x = 'https://opendata.arcgis.com/datasets/b5a9684856aa422f87b42e2331c4371f_0.geojson', what = 'sp')


health_status@data = health_status@data[,which(colnames(health_status@data) %in% c('OBJECTID', 'birth_teen', 'smoker obese_adult',
                                                                                   'overobese_teen', 'overobese_child'))]
health_and_safety_stats@data = health_and_safety_stats@data[,which(colnames(health_and_safety_stats@data) %in% c('OBJECTID',
                                                                                                                 'tobacco_dens',
                                                                                                                 'crime_1mi_ave',
                                                                                                                 'alchohol_dens'))]
#checking to make sure both of these spdf have the same geography --> they do. 
# par(mfrow = c(1,2))
# plot(health_status)
# plot(health_and_safety_stats)

scc_dat = health_and_safety_stats
scc_dat@data = merge(scc_dat@data, health_status@data, by = 'OBJECTID')
scc_dat@data$neibid = scc_dat@data$OBJECTID

#I doubt I will be using district obesity metrics, but this does give us a nifty spatial file for all of the districts in SCC
# district_obesity = geojsonio::geojson_read(x = 'https://opendata.arcgis.com/datasets/6e79cab5f045430ab7da5b61e8c75d42_0.geojson', what = 'sp')

#now to merge it with the SCC data. Since SCC data is neighborhoods and acs is tracts (which are inside the neighborhoods),
#I will calculate the centroid of each tract, and see which neighborhood it lies in. Then I will add the SCC data to the tracts

#Centroids of tracts
install_and_load('rgeos')
trueCentroids = gCentroid(acs_spdf,byid=TRUE)
# plot(acs_spdf)
# points(trueCentroids,pch=1)

acs_points = sp::SpatialPointsDataFrame(trueCentroids, data = data.frame(geoid = acs_spdf@data$GEOID))

#seeing which neighborhood each tract is over
object_id_of_tract = sp::over(acs_points, scc_dat)[,1]

acs_spdf@data$neibid = object_id_of_tract

full_spdf = acs_spdf 
full_spdf@data = merge(acs_spdf@data, scc_dat@data, by = "neibid")

############# Getting Tract Area (in square meters) - scc_tract_area, full_spdf #############

scc_tract_area = tigris::tracts(state = "CA", county = 'Santa Clara')@data %>% arrange(GEOID) %>% select(GEOID, ALAND)

full_spdf@data = merge(full_spdf@data, scc_tract_area, by = 'GEOID')

############## cannabis retailers - weed_dens, full_spdf ##########

weed_licenses = read.csv('CSV files/Weed retail Licenses 12-21-2018.csv', stringsAsFactors = F) %>% dplyr::filter(City == 'SAN JOSE')
#there are 5 weed retailers in San Jose, so we can manually find their locations

lat_lon = c(37.356812, -121.931894, #Airfield Supply Co
            37.365118, -121.899762, #Harborside
            37.313747, -121.865723, #Caliva
            37.364916, -121.890276, #Purple Lotus Patient Center
            37.301869, -121.856779) #White Fire

weed_licenses_spdf = sp::SpatialPointsDataFrame(coords = data.frame(lon = lat_lon[seq(from = 2, to = length(lat_lon), by = 2)],
                                                                    lat = lat_lon[seq(from = 1, to = (length(lat_lon) - 1), by = 2)]),
                                                data = weed_licenses,
                                                proj4string = full_spdf@proj4string)

weed_tracts = sp::over(weed_licenses_spdf, full_spdf)$GEOID

weed_tract_count = count_tract_appearance(geoid_list = full_spdf@data$GEOID, subject_geoid = weed_tracts)
weed_dens =data.frame(GEOID = weed_tract_count[,1], weed_dens = weed_tract_count[,2]/(as.numeric(full_spdf@data$ALAND) * SQUARE_METER_TO_SQUARE_MILE))

full_spdf@data = merge(full_spdf@data, weed_dens, by = 'GEOID')

######## Geocoding grafitti addresses ############

graf_16 = read.csv('Grafitti Data/SanJose_FY_2016-2017.csv', skip = 2, stringsAsFactors = FALSE)
graf_17 = read.csv('Grafitti Data/SanJose_FY_2017-2018.csv', skip = 2, stringsAsFactors = FALSE)
graf_16 = graf_16[,colnames(graf_16)[which(colnames(graf_16) %in% colnames(graf_17))]]
graf_17 = graf_17[,colnames(graf_16)[which(colnames(graf_16) %in% colnames(graf_17))]]
graf = rbind(graf_16, graf_17)


install_and_load('ggmap')
#registering API key
{
  api_key = names(read.delim(file = "C:\\Users\\albert.gehami\\Documents\\API Key.txt"))
  register_google(key = api_key, account_type = 'standard')
  remove(api_key)
}
#geocoding every address in the database, via google's API. This process takes about 12 hours, so only re-do it if you have to.
# geocode_graf_table = data.frame(lon = rep(NA, nrow(graf)), lat = NA, address = graf$Address, stringsAsFactors = FALSE)
# for(n in 1 : nrow(geocode_graf_table)){
#   addy = as.character(graf$Address[n])
#   if(gsub('[[:space:]]', '', addy) == '') next
#   geocode_graf_table[n,] = try(ggmap::geocode(location = paste0(graf$Address[n], ', San Jose, CA'), output = 'latlona')[1,])
#   if(n %% 100 == 0)print(paste0(n, ' out of ', nrow(geocode_graf_table)))
# }
# 
# saveRDS(geocode_graf_table, file = 'Grafitti Data/graffiti_16_18.rds')
geocode_graf_table = readRDS('Grafitti Data/graffiti_16_18.rds')

#determining the spatial outliers in the dataset to remove them
print(quantile(geocode_graf_table$lat,na.rm = TRUE, probs = c(0, 0.0001, 0.0002, 0.001, 0.002, 0.99, 0.998, 0.999, 0.9998, 0.9999, 1)))#so it looks like we should cut the bottom 0.1% and top 0.1%.
upperbound_lat = as.numeric(quantile(geocode_graf_table$lat, na.rm = TRUE, probs = .999))
lowerbound_lat = as.numeric(quantile(geocode_graf_table$lat, na.rm = TRUE, probs = .001))

print(quantile(geocode_graf_table$lon,na.rm = TRUE, probs = c(0, 0.0001, 0.0002, 0.001, 0.002, 0.01, .5, 0.99, 0.998, 0.999, 0.9998, 0.9999, 1)))#so it looks like we should cut the bottom 0.1% and top 0.1%.
upperbound_lon = as.numeric(quantile(geocode_graf_table$lon, na.rm = TRUE, probs = .999))
lowerbound_lon = as.numeric(quantile(geocode_graf_table$lon, na.rm = TRUE, probs = .001))

#marking the rows which are not spatial outliers
use_inds = which(!is.na(geocode_graf_table$lat) & geocode_graf_table$lat < upperbound_lat & geocode_graf_table$lat > lowerbound_lat & 
                   geocode_graf_table$lon < upperbound_lon & geocode_graf_table$lon > lowerbound_lon)

graf_coords = data.frame(lon = geocode_graf_table$lon[use_inds], lat = geocode_graf_table$lat[use_inds], stringsAsFactors = FALSE)

#creating the spatialpointsdataframe with a consistent proj4string
graf_fin = sp::SpatialPointsDataFrame(coords = graf_coords,
                                      data = data.frame(id = graf$Work.Order.Id[use_inds], address = geocode_graf_table$address[use_inds], surface = graf$Surface[use_inds],
                                                        sqft = graf$Surface.Square.Feet[use_inds], cost = graf$Cost[use_inds], date = graf$Closed.Date[use_inds], stringsAsFactors = FALSE),
                                      proj4string = full_spdf@proj4string)

graf_fin@data$date = gsub('(/20)([0-9]{2})', '/\\2', graf_fin@data$date)

#converting the data variable into a data object.
graf_fin$date = as.Date(as.character(graf_fin$date), format = '%m/%d/%y')

#marking the tract each observation is over. 
graf_fin$GEOID = sp::over(graf_fin, full_spdf)$GEOID

#converting the cost variable to numeric
graf_fin$cost = as.numeric(gsub('\\$', '', graf_fin$cost))


######### Getting neighborhood names via SCC health data - neib_names_data #########

install_and_load('geojsonio')
install_and_load('sp')
health_and_safety_stats = geojsonio::geojson_read(x = 'https://opendata.arcgis.com/datasets/48616d7b20e243f99388e847e30962d1_0.geojson', what = 'sp')
#Centroids of tracts
install_and_load('rgeos')
trueCentroids = gCentroid(full_spdf,byid=TRUE)
# plot(acs_spdf)
# points(trueCentroids,pch=1)
acs_points = sp::SpatialPointsDataFrame(trueCentroids, data = data.frame(geoid = full_spdf@data$GEOID))
#seeing which neighborhood each tract is over
object_id_of_tract = sp::over(acs_points, health_and_safety_stats)[,1]
#linking neighborhood name to tract GEOID
neib_names_data = data.frame(neib_name = health_and_safety_stats@data$NGBRHD2[object_id_of_tract], GEOID = full_spdf@data$GEOID)

######## Saving neib_names_data - 'neighborhood names by census tract GEOID.rds'#########

saveRDS(neib_names_data, file = 'RDS files/neighborhood names by census tract GEOID.rds')




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

############ Saving full_spdf and graf_fin files - 'Metrics without time dependency.rds' and 'Full Graffiti Data.rds' ##########
saveRDS(full_spdf, 'RDS files/Metrics without time dependency.rds')
saveRDS(graf_fin, 'RDS files/Full Graffiti Data.rds')

######## Adding East Side Union School District to the school boundaries - school_bounds_updated.rds ############
school_bounds = readRDS('RDS files/school_bounds_for_16-18_IRs.rds') %>% sp::spTransform(full_spdf@proj4string)

#so we need to add in east side union high school district to school boundaries.
district_bounds = rgdal::readOGR(dsn = "cb_2017_06_scsd_500k",
                                 layer = "cb_2017_06_scsd_500k", stringsAsFactors = FALSE) %>% sp::spTransform(school_bounds@proj4string)
esu_bounds = district_bounds[district_bounds$NAME == 'East Side Union High School District',] #district boundaries for East Side Union

#adding esu_bounds to school_bounds
head(esu_bounds@data)
head(school_bounds@data)
esu_bounds@data = data.frame(OBJECTID = 'East Side Union High School District',
                             address = 'NA', lat = NA, lon = NA, full_names = 'ESUHSD')
school_bounds = rbind(school_bounds, esu_bounds) #works

school_bounds$OBJECTID = trimws(school_bounds$OBJECTID)

#saving the updated school_bounds
saveRDS(school_bounds, file = 'RDS files/school_bounds_updated.rds')



############ Adding in School Graduation Rates 'graduation rates spdf.rds'###############
grad_rates = read.csv('CSV files/San Jose High School Graduation Data 2012-2017.csv', stringsAsFactors = FALSE)
grad_rates = grad_rates[,grep('^\\X', colnames(grad_rates), invert = TRUE)]
grad_rates$total_enrolled = rowSums(grad_rates[,grep('Enrolled', colnames(grad_rates))])
grad_rates$total_graduated = rowSums(grad_rates[,grep('Graduated', colnames(grad_rates))])
#if there is any total_graduated > total_enrolled, reduces it to the total_enrolled value
grad_rates$total_graduated[grad_rates$total_enrolled < grad_rates$total_graduated] = 
  grad_rates$total_enrolled[grad_rates$total_enrolled < grad_rates$total_graduated]
grad_rates$grad_rate = grad_rates$total_graduated / grad_rates$total_enrolled
grad_rates$grad_rate[grad_rates$grad_rate > 1] = 1
# misfits = grad_rates[which(grad_rates$grad_rate < 0.6 | grad_rates$grad_rate > 1),] #so apparently these aren't misfits, this is the truth. Goodness.

#seeing how the school names in grad_rates compares to the school names in school_bounds
# print(cbind(school_bounds = unique(school_bounds@data$OBJECTID[order(unique(school_bounds@data$OBJECTID))]), 
#             grad_school = unique(grad_rates$High.School[order(unique(grad_rates$High.School))])))

# #let's go into excel and create a conversion table here. 
# write.csv(cbind(school_bounds = unique(school_bounds@data$OBJECTID[order(unique(school_bounds@data$OBJECTID))]), 
#                 grad_school = unique(grad_rates$High.School[order(unique(grad_rates$High.School))])),
#           file = 'CSV files/Conversion_raw_grad_school_bounds.csv')
bounds_conversion = read.csv('CSV files/Conversion_raw_grad_school_bounds_done.csv', stringsAsFactors = FALSE)

grad_rates$bound_label = NA
for(n in seq_len(nrow(grad_rates))){
  grad_rates$bound_label[n] = bounds_conversion[bounds_conversion$grad_school == grad_rates$High.School[n],3][1]
}
grad_rates$bound_label = trimws(grad_rates$bound_label)
#now all the bound names are labeled in the grad_rates data. We now need to make the grad_rates a shapefile. with the boundaries attached
grad_rate_polygon_inds = match(grad_rates$bound_label, school_bounds$OBJECTID)
grad_rates_spdf = school_bounds[grad_rate_polygon_inds,]
grad_rates_spdf@data = grad_rates

grad_rates_spdf@data$date = as.Date(paste0(grad_rates_spdf$Graduating.Year, '-01-01'))

saveRDS(grad_rates_spdf, 'RDS files/graduation rates spdf.rds')

######CHECKPOINT - Anything beyond this is date-dependent and handled in the "Agregate Yearly Data.R" file. Below is just testing ############
# ############################
# 
# ############# Crime Data  - crime_data, crime_by_tract, and full_spdf ############
# install_and_load('dplyr')
# 
# 
# crime_data = crime_data_full %>% dplyr::filter(rpt_dt > START_DATE, rpt_dt < END_DATE)
# 
# 
# 
# #organizing the crimes of importance here. 
# {crimes = data.frame(
#   #crime title
#   crime = c(
#     #vocational training
#     'AUTO THEFT', 
#     'NARCOTICS',
#     'SEX',
#     #Substance Abuse
#     'DUI', #DUI with drugs or alc
#     '647(F) PC DRUNK IN PUBLIC', #also include NARCOTICS
#     #Parent Awareness
#     'CHILD ABUSE',
#     'CHILD MOLEST',
#     'CHILD NEGLECT',
#     '243(E)(1) PC BATTERY DOMESTIC', '273.5(A) PC DOMESTIC VIOLENCE', #Domestic Violence
#     'MISSING JUV', #runaway youth
#     #Gang Presence
#     'AGG ASSAULT', '261.5PC UNLAW INTERCRSE MINOR',#Developmental Trauma. Also include child molestation above
#     #Child Maltreatment. Also include domestic violence, child neglect/abuse, and Runaway youth
#     'ROBBERY', 'ASSAULT', '422 PC CRIMINAL THREATS', #Violent and Prejudice Victimization. Also include AGG ASSAULT. 422 is threats or hate crimes.
#     '594 PC VANDALISM-GRAFFITI', 'DISTURB PEACE', #Disorder, also include NARCOTICS, and DUI and drunk in public
#     'WEAPONS', '211 PC ROBBERY ARMED', '211 PC ROBBERY STRONG-ARM', 'DOMESTIC-211 PC ARMED ROBBERY' #Weapons. This is all Weapons possession and armed robberies.
#   ),
#   #which column the title is from (usually OFFENSE_DESCRIPTION or CATEGORY)
#   column = paste0('c_',c(
#     #vocational training
#     'auto_theft',
#     'narcotics',
#     'sex',
#     #substance abuse
#     'dui', #DUI with drugs or alc
#     'drunk_in_public',
#     #Parent Awarenesss
#     'child_abuse',
#     'child_molest',
#     'child_neglect',
#     'domestic_battery', 'domestic_violence',
#     'missing_juv',
#     #Gang Presence
#     'agg_assualt', 'intercourse_with_minor', #Also include child molestation above
#     #Child Maltreatment. Also include domestic violence, child neglect/abuse, and Runaway youth
#     'robbery', 'assault', 'threats',  #Violent and Prejudice Victimization. Also include AGG ASSAULT. 422 is threats or hate crimes.
#     'graffiti', 'disturbance', #Disorder, also include NARCOTICS and DUI and drunk in public
#     'weapons', 'armed_robbery', 'strong_armed_robbery', 'domestic_armed_robbery'
#   )), stringsAsFactors = F
# )
# }
# 
# crime_data = crime_data[crime_data$OFFENSE_DESCRIPTION %in% crimes[,1] | crime_data$CATEGORY %in% crimes[,1],]
# 
# crimes_matrix = matrix(0, nrow = nrow(full_spdf@data), ncol = length(crimes[,1]))
# colnames(crimes_matrix) = crimes[,1]
# crime_by_tract = data.frame(GEOID = full_spdf@data$GEOID, crimes_matrix, stringsAsFactors = F) %>% arrange(GEOID)
# colnames(crime_by_tract) = c('GEOID', crimes[,1])
# # geoid_list = crime_by_tract$GEOID
# # subject_geoid = crime_data[crime_data$CATEGORY == 'AGG ASSAULT','geoid']
# 
# 
# geoid_list = crime_by_tract$GEOID
# for(n in 2:ncol(crime_by_tract)){
#   crime_by_tract[,n] = count_tract_appearance(geoid_list = geoid_list, 
#                                               subject_geoid = crime_data[crime_data$CATEGORY == colnames(crime_by_tract)[n] | 
#                                                                            crime_data$OFFENSE_DESCRIPTION == colnames(crime_by_tract)[n],'geoid'])[,2]
# }
# 
# colnames(crime_by_tract) = c('GEOID', crimes[,2])
# 
# #Sanity check --> the above works, and quickly.
# #print(crime_data[crime_data$geoid == '06085502907',c('CATEGORY', 'OFFENSE_DESCRIPTION')] %>% arrange(CATEGORY, OFFENSE_DESCRIPTION))
# 
# 
# full_spdf@data = merge(full_spdf@data, crime_by_tract, by = 'GEOID')
# 
# 
# ############ SSCI Incident Data - incident_counts, full_spdf ##########
# 
# school_bounds = readRDS('RDS files/school_bounds_for_16-18_IRs.rds') %>% sp::spTransform(full_spdf@proj4string)
# ssci_incidents = ssci_incidents_full %>% dplyr::filter(level %in% INCLUDED_LEVELS) %>%
#   select(date, school, level) %>%
#   filter(date > START_DATE, date < END_DATE) %>% select(-date)
# 
# ssci = stats::aggregate(ssci_incidents$level, by = list(ssci_incidents$school), FUN = sum)
# ssci_bounds = school_bounds[school_bounds$OBJECTID %in% ssci$Group.1,]
# 
# intersect_matrix = matrix(data = 0, nrow = nrow(ssci_bounds@data), ncol = nrow(full_spdf@data))
# colnames(intersect_matrix) = full_spdf@data$GEOID
# rownames(intersect_matrix) = ssci_bounds@data$OBJECTID
# 
# 
# #Let's just attempt to identify the tracts that a single district is in.
# # one_dis = list(school_bounds@polygons[[1]]) %>% sp::SpatialPolygons(proj4string =  full_spdf@proj4string)
# for(school_ind in seq_along(ssci_bounds@polygons)){
#   intersect_matrix[school_ind,which(rgeos::gIntersects(school_bounds[school_ind,], full_spdf, byid = TRUE))] = 1
# }
# #Now we have a matrix which marks every tract that is in every school boundary as 1.
# 
# incident_counts = data.frame(GEOID = colnames(intersect_matrix), ssci_incidents = as.numeric(colSums(intersect_matrix)))
# 
# full_spdf@data = merge(full_spdf@data, incident_counts, by = 'GEOID')
# 
# 
# 
# ########### Neibor Matrix - loc_dist_matrix ###########
# # 1/x_ij where x is number of blocks between block i and j (starting at 1), 0 if more than MAX_BLOCK_DIST away 
# MAX_LOC_DIST = 1
# loc_it = 1
# 
# #initializing the matrix
# loc_dist_matrix = matrix(0, nrow = nrow(full_spdf@data), ncol = nrow(full_spdf@data))
# loc_matrix = gTouches(full_spdf, byid = TRUE)
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
# 
# 
# ############# That's all the data. #############
# 
# 
# ############# Handling data operations and dropping certain data - need_metrics_spdf #############
# 
# 
# 
# columns_to_convert_to_percentages_of_total_population = 
#   c('unemployed', 'not_in_labor_force', 'below_poverty_line', 
#     'no_diploma_18_24_male', 'no_diploma_18_24_female', 'under_18_pop', 'under_18_with_disability', 'single_mothers',
#     'no_diploma_25_64', 'family_in_poverty', 'entered_2010_later', 'foreign_born', 'disability_under_18',
#     'income_under_poverty_line', 'Poverty_6_and_younger', 'Poverty_6_11', 'Poverty_12_17',
#     crimes[,2], 'ssci_incidents')
# 
# #calculating ethnic diversity index (edi)
# race_cols = c('white', 'black', 'asian', 
# 'hispanic', 'amiakn', 'hawaiipi', 'other_races', 'two_or_more_races')
# 
# #calculating diversity index - 
# num_races = length(race_cols)
# total_race_pops = rowSums(full_spdf@data[,race_cols])
# race_fractions = full_spdf@data[,race_cols] / total_race_pops
# race_fractions_reduced = (race_fractions - (1/num_races))^2 #this works element-wise, so it works as we want it to.
# d_8 = sqrt(rowSums(race_fractions_reduced))
# c1 = 100
# c2 = -100*sqrt(num_races*(num_races-1))/(num_races - 1)
# edi = c1 + (c2*d_8)
# 
# #works
# # print(edi[1:6])
# # print(full_spdf@data[1:6, race_cols])
# 
# #Turning all of the census and crime data into percentages of total population
# full_spdf@data[,columns_to_convert_to_percentages_of_total_population] = 
#   full_spdf@data[,columns_to_convert_to_percentages_of_total_population]/full_spdf@data$total_pop
# 
# full_spdf@data$edi = edi
# 
# #creating the "youth in poverty" variable
# full_spdf$youth_in_poverty = full_spdf$Poverty_6_and_younger + full_spdf$Poverty_6_11 + full_spdf$Poverty_12_17
# #creating a variable for all 18-24 yo without a high school diploma
# full_spdf$no_diploma_18_24 = full_spdf$no_diploma_18_24_female + full_spdf$no_diploma_18_24_male
# 
# 
# need_metrics_spdf = full_spdf
# need_metrics_spdf@data = need_metrics_spdf@data[,-which(colnames(need_metrics_spdf@data) %in% c('ALAND', 'overobese_teen',
#                                                     'overobese_child', 'OBJECTID',
#                                                     'tract', race_cols, 'Poverty_12_17',
#                                                     'Poverty_6_11', 'Poverty_6_and_younger',
#                                                     'no_diploma_18_24_male',
#                                                     'no_diploma_18_24_female',
#                                                     'under_18_pop',
#                                                     'sampled_pop', 'neibid'))]
# 
# 
# 
# ########## Calculating Concentrated Economic Disadvantage ###########
# 
# #relative poverty
# poverty_vec = loc_dist_matrix %*% need_metrics_spdf@data$below_poverty_line
# division_vec = loc_dist_matrix %*% rep(1, nrow(loc_dist_matrix))
# need_metrics_spdf@data$rel_poverty = need_metrics_spdf@data$below_poverty_line/(poverty_vec/division_vec)
# 
# #relative unemployment
# unemployment_vec = loc_dist_matrix %*% need_metrics_spdf@data$unemployed
# need_metrics_spdf@data$rel_unemployment = need_metrics_spdf@data$unemployed/(unemployment_vec/division_vec)
# 
# 
# 
# ############# Keeping only San Jose Census Tracts - need_metrics_spdf #########
# 
# # sj = tigris::places('CA') %>% sp::spTransform(need_metrics_spdf@proj4string)
# # sj = sj[sj@data$NAME == 'San Jose',]
# 
# # saveRDS(sj, 'RDS files/sj_shapefile.rds')
# 
# #identifying all of the tracts which intersect with or are within san jose
# # tract_city_matrix = matrix(data = 0, nrow = nrow(full_spdf@data), ncol = 1)
# # rownames(tract_city_matrix) = need_metrics_spdf@data$GEOID
# # colnames(tract_city_matrix) = seq_along(sj@polygons)
# 
# need_metrics_spdf = need_metrics_spdf[which(gIntersects(sj, need_metrics_spdf, byid = TRUE)),]
# 
# ######### Min-Max Scaling all of the variables - need_metrics_spdf ##########
# 
# #given a vector, min-max scales it.
# min_max_vec = function(vec, ...){
#   return((vec - min(vec, ...))/(max(vec,...)-min(vec,...)))
# }
# 
# #minmax scaling all of the metrics
# for(n in 4 : ncol(need_metrics_spdf@data)){
#   need_metrics_spdf@data[,n] = min_max_vec(need_metrics_spdf@data[,n])
# }
# 
# 
# 
# ############### Saving need_metrics_spdf - 'need metrics all minmax scaled data.rds' ############
# 
# saveRDS(need_metrics_spdf, 'RDS files/need metrics all minmax scaled data.rds')
# 
# 
# 
# 
# 
# 
# 
# 
# 
