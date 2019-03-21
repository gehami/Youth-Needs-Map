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
ACS_YEAR = 2016
######### ACS data ###########


install_and_load('tidyverse')
install_and_load('tidycensus')

acs_2016_vars = load_variables(ACS_YEAR, 'acs5', cache = CACHE_OPTION)

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

install_and_load('rowr')



acs_dat <- get_acs(geography = '', #zipcode
                   variables = c(acs_vars[1,1]),
                   state = '006', #state fips code for CA
                   county = "085", #county fips code for santa clara
                   geometry = TRUE, year = ACS_YEAR) %>% sf::st_transform(crs = '+init=epsg:4326')
colnames(acs_dat)[colnames(acs_dat) == 'estimate'] = acs_vars[1,2]
for(var_i in 2:nrow(acs_vars)){
  var_name = acs_vars[var_i,1]
  new_var_table = get_acs(geography = 'zip code tabulation area', #zipcode
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

saveRDS(acs_spdf, '2016 ACS spdf.rds')








