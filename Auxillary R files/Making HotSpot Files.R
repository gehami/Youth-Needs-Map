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

######## Libraries ##########
install_and_load('geojsonio')
install_and_load('sp')
####### Opening Data ##########

file_folder = 'hotspots 17/'
hotspots_17_list = list.files(file_folder)
hotspot_17 = geojsonio::geojson_read(paste0(file_folder, hotspots_17_list[1]), what = 'sp')

for(file_name in hotspots_17_list[-1]){
  hotspot_17 = rbind(hotspot_17, geojsonio::geojson_read(paste0(file_folder,file_name), what = 'sp'))

}

hotspot_17@data = data.frame(hot_spot = tolower(gsub('\\.json', '', hotspots_17_list)))
hotspot_17_names = 
  {matrix(
  c(
    'C1', 'Jeanne',
    'C2', 'Roosevelt Park',
    'C3', '10th and Williams',
    'C4', 'Backesto Park',
    'F1', 'Mayfair',
    'F2', 'Poco Way',
    'F3', 'Overfelt',
    'F4', 'Valley Palms & San Jose Apts',
    'F5', 'Kollmar', 
    'F6', 'Capitol Park',
    'F7', 'Overfelt Gardens',
    'S1', 'Roundtable/Great Oaks',
    'S2', 'Seven Trees',
    'S3', 'Hoffman/Via Monte',
    'S4', 'Farm Dr/Dakan Ct',
    'W1', 'Washington',
    'W2', 'Cadillac',
    'W3', 'Santee',
    'W4', 'Fruitdale'
  ),
  byrow = TRUE,
  ncol = 2
)
}
hotspot_17@data = cbind(hotspot_17@data, hotspot_name = hotspot_17_names[,2])


file_folder = 'hotspots 15/'
hotspots_15_list = list.files(file_folder)
hotspot_15 = geojsonio::geojson_read(paste0(file_folder, hotspots_15_list[1]), what = 'sp')

for(file_name in hotspots_15_list[-1]){
  hotspot_15 = rbind(hotspot_15, geojsonio::geojson_read(paste0(file_folder,file_name), what = 'sp'))
  
}

hotspot_15@data = data.frame(hot_spot = tolower(gsub('\\.json', '', hotspots_15_list)))

hotspot_15_names = 
{matrix(
  c(
    'C1', '2nd Street and Santa Clara',
    'C2', 'Jeanne',
    'C3', '13th St to 4th St',
    'C4', 'South University Neighborhood',
    'F1', 'Independence HS/Avalani',
    'F2', 'Valley Palms/SJ Apts',
    'F3', 'Kollmar Apts',
    'F4', 'Overfelt',
    'F5', 'Poco Way', 
    'F6', 'Mayfair/Sanders and Sunset',
    'S1', 'Roundtable/Edenvale',
    'S2', 'Hoffman/Via Monte',
    'S3', 'Seven Trees',
    'S4', 'Eagles and Tradewinds',
    'W1', 'Washington',
    'W2', 'santee/Audubon',
    'W3', 'Cadillac/Winchester',
    'W4', 'Payne Ave and San Thomas Exp'
  ),
  byrow = TRUE,
  ncol = 2
)
}

hotspot_15@data = cbind(hotspot_15@data, hotspot_name = hotspot_15_names[,2])


saveRDS(hotspot_17, 'hotspots 2017.rds')
saveRDS(hotspot_15, 'hotspots 2015.rds')


