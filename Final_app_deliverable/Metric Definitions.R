get_all_metrics = function(){
  
all_metrics = list()

all_metrics[[1]] = list('GANG_PRESENCE', data.frame(matrix(
  data = c(
    'c_child_neglect',                          1, 'child_maltreatment',
    'c_child_abuse',                            1, 'child_maltreatment',
    'c_missing_juv',                            1, 'child_maltreatment',
    'c_domestic_violence',                      1, 'developmental_trauma',
    'c_domestic_battery',                       1, 'developmental_trauma',
    'c_domestic_armed_robbery',                 1, 'developmental_trauma',
    'c_child_molest',                           1, 'developmental_trauma',
    'c_intercourse_with_minor',                 1, 'developmental_trauma',#this is sex abuse with a youth
    'c_agg_assault',                            1, 'exposure_to_violence_and_racial_prejudice',
    'c_robbery',                                1, 'exposure_to_violence_and_racial_prejudice',
    'c_assault',                                1, 'exposure_to_violence_and_racial_prejudice',
    'c_threats',                                1, 'exposure_to_violence_and_racial_prejudice', #hate crimes and threats
    # 'c_graffiti',                               1, 'community_disorganization',
    # 'c_dui',                                    1, 'community_disorganization',
    'c_total_crime',                            1, 'community_disorganization', #public nuisance
    # 'c_drunk_in_public',                        1, 'community_disorganization',
    'below_poverty_line',                       1, 'community_disorganization',
    # 'c_narcotics',                              1, 'community_disorganization',
    'c_weapons',                                1, 'presence_of_illegal_firearms',
    'c_armed_robbery',                          1, 'presence_of_illegal_firearms',
    # 'c_strong_armed_robbery',                   1, 'presence_of_illegal_firearms',
    'c_gang_crime',                             1, 'gang_affiliated_crime',
    'graf_incidents',                           1, 'graffiti',
    'graf_area',                                1, 'graffiti',
    'ssci_incidents',                           0.5, 'school_violence_and_low_graduation',
    'no_grad_rate',                             1, 'school_violence_and_low_graduation',
    'disability_under_18',                      1, 'youth_with_disabilities',
    'family_in_poverty',                        1, 'poverty',
    'youth_in_poverty',                         1, 'poverty',
    'c_minor_tobacco',                          1, 'substance_abuse',
    'c_minor_alcohol',                          1, 'substance_abuse',
    'c_dui',                                    1, 'substance_abuse',
    # 'c_drunk_in_public',                        1, 'substance_abuse',
    'c_narcotics',                              5, 'substance_abuse', 
    'alchohol_dens',                            1, 'substance_abuse', 
    'tobacco_dens',                             1, 'substance_abuse',
    'weed_dens',                                1, 'substance_abuse'
    
    #,'edi',                                      1, 'social_discrimination'
  ),
  ncol = 3,
  byrow = TRUE
), stringsAsFactors = FALSE),

# length(which(GANG_PRESENCE_METRICS[,1] %in% colnames(need_metrics_spdf@data))) == length(GANG_PRESENCE_METRICS[,1]) #we good

#weights for classes
data.frame(
  matrix(
    data = c(
      'gang_affiliated_crime',           1,
      'graffiti',                        1,
      'child_maltreatment',              1,
      'developmental_trauma',            1,
      'exposure_to_violence_and_racial_prejudice', 1,
      'community_disorganization',        1,
      'presence_of_illegal_firearms',    1,
      'school_violence_and_low_graduation',               1,
      'youth_with_disabilities',           1,
      'poverty',            1,
      'substance_abuse',                 1#, 
      #'social_discrimination',           0.2
    ),
    ncol = 2,
    byrow = TRUE
  ), stringsAsFactors = FALSE
))

all_metrics[[2]] = list('VOCATIONAL_TRAINING', data.frame(matrix(
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
), stringsAsFactors = FALSE),
data.frame(
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
))

all_metrics[[3]] = list('PARENT_AWARENESS', data.frame(matrix(
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
), stringsAsFactors = FALSE),

data.frame(
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
))

return(all_metrics)

}





