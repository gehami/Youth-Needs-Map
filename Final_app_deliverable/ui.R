#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# install.packages('shinyjs')
# install.packages('Hmisc')
# install.packages('shinyBS')
library(shiny)
# library(shinyjs)
library(shinyBS)
library(DT)
library(leaflet)
# library(Hmisc)


MIN_INPUT = 0
MAX_INPUT = 5
STEP_SIZE = 0.1
START_VALUE = 1
TOGGLES_PER_ROW = 3
big_list = readRDS('big_list_cleaned.rds')
BIG_LIST_LENGTH = length(big_list) #should set this to the length of big_list at some point, but that can be later.

source('Metric Definitions.R')
all_metrics = get_all_metrics()




gp_cols = all_metrics[[1]][[3]][,1]
gp_cols_num = paste0(gp_cols, '_num')
gp_cols_fancy_names = Hmisc::capitalize(gsub('_', ' ', gp_cols))
gp_cols_num_rows = ceiling(length(gp_cols)/TOGGLES_PER_ROW)
while(length(gp_cols_fancy_names) < gp_cols_num_rows*TOGGLES_PER_ROW){
  gp_cols_fancy_names = c(gp_cols_fancy_names, '')
}


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    includeScript('js/update_sliders.js'),
    # Removes spin wheels from inputs - shout out to wolfpack on https://community.rstudio.com/t/how-to-remove-numeric-inputs-spin-button-in-r-shiny/13769/2
    tags$style(HTML("
                input[type=number] {
                    -moz-appearance:textfield;
                    }
                    input[type=number]::{
                    -moz-appearance:textfield;
                    }
                    input[type=number]::-webkit-outer-spin-button,
                    input[type=number]::-webkit-inner-spin-button {
                    -webkit-appearance: none;
                    margin: 0;
                    }
                    ")),
  
  # Application title
  titlePanel("Youth Risk Map"),
  
  ###### Sliders ##########
  bsCollapse(id = 'Sliders',
             bsCollapsePanel('Click here to edit map metrics',
                             fluidRow(
                               column(12,
                                      tabsetPanel(
                                        type = 'tabs',
                                        ######## Gang Presence ###########
                                        tabPanel('Gang Presence',
                                                 br(),
                                                 fluidRow(column(4, actionButton('gang_presence_map', 'Update and Show Map')),
                                                          column(4, actionButton('gp_reset_weights', 'Reset Metrics')),
                                                          column(4,'')),#actionButton('gp_predict', 'Predict'))),
                                                 fluidRow(
                                                   column(4, h3('Gang Affiliated Crime')),
                                                   column(4, h3('Child Maltreatment')),
                                                   column(4, h3('Developmental Trauma'))
                                                 ),
                                                 fluidRow(
                                                   column(3, sliderInput(inputId = 'gang_affiliated_crime', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'gang_affiliated_crime_num', label = NULL,
                                                                          value = START_VALUE, width = '100%')),
                                                   column(3, sliderInput(inputId = 'child_maltreatment', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'child_maltreatment_num', label = NULL,
                                                                          value = START_VALUE)),
                                                   column(3, sliderInput(inputId = 'developmental_trauma', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'developmental_trauma_num', label = NULL,
                                                                          value = START_VALUE))
                                                   
                                                 ),
                                                 fluidRow(
                                                   column(4, h3('Violent and Prejudiced Victimization')),
                                                   column(4, h3('Disorder in Neighborhood')),
                                                   column(4, h3('Presence of Firearms'))
                                                 ),
                                                 fluidRow(
                                                   column(3, sliderInput(inputId = 'violent_prejudice_victimization', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'violent_prejudice_victimization_num', label = NULL,
                                                                          value = START_VALUE, width = '100%')),
                                                   column(3, sliderInput(inputId = 'disorder_in_neighborhood', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'disorder_in_neighborhood_num', label = NULL,
                                                                          value = START_VALUE)),
                                                   column(3, sliderInput(inputId = 'presence_of_firearms', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'presence_of_firearms_num', label = NULL,
                                                                          value = START_VALUE))
                                                   
                                                 ),
                                                 fluidRow(
                                                   column(4, h3('Youth School Conflicts')),
                                                   column(4, h3('Youth with Disabilities')),
                                                   column(4, h3('Economic Deprivation'))
                                                 ),
                                                 fluidRow(
                                                   column(3, sliderInput(inputId = 'youth_school_conflicts', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'youth_school_conflicts_num', label = NULL,
                                                                          value = START_VALUE, width = '100%')),
                                                   column(3, sliderInput(inputId = 'reported_disability', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'reported_disability_num', label = NULL,
                                                                          value = START_VALUE)),
                                                   column(3, sliderInput(inputId = 'economic_deprivation', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'economic_deprivation_num', label = NULL,
                                                                          value = START_VALUE))
                                                   
                                                 ),
                                                 fluidRow(
                                                   column(4, h3('Substance Abuse')),
                                                   column(4, h3('Graffiti')),
                                                   column(4, h3(''))
                                                 ),
                                                 fluidRow(
                                                   column(3, sliderInput(inputId = 'substance_abuse', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'substance_abuse_num', label = NULL,
                                                                          value = START_VALUE, width = '100%')),
                                                   column(3, sliderInput(inputId = 'graffiti', label = NULL, 
                                                                         min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                                                         value = START_VALUE)),
                                                   column(1, numericInput(inputId = 'graffiti_num', label = NULL,
                                                                          value = START_VALUE, width = '100%')),
                                                   column(3),
                                                   column(1)
                                                   
                                                 )
                                                 
                                        )#,
                                       #  #######  Vocational Training ##############
                                       #  tabPanel('Vocational Training', 
                                       #           br(),
                                       #           fluidRow(column(4, actionButton('vocational_training_map', 'Update and Show Map')),
                                       #                    column(4, actionButton('vocational_reset_weights', 'Reset Metrics')),
                                       #                    column(4, '')),#actionButton('vocational_predict', 'Predict'))),
                                       #           fluidRow(
                                       #             column(4, h3('Joblessness')),
                                       #             column(4, h3('Poverty in Community')),
                                       #             column(4, h3('Concentrated Disadvantage'))
                                       #           ),
                                       #           fluidRow(
                                       #             column(3, sliderInput(inputId = 'joblessness', label = NULL, 
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'joblessness_num', label = NULL,
                                       #                                    value = START_VALUE, width = '100%')),
                                       #             column(3, sliderInput(inputId = 'poverty_in_community', label = NULL, 
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'poverty_in_community_num', label = NULL,
                                       #                                    value = START_VALUE)),
                                       #             column(3, sliderInput(inputId = 'concentrated_disadvantage', label = NULL, 
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'concentrated_disadvantage_num', label = NULL,
                                       #                                    value = START_VALUE))
                                       #             
                                       #           ),
                                       #           fluidRow(
                                       #             column(4, h3('Education Completion')),
                                       #             column(4, h3('Illegal Economic Activity')),
                                       #             column(4, h3(''))
                                       #           ),
                                       #           fluidRow(
                                       #             column(3, sliderInput(inputId = 'edu_completion', label = NULL, 
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'edu_completion_num', label = NULL,
                                       #                                    value = START_VALUE, width = '100%')),
                                       #             column(3, sliderInput(inputId = 'illegal_econ_activity', label = NULL, 
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'illegal_econ_activity_num', label = NULL,
                                       #                                    value = START_VALUE)),
                                       #             column(3),
                                       #             column(1)
                                       #             
                                       #           )),
                                       # ########## Parent Awareness ##########
                                       #  tabPanel('Parent Awareness', 
                                       #           br(),
                                       #           fluidRow(column(4, actionButton('parent_awareness_map', 'Update and Show Map')),
                                       #                    column(4, actionButton('parent_reset_weights', 'Reset Metrics')),
                                       #                    column(4, '')),#actionButton('parent_predict', 'Predict'))),
                                       #           fluidRow(
                                       #             column(4, h3('Child Maltreatment')),
                                       #             column(4, h3('Single Mothers')),
                                       #             column(4, h3('Teen Mothers'))
                                       #           ),
                                       #           fluidRow(
                                       #             column(3, sliderInput(inputId = 'child_maltreatment_pa', label = NULL,
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'child_maltreatment_pa_num', label = NULL,
                                       #                                    value = START_VALUE, width = '100%')),
                                       #             column(3, sliderInput(inputId = 'prop_single_mothers', label = NULL,
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'prop_single_mothers_num', label = NULL,
                                       #                                    value = START_VALUE)),
                                       #             column(3, sliderInput(inputId = 'teen_mothers', label = NULL,
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'teen_mothers_num', label = NULL,
                                       #                                    value = START_VALUE))
                                       #             
                                       #           ),
                                       #           fluidRow(
                                       #             column(4, h3('Families in Poverty')),
                                       #             column(4, h3('Families Unaware of Community')),
                                       #             column(4, h3('Drug Abuse'))
                                       #           ),
                                       #           fluidRow(
                                       #             column(3, sliderInput(inputId = 'families_in_poverty', label = NULL,
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'families_in_poverty_num', label = NULL,
                                       #                                    value = START_VALUE, width = '100%')),
                                       #             column(3, sliderInput(inputId = 'families_unaware_of_community', label = NULL,
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'families_unaware_of_community_num', label = NULL,
                                       #                                    value = START_VALUE)),
                                       #             column(3, sliderInput(inputId = 'drug_abuse', label = NULL,
                                       #                                   min = MIN_INPUT, max = MAX_INPUT, step = STEP_SIZE,
                                       #                                   value = START_VALUE)),
                                       #             column(1, numericInput(inputId = 'drug_abuse_num', label = NULL,
                                       #                                    value = START_VALUE))
                                       #             
                                       #           ))
                                        ######## Ending Sliders ########
                                      ))), style = "info"
             )
             ),
  
    ######### Done with Sliders ##########
  ######## Sidebar #########
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h2(textOutput('metric_focus')),
      tabsetPanel(
        id = 'data_tables',
        tabPanel('2013', DT::dataTableOutput('dt_2013')),
        tabPanel('2014', DT::dataTableOutput('dt_2014')),
        tabPanel('2015', DT::dataTableOutput('dt_2015')),
        tabPanel('2016', DT::dataTableOutput('dt_2016')),
        tabPanel('2017', DT::dataTableOutput('dt_2017')),
        tabPanel('2018', DT::dataTableOutput('dt_2018')),
        selected = '2018'
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       leafletOutput('need_map', width = '100%', height = '800px')
    )
  )
))
