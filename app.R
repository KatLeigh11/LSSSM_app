#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(meteo)
library(shiny)
library(shinythemes)
library(shinyjs)
options(shiny.trace=TRUE)
library(here)
library(janitor)
library(patchwork)
library(rnaturalearth)
library(sf)
library(rgeos)
library(tidyverse)
library(gginnards)
library(waiter)
library(data.table)
library(shinybrowser)


setwd("C:/Users/kleigh11/Documents/")

source(here('global.R'))

# Define UI for application
ui <-fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("darkly"),
               titlePanel(textOutput(outputId = "typed_name")),
  fluidRow("Settings",
           textInput(inputId = "dataset_name",
                     label = "Fishery/Dataset Name:",
                     value = "Indonesian Blue Swimming Crab",
                     placeholder = "type your dataset name here"),
           fileInput(inputId = "upload", label = "Upload Your Own Datasets", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
           radioButtons(inputId = "file_status", "Process data in uploaded file?", c("Process", "No processing needed")),
           radioButtons(inputId = "input_type","Averages or Extremes:", c("Average", "Extremes")),
           selectInput("time_period","Historical or Future:", c("Historical", "Future", "Both")),
           selectInput("aggregation","Aggregation:", c("By Decade", "Summarized")),
           checkboxGroupInput(inputId = "facet", label = "Choose faceting", choices = c("none")),
           checkboxGroupInput(inputId = "facet2", label = NULL, choices = c("life stage", "depth", "parameter")),
           downloadButton(outputId = "download_data", label = "Download Data"),
           waiter::use_waiter(),
           textOutput("button_instruct"),
           actionButton("run", "ViewPlot")
  ),
        tabsetPanel(type = "tabs",
                            tabPanel("Map",
                                     dataTableOutput('dt'),
                                     span(textOutput(outputId = "titlez"), style = "font-size:18px"),
                                     span(textOutput(outputId = "subtitlez"), style = "font-size:14px"),
                                     plotOutput(outputId = "model_map", height = 800),
                                     textOutput(outputId = "caption"),
                                     textOutput(outputId = "testText"),
                                     textOutput(outputId = "testText2"),
                                     shinybrowser::detect(),
                                     "Window size:",
                                     textOutput("size")
                                   ),
                            tabPanel("About",
                                     verbatimTextOutput(outputId = "summary")),
                            tabPanel("Examine Species Data",
                                     h4('Check out the distribution of the data used to determine the life stage, species-specific thresholds for each parameter in the model! The default data is for Indonesian Blue Swimming Crab, but you can also upload your own file, too.'),
                                     fileInput(inputId = "thresholds", label = "Upload Your Own Threshold Data", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                     plotOutput(outputId = "box_plot")),
                            tabPanel("Learn More",
                                     img(src = 'http://www.fish.wa.gov.au/PublishingImages/Swainston/crab_bl_sa.jpg'),
                                     h3("If you enjoyed this tool, check out these related resources!"),
                                     p(tags$a(href='https://www.ipcc.ch/data', 
                                              'Intergovernmental Panel on Climate Change')),
                                     
                                     p(tags$a(href='https://www.aquamaps.org/', 
                                              'AquaMaps')),
                                     
                                     p(tags$a(href='https://fisheryprogress.org/fip-profile/indonesian-blue-swimming-crab-gillnettrap-apri',
                                              'FisheryProgress- Indonesian Blue Swimming Crab')),
                                     
                                     p(tags$a(href='https://fisheries.msc.org/en/fisheries/', 
                                              'Marine Stewardship Council')),
                                     
                                     p(tags$a(href='https://www.edf.org/', 
                                              'The Environmental Defense Fund')))
      ),
               )

# Define server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize=80*1024^2) 
  
  observeEvent(list(input$facet, input$facet2), {
    if(is.null(input$facet) & is.null(input$facet2)) {
      updateCheckboxGroupInput(
        session, "facet2",
        choices = c("life stage", "depth", "parameter")
      )
    }
    else if("none" %in% input$facet & ! is.null(input$facet2)) {
      updateCheckboxGroupInput(session, "facet2",
                          choices = c("life stage", "depth", "parameter"),
                          selected = NULL)
      updateCheckboxGroupInput(session, "facet",
                               choices = "none",
                               selected = "none")
    }
    else if ("none" %in% input$facet){
      shinyjs::disable("facet2")
    }
  }, #autoDestroy = F,
   ignoreNULL = F
  )
  
  map_path_select<- reactive({
    print(input$upload)
    
    inFile <<- input$upload

    if (is.null(inFile)){
      if (input$input_type == "Average"){
        if(input$time_period == 'Historical'){
          if(input$aggregation == 'Summarized'){
            data_path <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/HISTORICAL_TIMEFRAMES/mean/historical/"}
          else if (input$aggregation == 'By Decade'){
            data_path <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/HISTORICAL_TIMEFRAMES/mean/decadal/"}}
        else if (input$time_period == 'Future'){
          if(input$aggregation == 'Summarized'){
            data_path <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/FUTURE_TIMEFRAMES/mean/future/"}
          else if (input$aggregation == 'By Decade'){
            data_path <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/FUTURE_TIMEFRAMES/mean/decadal/"}}
      else if (input$time_period == 'Both'){
        if(input$aggregation == 'Summarized'){
          data_pathF <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/FUTURE_TIMEFRAMES/mean/future/"
          data_pathH <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/HISTORICAL_TIMEFRAMES/mean/historical/"
          data_path <- list(data_pathH, data_pathF) }
        else if (input$aggregation == 'By Decade'){
          data_pathF <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/FUTURE_TIMEFRAMES/mean/decadal/"
          data_pathH <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/HISTORICAL_TIMEFRAMES/mean/decadal/"
          data_path <- list(data_pathH, data_pathF)}}}
      else if (input$input_type == "Extremes") {
        if(input$time_period == 'Historical'){
          if(input$aggregation == 'Summarized'){
            data_path <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/HISTORICAL_TIMEFRAMES/envelope/historical/"}
          else if (input$aggregation == 'By Decade'){
            data_path <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/HISTORICAL_TIMEFRAMES/envelope/decadal/"}}
        else if (input$time_period == 'Future'){
          if(input$aggregation == 'Summarized'){
            data_path <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/FUTURE_TIMEFRAMES/envelope/future/"}
          else if (input$aggregation == 'By Decade'){
            data_path <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/FUTURE_TIMEFRAMES/envelope/decadal/"}}
        else if (input$time_period == 'Both'){
          if(input$aggregation == 'Summarized'){
            data_pathF <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/FUTURE_TIMEFRAMES/envelope/future/"
            data_pathH <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/HISTORICAL_TIMEFRAMES/envelope/historical/"
            data_path <- list(data_pathH, data_pathF)}
          else if (input$aggregation == 'By Decade'){
            data_pathF <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/FUTURE_TIMEFRAMES/envelope/decadal/"
            data_pathH <- "C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/final_csvs/ready_to_plot/HISTORICAL_TIMEFRAMES/envelope/decadal/"
            data_path <- list(data_pathH, data_pathF)}}}
    }
    return(data_path)
    })
  
  data_select2 <- reactive({
    inFile <<- input$upload

    if (is.null(inFile)){
      if (all(c("life stage", "depth", "parameter") %in% input$facet2)) {
        path2 <- 'big_sf_per_lifestage_allParams.csv'
        }
      else if (all(c("parameter", "depth") %in% input$facet2)) {
        path2 <- 'big_sf_combod_lifestages_all.csv'
        }
      else if (all(c("life stage", "parameter") %in% input$facet2)) {
        path2 <- 'big_sf_per_lifestage_allParams.csv'
        }
      else if (all(c("life stage", "depth") %in% input$facet2)) {
        path2 <- 'big_sf_combod_params_all.csv'
        }
      else if (all(c("parameter") %in% input$facet2)) {
        path2 <- 'big_sf_combod_lifestages_all.csv'
        }
      else if (all(c("depth") %in% input$facet2)) {
        path2 <- 'big_sf_overall.csv'
        }
      else if (all(c("life stage") %in% input$facet2)) {
        path2 <- 'big_sf_combod_params_all.csv'
        }
      else if (input$facet == "none") {
        path2 <- 'big_sf_overall.csv'
      }}

    return(path2)
    })
  
  data <- reactive({
    inFile <<- input$upload

    if (is.null(inFile)){
      if (input$time_period == 'Both'){
      datazH <<- data.table::fread(paste0(map_path_select()[1], data_select2()), header = TRUE) %>%
        mutate(time = 'historical') %>% ungroup()
      
      datazF <<- data.table::fread(paste0(map_path_select()[2], data_select2()), header = TRUE) %>%
        mutate(time = 'future') %>% ungroup()
      
      dataz <- rbind(datazH, datazF)
      }
      else {dataz <- data.table::fread(paste0(map_path_select(), data_select2()), header = TRUE) %>% ungroup()}}
    else {
      if(nrow(inFile) >1){
        if(input$file_status == "No processing needed"){
          dataz <- paste("Error. Please only upload one file if no processing needed.")
        }
        else{ # for processing
          tmp_av <- inFile %>% 
            filter(grepl(pattern = "average", datapath))
          tmp_mxmn <- inFile %>% 
            filter(grepl(pattern = "max_min", datapath))
          dataz_av <<- data.table::fread(tmp_av$datapath, header = TRUE) %>% ungroup()
          dataz_mxmn <<- data.table::fread(tmp_mxmn$datapath, header = TRUE) %>% ungroup()
          
        }
      }
      else { # single file uploaded
        if(input$file_status == "No processing needed"){
          dataz <<- data.table::fread(inFile$datapath, header = TRUE) %>% ungroup()
        }
        else {
          tmp_av <- inFile %>% 
            filter(grepl(pattern = "average", datapath))
          tmp_mxmn <- inFile %>% 
            filter(grepl(pattern = "max_min", datapath))
          dataz_av <<- data.table::fread(tmp_av$datapath, header = TRUE) %>% ungroup()
          dataz_mxmn <<- data.table::fread(tmp_mxmn$datapath, header = TRUE) %>% ungroup()
        
        library(purrr)

        full_df <- data.table::fread(inFile$datapath, header = TRUE, col_types = "nnfnnf") %>% ungroup()
        
        full_df_minmax <- data.table::fread(inFile$datapath, header = TRUE, col_types = "nnfnnf") %>% ungroup()
        
        
        
        source2 <- function(file, start, end, ...) {
          file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='/n')
          file.lines.collapsed <- paste(file.lines, collapse='/n')
          source(textConnection(file.lines.collapsed), ...)
        }
        
        source2("C:/Users/kleigh11/Documents/BSC_EDF_model/final_files/purr version.R", start = 35, end = 1025)
        
      }
    }
    
    return(dataz)
    }
  })
  
  No_upload <- reactive({
    inFile <<- input$upload
    
    if (is.null(inFile)){
      NoUpload <- "No file uploaded"
    }
    else {
      NoUpload <- "File uploaded. Select filters and faceting to reflect your data and desired output.
      (i.e. if your data contains both historical and future values, select Both;
      if you want to depict the maximum and minimum values, select Extremes;
      and just one pair of values per time period (max/min historical, max/min future), select Summarized;
      and to not facet by depth (so each location will only have a mean(max-min pair of) value(s) for the Average(Extremes) case,
      select parameter and lifestage). *Note: processing of uploaded datasets into composite values is not yet supported
      (i.e. if you upload your own data, you must facet by at least parameter and lifestage)."    }
    return(NoUpload)
  })
  
      x <- reactive({
        system.time({
          y <- sum(rnorm(n = 1e6, mean = input$mean, sd = input$sd))
        })
        return(y)
      })

      x

     map_data_select <- reactive({
       if(input$time_period == 'Both'){
         if(input$input_type == "Average"){
           if(input$aggregation == 'Summarized'){
             if(all(c("life stage", "parameter") %in% input$facet2)) {
               if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, lifestage, parameter, time) %>%
                   summarise(value = mean(value, na.rm = T)) %>%
                   mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, lifestage, parameter, depth, time) %>%
                   summarise(value = mean(value, na.rm = T)) %>%
                               mutate_if(is.double, list(~na_if(., Inf))) %>%
                               mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
               }}
             else{
               if("life stage" %in% input$facet2 | "parameter" %in% input$facet2){
                 if(!"depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, type, time) %>%
                     summarise(value = mean(value, na.rm = T)) %>%
                     mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
                 else if("depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, depth, type, time) %>%
                     summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}}
               else if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, time) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, depth, time) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
             }}
           else if(input$aggregation == 'By Decade'){
             if(all(c("life stage", "parameter") %in% input$facet2)) {
               if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, parameter, lifestage, time) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, lifestage, parameter, depth, time) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
               }}
             else if("life stage" %in% input$facet2 | "parameter" %in% input$facet2){
                 if(!"depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, type, time, gr) %>%
                     summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
                 else if("depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, depth, type, time, gr) %>%
                     summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}}
             else if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, time, gr) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
             else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, depth, time, gr) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
             }}
         else if(input$input_type == "Extremes"){
           if(input$aggregation == 'Summarized'){
             if(all(c("life stage", "parameter") %in% input$facet2)) {
               if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, lifestage, parameter, time) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('name')) %>%
                   group_by(lat, long, lifestage, parameter, time) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, lifestage, parameter, time, depth) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('name')) %>%
                   group_by(lat, long, lifestage, parameter, time, depth) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}}
             else{
               if("life stage" %in% input$facet2 | "parameter" %in% input$facet2){
                 if(!"depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, time, type) %>%
                     summarise(value_min = min(value, na.rm = T),
                               value_max = max(value, na.rm = T)) %>%
                     pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                     dplyr::select(-contains('_'),
                            -contains('name')) %>%
                     group_by(lat, long, time, type) %>%
                     distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
                 else if("depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, depth, time, type) %>%
                     summarise(value_min = min(value, na.rm = T),
                               value_max = max(value, na.rm = T)) %>%
                     pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                     dplyr::select(-contains('_'),
                            -contains('name')) %>%
                     group_by(lat, long, time, type) %>%
                     distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
                 }}
               else if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, time) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(lat, long, time) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, depth, time) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(lat, long, depth, time) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
               }}}
           else if(input$aggregation == 'By Decade'){
             if(all(c("life stage", "parameter") %in% input$facet2)) {
               if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, lifestage, parameter, time, gr) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('name')) %>%
                   group_by(lat, long, lifestage, parameter, time, gr) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, lifestage, parameter, time, depth, gr) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('name')) %>%
                   group_by(lat, long, lifestage, parameter, time, depth, gr) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}}
             else if("life stage" %in% input$facet2 | "parameter" %in% input$facet2){
                 if(!"depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, time, type, gr) %>%
                     summarise(value_min = min(value, na.rm = T),
                               value_max = max(value, na.rm = T)) %>%
                     pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                     dplyr::select(-contains('_'),
                            -contains('name')) %>%
                     group_by(lat, long, time, type, gr) %>%
                     distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
                 else if("depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, depth, time, type, gr) %>%
                     summarise(value_min = min(value, na.rm = T),
                               value_max = max(value, na.rm = T)) %>%
                     pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                     dplyr::select(-contains('_'),
                            -contains('name')) %>%
                     group_by(lat, long, time, depth, type, gr) %>%
                     distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
                 }}
             else if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, time) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(gr, lat, long, time) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
             else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, depth, time) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(gr, lat, long, time, depth) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
               }}}}
       else if(input$time_period != 'Both'){
         if(input$input_type == "Average"){
          if(input$aggregation == 'Summarized'){
            if(all(c("life stage", "parameter") %in% input$facet2)) {
              if(!"depth" %in% input$facet2){
                tmp1 <- data() %>%
                  group_by(lat, long, lifestage, parameter) %>%
                  summarise(value = mean(value, na.rm = T))}
              else if("depth" %in% input$facet2){
                tmp1 <- data() %>%
                  group_by(lat, long, depth, lifestage, parameter) %>%
                  summarise(value = mean(value, na.rm = T))}
            }
            else if("life stage" %in% input$facet2 | "parameter" %in% input$facet2){
               if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, type) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
                 }
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, depth, type) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
                 }}
            else if(!"depth" %in% input$facet2){
               tmp1 <- data() %>%
                   group_by(lat, long) %>%
                     summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
            else if("depth" %in% input$facet2){
                tmp1 <- data() %>%
                  group_by(lat, long, depth) %>%
                  summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
            }
           else if(input$aggregation == 'By Decade'){
             if(all(c("life stage", "parameter") %in% input$facet2)) {
               if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, lifestage, parameter) %>%
                   summarise(value = mean(value, na.rm = T))}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, lifestage, parameter, depth) %>%
                   summarise(value = mean(value, na.rm = T))}
             }
             else if("life stage" %in% input$facet2 | "parameter" %in% input$facet2){
                 if(!"depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(gr, lat, long, type) %>%
                     summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
                 else if("depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(gr, lat, long, depth, type) %>%
                     summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}}
             else if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%    mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
             else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, depth) %>%
                   summarise(value = mean(value, na.rm = T)) %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
             }}
         else if(input$input_type == "Extremes"){
           if(input$aggregation == 'Summarized'){
             if(all(c("life stage", "parameter") %in% input$facet2)) {
               if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lifestage, parameter, lat, long) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(lifestage, parameter, lat, long) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, depth, lifestage, parameter) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(lat, long, depth, lifestage, parameter) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}}
               else if("life stage" %in% input$facet2 | "parameter" %in% input$facet2){
                 if(!"depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, type) %>%
                     summarise(value_min = min(value, na.rm = T),
                               value_max = max(value, na.rm = T)) %>%
                     pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                     dplyr::select(-contains('_'),
                            -contains('name')) %>%
                     group_by(lat, long, type) %>%
                     distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
                 else if("depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, depth, type) %>%
                     summarise(value_min = min(value, na.rm = T),
                               value_max = max(value, na.rm = T)) %>%
                     pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                     dplyr::select(-contains('_'),
                            -contains('name')) %>%
                     group_by(lat, long, type, depth) %>%
                     distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
                 }}
               else if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(lat, long) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(lat, long, depth) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(lat, long, depth) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}}
           else if(input$aggregation == 'By Decade'){
             if(all(c("life stage", "parameter") %in% input$facet2)) {
               if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, lifestage, parameter) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(gr, lat, long, lifestage, parameter) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
               else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, depth, lifestage, parameter) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(gr, lat, long, depth, lifestage, parameter) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}}
             else if("life stage" %in% input$facet2 | "parameter" %in% input$facet2){
                if(!"depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, type, gr) %>%
                     summarise(value_min = min(value, na.rm = T),
                               value_max = max(value, na.rm = T)) %>%
                     pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                     dplyr::select(-contains('_'),
                            -contains('name')) %>%
                     group_by(lat, long, type, gr) %>%
                     distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
                else if("depth" %in% input$facet2){
                   tmp1 <- data() %>%
                     group_by(lat, long, depth, type, gr) %>%
                     summarise(value_min = min(value, na.rm = T),
                               value_max = max(value, na.rm = T)) %>%
                     pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                     dplyr::select(-contains('_'),
                            -contains('name')) %>%
                     group_by(lat, long, type, depth, gr) %>%
                     distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                     mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
                 }}
             else if(!"depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(gr, lat, long) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()}
             else if("depth" %in% input$facet2){
                 tmp1 <- data() %>%
                   group_by(gr, lat, long, depth) %>%
                   summarise(value_min = min(value, na.rm = T),
                             value_max = max(value, na.rm = T)) %>%
                   pivot_longer(cols = c(value_min, value_max), values_to = "value") %>%
                   dplyr::select(-contains('_'),
                          -contains('name')) %>%
                   group_by(gr, lat, long, depth) %>%
                   distinct() %>% mutate_if(is.double, list(~na_if(., Inf))) %>%
                   mutate_if(is.double, list(~na_if(., -Inf))) %>% ungroup()
             }}}}
       
       return(tmp1)
    })

     facet_options <- c("depth", "time", "parameter", "lifestage", "type", "gr") # remember there's a different selection piece here for both hist or fut to filter on,
#so that's why there's 2 sections in the code below.
     
   N_gridz <- reactive({
     if(input$aggregation != 'By Decade'){ # summarized
       if(input$time_period == 'Both'){
         if (all(c("life stage", "depth", "parameter") %in% input$facet2)) {
             N_gridz <- as.numeric(uniqueN(map_data_select(), by = c("depth", "parameter", "lifestage")))
           }
         else if (all(c("parameter", "depth") %in% input$facet2)) {
             N_gridz <- as.numeric(uniqueN(map_data_select(), by = c("depth", "type")))
           }
         else if (all(c("life stage", "parameter") %in% input$facet2)) {
             N_gridz <- as.numeric(uniqueN(map_data_select(), by = c("lifestage")))
           }
         else if (all(c("life stage", "depth") %in% input$facet2)) {
             N_gridz <- as.numeric(uniqueN(map_data_select(), by = c("depth", "type")))
           }
       }
       else if(input$time_period != 'Both'){
         if (all(c("life stage", "depth", "parameter") %in% input$facet2)) {
             N_gridz <- as.numeric(uniqueN(map_data_select(), by = c("depth", "parameter", "lifestage")))
           }
         else if (all(c("parameter", "depth") %in% input$facet2)) {
             N_gridz <- as.numeric(uniqueN(map_data_select(), by = c("depth", "type")))
           }
         else if (all(c("life stage", "parameter") %in% input$facet2)) {
             N_gridz <- as.numeric(uniqueN(map_data_select(), by = c("lifestage")))
           }
         else if (all(c("life stage", "depth") %in% input$facet2)) {
             N_gridz <- as.numeric(uniqueN(map_data_select(), by = c("depth", "type")))
         }
      }}
     })
     
   map_facet_select <- reactive({
  if(input$time_period == 'Both'){
        if (all(c("life stage", "depth", "parameter") %in% input$facet2)) {
          if(input$aggregation == 'By Decade'){
                return(as.formula(paste("depth", "~", "lifestage")))
                }
          else { # summarized
            if(N_gridz()>16){
              return(as.formula(paste("time", "+", "depth", "~", "lifestage")))
            }
            else {return(as.formula(paste(facet_options[1], "+", facet_options[2], "~", facet_options[3], "+", facet_options[4])))
            }
          }}
        else if (all(c("parameter", "depth") %in% input$facet2)) {
          if(input$aggregation == 'By Decade'){
              return(as.formula(paste("type", "~", "depth")))
            }
          else { # summarized
            if(N_gridz()>16){
              return(as.formula(paste("time", "~", "depth")))
            }else {return(as.formula(paste(facet_options[2], "+", facet_options[1], "~", facet_options[5])))
            }
          }
        }
        else if (all(c("life stage", "parameter") %in% input$facet2)) {
          if(input$aggregation == 'By Decade'){
              return(as.formula(paste("parameter", "~", "lifestage")))
            }
          else { # summarized
            if(N_gridz()>16){
              return(as.formula(paste("time", "~", "parameter")))
            }else {return(as.formula(paste(facet_options[2], "+", facet_options[3], "~", facet_options[4])))
            }
          }
        }
        else if (all(c("life stage", "depth") %in% input$facet2)) {
          if(input$aggregation == 'By Decade'){
              return(as.formula(paste("type", "~", "depth")))
          }
          else { # summarized
            if(N_gridz()>16){
              return(as.formula(paste("time", "~", "depth")))
            }else {return(as.formula(paste(facet_options[2], "+", facet_options[1], "~", facet_options[5])))
            }
          }
        }
        else if (all(c("parameter") %in% input$facet2)) {
          if(input$aggregation == 'By Decade'){
              return(as.formula(paste("~", "type")))
          }
          else { # summarized
          return(as.formula(paste(facet_options[2], "~", facet_options[5])))
          }
        }
        else if (all(c("depth") %in% input$facet2)) {
          if(input$aggregation == 'By Decade'){
              return(as.formula(paste("~", "depth")))
            }
          else { # summarized
            return(as.formula(paste(facet_options[1], "~", facet_options[2])))
          }
        }
        else if (all(c("life stage") %in% input$facet2)) {
          if(input$aggregation == 'By Decade'){
              return(as.formula(paste("~", "type")))
            }
          else { # summarized
            return(as.formula(paste(facet_options[2], "~", facet_options[5])))
          }
        }
        else if (input$facet == "none") {
          if(input$aggregation == 'By Decade'){
              return(NULL)
            }
          else { # summarized
            return(as.formula(paste("~", facet_options[2])))
          }
        }
        }
  else if(input$time_period != 'Both'){
    if (all(c("life stage", "depth", "parameter") %in% input$facet2)) {
      if(input$aggregation == 'By Decade'){
          return(as.formula(paste("depth", "~", "lifestage")))
        }
      else { # summarized
        if(N_gridz()>16){
          return(as.formula(paste("depth", "~", "lifestage")))
        }else {return(as.formula(paste(facet_options[1], "~", facet_options[3], "+", facet_options[4])))
        }
      }
    }
    else if (all(c("parameter", "depth") %in% input$facet2)) {
      if(input$aggregation == 'By Decade'){
          return(as.formula(paste("type", "~", "depth")))
        }
      else { # summarized
        if(N_gridz()>16){
          return(as.formula(paste("~", "depth")))
        }else {return(as.formula(paste(facet_options[1], "~", facet_options[5])))
        }
      }
    }
    else if (all(c("life stage", "parameter") %in% input$facet2)) {
      if(input$aggregation == 'By Decade'){
          return(as.formula(paste("parameter", "~", "lifestage")))
        }
      else { # summarized
        if(N_gridz()>16){
          return(as.formula(paste("~", "parameter")))
        }else {return(as.formula(paste(facet_options[3], "~", facet_options[4])))
        }
      }
    }
    else if (all(c("life stage", "depth") %in% input$facet2)) {
      if(input$aggregation == 'By Decade'){
          return(as.formula(paste("type", "~", "depth")))
        }
      else { # summarized
        if(N_gridz()>16){
          return(as.formula(paste("~", "depth")))
        }else {return(as.formula(paste(facet_options[1], "~", facet_options[5])))
        }
      }
    }
    else if (all(c("parameter") %in% input$facet2)) {
      if(input$aggregation == 'By Decade'){
          return(as.formula(paste("~", "type")))
        }
      else { # summarized
        return(as.formula(paste("~", facet_options[5])))
      }
    }
    else if (all(c("depth") %in% input$facet2)) {
      if(input$aggregation == 'By Decade'){
          return(as.formula(paste("~", "depth")))
        }
      else { # summarized
        return(as.formula(paste("~", facet_options[1])))
      }
    }
    else if (all(c("life stage") %in% input$facet2)) {
      if(input$aggregation == 'By Decade'){
          return(as.formula(paste("~", "type")))
        }
      else { # summarized
        return(as.formula(paste("~", facet_options[5])))
      }
    }
    else if (input$facet == "none") {
      if(input$aggregation == 'By Decade'){
        return(as.formula(paste("~", facet_options[6])))
        }
      }else { # summarized
        return(NA)
      }
    }
})
     
   map_fill_lab_select <- reactive({
       if(all(c("parameter", "life stage") %in% input$facet2)){
         if(input$input_type == "Average"){
           paste('Scaled Difference from/n mean ideal value')}
         else if(input$input_type == "Extremes"){
           paste('Scaled Distance above/below/n ideal threshold')}}
       else{
         paste('Scaled Suitability Rank')}
     })
     
   mod_list <- reactive({
     if(input$time_period == 'Both'){
       if (all(c("life stage", "depth", "parameter") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr, parameter) %>% 
                      group_split())
           }
         else { # summarized
           if(N_gridz()>16){
             return(map_data_select() %>%
                      group_by(parameter) %>% 
                      group_split())
           }
         }
       }
       else if (all(c("parameter", "depth") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
           }
         else { # summarized
           if(N_gridz()>16){
             return(map_data_select() %>%
                      group_by(type) %>% 
                      group_split())
           }
         }
       }
       else if (all(c("life stage", "parameter") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
         }
         else { # summarized
           if(N_gridz()>16){
             return(map_data_select() %>%
                      group_by(lifestage) %>% 
                      group_split())
           }
         }
       }
       else if (all(c("life stage", "depth") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
           }
       else { # summarized
           if(N_gridz()>16){
             return(map_data_select() %>%
                      group_by(type) %>% 
                      group_split())
           }
         }
       }
       else if (all(c("parameter") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
           }
         }
       else if (all(c("depth") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
           }
         }
       else if (all(c("life stage") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
         }
       }
       else if (input$facet == "none") {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
         }
       }
     }
     else if(input$time_period != 'Both'){
       if (all(c("life stage", "depth", "parameter") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr, parameter) %>% 
                      group_split())
           }
         else { # summarized
           if(N_gridz()>16){
             return(map_data_select() %>%
                      group_by(parameter) %>% 
                      group_split())
           }
         }
       }
       else if (all(c("parameter", "depth") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())        }
         else { # summarized
           if(N_gridz()>16){
             return(map_data_select() %>%
                      group_by(type) %>% 
                      group_split())
           }
         }
       }
       else if (all(c("life stage", "parameter") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
           }
         else { # summarized
           if(N_gridz()>16){
             return(map_data_select() %>%
                      group_by(lifestage) %>% 
                      group_split())
           }
         }
       }
       else if (all(c("life stage", "depth") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
           }
         else { # summarized
           if(N_gridz()>16){
             return(map_data_select() %>%
                      group_by(type) %>% 
                      group_split())
           }
         }
       }
       else if (all(c("parameter") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
           }
         }
       else if (all(c("depth") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
         }
       }
       else if (all(c("life stage") %in% input$facet2)) {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
         }
       }
       else {
         if(input$aggregation == 'By Decade'){
             return(map_data_select() %>%
                      group_by(gr) %>% 
                      group_split())
         }}
     }
   }) %>%
     bindCache(input$aggregation, input$time_period, input$input_type, input$facet, input$facet2)
   
map_title_select <- reactive({
     t_srbotwaot <- "suitability rank based on the weighted average of the"
     t_ndb <- "normalized difference between"
     t_pr <- "predicted/recorded"
     t_p <- "predicted"
     t_r <- "recorded"
     t_mmv <- "maximum/minimum values"
     t_aivfbsc <-  paste0("and ideal values for ", input$dataset_name, ",")
     t_aivfbscMM <- paste0("and maximum/minimum tolerable values for ", input$dataset_name, ",")
     t_ste <- "specific to each"
     t_fa <- "for all"
     t_pd <- "per depth"
     t_pdec <- "per decade"
     t_suma <- "summarized across"
     t_sumw <- "summarized within"
     
     if (is.null(inFile)){
       t_H <- "Historical (1956 - 2009)"
       t_F <- "Future (2010 - 2100)"
     }
     else {
       t_H <- "Historical"
       t_F <- "Future"
     }
   
  if(input$input_type == "Average"){   
    if(all(c("life stage", "parameter") %in% input$facet2)){
      if("depth" %in% input$facet2){
        if(input$time_period == 'Both'){
          if(input$aggregation == 'By Decade'){
            return(paste(t_ndb, t_pr, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pd, "and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
          }
          else { #summarized
            return(paste(t_ndb, t_pr, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pd, t_sumw, 'both', t_H, "and", t_F, "time periods."))
          }
        }
         else if(input$time_period == 'Future'){
           if(input$aggregation == 'By Decade'){
            return(paste(t_ndb, t_p, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pd, "and", t_pdec, "for the", t_F, "time period."))
           }
           else { # summarized
             return(paste(t_ndb, t_p, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pd, "for the", t_F, "time period."))
           }
         }
        else if(input$time_period == 'Historical'){
          if(input$aggregation == 'By Decade'){
            return(paste(t_ndb, t_r, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pd, "and", t_pdec, "for the", t_H, "time period."))
          }
          else { # summarized
            return(paste(t_ndb, t_r, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pd, "for the", t_H, "time period."))
          }
        }
      }
       else { # no depth
         if(input$time_period == 'Both'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_ndb, t_pr, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pdec, "for both", t_H, "and", t_F, "time periods."))
           }
           else { #summarized
             return(paste(t_ndb, t_pr, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_sumw, 'both', t_H, "and", t_F, "time periods."))
           }
         }
         else if(input$time_period == 'Future'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_ndb, t_p, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pdec, "for the", t_F, "time period."))
           }
           else { # summarized
             return(paste(t_ndb, t_p, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", "for the", t_F, "time period."))
           }
         }
         else if(input$time_period == 'Historical'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_ndb, t_r, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", t_pdec, "for the", t_H, "time period."))
           }
           else { # summarized
             return(paste(t_ndb, t_r, "values", t_aivfbsc, t_ste, "parameter", "and", "life stage", "for the", t_H, "time period."))
           }
         }
       }
    }
     else if("life stage" %in% input$facet2){
       if("depth" %in% input$facet2){
         if(input$time_period == 'Both'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage,", t_pd, ", and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
           }
           else { #summarized
             return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "and", t_pd, "for both", t_H, "and", t_F, "time periods."))
           }
         }
         else if(input$time_period == 'Future'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", t_pdec, "and", t_pd, "for the", t_F, "time period."))
           }
           else { # summarized
             return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "and", t_pd, "for the", t_F, "time period."))
           }
         }
         else if(input$time_period == 'Historical'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", t_pdec, "and", t_pd, "for the", t_H, "time period."))
           }
           else { # summarized
             return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "and", t_pd, "for the", t_H, "time period."))
           }
         }
       }
       else { # no depth
         if(input$time_period == 'Both'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
           }
           else { #summarized
             return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "for both", t_H, "and", t_F, "time periods."))
           }
         }
         else if(input$time_period == 'Future'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "and", t_pdec, "for the", t_F, "time period."))
           }
           else { # summarized
             return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "for the", t_F, "time period."))
           }
         }
         else if(input$time_period == 'Historical'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "and", t_pdec, "for the", t_H, "time period."))
           }
           else { # summarized
             return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "parameters", "per", "life stage", "for the", t_H, "time period."))
           }
         }
       }
     }
     else if("parameter" %in% input$facet2){
         if("depth" %in% input$facet2){
           if(input$time_period == 'Both'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter,", t_pd, ", and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
             }
             else { #summarized
               return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "and", t_pd, "for both", t_H, "and", t_F, "time periods."))
             }
           }
           else if(input$time_period == 'Future'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", t_pdec, "and", t_pd, "for the", t_F, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "and", t_pd, "for the", t_F, "time period."))
             }
           }
           else if(input$time_period == 'Historical'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", t_pdec, "and", t_pd, "for the", t_H, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "and", t_pd, "for the", t_H, "time period."))
             }
           }
         }
         else { # no depth
           if(input$time_period == 'Both'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
             }
             else { #summarized
               return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "for both", t_H, "and", t_F, "time periods."))
             }
           }
           else if(input$time_period == 'Future'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "and", t_pdec, "for the", t_F, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "for the", t_F, "time period."))
             }
           }
           else if(input$time_period == 'Historical'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "and", t_pdec, "for the", t_H, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "life stages", "per", "parameter", "for the", t_H, "time period."))
             }
           }
         }
     }
     else if("depth" %in% input$facet2){
       if(input$time_period == 'Both'){
         if(input$aggregation == 'By Decade'){
           return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "life stages and parameters,", t_pd, ", and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
         }
         else { #summarized
           return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "life stages and parameters,", t_pd, "for both", t_H, "and", t_F, "time periods."))
         }
       }
       else if(input$time_period == 'Future'){
         if(input$aggregation == 'By Decade'){
           return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "life stages and parameters,", t_pdec, "and", t_pd, "for the", t_F, "time period."))
         }
         else { # summarized
           return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "life stages and parameters,", t_pd, "for the", t_F, "time period."))
         }
       }
       else if(input$time_period == 'Historical'){
         if(input$aggregation == 'By Decade'){
           return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "life stages and parameters,", t_pdec, "and", t_pd, "for the", t_H, "time period."))
         }
         else { # summarized
           return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "life stages and parameters,", t_pd, "for the", t_H, "time period."))
         }
       }
     }
     else if("none" %in% input$facet){
       if(input$time_period == 'Both'){
         if(input$aggregation == 'By Decade'){
           return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "life stages, parameters, and depths,", t_pdec, "for both", t_H, "and", t_F, "time periods."))
         }
         else { #summarized
           return(paste(t_srbotwaot, t_ndb, t_pr, "values", t_aivfbsc, t_fa, "life stages, parameters, and depths", "for both", t_H, "and", t_F, "time periods."))
         }
       }
       else if(input$time_period == 'Future'){
         if(input$aggregation == 'By Decade'){
           return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "life stages, parameters, and depths,", t_pdec, "for the", t_F, "time period."))
         }
         else { # summarized
           return(paste(t_srbotwaot, t_ndb, t_p, "values", t_aivfbsc, t_fa, "life stages, parameters, and depths", "for the", t_F, "time period."))
         }
       }
       else if(input$time_period == 'Historical'){
         if(input$aggregation == 'By Decade'){
           return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "life stages, parameters, and depths,", t_pdec, "for the", t_H, "time period."))
         }
         else { # summarized
           return(paste(t_srbotwaot, t_ndb, t_r, "values", t_aivfbsc, t_fa, "life stages, parameters, and depths", "for the", t_H, "time period."))
         }
       }
     }
    }
     else if (input$input_type == "Extremes"){
       if(all(c("life stage", "parameter") %in% input$facet2)){
         if("depth" %in% input$facet2){
           if(input$time_period == 'Both'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_ndb, t_pr, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pd, "and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
             }
             else { #summarized
               return(paste(t_ndb, t_pr, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pd, t_sumw, 'both', t_H, "and", t_F, "time periods."))
             }
           }
           else if(input$time_period == 'Future'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_ndb, t_p, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pd, "and", t_pdec, "for the", t_F, "time period."))
             }
             else { # summarized
               return(paste(t_ndb, t_p, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pd, "for the", t_F, "time period."))
             }
           }
           else if(input$time_period == 'Historical'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_ndb, t_r, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pd, "and", t_pdec, "for the", t_H, "time period."))
             }
             else { # summarized
               return(paste(t_ndb, t_r, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pd, "for the", t_H, "time period."))
             }
           }
         }
         else { # no depth
           if(input$time_period == 'Both'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_ndb, t_pr, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pdec, "for both", t_H, "and", t_F, "time periods."))
             }
             else { #summarized
               return(paste(t_ndb, t_pr, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_sumw, 'both', t_H, "and", t_F, "time periods."))
             }
           }
           else if(input$time_period == 'Future'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_ndb, t_p, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pdec, "for the", t_F, "time period."))
             }
             else { # summarized
               return(paste(t_ndb, t_p, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", "for the", t_F, "time period."))
             }
           }
           else if(input$time_period == 'Historical'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_ndb, t_r, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", t_pdec, "for the", t_H, "time period."))
             }
             else { # summarized
               return(paste(t_ndb, t_r, t_mmv, t_aivfbscMM, t_ste, "parameter", "and", "life stage", "for the", t_H, "time period."))
             }
           }
         }
       }
       else if("life stage" %in% input$facet2){
         if("depth" %in% input$facet2){
           if(input$time_period == 'Both'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage,", t_pd, ", and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
             }
             else { #summarized
               return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "and", t_pd, "for both", t_H, "and", t_F, "time periods."))
             }
           }
           else if(input$time_period == 'Future'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", t_pdec, "and", t_pd, "for the", t_F, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "and", t_pd, "for the", t_F, "time period."))
             }
           }
           else if(input$time_period == 'Historical'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", t_pdec, "and", t_pd, "for the", t_H, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "and", t_pd, "for the", t_H, "time period."))
             }
           }
         }
         else { # no depth
           if(input$time_period == 'Both'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
             }
             else { #summarized
               return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "for both", t_H, "and", t_F, "time periods."))
             }
           }
           else if(input$time_period == 'Future'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "and", t_pdec, "for the", t_F, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "for the", t_F, "time period."))
             }
           }
           else if(input$time_period == 'Historical'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "and", t_pdec, "for the", t_H, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "parameters", "per", "life stage", "for the", t_H, "time period."))
             }
           }
         }
       }
       else if("parameter" %in% input$facet2){
         if("depth" %in% input$facet2){
           if(input$time_period == 'Both'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter,", t_pd, ", and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
             }
             else { #summarized
               return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "and", t_pd, "for both", t_H, "and", t_F, "time periods."))
             }
           }
           else if(input$time_period == 'Future'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", t_pdec, "and", t_pd, "for the", t_F, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "and", t_pd, "for the", t_F, "time period."))
             }
           }
           else if(input$time_period == 'Historical'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", t_pdec, "and", t_pd, "for the", t_H, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "and", t_pd, "for the", t_H, "time period."))
             }
           }
         }
         else { # no depth
           if(input$time_period == 'Both'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
             }
             else { #summarized
               return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "for both", t_H, "and", t_F, "time periods."))
             }
           }
           else if(input$time_period == 'Future'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "and", t_pdec, "for the", t_F, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "for the", t_F, "time period."))
             }
           }
           else if(input$time_period == 'Historical'){
             if(input$aggregation == 'By Decade'){
               return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "and", t_pdec, "for the", t_H, "time period."))
             }
             else { # summarized
               return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "life stages", "per", "parameter", "for the", t_H, "time period."))
             }
           }
         }
       }
       else if("depth" %in% input$facet2){
         if(input$time_period == 'Both'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "life stages and parameters,", t_pd, ", and", t_pdec, "for both", t_H, "and", t_F, "time periods."))
           }
           else { #summarized
             return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "life stages and parameters,", t_pd, "for both", t_H, "and", t_F, "time periods."))
           }
         }
         else if(input$time_period == 'Future'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "life stages and parameters,", t_pdec, "and", t_pd, "for the", t_F, "time period."))
           }
           else { # summarized
             return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "life stages and parameters,", t_pd, "for the", t_F, "time period."))
           }
         }
         else if(input$time_period == 'Historical'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "life stages and parameters,", t_pdec, "and", t_pd, "for the", t_H, "time period."))
           }
           else { # summarized
             return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "life stages and parameters,", t_pd, "for the", t_H, "time period."))
           }
         }
       }
       else if("none" %in% input$facet){
         if(input$time_period == 'Both'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "life stages, parameters, and depths,", t_pdec, "for both", t_H, "and", t_F, "time periods."))
           }
           else { #summarized
             return(paste(t_srbotwaot, t_ndb, t_pr, t_mmv, t_aivfbscMM, t_fa, "life stages, parameters, and depths", "for both", t_H, "and", t_F, "time periods."))
           }
         }
         else if(input$time_period == 'Future'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "life stages, parameters, and depths,", t_pdec, "for the", t_F, "time period."))
           }
           else { # summarized
             return(paste(t_srbotwaot, t_ndb, t_p, t_mmv, t_aivfbscMM, t_fa, "life stages, parameters, and depths", "for the", t_F, "time period."))
           }
         }
         else if(input$time_period == 'Historical'){
           if(input$aggregation == 'By Decade'){
             return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "life stages, parameters, and depths,", t_pdec, "for the", t_H, "time period."))
           }
           else { # summarized
             return(paste(t_srbotwaot, t_ndb, t_r, t_mmv, t_aivfbscMM, t_fa, "life stages, parameters, and depths", "for the", t_H, "time period."))
           }
         }
       }
     }
    })
   
map_cap_select <- reactive({
  
  if (is.null(inFile)){
    t_H <- "Historical (1956 - 2009)"
    t_F <- "Future (2010 - 2100)"
  }
  else {
    t_H <- "Historical"
    t_F <- "Future"
  }
  
  c_tdb <- "The differences between"
  c_rp <- "predicted/recorded"
  c_va <- "values and"
  c_mm <- "maximum/minimum"
  c_1f <- paste0("values specific to each ", input$dataset_name, " life stage for each parameter were calculated for gridded points located at a 0.5 degree resolution within a bounding box of -23.0°, 24.5° latitude; and 83.0°, 153.5° longitude. These differences were caluclated for four parameters (temperature, salinity, pH, and dissolved oxygen), five life stages (egg, zoea, megalopae, juvenille, and adult), and four depth categories (surface, 30m, 100m, and bottom).")
  c_scale1 <- "The differences were then normalized across time periods, parameters, life stages, and depths; and scaled to range from -1 to 1, with negative values indicating the"
  c_2f <- "value falls below the"
  c_3f <- "value, and positive values indicating the"
  c_4f <- "value falls above"
  c_Norm <- "Normalization and scaling were conducted to facilitate comparisons. Note visualizations use standard interpolation to produce smooth transitions across a continuous color scale."

  c_tsa <- "To summarize across all"
  c_fab <- ", first the absolute value of the differences was taken, then these absolute values were multiplied by their respective"
  c_parmw <- "parameter weight ()"
  c_lsw <- "life stage weight (all = 1)"
  c_atwv <- "and these weighted values were summed across all"
  c_dtl <- "depth, time period, and location. The summed weighted values were then normalized across"
  c_tpdr <- paste0(", time periods, ", t_H, " and ", t_F, ", and depths; and scaled to range from 0 to 1.")
  
  c_no_dep_tpr <- paste0(", and time periods", t_H, " and ", t_F, " . These differences were averaged for all depths, and then normalized and scaled to range from 0 to 1.")
  
  c_no_dep_only <- "These differences were averaged for all depths, and then normalized and scaled to range from 0 to 1."
  
  c_sum_dep <- "To summarize across depths, the average difference was taken, and then normalization and scaling was applied to range from 0 to 1."
  
  c_sum_pls1 <- "To summarize across all parameters and life stages"
  c_sum_pls2 <- ", and these weighted values were summed for each location, depth,"
  c_sum_pls3 <- "and time period. The summed weighted values were then normalized; and scaled to range from 0 to 1."
  
  if(input$input_type == "Average"){   
    if(all(c("life stage", "parameter") %in% input$facet2)){
      if("depth" %in% input$facet2){
        if(input$time_period == 'Both'){
            return(paste(c_tdb, c_rp, c_va, "ideal", c_1f, c_scale1, c_rp, c_2f, "ideal", c_3f, c_rp, c_4f, ".", c_Norm))
          }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, "predicted", c_va, "ideal", c_1f, c_scale1, "predicted", c_2f, "ideal", c_3f, "predicted", c_4f, ".", c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, "recorded", c_va, "ideal", c_1f, c_scale1, "recorded", c_2f, "ideal", c_3f, "recorded", c_4f, ".", c_Norm))
        }
      }
      else { # no depth
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_rp, c_va, "ideal", c_1f, c_no_dep_only, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, "predicted", c_va, "ideal", c_1f, c_no_dep_only, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, "recorded", c_va, "ideal", c_1f, c_no_dep_only, c_Norm))
        }
      }
    }
    else if("life stage" %in% input$facet2){
      if("depth" %in% input$facet2){
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_rp, c_va, "ideal",  c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_tpdr, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, "predicted", c_va, "ideal", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_tpdr, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, "recorded", c_va, "ideal", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_tpdr, c_Norm))
        }
      }
      else { # no depth
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_rp, c_va, "ideal", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_no_dep_tpr, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, "predicted", c_va, "ideal", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_no_dep_tpr, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, "recorded", c_va, "ideal", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_no_dep_tpr, c_Norm))
        }
      }
    }
    else if("parameter" %in% input$facet2){
      if("depth" %in% input$facet2){
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_rp, c_va, "ideal", c_1f, c_tsa, "lifestages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_tpdr, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, "predicted", c_va, "ideal", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_tpdr, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, "recorded", c_va, "ideal", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_tpdr, c_Norm))
        }
      }
      else { # no depth
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_rp, c_va, "ideal", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_no_dep_tpr, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, "predicted", c_va, "ideal", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_no_dep_tpr, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, "recorded", c_va, "ideal", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_no_dep_tpr, c_Norm))
        }
      }
    }
    else if("depth" %in% input$facet2){
      if(input$time_period == 'Both'){
        return(paste(c_tdb, c_rp, c_va, "ideal", c_1f, c_no_dep_only, c_Norm))
      }
      else if(input$time_period == 'Future'){
        return(paste(c_tdb, "predicted", c_va, "ideal", c_1f, c_no_dep_only, c_Norm))
      }
      else if(input$time_period == 'Historical'){
        return(paste(c_tdb, "recorded", c_va, "ideal", c_1f, c_no_dep_only, c_Norm))
      }
    }
    else if("none" %in% input$facet){
      if(input$time_period == 'Both'){
        return(paste(c_tdb, c_rp, c_va, "ideal", c_1f, c_sum_pls1, c_fab, c_parmw, "and", c_lsw, c_sum_pls2, c_sum_pls3, c_Norm))
      }
      else if(input$time_period == 'Future'){
        return(paste(c_tdb, "predicted", c_va, "ideal", c_1f, c_sum_pls1, c_fab, c_parmw, "and", c_lsw, c_sum_pls2, c_sum_pls3, c_Norm))
      }
      else if(input$time_period == 'Historical'){
        return(paste(c_tdb, "recorded", c_va, "ideal", c_1f, c_sum_pls1, c_fab, c_parmw, "and", c_lsw, c_sum_pls2, c_sum_pls3, c_Norm))
      }
    }
  }
  else if (input$input_type == "Extremes"){
    if(all(c("life stage", "parameter") %in% input$facet2)){
      if("depth" %in% input$facet2){
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_scale1, c_rp, c_2f, "minimum tolerable", c_3f, c_rp, c_4f, "the maximum tolerable value.", c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, c_mm, "predicted", c_va, c_mm, "tolerable", c_1f, c_scale1, "predicted", c_2f, "minimum tolerable", c_3f, "predicted", c_4f, "the maximum tolerable value.", c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, c_mm, "recorded", c_va, c_mm, "tolerable", c_1f, c_scale1, "recorded", c_2f, "minimum tolerable", c_3f, "recorded", c_4f, "the maximum tolerable value.", c_Norm))
        }
      }
      else { # no depth
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_no_dep_only, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, c_mm, "predicted", c_va, c_mm, "tolerable", c_1f, c_no_dep_only, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, c_mm, "recorded", c_va, c_mm, "tolerable", c_1f, c_no_dep_only, c_Norm))
        }
      }
    }
    else if("life stage" %in% input$facet2){
      if("depth" %in% input$facet2){
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_tpdr, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, c_mm, "predicted", c_va, c_mm, "tolerable", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_tpdr, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, c_mm, "recorded", c_va, c_mm, "tolerable", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_tpdr, c_Norm))
        }
      }
      else { # no depth
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_no_dep_tpr, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, c_mm, "predicted", c_va, c_mm, "tolerable", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_no_dep_tpr, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, c_mm, "recorded", c_va, c_mm, "tolerable", c_1f, c_tsa, "parameters", c_fab, c_parmw, "," , c_atwv, "parameters for each life stage,", c_dtl, "life stages", c_no_dep_tpr, c_Norm))
        }
      }
    }
    else if("parameter" %in% input$facet2){
      if("depth" %in% input$facet2){
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_tsa, "life stage", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_tpdr, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, c_mm, "predicted", c_va, c_mm, "tolerable", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_tpdr, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, c_mm, "recorded", c_va, c_mm, "tolerable", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_tpdr, c_Norm))
        }
      }
      else { # no depth
        if(input$time_period == 'Both'){
          return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_no_dep_tpr, c_Norm))
        }
        else if(input$time_period == 'Future'){
          return(paste(c_tdb, c_mm, "predicted", c_va, c_mm, "tolerable", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_no_dep_tpr, c_Norm))
        }
        else if(input$time_period == 'Historical'){
          return(paste(c_tdb, c_mm, "recorded", c_va, c_mm, "tolerable", c_1f, c_tsa, "life stages", c_fab, c_parmw, "," , c_atwv, "life stages for each parameter,", c_dtl, "parameters", c_no_dep_tpr, c_Norm))
        }
      }
    }
    else if("depth" %in% input$facet2){
      if(input$time_period == 'Both'){
        return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_no_dep_only, c_Norm))
      }
      else if(input$time_period == 'Future'){
        return(paste(c_tdb, c_mm, "predicted", c_va, c_mm, "tolerable", c_1f, c_no_dep_only, c_Norm))
      }
      else if(input$time_period == 'Historical'){
        return(paste(c_tdb, c_mm, "recorded", c_va, c_mm, "tolerable", c_1f, c_no_dep_only, c_Norm))
      }
    }
    else if("none" %in% input$facet){
      if(input$time_period == 'Both'){
        return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_sum_pls1, c_fab, c_parmw, "and", c_lsw, c_sum_pls2, c_sum_pls3, c_Norm))
      }
      else if(input$time_period == 'Future'){
        return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_sum_pls1, c_fab, c_parmw, "and", c_lsw, c_sum_pls2, c_sum_pls3, c_Norm))
      }
      else if(input$time_period == 'Historical'){
        return(paste(c_tdb, c_mm, c_rp, c_va, c_mm, "tolerable", c_1f, c_sum_pls1, c_fab, c_parmw, "and", c_lsw, c_sum_pls2, c_sum_pls3, c_Norm))
      }
    }
  }
})  
   
plotzR <- reactive({
  plotz <- c()
  mod_lst <<- mod_list()
  
  if(length(mod_lst)>0){
    
    for(i in 1:length(mod_lst)){
    xlabs = seq(floor(min(mod_lst[[i]]$long, na.rm = T)),ceiling(max(mod_lst[[i]]$long, na.rm = T)),10)
    ylabs1 = seq(floor(-min(mod_lst[[i]]$lat, na.rm = T)),10,-10)
    ylabs2 = seq(10,ceiling(max(mod_lst[[i]]$lat, na.rm = T)),10)
    
    ylabzS <- paste0(ylabs1, '°S')
    ylabzN <- paste0(ylabs2, '°N')
    ylabz <- c(ylabzS, '0°', ylabzN)
    
         if(all(c("parameter", "life stage") %in% input$facet2)){
             plotz[[i]] <- ggplot() +
               geom_raster(data= mod_lst[[i]], aes(x=long, y=lat, fill = value), interpolate = T)+
               scale_fill_viridis_c(guide = guide_colourbar(direction = "horizontal",
                                                            barheight = 2,
                                                            barwidth = 30),
                                    na.value = "transparent",
                                    limits = c(-1,1)) +
               scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E'))+
               scale_y_continuous(breaks = c(-ylabs1, 0, ylabs2), labels = ylabz)+
               theme(panel.spacing = unit(0, "lines"), legend.key.height = unit(0, "lines"),
                     legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "lines"),
                     plot.title= element_text(size=20),
                     plot.subtitle = element_text(size=15))+
               labs(x = "Longitude", y= "Latitude", fill = map_fill_lab_select()) +
               facet_wrap(map_facet_select(), ncol = uniqueN(mod_lst[[i]]$lifestage)) +
               geom_sf(data = crop_sea50)}
         else if(!is.null(input$facet2)) {
                 plotz[[i]] <- ggplot() +
                   geom_raster(data= mod_lst[[i]], aes(x=long, y=lat, fill = value), interpolate = T)+
                   scale_fill_viridis_c(guide = guide_colourbar(direction = "horizontal",
                                                                barheight = 2,
                                                                barwidth = 30),
                                        na.value = "transparent",
                                        option = "magma",
                                        limits = c(0,1)
                                        ) +
                   scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E'))+
                   scale_y_continuous(breaks = c(-ylabs1, 0, ylabs2), labels = ylabz)+
                   theme(panel.spacing = unit(0, "lines"), legend.key.height = unit(0, "lines"),
                         legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "lines"),
                         plot.title= element_text(size=20),
                         plot.subtitle = element_text(size=15))+
                   labs(x = "Longitude", y= "Latitude", fill = map_fill_lab_select()) +
                   facet_wrap(map_facet_select(), ncol = if("depth" %in% input$facet2){
                     uniqueN(mod_lst[[i]]$depth)})+
                   geom_sf(data = crop_sea50)}
         else {
               plotz[[i]] <- ggplot() +
                 geom_raster(data = mod_lst[[i]], aes(x=long, y=lat, fill = value), interpolate = T)+
                 scale_fill_viridis_c(guide = guide_colourbar(direction = "horizontal",
                                                              barheight = 2,
                                                              barwidth = 30),
                                      na.value = "transparent",
                                      option = "magma",
                                      limits = c(0,1)
                                      ) +
                 scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E'))+
                 scale_y_continuous(breaks = c(-ylabs1, 0, ylabs2), labels = ylabz)+
                 theme(panel.spacing = unit(0, "lines"), legend.key.height = unit(1, "lines"),
                       legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "lines"),
                       plot.title= element_text(size=20),
                       plot.subtitle = element_text(size=15))+
                 labs(x = "Longitude", y= "Latitude", fill = map_fill_lab_select()) +
                 geom_sf(data = crop_sea50)}
    }
    return(plotz)
  }
  else{
    xlabs = seq(floor(min(map_data_select()$long, na.rm = T)),ceiling(max(map_data_select()$long, na.rm = T)),10)
    ylabs1 = seq(floor(-min(map_data_select()$lat, na.rm = T)),10,-10)
    ylabs2 = seq(10,ceiling(max(map_data_select()$lat, na.rm = T)),10)
    
    ylabzS <- paste0(ylabs1, '°S')
    ylabzN <- paste0(ylabs2, '°N')
    ylabz <- c(ylabzS, '0°', ylabzN)
    
            if(all(c("parameter", "life stage") %in% input$facet2)){
              plotz[[1]] <- ggplot() +
                   geom_raster(data= map_data_select(), aes(x=long, y=lat, fill = value), interpolate = T)+
                   scale_fill_viridis_c(guide = guide_colourbar(direction = "horizontal",
                                                                barheight = 2,
                                                                barwidth = 30),
                                        na.value = "transparent",
                                        limits = c(-1,1)) +
                   theme(panel.spacing = unit(0, "lines"), legend.key.height = unit(0, "lines"),
                         legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "lines"),
                         plot.title= element_text(size=20))+
                scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E'))+
                scale_y_continuous(breaks = c(-ylabs1, 0, ylabs2), labels = ylabz)+
                   labs(x = "Longitude", y= "Latitude", fill = map_fill_lab_select()) +
                   facet_wrap(map_facet_select(), ncol = uniqueN(map_data_select()$lifestage)
                   )+
                   geom_sf(data = crop_sea50)
                 return(plotz)
            }
            else if(!is.null(input$facet2)) {
              plotz[[1]] <- ggplot() +
                geom_raster(data= map_data_select(), aes(x=long, y=lat, fill = value), interpolate = T)+
                scale_fill_viridis_c(guide = guide_colourbar(direction = "horizontal",
                                                             barheight = 2,
                                                             barwidth = 30),
                                     na.value = "transparent",
                                     option = "magma",
                                     limits = c(0,1)
                                     ) +
                theme(panel.spacing = unit(0, "lines"), legend.key.height = unit(0, "lines"),
                      legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "lines"),
                      plot.title= element_text(size=20))+
                scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E'))+
                scale_y_continuous(breaks = c(-ylabs1, 0, ylabs2), labels = ylabz)+
                labs(x = "Longitude", y= "Latitude", fill = map_fill_lab_select()) +
                facet_wrap(map_facet_select(), ncol = if(all(c("depth") %in% input$facet2)){uniqueN(map_data_select()$depth)}
                           else {uniqueN(map_data_select()$type)}
                )+
                geom_sf(data = crop_sea50)
              return(plotz)
            }
            else if(is.null(input$facet2)) {
      plotz[[1]] <- ggplot() +
        geom_raster(data= map_data_select(), aes(x=long, y=lat, fill = value), interpolate = T)+
        scale_fill_viridis_c(guide = guide_colourbar(direction = "horizontal",
                                                     barheight = 2,
                                                     barwidth = 30),
                             na.value = "transparent",
                             option = "magma",
                             limits = c(0,1)
                             ) +
        theme(panel.spacing = unit(0, "lines"), legend.key.height = unit(0, "lines"),
              legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "lines"),
              plot.title= element_text(size=20))+
        scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E'))+
        scale_y_continuous(breaks = c(-ylabs1, 0, ylabs2), labels = ylabz)+
        labs(x = "Longitude", y= "Latitude", fill = map_fill_lab_select()) +
        facet_wrap(map_facet_select())+
        geom_sf(data = crop_sea50)
      return(plotz)
    }
            else {
              plotz[[1]] <- ggplot() +
                   geom_raster(data = map_data_select(), aes(x=long, y=lat, fill = value), interpolate = T)+
                   scale_fill_viridis_c(guide = guide_colourbar(direction = "horizontal",
                                                                barheight = 2,
                                                                barwidth = 30),
                                        na.value = "transparent",
                                        option = "magma",
                                        limits = c(0,1)
                                        ) +
                   theme(panel.spacing = unit(0, "lines"), legend.key.height = unit(1, "lines"),
                         legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "lines"),
                         plot.title= element_text(size=20))+
                facet_wrap(map_facet_select())+
                scale_x_continuous(breaks = xlabs, labels = paste0(xlabs,'°E'))+
                scale_y_continuous(breaks = c(-ylabs1, 0, ylabs2), labels = ylabz)+
                   labs(x = "Longitude", y= "Latitude", fill = map_fill_lab_select()) +
                   geom_sf(data = crop_sea50)
                 return(plotz)
               }
             }
}) %>%
  bindCache(input$aggregation, input$time_period, input$input_type, input$facet, input$facet2)  
  
subtitlezR <- reactive({
  subz <- c()
  
  if(length(mod_list())>0){
    for(i in 1:length(mod_list())){
      if(all(c("parameter", "life stage") %in% input$facet2)){
        if (input$aggregation == 'By Decade'){
          subz[[i]] <- paste0('Depicted year: ', mod_list()[[i]]$gr[1], '.  Click "View Plot" to show next year.')}
        else{
          subz[[i]] <- paste0('Depicted parameter: ', mod_list()[[i]]$parameter[1], '.  Click "View Plot" to show next parameter')}
      }
      else if(!is.null(input$facet2)) {
        if (input$aggregation == 'By Decade'){
          subz[[i]] <- paste0('Depicted Year: ', mod_list()[[i]]$gr[1], '.  Click "View Plot" to show next year.')}
        else if ("life stage" %in% input$facet2){
          subz[[i]] <- paste0('Depicted life stage: ', mod_list()[[i]]$type[1], '.  Click "View Plot" to show next life stage.')
        }
        else{NULL}
      }
      else {
        if (input$aggregation == 'By Decade'){
          subz[[i]] <- paste0('Depicted year: ', mod_list()[[i]]$gr[1], '.  Click "View Plot" to show next year')}
        else{NULL}
      }
    }
  }
  else {NULL}
  return(subz)
})

  output$dt <- renderDataTable({
    if(length(mod_list)>0){
      req(mod_list()[[1]])
    }else {
      req(map_data_select())
    }
  })
  output$button_instruct <- renderText("Click button to display plot. Note that multiple plot pages will be produced if several faceting options are selected. To view the next page of plots, click the button again.")

  count <- 0   # This is the counter which keeps track on button count
  
  observeEvent(input$run, {
    waiter::Waiter$new(id = "model_map")$show()
    if(length(mod_list())>0){
      if(input$run <= length(mod_list())){
        output$model_map <- renderPlot({
          plotzR()[[input$run]]}
          )
        output$subtitlez <- renderText({subtitlezR()[[input$run]]})
      }
      else if (input$run <= 2*length(mod_list())){
        output$model_map <- renderPlot({
          plotzR()[[input$run-length(mod_list())]]}
        )
        output$subtitlez <- renderText({subtitlezR()[[input$run-length(mod_list())]]})
      }
      else {
        output$model_map <- renderPlot({
          plotzR()[[1]]}
        )
        output$subtitlez <- renderText({subtitlezR()[[1]]})
        }
    } else{
      if(any(input$run)){
        output$model_map <- renderPlot({plotzR()[[1]]}
        )
        output$subtitlez <- renderText({subtitlezR()[[1]]})
        }
      }
  })
  
  output$size <- renderText({
    paste(
      shinybrowser::get_width(),
      "x",
      shinybrowser::get_height()
    )
  })
  
    output$testText2 <- renderPrint({paste(
      "plotzR", length(plotzR()),
      "mapdataselect", length(map_data_select()),
      "subtitlezR", length(subtitlezR()),
      # "modlist", typeof(mod_list()),
      # "modlist", class(mod_list()),
      "modlist", length(mod_list()),
      "mapfacetselect", map_facet_select(),
      "inputfacet2", input$facet2,
      "Ngridz16", N_gridz()>16, sep = "...")})
    
  output$titlez <- renderText({str_wrap(tools::toTitleCase(tolower(map_title_select())), 95)})
    
  output$caption <- renderText({map_cap_select()})
  output$typed_name <- renderText({
    paste('A Life Stage-specific Climate Model for ', input$dataset_name)
  })
  
  # Downloadable csv of selected dataset ----
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$dataset_name, input$version, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(map_data_select(), file, row.names = FALSE)
    })
  
  # Store in a convenience variable
  #cdata <- session$clientData
  
  # Values from cdata returned as text
  #output$clientdataText <- renderText({
  #   cnames <- names(cdata)
  #   
  #   allvalues <- lapply(cnames, function(name) {
  #     paste(name, cdata[[name]], sep = " = ")
  #   })
  #   paste(allvalues, collapse = "/n")
  # })

#   # Threshold Tab
#   
#   reactive({
#     inFile2 <- input$thresholds
#     
#     if (is.null(inFile2)){
#       thresh_distrib <- thresh_distrib
#     }
#     else {
#       thresh_distrib <- data.table::fread(inFile2$datapath, header = TRUE)
#     }  
#   }) %>%
#     bindCache(input$thresholds)
#   
#   
#   # plot the boxplot in threshold tab
#   
#   output$box_plot <- renderPlot({
#     
#     facet.labs <- c("Dissolved Oxygen (mg/L)", "pH", "Salinity (ppt)", "Temperature (deg C)")
#     names(facet.labs) <- c("DO", "pH", "salinity", "temp")
#     
#     boxplot1 <- longer_thresh %>% 
#       filter(!is.na(value)) %>%
#       ggplot(aes(x= lifestage, y = value, na.rm = TRUE))+
#       geom_point(color = "skyblue2", size = 2.5)+
#       facet_wrap(~threasholds, scales = "free", labeller = labeller(threasholds = facet.labs)) +
#       geom_boxplot(fill = "transparent",
#                    color = "grey17") + labs(
#                      title <- "Distribution of Values Reported in Blue Swimming Crab Literature",
#                      x = "Life Stage",
#                      y = "Value")
#     
#     boxplot1
#   })
#   
#   # About Tab
#   output$summary <- renderText({
#     
#     "This app is designed to produce maps of suitable conditions specific to a chosen fishery. It works
#     by taking in csv files that contain:
#     A) observations of aquatic parameters (i.e. temperature, salinity, dissolved oxygen, pH, etc.)
#     over space and time, and
#     B) tolerable thresholds of these parameters for each life stage of the species of interest
#     (i.e. adult crabs need temperatures between 17 and 24 degrees Celcius (hypothetical example)).
# The difference between these thresholds and the observation data is then found and normalized to produce
# color-coded maps of regions more or less conducive to the species of interest. To combine multiple
# parameters or lifestages, a weighted sum is obtained by applying parameter-specific and life stage
# -specific coefficients to the normalized differences and then summing these values together to obtain
# a rank.
# 
# You can upload your own datasets to produce maps of other fisheries, time periods, depths, parameters,
# and/or lifestages by using the embedded widget. You can also download an extract of the data depicted
# via another embedded widget. Check out the 'Learn More' tab to discover other cool related resources.
#     
# The default data is for Indonesian Blue Swimming Crab in Southeast Asia. Data was complied from a variety
# of sources with help from The Environmental Defense Fund and partners. See list below for data sources.
#     
#     Data Sources for Blue Swimming Crab dataset:
#       Defining ideal values for parameters
#     -	ResearchGate. “(16) Embryonic Development and Hatching Rate of Blue Swimming Crab, Portunus
#     Pelagicus (Linnaeus, 1758) under Different Water Salinities | Request PDF.” Accessed July 9, 2020. 
#     -	ResearchGate. “(16) (PDF) Fecundity, Embryonic and Ovarian Development of Blue Swimming Crab,
#     Portunus Pelagicus (Linnaeus, 1758) in Coastal Water of Johor, Malaysia.” Accessed July 7, 2020. 
#     -	ResearchGate. “(16) (PDF) Ingestion Rates of Brachionus Sp. And Artemia Sp. Nauplii by Blue
#     Swimming Crab, Portunus Pelagicus (Linnaeus, 1758) Larvae.” Accessed July 9, 2020. 
#     -	ResearchGate. “(16) (PDF) Natural Diet of Blue Swimming Crab, Portunus Pelagicus at Strait of
#     Tebrau, Johor, Malaysia.” Accessed July 7, 2020. 
#     -	“(16) Reproductive Performance and Larval Quality of First and Second Spawning of Pond-Reared
#     Blue Swimming Crab, Portunus Pelagicus, Broodstock.” Accessed July 9, 2020. 
#     -	Abol-Munafi, Ambok-Bolong, Nordiana Pilus, Roswati Md Amin, Mohamad N. Azra, and Mhd Ikhwanuddin.
#     “Digestive Enzyme Profiles from Foregut Contents of Blue Swimming Crab, Portunus Pelagicus from Straits
#     of Johor, Malaysia.” Journal of the Association of Arab Universities for Basic and Applied Sciences
#     24, no. 1 (October 1, 2017): 120–25.
#     -	Azra, Mohamad N., Jiann-Chu Chen, Te-Hua Hsu, Mhd Ikhwanuddin, and Ambok Bolong Abol-Munafi.
#     “Growth, Molting Duration and Carapace Hardness of Blue Swimming Crab, Portunus Pelagicus, Instars
#     at Different Water Temperatures.” Aquaculture Reports 15 (November 1, 2019): 100226.
#     -	Azra, Mohamad N., Jiann-Chu Chen, Mhd Ikhwanuddin, and Ambok Bolong Abol-Munafi. “Thermal Tolerance
#     and Locomotor Activity of Blue Swimmer Crab Portunus Pelagicus Instar Reared at Different Temperatures.”
#     Journal of Thermal Biology 74 (May 2018): 234–40. 
#     -	Bryars, Simon R., and Jon N. Havenhand. “Effects of Constant and Varying Temperatures on the
#     Development of Blue Swimmer Crab (Portunus Pelagicus) Larvae: Laboratory Observations and Field
#     Predictions for Temperate Coastal Waters.” Journal of Experimental Marine Biology and Ecology 329,
#     no. 2 (February 21, 2006): 218–29.
#     -	“BSC Manuscript_28September2017_RC (1).Pdf,” n.d.
#     -	“CastanosM1997-Grow-Mudcrab-in-Ponds.Pdf.” Accessed August 19, 2020. 
#     -	Dodd, Luke F., Jonathan H. Grabowski, Michael F. Piehler, Isaac Westfield, and Justin B. Ries.
#     “Ocean Acidification Impairs Crab Foraging Behaviour.” Proceedings of the Royal Society B: Biological
#     Sciences 282, no. 1810 (July 7, 2015): 20150333. 
#     -	“Effects of Temperature on the Whole Body Fatty Acid Composition and Histological Changes of the
#     Gills in Blue Swimmer Crabs, Portunus Pelagicus | Paper | Microsoft Academic.” Accessed June 26, 2020. 
#     -	Efrizal, Efrizal, Zuhri Syam, Rusnam Rusnam, and Suryati Suryati. “Growth Performance and Survival
#     Rate of Portunus Pelagicus (Linnaeus, 1758) Broodstock Females Fed Varying Doses of Amaranth Extracts.”
#     F1000Research 8 (August 19, 2019): 1466. 
#     -	“Food and Feeding of the Blue Swimmer Crab, Portunus Pelagicus (Linnaeus, 1758) (Decapoda,
#     Brachyura) along the Coast of Mandapam, Tamil Nadu, India | Paper | Microsoft Academic.” Accessed
#     July 20, 2020.
#     -	Ikhwanuddin, Mhd. “Effects of Stress Tests on Larvae of Blue Swimming Crab, Portunus Pelagicus
#     (Linnaeus, 1758),” 2012.
#     -	Johnson, Daniel D., Charles A. Gray, and William G. Macbeth. “REPRODUCTIVE BIOLOGY OF PORTUNUS
#     PELAGICUS IN A SOUTH-EAST AUSTRALIAN ESTUARY.” Journal of Crustacean Biology 30, no. 2 (2010): 200–205.
#     -	Kangas, M I. “FISHERIES RESEARCH REPORT NO. 121, 2000,” n.d., 25.
#     -	———. “FISHERIES RESEARCH REPORT NO. 121, 2000,” n.d., 25.
#     -	Kunsook, Chutapa, Nantana Gajaseni, and Nittharatana Paphavasit. “The Feeding Ecology of the Blue
#     Swimming Crab, Portunus Pelagicus (Linnaeus, 1758), at Kung Krabaen Bay, Chanthaburi Province, Thailand.”
#     Tropical Life Sciences Research 25, no. 1 (August 2014): 13–27.
#     -	“Larval Culture and Rearing Techniques of Commercially Important Crab, Portunus Pelagicus (Linnaeus,
#     1758): Present Status and Future Prospects | Paper | Microsoft Academic.” Accessed June 26, 2020.
#     -	“Live Foods for Juveniles’ Production of Blue Swimming Crab, Portunus Pelagicus (Linnaeus, 1766).”
#     Accessed July 20, 2020.
#     -	M. Ikhwanuddin*1, Shabdin, and A. B. 1 Abol-Munafi. “Natural Diet of Blue Swimming Crabs Found in
#     Sarawak Coastal Water,” 2009.
#     -	Marshall, Sharon, Kevin Warburton, Brian Paterson, and David Mann. “Cannibalism in Juvenile Blue
#     -Swimmer Crabs Portunus Pelagicus (Linnaeus, 1766): Effects of Body Size, Moult Stage and Refuge
#     Availability.” Applied Animal Behaviour Science 90, no. 1 (January 1, 2005): 65–82. 
#     -	Mhd, Ikhwanuddin, Azra Mohamad N., Talpur Mir A. D., Abol-Munafi Ambok B., and Shabdin Mohammad L.
#     “Optimal Water Temperature and Salinity for Production of Blue Swimming Crab, Portunus Pelagicus 1st
#     Day Juvenile Crab.” International Journal of the Bioflux Society, Volume 5 Issue 1, 2012.
#     -	Oniam, Vutthichai, and Wasana Arkronrat. “Development of Crab Farming: The Complete Cycle of Blue
#     Swimming Crab Culture Program (CBSC Program) in Thailand.” Journal of Fisheries and Environment 37,
#     no. 2 (2013): 31–43.
#     -	Oniam, Vutthichai, Likhit Chuchit, and Wasana Arkronrat. “Reproductive Performance and Larval
#     Quality of Blue Swimming Crab (Portunus Pelagicus) Broodstock, Fed with Different Feeds,” 2012, 6.
#     -	“Ontogenetic Changes in Tolerance to Acute Ammonia Exposure and Associated Gill Histological
#     Alterations during Early Juvenile Development of the Blue Swimmer Crab, Portunus Pelagicus | Paper
#     | Microsoft Academic.” Accessed June 26, 2020. 
#     -	“Portunus Pelagicus, Blue Swimming Crab : Fisheries.” Accessed June 26, 2020. 
#     -	Qari, S. A. “Thermal Tolerance of the Marine Crab, Portunus Pelagicus (Brachyura, Portunidae).”
#     Crustaceana 87, no. 7 (January 1, 2014): 827–33. 
#     -	Rajendiran, Saravanan, Beema Mahin Muhammad Iqbal, and Sugumar Vasudevan. “Induced Thermal Stress on
#     Serotonin Levels in the Blue Swimmer Crab, Portunus Pelagicus.” Biochemistry and Biophysics Reports 5
#     (March 1, 2016): 425–29. 
#     -	Romano, Nicholas, and Chaoshu Zeng. “The Effects of Salinity on the Survival, Growth and Haemolymph
#     Osmolality of Early Juvenile Blue Swimmer Crabs, Portunus Pelagicus.” Aquaculture 260, no. 1 (September
#     29, 2006): 151–62. 
#     -	Science Alert. “Some Aspects of Reproductive Biology of Blue Swimming Crab (Portunus Pelagicus
#     (Linnaeus, 1758)) Under Laboratory Conditions.” Accessed August 18, 2020. 
#     -	“STOCK ASSESSMENT AND REPRODUCTIVE BIOLOGY OF THE BLUE SWIMMING CRAB, PORTUNUS PELAGICUS IN BANDAR
#     ABBAS COASTAL WATERS, NORTHERN PERSIAN GULF | Paper | Microsoft Academic.” Accessed July 9, 2020.
#     -	“Studies on Reproductive Biology and Ecology of Blue Swimming Crab Portunus Pelagicus from Chilika
#     Lagoon, Orissa, India | Journal of the Marine Biological Association of the United Kingdom | Cambridge
#     Core.” Accessed July 9, 2020. 
#     -	Taufik, Mhd, Zainuddin Bachok, Mohamad N. Azra, and Mhd Ikhwanuddin. “Effects of Various Microalgae
#     on Fatty Acid Composition and Survival Rate of the Blue Swimming Crab Portunus Pelagicus Larvae.”
#     IJMS Vol.45(11) [November 2016], November 2016. 
#     -	“The Biology of the Blue Manna Crab Portunus Pelagicus in an Australian Estuary | Paper | Microsoft
#     Academic.” Accessed June 26, 2020. 
#     -	“View of Development of Crab Farming: The Complete Cycle of Blue Swimming Crab Culture Program
#     (CBSC Program) in Thailand.” Accessed August 18, 2020. 
#     -	Yusinta Fujaya, Muslimin, Dody Dh. Trijuno, and dan Syafiuddin. “Growth Performance of Blue
#     Swimming Crab Larvae (Portunus Pelagicus) At Controled Temperature.” International Journal of
#     Scientific and Research Publications (IJSRP) 9, no. 10 (October 12, 2019): p9442. 
#     -	Zairion, Yusli Wardiatno, and Achmad Fahrudin. “Sexual Maturity, Reproductive Pattern and Spawning
#     Female Population of the Blue Swimming Crab, &lt;I&gt;Portunus Pelagicus&lt;/I&gt; (Brachyura:
#     Portunidae) in East Lampung Coastal Waters, Indonesia.” Indian Journal of Science and Technology 8,
#     no. 7 (n.d.): 596.
#     Spatial inputs
#     -	Jiang, Li-Qing, Brendan R. Carter, Richard A. Feely, Siv K. Lauvset, and Are Olsen. “Surface Ocean
#     PH and Buffer Capacity: Past, Present and Future.” Scientific Reports 9, no. 1 (December 2019): 18624.
#     https://doi.org/10.1038/s41598-019-55039-4. https://data.nodc.noaa.gov/ncei/ocads/data/0206289
#     /Surface_pH_1770_2100/ https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.nodc:0206289 DOI:
#     10.25921/kgqr-9h49 
#     o	pH only
#     -	“OCADS - GLODAPv2.” Accessed July 20, 2020. https://www.nodc.noaa.gov/ocads/oceans/GLODAPv2/.
#     https://www.nodc.noaa.gov/ocads/oceans/GLODAPv2_2020/ 
#       o	pH, temperature, DO, salinity
#     -	Hirahara, Shoji, Masayoshi Ishii, and Yoshikazu Fukuda. “Centennial-Scale Sea Surface Temperature
#     Analysis and Its Uncertainty.” Journal of Climate 27, no. 1 (January 1, 2014): 57–75.
#     https://doi.org/10.1175/JCLI-D-12-00837.1. https://psl.noaa.gov/data/gridded/data.cobe.html 
#     o	Temperature only
#     -	US Department of Commerce, NOAA National Centers for Environmental Information. “World Ocean Atlas
#     2018: Data Access.” Accessed July 20, 2020. https://www.nodc.noaa.gov/OC5/woa18/woa18data.html.
#     o	DO, salinity, temperature
#     -	“Catalog Services.” Accessed September 11, 2020.
#     https://tds.hycom.org/thredds/catalogs/GLBy0.08/expt_93.0.html?dataset=GLBy0.08-expt_93.0-ts3z.
#     o	Temperature, salinity
#     -	“Climate Change Web Portal _ Maps MM: NOAA Physical Sciences Laboratory.” Accessed September
#     11, 2020. https://www.psl.noaa.gov/ipcc/ocn/.
#     o	Temperature, salinity, DO"
#     
#   })
#   
 }

# Run the application
shinyApp(ui = ui, server = server)

