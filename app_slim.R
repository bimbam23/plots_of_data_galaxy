##############################################################################
# PlotsOfData: Shiny app for plotting and comparing the data
# Created by Joachim Goedhart (@joachimgoedhart), first version 2018
# Takes non-tidy, spreadsheet type data as input or tidy format
# Non-tidy data is converted into tidy format
# For tidy data the x and y variables need to be selected
# Raw data is displayed with user-defined visibility (alpha)
# Summary statistics are displayed with user-defined visibility (alpha)
# Inferential statistics (95%CI) can be added
# The 95%CI of the median is determined by resampling (bootstrap)
# A plot and a table with stats are generated
# Colors can be added to the data and/or the stats
# Several colorblind safe palettes are available
# Ordering of the categorial data is 'as is, based on median or alphabetical
##############################################################################
# Copyright (C) 2018  Joachim Goedhart
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
##############################################################################

# install.packages.auto("readxl")

library(shiny)
library(ggplot2)
# library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggbeeswarm)
library(readxl)
library(shinyWidgets)
library(ggfortify)
# library(Rmisc)
################
#log file

log_con <- file("test.log", open="a")



#Function that resamples a vector (with replacement) and calculates the median value
boot_median = function(x) {
  median(sample(x, replace = TRUE))
}

i=0
#Number of bootstrap samples
nsteps=1000

#Confidence level
Confidence_Percentage = 95
Confidence_level = Confidence_Percentage/100

alpha=1-Confidence_level
lower_percentile=(1-Confidence_level)/2
upper_percentile=1-((1-Confidence_level)/2)


#Several qualitative color palettes that are colorblind friendly
#From Paul Tol: https://personal.sron.nl/~pault/
#Code to generate vectors in R to use these palettes

#Red, Green, Blue, yellow, cyan, purple, grey
Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#999933', '#DDCC77', '#CC6677', '#882255', '#AA4499', '#332288', '#DDDDDD')
Tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')


#Read a text file (comma separated values)
# df_wide_example <- read.csv("Data_wide_example.csv", na.strings = "")
df_tidy_example <- read.csv("Data_tidy_example.csv", na.strings = "")
#mtcars_example <- read.csv("mtcars.csv", na.strings = "")
mpg_example <- read.csv("mpg.csv", na.strings = "")

 #######################################
###### Define the User interface #########

ui <- fluidPage(
  titlePanel("PlotsOfData - Plots all Of the Data"),
  sidebarLayout(
    sidebarPanel(width=3,
      conditionalPanel( 
        condition = "input.tabs=='Plot'",
        radioButtons("plot_type", "Type of Plot", 
                     choices = list("Barplot" = "barplot", 
                                    "Dotplot" = "dotplot", 
                                    "Boxplot" = "boxplot",
                                    "Heatmap" = "heatmap",
                                    "PCA" = "pca"), 
                     selected = "boxplot"),
        conditionalPanel( 
          condition = "input.plot_type=='boxplot' || input.plot_type=='barplot' || input.plot_type=='dotplot'",
          selectInput("select_xaxes", "Select x-axes:", choices = ""),
          selectInput("select_yaxes", "Select y-axes:", choices = ""),
          selectInput("select_fill", "Select fill:",  choices = "")
        ),
        
        #######################################
        ############## BOXPLOT ###############        
        
        conditionalPanel( 
          condition = "input.plot_type=='boxplot'",
          radioButtons("jitter_type", "Data offset", 
                       choices = list("Beeswarm" = "beeswarm", "Random" = "random", "None (for small n)" = "none"), 
                       selected = "beeswarm"),
       
          sliderInput("alphaInput", "Visibility of the data",
                      0, 1, 0.3),
  
          # conditionalPanel(
          #   condition = "input.adjust_jitter == true",
          #   sliderInput("jitter_width", "Width:", 0,0.5,0.3),
          #   checkboxInput(inputId = "random_jitter", label = ("Randomize Jitter"), value = TRUE)
          # ),
            
          radioButtons("summaryInput", "Statistics", 
                       choices = list("Median" = "median", "Mean" = "mean", "Boxplot (minimal n=10)" = "boxplot", "Violin Plot (minimal n=10)" = "violin"),
                       selected = "median"),
  #        sliderInput("Input_CI", "Confidence Level", 90, 100, 95),
          checkboxInput(inputId = "add_CI", 
                        label = HTML("Add 95% CI <br/> (minimal n=10)"), 
                        value = FALSE)
        ),
  
 
  #######################################
  ############## BARPLOT ###############        
  
        conditionalPanel( 
          condition = "input.plot_type=='barplot'",
          conditionalPanel(condition = "input.select_yaxes!='none'",
                           radioButtons("position_type", "Barplot options position", 
                                        choices = list("Stacked" = "none", "Side by side (dodge)" = "dodge"), 
                                        selected = "none"),#),
          conditionalPanel(condition = "input.select_yaxes!='none' || input.position_type=='none'",
                           checkboxInput(inputId = "errorbar", label = "Error bars", value = FALSE))
        )),
  
  conditionalPanel( 
    condition = "input.plot_type=='pca'",
    checkboxInput(inputId = "modify_col_rows",
                  label = "Exclude/Select data columns or rows",
                  value = FALSE),
    conditionalPanel(
      condition = "input.modify_col_rows == true",
      pickerInput("select_colnames_pca", "Select columns:", options = list(`actions-box` = TRUE), choices = "", multiple = TRUE),
      pickerInput("select_rownames_pca", "Select rows:", options = list(`actions-box` = TRUE), choices = "", multiple = TRUE)
    ),
    radioButtons("transform", "Sample names orientation", 
                 choices = list("Horizonal" = "horizontal", 
                                "Vertial" = "vertial"), 
                 selected = "horizontal"),
    radioButtons("pca_plot_type", "Select plot layout", 
                 choices = list("Show shapes" = "shape", 
                                "Show labels (group elements)" = "label", 
                                "Show shapes and labels" = "both"), 
                 selected = "label")
    ),
  
  #######################################
  ############## ALLPLOT ###############        
  
        sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),
        # radioButtons(inputId = "ordered",
        #      label= "Order of the data/statistics:",
        #      choices = list("As supplied" = "none", "By median value" = "median", "By alphabet/number" = "alphabet"),
        #      selected = "none"),
        h4("Plot Layout"),      

        checkboxInput(inputId = "rotate_plot",
              label = "Rotate plot 90 degrees",
              value = FALSE),

        checkboxInput(inputId = "no_grid",
                      label = "Remove gridlines",
                      value = FALSE),

        checkboxInput(inputId = "adjust_scale",
                      label = "Adjust scale",
                      value = FALSE),
        conditionalPanel(
          condition = "input.adjust_scale == true",
          textInput("range", "Range of values (min,max)", 
                    value = "0,2")),
        
        checkboxInput("color_data", "Use color for the data", 
                      value=FALSE),
        checkboxInput("color_stats", "Use color for the stats", 
                      value=FALSE),

        conditionalPanel(
          condition = "input.color_data == true || input.color_stats == true",
          ########## Choose color from list
          #selectInput("colour_list", "Colour:", choices = ""),

          radioButtons("adjustcolors", "Color palette:", 
                       choices = list("Standard" = 1,"Colorblind safe (bright)" = 2,"Colorblind safe (muted)" = 3,"Colorblind safe (light)" = 4, "User defined"=5) , 
                       selected =  1),
          conditionalPanel(
            condition = "input.adjustcolors == 5",
            textInput("user_color_list", "List of names or hexadecimal codes", value = "turquoise2,#FF2222,lawngreen"), 
            h5("", a("Click here for more info on color names", 
                     href = "http://www.endmemo.com/program/R/color.php", 
                     target="_blank"))
          )
        ),

        sliderInput("plot_height", "Height (# pixels): ", 1, 4000, 480),
        sliderInput("plot_width", "Width (# pixels):",  1, 4000, 480),

        h4("Labels"),

        checkboxInput(inputId = "add_title",
                        label = "Add title",
                        value = FALSE),
        conditionalPanel(
        condition = "input.add_title == true",
        textInput("title", "Title:", value = "")
        ),

        checkboxInput(inputId = "label_axes",
              label = "Change labels",
              value = FALSE),
        conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = ""),
              textInput("lab_y", "Y-axis:", value = "")),
        checkboxInput(inputId = "adj_fnt_sz",
              label = "Change font size",
              value = FALSE),
       conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
              numericInput("fnt_sz_ax", "Size axis labels:", value = 18),
              numericInput("fnt_sz_leg", "Size axis labels:", value = 18)),
        conditionalPanel(
          condition = "input.color_data == true || input.color_stats == true || input.select_fill != 'none' ",
          checkboxInput(inputId = "add_legend",
                        label = "Add legend",
                        value = FALSE),
          conditionalPanel(
            condition = "input.add_legend == true",
            checkboxInput(inputId = "modi_legend_title",
                          label = "Modify legend title",
                          value = FALSE),
            radioButtons("legend_pos", "Legend postion:", 
                         choices = list("Standard" = "right", "Botton" = "bottom", "Top" = "top", "Left"="left") , 
                         selected = "right")),
            conditionalPanel(
              condition = "input.modi_legend_title == true && input.add_legend == true",
              textInput("leg_title", "Legend title:", value = "")
          ))

    ),
  #######################################
  ############## DATA UPLOAD ###############        
    conditionalPanel(
        condition = "input.tabs=='Data upload'",
        h4("Data upload"),
        radioButtons(
          "data_input", "",
          choices = 
            list("Example 1 (mpg)" = 1,
                 "Example 2 (tidy format)" = 2,
                 "Upload file" = 3,
                 "Paste data" = 4)
          ,
          selected =  1),
        conditionalPanel(
          condition = "input.data_input=='1'"
          
        ),
        conditionalPanel(
          condition = "input.data_input=='3'",
          h5("Upload file: "),
          fileInput("upload", "", multiple = FALSE),
          selectInput("file_type", "Type of file:",
                      list("text (csv)" = "text",
                           "Excel" = "Excel"
                      ),
                      selected = "text"),
          conditionalPanel(
            condition = "input.file_type=='text'",

          radioButtons(
              "upload_delim", "Delimiter",
              choices = 
                list("Comma" = ",",
                     "Tab" = "\t",
                     "Semicolon" = ";",
                     "Space" = " "),
              selected = ",")),
          
          actionButton("submit_datafile_button",
                       "Submit datafile")),
        conditionalPanel(
          condition = "input.data_input=='4'",
          h5("Paste data below:"),
          tags$textarea(id = "data_paste",
                        placeholder = "Add data here",
                        rows = 10,
                        cols = 20, ""),
          actionButton("submit_data_button", "Submit data"),
              radioButtons(
                "text_delim", "Delimiter",
                choices = 
                    list("Tab (from Excel)" = "\t",
                         "Space" = " ",
                         "Comma" = ",",
                         "Semicolon" = ";"),
                          selected = "\t")),
        checkboxInput(inputId = "tidyInput",
                      label = "These data are Tidy",
                      value = FALSE),
        conditionalPanel(
          condition = "input.tidyInput==true",
          h5("",
             a("Click here for more info on tidy data",
               href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")),
          selectInput("x_var", "Conditions to compare:", choices = ""),
          selectInput("y_var", "Variables:", choices = "")
          
          )
      ),
      
      conditionalPanel(
        condition = "input.tabs=='About'",
        h4("About")    
      )
  #,
      
      # conditionalPanel(
      #   condition = "input.tabs=='Data Summary'",
      #   h4("Data summary")    
      # )
      
    ),
  #######################################
  ############## SAVE FILE ##############        
    mainPanel(
 
       tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"), dataTableOutput("data_uploaded")),
                  tabPanel("Plot",
                           #downloadButton("downloadPlotPDF", "Download pdf-file"), 
                           #downloadButton("downloadPlotPNG", "Download png-file"),
                           downloadButton("downloadPlot", "Download file"),
                           selectInput("output_format", "Choose output format:",
                                       list(`Picture` = c("png", "jpeg", "tiff", "bmp"),
                                            `Vector` = c("pdf", "ps", "eps", "svg", "tex")
                                            )),
                           plotOutput("coolplot")
                  ), 
                  #tabPanel("Data Summary", tableOutput('data_summary')),
                  tabPanel("About", includeHTML("about.html")
                           )
                  
      )
    )
  )         
)

 #######################################

server <- function(input, output, session) {

  #####################################
  ###### DATA INPUT ###################

  df_upload <- reactive({
    if (input$data_input == 1) {
      data <- mpg_example
    }  else if (input$data_input == 2) {
        data <- df_tidy_example 
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        
        return(data.frame(x = "Select your datafile"))
      } else if (input$submit_datafile_button == 0) {
        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
          if (input$file_type == "text") {
            data <- read_delim(file_in$datapath,
                               delim = input$upload_delim,
                               col_names = TRUE)
          } else if (input$file_type == "Excel") {
            data <- read_excel(file_in$datapath)
          } 
        })
      }
    } else if (input$data_input == 4) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
          })
        }
      }
  }
    return(data)
})

 #####################################
  
 ####################################
##### CONVERT TO TIDY DATA ##########
  
#Need to tidy the data?!
#Untidy data will be converted to long format with two columns named 'Condition' and 'Value'
#The input for "Condition" will be taken from the header, i.e. first row
#Tidy data will be used as supplied
df_upload_tidy <- reactive({
    if(input$tidyInput == FALSE ) {
      klaas <- gather(df_upload(), Condition, Value)
    }
    else if(input$tidyInput == TRUE ) {
      klaas <- df_upload()
    }
  return(klaas)
})
 ###################################


 ####################################
##### Get the Variables ##############

observe({ 
        var_names  <- names(df_upload_tidy())
        var_list <- c("none", var_names)
        var_names_upload  <- names(df_upload())
        var_rownames_upload <- row.names(df_upload())
        #        updateSelectInput(session, "colour_list", choices = var_list)
        updateSelectInput(session, "y_var", choices = var_list)
        updateSelectInput(session, "x_var", choices = var_list)
        
        var_list_x <- var_names_upload
        var_list_fill <- c("none", var_names_upload)
        # check if numeric
        var_list_y <- c("none", var_names_upload[unlist(lapply(df_upload(), is.numeric))])
        
        # used for bar box and dot plot
        updateSelectInput(session, "select_xaxes", choices = var_list_x)
        updateSelectInput(session, "select_yaxes", choices = var_list_y)
        updateSelectInput(session, "select_fill", choices = var_list_fill)
        
        var_names_upload <- var_names_upload[unlist(lapply(df_upload(), is.numeric))]
        # used for PCA selection
        updateSelectInput(session, "select_colnames_pca", choices = var_names_upload)
        updateSelectInput(session, "select_rownames_pca", choices = var_rownames_upload)
        })
 ###################################    

###########################################################  
######## Determine and set the order of the Conditions #######  

df_sorted <- reactive({
  
    #  klaas <- df_upload_tidy()
    klaas <-  df_subset()

   if(input$ordered == "median") {
     klaas[, input$select_xaxes] <- reorder(klaas[, input$select_xaxes], klaas[, input$select_yaxes], median, na.rm = TRUE)

   } else if (input$ordered == "none") {
      klaas[, input$select_xaxes] <- factor(klaas[, input$select_xaxes], levels=unique(klaas[, input$select_xaxes]))

   } else if (input$ordered == "alphabet") {
     klaas[, input$select_xaxes] <- factor(klaas[, input$select_xaxes], levels=unique(sort(klaas[, input$select_xaxes])))
   }  

  return(klaas)
  
})

########################################################### 


 ###########################################################  
######## Extract the data for display & summary stats #######  

# df_selected <- reactive({
#     if(input$tidyInput == TRUE ) {
#     df_temp <- df_upload_tidy() 
#     input$select_xaxes <- input$x_var
#     input$select_yaxes <- input$y_var
#     
#     koos <- df_temp %>% select(Condition = !!input$select_xaxes , Value = !!input$select_yaxes) %>% filter(!is.na(Value))
# 
#     } else if (input$tidyInput == FALSE ) {
#       koos <- df_upload_tidy() %>% filter(!is.na(Value))
#     }
# 
#     return(koos)
# })
 ###########################################################  

df_subset <- reactive({
  df_temp <- as.data.frame(df_upload())
  print(tbl_df(df_temp), n=10)
  # capture.output(df_temp, file = log_con)
  # input$select_xaxes <- input$select_xaxes
  # input$select_yaxes <- input$select_yaxes
  #koos <- df_temp %>% select(!!input$select_xaxes , !!input$select_yaxes) %>% filter(!is.na(.[[input$select_yaxes]]))
  koos <- df_temp[, c(input$select_xaxes , input$select_yaxes)]
  # capture.output(koos, file = log_con)
  cat("df_subset():\n")
  cat(input$select_xaxes, input$select_yaxes, "\n")
  
 # names(koos) <- c(as.character(input$select_xaxes), as.character(input$select_yaxes))
  
  print(tbl_df(koos), n=10)
  cat("END df_subset():\n")
  return(koos)
})
  
 #############################################################
#### DISPLAY UPLOADED DATA (exactly as provided) ##################

output$data_uploaded <- renderDataTable({
#    observe({ print(input$tidyInput) })
      df_upload()
  })
 #############################################################


 ##################################################
#### Caluclate Summary of the DATA for the MEAN ####

df_summary_mean <- reactive({
  # koos <- df_selected()
  koos <- df_subset()
  df_sum_mean <- koos %>%
    select(Con = !!input$select_xaxes , Val = !!input$select_yaxes) %>%
    group_by(Con) %>%
    summarise(n = n(),
              mean = mean( Val, na.rm = TRUE),
              median = median( Val, na.rm = TRUE),
              sd = sd( Val, na.rm = TRUE)) %>%
    mutate(sem = sd / sqrt(n - 1),
           CI_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
           CI_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem)
  
  # fix input$select_xaxes and input$select_yaxes
  names(df_sum_mean)[1] <- input$select_xaxes
  names(df_sum_mean)[2] <- input$select_yaxes
  
  cat("df_summary_mean():\n")
  print(tbl_df(df_sum_mean), n=40)
  return(df_sum_mean)
})


 #################################################

 ####################################################
#### Caluclate Summary of the DATA for the Median ####

df_summary_median <- reactive({
  # kees <- mpg[, c("manufacturer", "cty")]
  # input$select_xaxes <- "manufacturer"
  # input$select_yaxes <- "cty"
  kees <- df_subset()
    # names(kees) <- c("Condition", "Value")
 #   df_booted <- data.frame(Condition=levels(factor(kees$Condition)), n=tapply(kees$Value, kees$Condition, length), median=tapply(kees$Value, kees$Condition, median))
  df_booted <- kees %>%
    select(Con = !!input$select_xaxes , Val = !!input$select_yaxes) %>%
    group_by(Con) %>%
    summarise(n= n(),
              median = median(Val, na.rm = TRUE),
              MAD= mad(Val, na.rm = TRUE, constant=1))
    
  # fix input$select_xaxes and input$select_yaxes
  names(df_booted)[1] <- input$select_xaxes
  names(df_booted)[2] <- input$select_yaxes
  
  i=0
  df_new_medians <- data.frame(Condition=levels(factor(kees[, input$select_xaxes])), resampled_median=tapply(kees[, input$select_yaxes], kees[, input$select_xaxes], boot_median))
    
    #Perform the resampling nsteps number of times (typically 1,000-10,000x)
    for (i in 1:nsteps) {
      
      #Caclulate the median from a boostrapped sample (resampled_median) and add to the dataframe
      df_boostrapped_median <- data.frame(Condition=levels(factor(kees[,input$select_xaxes])), resampled_median=tapply(kees[,input$select_yaxes], kees[,input$select_xaxes], boot_median))
      
      #Add the new median to a datafram that collects all the resampled median values
      df_new_medians <- bind_rows(df_new_medians, df_boostrapped_median)
    }
    
    df_booted$CI_lo <- tapply(df_new_medians$resampled_median, df_new_medians$Condition, quantile, probs=lower_percentile)
    df_booted$CI_hi <- tapply(df_new_medians$resampled_median, df_new_medians$Condition, quantile, probs=upper_percentile)
    
    cat("df_summary_median():\n")
    print(tbl_df(df_booted), n=40)
    return(df_booted)
  })
 ###################################################
  

##################################################
#### Caluclate Summary of the DATA for Box (&Violin) ####

df_summary_box <- reactive({
  # input$select_xaxes <- input$select_xaxes
  # y_choice <- input$select_yaxes
  kees <- df_subset()
  # names(kees) <- c("Condition", "Value")
  df_sum_box <- kees %>%
    select(Con = !!input$select_xaxes , Val = !!input$select_yaxes) %>%
    group_by(Con) %>%
    summarise(n = n(),
              mean = mean(Val),
              SD = sd(Val),
              median = median(Val),
              MAD = mad(Val, constant=1),
              IQR = IQR(Val))
  
  # fix input$select_xaxes and input$select_yaxes
  names(df_sum_box)[1] <- input$select_xaxes
  names(df_sum_box)[2] <- input$select_yaxes
  cat("df_summary_box():\n")
  print(tbl_df(df_sum_box), n=40)
  return(df_sum_box)
})

df_summary_error <- reactive({
  kees <- df_subset()
  # names(kees) <- c("Condition", "Value")
  cat("df_summary_error() before: \n")
  print(tbl_df(kees), n=40)
  df_sum_error <- kees %>%
    select(Con = !!input$select_xaxes , Val = !!input$select_yaxes) %>%
    group_by(Con) %>%
    summarise(mean_PL = mean(Val),  # calculates the mean of each group
              sd_PL = sd(Val), # calculates the standard deviation of each group
              n_PL = n(),  # calculates the sample size per group
              SE_PL = sd(Val)/sqrt(n())) # calculates the standard error of each group
  # capture.output(df_sum_error, file = log_con)
  
  # fix input$select_xaxes and input$select_yaxes
  names(df_sum_error)[1] <- input$select_xaxes
  names(df_sum_error)[4] <- input$select_yaxes
  cat("df_summary_error():\n")
  print(tbl_df(df_sum_error), n=40)
  return(df_sum_error)
})


#################################################


 ###########################################
######### DEFINE DOWNLOAD BUTTONS ###########

##### Set width and height of the plot area
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })

# output$downloadPlotPDF <- downloadHandler(
#   filename <- function() {
#     paste("ComparisonPlot_", Sys.time(), ".pdf", sep = "")
#   },
#   content <- function(file) {
#     ggsave(file, width = input$plot_width/72,
#            height = input$plot_height/72, dpi="retina")
#   },
#   contentType = "application/pdf" # MIME type of the image
# )
# 
# output$downloadPlotPNG <- downloadHandler(
#   filename <- function() {
#     paste("ComparisonPlot_", Sys.time(), ".png", sep = "")
#   },
#   content <- function(file) {
#     ggsave(file, width = input$plot_width/72,
#            height = input$plot_height/72)
#   },
#   contentType = "application/png" # MIME type of the image
# )

output$downloadPlot<- downloadHandler(
  filename <- function() {
    paste("ComparisonPlot_", Sys.time(), paste0(".", input$output_format))
  },
  content <- function(file) {
    ggsave(file, width = input$plot_width/72,
           height = input$plot_height/72)
  },
  contentType = paste0("application/", input$output_format) # MIME type of the image
)

 ###########################################


 ###########################################
######## PREPARE PLOT FOR DISPLAY ##########
 ###########################################

output$coolplot <- renderPlot(width = width, height = height, {

  
####### Read the order from the ordered dataframe #############  
    # koos <- df_sorted()
    # custom_order <-  levels(factor(koos$Condition))
    
#    observe({ print(custom_order) })

    
  # x y fill catch
  # x_choice <- input$select_xaxes
  # y_choice <- input$select_yaxes
  # fill_choice <- input$select_fill
  # 
  cat("x_choice:", input$select_xaxes, "\n")
  cat("y_choice:", input$select_yaxes, "\n")
  cat("fill_choice:",  input$select_fill, "\n")
  
  ########## Define alternative color palettes ##########
  
  
  if(input$plot_type %in% c("boxplot", "barplot", "dotplot")){
  cat("Check colors: ")
  newColors <- NULL
  
  if (input$adjustcolors == 2) {
    newColors <- Tol_bright
  } else if (input$adjustcolors == 3) {
    newColors <- Tol_muted
  } else if (input$adjustcolors == 4) {
    newColors <- Tol_light
  } else if (input$adjustcolors == 5) {
    newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
  }
  
  # cat(newColors, file = log_con)
  
######## Repeat the colors, if number of colors < number of conditions
  klaas <- df_upload()
  max_colors <- nlevels(as.factor(klaas[, input$select_xaxes]))
  if(length(newColors) < max_colors) {
    newColors<-rep(newColors,times=(round(max_colors/length(newColors)))+1)
  }

  
  
########## Set default to Plotting "Condition" and "Value"
    # if (input$x_var == "none") {
    #   input$select_xaxes <- "Condition"
    # } else if (input$x_var != "none") {
    #   input$select_xaxes <- as.character(input$x_var)
    # }  
    # 
    # if (input$y_var == "none") {
    #   input$select_yaxes <- "Value"
    # } else if (input$y_var != "none") {
    #   input$select_yaxes <- as.character(input$y_var)
    # }

  ########## Define if color is used for the data
  #    observe({ print(class(input$colour_list)) })
  if (input$color_data == FALSE) {
    kleur <- NULL
  } else if (input$color_data == TRUE) {
    #    kleur <- as.character(input$colour_list)
    kleur <- input$select_xaxes
  }
  
  ########## Define if/how color is used for the stats ############
  #    observe({ print(class(input$colour_list)) })
  if (input$color_stats == FALSE) {
    kleur_stats <- NULL
  } else if (input$color_stats == TRUE && input$summaryInput == "boxplot") {
    kleur_stats <- input$select_xaxes
  } else if (input$color_stats == TRUE && input$summaryInput == "violin") {
    kleur_stats <- input$select_xaxes
  } else if (input$color_stats == TRUE) {
    kleur_stats <- input$select_xaxes
  }  
  cat("...color checked!\n")  
  
  ########## Define minimal n - only plot box/violinplots for min_n>9
  
  
  ########################### changes till here ###################################
  
  cat("Calculate minimum n: ")
  
  
  df_temp <- data.frame()
  
  min_n <- 0
  
  if(input$select_yaxes != "none" & input$plot_type == "boxplot"){
    df_temp <- df_summary_median()
    min_n <- min(df_temp[, input$select_yaxes])
  }
  cat("...calculated min_n:", min_n, "\n")
    ########## Define barplot df
  }  
    df_bar <- df_upload()
    # df_bar_error <- df_summary_error()
    
    # error bar
    # cat(input$select_xaxes, input$select_yaxes)
    # x_value <- input$select_xaxes
    # y_value <- input$select_yaxes
    # df_bar_error <- df_bar[ ,c(x_value, y_value)]
    # df_bar_error <- df_bar_error %>%
    #   group_by(x_value) %>% 
    #   summarise(mean_PL = mean(y_value),  # calculates the mean of each group
    #             sd_PL = sd(y_value), # calculates the standard deviation of each group
    #             n_PL = n(),  # calculates the sample size per group
    #             SE_PL = sd(y_value)/sqrt(n()))
    # 
    # print(names(df_bar))

 ###############################################
############## GENERATE PLOT LAYERS #############
    
    
    # get_aes_string <- function(type) {
    #   switch(type,
    #          mean = 1,
    #          median = 2,
    #          trimmed = 3)
    # }
    # test1('mean') # 0.89 secs
    
    cat("run aes_string on plot type:", input$plot_type, ": ")
    
    if(input$plot_type %in% c("boxplot", "barplot", "dotplot")){
      if(input$select_yaxes == "none" & input$select_fill == "none"){
        astring <- aes_string(x=input$select_xaxes)
      }
      if(input$select_yaxes != "none" & input$select_fill != "none" & input$plot_type %in% c("barplot")){
        astring <- aes_string(x=input$select_xaxes, y=input$select_yaxes, fill=input$select_fill)
      }
      if(input$select_yaxes != "none" & input$select_fill != "none" & input$plot_type == "boxplot"){
        astring <- aes_string(x=input$select_xaxes, y=input$select_yaxes)
      }
      if(input$select_yaxes == "none" & input$select_fill != "none" & input$plot_type %in% c("barplot")){
        astring <- aes_string(x=input$select_xaxes, fill=input$select_fill)
      }
      if(input$select_yaxes == "none" & input$select_fill != "none" & input$plot_type %in% c("boxplot")){
        astring <- aes_string(x=input$select_xaxes)
      }
      if(input$select_yaxes != "none" & input$select_fill == "none"){
        astring <- aes_string(x=input$select_xaxes, y=input$select_yaxes)
      }
      p <- ggplot(data=df_bar, astring)
    }else if(input$plot_type == "pca"){
      
      cat("input$plot_type:", input$pca_plot_type, "\n")
      # basic vars
      plot_options <- c("shape", "label") %in% input$pca_plot_type
      cat("plot_options:", plot_options, "\n")
      if(all(!plot_options[1], !plot_options[2])){ # both 
        plot_options <- c(TRUE, TRUE)
      }
      cat("plot_options:", plot_options, "\n")
      # subset to numeric only
      if(input$modify_col_rows){
        df_pca <- df_bar[input$select_rownames_pca, input$select_colnames_pca]
        if(input$transform == "horizontal"){ # fix df_bar in the case of modifying the data.frame
          df_bar <- df_bar[, input$select_colnames_pca]
        }else{
          df_bar <- df_bar[input$select_rownames_pca, ]  
        }
        # cat(input$select_rownames_pca)
      }else{
        colnames_numeric <- names(df_bar)[unlist(lapply(df_bar, is.numeric))]
        df_pca <- df_bar[, colnames_numeric]
      }
      if(input$transform == "horizontal"){
        p <-autoplot(prcomp(t(df_pca)), data = t(df_bar), frame = FALSE,
                     shape = plot_options[1], label = plot_options[2])
      }else{
        p <-autoplot(prcomp(df_pca), data = df_bar, frame = FALSE,
                     shape = plot_options[1], label = plot_options[2])
      }
     
    }
    cat("...done\n")
    
    #
    # df_bar <- data.frame(v1=1:20,v2=1:20,v3=1:20,v4=letters[1:20])
    # df_bar <- df_bar[c(1,2,3,12), ]
    # df_pca <- df_bar[c(1,2,3,12), c(1,2,3)]



    # x[row.names(x)[c(1,2,3,12)],c(1,2,3)]
    ### BOXPLOT ###
    
    if (input$plot_type == "boxplot" && input$select_yaxes != "none"){
      # p <- ggplot(data=df_upload(), aes_string(x = input$select_xaxes)) 
      # 
      # # Setting the order of the x-axis
      # p <- p + scale_x_discrete(limits=custom_order)
  
    ##### plot selected data summary (bottom layer) ####
    #### plot boxplot ####
    if (input$summaryInput == "boxplot" && min_n>9 && input$select_yaxes != "none" && input$select_fill == "none") { # && input$select_fill == "none" # coming soon
      p <- p + geom_boxplot(notch = input$add_CI, outlier.color=NA, width=0.8, size=0.5, alpha=input$alphaInput_summ)
    }else if (input$summaryInput == "boxplot" && min_n>9 && input$select_yaxes != "none" && input$select_fill != "none"){
      p <- p + geom_boxplot(aes_string(fill = input$select_fill), notch = input$add_CI, outlier.color=NA, width=0.8, size=0.5, alpha=input$alphaInput_summ)
    #### plot violine ####  
    }else if (input$summaryInput == "violin" && input$select_fill == "none" && min_n>9) {
      p <- p + geom_violin(scale = "width", draw_quantiles = c(0.5), width=0.8, size=0.5, alpha=input$alphaInput_summ) 
      if (input$add_CI == TRUE) {
        p <- p + geom_linerange(data=df_summary_median(), aes_string(x=input$select_xaxes, ymin = "CI_lo", ymax = "CI_hi"), colour="black", size =3, alpha=input$alphaInput_summ)
      }
    }else if (input$summaryInput == "violin" && input$select_fill != "none" && min_n>9){
      p <- p + geom_violin(aes_string(fill = input$select_fill), scale = "width", draw_quantiles = c(0.5), width=0.8, size=0.5, alpha=input$alphaInput_summ) 
      if (input$add_CI == TRUE) {
         p <- p + geom_linerange(data=df_summary_median(), aes_string(x=input$select_xaxes, ymin = "CI_lo", ymax = "CI_hi"), colour="black", size =3, alpha=input$alphaInput_summ)
      }
    }
    #### plot individual measurements (middle layer) ####
    if (input$jitter_type == "beeswarm") {
      p <- p + geom_quasirandom(data=df_subset(), aes_string(x=input$select_xaxes, y=input$select_yaxes, colour = kleur), varwidth = TRUE, cex=3, alpha=input$alphaInput)
    } else if (input$jitter_type == "random") {
      p <- p + geom_jitter(data=df_subset(), aes_string(x=input$select_xaxes, y=input$select_yaxes, colour = kleur), width=0.3, height=0.0, cex=3, alpha=input$alphaInput)
    } else if (input$jitter_type == "none") {
      p <- p + geom_jitter(data=df_subset(), aes_string(x=input$select_xaxes, y=input$select_yaxes, colour = kleur), width=0, height=0.0, cex=3, alpha=input$alphaInput)
    }
    
  ##### plot selected data summary (top layer) ####
    if (input$summaryInput == "median"  && input$add_CI == TRUE && min_n>9) {
      p <-  p + geom_point(data=df_summary_median(), aes_string(x=input$select_xaxes, y = "median", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ) +
        geom_linerange(data=df_summary_median(), aes_string(x=input$select_xaxes, ymin = "CI_lo", ymax = "CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)
    } else if (input$summaryInput == "median"  && input$add_CI == FALSE || min_n<10) {
      p <-  p + geom_errorbar(data=df_summary_median(), aes_string(x=input$select_xaxes, ymin="median", ymax="median", colour = kleur_stats), width=.8, size=2, alpha=input$alphaInput_summ)
    } else if (input$summaryInput == "mean"  && input$add_CI == TRUE && min_n>9) {
      p <- p + geom_linerange(data=df_summary_mean(), aes_string(x=input$select_xaxes, ymin = "CI_lo", ymax = "CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)+
        geom_point(data=df_summary_mean(), aes_string(x=input$select_xaxes, y = "mean", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)
    } else if (input$summaryInput == "mean"  && input$add_CI == FALSE || min_n<10) {
      p <- p + geom_errorbar(data=df_summary_mean(), aes_string(x=input$select_xaxes, ymin="mean", ymax="mean", colour=kleur_stats), width=.8, size=2, alpha=input$alphaInput_summ)
    }
  }else if(input$plot_type == "barplot"){
      # Setting the order of the x-axis
      # p <- p + scale_x_discrete(limits=custom_order)
      if (input$position_type == "dodge" & input$select_yaxes != "none"){
        # cat("position", input$position_type, input$stat_type)
        # mpg_example
        # df_bar <- mpg
        # 
        # data_summary <- function(data, varname, groupnames){
        #   require(plyr)
        #   summary_func <- function(x, col){
        #     c(mean = mean(x[[col]], na.rm=TRUE),
        #       sd = sd(x[[col]], na.rm=TRUE))
        #   }
        #   data_sum<-ddply(data, groupnames, .fun=summary_func,
        #                   varname)
        #   data_sum <- rename(data_sum, c("mean" = varname))
        #   return(data_sum)
        # }
        # df_bar_error <- data_summary(df_bar,  "displ", c("class", "drv"))
        # 
        # df_bar_sub <- df_bar %>% select(Condition = !!"class" , Value = !!"displ") %>% filter(!is.na(Value))
        # df_bar_error <- df_bar_sub %>%
        #   group_by(Condition) %>%
        #   summarise(mean_PL = mean(Value),  # calculates the mean of each group
        #             sd_PL = sd(Value), # calculates the standard deviation of each group
        #             n_PL = n(),  # calculates the sample size per group
        #             SE_PL = sd(Value)/sqrt(n())) # calculates the standard error of each group
        # p <- ggplot(data=df_bar, aes(x=class, y=displ, fill=drv))
        # p <- p + geom_bar(stat = "identity", position = "stack", width=0.8, size=0.5)
        # p + 
        #   geom_errorbar(data= df_bar_error, aes(ymin=displ-sd, ymax=displ+sd), width=.2,
        #                   position=position_dodge(.9))
          #geom_errorbar(data= df_bar_error, aes(ymin = df_bar_error$mean_PL - df_bar_error$sd_PL, ymax = df_bar_error$mean_PL + df_bar_error$sd_PL), width=0.2, position = "dodge")
        
        
        
        # cat(input$position_type, input$stat_type, input$select_xaxes, input$select_yaxes, input$select_fill, input$alphaInput_summ)
      #   print(aes_string(x=input$select_xaxes, y=input$select_yaxes, fill=input$select_fill))
        p <- p + geom_bar(stat = "identity",
                          position = "dodge",
                          width=0.8, size=0.5, 
                          alpha=input$alphaInput_summ)
      }else if (input$position_type == "none" & input$select_yaxes != "none"){
        # cat("no_position", input$position_type, input$stat_type)
        # cat(input$position_type, input$stat_type, input$select_xaxes, input$select_yaxes, input$select_fill, input$alphaInput_summ)
        p <- p + geom_bar(width=0.8, size=0.5, stat = "identity",
                          alpha=input$alphaInput_summ)
      }
      if(input$select_yaxes == "none"){
        # cat(input$position_type, input$stat_type, input$select_xaxes, input$select_yaxes, input$select_fill, input$alphaInput_summ)
        p <- p + geom_bar(width=0.8, size=0.5,
                          alpha=input$alphaInput_summ)
      }
        # error bars
      if(input$errorbar == TRUE){
         p <- p + geom_errorbar(data=df_summary_error(), aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2)
                               #aes_string(x="Condition", ymin="median", ymax="median"), width=.8, size=2, alpha=input$alphaInput_summ)
        # tgc <- summarySE(df_upload_tidy(), measurevar=input$data_column, groupvars= input$data_group)# c("supp","dose"))
        # p <- p + geom_errorbar(aes(ymin=tgc$`input$data_column`-tgc$sd, ymax=tgc$`input$data_column`+tgc$sd), width=.2,
        #               position=position_dodge(.9))
        # p <- p + geom_errorbar()
      }
      
  }else if(input$plot_type == "pca"){
      
  }
########### Do some formatting of the lay-out
     # Setting the order of the x-axis
     # p <- p + scale_x_discrete(limits=custom_order)
     p <- p + theme_light(base_size = 16)
    
    #### If selected, rotate plot 90 degrees CW ####
     rng <- as.numeric(strsplit(input$range,",")[[1]])
 
     # if the range of values is specified    
     if (input$adjust_scale == TRUE) { 
       p <- p + coord_cartesian(ylim=c(rng[1],rng[2]))
     } else if (input$adjust_scale == FALSE)
     {
       rng <- c(NULL,NULL)
     }
    if (input$rotate_plot == TRUE) { p <- p + coord_flip(ylim=c(rng[1],rng[2]))}
    
    # if title specified
    if (input$add_title)
      p <- p + ggtitle(input$title)
    
    # if labels specified
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    
    # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_ttl))
      p <- p + theme(plot.title = element_text(size = input$fnt_sz_leg),
                     legend.title=element_text(size=input$fnt_sz_leg-1), 
                     legend.text=element_text(size=input$fnt_sz_leg-2))
    }
     
    #remove legend (if selected)
    if (input$add_legend == FALSE) {  
      p <- p + theme(legend.position="none")
    }else{
      p <- p + theme(legend.position=input$legend_pos)
    }
     
     # modifiy legend title
     if (input$modi_legend_title == TRUE) {
       p <- p + scale_fill_discrete(name = input$leg_title) # not working
     }

     #remove gridlines (if selected)
     if (input$no_grid == TRUE) {  
       p <- p+ theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
     }
     
      # change colors   
     if(input$plot_type %in% c("boxplot", "barplot", "dotplot")){
       if (input$adjustcolors >1) {
           p <- p+ scale_color_manual(values=newColors)
           p <- p+ scale_fill_manual(values=newColors)
       }
     }
      
    ### Output the plot ######
    p
    
  }) #close output$coolplot


###########################################
#### Render the data summary as a table ###########

output$data_summary <- renderTable({

  df_out <- NULL
  if (input$summaryInput == "mean") {
    df_out <- df_summary_mean()
    df_out$median <- NULL
  } else if (input$summaryInput == "median") {
    df_out <- df_summary_median()
  }  else if (input$summaryInput == "boxplot") {
    df_out <- df_summary_box()
  } else if (input$summaryInput == "violin") {
    df_out <- df_summary_box()
  } 
  return(df_out)
})
###########################################

} #close "server"

shinyApp(ui = ui, server = server)