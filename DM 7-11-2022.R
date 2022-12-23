###############################################################################
# Required Libraries for the Application
###############################################################################

library(shiny);library(readxl);library(flextable);library(dplyr);
library(tibble);library(magrittr);library(pastecs);library(nlme);
library(sqldf);library(bslib);library(rlang);library(shinythemes);
library(shinycssloaders);library(haven);library(Hmisc);
library(arsenal);library(knitr);library(survival);library(r2rtf);
library(tidyverse);library(gtools);library(stringr);
library(spsComps);library(shinyjs);

###############################################################################
# Read Imported Data File Function
###############################################################################

Import = function(dataT,file1) { 
  file = file1
  ext = tools::file_ext(file$datapath)
  req(file)    
  
  if(ext == "csv"){
    validate(need(ext == "csv", "Please upload a csv file"))
    data = read.csv(file$datapath)
    print(data)        
  }
  else if (ext == "xlsx"){
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    data = read_excel(file$datapath)
    print(data)
  }
  else if (ext == "xpt"){
    validate(need(ext == "xpt", "Please upload a xpt file"))
    data = read_xpt(file$datapath)
    print(data)
  }
  else if (ext == "sas7bdat") {
    validate(need(ext == "sas7bdat","Please upload a sas7bdat file"))
    data = read_sas(file$datapath)
    print(data)
  }
}

#############################################################################
## User Interface for the Application
#############################################################################

ui <- fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = NULL),
  
  titlePanel(
    fluidRow(
      column(12,div(style = "height : 50px ; color : green" 
                    , strong("Demographic Characteristics"))
             , align = "center")
    )
  ),
  
  # useShinyjs(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("Demog",h5(paste0("Upload Demographic Data File")
                           , accept = c(".csv",".xlsx","xpt","sas7bdat"))
      ),
      
      # selectInput("trt","Select Option :", 
      #             choices = c("Without Treatment Group", "With Treatment Group")),
      
      fluidRow(column(12,actionButton("update",label =  "Go", class = "glow", 
                                      style='height:px; font-size:100%; boarder: initial; border-color: transparent;'),align = "center")),
      animateUI("update", animation = "bounce", hover = TRUE)
      
      
    ), # End of Sidebarpanel
    
    mainPanel(
      
      tabsetPanel(
        id = "A",
        tabPanel(tags$h6("1. Listing Of Data"),
                 uiOutput("sidebar1"),
                 uiOutput("out1")
        ),
        
        tabPanel(tags$h6("2. Table"),
                 uiOutput("sidebar2"),
                 uiOutput("out2")
                 
        )
        
      ) # End Tabset Panel - 1
      
    ) # End Main Panel
    
  ) # Sidebar Layout
  
) # End UI

#############################################################################
## Server side for the Application
#############################################################################

server <- function(input, output, session) {
  
  # Theme Toggle
  ## bs_themer()
  
  # Action Button reactive
  observeEvent(input$update, {
    runjs(paste0('$("#update").css("animation","")'))
  })
  
#############################################################################
#############################################################################
  
  # Import Data
  Demog = eventReactive(input$update,{
    
    Data = Import(Demog, input$Demog)
    Data
    
    output1 = if (is.null(input$select)) {
      Data
    } else {
      Data = Data[, c(input$select)]
      Data
    }
    output1
   
  }) # end of Demog eventReactive
  
  
  # Raw Data Listing
  output$sidebar1 = renderUI({
    
    data = Demog()
    selectInput("select", "Select the variables:", choices = names(data), multiple = T, selected = NULL)
    
  }) # end of sidebar1 renderUI
  
  # Output of Listing
  output$out1 = renderUI({
    
    input$update
    output = Demog()
    output
    
    output1 = data.frame(output)
    output1 = flextable(output) %>%
      
      add_header_lines() %>%
      add_footer_lines() %>%

      bold(part = "header") %>%

      align(align = "center", part = "header") %>%
      align(align = "center", part = "body")
    htmltools_value(output1)
    
  }) # end of out1 renderTable
  
#############################################################################
############################################################################# 
  
  # Demographic Table function
  Tableby = function(trt, var, data, name){
    control <- tableby.control(test = FALSE,
                               numeric.stats=c("N", "meansd", "medianq1q3", "iqr", "range"),
                               cat.stats=c("countpct"),
                               stats.labels=list(N='n',
                                                 meansd='Mean (SD)', 
                                                 medianq1q3='Median (Q1, Q3)',
                                                 iqr='IQR', 
                                                 range='Minimum, Maximum'))
    
    Table=tableby(trt ~ var, data = data, control = control,
                  test = F, total = F)
    Table
    Table1 = as.data.frame(summary(Table, text = TRUE, digits=2))
    Table1
    
    A = factor(trt)
    B = levels(A)
    
    colnames(Table1) = c("Statistics", B)
    Parameter = name
    
    Table1 = cbind(Parameter,Table1)
    Table1$Parameter = ifelse(Table1$Statistics %in% c("-  Female"), Parameter,
                              ifelse(Table1$Statistics %in% c("-  n"), Parameter,
                                     ifelse(Table1$Statistics %in% c("-  F"), Parameter,"")))
    Table1$Statistics = gsub("- ", "", Table1$Statistics)
    Table1[, c(B)] = gsub(" - ", ", ", Table1[, c(B)])
    colnames(Table1) = c("Parameter", "Statistics", c(B))
    Table1 = Table1[-1, ]
    Table1
    
  } # end of tableby
  
  output$sidebar2 = renderUI({
    
    data = Demog()
    data = Demog()[, c("SEX", "AGE", "HEIGHT", "WEIGHT")]
    selectInput("select1", "Select the variables:", choices = colnames(data), multiple = T, selected = NULL)
    
  })
  
  # Analysis output reactive
  Analysis = reactive({
    
    Data = Demog()
    Data = Data[, c("SEX", "AGE", "HEIGHT", "WEIGHT")]
    
    AA = if(is.null(input$select2)){
      return(NULL)
    } else if (input$select1 == "SEX"){
      Tableby(data$ARM, data$SEX, data, "SEX")
    } else if (input$select1 == "AGE"){
      Tableby(data$ARM, data$AGE, data, "AGE")
    } else if (input$select1 == "HEIGHT"){
      Tableby(data$ARM, data$HEIGHT, data, "HEIGHT")
    } else if (input$select1 == "WEIGHT"){
      Tableby(data$ARM, data$WEIGHT, data, "WEIGHT")
    }
    
    AA = sapply(Data, Tableby())
    AA = print(AA)
      Data5 = flextable(as.data.frame(AA), cwidth = 1.5) %>%        
        set_header_labels(Statistics = "Statistics") %>%
        align(j = 3, align = "center", part ="all") %>%
        
        font(fontname = "Times New Roman", part = "header") %>%
        fontsize(size = 15, part = "header") %>%
        bold(part = "header") %>%
        
        font(fontname = "Times New Roman", part = "body") %>%
        fontsize(size = 11, part = "body")
      Data5

  }) # end of Analysis reactive
  
  # Output of Demographic Table
  output$out2 = renderUI({
    
    output = Analysis()
    output 
    htmltools_value(output)
    
  }) # end of out2 renderUI
  
} # end of server

#############################################################################
## Connects the UI and Server
#############################################################################

shinyApp(ui, server) # Run the Shiny Application