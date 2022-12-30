###############################################################################
# Required Libraries for the Application
###############################################################################

library(shiny);library(readxl);library(flextable);library(dplyr);
library(tibble);library(magrittr);library(pastecs);library(nlme);
library(sqldf);library(bslib);library(rlang);library(shinythemes);
library(shinycssloaders);library(haven);library(Hmisc);
library(arsenal);library(knitr);library(survival);library(r2rtf);
library(tidyverse);library(gtools);library(stringr);

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
      
      selectInput("trt","Select Option :", 
                  choices = c("Without Treatment Group", "With Treatment Group"))
      
    ), # End of Sidebarpanel
    
    mainPanel(
      
      tabsetPanel(
        id = "A",
        tabPanel(tags$h6("1. Listing Of Data"),
                 uiOutput("sidebar1"),
                 uiOutput("out1")
        ),
        
        tabPanel(tags$h6("2. Table"),
                 # uiOutput("sidebar2"),
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
  # bs_themer()
  
  # Action Button reactive
  
  observeEvent(input$update, {
    runjs(paste0('$("#update").css("animation","")'))
  })
  
  
  # Imported Demographic Data File
  
  # Function to procase the variable names
  # capwords <- function(s, strict = FALSE) {
  #   cap <- function(s) paste(toupper(substring(s, 1, 1)),
  #                            {s <- substring(s, 2); if(strict) tolower(s) else s},
  #                            sep = "", collapse = " " )
  #   sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  # } # end of capwords function
  
  Demog = reactive({
    
    Data = Import(Demog, input$Demog)
    # Data1 = str_to_title(colnames(Data))
    # Data1 = capwords(colnames(Data), strict = T)
    colnames(Data) = str_to_title(colnames(Data))
    Data
    
  }) # end of Demog reactive
  
  # Raw Data Listing
  
  output$sidebar1 = renderUI({
    
    data = Demog()
    BB = levels(factor(data[, 1]))
    data = mixedsort(BB)
    CC = c(BB)
    varSelectInput("select", "Select the variables", data = CC, multiple = T, selected = NULL)
    
  }) # end of sidebar1 renderUI
  
  Listing = reactive({
    
    Listing = Demog()
    Listing
    
    if (is.null(input$select)) {
      Listing = Listing
    } else { 
      Listing = Listing[!(Listing[,1] %in% c(input$select)),]
      Listing 
    }
    Listing
    
    # D = if (!is.na(input$select)) return(Listing)
    # Listing %>% select(!!!input$select)
    # D
    
  }) # end of Listing reactive
  
  output$out1 = renderUI({
    
    output = Listing()
    output
    
    # req(input$select)
    # get(input$select) %>%
    output = data.frame(output)
    output = flextable(output, cwidth = 2) %>%
      
      add_header_lines() %>% 
      add_footer_lines() %>%
      
      bold(part = "header") %>%
      
      align(align = "center", part = "header") %>%
      align(align = "center", part = "body")
    htmltools_value(output)
    
  }) # end of out1 renderTable
  
  # Demographic Table
  
  # Without treatment arm
  Tableby = function(var, data, name){
    Table=tableby( ~ var, data = data,
                   numeric.stats = c("N","meansd","medianq1q3","iqr", "range"),
                   cat.stats = c("countpct"), 
                   stats.labels = list(N = "n",
                                       meansd = "Mean (SD)",
                                       medianq1q3 = "Median (Q1, Q3)",
                                       IQR = "IQR",
                                       range = "Minimum, Maximum"),
                   test = F, total = F)
    Table
    Table1 = as.data.frame(summary(Table, text = TRUE, digits=2))
    Table1
    
    colnames(Table1) = c("Statistics", "Values")
    Parameter = name
    Table1 = cbind(Parameter,Table1)
    Table1$Parameter = ifelse(Table1$Statistics %in% c("-  n"), Parameter,
                              ifelse(Table1$Statistics %in% c("-  Female"), Parameter,
                                     ifelse(Table1$Statistics %in% c("-  F"), Parameter,
                                            ifelse(Table1$Statistics %in% c("-  1"), Parameter,""))))
    Table1$Statistics = gsub("- ", "", Table1$Statistics)
    Table1$Values = gsub(" - ", ", ", Table1$Values)
    Table1$Statistics = format(Table1$Statistics, justify = "left")
    Table1$Values = format(Table1$Values, justify = "centre")
    Table1
    Table1 = Table1[,c("Parameter","Statistics", "Values")]
    Table1 = Table1[-1, ]
    Table1
    
  } # end of tableby
  
  # With treatment arm
  Tableby_grp = function(trt, var, data, name){
    Tr = trt
    
    Table=tableby(trt ~ var, data = data,
                  numeric.stats = c("N","meansd","medianq1q3","iqr", "range"),
                  cat.stats = c("countpct"), 
                  stats.labels = list(N = "n",
                                      meansd = "Mean (SD)",
                                      medianq1q3 = "Median (Q1, Q3)",
                                      IQR = "IQR",
                                      range = "Minimum, Maximum"),
                  test = F, total = F)
    Table
    Table1 = as.data.frame(summary(Table, text = TRUE, digits = 2))
    Table1

    A = factor(trt)
    B = levels(A)

    colnames(Table1) = c("Statistics", B)
    Parameter = name

    Table1 = cbind(Parameter,Table1)
    Table1$Parameter = ifelse(Table1$Statistics %in% c("-  n"), Parameter,
                              ifelse(Table1$Statistics %in% c("-  Female"), Parameter,
                                     ifelse(Table1$Statistics %in% c("-  F"), Parameter,
                                            ifelse(Table1$Statistics %in% c("-  1"), Parameter,""))))
    Table1$Statistics = gsub("- ", "", Table1$Statistics)
    Table1[, c(B)] = gsub(" - ", ", ", Table1[, c(B)])
    colnames(Table1) = c("Statistics", c(B))

    colnames(Table1) = c("Parameter", "Statistics", c(B))
    Table1 = Table1[-1, ]
    Table1
    
  } # end of tableby_grp
  
  # Analysis output reactive
  Analysis = reactive({
    
    Data = Listing()
    
    D = if (input$trt == "Without Treatment Group") {
      
      Data1 = Tableby(as.character(Data$Sex), Data, "Gender n(%)")
      Data2 = Tableby(as.numeric(Data$Age), Data, "Age (Years)")
      Data3 = Tableby(as.numeric(Data$Height), Data, "Height (cm)")
      Data4 = Tableby(as.numeric(Data$Weight), Data, "Weight (kg)")
      Data5 = data.frame(rbind(Data1, Data2, Data3, Data4))
      Data5
      Data5 = flextable(as.data.frame(Data5), cwidth = 2) %>%
        set_header_labels(Statistics = "Statistics",
                          Values = "Values") %>%
        align(j = 3, align = "center", part ="all") %>%
        font(fontname = "Times New Roman", part = "header") %>%
        fontsize(size = 15, part = "header") %>%
        bold(part = "header") %>%
        
        font(fontname = "Times New Roman", part = "body") %>%
        fontsize(size = 11, part = "body")
      Data5
      
    } else if(input$trt == "With Treatment Group") {
      
      Data1 = Tableby_grp(as.character(Data$Arm), as.character(Data$Sex), Data, "Gender n(%)")
      Data2 = Tableby_grp(as.character(Data$Arm), as.numeric(Data$Age), Data, "Age (Years)")
      Data3 = Tableby_grp(as.character(Data$Arm), as.numeric(Data$Height), Data, "Height (cm)")
      Data4 = Tableby_grp(as.character(Data$Arm), as.numeric(Data$Weight), Data, "Weight (kg)")
      Data5 = data.frame(rbind(Data1, Data2, Data3, Data4))
      Data5
      Data5 = flextable(as.data.frame(Data5), cwidth = 2) %>%
        set_header_labels(Statistics = "Statistics",
                          Group.1 = "Group 1",
                          Group.2 = "Group 2") %>%
        align(j = 3, align = "center", part ="all") %>%
        font(fontname = "Times New Roman", part = "header") %>%
        fontsize(size = 15, part = "header") %>%
        bold(part = "header") %>%
        
        font(fontname = "Times New Roman", part = "body") %>%
        fontsize(size = 11, part = "body")
      Data5
    }
    D
  }) # end of Analysis reactive
  
  output$out2 = renderUI({
    
    output = Analysis()
    output 
    htmltools_value(output)
    
  }) # end of out2 renderUI
  
} # end of server

#############################################################################
##  Connects the UI and Server
#############################################################################

shinyApp(ui, server) # Run the Shiny Application