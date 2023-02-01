library(shiny)
library(tidyselect)
library(shinydashboard)


e = new.env()

e1 = new.env()

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "side_bar",
    menuItem("Introduction", tabName = "Introduction", icon = icon("house")),
    menuItem("Importation", icon = icon("cloud-upload"), tabName = "Importation"),
    menuItem("AED", tabName = "AED", icon = icon("chart-column")),
    menuItem("APD", icon = icon("sitemap"), tabName = "APD"),
    conditionalPanel(
      condition = "input.side_bar == 'Importation'",
      selectInput(inputId = "data_type", "Select a Data Type",
                  choices = c("Txt File", "Excel File", "Rdata File")),
      # Txt:
      conditionalPanel(
        condition = "input.data_type == 'Txt File'",
        
        fileInput(inputId = "file_txt", label = "Choose a File"),
        checkboxInput(inputId = "col_name", label = "First Row as Names ?", value = TRUE),
        radioButtons(inputId = "delim", "Select a Delim",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t",
                                 Other = "Other"),
                     selected = ","),
        conditionalPanel(
          condition = "input.delim == 'Other'",
          textInput("delim_Autre", label = "Delimator")
        ),
        radioButtons(inputId = "sep_dec", "Select what separates the Decimal Number", 
                     choices = c(Comma = ",", Point = "."), selected = "."),
        checkboxInput("skip", label = "Ignore First Rows ?"),
        conditionalPanel(
          condition = "input.skip",
          textInput("skipNb", label = "Row Nomber", value = 0))
      ),
      # Excel :
      conditionalPanel(
        condition = "input.data_type == 'Excel File'",
        fileInput(inputId = "file_excel", label = "Choose a File"),
        checkboxInput("col_name", label = "First Row as Names ?", value = TRUE),
        selectInput(inputId = "sheet_choice", label = "What Sheet to Import?", choices = NULL)
      ),
      # Rdata :
      conditionalPanel(
        condition = "input.data_type == 'Rdata File'",
        fileInput(inputId = "file_rdata", label = "Choose a File"),
        selectInput(inputId = "Rdata_choice", label = "Choose The Data to Import", choices = NULL)
      ),         
      actionButton("button", "Show")  
    )
  )
)

body <- dashboardBody(
  tabItems(
    
    
    ##### ============================================ tab 1 =================================================
    tabItem(tabName = "Introduction",
            h1("Machine Learning Web App"),
            h2("Introductin"),
            verbatimTextOutput("intro")
    ),
    
    ##### ============================================ tab 2 =================================================
    
    tabItem(tabName = "Importation",
            h2("Widgets tab content"),
            tabsetPanel(
              tabPanel("Overview", value = "Overview",
                       DT::dataTableOutput(outputId = "tab")
              ),
              tabPanel("Processing", value = "Processing",
                       column(3,
                              selectInput(inputId = "tran", "Choose a Transformation", choices = c("Conversion", "Mutation"))),
                       column(3,
                              selectInput(inputId = "class", "Choose a Class ", choices = c("as_integer",
                                                                                            "as_numeric",
                                                                                            "as_character",
                                                                                            "as_factor",
                                                                                            "as_mdy",
                                                                                            "as_dmy",
                                                                                            "as_ymd",
                                                                                            "as_ymd_hms",
                                                                                            "as_ymd_hm",
                                                                                            "as_mdy_hms",
                                                                                            "as_mdy_hm",
                                                                                            "as_dmy_hms",
                                                                                            "as_dmy_hm",
                                                                                            "as_hms",
                                                                                            "as_hm")),
                              actionButton("button1", "Convert")),
                       
                       box(DT::DTOutput(outputId = "trans_tab"), width = 8),
                       
                       uiOutput("col"),
                       
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }")
                       
              ),
              tabPanel("Summary", value = "Summary",
                       verbatimTextOutput(outputId = "sum")
              )
              
            )
    ),
    ##### ============================================ tab 3 =================================================
    tabItem(tabName = "AED",
            h2("Analyse Exploratoire")
    ),
    
    
    ##### ============================================ tab 4 =================================================
    tabItem(tabName = "APD",
            h2("Analyse Predictive"))
  )
)


# Put them together into a dashboardPage

ui <-dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)

server <- function(input, output, session){
  
  ### ===================Intro 
  output$intro <- renderText(" Hello !")
  
  ##### ==============Importation :
  
  ## Excel :
  
  observeEvent(input$file_excel, {
    
    input$file_excel
    
    df_list <- purrr::map(purrr::set_names(readxl::excel_sheets(input$file_excel$datapath)),
                          readxl::read_excel, path = input$file_excel$datapath)
    
    lapply(names(df_list), function(x)
      assign(x, df_list[[x]], envir = e))
    
    listes = ls(name = df_list, e)
    
    updateSelectInput(session = session, inputId = "sheet_choice", choices = listes)
    
  })
  
  donneesExcel <- reactive({
    if (is.null(input$file_excel)) return (NULL)
    don = NULL
    try({
      don = readxl::read_excel(
        input$file_excel$datapath,
        sheet = input$sheet_choice,
        col_names = input$col_name)
    })
    don
  })
  
  # Txt :
  
  donneesTXT <- reactive({
    if (is.null(input$file_txt)) return (NULL)
    else {
      if (input$delim == 'Other') sep = input$delim_Autre
      else sep = input$delim
      don = NULL
      try({
        don = read.table(
          input$file_txt$datapath,
          header = input$col_name,
          sep = sep,
          dec = input$sep_dec,
          skip = input$skipNb,
          stringsAsFactors = FALSE)
      }, silent = TRUE)
      don
      
    }
  })
  
  # Rdata:
  
  observeEvent(input$file_rdata, {
    
    input$file_rdata
    
    load(input$file_rdata$datapath, envir = e1)
    
    liste = ls(e1)
    
    updateSelectInput(session = session, inputId = "Rdata_choice", choices = liste)
  })
  
  donneesRDATA <- reactive({
    don = NULL
    try({
      don = get(input$Rdata_choice, envir = e1)
    }, silent = TRUE)
    don
  })
  
  ## Import Data
  donnee <- reactive({
    don = NULL
    if (input$data_type == "Txt File") {
      don = donneesTXT()
    } else if (input$data_type == "Excel File"){
      don = donneesExcel()
    } else {
      if (input$data_type == "Rdata File") {
        don = donneesRDATA()
      } else {
        don = get(input$data_type)
      }
    }
    don
    
  })
  
  #########=========================== Affichage 
  
  df_tab <- eventReactive(input$button, {
    donnee()
  })
  
  output$tab <- DT::renderDataTable({
    df_tab()
  })

  #### ======================= Summary 
  
  output$sum <-  renderPrint({
    skimr::skim_without_charts(donnee())
  })

  
}

shinyApp(ui, server)