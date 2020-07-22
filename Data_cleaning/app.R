#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data cleaning"),
    tabsetPanel(
        tabPanel("Data loading",
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         fileInput("file1", "Choose CSV File",
                                   accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv",
                                       ".xlsx",
                                       ".xls")
                         ),
                         tags$hr(),
                         checkboxInput("header", "Header", TRUE)
                         
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         tableOutput("contents")
                     ))
        ),
        tabPanel("Row cleaning",
                 sidebarLayout(
                     sidebarPanel(
                         actionButton("remove", "Remove selected rows")
                     ),
                     
                     mainPanel(
                         DT::dataTableOutput("editRowContents")
                     )
                 )),
        tabPanel("Column cleaning",
                 sidebarLayout(
                     sidebarPanel(
                         actionButton("remove2", "Remove selected columns")
                     ),
                     
                     mainPanel(
                         DT::dataTableOutput("editColumnContents")
                     )
                 )),
        tabPanel("Test",
                 sidebarLayout(
                     sidebarPanel(),
                     mainPanel(
                         DT::dataTableOutput("proxyRowContents")
                     )
                 ))
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    loadedData <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        if(endsWith(inFile$type, "csv")) 
            read.csv(inFile$datapath, header = input$header)
        else if(endsWith(inFile$datapath, "xlsx"))
            readxl::read_xlsx(inFile$datapath, col_names = input$header)
        else if(endsWith(inFile$datapath, "xls"))
            readxl::read_xls(inFile$datapath, col_names = input$header)
    })
    output$contents <- renderTable({
        head(loadedData())
    })
    output$editRowContents <- DT::renderDataTable(
        loadedData(),
        server = TRUE,
        selection = 'multiple'
    )
    
    ### INFO ON USING DT
    #https://rstudio.github.io/DT/shiny.html
    
    #### INFO ON USING PROXY
    #https://yihui.shinyapps.io/DT-proxy/
    
    proxyRowContents = DT::dataTableProxy('editRowContents')
    
    observeEvent(input$remove, {
        proxyRowContents %>% selectRows(as.numeric(input$rows))
    })
    
    output$proxyRowContents = DT::renderDataTable(
        proxyRowContents
    )
    
    
    selectedContents <- reactive({
        loadedData()[-c(input$editRowContents_rows_selected),]
    })
    

    
    output$test <- DT::renderDataTable(
            selectedContents(),
            selection = 'multiple'
        )
    
    
    
    output$editColumnContents <- DT::renderDataTable(
        #will need to draw on final editRowContents version, not loadedData()
        loadedData(),
        selection = list(target = "column", selection = 'multiple')
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
