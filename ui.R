#ui.R SRAdb-app
library(shiny)
library(DT)


shinyUI(fixedPage(
  titlePanel("Query SRA using SRAdb"),
  verticalLayout( 
    
    inputPanel(
      column( 12,
        textInput("searchTerms", label = h4("Search Terms:"), 
                  value = ""),
        checkboxInput("exactMatch", label = "Exact Text Search", 
                      value = FALSE)
      ),
      column( 12, selectInput("dataType", label = h4("Result Type:"), 
                          choices = list("Study" = "study", "Experiment" = "experiment" ,
                                         "SRA" = "sra","Submission" = "submission",
                                         "Sample" = "sample", "Run" = "run"),
                          selected = "study"),
              
              checkboxInput("acc_only", label ="Display Accession Codes Only", 
                            value = FALSE)
              
      ),
      column( 12,
            textInput("fastqcode", label = h5("Get Fast-Q File"), 
                  value = ""),
            textOutput('fqdmessage'),
            actionButton("fqdbutton", label = h6("Enter"))
            ),
      column(12,
             actionButton("searchButton", label = h2("Search")
                          )
             )
      ), #end input Panel
         

    mainPanel(
      
      fixedRow(
          column(
            10, DT::dataTableOutput('mainTable'),
            conditionalPanel(
              condition = "input.acc_only == true",
              textOutput('errorBoard')
            )
            )
        )      
      )# mainPanel
  )#sidebarLayout 
  
))