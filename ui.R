#ui.R    SRAdb-app
#Olivia Zhang  July 9, 2015
#======================#
library(shiny)
library(DT)
library(shinythemes)
library(shinyFiles)
#======================#


shinyUI(fluidPage(theme = shinytheme("cerulean"),
  titlePanel("SRAdb Web App"),
    wellPanel(
        fluidRow(
            column(4,
                textInput("searchTerms", label = h4("Search Terms:"), 
                          value = "")
            ),
            column(3,
                selectInput("dataType", label = h4("View:"), 
                            choices = list("Study" = "study", "Experiment" = "experiment" ,
                                          "SRA" = "sra","Submission" = "submission",
                                          "Sample" = "sample", "Run" = "run", 
                                          "SRA Accessions" = "sra_acc", 
                                          "SRA Summary" = "srabrief"), selected = "srabrief"
                )
            ),
            column(2,
                hr(),
                actionButton("searchButton",icon("search", lib = "glyphicon"))
            )
          )),
        fluidRow(
            column(12,
                   fluidRow(
                    
                     column(3,offset = 1,
                            h5('Operate on Selected Rows:')
                            ),
                     column(3, 
                            selectInput("operationType", label = NULL,
                                        choices = list("Choose Action" = "none",
                                                       "Get FastQ Dump Files" = "fastqdump",
                                                       "Download" = "download",
                                                       "FastQ info" = "fqinfo",
                                                       "Start IGV" = "igv"))
                     ),
                     column(2,
                            conditionalPanel( condition = "input.operationType != 'download'",
                              actionButton("actionButton", label = 'Submit', class = "btn btn-primary" )
                            ), #different button depending on operationType
                            conditionalPanel( condition = "input.operationType == 'download'",
                              downloadButton("downloadSelected", label = 'Submit')              
                              )
                     ),
                     column(2,
                            downloadButton("downloadFullSRA", label = " Download Full SRA Table")
                     )
                     ),
            
                conditionalPanel(condition = "input.operationType == 'fastqdump'",
                                 wellPanel(
                                   fluidRow(
                                   column(2,
                                          radioButtons("fqd_splitStyle", label = "Output Format:",
                                                       choices = list(" .gzip" = "gzip", ".bzip2" = "bzip2", ".fastq" = "fastq"), 
                                                       selected = "gzip"),
                                          checkboxInput("fullFile", label = strong("Get Entire File"), value = TRUE)
                                   ),
                                   column(2,
                                          conditionalPanel(condition = "!input.fullFile",
                                                           numericInput('fqd_min', label = "Min SpotID", value = 0),
                                                           numericInput('fqd_max', label = "Max SpotID", value =0)
                                          )
                                   ),
                                   column(4,
                                          shinyDirButton("outdirButton",title = "Choose Download Directory for Fastq Files",
                                                         label = "Choose Download Location:",
                                                         class = "btn btn-link"),
                                          verbatimTextOutput('show_outdirpath'),
                                          selectizeInput("fqd_options", label = "Options",
                                                multiple = TRUE,
                                                choices = list("Split Spot", "Skip Technical",
                                                               "Offset", "Original Format",
                                                               "Fasta", "Dump Base", "Dump cs"),
                                                options = list(placeholder = 'Click to Specify Options'))
                                          
                                   ),
                                   column(4,
                                          conditionalPanel(condition = "input.viewFiles == 'finished'",
                                          shinyFilesButton("viewFiles", label = "View Files",
                                                           title = "View Files", class = NULL, multiple = FALSE)
                                          )
                                   )
                                )
                              )
                )
            )
        ),
       hr(),  
  
       tabsetPanel( id = "tabSet",
         tabPanel( "Search Results", value = "results",
                hr(),
                DT::dataTableOutput('mainTable')),
         tabPanel("Operation Results", value = "operation",
                uiOutput("operationResults")
       ))  
          
  )
) 

  
