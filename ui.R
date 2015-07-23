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
                     column(1),
                     column(3,
                            h4('Selected Rows:')
                            ),
                     column(3, 
                            selectInput("actionType", label = NULL,
                                        choices = list("Choose Action" = "none",
                                                       "Get FastQ Dump Files" = "fastqdump",
                                                       "Download" = "download",
                                                       "FastQ info" = "fqinfo",
                                                       "Start IGV" = "igv"))
                     ),
                     column(2,
                            conditionalPanel( condition = "input.actionType != 'download'",
                              actionButton("actionButton", label = 'Submit')#icon("arrow-right", lib = "font-awesome"))
                            ),
                            conditionalPanel( condition = "input.actionType == 'download'",
                              downloadButton("downloadSelected", label = 'Submit')              
                              )
                     ),
                     column(2,
                            downloadButton("downloadFullSRA", label = " Download Full SRA Table")
                     )
                     ),
            
                conditionalPanel(condition = "input.actionType == 'fastqdump'",
                                 wellPanel(
                                   fluidRow(
                                   column(2,
                                          radioButtons("fqd_splitStyle", label = "Output Format:",
                                                        choices = list(" .gzip" = "gzip", ".bzip2" = "bzip2", ".fastq" = "fastq"), 
                                                        selected = "gzip")),
                                   column(2,
                                          numericInput('fqd_min', label = "Min Spot ID", value = 0),
                                          numericInput('fqd_max', label = "Max Spot ID", value =0)
                                   ),
                                   column(4,
                                          selectizeInput("fqd_options", label = NULL,
                                                multiple = TRUE,
                                                choices = list("Split Spot", "Skip Technical",
                                                               "Offset", "Original Format",
                                                               "Fasta", "Dump Base", "Dump cs"),
                                                options = list(placeholder = 'Use Options'))
                                   ),
                                   column(4,
                                          shinyDirButton("fqd_outdir",title = "Choose Download Directory for Fastq Files",
                                                         label = "Choose OutDir",
                                                         class = NULL),
                                          verbatimTextOutput('fqd_outdirpath')
                                   )
                                   
                                   )
                                 )
                )
            )
        ),
       hr(),      
       tabsetPanel( id = "tabSet",
         tabPanel( "Results Table", value = "results",
                hr(),
                DT::dataTableOutput('mainTable')),
         tabPanel("operation", value = "operation",
                  uiOutput("operation"))
       )  
          
  )
) 

  
