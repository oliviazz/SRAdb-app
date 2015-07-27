#ui.R    SRAdb-app
#Olivia Zhang  July 9, 2015
#======================#
library(shiny)
library(DT)
library(shinythemes)
library(shinyFiles)
library(shinyBS)
#======================#


shinyUI(fluidPage(theme = shinytheme("cerulean"),
  titlePanel("SRAdb Web Application"),
    wellPanel(
        fluidRow(
            column(4,
                textInput("searchTerms", label = h4("Search Terms:"), 
                          value = "")
            ),
            column(3,
                selectInput("dataType", label = h4("View:"), 
                            choices = list("Study" = "study", "Experiment" = "experiment" ,
                                          "Submission" = "submission",
                                          "Sample" = "sample", "Run" = "run", "SRA" = "sra",
                                          "SRA Accessions" = "sra_acc", 
                                          "SRA Summary" = "srabrief"), selected = "srabrief"
                )
            ),
            column(2,
                hr(),
                actionButton("searchButton",icon("search", lib = "glyphicon"), 
                                                 class = "btn btn-primary")
            )
          )),
        fluidRow(
            column(12,
                   fluidRow(
                    column(3,offset = 1,
                            em(h5('Perform on Selected Rows:'))
                            ),
                     column(3, 
                            selectInput("operationType", label = NULL,
                                        choices = list("Choose Operation" = "none",
                                                       "Export Selected" = "download",
                                                       "Get FastQ Dump Files" = "fastqdump",
                                                       "Get FastQ Info" = "fqinfo",
                                                       "Display Graphic View" = "eGraph",
                                                       "Get SRA Info" = "srainfo",
                                                       "Start IGV" = "igv"))
                     ),
                     column(2,
                            conditionalPanel( condition = "input.operationType != 'download'",
                              actionButton("actionButton", label = 'Submit', class = "btn btn-primary" )
                            ), 
                            #Change button to download button if operation = download
                            conditionalPanel( condition = "input.operationType == 'download'",
                              downloadButton("downloadSelected", label = 'Submit', class = "btn btn-primary")              
                              )
                     ),
                     column(2,
                            downloadButton("downloadFullSRA", label = " Export Full SRA Table")
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
                                                           numericInput('fqd_max', label = "Max SpotID", value = 0)
                                          )
                                   ),
                                   column(4,
                                          shinyDirButton("outdirButton",title = "Choose Download Directory for Fastq Files",
                                                         label = "Choose Download Location:",
                                                         class = "btn btn-link"),
                                          verbatimTextOutput('show_outdirpath'),
                                          shinyFilesButton("fqdCMDButton",title = "Choose Location of FastQ-dump command from SRA toolkit",
                                                         label = "Choose FastQ-dump command location:",
                                                         class = "btn btn-link", multiple = T),
                                          verbatimTextOutput('show_fqdCMDpath'),
                                          selectizeInput("fqd_options", label = "Options",
                                                multiple = TRUE,
                                                choices = list("Split Spot" = "split_spot",
                                                               "Skip Technical" = 'skip_technical',
                                                               "Offset" = "offset", 
                                                               "Original Format" = "origfmt",
                                                               "Fasta" = "fasta",
                                                               "Dump Base" = "dumpbase", 
                                                               "Dump cs" = "dumpcs"),
                                                options = list(placeholder = 'Click to Specify Options'))
                                   ),
                                   column(4,
                                          bsAlert('fqdalert')
                                         )
                                   )
                                )
                              )
                )  
        ),
       hr(), 
       tabsetPanel( id = "tabSet",
         tabPanel( "Search Results", value = "search_results",
                hr(),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div("Loading...",id="loadmessage")
                                ,
                                 tags$style(type="text/css", "
                                   #loadmessage {
                                  position: fixed;
                                  top: 0px;
                                  left: 0px;
                                  width: 100%;
                                  padding: 5px 0px 5px 0px;
                                  text-align: center;
                                  font-weight: bold;
                                  font-size: 100%;
                                  color: #FFFF19;
                                  background-color: #33CCFF;
                                  z-index: 105;
                                  }")
                                 ),
                DT::dataTableOutput('mainTable')
                ),
          tabPanel("Operation Results", value = "operation",
                  column(5, offset = 3,
                          hr(),
                         bsAlert("alert")),
                  conditionalPanel(condition = "input.operationType == 'fqinfo' || 'srainfo'",
                                   dataTableOutput('operationResultsTable')
                                    ),
                  conditionalPanel(condition = "input.operationType == 'eGraph'",
                                   textOutput('test'),
                                   plotOutput('eGraphPlot')
                                   )
                  
                 )
       )  
          
  ))

  
