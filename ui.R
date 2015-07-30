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
  titlePanel(h5(em(("SRAdb Web Application")))),
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
                                          "Sample" = "sample", "Run" = "run", 
                                          "SRA Summary" = "srabrief",
                                          "SRA Accession Codes" = "sra_acc", 
                                          "Full SRA" = "sra"), selected = "srabrief"
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
                                                       "FastQ Dump File" = "fastqdump",
                                                       "Get FastQ Info" = "fqinfo",
                                                       "Get SRA Info" = "srainfo",
                                                       "Display Graphic View" = "eGraph"
                                                       ))
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
                                                           numericInput('fqd_min', label = "Min SpotID", value = NULL),
                                                           numericInput('fqd_max', label = "Max SpotID", value = NULL)
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
                                          bsAlert('fqdalert'),
                                          actionButton("viewFiles", label = "View Download Directory")
                                         )
                              )
                          )
               )
          )  
       ),
       hr(), 
       tabsetPanel( id = "tabSet",
         tabPanel( "Home", value = "home",
                   br(),
                   h2("SRAdb Web Application", align = "center"),
                   h4("Query the NCBI Sequence Read Archive", align = "center"),
                   br(),
                   fluidRow(
                     h4( a("Download SRAdb", href="https://www.bioconductor.org/packages/release/bioc/html/SRAdb.html"), align = "center"),
                     h4( a("Download SRA toolkit", href="http://www.ncbi.nlm.nih.gov/Traces/sra/?view=software"), align = "center"),
                     h4( a("SRA Homepage", href="http://www.ncbi.nlm.nih.gov/Traces/sra/"), align = "center"),
                     h4( "Help", align = "center")     
                     
                   )
                   
         ),
         tabPanel( "Search Results", value = "search_results",
                hr(),
                column(5, offset = 3,
                bsAlert("TBalert")
                ),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div(" . . . . ",id="loadmessage")
                                 ,
                                 tags$style(type="text/css", "
                                            #loadmessage {
                                            position: fixed;
                                            top:6px;
                                            left: 0px;
                                            width: 100%;
                                            padding: 5px 0px 5px 0px;
                                            text-align: center;
                                            font-weight: bold;
                                            font-size: 100%;
                                            color: #FFFFFF ;
                                            background-color: #33CCFF;
                                            z-index: 105;
                                            }")
                                 ),
                DT::dataTableOutput('mainTable')
                ),
        tabPanel("Operation Results", value = "operation",
                column(5, offset = 4,
                          hr(),
                          bsAlert("alert")),
                conditionalPanel(
                  condition="$('html').hasClass('shiny-busy')",
                                 tags$div(" . . . . ",id="loadmessage")
                                 ,
                                 tags$style(type="text/css", "
                                   #loadmessage {
                                  position: fixed;
                                  top: 6x;
                                  left: 0px;
                                  width: 100%;
                                  padding: 5px 0px 5px 0px;
                                  text-align: center;
                                  font-weight: bold;
                                  font-size: 100%;
                                  color: #FFFFFF ;
                                  background-color: #33CCFF;
                                  z-index: 105;
                                  }")
                ),
                          conditionalPanel(condition = "input.operationType == 'fqinfo' || 'srainfo'",
                                   dataTableOutput('operationResultsTable')
                                    ),
                          conditionalPanel(condition = "input.operationType == 'eGraph'",
                                   
                                   plotOutput('eGraphPlot')
                                   )
                 )
       )       
  ))

  
