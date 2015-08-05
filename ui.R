#ui.R    SRAdb-app
#Olivia Zhang  July 9, 2015
#======================#
Libs = c('shiny', 'DT', 'SRAdb', 'shinyBS', 'Rgraphviz', 'shinythemes', 'shinyFiles')
for( Lib in Libs ) {
  if( !require( Lib, character.only = T ) ) {
    source("http://bioconductor.org/biocLite.R")
    biocLite( Lib, type='source', lib = .libPaths()[2] )
    library( Lib, character.only = T )
  }
}
#======================#
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  tags$head(
                    tags$style(HTML(".shiny-progress .bar {
                                                  background-color: #39FF14;
                                                  .opacity = 0.8;
                                                  }
                                    .shiny-progress .progress {
                                                  height:8px;
                                                  }
                                    
                                    "))
                    ),
  titlePanel(h4(em("SRAdb Web Application"))),
    wellPanel(
        fluidRow(
            column(4,
                textInput("searchTerms", label = h5("Search Terms:"), 
                          value = "")
            ),
            column(3,
                selectInput("dataType", label = h5("View:"), 
                            choices = list("SRA Overview" = "srabrief",
                                          "Study" = "study", "Experiment" = "experiment" ,
                                          "Submission" = "submission",
                                          "Sample" = "sample", "Run" = "run", 
                                          "Full SRA" = "sra",
                                           "Accession Codes" = "acc_table"), selected = "srabrief"
                )
            ),
            column(1,
                br(),br(),
                actionButton("searchButton",icon("search", lib = "glyphicon"), 
                                                 class = "btn btn-primary")
            ),
            column(3,
                   br(), br(),
                   h6(textOutput("smartSearch"))
                   )
          )),
        
  br(),
       tabsetPanel( id = "tabSet",
         tabPanel( "Home", value = "home",
                   wellPanel(
                   br(),
                   h2("SRAdb Web Application", align = "center"),
                   h4("Query the NCBI Sequence Read Archive", align = "center"),
                   br(),
                   fluidRow(
                     h4( a("SRA Homepage", href="http://www.ncbi.nlm.nih.gov/Traces/sra/"), align = "center"),
                     h4( a("Download SRAdb", href="https://www.bioconductor.org/packages/release/bioc/html/SRAdb.html"), align = "center"),
                     h4( a("Download SRA toolkit", href="http://www.ncbi.nlm.nih.gov/Traces/sra/?view=software"), align = "center"),
                     h4( a("Help"), align = "center"),     
                   
                   column(4,offset = 4, 
                          DT::dataTableOutput("instr_models")
                   )
                   ))
                   
         ),
         tabPanel( "Search Results", value = "search_results",
                   br(),
                   bsAlert("TBalert"),
                conditionalPanel( "input.searchButton > 0",
                                  wellPanel(
                                  fluidRow(
                                    column(12,
                                           fluidRow(
                                             column(2, offset = 1, (h6(textOutput('selectHelp')))),
                                            column(4,
                                                    selectizeInput("operationType", label = NULL,
                                                                   choices = list("Get All Related Accession Codes" = "related_acc",
                                                                                  "Export Selected" = "download",
                                                                                  "Get FastQ Info" = "fqinfo",
                                                                                  "Get SRA Info" = "srainfo",
                                                                                  "FastQ Dump File" = "fastqdump",
                                                                                  "Display Entity Relationship Graph" = "eGraph"
                                                                   ),
                                                                   options = list(
                                                                     placeholder = 'Perform on Selected Rows:   ',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                                   ))
                                             ),
                                             column(2,
                                                    conditionalPanel( condition = "input.operationType != 'download'",
                                                                      actionButton("actionButton", label = 'Submit', class = "btn btn-primary" )
                                                    ), 
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
                                                                column(3,
                                                                       radioButtons("fqd_splitStyle", label = "Output Format:",
                                                                                    choices = list(" .gzip" = "gzip", ".bzip2" = "bzip2", ".fastq" = "fastq"), 
                                                                                    selected = "gzip"),
                                                                       selectizeInput("fqd_options", label = "FastQ Dump Options:",
                                                                                      multiple = TRUE,
                                                                                      choices = list("Split Spot" = "split_spot",
                                                                                                     "Skip Technical" = 'skip_technical',
                                                                                                     "Offset" = "offset", 
                                                                                                     "Original Format" = "origfmt",
                                                                                                     "Fasta" = "fasta",
                                                                                                     "Dump Base" = "dumpbase", 
                                                                                                     "Dump cs" = "dumpcs"),
                                                                                      options = list(placeholder = 'Click to specify options')),
                                                                       checkboxInput("fullFile", label = strong("Get Entire File"), value = TRUE),
                                                                       conditionalPanel(condition = "!input.fullFile",
                                                                                        fluidRow(column(6, numericInput('fqd_min', label = "Min SpotID", value = NULL)
                                                                                                        ),
                                                                                                 column(6,numericInput('fqd_max', label = "Max SpotID", value = NULL)
                                                                                                        )
                                                                                                 )
                                                                       )
                                                                ),
                                                               column(5, 
                                                                      fluidRow(
                                                                        column(10,textInput('show_outdirpath', label = "Download Location:"),
                                                                               textInput('fqdPlaces', label = "Fastq-dump Command Location:", value = '')
                                                                               ),
                                                                                                                                
                                                                        column(1, br(), 
                                                                               shinyDirButton("outdirButton",title = "Choose Download Directory for Fastq Files",
                                                                                                label = "Browse",
                                                                                                class = "btn btn-link")
                                                                               )
                                                                      )
                                                                      ),
                                                                column(4, 
                                                                       fluidRow(
                                                                         column(2),
                                                                         column(10,bsAlert('fqdalert')
                                                                                )
                                                                       ),
                                                                       br(),
                                                                       fluidRow(
                                                                         column(4),
                                                                         column(8,  actionButton("viewFiles", label = "View Download Directory")
                                                                                )
                                                                       )
                                                                     
                                                                )
                                                              )
                                                            )
                                           )
                                    )  
                                  )
                                  
                                  )),
               
                br(),
                
                DT::dataTableOutput('mainTable')
                ),
        tabPanel("Operation Results", value = "operation",
                column(5, offset = 4,
                          br(),
                          bsAlert("alert")),
                          conditionalPanel(condition = "input.operationType == 'fqinfo' || 'srainfo' || 'related_acc'",
                                   dataTableOutput('operationResultsTable')
                                    ),
                          conditionalPanel(condition = "input.operationType == 'eGraph'",
                                   plotOutput('eGraphPlot')
                                   )
                 )
       )       
  ))

  
