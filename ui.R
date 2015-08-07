#ui.R SRAdb-app
#Olivia Zhang  July 9, 2015
#======================#
Libs = c('shiny', 'DT', 'SRAdb', 'shinyBS', 'Rgraphviz', 'shinythemes', 'shinyFiles')
for( Lib in Libs ) {
  if( !require( Lib, character.only = T ) ) {
    print(Lib)
    source("http://bioconductor.org/biocLite.R")
    biocLite( Lib, type='source', lib = .libPaths()[2] )
    library( Lib, character.only = T )
  }
}
#======================#
shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  tags$head(tags$style(HTML(".shiny-progress .bar {
                                                  background-color: #43B33C;
                                                  .opacity = .8;
                                                  }
                                    .shiny-progress .progress {
                                                  height:6px;
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
                                           "Accession Codes" = "acc_table"), selected = "srabrief")
            ),
            column(1,
                br(),br(),
                actionButton("searchButton", label =  "Search", #icon("search", lib = "glyphicon"), 
                                                 class = "btn btn-primary")
            ),
            column(3,
                   br(), br(),
                   h6("Advanced Search [?]", id = "smartSearch")
                   )
          )),      
  br(),
       tabsetPanel( id = "tabSet",
         tabPanel( "Home", value = "home",
                   wellPanel(
                   br(),
                   h2("SRAdb Web Application", align = "center"),
                   h4(em("Query the NCBI Sequence Read Archive"), align = "center"),
                   br(),
                   h4( a("Current SRA Data Amounts:"),align = "center"),
                   fluidRow(
                  column(4,offset = 4, 
                          DT::dataTableOutput("instr_models")
                   )),
                  br(),
                  h4( a("SRA Homepage", href="http://www.ncbi.nlm.nih.gov/Traces/sra/"), align = "center"),
                  h4( a("Download SRAdb", href="https://www.bioconductor.org/packages/release/bioc/html/SRAdb.html"), align = "center"),
                  h4( a("Download SRA toolkit", href="http://www.ncbi.nlm.nih.gov/Traces/sra/?view=software"), align = "center"),
                  h4( a("Help"), align = "center")
                   )
                   
         ),
         tabPanel( "Search Results", value = "search_results",
                   br(),
                   bsAlert("TBalert"),
                   conditionalPanel( "input.searchButton > 0",
                                  wellPanel(
                                    fluidRow(
                                      column(12,
                                           fluidRow(
                                              column(2, offset = 1, (h6("Select Rows [?]", id = 'selectHelp'))),
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
                                                                      )
                                                                   )
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
                                                              column(2,
                                                                       radioButtons("fqd_zipFormat", label = "Output Format:",
                                                                                    choices = list(" .gzip" = "gzip", ".bzip2" = "bzip2", ".fastq" = "fastq"), 
                                                                                    selected = "gzip"), 
                                                                       selectizeInput("fqd_options", label = " Options:",
                                                                                      multiple = TRUE,
                                                                                      choices = list("Split Spot" = "split_spot",
                                                                                                     "Skip Technical" = 'skip_technical',
                                                                                                     "Offset" = "offset", 
                                                                                                     "Original Format" = "origfmt",
                                                                                                     "Fasta" = "fasta",
                                                                                                     "Dump Base" = "dumpbase", 
                                                                                                     "Dump cs" = "dumpcs"),
                                                                                      options = list(placeholder = 'Click to show:'))
                                                                       
                                                                       
                                                                ),
                                                              column(3, 
                                                                     column(1),
                                                                     column(11,
                                                                     radioButtons("fullFile", label = NULL, choices = c("Get Entire File" = "entire", "Custom Range" = "range")) ,
                                                                     conditionalPanel(condition = "input.fullFile == 'range'",
                                                                                      numericInput('fqd_min', label = "Min SpotID", value = NULL),
                                                                                      numericInput('fqd_max', label = "Max SpotID", value = NULL)
                                                                     ))
                                                              ),
                                                                column(4,
                                                                       fluidRow(
                                                                              column(11,
                                                                                textInput('show_outdirpath', label = "Download Directory:"),
                                                                                shinyDirButton("outdirButton",title = "Choose Download Directory",
                                                                                               label = "Browse Download Directory ", class = "btn-block btn-link"),
                                                                                br(),
                                                                                textInput('fqdPlaces', label = "Fastq-dump Command Location:", value = ''))
                                                                       ),
                                                                       br()
                                                                       
                                                                        
                                                                ),
                                                              
                                                                
                                                                column(3,
                                                                       br(),
                                                                       bsButton("viewFiles", label = "View Download Directory", disabled = TRUE,block = TRUE))
                                                               ),
                                                                fluidRow(bsAlert('fqdalert')
                                                                )
                                                                     
                                                                ))
                                                          ))
                                      )), #end well Panel
                br(),
                DT::dataTableOutput('mainTable')
                ),
        tabPanel("Operation Results", value = "operation",
                          br(),
                          bsAlert("alert"),
                          conditionalPanel(condition = "input.operationType == 'fqinfo' || 'srainfo' || 'related_acc'",
                                   dataTableOutput('operationResultsTable')
                                   ),
                          conditionalPanel(condition = "input.operationType == 'eGraph'",
                                   plotOutput('eGraphPlot')
                                   )
        ))#End Tabset Panel       
  ))

  
