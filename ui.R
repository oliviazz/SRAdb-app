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
  titlePanel(h6(em(strong("SRAdb Web Application")))),
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
                                          "Full SRA" = "sra"), selected = "srabrief"
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
                     h4( "Help", align = "center")     
                   )
                   )
                   
         ),
         tabPanel( "Search Results", value = "search_results",
                column(5, offset = 4,
                       br(),
                bsAlert("TBalert")
                ),
                conditionalPanel( "input.searchTerms != '' && input.searchButton > 0",
                                  wellPanel(
                                  fluidRow(
                                    column(12,
                                           fluidRow(
                                             column(2, offset = 1, em(h6(textOutput('selectHelp')))),
                                            column(4,
                                                    selectizeInput("operationType", label = NULL,
                                                                   choices = list("Export Selected" = "download",
                                                                                  "Get Related Accession Codes" = "related_acc",
                                                                                  "Get FastQ Info" = "fqinfo",
                                                                                  "Get SRA Info" = "srainfo",
                                                                                  "FastQ Dump File" = "fastqdump",
                                                                                  "Display ERD" = "eGraph"
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
                                                                                      label = "Browse Download Location:",
                                                                                      class = "btn btn-link"),
                                                                       textInput('show_outdirpath', label = NULL),
                                                                       shinyFilesButton("fqdCMDButton",title = "Browse Location of FastQ-dump command from SRA toolkit",
                                                                                        label = "Browse FastQ-dump command location:",
                                                                                        class = "btn btn-link", multiple = T),
                                                                       textInput('show_fqdCMDpath', label = NULL),
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
                                  ))),
                br(),
                bsAlert("TBalert"),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                 tags$div(" . . . . ",id="loadmessage"),
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
                                            background-color: #ffff00 ;
                                            z-index: 105;
                                            }")
                ),
                DT::dataTableOutput('mainTable')
                ),
        tabPanel("Operation Results", value = "operation",
                column(5, offset = 4,
                          br(),
                          bsAlert("alert")),
                conditionalPanel(
                  condition="$('html').hasClass('shiny-busy')",
                                 tags$div(" . . . . ",id="oloadmessage"),
                                 tags$style(type="text/css", "
                                   #oloadmessage {
                                  position: fixed;
                                  top: 6x;
                                  left: 0px;
                                  width: 100%;
                                  padding: 5px 0px 5px 0px;
                                  text-align: center;
                                  font-weight: bold;
                                  font-size: 100%;
                                  color: #FFFFFF ;
                                  background-color: #ffff00 ;
                                  z-index: 105;
                                  }")
                ),
                          conditionalPanel(condition = "input.operationType == 'fqinfo' || 'srainfo' || 'related_acc'",
                                   dataTableOutput('operationResultsTable')
                                    ),
                          conditionalPanel(condition = "input.operationType == 'eGraph'",
                                   plotOutput('eGraphPlot')
                                   )
                 )
       )       
  ))

  
