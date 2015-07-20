#ui.R SRAdb-app
library(shiny)
library(DT)
library(shinythemes)

teal_color = '#008B8B'

shinyUI(fixedPage(theme = shinytheme("flatly"),
  titlePanel("SRAdb Web App"),
   navbarPage("Pages:",
      tabPanel(
              "Text Search",
              wellPanel(
              fixedRow(
                column(6,
                     textInput("searchTerms", label = h4("Search Terms:"), 
                               value = "Flamingo"),
                     checkboxInput("exactMatch", label = "Exact Text Search", 
                                   value = FALSE)
                ),
                column(4,
                     selectInput("dataType", label = h4("Result Type:"), 
                                 choices = list("Study" = "study", "Experiment" = "experiment" ,
                                                "SRA" = "sra","Submission" = "submission",
                                                "Sample" = "sample", "Run" = "run", 
                                                "SRA Accessions" = "sra_acc"), selected = "run"
                                                #"SRA Brief" = "sra_brief", "SRA Accession" = "sra_acc"
                                                
                                 ),
                     checkboxInput("acc_only", label ="Display Accession Codes Only", 
                                   value = FALSE)
                ),
                column(1,
                       hr(),
                    actionButton("searchButton",icon("search", lib = "glyphicon"))
              )
              )),#end Fluid Row and WellPanel
          #hr(), 
          fixedRow(
            column(5,
            selectInput("actionType", label = h4("Action:"), choices = list("Get FastQ Dump Files" = 1, 
                                                                   "Start IGV" = 2))),
            column(1,
            hr(),
            actionButton("actionButton", icon("arrow-right", lib = "font-awesome"))
            ),
            column(6,
            conditionalPanel(condition = "input.action == 1",
                             wellPanel(radioButtons("splitStyle", label = "Output File Format:",
                                          choices = list(" .gzip" = "gzip", ".bzip2" = "bzip2", ".fastq" = "fastq"), 
                                          selected = 1))
                             )
                   )
          ),
             
          
          DT::dataTableOutput('mainTable'),
          downloadButton("downloadSearchResults", label = "Download Results")
          
          
          ),
      tabPanel( "Download FastQ Files",
           wellPanel(
              fluidRow(
                column(4,textInput("fastqcode", label = h4("Accession Code:"), 
                                   value = ""))
                      ),
              h4("Download Options:"),
              hr(),
              fluidRow(
                column(3,
                radioButtons("splitStyle", label = "Output File Format:",
                             choices = list(" .gzip" = "gzip", ".bzip2" = "bzip2", ".fastq" = "fastq"), 
                             selected = 1)
                ),
                column(3,
                       textInput( "outDir",label = "Specify Desired Download Location")
                       ),
                
              column(2,
                     fileInput("justLook", label = "Browse to check downloaded")
                     )
              ),
              downloadButton("fastqDump", label = h6("Download")),
              textOutput("fastqMessage")
           )
      )
   ) #end tabsetPanel 

)#sidebarLayout 
  
)