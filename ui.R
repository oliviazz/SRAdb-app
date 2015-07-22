#ui.R SRAdb-app
library(shiny)
library(DT)
library(shinythemes)

teal_color = '#008B8B'

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
          )),#end Fluid Row and WellPanel
           
        fixedRow(
            column(12,
                   fixedRow(
                     
                     column(4, 
                            selectInput("actionType", label = NULL,
                                        choices = list("Choose Action" = "none",
                                                       "Get FastQ Dump Files" = "fastqdump",
                                                       "Download" = "download",
                                                       "FastQ info" = "fqinfo",
                                                       "Start IGV" = "igv"))
                     ),
                     column(5,
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
                                 wellPanel(radioButtons("splitStyle", label = "Output File Format:",
                                                        choices = list(" .gzip" = "gzip", ".bzip2" = "bzip2", ".fastq" = "fastq"), 
                                                        selected = "gzip")
                                 )
                )
            )
        ),
             
       fixedRow(   
         column(12,
                hr(),
                DT::dataTableOutput('mainTable'))
       )  
          
  )
) 

  
