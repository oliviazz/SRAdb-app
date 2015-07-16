#server.R  #SRAdb-app Olivia Zhang   July 9, 2015 

#Load required packages
library(shiny)
library(SRAdb)
library(DT)

sra_dbname <- 'SRAmetadb.sqlite'	
if(!file.exists('SRAmetadb.sqlite'))
  sqlfile <<- getSRAdbFile()
sra_con<- dbConnect(dbDriver("SQLite"), sra_dbname) 
source('~/sra_toolkit/bin/fastqDump_v1.R')

#initialize columns 

  colSettings = list(list(),list(),list(),list(),list())
  names(colSettings) = c("study", "run", "sample", "submission", "experiment")
  print(paste("Column Settings: at begining",colSettings))

#Start ShinyServer Function
shinyServer(function(input,output){
  
#####Reactive functions 
  
  
  getFullTable <- reactive({
    #Returns full table given dataType 
    dataType <- input$dataType
     
      if (input$exactMatch == FALSE)
        searchTerms <- paste0(input$searchTerms,'*')
      else
        searchTerms <- input$searchTerms
      
      n = getSRA(search_terms = searchTerms, sra_con = sra_con, out_types = dataType,
                    acc_only = input$acc_only)
     
  })
  
  getColumns <- reactive({
    
    columns <- input$col_choices  #this is saving the "old" column settings from previous. 
    if (length(columns) == 0 || ! (columns %in% colnames(getFullTable())))
       #if no preferences entered yet or is left over from past
      columns <- colnames(getFullTable())

    })
    
  
  output$fqdmessage<- renderPrint({
     input$fqdbutton
     isolate({
     n <-  doFastqDump()
     })
   
  })
  
  doFastqDump <- reactive({
    fastqDump(sraAccession = input$fastqcode, maxSpotId = 25)
  })
  output$mainTable <- DT::renderDataTable({
    #Draw main result table, only after button pressed 
    input$searchButton
    isolate({
      n <- getFullTable()
      
      })
    }, 
    
    options = list(columnDefs = list(list(
      targets = 6,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 6 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
        "}")
    )),lengthMenu = c(10, 25, 50),pageLength = 10)
   )
  
  output$errorBoard <- renderText({
    n <- " SEARCH ACCESSION RESULTS: 
           Enter a correctly formatted code, such as 
          'SRA003625', 'SRP000403', 'SRS001834',
          'SRR013350', or 'SRX002512 to display related studies, experiments,samples,  
           runs, or submissions. An improperly formatted code will yield an
           error. "
    
  })
  
#   output$columnPanel <- renderUI({
#     #Advanced Setting Panel 
  
#     input$searchButton
#     isolate({
#     table <- getFullTable
#     checkboxGroupInput('col_choices', label = "Display Fields", choices = colnames(getFullTable()),
#                        selected = getColumns() )
#     })
#   })
  #Downloading Fastq File Help:
  #http://shiny.rstudio.com/articles/download.html
  #----------> How to get fastq result to show in app, instead of going to default directory? 
  


  
})#End server 
    


