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

#Start Shiny Server 
shinyServer(function(input,output){
  
#####Reactive functions 
  
  getFullTable <- reactive({
  #returns search results given data type 
    dataType <- input$dataType
    searchTerms <- input$searchTerms
    if(dataType == 'sra_acc'){
      n <- getSRA(search_terms = searchTerms, sra_con = sra_con, out_types = 'run', acc_only = TRUE)
      run_codes <- as.vector(n[,1])
      print(class(run_codes))
      ftp_col <-  listSRAfile( "SRR390728", sra_con,fileType = 'fastq', srcType='fasp')
      print(ftp_col)
      #print(class(run_codes))
        #getFASTQinfo( sra_con = sra_con,run_codes, srcType = 'fasp' )
      n <- cbind(n, ftp_col)
      
    }
    else{
    n <- getSRA(search_terms = searchTerms, sra_con = sra_con, out_types = dataType)
    indexes = 1:(nrow(n))
    n <- cbind(indexes, n)
    }
  })
  
  observeEvent(input$actionButton,{
    selected_acc  = as.integer(input$mainTable_rows_selected)
    #ftp_col <- getFASTQinfo(selected_acc)
    table <- data.frame()
    n = getFullTable()
    n <- n[selected_acc,]
    n <- n[,"run"]
    print(n)
    #ftp_col <- getFASTQinfo(selected_acc, sra_con,)
    
    ftp_col = listSRAfile( as.vector(n), sra_con, fileType = 'sra' )
    print(ftp_col)
  })
  
  #### Output Functions
  output$mainTable <- DT::renderDataTable({
    input$searchButton
    isolate({
      table  <- getFullTable() 
      })
    }, 
    # LOOK AT DOM 
    ####################<<<<<<<<<<<<<<<<<
    rownames = FALSE,
    extensions = c('ColVis','ColReorder'),
    options = list(dom = 'RC<"clear">lfrtp',
                   scrollX = TRUE, scrollCollapse = TRUE,
                   colReorder = list(realtime = TRUE),
                   lengthMenu = c(15, 30, 50),pageLength = 15,
                   searchHighlight = TRUE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css
                     ({'background-color': '#008B8B', 'color': '#fff'});",
                     "}"
                     )
                   ))
  doFastqDump <- reactive({
    
    input$fqdbutton
    isolate({
      splitStyle <- input$splitStyle
      acc <- input$fastqcode
      range <- input$maxMin
      outdir <- input$outDir
      if(! acc == '')
        fastqDump(sraAccession = acc,  splitStyle = splitStyle,
                  maxSpotId = range[2],minSpotId = range[1], outdir = outdir)
    })
  })
  
  
  getFileName <- reactive({
    acc_code <- input$fastqcode
    splitStyle <- input$splitStyle
    print(paste(input$fastqcode))
    p <- paste(acc_code, '.', splitStyle,sep='') 
    print(p)
  })
  
  output$downloadSearchResults <- downloadHandler(
    
    filename = function(){
      name <-paste0(Sys.Date(),'-',input$searchTerms,'results.csv')
      name <- gsub(" ","",a)
      name
      },
    
    content = function(file) {
        n <- getFullTable()
#         if(length(input$mainTable_rows_selected) != 0){
#            selected_acc  = as.integer(input$mainTable_rows_selected)
#             print(class(selected_acc))
#             n <- n[selected_acc,]
#           
#       }
       write.csv(n, file)
    }
  )

  
}) #End server 
    


