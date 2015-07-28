#server.R  SRAdb-app 
#Olivia Zhang   July 9, 2015 

#===========================================#
library(shiny)
library(SRAdb)
library(DT)
library(shinyBS)
library(Rgraphviz)
sra_dbname <- 'SRAmetadb.sqlite'	
if(!file.exists('SRAmetadb.sqlite'))
  sqlfile <<- getSRAdbFile()
sra_con<- dbConnect(dbDriver("SQLite"), 
                    sra_dbname) 
source('~/sra_toolkit/bin/fastqDump_v1.R')

#===========================================#

#Start Shiny Server 
shinyServer(function(input,output,session){


#%%%%%%%%%%%%%%%%%%%%SEARCH RESULT TABLE %%%%%%%%%%%%%%%%%%%%%%%%%
#Switch panel on search--------------------------------------------
  observeEvent(input$searchButton,
      updateTabsetPanel(session,"tabSet", selected = "search_results"))
  
#Return Table fromsearch terms, data type-------------------------- 
  getFullTable <- reactive({
    
    progress <- shiny::Progress$new()
    progress$set(message = 'Loading Search Results . . . ', value = 5)
    on.exit(progress$close())
    dataType <- input$dataType
    searchTerms <- input$searchTerms
    
    if(dataType == 'sra_acc'){
          n <- getSRA_1(search_terms = searchTerms, sra_con = sra_con, 
                      out_types = 'run', acc_only = TRUE)
          run_codes <- n[,"run"]
          n <- listSRAfile( as.vector(run_codes), sra_con, 
                            fileType = 'sra')
          n$ftp <- createLink(n$ftp) #links to download SRA 
          return(n)
         }
     else{
         n <- getSRA_1(search_terms = searchTerms, sra_con = sra_con,
                    out_types = dataType, acc_only = FALSE)
     }
  })

#Create Results Table with Options-------------------------------
  output$mainTable <- DT::renderDataTable({
    input$reload
    Sys.sleep(2)
    input$searchButton
    isolate({
      searchTerms = input$searchTerms
      if(searchTerms != ''){
        table  <- getFullTable() 
      }
    })
    }, rownames = TRUE,
    escape = FALSE,
    extensions = c('ColVis','ColReorder', 'TableTools'),
    options = list(dom = 'TRC<"clear">lifrStp',
                   scrollX = TRUE, scrollCollapse = TRUE,
                   autoWidth = TRUE,
                   colReorder = list(realtime = TRUE),
                   lengthMenu = c(20,50, 100, 200),pageLength = 50,
                   searchHighlight = TRUE,
                  tableTools = list(sSwfPath = copySWF("www"), pdf = TRUE,
                                    aButtons = list('print', 'select_none','select_all')),
                                    #sRowSelect = "os"),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css
                     ({'background-color': '#6AB4FF', 'color': '#fff'});",
                     "}"
                   )
))
#%%%%%%%%%%%%%%%%%%%%%% Download Handlers %%%%%%%%%%%%%%%%%%%%%%%%%%
#Download entire SRA Table---------------------------------------- 
  output$downloadFullSRA<- downloadHandler(
    filename  = function() { 
      a <- paste0(Sys.Date(),'-',input$searchTerms,'FullResults.csv')
      a <- gsub(" ","",a) 
      },
    content = function(file){
      n <- getSRA(search_terms = input$searchTerms, sra_con = sra_con, out_types = "sra")
      write.csv(n,file)
    }
  )
  
#Download selected Columns---------------------------------------
  output$downloadSelected <- downloadHandler(
    filename  = function() { 
      a <- paste0(Sys.Date(),'-',input$searchTerms,'Results.csv')
      a <- gsub(" ","",a) 
    },
    content = function(file){
      n <- getFullTable()
      if(length(input$mainTable_rows_selected) != 0){
        selected_acc  = input$mainTable_rows_selected 
        n <- n[selected_acc,]
      }
      write.csv(n,file)
    }
  )
#%%%%%%%%%%%%%%%%%%%%%%%%% Choosing Directory %%%%%%%%%%%%%%%%%%%%%%%
  
#Link button to directory path ---------------------------------
  shinyDirChoose("outdirButton", input = input, session = session,
                 roots=c(wd = '/Users'), filetypes=c('', '.*'))
  
#Get chosen directory ------------------------------------------
  getOutdirpath <- reactive({
    roots = c(wd='/Users')
    path <- parseDirPath(roots , input$outdirButton)
    if(length(path) == 0)
      return(getwd())
    else
      return(parseDirPath(roots , input$outdirButton))
  })
  
#Display chosen directory --------------------------------------
  output$show_outdirpath <- renderText({
    input$outdirButton
    isolate({
      getOutdirpath()
    })
  })
  
  #Link button to directory path ---------------------------------
  shinyFileChoose("fqdCMDButton", input = input, session = session,
                 roots=c(wd = '/Users'), filetypes=c('', '.*'))
  
  #Display chosen directory --------------------------------------
  output$show_fqdCMDpath<- renderText({
    getfqdCMD()
  })
  
  getfqdCMD <- reactive({
    roots = c(wd='/Users')
    path <- parseFilePaths(roots , input$fqdCMDButton)
    return(levels(path$datapath))
  })

observeEvent(input$viewFiles, {
  outdir = getOutdirpath()
  system(sprintf("open %s", outdir))
})  
#%%%%%%%%%%%%%%%%%%% Display Operation Results %%%%%%%%%%%%%%%%%%%%%%%  
  
#Trigger Operation when ActionButton Pressed ----------------------------
  observeEvent(input$actionButton,{
    rows_selected <- input$mainTable_rows_selected
    operation <- input$operationType
    
    if(length(rows_selected) == 0){
      createAlert(session, "alert", alertId = "noRows",title = "No Rows Selected",
                  content = "Please select at least one row to perform operation on.",append = F)
      createAlert(session, "fqdalert",alertId = "fqdnoRows" ,title = "No Rows Selected",
                  content = "Please select at least one row to perform operation on.",
                  style = "danger", append = F)
    }
    else{
      closeAlert(session, "noRows")
      closeAlert(session, "fqdnoRows")
      n <- getFullTable()
      n_selected <- n[rows_selected,]
      
      if(is.element(operation, c('srainfo', 'fqinfo', 'eGraph', 'fastqdump')) 
          && !is.element('run', colnames(n_selected))){
            createAlert(session, "alert", "notRun", title = "Error:",
                    content = "Please select data of type 'run'.",style = "danger",append = FALSE)
            createAlert(session, "fqdalert", "fqdnotRun", title = "Error:",
                    content = "Please select data of type 'run'.",style = "danger",append = FALSE)
      }
      else
      {
          closeOperationAlerts(session)
          switch( operation,
            "srainfo"= updateTabsetPanel(session, "tabSet",
                                         selected = "operation"),
            "fqinfo" = updateTabsetPanel(session, "tabSet",
                                         selected = "operation"),
            "eGraph" = updateTabsetPanel(session, "tabSet",
                                         selected = "operation"),
            "fastqdump" = {
                options <- input$fqd_options
                splitStyle <- input$fqd_splitStyle
                minSpotId <- input$fqd_min
                maxSpotId <- input$fqd_max
                fastqDumpCMD <- getfqdCMD()
                outdir <- getOutdirpath()
                if(maxSpotId <1 || minSpotId < 0 ){
                  maxSpotId = -1
                  minSpotId = 0
                }
                run_code <- n_selected[,"run"]
                if(length(run_code) > 1){
                    createAlert(session, "fqdalert", "fqdmultiple", title = "Error",
                              content = "Multiple runs are selected. Please choose only 
                               one to perform Fastq dump.", style = "danger",append = F)
                }
                else{  
                    message <- capture.output(
                    fastqDump(run_code, minSpotId = minSpotId, maxSpotId = maxSpotId,
                        outdir = outdir, splitStyle = splitStyle,
                        split_spot = is.element("split_splot", options),
                        skip_technical = is.element("split_splot", options),
                        origfmt = is.element("origfmt", options),
                        fasta = is.element("fasta", options),
                        dumpbase = is.element("dumpbase ", options),
                        dumpcs = is.element("dumpcs", options),
                        fastqDumpCMD = fastqDumpCMD
                    ))
                    message <- paste(gsub('"', "",message[1], 'and', message[2]),sep="",collapse="")
                    createAlert(session, "fqdalert",title = "FASTQ Dump completed",
                          content = paste(message, 'in', outdir), style = "success", append = F)
                }
            } 
        )#end Switch 
      }
    }
    
    })
# Operation Message ---------------------------------------------
  observeEvent( input$tabSet == 'operation',{
    if(( input$operationType == 'none' || input$actionButton == 0)
        && input$tabSet == 'operation'){
              createAlert(session, "alert", alertId = "noAction", title = "No results to display",
                          content = "No action submitted for selected rows.", append = F)
      }
  }
  ) 

# Results Table for selected operations--------------------------
  output$operationResultsTable <- renderDataTable({
    input$reload
    Sys.sleep(2)
    input$actionButton #Table not drawn until Action button clicked 
    progress <- shiny::Progress$new()
    progress$set(message = 'Calculating Results . . . ', value = 5)
    on.exit(progress$close())
    
    isolate({
      operationType = input$operationType
      selected_rows  = as.integer(input$mainTable_rows_selected)
      if(length(selected_rows) != 0){
        n <- getFullTable()
        n_selected <- n[selected_rows,]
        run_codes <- as.vector(n_selected[,"run"])
        switch( input$operationType,
           "fqinfo" = {
               fqinfo <- getFASTQinfo(run_codes)
               fqinfo$ftp <- createLink(fqinfo$ftp)
               newBottom <- fqinfo[,1:5]  #rearrange columns to look better
               newTop <- fqinfo[,6:11]
               fqinfo <- cbind(newTop, newBottom)
           },
           "srainfo" = {
               srainfo <- getSRAinfo(run_codes, sra_con = sra_con, sraType = "sra")
               srainfo$ftp <- createLink(srainfo$ftp)
               srainfo
             }
        )}
    })
  },
  escape = FALSE,
  extensions = c('ColVis','ColReorder', 'TableTools'),
  options = list(dom = 'RC<"clear">lfrtip', # RC<"clear">lifrStp',
               scrollX = TRUE, scrollCollapse = TRUE,
               colReorder = list(realtime = TRUE),
               autoWidth = TRUE,
               lengthMenu = c(15, 50, 100),pageLength = 15,
               searchHighlight = TRUE,
               tableTools = list(sSwfPath = copySWF())
          )      
  )

output$eGraphPlot <- renderPlot( {
    plot <- makePlot()
})

makePlot <- reactive({
  input$actionButton
  isolate({
      n = getFullTable()
      selected_rows <- input$mainTable_rows_selected
      n_selected <- n[selected_rows,]
      runcodes <- as.vector(n_selected[,"run"])
      print(runcodes)
      acc_table <-  sraConvert( runcodes, sra_con = sra_con ) #convert runs into full table 
      g <- sraGraph_1(acc_table)
      attrs <- getDefaultAttrs(list(node=list(
      fillcolor='lightblue', shape='ellipse'),edge=list(color="darkblue")))
      plot(g, attrs=attrs)
  })
})

})#END SERVER 

######################################################################################################
##################################### OTHER FUNCTUONS ################################################
######################################################################################################
# Close Alerts 
closeOperationAlerts <- function(session){
  closeAlert(session, "fqdnotrun")
  closeAlert(session, "notrun")
  closeAlert(session, "noRows")
  closeAlert(session, "fqdnoRows")
}
# Clickable HTML Link Given URL(val)-----------------------------
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" 
          class="btn btn-link"> %s </a>',val,val)
}
# Disable Action button------------------------------------------
disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

# Modified getSRA------------------------------------------------
getSRA_1 <- function (search_terms, out_types = c("sra", "submission", "study", 
                                      "experiment", "sample", "run", "srabrief"), sra_con, acc_only = FALSE) 
{
  out_types <- match.arg(out_types, several.ok = T)
  sra_fields <- dbGetQuery(sra_con, "PRAGMA table_info(sra)")$name
  
  #defining correct indices 
  sra_fields_indice <- list( srabrief = c(20,19,58,7,8,22,47,72), #sample title include 
                    run = seq(which(sra_fields == "run_ID") + 1,
                              which(sra_fields == "experiment_ID") - 1),
                    experiment = seq(which(sra_fields == "experiment_ID") + 1, 
                              which(sra_fields == "sample_ID") - 1), 
                    sample = seq(which(sra_fields == "sample_ID") + 1, 
                                 which(sra_fields == "study_ID") - 1), 
                    study = seq(which(sra_fields == "study_ID") + 1,
                                which(sra_fields == "submission_ID") - 1), 
                    submission = seq(which(sra_fields == "submission_ID") +1,
                                length(sra_fields)),
                    sra = c(6:75))
  
  if (is.element("sra", out_types)) {
    sra_fields_indice_1 <- sra_fields_indice[["sra"]]
    select_fields = sra_fields[sra_fields_indice_1]
    not_null_str = ""
  }
  else {
    select_fields = NULL
    not_null = NULL
    for (type in out_types) {
      sra_fields_indice_1 <- sra_fields_indice[[type]]
      select_fields = c(select_fields, sra_fields[sra_fields_indice_1]) #add on each type
      if(type != 'srabrief')
           not_null = c(not_null, paste(type, "_accession IS NOT NULL ", 
                                   sep = ""))
      else
        not_null = c(not_null, paste('experiment', "_accession IS NOT NULL ", 
                                    sep = ""))
    }
    not_null_str = paste(" AND (", paste(not_null, collapse = " OR "), 
                         ")", sep = "")  #use for getQuery for accession codes
  }
  if (acc_only) 
    select_fields = rev(select_fields[grep("_accession", 
                                           select_fields)])
  select_fields_str <- paste(select_fields, collapse = ",")
  sql <- paste("SELECT DISTINCT ", select_fields_str, " FROM sra_ft WHERE sra_ft MATCH '", 
               search_terms, "' ", not_null_str, ";", sep = "")
  rs <- dbGetQuery(sra_con, sql)
  names(rs) <- sub("_accession", "", names(rs))
  return(rs)
}

sraGraph_1 <- function( acc_table) 
{ 
  g <- entityGraph(acc_table)
  return(g)
}
############################################################################################




    


