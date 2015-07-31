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
source('copyfastqdump_v1.R')


#===========================================#

#Start Shiny Server 
shinyServer(function(input,output,session){

  createAlert(anchorId = "TBalert", alertId = "NoSearch", title = "No Search Terms Entered",
              content = "Please enter search terms to display results", session= session)
#%%%%%%%%%%%%%%%%%%%%SEARCH RESULT TABLE %%%%%%%%%%%%%%%%%%%%%%%%%
#Switch panel on search--------------------------------------------
  observeEvent(input$searchButton,{
      updateTabsetPanel(session,"tabSet", selected = "search_results")
     if(input$searchTerms != '')
       closeAlert(session, "NoSearch")})
  
#Search Table fromsearch terms, data type-------------------------- 
  getFullTable <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = 'Loading Search Results . . . ', value = 5)
    on.exit(progress$close())
    input$searchButton
    isolate({
    dataType <- input$dataType
    searchTerms <- input$searchTerms
    n <- getSRA_1(search_terms = searchTerms, sra_con = sra_con,
                    out_types = dataType, acc_only = FALSE)
  
  })
  })
  
#Results Table with Options-------------------------------
  output$mainTable <- DT::renderDataTable({
    input$reload
    input$searchButton
    isolate({
      searchTerms = input$searchTerms
      if(searchTerms != ''){
        table  <- getFullTable() 
      }
    })
    }, rownames = TRUE,
    escape = FALSE,
    class = 'order-column nowrap',
    extensions = c('ColVis','ColReorder', 'TableTools'),
    #selection = list(mode = 'multiple', selected = as.character(selected_rows)),
    options = list(dom = 'TRC<"clear">lifrStp',
                   scrollX = TRUE, scrollCollapse = TRUE,
                   
                   autoWidth = TRUE,
                   orderClasses = TRUE,
                   colReorder = list(realtime = TRUE),
                   lengthMenu = c(20,50, 100, 200),pageLength = 50,
                   searchHighlight = TRUE,
                   server = FALSE,
                   stateSave = TRUE,
                   tableTools = list("sSwfPath" = copySWF("www"), pdf = TRUE,
#                                   "aButtons" = list("print", "select_none",list("sExtends" = "select_all",
#                                                                                 "oSelectorOpts"=list("page"="all"
#                                                                                                     ),
#                                                                                 "sButtonText" = "Select All"
#                                                                                 #"fnClick" = 
#                                                                                   #JS(" function ( nButton, oConfig, oFlash ) {
#                                                                                     #")))),
                                    aButtons = list(
                                                    'print', 'select_none','select_all'
                                                   )),
                   columnDefs = list(list(width = '200px', targets = c(1,2,3,4,5) )),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css
                     ({'background-color': '#6AB4FF', 'color': '#fff'});",
                     "}"
                   )
    )
                   
)
  observeEvent(input$selectAll,
               {selected_rows <- 1:nrow(getFullTable())})
  #%%%%%%%%%%%%%%%%%%% Display Operation Results %%%%%%%%%%%%%%%%%%%%%%%  
  
  #Trigger Operation when ActionButton Pressed ----------------------------
  observeEvent(input$actionButton,{
    selected_rows <<-as.integer(input$mainTable_rows_selected)
#     if(input$selectAll >= input$actionButton)
#       selected_rows <- 1:nrow(getFullTable())
    operation <- input$operationType
    print(selected_rows)
    if(length(selected_rows) == 0 ){
      createAlert(session, "alert", alertId = "noRows",title = "No Rows Selected",
                  content = "Please select at least one row to perform operation on.",append = F)
      createAlert(session, "fqdalert",alertId = "fqdnoRows" ,title = "No Rows Selected",
                  content = "Please select at least one row to perform operation on.",
                  style = "danger", append = F)
      return()
    }
    else{
      #if there are selected rows: close any possible previous noRows alerts 
      closeAlert(session, "noRows")
      closeAlert(session, "fqdnoRows")
      n <- getFullTable()
      n_selected <- n[selected_rows,]
      
      if(is.element(operation, c('srainfo', 'fqinfo', 'eGraph', 'fastqdump')) 
         && !is.element('run', colnames(n_selected))){
        createAlert(session, "alert", "notRun", title = "Error:",
                    content = "Please select data of type 'run'.",style = "danger",append = FALSE)
        createAlert(session, "fqdalert", "fqdnotRun", title = "Error:",
                    content = "Please select data of type 'run'.",style = "danger",append = FALSE)
        updateTabsetPanel(session, "tabSet",selected = "operation")
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
                "related_acc" = updateTabsetPanel(session, "tabSet",
                                             selected = "operation"),
                "fastqdump" = {
                  options <- input$fqd_options
                  splitStyle <- input$fqd_splitStyle
                  minSpotId <- input$fqd_min
                  maxSpotId <- input$fqd_max
                  fastqDumpCMD <- getfqdCMD()
                  if(is.null(fastqDumpCMD ) || !grepl('fastq-dump',fastqDumpCMD)){
                      createAlert(session, anchorId = 'fqdalert', title = "Missing FASTQ Dump Command",
                                  content = paste0("Please indicate the location to the fastq-dump command on your device.
                                                  The fastq-dump command is part of the sra toolkit, which can be downloaded", 
                                                  createLink("http://www.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software", 
                                                             "here")), style = "danger"
                                             )
                      return()
                    }
                  outdir <- getOutdirpath()
                  if(is.na(maxSpotId) && is.na(minSpotId)){
                    maxSpotId = -1
                    minSpotId = 0
                  }
                  else if(maxSpotId <1 || minSpotId < 0 || minSpotId > maxSpotId ){
                    createAlert(session, anchorId = 'fqdalert', content = "Invalid spotID range. Please enter valid range
                                or check 'Entire File' option.")
                    return()
                  }
                  
                  run_code <- n_selected[,"run"]
                  if(length(run_code) > 1){
                    createAlert(session, "fqdalert", "fqdmultiple", title = "Error",
                                content = "Multiple runs are selected. Please choose only 
                                one to perform Fastq dump.", style = "danger",append = F)
                    return()
                  }
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
                    message <- paste(gsub('"', "",message[1], 'and', message[2]), ' in ', outdir, sep="",collapse="")
                    createAlert(session, "fqdalert",title = "FASTQ Dump completed",
                                content = message, style = "success", append = F)
                  }
             
        )#end Switch 
      }
    }
    
  })
  
  # Operation Results Table-------------------------
  output$operationResultsTable <- renderDataTable({
    input$reload
    input$actionButton #Table not drawn until Action button clicked 
    progress <- shiny::Progress$new()
    progress$set(message = 'Calculating Results . . . ', value = 5)
    on.exit(progress$close())
    
    isolate({
      operationType = input$operationType
      selected_rows  <<- as.integer(input$mainTable_rows_selected)
      if(length(selected_rows) != 0){
        n <- getFullTable()
        n_selected <- n[selected_rows,]
        run_codes <- as.vector(n_selected[,"run"])
        switch( input$operationType,
                "fqinfo" = {
                  fqinfo <- getFASTQinfo(in_acc = run_codes, sra_con = sra_con, srcType =  'fasp')
                  fqinfo$fasp <- createLink(fqinfo$fasp,paste("Download", fqinfo$run, "fastq file"))
                  newBottom <- fqinfo[,1:5]  #rearrange columns to look better
                  newTop <- fqinfo[,6:11]
                  outputTable <- cbind(newTop, newBottom)
                },
                "srainfo" = {
                  srainfo <- getSRAinfo(run_codes, sra_con = sra_con, sraType = "sra")
                  srainfo$ftp <- createLink(srainfo$ftp, paste("Download SRA for ",srainfo$run))
                  outputTable <- srainfo
                },
                "related_acc" = {
                  acc_possible <- c("run", "study", "experiment", "sample", "submission" )
                  yesAcc = intersect(acc_possible,colnames(n_selected))
                  if('run'%in%colnames(n_selected))
                    codeVector = n_selected[,"run"]
                  else
                    codeVector = n_selected[,yessAcc[1]]
                  #sraConvert(codeVector = in_acc, sra_con = sra_con)
                  n <- listSRAfile( codeVector, sra_con, 
                                    fileType = 'sra')
                  n$ftp <- createLink(n$ftp, "Download SRA") #links to download SRA 
                  outputTable <- n
                  
                }
        )}
    })
  },
  escape = FALSE,
  class = 'nowrap order-column',
  extensions = c('ColVis','ColReorder', 'TableTools'),
  options = list(dom = 'RC<"clear">lfrtip', # RC<"clear">lifrStp',
                 scrollX = TRUE, scrollCollapse = TRUE,
                 colReorder = list(realtime = TRUE),
                 saveState = TRUE,
                 autoWidth = TRUE,
                 orderClasses = TRUE,
                 lengthMenu = c(15, 50, 100),pageLength = 15,
                 searchHighlight = TRUE,
                 tableTools = list(sSwfPath = copySWF())
                 #columnDefs = list(list(width = '450px', targets = c(1) ))
  )      
  )
  # Operation Message ---------------------------------------------
  observeEvent( input$tabSet == 'operation',{
    if(( input$operationType == 'none' || input$actionButton == 0)
       && input$tabSet == 'operation'){
      createAlert(session, "alert", alertId = "noAction", title = "No results to display",
                  content = "No action submitted for selected rows.", append = F)
    }
  }
  ) 
#%%%%%%%%%%%%%%%%%%%%%% Download Handlers %%%%%%%%%%%%%%%%%%%%%%%%%%
#Download handler ---------------------------------------- 
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
  
#Download selected rows ---------------------------------------
  output$downloadSelected <- downloadHandler(
    filename  = function() { 
      a <- paste0(Sys.Date(),'-',input$searchTerms,'Results.csv')
      a <- gsub(" ","",a) 
    },
    content = function(file){
      n<- getFullTable()
      selected_acc  = input$mainTable_rows_selected 
      n <- n[selected_acc,]
      write.csv(n,file)
    }
  )
#%%%%%%%%%%%%%%%%%%%%%%%%% Choosing Directory %%%%%%%%%%%%%%%%%%%%%%%
  
#Links to Directory button  ---------------------------------
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
  
#Show chosen directory --------------------------------------
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
    input$fqdCMDButton
    isolate({
      getfqdCMD()
    })
    
  })
  
#Choose fqdCMD ------------------------
  getfqdCMD <- reactive({
    roots = c(wd='/Users')
    path <- parseFilePaths(roots , input$fqdCMDButton)
    if(is.data.frame(path) & nrow(path)==0)
      return(NULL)
    return(levels(path$datapath))
  })
  
#Open Finder to see files -------------
 observeEvent(input$viewFiles, {
  outdir = getOutdirpath()
  system(sprintf("open %s", outdir))
})  
#Plot of ERD---------------------------
output$eGraphPlot <- renderPlot( {
  input$actionButton
  isolate({
    plot <- makePlot()})
})
#return ERD graph---------------------
makePlot <- reactive({
  input$actionButton
  input$reload
  progress <- shiny::Progress$new()
  progress$set(message = 'Generating Plot . . . ', value = 5)
  on.exit(progress$close())
  isolate({
      n = getFullTable()
      selected_rows <<- as.integer(input$mainTable_rows_selected)
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
  
output$searchPanel <- renderUI({
    #     list(
    # 
    #     )
    #   })
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
  closeAlert(session, "noAction")
}
# Clickable HTML Link Given URL(val)-----------------------------
createLink <- function(val, linkdisplay) {
  sprintf('<a href="%s" target="_blank" 
          class="btn btn-link"> %s </a>',val,linkdisplay)
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




    


