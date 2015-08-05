#server.R  SRAdb-app 
#Olivia Zhang   July 9, 2015 
#===========================================#
Libs = c('shiny', 'DT', 'SRAdb', 'shinyBS', 'Rgraphviz', 'shinyFiles', 'R.utils')
for( Lib in Libs ) {
  if( !require( Lib, character.only = T ) ) {
    source("http://bioconductor.org/biocLite.R")
    biocLite( Lib, type='source', lib = .libPaths()[2] )
    library( Lib, character.only = T )
  }
}
sra_dbname <- 'SRAmetadb.sqlite'	
if(!file.exists('SRAmetadb.sqlite'))
  sqlfile <<- getSRAdbFile()
sra_con<- dbConnect(dbDriver("SQLite"), 
                    sra_dbname)
source('copyfastqdump_v1.R')
#===========================================#

#Start Shiny Server 
shinyServer(function(input,output,session){
 
#%%%%%%%%%%%%%%%%%%%% SEARCH RESULT TABLE %%%%%%%%%%%%%%%%%%%%%%%%%%%
#Switch panel on search--------------------------------------------
  createAlert(session, "TBalert", alertId = "NoSearch", title = "No results to display",
              content = "Please enter search terms to display results", append = F)
  createAlert(session, "alert", alertId = "noAction", title = "No results to display",
              content = "Please select rows and select operation to display results.", append = F)
  
  observeEvent(input$searchButton,{
      updateTabsetPanel(session,"tabSet", selected = "search_results")
     if(input$searchTerms != '')
       closeAlert(session, "NoSearch")})
  
  observeEvent( input$actionButton, {
    if(input$operationType != '' && length(input$mainTable_rows_selected) != 0){
      closeAlert(session, alertId = "TBnoAction")
      closeAlert(session, alertId = "noAction")
    }
    else
      createAlert(session, "TBalert", alertId = "TBnoAction", title = "No results to display",
                  content = "Please select rows and select operation to display results.", append = F,
                  style = "danger")
  })
 
#Search Table fromsearch terms, data type-------------------------- 
  getFullTable <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = 'Loading Search Results . . . ', value = 5)
    on.exit(progress$close())
    input$searchButton
    isolate({
    dataType <- input$dataType
    searchTerms <- input$searchTerms
    if(dataType == "acc_table"){
      n <- getSRA_1(search_terms = searchTerms, sra_con = sra_con,
                     out_types = c("study","experiment","sample","run", "submission"), acc_only = TRUE)
      n <- n[,c("study","experiment","sample","run", "submission")]
    }
    else{
    n <- getSRA_1(search_terms = searchTerms, sra_con = sra_con,
                    out_types = dataType, acc_only = FALSE)
    }
  })
  })
  
#Make dir -------------------------------------------------------  
if( ! file.exists("www") ) {
    dir.create( "www" )
  }
  
#Results Table with Options--------------------------------------
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
    options = list(dom = 'CTRf<"clear">lirSpt',
                   scrollX = TRUE, scrollCollapse = TRUE,
                   autoWidth = TRUE,
                   orderClasses = TRUE,
                   colReorder = list(realtime = TRUE),
                   lengthMenu = c(20,50, 100, 200),pageLength = 50,
                   searchHighlight = TRUE,
                   server = FALSE,
                   stateSave = TRUE,
                   tableTools = list("sSwfPath" = copySWF("www"), pdf = TRUE,
                                     #sRowSelect = "os",
                                    aButtons = list(
                                                    'print'
                                                   )),
                   #columnDefs = list(list(width = '200px', targets = c(1,2,3,4,5) )),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css
                     ({'background-color': '#6AB4FF', 'color': '#fff'});",
                     "}"
                   )
    )                 
)
# Intrument Model Table on Front Page---------------------------------
  output$instr_models <- renderDataTable({
    dt <- read.csv("SRAinstrument_counts.xls", sep= ",", header = TRUE)
  },
  rownames = FALSE,
  options = list(dom = "T"),
  colnames = c("Count:", "Instrument Model" ))
 
#%%%%%%%%%%%%%%%%%%% Handling Operation Results %%%%%%%%%%%%%%%%%%%%%%%  
  
  #Trigger Operation when ActionButton Pressed ----------------------------
  observeEvent(input$actionButton,{
    selected_rows <-as.integer(input$mainTable_rows_selected)
    operation <- input$operationType
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
      n <- getFullTable()
      n_selected <- n[selected_rows,]
      if(is.element(operation, c('srainfo', 'fqinfo', 'fastqdump')) 
         && !is.element('run', colnames(n_selected))){
        createAlert(session, "alert", "notRun", title = "Error:",
                    content = "Please select data of type 'run'.",style = "danger",append = FALSE)
        createAlert(session, "fqdalert", "fqdnotRun", title = "Error:",
                    content = "Please select data of type 'run'.",style = "danger",append = FALSE)
        if(operation == 'fastqdump'){
          return()
        }
      }
      else
      {
        closeOperationAlerts(session) }
        switch( operation,
                "srainfo"= 
                  updateTabsetPanel(session, "tabSet",
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
                  print(paste(minSpotId, maxSpotId))
                  fastqDumpCMD <- input$fqdPlaces
                  if(is.null(fastqDumpCMD ) || !grepl('fastq-dump',fastqDumpCMD)){
                      createAlert(session, anchorId = 'fqdalert', title = "Missing FASTQ Dump Command",
                                  content = paste0("Please indicate the location to the fastq-dump command on your device. Create a link to the command using 'sudo'. 
                                                  The fastq-dump command is part of the sra toolkit, which must be downloaded to perform a fast-q dump.", 
                                                  createLink("http://www.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software", 
                                                             "here")), style = "danger"
                                             )
                      return()
                    }
                  outdir <- input$show_outdirpath
                  if(input$fullFile){
                    maxSpotId = -1
                    minSpotId = 0
                  }
                  else if(is.na(maxSpotId) || is.na(minSpotId) || maxSpotId <1 || minSpotId < 0 || minSpotId > maxSpotId){
                    createAlert(session, anchorId = 'fqdalert', content = "Invalid spotID range. Please enter valid range
                                or check 'Entire File' option.", style = "danger")
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
})
  
#Operation Results Table-------------------------
  output$operationResultsTable <- renderDataTable({
    input$actionButton 
    input$reload
    progress <- shiny::Progress$new()
    progress$set(message = 'Loading Search Results . . . ', value = 5)
    on.exit(progress$close())
    isolate({
      operationType = input$operationType
      selected_rows  <<- as.integer(input$mainTable_rows_selected)
      if(length(selected_rows) == 0){
        return()
      }
      else{
        n <- getFullTable()
        n_selected <- n[selected_rows,]
        if (is.element(operationType, c('srainfo', 'fqinfo', 'fastqdump'))  #//////////////HERE
            && !is.element('run', colnames(n_selected))){
          return()
        }
        if( operationType != ("related_acc") && operationType != ("eGraph"))
        {
          run_codes <- as.vector(n_selected[,"run"])
        }
        switch( input$operationType,
                
                "fqinfo" = {
                  fqinfo <- getFASTQinfo(in_acc = run_codes, sra_con = sra_con, srcType =  'fasp')
                  fqinfo$fasp <- createLink(fqinfo$fasp,paste("Download", fqinfo$run, "fastq file"))
                  newBottom <- fqinfo[,1:5] 
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
                    {print(colnames(n_selected))
                     codeVector = n_selected[,"run"]
                  }
                  else
                    codeVector = n_selected[,yesAcc[1]]
                  print(codeVector)
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
                 
  )      
  )
  
#Plot of ERD---------------------------
  output$eGraphPlot <- renderPlot( {
    input$actionButton
    isolate({
      plot <- makePlot()})
  })
  observeEvent(input$operationType == "eGraph",{
    if(input$operationType == "eGraph"){
    createAlert(session, alertId = "eGraphWarning", anchorId = "TBalert", title = "Warning:", content = 
                  "Entity Graph creation for large data sets,including those of type STUDY or SUBMISSION, will take a 
                long time and is not recommended.", style = "warning")
    }
    else{
      closeAlert(session, alertId = "eGraphWarning")
    }
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
      if(length(selected_rows) == 0 ){
        return()
      }
      acc_possible <- c("run", "study", "experiment", "sample", "submission" )
      yesAcc = intersect(acc_possible,colnames(n_selected))
      
      if('run'%in%colnames(n_selected))
      { print(colnames(n_selected))
        codeVector = as.vector(n_selected[,"run"])
      }
      else
        codeVector = as.vector(n_selected[,yesAcc[1]])
      print(codeVector)
      acc_table <-  sraConvert( codeVector, sra_con = sra_con ) #convert runs into full table 
      g <- sraGraph_1(acc_table)
      attrs <- getDefaultAttrs(list(node=list(
        fillcolor='lightblue', shape='ellipse'),edge=list(color="darkblue")))
      plot(g, attrs=attrs)
    })
  })
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
  observe({
    fastqCMD <- system(' which fastq-dump', intern = TRUE)
     updateTextInput(session, 'fqdPlaces', label = NULL,
                     value = fastqCMD)
   }
   )
#Get chosen directory ------------------------------------------
  getOutdirpath <- reactive({
    roots = c(wd='/Users')
    path <- parseDirPath(roots , input$outdirButton)
    if(length(path) == 0)
      return(getwd())
    else
      return(parseDirPath(roots , input$outdirButton))
  })

  observe({
      updateTextInput(session, "show_outdirpath", value = getOutdirpath())
    })
 
#Open Finder to see files ----------------------
observeEvent(input$viewFiles, {
  outdir = getOutdirpath()
  system(sprintf("open %s", outdir))
})  
  
  
#Adding help popups------------------------------
 output$selectHelp <- renderText({
    return('Select Rows [?]')
  })
 addPopover(session, "selectHelp", title = NULL,content = paste0("Click on a row 
          to select it for an operation, choose an operation, then press 'Submit'."), trigger = 'click')
  
 output$smartSearch <- renderText({
    return('Advanced Search [?]')
  })
addPopover(session, "smartSearch", title = NULL,content = HTML(paste('<b>Smart Search Options: </b> 
                                                                     <ul>
                                                                     <li><em>OR</em> to combine searches</li>
                                                                     <li><em>column</em>:<em>value</em> for all reslts with given column value</li> 
                                                                     <li><em>"Search Terms"</em> for full phrase search</li>
                                                                     <li><em> term* </em> for all results that begin with term</li>
                                                                     </ul>')),
             trigger = 'click')  
  
 
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




    


