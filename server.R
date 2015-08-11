#server.R  SRAdb-app 
#============================================================================#
Libs = c('shiny', 'DT', 'SRAdb', 'shinyBS', 'Rgraphviz', 'shinyFiles', 'R.utils')
for( Lib in Libs ) {
  if( !require( Lib, character.only = T ) ) {
    source("http://bioconductor.org/biocLite.R")
    biocLite( Lib, type='source', lib = .libPaths()[2] )
    library( Lib, character.only = T )
  }
}

if( ! file.exists("www") ) {
  dir.create( "www" )
}

sra_dbname <- 'SRAmetadb.sqlite'	
if(!file.exists('SRAmetadb.sqlite'))
  sqlfile <<- getSRAdbFile()
sra_con<- dbConnect(dbDriver("SQLite"), 
                    sra_dbname)
source('fastqdump_v1.R')
#============================================================================#


shinyServer(function(input,output,session){
 

## Search Table from search terms and data type-------------------------- 
  getFullTable <- reactive({
    progress <- shiny::Progress$new()
    progress$set(message = 'Loading Search Results . . . ', value = 5)
    on.exit(progress$close())
    input$searchButton
    
    isolate({
      dataType <- input$dataType
      searchTerms <- input$searchTerms
      
      if(grepl("'", searchTerms)){
        createAlert(session, "TBalert", alertId = "quoteError", title = "Error",
          content = " Single quotes not accepted. Please use Double quotes to search for a phrase", style = "danger")
        return()
       }
       else{ closeAlert(session, "quoteError")
      }
      if(grepl('"', searchTerms)){
        searchTerms = paste0('\"',substring(searchTerms, 2, nchar(searchTerms) -1 ), '\"')
        print(searchTerms)
      }
      if(dataType == "acc_table"){
        searchResults <- getSRA_1(search_terms = searchTerms, sra_con = sra_con,
                     out_types = c("study","experiment","sample","run", "submission"), acc_only = TRUE)
        searchResults <- n[,c("study","experiment","sample","run", "submission")]
      }
      else{
      searchResults <- getSRA_1(search_terms = searchTerms,
                                sra_con = sra_con,out_types = dataType)
    
        }
      })
 })
  
## Results Table with Options--------------------------------------
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
                 autoWidth = TRUE,orderClasses = TRUE,
                 colReorder = list(realtime = TRUE),
                 lengthMenu = c(20,50, 100, 200),pageLength = 50,
                 searchHighlight = TRUE,
                 server = FALSE,stateSave = TRUE,
                 tableTools = list("sSwfPath" = copySWF("www"), pdf = TRUE,
                                   aButtons = list('print')),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css
                   ({'background-color': '#6AB4FF', 'color': '#fff'});",
                   "}"
                 )
                
                 )                 
  )
  
## Intrument Model Table on Front Page---------------------------------
  output$instr_models <- renderDataTable({
      if( file.exists('sra_counts.txt')) {
        dt <- read.delim("sra_counts.txt" )
      } else {
        getTableCounts <- function(tableName,conn) {
          sql <- sprintf("select count(*) from %s",tableName)
          return(dbGetQuery(conn,sql)[1,1])
        }
        dt <- do.call(rbind,sapply(c('submission', 'study', 'sample',
                                     'experiment', 'run'),
                                   getTableCounts, sra_con, simplify=FALSE))
      }
      df <- as.data.frame(dt)
      rownames(df) <- toupper(rownames(df))
      colnames(df) <- c("Count")
      write.table(df, file = 'sra_counts.txt', sep="\t")
      df
    },
  options = list(dom = "T"),rownames = TRUE
  )
 
#%%%%%%%%%%%%%%%%%%% Handling Operation Results %%%%%%%%%%%%%%%%%%%%%%%  
 
## Trigger Operation when ActionButton Pressed ----------------------------
  observeEvent(input$actionButton,{
    if(is.null(getFullTable())){
      return()
    }
    selected_rows <-as.integer(input$mainTable_rows_selected)
    operation <- input$operationType
    
    n <- getFullTable()
    n_selected <- n[selected_rows,]
    if(length(selected_rows) == 0 || operation == '')
      return
    if(is.element(operation, c('srainfo', 'fqinfo', 'fastqdump')) 
         && !is.element('run', colnames(n_selected))){
        createAlert(session, "alert", "notRun", title = "Error:",
                    content = "Please select data of type 'run'.",style = "danger",append = FALSE)
        createAlert(session, "fqdalert", "fqdnotRun", title = NULL,
                    content = "Please select data of type 'run'.",style = "danger",append = FALSE)
        if(operation == 'fastqdump'){
          return()
        }
    }
    else
    { closeAlert(session, "notRun")
      closeAlert(session, "fqdnotRun")
    }
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
                  progress <- shiny::Progress$new()
                  progress$set(message = 'Dumping FASTQ Files . . .', value = 5)
                  on.exit(progress$close())
                  
                  options <- input$fqd_options
                  zipFormat <- input$fqd_zipFormat
                  minSpotId <- input$fqd_min
                  maxSpotId <- input$fqd_max
                  fastqDumpCMD <- input$fqdPlaces
                  run_code <- n_selected[,"run"]
                  outdir <- input$show_outdirpath
                  
                  if (!file.exists(outdir)){
                    createAlert(session, "fqdalert", alertId = "noDir", content = "Invalid out directory
                                selected. Please enter a valid directory.", style = "danger",append = TRUE)
                    return()
                  } 
                  if(length(run_code) > 1){
                    createAlert(session, "fqdalert", "fqdmultiple", title = NULL,
                                content = "Multiple runs are selected: please choose only 
                                one to perform FASTQ dump.", style = "danger",append = F)
                    return()
                  }
                  else if(length(run_code) == 0 ){
                    createAlert(session, "fqdalert",alertId = "fqdnoRows" ,title = NULL,
                                content = "No rows selected: please select one row to perform operation on.",
                                style = "danger", append = F)
                    return()
                  }
                  if(is.null(fastqDumpCMD ) || !grepl('fastq-dump',fastqDumpCMD)){
                      createAlert(session, anchorId = 'fqdalert', title = "Missing FASTQ Dump Command",
                                  content = paste0("Please indicate the location to the FASTQ-dump command on your device. Create a link to the command using 'sudo'. 
                                                  The FASTQ-dump command is part of the sra toolkit, which must be downloaded to perform a fast-q dump.", 
                                                  createLink("http://www.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software", 
                                                             "here")), style = "danger"
                                             )
                      return()
                  }
                  if(input$fullFile == 'entire'){
                    maxSpotId = -1
                    minSpotId = 0
                  }
                  else if(is.na(maxSpotId) || is.na(minSpotId) || maxSpotId <1 || minSpotId < 0 || minSpotId > maxSpotId){
                    createAlert(session, anchorId = 'fqdalert', content = "Invalid spotID range: please enter valid range
                                or check 'Entire File' option.", style = "danger", append = FALSE)
                    return()
                  }
                  message <- capture.output(
                      fastqDump(run_code,outdir = outdir, minSpotId = minSpotId,
                                maxSpotId = maxSpotId,
                                zipFormat = zipFormat,
                                split_spot = is.element("split_splot", options),
                                skip_technical = is.element("split_splot", options),
                                origfmt = is.element("origfmt", options),
                                fasta = is.element("fasta", options),
                                dumpbase = is.element("dumpbase ", options),
                                dumpcs = is.element("dumpcs", options),
                                fastqDumpCMD = fastqDumpCMD
                      ))
                    message <- paste(gsub('"', "",message[1]), ' in ', outdir, sep="",collapse="")
                    start = gregexpr(pattern ='Written', message)
                    message = substring(message, start, nchar(message))
                    createAlert(session, "fqdalert",title = NULL,
                                content = HTML(paste("<font size='3'><b>FASTQ Dump Completed:</b></font>",message)), style = "success", append = F)
                  
                  }
         )#end Switch 
})
#-Enable View directory button
  observeEvent(input$actionButton, ({
    if(input$operationType == 'fastqdump')
       updateButton(session, "viewFiles", disabled = FALSE)
  }))
  
## Operation Results Table-------------------------
  output$operationResultsTable <- renderDataTable({
    input$actionButton 
    input$reload
    progress <- shiny::Progress$new()
    progress$set(message = 'Loading Operation Results . . . ', value = 5)
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
        if (is.element(operationType, c('srainfo', 'fqinfo', 'fastqdump')) 
            && !is.element('run', colnames(n_selected))){
          return()
        }
        
        switch( input$operationType,
                
                "fqinfo" = {
                    run_codes <- as.vector(n_selected[,"run"])
                    fqinfo <- getFASTQinfo(in_acc = run_codes, sra_con = sra_con)
                    
                    fqinfoLink = fqinfo$ftp
                    fqinfo$ftp <- createLink(fqinfoLink, paste("Download", fqinfo$run, "FASTQ file"))
                    newTop<- fqinfo[,1:5] 
                    newBottom<- fqinfo[,9:11]
                    newFirst <- fqinfo[7]
                    resultTable <- cbind(newFirst,newTop, newBottom)
                  },
                
                "srainfo" = {
                    run_codes <- as.vector(n_selected[,"run"])
                    srainfo <- getSRAinfo(run_codes, sra_con = sra_con, sraType = "sra")
                    srainfo$ftp <- createLink(srainfo$ftp, paste("Download SRA for ",srainfo$run))
                    resultTable <- srainfo
                  },
                
                "related_acc" = {
                    acc_possible <- c("run", "study", "experiment", "sample", "submission" )
                    yesAcc = intersect(acc_possible,colnames(n_selected))
                    if('run'%in%colnames(n_selected))
                      {
                        codeVector = n_selected[,"run"]
                      }
                    else
                      codeVector = n_selected[,yesAcc[1]]
                    n <- listSRAfile( codeVector, sra_con, 
                                    fileType = 'sra')
                    n$ftp <- createLink(n$ftp, "Download SRA") 
                    resultTable <- n
                }
          )}
      })
  },
  escape = FALSE,class = 'nowrap order-column',
  extensions = c('ColVis','ColReorder', 'TableTools'),
  options = list(dom = 'RC<"clear">lfrtip', scrollX = TRUE,
                 scrollCollapse = TRUE,
                 colReorder = list(realtime = TRUE),
                 saveState = TRUE,autoWidth = TRUE,
                 orderClasses = TRUE,lengthMenu = c(15, 50, 100),
                 pageLength = 15,searchHighlight = TRUE,
                 tableTools = list(sSwfPath = copySWF())
            )      
  )
  
## Plot of ERD-----------------------------------
  output$eGraphPlot <- renderPlot( {
    input$actionButton
    isolate({
      plot <- makePlot()})
  })
  
## Reactive Function
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
          codeVector = as.vector(n_selected[,"run"])
      else
          codeVector = as.vector(n_selected[,yesAcc[1]])
      acc_table <-  sraConvert( codeVector, sra_con = sra_con )
      g <- sraGraph_1(acc_table)
      attrs <- getDefaultAttrs(list(node=list(fillcolor='lightblue', shape='ellipse'),
                                    edge=list(color="darkblue")))
      plot(g, attrs=attrs)
    })
  })
  
##Error Alert Handling -------------------------------------------
  observeEvent(input$searchButton,{
    updateTabsetPanel(session,"tabSet", selected = "search_results")
    if(input$searchTerms != '')
      closeAlert(session, "noSearch")}
    )
  
  createAlert(session, "TBalert", alertId = "noSearch", title = "No results to display",
              content = "Please enter search terms to display results", append = F)
  createAlert(session, "alert", alertId = "noAction", title = "No results to display",
              content = "Please select rows and select operation to display results", append = F)
  
  observeEvent( input$actionButton, {
    if((input$operationType != '' && length(input$mainTable_rows_selected ) > 0)){
      closeAlert(session, alertId = "TBnoAction")
      closeAlert(session, alertId = "noAction")
    }
    else
      createAlert(session, "TBalert", alertId = "TBnoAction", title = "No results to display",
                  content = "Please select rows and select operation to perform operation", append = F,
                  style = "danger")
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

##Download handler ---------------------------------------- 
  #-Download full table
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
 #-Download selected rows 
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
#Directory button, find fastqdump command, open download dir ------------------------------------
  
#-Update text input
  observe({
    updateTextInput(session, "show_outdirpath", value = getOutdirpath())
  })
#-Get Outdir path
  getOutdirpath <- reactive({
    roots = c(wd='/Users')
    path <- parseDirPath(roots , input$outdirButton)
    if(length(path) == 0)
      return(getwd())
    else
      return(path)
  })
#-Link button
  shinyDirChoose("outdirButton", input = input, session = session,
                 roots=c(wd = '/Users'), filetypes=c('', '.*'))
  
#-Find fastqCMD
  observe({
    fastqCMD <- system(' which fastq-dump', intern = TRUE)
    updateTextInput(session, 'fqdPlaces', label = NULL,
                     value = fastqCMD)
  })

#-Open Finder to see files
observeEvent(input$viewFiles, {
  outdir = getOutdirpath()
  system(sprintf("open %s", outdir))
})  

##Help-Text Popovers-----------------------------------------------------------------------------  
 addPopover(session, "selectHelp", title = NULL,content = paste0("Click on a row
          to select it, then choose an operation for it and press 'Submit'."), trigger = 'click')

 addPopover(session, "smartSearch", title = NULL,
           content = HTML(paste('<b>Smart Search Options: </b><ul>
                                <li><em>OR</em> to combine searches</li>
                                <li><em>column</em>:<em>value</em> for all reslts with given column value</li> 
                                <li><em>"Search Terms"</em> for full phrase search</li>
                                <li><em> term* </em> for all results that begin with term</li>
                                </ul>')),
           trigger = 'click')  
 
})

#END SERVER 
##

# Alert System:
# Automatically shows noSearch (search results tab) and noAction(on operaton tab) results 
# Valid search, removes no search results alert
# On action click
# -- Valid action + selected rows, closes  noAction and TBnoAction
# -- Invalid action/no rows, creates new TBnoAction on same page as search results


######################################################################################################
##################################### OTHER FUNCTUONS ################################################
######################################################################################################

# get HTML Link -------------------------------------------------
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
  sra_fields_indice <- list( srabrief = c(9,7,11,22,47,20,19,70,72,58,59), #sample title include 
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

disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}
############################################################################################




    


