#server.R  SRAdb-app 
#Olivia Zhang   July 9, 2015 

#..........TO DO:..........................
# FastQ dump, Diagram from R (?)
# shift select many 
#..........................................
# Imports and Loading 
#===========================================#
library(shiny)
library(SRAdb)
library(DT)
library(shinyBS)
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
    isolate({
    dataType <- input$dataType
    searchTerms <- input$searchTerms
    })
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
    input$searchButton
    isolate({
      searchTerms = input$searchTerms
      if(searchTerms != "")
      { 
        table  <- getFullTable() 
      }
      })
    }, rownames = TRUE,
    escape = FALSE,
    extensions = c('ColVis','ColReorder', 'TableTools'),
    options = list(dom = 'RC<"clear">lifrStp',
                   scrollX = TRUE, scrollCollapse = TRUE,
                   autoWidth = TRUE,
                   colReorder = list(realtime = TRUE),
                   lengthMenu = c(20,50, 100, 200),pageLength = 50,
                   searchHighlight = TRUE,
                   #tableTools = list(sSwfPath = copySWF()),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css
                     ({'background-color': '#008B8B', 'color': '#fff'});",
                     "}"
                   )
#                 columnDefs = list(list(
#                      targets = 6,
#                      render = JS(
#                      "function(data, type, row, meta) {",
#                      "return type === 'display' && data.length > 6 ?",
#                      "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
#                      "}")
#                    ))
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
        selected_acc  = input$mainTable_rows_selected #gets row indexes 
        n <- n[selected_acc,]#
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
  
#%%%%%%%%%%%%%%%%%%% Display Operation Results %%%%%%%%%%%%%%%%%%%%%%%  
  
#Trigger Operation when ActionButton Pressed ----------------------------
  observeEvent(input$actionButton, {
    rows_selected <- input$mainTable_rows_selected
    if(length(rows_selected) != 0){
      closeAlert(session, alertId = "alert")
      n <- getFullTable()
      n_selected <<- n[rows_selected,]
       output$operationMessage <- renderText({"Loading..."})
       switch( input$operationType,
            "srainfo"= updateTabsetPanel(session, "tabSet",
                                         selected = "operation"),
            "fqinfo" = updateTabsetPanel(session, "tabSet",
                                         selected = "operation"),
            "eGraph" = updateTabsetPanel(session, "tabSet",
                                         selected = "operation"),
            "fastqdump" = {
              options <- input$fqd_options
              print(options)
              splitStyle <- input$fqd_splitStyle
              minSpotId <- input$fqd_min
              maxSpotId <- input$fqd_max
              outdir <- getOutdirpath()
              if(maxSpotId <1 || minSpotId < 0 ){
                maxSpotId = -1
                minSpotId = 0
              }
            
              if(!is.element("run", colnames(n_selected))){
                createAlert(session, "fqdalert", "fqdnotRun", title = "Error:",
                            content = "Please select data of type 'run'.",style = "danger",append = FALSE)
              }
              else{
                run_code <- n_selected[,"run"]
                if(length(run_code) > 1){
                   createAlert(session, "fqdalert", "fqdmultiple", title = "Error:",
                                content = "Multiple runs are selected. Please choose only 
                               one to perform Fastq dump.", style = "danger",append = FALSE)
                }
                else
                {  
                message <- capture.output(fastqDump(run_code, minSpotId = minSpotId, maxSpotId = maxSpotId,
                          outdir = outdir, splitStyle = splitStyle,
                          split_spot = is.element("split_splot", options),
                          skip_technical = is.element("split_splot", options),
                          origfmt = is.element("origfmt", options),
                          fasta = is.element("fasta", options),
                          dumpbase = is.element("dumpbase ", options),
                          dumpcs = is.element("dumpcs", options)
                          )
                          #Captures what is printed in fastqdump. 
                )
                  
                  message <- paste(gsub('"', "",message),sep="",collapse="")
                  createAlert(session, "fqdalert", "fqdsuccessAlert",title = "FASTQ Dump completed",
                              content = message, style = "success")
                }
              }
            }
    )
       output$operationMessage <- renderText("")
    }
    else{
      createAlert(session, "alert", alertId = "noRows",title = "No Rows Selected",
                  content = "Please select at least one row to perform operation on.")
      createAlert(session, "fqdalert",alertId = "fqdnoRows" ,title = "No Rows Selected",
                  content = "Please select at least one row to perform operation on.",
                  style = "danger")
    }
    })
# Operation Message ---------------------------------------------
  observeEvent( input$tabSet == 'operation',{
          
            if(( input$operationType == 'none' || input$actionButton == 0)
               && input$tabSet == 'operation'){
              createAlert(session, "alert", alertId = "noAction", title = "No results to display",
                          content = "No action submitted for selected rows.", append = FALSE)
              }
            }
  ) 
# Results Table for selected operations--------------------------
  output$operationResultsTable <- renderDataTable({
  input$actionButton #Table not drawn until Action button clicked 
  isolate({
    operationType = input$operationType
    selected_rows  = as.integer(input$mainTable_rows_selected)
    if(length(selected_rows) != 0){
        #n <- getFullTable()
        #n_selected <<- n[selected_rows,]
        switch( input$operationType,
           "fqinfo" = {
             if(!is.element("run", colnames(n_selected)))
               { createAlert(session, "alert", "notRunAlert", title = "Wrong Data Type",
                             content = "Please select data of type 'run'", append = FALSE,
                             style = "danger")
             }
             else{
               run_codes <- as.vector(n_selected[,"run"])
               fqinfo <- getFASTQinfo(run_codes)
               fqinfo$ftp <- createLink(fqinfo$ftp)
               newBottom <- fqinfo[,1:5]  #rearrange columns to look better
               newTop <- fqinfo[,6:11]
               fqinfo <- cbind(newTop, newBottom)
             }
           },
           "srainfo" = {
             if(!is.element("run", colnames(n_selected)))
             { createAlert(session, "alert", "notRunAlert", title = "Wrong Data Type",
                           content = "Please select data of type 'run'", append = FALSE,
                           style = "danger")
             }
             else{
               run_codes <- as.vector(n_selected[,"run"])
               fqinfo <- getSRAinfo(run_codes, sra_con = sra_con, sraType = "sra")
               fqinfo$ftp <- createLink(fqinfo$ftp)
               fqinfo
             }
           }
        )
    }
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
#                columnDefs = list(list(
#                  targets = 1:7,
#                 render = JS(
#                  "function(data, type, row, meta) {",
#                  "return type === 'display' && data.length > 6 ?",
#                  "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
#                  "}")
#               ))
          )      
)

output$eGraph <- renderPlot({
  runcodes <- n_selected[,"run"]
  acc_table <-  sraConvert( runcodes, sra_con = sra_con )
  g <- sraGraph_1(acc_table)
  attrs <- getDefaultAttrs(list(node=list(fillcolor='lightblue', shape='ellipse')))
  plotPNG(plot(g, attrs=attrs))
})

})#END SERVER 

######################################################################################################
##################################### OTHER FUNCTUONS ################################################
######################################################################################################

# Clickable HTML Link Given URL(val)------------------------------
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" 
          class="btn btn-link"> %s </a>',val,val)
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

sraGraph_1 <- function(search_terms,  acc_table) 
{ 
  g <- entityGraph(acc_table)
  return(g)
}
############################################################################################




    


