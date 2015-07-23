#server.R  SRAdb-app 
#Olivia Zhang   July 9, 2015 

#..........TO DO:..........................
#
# FastQ dump, Diagram from R (?)
# shift select many 
#..........................................

#===========================================#
library(shiny)
library(SRAdb)
library(DT)
sra_dbname <- 'SRAmetadb.sqlite'	
if(!file.exists('SRAmetadb.sqlite'))
  sqlfile <<- getSRAdbFile()
sra_con<- dbConnect(dbDriver("SQLite"), 
                    sra_dbname) 
source('~/sra_toolkit/bin/fastqDump_v1.R')
#===========================================#
#Start Shiny Server 
shinyServer(function(input,output,session){
                                # Any danger w/ including "session" as arg?
  
#Return Table fromsearch terms, data type-------------------------- 
  getFullTable <- reactive({
    dataType <- input$dataType
    searchTerms <- input$searchTerms
    if(dataType == 'sra_acc'){
      
          n <- getSRA_1(search_terms = searchTerms, sra_con = sra_con, 
                      out_types = 'run', acc_only = TRUE)
          run_codes <- n[,"run"]
          n <- listSRAfile( as.vector(run_codes), sra_con, 
                            fileType = 'sra')
          #getsize <- getSRAinfo(as.vector(run_codes),sra_con, sraType = 'sra')
          #print(getsize)
          n$ftp <- createLink(n$ftp) #links to download SRA
          
          #n <- cbind(n,getsize[,"size(KB)"])
          
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
      {table  <- getFullTable() 
      }
      })
    }, rownames = TRUE,
    escape = FALSE,
    extensions = c('ColVis','ColReorder'),
    options = list(dom = 'RC<"clear">lifrtp',
                   scrollX = TRUE, scrollCollapse = TRUE,
                   colReorder = list(realtime = TRUE),
                   lengthMenu = c(15, 50, 100),pageLength = 15,
                   searchHighlight = TRUE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css
                     ({'background-color': '#008B8B', 'color': '#fff'});",
                     "}"
                   )
#                    columnDefs = list(list(
#                      targets = 6,
#                      render = JS(
#                      "function(data, type, row, meta) {",
#                      "return type === 'display' && data.length > 6 ?",
#                      "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
#                      "}")
#                    ))
                   ))

#Download entire SRA Table-------------------------------------- 
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
  
#Download selected Columns-------------------------------------
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
  
  #Display chosen directory
  output$fqd_outdirpath <- renderPrint({
    roots = c(wd='/Users')
    print(parseDirPath(roots , input$fqd_outdir))
  })
  
  #Render Filesystem choosing  
  shinyDirChoose("fqd_outdir", input = input, session = session,
               roots=c(wd = '/Users'), filetypes=c('', '.*'))

  
  #Perform actions on selected rows-------------------------------  
  
  
  observeEvent(input$actionButton, 
               { 
                 actionType = input$actionType
                 selected_rows  = as.integer(input$mainTable_rows_selected)
                 if(length(selected_rows) != 0){
                   n = getFullTable()
                   n_selected <- n[selected_rows,]
                   print(n_selected)
                   switch(actionType,
                          
                          
                          "fqinfo" = {
                            print(colnames(n_selected))
                            if(!is.element("run", colnames(n_selected))){
                              warning('Select data of type "run"')
                            }
                            else{
                              run_codes <- as.vector(n_selected[,"run"])
                              fqinfo <- getFASTQinfo(run_codes, srcType = 'ftp')
                              updateTabsetPanel(session, "tabSet", selected = "operation")
                              update
                            }
                          },
                          "fastqdump" = {
                            print('hello')
                            splitStyle <- input$fqd_splitStyle
                            minSpotId <- input$fqd_min
                            maxSpotId <- input$fqd_max
                            outdir <- "output.fqd_outdirpath"
                            updateTabsetPanel(session, "tabSet", selected = "operation")
                            print(outdir)
                            
                          }
                   )
                 }
                 else{
                   warning('Choose columns!')
                 }
                 
               })
  
})
#######_FUNCTIONS_#######################################

# Bring Directory Selection pop-up-------------------------------
choose.dir <- function() {
  system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder", 
         intern = FALSE, ignore.stderr = TRUE)
  p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  return(ifelse(length(p), p, NA))
}

# Clickable HTML Link Given URL(val)------------------------------
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" 
          class="btn btn-link"> Download Run SRA </a>',val)
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

############################################################################################




    


