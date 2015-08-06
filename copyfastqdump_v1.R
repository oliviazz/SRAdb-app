#   fastqDump_v1.R | Olivia Zhang | 6 July 2015 


fastqDump <- function( sraAccession, outdir = getwd(), stdout = FALSE, split_files = TRUE,
                              aligned = TRUE, unaligned = TRUE, 
                              zipFormat = 'gzip', maxSpotId = -1, minSpotId = 0, minReadLen = 0,
                              splitStyle = '', skip_technical = TRUE, 
                              split_spot = FALSE, fasta = FALSE, 
                              origfmt = FALSE, dumpcs = FALSE,dumpbase = FALSE,offset = FALSE,
                              fastqDumpCMD ){
                              #"/Applications/bin/sratoolkit.2.5.2-mac64/bin/fastq-dump"
                              #"/Users/zhangoz/sra_toolkit/bin/fastq-dump.2.5.2"){
 
  if( missing(sraAccession)){
    stop( "Please supply a SRA accession. Type fastqDump ('help') for help manual') ")
  }
  

  ## Help manual 
  if ( sraAccession == 'help' ){
    system( paste0(fastqDump_CMD, ' -help') )
    return()
  }
  
  ## Display version
  if ( sraAccession == 'version' ){
    system( paste0(fastqDump_CMD, ' --version') )
    return()
  }
  
  ## Check if fastqDumpCMD exists
    if( !file.exists(fastqDumpCMD) ){
      stop('SRA toolkit not found. Please specify fastqDumpCMD location, or 
      download SRA toolkit from http://www.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software')
   }

  ## Check if outdir exists
  if( !file.exists(outdir) ) {
    warning( paste("output dir (' ", outdir, "') does not exist 
                   and the current working dir ('", getwd(), "') will be used."))
    outdir = getwd()
  }
  opt_outdir = paste('-O', outdir)
  
  ## Split files
  if( split_files ) {
    opt_split_files = ' --split-files'
  } else {
    opt_split_files = ''
  }
  ## Split Spot 
  if( split_spot)
    opt_splitspot = '--split-spot'
  else
    opt_splitspot = ''
  
  ## Fasta 
  if(fasta){
    opt_fasta = '--fasta'
    if(is.numeric(fasta))
      opt_fasta = paste('--fasta', fasta)
  }
  else
    opt_fasta = ''
  
  ## Original Format 
  if(origfmt){
      opt_origfmt = '-F'
  }
  else{
      opt_origfmt = ''
  }
  
  ## DumpCS
 if(dumpcs){
      opt_dumpcs = '--dumpcs'
      if (dumpcs != TRUE && dumpcs != FALSE)
        opt_dumpcs = paste(oopt_dumpcs, dumpcs)
  }
  else{
      opt_dumpcs = ''
  }
  ## DumpBase
  if(dumpbase)
      opt_dumpbase = '--dumpbase'
  else
      opt_dumpbase = ''
  
  ## Offset
  if(offset){
    if(is.numeric(offset))
      opt_offset = paste('--offset', offset)
    else
      opt_offset = paste('--offset')
  }
  else{
    opt_offset = ''
  }
  ## MaxSpotId
  if( maxSpotId < 0) {
    if (maxSpotId < -1)
      warning("Negative values not accepted for maxSpotId. Defaulted to full length")
    opt_maxSpotId = ''  #not specified 
  } 
  else {
    opt_maxSpotId = paste(' -X ', maxSpotId, sep = '')
  }
  
  ## MinSpotId
  if( minSpotId <= 0) {
    if (maxSpotId < -1)
      {warning("Negative values not accepted for minSpotId. Defaulted to 0")
    }
    opt_minSpotId = ''  #not specified 
  }
  else{
    opt_minSpotId = paste(' -N ', minSpotId, sep = "")
  }
  
  ## MinReadLength
  if(minReadLen <= 0){
    if (minReadLen < 0)
      warning('Negative Values and non-integer values not accepted for minReadLength. 
               minReadLength defaulted to 0.')
    opt_minReadLen = ''
  }
  else{
    opt_minReadLen = paste('--minReadLen', minReadLen )
  }
  
  ## Zipformat
  if( is.element(zipFormat, c('gzip', 'bzip2', 'stdout') ) )
    {opt_zipFormat = paste(' --', zipFormat, sep = '')
    if(zipFormat == 'stdout')
      opt_outdir = ''
  }
  else 
    opt_zipFormat = ''
  print(opt_zipFormat)
  
  ## Split styles (split3, spotgroup, readfilter)
  if( is.element(splitStyle, c( 'split-3', 'spot--group','read-filter'))) {
    opt_splitStyle = paste(' --', splitStyle, sep = '')
  } else {
    opt_splitStyle= ''
  }
  print(opt_splitStyle)
  
  ## Skip technical
  if(skip_technical){
      opt_skip_technical = '--skip-technical'
  } else {
    opt_skip_technical = ''
  }
  
  ## Aligned
  opt_align = ''
  if(aligned){
    opt_align <- paste(opt_align, '--aligned')
  }
  if (unaligned){
    opt_align <- paste(opt_align, '--unaligned')
  }

#   
#   ### Debugging Help 
#     print(paste( fastqDumpCMD,
#                  opt_maxSpotId,opt_minSpotId, opt_minReadLen, opt_outdir,
#                  opt_zipFormat, opt_skip_technical, opt_fasta,
#                  opt_origfmt, opt_dumpbase, opt_dumpcs, opt_offset,
#                  opt_split_files, opt_splitStyle, sraAccession))
  
    message <- system(paste( fastqDumpCMD,
                opt_maxSpotId,opt_minSpotId, opt_minReadLen, opt_outdir,
                opt_zipFormat, opt_skip_technical, opt_fasta,
                opt_origfmt, opt_dumpbase, opt_dumpcs, opt_offset,
                opt_split_files, opt_splitStyle, sraAccession), intern = TRUE)
   
   print(message) 
   
  
  
}



