\name{fastqDump}
\alias{fastqDump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dump fastq file(s) from SRA
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fastqDump(sraAccession, outdir = getwd(), stdout = FALSE, split_files = TRUE, aligned = TRUE, unaligned = TRUE, zipFormat = "gzip", maxSpotId = -1, minSpotId = 0, minReadLen = 0, splitStyle = "", skip_technical = TRUE, split_spot = FALSE, fasta = FALSE, origfmt = FALSE, dumpcs = FALSE, dumpbase = FALSE, offset = FALSE, fastqDumpCMD)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sraAccession}{
%%     ~~Describe \code{sraAccession} here~~
}
  \item{outdir}{
%%     ~~Describe \code{outdir} here~~
}
  \item{stdout}{
%%     ~~Describe \code{stdout} here~~
}
  \item{split_files}{
%%     ~~Describe \code{split_files} here~~
}
  \item{aligned}{
%%     ~~Describe \code{aligned} here~~
}
  \item{unaligned}{
%%     ~~Describe \code{unaligned} here~~
}
  \item{zipFormat}{
%%     ~~Describe \code{zipFormat} here~~
}
  \item{maxSpotId}{
%%     ~~Describe \code{maxSpotId} here~~
}
  \item{minSpotId}{
%%     ~~Describe \code{minSpotId} here~~
}
  \item{minReadLen}{
%%     ~~Describe \code{minReadLen} here~~
}
  \item{splitStyle}{
%%     ~~Describe \code{splitStyle} here~~
}
  \item{skip_technical}{
%%     ~~Describe \code{skip_technical} here~~
}
  \item{split_spot}{
%%     ~~Describe \code{split_spot} here~~
}
  \item{fasta}{
%%     ~~Describe \code{fasta} here~~
}
  \item{origfmt}{
%%     ~~Describe \code{origfmt} here~~
}
  \item{dumpcs}{
%%     ~~Describe \code{dumpcs} here~~
}
  \item{dumpbase}{
%%     ~~Describe \code{dumpbase} here~~
}
  \item{offset}{
%%     ~~Describe \code{offset} here~~
}
  \item{fastqDumpCMD}{
%%     ~~Describe \code{fastqDumpCMD} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sraAccession, outdir = getwd(), stdout = FALSE, split_files = TRUE, 
    aligned = TRUE, unaligned = TRUE, zipFormat = "gzip", maxSpotId = -1, 
    minSpotId = 0, minReadLen = 0, splitStyle = "", skip_technical = TRUE, 
    split_spot = FALSE, fasta = FALSE, origfmt = FALSE, dumpcs = FALSE, 
    dumpbase = FALSE, offset = FALSE, fastqDumpCMD) 
{
    if (missing(sraAccession)) {
        stop("Please supply a SRA accession. Type fastqDump ('help') for help manual') ")
    }
    if (sraAccession == "help") {
        system(paste0(fastqDump_CMD, " -help"))
        return()
    }
    if (sraAccession == "version") {
        system(paste0(fastqDump_CMD, " --version"))
        return()
    }
    if (!file.exists(fastqDumpCMD)) {
        stop("SRA toolkit not found. Please specify fastqDumpCMD location, or \n      download SRA toolkit from http://www.ncbi.nlm.nih.gov/Traces/sra/sra.cgi?view=software")
    }
    if (!file.exists(outdir)) {
        warning(paste("output dir (' ", outdir, "') does not exist \n                   and the current working dir ('", 
            getwd(), "') will be used."))
        outdir = getwd()
    }
    opt_outdir = paste("-O", outdir)
    if (split_files) {
        opt_split_files = " --split-files"
    }
    else {
        opt_split_files = ""
    }
    if (split_spot) 
        opt_splitspot = "--split-spot"
    else opt_splitspot = ""
    if (fasta) {
        opt_fasta = "--fasta"
        if (is.numeric(fasta)) 
            opt_fasta = paste("--fasta", fasta)
    }
    else opt_fasta = ""
    if (origfmt) {
        opt_origfmt = "-F"
    }
    else {
        opt_origfmt = ""
    }
    if (dumpcs) {
        opt_dumpcs = "--dumpcs"
        if (dumpcs != TRUE && dumpcs != FALSE) 
            opt_dumpcs = paste(oopt_dumpcs, dumpcs)
    }
    else {
        opt_dumpcs = ""
    }
    if (dumpbase) 
        opt_dumpbase = "--dumpbase"
    else opt_dumpbase = ""
    if (offset) {
        if (is.numeric(offset)) 
            opt_offset = paste("--offset", offset)
        else opt_offset = paste("--offset")
    }
    else {
        opt_offset = ""
    }
    if (maxSpotId < 0) {
        if (maxSpotId < -1) 
            warning("Negative values not accepted for maxSpotId. Defaulted to full length")
        opt_maxSpotId = ""
    }
    else {
        opt_maxSpotId = paste(" -X ", maxSpotId, sep = "")
    }
    if (minSpotId <= 0) {
        if (maxSpotId < -1) {
            warning("Negative values not accepted for minSpotId. Defaulted to 0")
        }
        opt_minSpotId = ""
    }
    else {
        opt_minSpotId = paste(" -N ", minSpotId, sep = "")
    }
    if (minReadLen <= 0) {
        if (minReadLen < 0) 
            warning("Negative Values and non-integer values not accepted for minReadLength. \n               minReadLength defaulted to 0.")
        opt_minReadLen = ""
    }
    else {
        opt_minReadLen = paste("--minReadLen", minReadLen)
    }
    if (is.element(zipFormat, c("gzip", "bzip2", "stdout"))) {
        opt_zipFormat = paste(" --", zipFormat, sep = "")
        if (zipFormat == "stdout") 
            opt_outdir = ""
    }
    else opt_zipFormat = ""
    print(opt_zipFormat)
    if (is.element(splitStyle, c("split-3", "spot--group", "read-filter"))) {
        opt_splitStyle = paste(" --", splitStyle, sep = "")
    }
    else {
        opt_splitStyle = ""
    }
    print(opt_splitStyle)
    if (skip_technical) {
        opt_skip_technical = "--skip-technical"
    }
    else {
        opt_skip_technical = ""
    }
    opt_align = ""
    if (aligned) {
        opt_align <- paste(opt_align, "--aligned")
    }
    if (unaligned) {
        opt_align <- paste(opt_align, "--unaligned")
    }
    message <- system(paste(fastqDumpCMD, opt_maxSpotId, opt_minSpotId, 
        opt_minReadLen, opt_outdir, opt_zipFormat, opt_skip_technical, 
        opt_fasta, opt_origfmt, opt_dumpbase, opt_dumpcs, opt_offset, 
        opt_split_files, opt_splitStyle, sraAccession), intern = TRUE)
    print(message)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
