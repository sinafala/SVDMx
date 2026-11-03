#' Many life tables from a file of inputs
#'
#' @description Read a file (or data frame) with one row for each life table wanted, giving its sex, 5q0 and optionally 45q15 and other settings, make all the life tables with \code{\link{lifetable}}, and optionally write them to Excel, CSV, fixed-column text or JSON files.
#' @author Samuel J. Clark, \email{work@samclark.net}
#'
#' @param input A data frame, or the name of a file ending in .csv, .txt (columns separated by tabs or spaces, with a header line) or .xlsx (the first sheet; needs the \pkg{readxl} package), with one row for each life table and these columns (names not case sensitive):
#' * sex (required): female or male (or f, m);
#' * cm (required): 5q0, also accepted under the names 5q0 or q5;
#' * am (optional): 45q15, also accepted under the names 45q15 or q45; leave it empty (or NA) to predict 45q15 from 5q0;
#' * id (optional): a name for the life table; the default is LT1, LT2, ...;
#' * modsv, smooth, match.inputs, out5 and radix (optional): settings for each life table, as in \code{\link{lifetable}}; where a column is missing or a value is empty, the argument of the same name below is used.
#'
#' Any other columns, such as country or year, are kept as labels in the summary of the life tables and in the files written.
#' @param file Optional character: one or more file names to write the life tables to, in the format given by each extension: .xlsx, .csv, .txt or .json; see \code{\link{writeLifetables}}. Default is NULL (no file).
#' @param smooth,out5,modsv,match.inputs,radix Defaults for the settings that the input does not give; see \code{\link{lifetable}}.
#' @return An object of class \code{lifetables}, as returned by \code{\link{lifetable}}, with one life table for each row of the input, in order. Its summary carries the input's id and label columns.
#'
#' @details Every row is checked before any file is written. If any row cannot be used (for example, 5q0 outside the range of the selected calibration), \code{lifetableBatch()} stops with a list of the rows and the problems, and writes nothing. Warnings for individual rows, such as those from \code{qx()} with \code{match.inputs = FALSE}, are collected into one warning that names the rows.
#'
#' @examples
#' batch <- data.frame(country=c("A","A","B"),sex=c("female","male","female"),
#'                     cm=c(0.02,0.025,0.1),am=c(0.08,NA,0.3))
#' lts <- lifetableBatch(batch)
#' lts$summary
#' f <- file.path(tempdir(),"batch.csv")
#' write.csv(batch,f,row.names=FALSE)
#' lifetableBatch(f,file=file.path(tempdir(),c("batch-lifetables.csv","batch-lifetables.txt")))
#' @seealso \code{\link{lifetable}}, \code{\link{writeLifetables}}.
#' @export
lifetableBatch <- function(input,file=NULL,smooth=TRUE,out5=TRUE,modsv=2026,match.inputs=TRUE,radix=100000) {

  # read the input and find its columns
  inp <- readBatchInput(input)
  if (nrow(inp) == 0)
    stop('<input> has no rows')
  key <- tolower(gsub("^[Xx](?=[0-9])","",trimws(names(inp)),perl=TRUE))
  aliases <- list(
    sex = c("sex"),
    cm = c("cm","5q0","q5"),
    am = c("am","45q15","q45"),
    id = c("id"),
    modsv = c("modsv"),
    smooth = c("smooth"),
    match.inputs = c("match.inputs","match_inputs","matchinputs"),
    out5 = c("out5"),
    radix = c("radix")
  )
  column <- function(name) {
    k <- which(key %in% aliases[[name]])
    if (length(k) > 1)
      stop(paste('<input> has more than one column for ',name,': ',paste(names(inp)[k],collapse=", "),sep=''))
    if (length(k) == 0) NULL else inp[[k]]
  }
  if (is.null(column("sex")) | is.null(column("cm")))
    stop('<input> must have columns for sex and 5q0 (named sex and cm, 5q0 or q5)')
  used <- which(key %in% unlist(aliases))
  labels <- inp[,setdiff(seq_along(inp),used),drop=FALSE]

  # one value for each row: from the input where given, otherwise the default
  n <- nrow(inp)
  empty <- function(v) is.na(v) | trimws(as.character(v)) == ""
  setting <- function(name,default,convert) {
    v <- column(name)
    if (is.null(v)) return(rep(default,n))
    out <- convert(v)
    bad <- !empty(v) & is.na(out)
    if (any(bad))
      stop(paste('<input> column ',name,' has values that cannot be read in rows ',
                 paste(utils::head(which(bad),10),collapse=", "),sep=''))
    out[empty(v)] <- default
    return(out)
  }
  number <- function(v) suppressWarnings(as.numeric(v))
  sex <- tolower(trimws(as.character(column("sex"))))
  sex[sex == "f"] <- "female"
  sex[sex == "m"] <- "male"
  cm <- setting("cm",NA,number)
  am <- if (is.null(column("am"))) rep(NA,n) else setting("am",NA,number)
  ids <- if (is.null(column("id"))) paste("LT",1:n,sep="") else as.character(column("id"))
  modsvs <- setting("modsv",modsv,number)
  smooths <- setting("smooth",smooth,asLogical)
  matches <- setting("match.inputs",match.inputs,asLogical)
  out5s <- setting("out5",out5,asLogical)
  radixes <- setting("radix",radix,number)
  if (any(duplicated(ids)))
    stop(paste('<input> ids must be distinct; repeated:',paste(unique(ids[duplicated(ids)]),collapse=", ")))

  # make each life table, collecting problems and warnings by row
  tables <- list()
  summaries <- list()
  problems <- character(0)
  warns <- character(0)
  for (r in 1:n) {
    res <- withCallingHandlers(
      tryCatch(
        lifetable(sex[r],cm[r],smooth=smooths[r],out5=out5s[r],am=if (is.na(am[r])) NULL else am[r],
                  modsv=modsvs[r],match.inputs=matches[r],radix=radixes[r]),
        error=function(e) e),
      warning=function(w) {
        warns <<- c(warns,paste("row ",r," (",ids[r],"): ",conditionMessage(w),sep=""))
        invokeRestart("muffleWarning")
      })
    if (inherits(res,"error")) {
      problems <- c(problems,paste("row ",r," (",ids[r],"): ",conditionMessage(res),sep=""))
    } else {
      tables[[r]] <- res$tables[[1]]
      summaries[[r]] <- res$summary
    }
  }
  if (length(problems) > 0)
    stop(paste(length(problems)," of ",n," rows of <input> cannot be used, so no life tables were made:\n",
               paste(utils::head(problems,20),collapse="\n"),
               if (length(problems) > 20) paste("\n... and",length(problems) - 20,"more") else "",sep=""),call.=FALSE)
  if (length(warns) > 0)
    warning(paste(length(warns)," warning(s) for individual rows:\n",paste(utils::head(warns,10),collapse="\n"),
                  if (length(warns) > 10) paste("\n... and",length(warns) - 10,"more") else "",sep=""),call.=FALSE)

  # the batch: the input's ids and labels, then each life table's summary
  summary <- do.call(rbind,summaries)
  summary$id <- ids
  summary <- cbind(summary["id"],labels,summary[,names(summary) != "id",drop=FALSE])
  rownames(summary) <- NULL
  names(tables) <- ids

  lt <- structure(list(summary=summary,tables=tables),class="lifetables")
  if (!is.null(file)) {
    writeLifetables(lt,file)
  }
  return(lt)

}

# read the input of lifetableBatch(): a data frame, or a .csv, .txt or .xlsx file
readBatchInput <- function(input) {
  if (is.data.frame(input))
    return(as.data.frame(input,stringsAsFactors=FALSE))
  if (!is.character(input) || length(input) != 1)
    stop('<input> must be a data frame or the name of a .csv, .txt or .xlsx file')
  if (!file.exists(input))
    stop(paste('<input> file not found:',input))
  ext <- tolower(sub("^.*\\.([^.]+)$","\\1",input))
  if (ext == "csv") {
    inp <- utils::read.csv(input,stringsAsFactors=FALSE,check.names=FALSE,strip.white=TRUE)
  } else if (ext %in% c("txt","tsv")) {
    first <- readLines(input,n=1,warn=FALSE)
    sep <- if (grepl("\t",first)) "\t" else ""
    inp <- utils::read.table(input,header=TRUE,sep=sep,stringsAsFactors=FALSE,check.names=FALSE,strip.white=TRUE)
  } else if (ext == "xlsx") {
    if (!requireNamespace("readxl",quietly=TRUE))
      stop('reading Excel files needs the readxl package: install.packages("readxl")')
    inp <- as.data.frame(readxl::read_excel(input),stringsAsFactors=FALSE)
  } else {
    stop('<input> file must end in .csv, .txt or .xlsx')
  }
  return(inp)
}

# TRUE/FALSE from logical, number or text (TRUE, T, yes, 1 and so on)
asLogical <- function(v) {
  if (is.logical(v)) return(v)
  v <- tolower(trimws(as.character(v)))
  out <- rep(NA,length(v))
  out[v %in% c("true","t","yes","y","1")] <- TRUE
  out[v %in% c("false","f","no","n","0")] <- FALSE
  return(out)
}
