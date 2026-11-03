#' Write life tables to Excel, CSV, fixed-column text or JSON files
#'
#' @description Write life tables made by \code{\link{lifetable}} or \code{\link{lifetableBatch}} to one or more files, in the format given by each file's extension.
#' @author Samuel J. Clark, \email{work@samclark.net}
#'
#' @param x An object of class \code{lifetables}, from \code{lifetable()} or \code{lifetableBatch()}.
#' @param file Character: one or more file names, each ending in
#' * .xlsx: an Excel workbook with a 'summary' sheet and one sheet for each life table (needs the \pkg{writexl} package);
#' * .csv: all the life tables in one table, each row carrying the summary of its life table;
#' * .txt: fixed-column text, each life table under a heading that describes it, as printed;
#' * .json: an array with one object for each life table, holding its summary and its rows (needs the \pkg{jsonlite} package).
#'
#' The Excel, CSV and JSON files hold the values at full precision; the text file rounds them for reading: nmx and nqx to 6 decimal places, nax to 3, lx, ndx, nLx and Tx to whole numbers, and ex to 2.
#' @return The file names, invisibly.
#' @examples
#' lt <- lifetable("female",c(0.02,0.1))
#' writeLifetables(lt,file.path(tempdir(),c("lifetables.csv","lifetables.txt")))
#' @export
writeLifetables <- function(x,file) {

  if (!inherits(x,"lifetables"))
    stop('<x> must be life tables made by lifetable() or lifetableBatch()')
  if (!is.character(file) || length(file) == 0 || any(is.na(file)))
    stop('<file> must be one or more file names')
  ext <- tolower(sub("^.*\\.([^.]+)$","\\1",file))
  bad <- !(ext %in% c("xlsx","csv","txt","json")) | !grepl("\\.",file)
  if (any(bad))
    stop(paste('<file> must end in .xlsx, .csv, .txt or .json:',paste(file[bad],collapse=", ")))
  if (any(ext == "xlsx") && !requireNamespace("writexl",quietly=TRUE))
    stop('writing Excel files needs the writexl package: install.packages("writexl")')
  if (any(ext == "json") && !requireNamespace("jsonlite",quietly=TRUE))
    stop('writing JSON files needs the jsonlite package: install.packages("jsonlite")')

  for (i in seq_along(file)) {
    if (ext[i] == "csv") {
      utils::write.csv(as.data.frame(x),file[i],row.names=FALSE,na="")
    } else if (ext[i] == "txt") {
      lines <- unlist(lapply(seq_along(x$tables),function(j) c(formatLifetable(x,j),"")))
      writeLines(lines,file[i])
    } else if (ext[i] == "xlsx") {
      sheets <- c(list(summary=x$summary),x$tables)
      names(sheets) <- c("summary",sheetNames(names(x$tables)))
      writexl::write_xlsx(sheets,file[i],format_headers=TRUE)
    } else if (ext[i] == "json") {
      out <- lapply(seq_along(x$tables),function(j) {
        list(summary=as.list(x$summary[j,,drop=FALSE]),lifetable=x$tables[[j]])
      })
      jsonlite::write_json(out,file[i],pretty=TRUE,digits=NA,auto_unbox=TRUE,na="null",dataframe="rows")
    }
  }

  return(invisible(file))

}

# Excel sheet names: at most 31 characters, none of []:*?/\, and distinct
sheetNames <- function(ids) {
  s <- ids
  for (ch in c("[","]",":","*","?","/","\\")) {
    s <- gsub(ch,"_",s,fixed=TRUE)
  }
  s <- substr(s,1,31)
  s[s == "summary"] <- "summary_"
  dup <- duplicated(s) | duplicated(s,fromLast=TRUE)
  if (any(dup)) {
    s[dup] <- paste(substr(s[dup],1,26),"_",seq_len(sum(dup)),sep="")
  }
  return(s)
}

# one life table of x as lines of fixed-column text, under a heading that describes it
formatLifetable <- function(x,i) {

  s <- x$summary[i,]
  t <- x$tables[[i]]

  # heading: the life table's id, inputs, settings and results, then any other labels
  fixed <- c("id","sex","table","cm","am","modsv","smooth","match.inputs","cm.lt","am.lt","e0")
  heading <- paste("Life table ",s$id,": ",s$sex,", ",s$table,sep="")
  inputs <- paste("  5q0 = ",signif(s$cm,4),
                  ifelse(is.na(s$am),", 45q15 predicted from 5q0",paste(", 45q15 = ",signif(s$am,4),sep="")),
                  "; modsv = ",s$modsv,", smooth = ",s$smooth,", match.inputs = ",s$match.inputs,sep="")
  results <- paste("  life table: 5q0 = ",formatC(s$cm.lt,format="f",digits=6),
                   ", 45q15 = ",formatC(s$am.lt,format="f",digits=6),
                   ", e0 = ",formatC(s$e0,format="f",digits=2),sep="")
  others <- setdiff(names(s),fixed)
  labels <- if (length(others) > 0) paste("  ",paste(others,"=",sapply(s[others],as.character),collapse=", "),sep="") else NULL

  # columns: name, values formatted, width
  cols <- list(
    c("age",formatC(t$age,width=-7)),
    c("n",ifelse(is.na(t$n),"",formatC(t$n,format="d"))),
    c("nmx",formatC(t$nmx,format="f",digits=6)),
    c("nqx",formatC(t$nqx,format="f",digits=6)),
    c("nax",formatC(t$nax,format="f",digits=3)),
    c("lx",formatC(t$lx,format="f",digits=0)),
    c("ndx",formatC(t$ndx,format="f",digits=0)),
    c("nLx",formatC(t$nLx,format="f",digits=0)),
    c("Tx",formatC(t$Tx,format="f",digits=0)),
    c("ex",formatC(t$ex,format="f",digits=2))
  )
  widths <- sapply(cols,function(c) max(nchar(c)))
  # the age column is left-aligned, the others right-aligned
  body <- mapply(function(c,w,left) formatC(c,width=ifelse(left,-w,w)),cols,widths,c(TRUE,rep(FALSE,9)))
  rows <- apply(body,1,paste,collapse="  ")

  return(c(heading,inputs,results,labels,"",rows))

}
