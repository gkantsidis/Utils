#
# Utils.R
#
# Various utilities of general use

#' current script file (in full path)
#' @description current script file (in full path)
#' @examples
#' works with Rscript, source() or in RStudio Run selection, RStudio Console
#' @export
ez.csf <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = grep(needle, cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    return(normalizePath(sub(needle, "", cmdArgs[match])))
  } else {
    ls_vars = ls(sys.frames()[[1]])
    if ("fileName" %in% ls_vars) {
      # Source'd via RStudio
      return(normalizePath(sys.frames()[[1]]$fileName))
    } else {
      if (!is.null(sys.frames()[[1]]$ofile)) {
        # Source'd via R console
        return(normalizePath(sys.frames()[[1]]$ofile))
      } else {
        # RStudio Run Selection
        # http://stackoverflow.com/a/35842176/2292993
        pth = rstudioapi::getActiveDocumentContext()$path
        if (pth!='') {
          return(normalizePath(pth))
        } else {
          # RStudio Console
          tryCatch({
            pth = rstudioapi::getSourceEditorContext()$path
            pth = normalizePath(pth)
          }, error = function(e) {
            # normalizePath('') issues warning/error
            pth = ''
          }
          )
          return(pth)
        }
      }
    }
  }
}

#' path of current script file
#' @description path of current script file
#' @examples
#' works with Rscript, source() or in RStudio Run selection, RStudio Console
#' @export
ez.csp <- function () { dirname(ez.csf()) }