#' Copies data from the system clipboard
#'
#' Copies the data from the clipboard to a local variable.
#' It expects that the data in clipboard are in a table format.
#' @param header Whether the data contains names for the columns, i.e. header. (logical)
#' @param column_separator Separator for the columns. (character)
#' @returns The data read from the clipboard.
#' @examples
#' data <- copy_from_clipboard()
copy_from_clipboard <- function(header=TRUE, column_separator=",") {
    data <- read.delim2("clipboard", header=header, sep=column_separator)
    return(data)
}