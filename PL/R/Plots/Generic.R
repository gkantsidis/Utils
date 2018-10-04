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
    # TODO: Transform factors to numeric where applicable.
    # The code below will transform all strings to factors. Sometimes, the strings are actually numeric values.
    # It should figure out the columns that have that problem and transform them, e.g. with unfactor() from the varhandle package.

    data <- read.delim2("clipboard", header=header, sep=column_separator)
    return(data)
}