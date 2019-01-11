#
# Various examples of using the commands provided in this directory

# The following assumes that you work with RStudio.
rootDir <- dirname(rstudioapi::getActiveDocumentContext()$path)

source(file.path(rootDir, "Utils.R"))

# In your script, you may use the following to get the full path of the script.
ez.csf()