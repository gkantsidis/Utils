packages = c(
    "data.table",
    "dplyr",
    "filehash",             # key-value database
    "ggplot2",
    "plyr",                 # tools for splitting, applying, and combining data
    "psych",                # tools for psychological, psychometric, and personality research,
                            #   including read.clipboard.csv()
    "ProjectTemplate",      # automate creation of new projects
    "tidyverse",
    "tikzDevice",           # graphics output in LaTeX format
    "varhandle"
)

install.packages(packages, repos="http://cran.rstudio.com/")
