packages = c(
    "ggplot2",
    "plyr",                 # tools for splitting, applying, and combining data
    "dplyr",
    "ProjectTemplate",      # automate creation of new projects
    "filehash",             # key-value database
    "tikzDevice",           # graphics output in LaTeX format
    "varhandle",
    "psych"                 # tools for psychological, psychometric, and personality research,
                            #   including read.clipboard.csv()
)

install.packages(packages, repos="http://cran.rstudio.com/")
