rm(list = ls()); gc()

# packages
packages <- c("readxl", "tidyverse", "flextable")
sapply(packages, library, character.only = T)

# source functions
files.sources = list.files(path="R", full.names = T)
sapply(files.sources, source, encoding = "utf-8")

# data --------------------------------------------------------------------------------------------

data <- readxl::read_xlsx("data/menues.xlsx")

# get metadata: when was file last updated --------------------------------------------------------

# mtime = modified
# ctime = created
# atime = accessed
last_modified <- unlist(file.info("data/menues.xlsx")$mtime)

# make nicely looking table -----------------------------------------------------------------------

