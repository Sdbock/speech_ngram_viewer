

library(tidyverse)
library(tidytext)
library(ggthemes)
library(ggrepel)
library(grid)
library(arrow)
library(bslib)
library(shinybrowser)
library(markdown)


ngrams_overall <- read_feather("ngrams_overall")
ngrams_byparty <- read_feather("ngrams_byparty")
choices <- read_feather("ngrams_choices")

source("functions.R")

# creating bslib theme

my_theme <- bs_theme(bootswatch = "flatly", 
                     bg = "#fff", 
                     fg = "#2a4d69",
                     primary = "#2a4d69",
                     secondary = "#adcbe3",
                     success = "#adcbe3",
                     info = "#2a4d69",
                     spacer = "2rem")

