Sys.setenv(RETICULATE_PYTHON = "C:\\Users\\DNS\\Anaconda3\\python.exe")

Sys.setenv(PATH = paste("C:\\Users\\DNS\\Anaconda3\\Library\\bin",
                        Sys.getenv()["PATH"], sep = ";"))
Sys.setenv(PATH = paste("C:\\Users\\DNS\\Anaconda3\\Scripts",
                        Sys.getenv()["PATH"], sep = ";"))
Sys.setenv(PATH = paste("C:\\Users\\DNS\\Anaconda3\\",
                        Sys.getenv()["PATH"], sep = ";"))


library(knitr) # комбинирование кода и текста
library(Statamarkdown) # взаимодействие со статой
library(reticulate) # взаимодействие с питоном


Sys.setenv(language = "russian")


use_condaenv("Anaconda3")
use_python("C:\\Users\\DNS\\Anaconda3\\python.exe")
pandas = reticulate::import("pandas")
stataexe = "C:/Program Files (x86)/Stata13/StataMP-64.exe"

n_chap = 16
child = tibble::tibble(r = rep(0, n_chap), 
                       py = rep(0, n_chap), 
                       stata = rep(0, n_chap))


child$py[2] = 1

# py_config()
# py_available()
# py_discover_config()
# conda_list()

knitr::opts_chunk$set(engine.path = list(stata = stataexe), collectcode = TRUE)
