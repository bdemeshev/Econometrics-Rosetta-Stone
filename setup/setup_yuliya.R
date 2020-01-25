library(knitr) # комбинирование кода и текста
library(Statamarkdown) # взаимодействие со статой
library(reticulate) # взаимодействие с питоном

use_python("/Users/Юлия/AppData/Local/Programs/Python/Python37/python.exe")
stataexe <- find_stata()

n_chap = 16
child = tibble::tibble(r = rep(0, n_chap), 
                       py = rep(0, n_chap), 
                       stata = rep(0, n_chap))

child$r[1] = 1
child$py[1] = 1
child$stata[1] = 1


knitr::opts_chunk$set(engine.path = list(stata = stataexe), collectcode = TRUE)
