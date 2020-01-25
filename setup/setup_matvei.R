library(knitr) # комбинирование кода и текста
library(Statamarkdown) # взаимодействие со статой
library(reticulate) # взаимодействие с питоном


use_python("Users/The_sun/Anaconda3/python.exe")
stataexe = "C:/Program Files (x86)/Stata13/StataMP-64.exe"


n_chap = 16
child = tibble::tibble(r = rep(0, n_chap), 
                       py = rep(0, n_chap), 
                       stata = rep(0, n_chap))


child$r[1] = 1
child$py[1] = 1
child$stata[1] = 1


knitr::opts_chunk$set(engine.path = list(stata = stataexe), collectcode = TRUE)
=======
child$r[9] = 1
child$py[9] = 1
child$stata[9] = 1


child$r[13] = 1
child$py[13] = 1
child$stata[13] = 1
