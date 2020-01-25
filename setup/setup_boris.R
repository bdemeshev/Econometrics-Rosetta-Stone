library(knitr) # комбинирование кода и текста
library(Statamarkdown) # взаимодействие со статой
library(reticulate) # взаимодействие с питоном


stataexe <- find_stata()

n_chap = 16
child = tibble::tibble(r = rep(0, n_chap), 
                       py = rep(0, n_chap), 
                       stata = rep(0, n_chap))

child$r[1] = 1
child$py[1] = 1
child$stata[1] = 1

child$r[2] = 1
child$py[2] = 0
child$stata[2] = 0

child$r[3] = 1
child$py[3] = 0
child$stata[3] = 0

child$r[4] = 1
child$py[4] = 0
child$stata[4] = 0

child$r[5] = 1
child$py[5] = 0
child$stata[5] = 0

child$r[8] = 1
child$py[8] = 0
child$stata[8] = 0


child$r[10] = 1
child$py[10] = 0
child$stata[10] = 0


knitr::opts_chunk$set(engine.path = list(stata = stataexe), collectcode = TRUE)



