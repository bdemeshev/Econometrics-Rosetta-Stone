use_python("/Users/Юлия/AppData/Local/Programs/Python/Python37/python.exe")
stataexe <- find_stata()

n_chap = 16
child = tibble::tibble(r = rep(1, n_chap), 
                       py = rep(1, n_chap), 
                       stata = rep(1, n_chap))


