Sys.setenv(language = "russian")

Sys.setenv(PATH = paste("C:/Users/DNS/Anaconda3/Library/bin",
                        Sys.getenv()["PATH"], sep = ";"))
Sys.setenv(PATH = paste("C:/Users/DNS/Anaconda3/Scripts",
                        Sys.getenv()["PATH"], sep = ";"))
Sys.setenv(PATH = paste("C:/Users/DNS/Anaconda3/",
                        Sys.getenv()["PATH"], sep = ";"))

use_condaenv("base")
use_python("C:/Users/DNS/Anaconda3/python.exe")
pandas = reticulate::import("pandas")
stataexe = "C:/Program Files (x86)/Stata13/StataMP-64.exe"

n_chap = 16
child = tibble::tibble(r = rep(0, n_chap), 
                       py = rep(0, n_chap), 
                       stata = rep(0, n_chap))


child$py[2] = 1