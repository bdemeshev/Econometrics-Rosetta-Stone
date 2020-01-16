use_python('/Users/Sasha/anaconda3/bin/python3')
stataexe = '/Applications/Stata/StataSE.app/Contents/MacOS/stataSE'

n_chap = 16
child = tibble::tibble(r = rep(1, n_chap), 
                       python = rep(1, n_chap), 
                       stata = rep(1, n_chap))


