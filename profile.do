use us-return.dta
summarize
ren A n
ren B date
drop if n == .
gen y = MOTOR - RKFREE
gen x = MARKET - RKFREE
reg y x
test x = 1
predict u_hat, resid
predict y_hat
sktest u_hat
swilk u_hat
qnorm u_hat 
rvfplot, yline(0)
lvr2plot
predict D, cooksd
predict standard, rstandard

graph twoway scatter standard y_hat [aweight=D], msymbol(oh) yline(0)
set seed 7

set obs 120
gen x_new = x+ 0.5 *rnormal()
gen y_hat_new =  .8481496 * x_new+ .0052529
