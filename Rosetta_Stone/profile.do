use us-return.dta
ren A n
ren B date
gen y_delta=MOTOR-RKFREE
drop if n==.
gen x=MARKET-RKFREE
reg y_delta x

scatter y x
