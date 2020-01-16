*** Preliminary analysis ***

sum price price_rub rooms price totsp livesp kitsp dist metrdist walk brick tel bal floor new floors w s se lift window wc type metro_time zamkad
tab okrug

tab okrug walk
tab okrug brick

tab metro


/* how to generate new variable */
gen price_msq=price_m_rub/totsp

*sum price_msq rooms price totsp livesp kitsp dist metrdist walk brick tel bal floor new floors w s se lift window wc type metro_time zamkad

***************************************************************
* Graphs
hist price_rub
hist price_msq
hist price_msq, normal
sktest price_msq

/* Twoway graph and fitting of data */
* For continuous variables (Scatterplot)
twoway (scatter price_msq dist)
twoway (scatter price_msq dist) (lfit price_msq dist)
twoway (scatter price_msq livesp) (lfit price_msq livesp)
twoway (scatter price_msq metrdist) (lfit price_msq metrdist)
twoway (scatter price_msq metro_time) (lfit price_msq metro_time)

* For discrete variables (Box-plot, Bar chart)
graph bar price_msq, over(walk)
graph box price_msq, over(walk)

graph bar price_msq, over(tel)
graph box price_msq, over(tel)

graph bar price_msq, over(zamkad)
graph box price_msq, over(zamkad)

graph bar price_msq, over(floor)
graph box price_msq, over(floor)

graph bar price_msq, over(okr)

gen okrug="SE" if se==1
replace okrug="S" if s==1
replace okrug="W" if w==1
graph bar price_msq, over(okrug)
graph box price_msq, over(okrug)

graph bar price_msq, over(okrug) by(walk)
graph bar price_msq, over(okrug) by(lift)

graph bar price_msq, over(window) by(okrug)

graph bar price_msq, over(metro, sort(price_msq) label(angle(vertical)))

graph bar price_msq if okrug=="SE", over(metro, sort(price_msq) label(angle(vertical))) 
graph bar price_msq if okrug=="S", over(metro, sort(price_msq) label(angle(vertical))) 
graph bar price_msq if okrug=="W", over(metro, sort(price_msq) label(angle(vertical))) 

* Scatter plots for subsamples (groups)
twoway (scatter price_msq dist if walk==1, color(red)) (scatter price_msq dist if walk==0, color(blue))
twoway (scatter price_msq metrdist if walk==1, color(red)) (scatter price_msq metrdist if walk==0, color(blue))
twoway (scatter price_msq metro_time if walk==1, color(red)) (scatter price_msq metro_time if walk==0, color(blue))
twoway (scatter price_msq livesp if walk==1, color(red)) (scatter price_msq livesp if walk==0, color(blue))

twoway (scatter price_msq metro_time if zamkad==0, color(red)) (scatter price_msq metro_time if zamkad==1, color(blue))
twoway (scatter price_msq metrdist if zamkad==0, color(red)) (scatter price_msq metrdist if zamkad==1, color(blue))
count if zamkad==1
browse if zamkad==1


/* correalation */
pwcorr price_msq livesp kitsp dist metrdist, star(0.05)
*spearman price_msq livesp kitsp dist metrdist walk brick floor code, star(0.05)

corr price_msq livesp kitsp dist metrdist, cov

tab okrug
tabulate okrug walk, chi2

* t-test for mean comparison
ttest price_msq, by(floor) unequal
    *** "unequal" specifies that the unpaired data not be assumed to have equal variances.
ttest price_msq, by(walk) unequal
ttest price_msq, by(tel) unequal
ttest price_msq, by(zamkad) unequal

*****************************************************************************
*** OLS ***

/* Goodness of fit, comparison of models */
reg price_msq dist
est store reg0 // store the results              
reg price_msq dist kitsp
est store reg1
reg price_msq dist kitsp livesp
est store reg2
reg price_msq dist kitsp livesp metrdist
est store reg3

est tab reg0 reg1 reg2 reg3 , stats(N r2 r2_a) star(0.1 0.05 0.01) // table of results

reg price_msq dist kitsp livesp metrdist
    *** linear prediction
predict y_hat, xb     
    *** residuals
predict e, residual

graph bar e, over(okrug) 

***********************************************************************

/* tests of linear restrictions*/
********beta2=0**********************
********beta3=0**********************
reg price_msq dist kitsp livesp metrdist

********beta2=0 and beta3=0**********
quietly reg price_msq dist metrdist
scalar rss_rm=e(rss)
scalar df_rm=e(df_r)
est store reg0
quietly reg price_msq dist kitsp livesp metrdist
scalar rss_urm=e(rss)
scalar df_urm=e(df_r)
est store reg1
est tab reg0 reg1, stats( N rss df_r) star(0.1 0.05 0.01) 
scalar list rss_rm df_rm rss_urm df_urm
    *** Make calculation by the formula
    *** Compare with this
quietly reg price_msq dist kitsp livesp metrdist
test kitsp livesp

/* test of joint significance: beta_i=0 for all i */
reg price_msq dist kitsp livesp metrdist
  scalar fjs=(e(mss)/e(df_m))/(e(rss)/e(df_r))
  scalar pval_js=Ftail(e(df_m), e(df_r), fjs)

 scalar list fjs
 scalar list pval_js

*** or
test dist kitsp livesp metrdist
reg price_msq dist kitsp livesp metrdist

******** H/W - calculate the staticatic and check the answer 
test kitsp=livesp 

**************************************************************************
/* DUMMY variables */
graph bar e, over(walk) 

/* effect of walk - shift*/
reg price_msq dist kitsp livesp metrdist walk 

/* effect of walk - shift and slope*/
gen walk_dist = walk*dist
gen walk_kitsp = walk*kitsp
gen walk_livesp = walk*lives
gen walk_metrdist = walk*metrdist

*** How different the impact of one extra minute for metrdist is between walk and transport
quietly reg price_msq dist kitsp livesp metrdist walk walk_metrdist
est store reg_walk_metr
est tab reg1 reg_walk_metr, stats( N rss r2) star(.01, .05, .1)
test walk walk_metrdist


*** Do we need different models for flats with walk and transport availability to metro? 
quietly reg price_msq dist kitsp livesp metrdist walk walk_dist walk_kitsp walk_livesp walk_metrdist
est store reg_walk
est tab reg1 reg_walk, stats( N rss r2) star(.01, .05, .1)

*** or
quietly reg price_msq dist kitsp livesp metrdist walk walk_dist walk_kitsp walk_livesp walk_metrdist
test walk walk_dist walk_kitsp walk_livesp walk_metrdist

/* chow test of walk*/
quietly reg price_msq dist kitsp livesp metrdist
est store reg1
  scalar rss_rm=e(rss)
  scalar df_rm=e(df_r)

quietly reg price_msq dist kitsp livesp metrdist if walk==1
est store reg_walk1
  scalar rss_walk1=e(rss)
  scalar df_walk1=e(df_r)

quietly reg price_msq dist kitsp livesp metrdist if walk!=1
est store reg_walk0
  scalar rss_walk0=e(rss)
  scalar df_walk0=e(df_r)

est tab reg1 reg_walk1 reg_walk0, stats( N rss r2) star(.01, .05, .1)

  scalar fchow_walk=((rss_rm-(rss_walk1+rss_walk0))/(df_rm-(df_walk1+df_walk0)))/((rss_walk1+rss_walk0)/(df_walk1+df_walk0))
  scalar pval_chow_walk=Ftail(df_rm-(df_walk1+df_walk0), df_walk1+df_walk0, fchow_walk)

 scalar list rss_walk1 rss_walk0 rss_rm
 scalar list df_walk1 df_walk0 df_rm
 scalar list fchow_walk
 scalar list pval_chow_walk

* More than 2 categories 
graph bar e, over(okrug) 

encode okrug, gen(okr)
reg price_msq dist kitsp livesp metrdist walk i.okr
/* chow test of okrug */
quietly reg price_msq dist kitsp livesp metrdist walk
est store all
quietly reg price_msq dist kitsp livesp metrdist walk if okrug=="SE"
est store se
quietly reg price_msq dist kitsp livesp metrdist walk if okrug=="S" 
est store s
quietly reg price_msq dist kitsp livesp metrdist walk if okrug=="W"
est store w

est tab all se s w, stats( N rss r2) star(.01, .05, .1)

* with crossterms
reg price_msq dist kitsp livesp metrdist bal tel floor lift  i.okr i.walk#i.zamkad  
reg price_msq dist kitsp livesp metrdist bal tel i.floor#i.lift  i.okr i.walk#i.zamkad  

reg price_msq dist kitsp livesp metrdist bal tel i.floor1#i.lift i.floor2#i.lift i.okr i.walk#i.zamkad  

