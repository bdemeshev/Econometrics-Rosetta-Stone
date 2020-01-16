/* predestimation analysis */

cor lQ_Tobin DtE l_TA l_RD lEmp
kdensity lQ_Tobin if Year==2012
kdensity DtE if Year==2012
count if DtE==0 & Year==2012

/* OLS */
reg lQ_Tobin DtE l_TA l_RD lEmp if Year==2012
est store ols

/* OLS with sector dummy */
xi i.Sector_ID
reg lQ_Tobin DtE  l_TA l_RD lEmp _I* if Year==2012
testparm _I*
est store ols_s

/* OLS with country dummy */
xi i.Country_ID
reg lQ_Tobin DtE l_TA l_RD lEmp _I*  if Year==2012
testparm _I*
est store ols_c

tsset Comp_ID Year 
/* Regression with instrumental variables. 2SLS */
ivregress 2sls  lQ_Tobin DtE  l_RD lEmp (l_TA = ROA Delta_margin) if Year==2012, first 
est store twosls

est tab ols ols_s ols_c twosls, b(%7.4f) star

/* Test for overidentification - instruments ROA Delta_margin validity */
estat overid 

ivregress 2sls  lQ_Tobin DtE  l_RD lEmp (l_TA = L.l_TA L.lEmp) if Year==2012, first 
est store twosls1

/* Test for overidentification - instruments L.l_TA L.lEmp validity */
estat overid 

/* Test for relevance of instruments */
reg l_TA  L.l_TA L.lEmp DtE l_RD lEmp if Year==2012
test L.l_TA L.lEmp

/* Estimation with more then 1 endogenous regressors. 2SLS */

ivregress 2sls lQ_Tobin DtE  l_RD  (l_TA lEmp = L.l_TA L.lEmp L2.l_TA L2.lEmp) if Year==2012
/* Test for overidentification - instruments validity */
estat overid 
/* Test for relevance of instruments */
estat firststage, all
est store twosls2 

est tab ols ols_s twosls twosls1 twosls2, b(%7.4f) star

/* Test the exogeneity  of regressors */
hausman twosls ols 
hausman twosls1 ols 
hausman twosls2 ols 

/* More powerfull way to test exogeneity of regressors*/
ivregress 2sls lQ_Tobin DtE  l_RD  (l_TA lEmp = L.l_TA L.lEmp L2.l_TA L2.lEmp) if Year==2012
estat endogenous

est tab ols ols_s twosls twosls1 twosls2, b(%7.4f) se

/* Regression with instrumental variables. GMM */
ivregress gmm lQ_Tobin DtE  l_RD  (l_TA lEmp = L.l_TA L.lEmp L2.l_TA L2.lEmp) if Year==2012
est store gmm 
estat firststage, all 
estat overid
estat endogenous

// Testing for the presence of heteroskedasticity in IV/GMM estimation
findit ivhettest
ivregress gmm lQ_Tobin DtE  l_RD  (l_TA lEmp = L.l_TA L.lEmp L2.l_TA L2.lEmp) if Year==2012
ivhettest

****More complex way to estimate the regression with endogeneity  of regressors
findit ivreg2
// ivreg2 is meant to highlight its ability to deal with a heteroskedastic error
// process (at lag 0) and with autocorrelation of unknown form
ivreg2 lQ_Tobin DtE  l_RD lEmp (l_TA = L.l_TA L.lEmp) if Year==2012
ivreg2 lQ_Tobin DtE  l_RD lEmp (l_TA = L.l_TA L.lEmp) if Year==2012, small ffirst

// Testing for the presence of heteroskedasticity in IV/GMM estimation
ivhettest, ph nr2
ivhettest lEmp, ph nr2
ivhettest l_TA, ph nr2
ivhettest DtE, ph nr2
ivhettest l_RD, ph nr2

ivreg2 lQ_Tobin DtE  l_RD  (l_TA lEmp = L.l_TA L.lEmp L2.l_TA L2.lEmp) if Year==2012
ivhettest

// Two-step GMM efficient in the presence of arbitrary heteroskedasticity
ivreg2 lQ_Tobin DtE  l_RD lEmp (l_TA = L.l_TA L.lEmp) if Year==2012, gmm2s robust
ivreg2 lQ_Tobin DtE  l_RD  (l_TA lEmp = L.l_TA L.lEmp L2.l_TA L2.lEmp) if Year==2012, gmm2s robust

// Testing for the absence of the spesification errors in ivreg 
// with the presence of arbitrary heteroskedasticity
predict e, residual
kdensity e
sktest e
ovtest
findit ivreset
ivreset
ivreset, poly(4) rf small
