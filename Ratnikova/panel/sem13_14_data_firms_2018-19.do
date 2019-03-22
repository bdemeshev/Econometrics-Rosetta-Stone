*** Firms_panel_2007_2012.dta ***

/* declaration of panel */
xtset Comp_ID Year

********************************************************************
************* predestimation analysis ******************************
********************************************************************

xtdes
*drop if Comp_ID<10 & Year==2008
xtsum
xtline lQ_Tobin if Comp_ID<31, overlay i(Comp_ID) t(Year) legend(off)
xtline lQ_Tobin if Comp_ID<31, i(Comp_ID) t(Year)

global LIST "lQ_Tobin DtE  l_TA l_RD lEmp"

/* усреднение по времени для каждого индивида */
/*	egen miX=mean(X), by(Comp_ID) */
/* вычисление отклонений от средних */
/*	gen diX=X-miX */
foreach var of global LIST {
egen mi`var'=mean(`var'), by(Comp_ID)
gen di`var'=`var'- mi`var'
}

/* усреднение по индивидам в каждой волне */
/* egen mtX=mean(X), by(Year) */
/* вычисление отклонений от средних */
/* gen dtX=X-mtX */
foreach var of global LIST {
egen mt`var'=mean(`var'), by(Year)
gen dt`var'=`var'- mt`var'
}

twoway scatter lQ_Tobin Year, msymbol(circle_hollow) ///
       || connected mtlQ_Tobin Year, msymbol(diamond) || , xlabel(2007(1)2012)

twoway scatter lQ_Tobin      DtE, mlabel (Comp_ID) || lfit lQ_Tobin  DtE, clstyle (p2)
twoway scatter dilQ_Tobin  diDtE, mlabel (Comp_ID) || lfit dilQ_Tobin  diDtE, clstyle (p2)
twoway scatter dtlQ_Tobin  dtDtE, mlabel (Comp_ID) || lfit dtlQ_Tobin  dtDtE, clstyle (p2)

twoway scatter lQ_Tobin     l_TA, mlabel (Comp_ID) || lfit lQ_Tobin l_TA, clstyle (p2)
twoway scatter dilQ_Tobin dil_TA, mlabel (Comp_ID) || lfit dilQ_Tobin dil_TA, clstyle (p2)
twoway scatter dtlQ_Tobin dtl_TA, mlabel (Comp_ID) || lfit dtlQ_Tobin dtl_TA, clstyle (p2)

twoway scatter lQ_Tobin     l_RD, mlabel (Comp_ID) || lfit lQ_Tobin l_RD, clstyle (p2)
twoway scatter dilQ_Tobin dil_RD, mlabel (Comp_ID) || lfit dilQ_Tobin dil_RD, clstyle (p2)
twoway scatter dtlQ_Tobin dtl_RD, mlabel (Comp_ID) || lfit dtlQ_Tobin dtl_RD, clstyle (p2)

twoway scatter lQ_Tobin     lEmp, mlabel (Comp_ID) || lfit lQ_Tobin lEmp, clstyle (p2)
twoway scatter dilQ_Tobin dilEmp, mlabel (Comp_ID) || lfit dilQ_Tobin dilEmp, clstyle (p2)
twoway scatter dtlQ_Tobin dtlEmp, mlabel (Comp_ID) || lfit dtlQ_Tobin dtlEmp, clstyle (p2)

****************************************************************************************
*********            Poolability to panel by Year test        **************************
****************************************************************************************

/* оценивание модели (0) без ограничений */
scalar rss0=0
scalar totn=0
forvalues i=2007/2012 {
qui reg dtlQ_Tobin dtl_TA dtlEmp dtl_RD dtDtE if Year==`i'
scalar z`i'=e(rss)
scalar x`i'=e(N)
scalar rss0=rss0+z`i'
scalar totn=totn+x`i'
}
 scalar list rss0 totn
 
/* оценивание модели с ограничением (1) FE*/
reg dtlQ_Tobin dtl_TA dtlEmp dtl_RD dtDtE
scalar rss1= e(rss)

/* оценивание модели с ограничением (2) Pool*/
reg  lQ_Tobin l_TA lEmp l_RD DtE
scalar rss2 = e(rss)

/* вычисление тестовых статистик и их p-values */
scalar ddf = totn-5*6
/* comparison (0) & (1) */
scalar fh1=((rss1-rss0)/(5*6-(6+4)))/(rss0/ddf)
scalar pval1 = Ftail(5*6-(6+4),ddf,fh1)
/* comparison (0) & (2) */
scalar fh2 =((rss2-rss0)/(5*6-(1+4)))/(rss0/ddf)
scalar pval2 = Ftail(5*6-(1+4),ddf,fh2)
/* comparison (1) & (2) */
scalar fh3 =((rss2-rss1)/((6+4)-(1+4)))/(rss1/(totn-(6+4)))
scalar pval3 = Ftail((6+4)-(1+4),totn-(6+4),fh3)
/* просмотр результатов */
scalar list pval1 pval2 pval3 ddf fh1 fh2 fh3


**********************************************************************************
*********        Poolability to panel by Comp_ID test        *********************
**********************************************************************************

reg  lQ_Tobin l_TA lEmp l_RD DtE
est store ols0
xtreg  lQ_Tobin l_TA lEmp l_RD DtE, fe
est store fe0
xtreg  lQ_Tobin l_TA lEmp l_RD DtE, re
est store re0
xttest0
hausman fe0 re0
est tab ols0 fe0 re0, b(%7.4f) star

*********************************************************************************
*********         Multicollinearity test      ***********************************
*********************************************************************************

xtreg  lQ_Tobin l_TA lEmp l_RD DtE, fe
vif, uncentered

cor dilQ_Tobin diDtE dil_TA dil_RD dilEmp

xtreg  lQ_Tobin l_TA lEmp l_RD DtE, fe
predict e_fe, u
xtreg e_fe l_TA lEmp l_RD DtE, be

**********************************************************************************
*********        Poolability to panel by Comp_ID test for new model     **********
**********************************************************************************

reg  lQ_Tobin  lEmp l_RD DtE
est store ols1
xtreg  lQ_Tobin  lEmp l_RD DtE, fe
est store fe1
xtreg  lQ_Tobin  lEmp l_RD DtE, re
est store re1
xttest0
hausman fe1 re1
est tab ols1 fe1 re1, b(%7.4f) star

xtreg  lQ_Tobin  lEmp l_RD DtE, fe
vif, uncentered

******************************************************************************************
*********   Poolability to panel by Comp_ID test for model with fixed time effect  *******
******************************************************************************************

reg  lQ_Tobin  lEmp l_RD DtE i.Year
est store ols2
xtreg  lQ_Tobin  lEmp l_RD DtE i.Year, fe
est store fe2
xtreg  lQ_Tobin  lEmp l_RD DtE i.Year, re
est store re2
xttest0
hausman fe2 re2
est tab ols1 fe1 re1 ols2 fe2 re2, b(%6.3f) star

xtreg  lQ_Tobin  lEmp l_RD DtE i.Year, fe
vif, uncentered
