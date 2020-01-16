// предварительный анализ данных //
sum is ie kelag y
kdensity is
kdensity ie
kdensity kelag
kdensity y
sort date
gen t=_n
tsset t
line is date, name(is)
line ie date, name(ie)
line kelag date, name(kelag)
line y date, name(y)
graph combine is ie kelag y
cor ie kelag y
// оценивание модели 1 //
reg ie kelag y
findit outreg2
outreg2 using models, excel replace
outreg2 using models, word replace

// вычисление остатков и предсказанных моделью значений //
predict ie1_hat, xb
predict e1_hat, residual
// визуальное сопоставление исходных данных об инвестициях, значений, предсказанных регрессией и остатков //
twoway (line ie date) (line ie1_hat date) (line e1_hat date)

// тестирование нормальности остатков //
sktest e1_hat
swilk e1_hat
sfrancia e1_hat
kdensity e1_hat
// тестирование гетероскедастичности остатков //
estat hettest e1_hat
estat szroeter e1_hat
// тестирование ошибок спецификации //
estat ovtest
rvfplot
scatter e1_hat date

// тестирование автокорреляции //
tsset date
gen t=_n
tsset t
browse date t
scatter e1_hat L.e1_hat
estat dwatson
bgodfrey
reg e1_hat kelag y L.e1_hat L2.e1_hat L3.e1_hat  L4.e1_hat 

// тестирование H0: ro=1
dfuller e1_hat
dfuller D.e1_hat

// ARIMA
corrgram D.e1_hat
ac D.e1_hat
pac D.e1_hat
qui arima D.e1_hat, arima(1,0,0)
estat ic
qui arima D.e1_hat, arima(1,0,1)
estat ic

// оценивание модели 2 //
reg ie y L.y L.ie
outreg2 using models, excel append
outreg2 using models, word append
predict ie2_hat, xb
predict e2_hat, residual
twoway (line ie date) (line ie2_hat date) (line e2_hat date)

// тестирование нормальности остатков //
sktest e2_hat
swilk e2_hat
sfrancia e2_hat
kdensity e2_hat

// тестирование гетероскедастичности остатков //
estat hettest e2_hat
estat szroeter e2_hat

// тестирование ошибок спецификации //
estat ovtest
rvfplot
scatter e2_hat t

// тестирование автокорреляции //
scatter e2_hat L.e2_hat
estat durbinalt
ac e2_hat
pac e2_hat

// тестирование H0: ro=1
dfuller e2_hat

// ARIMA
corrgram e2_hat
arima e2_hat, arima(1,0,0)
estat ic
arima e2_hat, arima(1,0,1)
estat ic

// ARIMAX(1,0,0)
arima ie y L.ie L.y, arima(1,0,0)
predict ie2_arima, xb
predict e2_arima, residual
twoway (line ie date) (line ie2_arima date) (line e2_arima date)
arima ie y L.ie L.y, arima(1,0,0)
nlcom (mu: _b[y]/(1-_b[L.ie])) ///
(delta: 1+_b[L.y]/_b[y]) (lambda: 1-_b[L.ie]), post 
outreg2 using parameters, see word 
 
// ARIMAX(1,0,1)
arima ie y L.ie L.y, arima(1,0,1)
predict ie2_arima, xb
predict e2_arima, residual
twoway (line ie date) (line ie2_arima date) (line e2_arima date)
arima ie y L.ie L.y, arima(1,0,1)
nlcom (mu: _b[y]/(1-_b[L.ie])) ///
(delta: 1+_b[L.y]/_b[y]) (lambda: 1-_b[L.ie]), post 
outreg2 using parameters,  word append

// OLS
reg ie y L.ie L.y
predict ie2_arima, xb
predict e2_arima, residual
twoway (line ie date) (line ie2_arima date) (line e2_arima date)
reg ie y L.ie L.y
nlcom (mu: _b[y]/(1-_b[L.ie])) ///
(delta: 1+_b[L.y]/_b[y]) (lambda: 1-_b[L.ie]), post 
outreg2 using parameters,  word append


