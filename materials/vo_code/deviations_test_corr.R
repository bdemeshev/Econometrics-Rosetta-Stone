# Импортируем библиотеки
library(tidyverse)
library(plm)
library(texreg)
library(tseries)
library(corrplot)
library(lmtest)
library(nlme)
library(car)
library(forecast) # Для построения графиков Acf, Pacf
library(gridExtra) # Для построения нескольких графиков (функция grid.arrange)

############################## Подготовка ##############################

## Импорт данных
data = read_csv("../data/final_data/data_final_clear.csv")

## Оценка модели FE с двунаправленными эффектами
fe_tw = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
         model = "within", effect = "twoways")

############################## Мультиколлинеарность ##############################

# Расчёт корреляционной матрицы
data_for_cor = select(data, -c(bank, month, srRate, infl))
data_for_cor.cor = cor(data_for_cor)
corrplot(data_for_cor.cor, method = "number", mar = c(0,0,1,0))
# Сильно коррелируют gdpGrowth и crRate, а также proft и profit2

# VIF
vif(re_t) # Все VIF меньше 10, мультиколлинеарности не обнаружено
vif(fe) # Оценить невозможно

# VIF для Pool и RE с плохими переменными
pool_bad = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage 
               + cbSupp + gdpGrowth,
           model = "pooling")
re_t_bad = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage 
               + cbSupp + gdpGrowth, 
               model = "random", effect = "twoways")
vif(pool_bad)
vif(re_t_bad)

# В соответствии с
# https://stackoverflow.com/questions/20281055/test-for-multicollinearity-in-panel-data-r
# проверим также pool-модель
pool = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
           model = "pooling")
vif(pool) # Для Profit и Profit2 VIF превышает 10, что может говорить о мультиколлинеарности
# Отметим, что коэффициенты перед crRate и cbSupp оказались незначимыми, а перед profit и
# profit2 -- значимыми. Таким образом, мультиколлинеарность мешает оценить коэффициент перед
# crRate. Посмотрим, что будет при удалении данной переменной:
re_t_no_crRate = plm(data = data, liqSh ~ profit + profit2 + size + leverage + cbSupp + gdpGrowth, 
           model = "random", effect = "twoways")
pooled_no_crRate = lm(data = data, liqSh ~ profit + profit2 + size + leverage + cbSupp + gdpGrowth)
summary(re_t_no_crRate) # Всё значимо, кроме cbSupp
summary(pooled_no_crRate) # Всё значимо, кроме cbSupp
vif(re_t_no_crRate) # Все < 10
vif(pooled_no_crRate) # При profit и profit2 > 10

# Вывод: используем модель без переменной crRate (из-за м/к с gdpGrowth и незначимого коэф.)
# М/к между profit и profit2 не мешает оценке коэффициентов, поэтому ничего не делаем

# Уберём также незначимую переменную cbSupp (при дальнейшем анализе выяснилось, что она
# мешает оценке моделей

# Модифицируем данные, чтобы случайно не ошибиться в дальнейшем
data = select(data, -c(crRate, cbSupp))

# Переобучаем модель с учётом изменений
re_t = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
           model = "random", effect = "twoways")

fe_tw = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
            model = "within", effect = "twoways")


### Для FE (two-way)
vif(fe_tw) # Не работает

profit_m = plm(data = data, profit ~ profit2 + size + leverage + gdpGrowth, 
               model = "within", effect = "twoways") # R-Squared: 0.89372
profit2_m = plm(data = data, profit2 ~ profit + size + leverage + gdpGrowth, 
               model = "within", effect = "twoways") # R-Squared: 0.85879
size_m = plm(data = data, size ~ profit + profit2 + leverage + gdpGrowth, 
                model = "within", effect = "twoways") # R-Squared: 0.53006
leverage_m = plm(data = data, leverage ~ profit + profit2 + size + gdpGrowth, 
             model = "within", effect = "twoways") # R-Squared: 0.064017
gdpGrowth_m = plm(data = data, gdpGrowth ~ profit + profit2 + size + leverage, 
                 model = "within", effect = "twoways") # NA

r_s = c(0.89372, 0.85879, 0.53006, 0.064017)
vifs = c()

for (r in r_s) {
  vif = 1 / (1 - r)
  vifs = c(vifs, vif)
}

############################## Гетероскедастичность ##############################

# Тест Бройша-Пагана (https://www.princeton.edu/~otorres/Panel101R.pdf, слайд 23)
# H0: гомоскедастичность ошибок (так как метод из crossection, всех ошибок)
bptest(formula = fe_tw, data = data)
# p-value < 2.2e-16
# H0 отвергается на любом уровне значимости => гетероскедастичность (но непонятно, по объектам
# или по времени)

# Модифицированный тест Вальда
# https://www.stata-journal.com/sjpdf.html?articlenum=st0004 (xttest 2)
within_model = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
         model = "within", effect = "twoways")
rss_within = sum(within_model$residuals^2)
n_within = nobs(within_model)
df_within = within_model$df.residual # 8280 - 690 - 6

sigma2_epsilon = rss_within / df_within

sigma2_is = c()
for (i in seq(1, 8269, 12)) {
  sigma2_i = sum(within_model$residuals[i:(i + 11)]^2)/12
  sigma2_is = c(sigma2_is, sigma2_i)
}

V_is = c()
j = 1
for (i in seq(1, 8269, 12)) {
  V_i = sum(within_model$residuals[i:(i + 11)]^2 - sigma2_is[j])^2/(11 * 12)
  V_is = c(V_is, V_i)
  j = j + 1
}

wald = 0
for (i in 1:690) {
  # Придётся пропустить те наблюдения, где V_is[i] =0 0
  if (V_is[i] == 0) {
    next
  }
  wald = wald + (sigma2_is[i] - sigma2_epsilon)^2 / V_is[i]
}

pchisq(wald, 690, lower.tail = FALSE) # p-val = 0 => groupwise h/sc (по объектам)

# LR-тест
# UR-model (assuming h/sc)
gls_ur = gls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
                    weights = varPower())
gls_r = gls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth)




############################## Автокорреляция по времени ##############################

# Тест Вулдриджа: H0: нет AR(1) по времени в ошибках 
pwartest(fe_tw) # F = 235.97, df1 = 1, df2 = 7588, p-value < 2.2e-16 => serial a/c

# Тест Балтаги-Ли: H0:sigma2_mu = 0, rho = 0 в RE
#pbltest(re_t) # chisq = 1526.7, df = 1, p-value < 2.2e-16 => sigma2mu != OR rho != 0 (serial a/c)
#pbltest(fe) # только для re

# Тест Дарбина-Уотсона H0: нет AR(1) по времени в ошибках 
#pdwtest(re_t) # DW = 1.2357, p-value < 2.2e-16 => serial a/c
pdwtest(fe_tw) # DW = 1.3383, p-value < 2.2e-16 => serial a/c

# Тест Бройша-Пагана: H0: нет AR(1) по времени в ошибках ??? (в лекциях N(0,1))
#plmtest(re_t, type = "bp") # chisq = 30499, df = 1, p-value < 2.2e-16 => serial a/c

############################## Пространственная корреляция ##############################

# LM-тест Бройша-Пагана: H0: нет пространственной а/к
#pcdtest(re_t, test = 'lm') # chisq = 436860, df = 237700, p-value < 2.2e-16 => spatial a/c
pcdtest(fe_tw, test = 'lm') # chisq = 441010, df = 237700, p-value < 2.2e-16

# LM-тест Песарана: H0: нет пространственной а/к
#pcdtest(re_t, test = 'cd') # z = 8.4153, p-value < 2.2e-16 => spatial a/c
pcdtest(fe_tw, test = 'cd') # z = 3.9218, p-value = 8.79e-05 => sp/c

# Пространственная автокорреляция - подсчет Moran's I?
# Здесь подробнее: https://rspatial.org/raster/analysis/3-spauto.html

############################## Корректировки ##############################

# В моделях обнаружились все проблемы: г/ск, а/к, пр/к

## Корректировка гетероскедастичности 

# Робастные стандартные ошибки
white = vcovHC(fe_tw, type = "HC0", cluster = "group") # В форме Уайта
cross_val = vcovHC(fe_tw, type = "HC3", cluster = "group") # Кроссвалидационные
sss = vcovHC(fe_tw, type = "sss", cluster = "group") # Как в Stata

# GLS с учётом г/ск
gls_model_gsc = gls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
                weights = varPower())
summary(gls_model_gsc)

## Корректировка автокорреляции

# GLS с учётом а/к
gls_model_ac = gls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
                correlation = corAR1())
summary(gls_model_ac)

## Корректировка пространственной корреляции

# GLS с учётом пр/к
gls_model_sc = gls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
                   correlation = corSpatial())
summary(gls_model_sc)

## Корректировка гетероскедастичности и автокорреляции

# GLS с учётом г/ск и а/к
gls_model_gscac = gls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
                weights = varPower(), correlation = corAR1())
summary(gls_model_gscac)

## Корректировка гетероскедастичности и пространственной корреляции
gls_model_gscsc = gls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
                      weights = varPower(), correlation = corLin())
summary(gls_model_gscsc)

## Корректировка гетероскедастичности, автокорреляции и пространственной корреляции

## GLS со свободными ковариационными матрицами
gls_model_gscacsc = gls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
                      weights = varComb(), correlation = corSymm())
summary(gls_model_gscacsc)

# Двухшаговый панельный GLS
pgls = pggls(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth,
      model = "within")
summary(pgls)

