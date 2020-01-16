# Коан о простой линейной регрессии {#simplereg}






## R

Построим простую линейную регрессию в R и проведем несложные тесты. 

Загрузим необходимые пакеты.


```r
library(tidyverse) # манипуляции с данными и построение графиков
library(sjPlot) # красивые графики для линейных моделей
library(skimr) # симпатичное summary
library(rio) # чтение .dta файлов
library(car) # проверка линейных гипотез
library(tseries) # тест Харке-Бера
```

Импортируем данные.


```r
df = import("data/us-return.dta")
```

```
Error in import("data/us-return.dta"): No such file
```

Исследуем наш датасет.


```r
skim(df) # добавить select
```

```
Error in as.data.frame.default(data): cannot coerce class '"function"' to a data.frame
```

Переименуем столбцы.


```r
df = rename(df, n = A, date = B) 
```

```
Error in UseMethod("rename_"): no applicable method for 'rename_' applied to an object of class "function"
```

И уберем строчки, в которых хотя бы один элемент пустой.


```r
df = na.omit(df)
```

Будем верить в CAPM :) (в начале объяснить, что такое capm)
Оценим параметры модели для компании MOTOR. Тогда зависимая переменная - разница доходностей акций MOTOR и безрискового актива, а регрессор - рыночная премия.


```r
df = mutate(df, y = MOTOR - RKFREE, x = MARKET - RKFREE) # придумать говорящие названия
```

```
Error in UseMethod("mutate_"): no applicable method for 'mutate_' applied to an object of class "function"
```

Оценим нашу модель и проверим гипотезу об адекватности регрессии.


```r
ols = lm(y ~ x, data = df)
```

```
Error in as.data.frame.default(data, optional = TRUE): cannot coerce class '"function"' to a data.frame
```

```r
summary(ols)
```

```
Error in summary(ols): object 'ols' not found
```

Вызовом одной функции получаем кучу полезных графиков :) 
Можем визуально оценить наличие гетероскедастичности, нормальность распределения остатков, наличие выбросов.


```r
plot(ols) # добавить, что значит каждый график, после 
```

```
Error in plot(ols): object 'ols' not found
```

Строим 90%-й доверительный интервал для параметров модели.


```r
confint(ols, level = 0.9)
```

```
Error in confint(ols, level = 0.9): object 'ols' not found
```

Проверим гипотезу о равенстве коэффициента при регрессоре единице.


```r
linearHypothesis(ols, c("x = 1"))
```

```
Error in linearHypothesis(ols, c("x = 1")): object 'ols' not found
```

Посмотрим на остатки :) Протестируем остатки регрессии на нормальность с помощью теста Харке-Бера.

\[
H_{0}: S = 0, K = 3,
\]
где S — коэффициент асимметрии (Skewness), K — коэффициент эксцесса (Kurtosis)


```r
jarque.bera.test(resid(ols)) 
```

```
Error in resid(ols): object 'ols' not found
```

И заодно посмотрим на результаты теста Шапиро – Уилка.

\[
H_{0}: \epsilon_{i} \sim  N(\mu,\sigma^2)
\]

```r
shapiro.test(resid(ols))
```

```
Error in is.numeric(x): object 'ols' not found
```

Оба теста указывают на нормальность распределения остатков регрессии.

Сделаем прогноз модели по 20 новым наблюдениям. Будем считать, что новые наблюдения нормально распределены с мат ожиданием и дисперсией


```r
set.seed(7)

new_data = tibble(x = rnorm(10, mean = 1, sd = 3)) 
yhat = predict(ols, newdata = new_data, se = TRUE)
```

```
Error in predict(ols, newdata = new_data, se = TRUE): object 'ols' not found
```

```r
yhat
```

```
Error in eval(expr, envir, enclos): object 'yhat' not found
```


## Python

Много полезных функций для статистических расчетов можно найти в пакете Statsmodels. 


```python
import pandas as pd # работа с таблицами
import numpy as np # математика, работа с матрицами
import matplotlib.pyplot as plt # графики
import statsmodels.api as sm
import statsmodels.formula.api as smf
import statsmodels.graphics.gofplots as gf
import seaborn as sns # еще более классные графики
import statsmodels.discrete.discrete_model
from statsmodels.stats.outliers_influence import summary_table
from scipy.stats import shapiro # еще математика
```

При желании, можем настроить графики по своему вкусу :)


```python
plt.style.use('seaborn')
plt.rc('font', size=14)
plt.rc('figure', titlesize=15)
plt.rc('axes', labelsize=15)
plt.rc('axes', titlesize=15)
```

Загрузим данные.


```python
df = pd.read_stata('data/us-return.dta') # добавить говорящее имя для данных
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): FileNotFoundError: [Errno 2] No such file or directory: 'data/us-return.dta'

Detailed traceback: 
  File "<string>", line 1, in <module>
  File "C:\Users\DNS\ANACON~1\lib\site-packages\pandas\util\_decorators.py", line 208, in wrapper
    return func(*args, **kwargs)
  File "C:\Users\DNS\ANACON~1\lib\site-packages\pandas\util\_decorators.py", line 208, in wrapper
    return func(*args, **kwargs)
  File "C:\Users\DNS\ANACON~1\lib\site-packages\pandas\io\stata.py", line 227, in read_stata
    chunksize=chunksize,
  File "C:\Users\DNS\ANACON~1\lib\site-packages\pandas\util\_decorators.py", line 208, in wrapper
    return func(*args, **kwargs)
  File "C:\Users\DNS\ANACON~1\lib\site-packages\pandas\util\_decorators.py", line 208, in wrapper
    return func(*args, **kwargs)
  File "C:\Users\DNS\ANACON~1\lib\site-packages\pandas\io\stata.py", line 1093, in __init__
    self.path_or_buf = open(path_or_buf, "rb")
```

Избавимся от наблюдений с пропущенными значениями. 


```python
df.dropna(inplace=True)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
df.reset_index(drop=True, inplace=True) # пояснить, что это
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Переименуем столбцы.


```python
df = df.rename(columns={'A':'n', 'B': 'date'})
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Добавим в набор данных объясняемую переменную и регрессор.


```python
df['y'] = df['MOTOR'] - df['RKFREE']
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
df['x'] = df['MARKET'] - df['RKFREE'] 
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Оценим модель и посмотрим на саммари :)


```python
regr = smf.ols('y~x', data = df).fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
regr.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Можем посчитать прогнозное значение. 


```python
df['yhat'] = regr.fittedvalues # в R добавить для исходных, а сюда для новых
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Красивые графики для остатков, выборосов и прочих радостей, как в R, придется строить ручками. Зато приятно поиграть с оформлением :)


```python
fig, ax = plt.subplots()
ax.plot(df['x'],regr.fittedvalues, color='g', alpha=0.8)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
ax.scatter(df['x'],regr.fittedvalues + regr.resid, color = 'g', alpha = 0.8, s = 40)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
ax.vlines(df['x'],regr.fittedvalues,regr.fittedvalues+regr.resid, color='gray', alpha=0.5)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.title('Линия регрессии и остатки')
plt.xlabel('RKFREE')
plt.ylabel('MARKET')
plt.show()
```

![](02-simplereg_files/figure-latex/unnamed-chunk-15-1.pdf)<!-- --> 

Строим доверительный интервал.


```python
regr.conf_int() #добавить уровень значимости
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

И проведем F-test.


```python
hypotheses = '(x = 1)'
regr.f_test(r_matrix = hypotheses)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Тест Шапиро - Уилка. Такой же, как и в R. 

```python
W, p_value = shapiro(regr.resid)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```


Генерируем новые данные и строим предсказание.


```python
import random
random.seed(7)

newData = df['x'] + 0.5 * np.random.normal(len(df))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
prediction = regr.predict(newData)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

А теперь жесть! Построим графички, похожие на autoplot R.

```python
fig_1 = plt.figure(1)

fig_1.axes[0] = sns.residplot(df['x'], df['y'],
                                  lowess=True,
                                  scatter_kws={'alpha': 0.6},
                                  line_kws={'color': 'red', 'lw': 2, 'alpha': 0.8})
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_1.axes[0].set_title('Residuals vs Fitted')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_1.axes[0].set_xlabel('Fitted values')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_1.axes[0].set_ylabel('Residuals')


# можем добавить метки потенциальных аутлаеров
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
abs_resid = abs(regr.resid).sort_values(ascending=False)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
abs_resid_top3=abs_resid[:3]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'abs_resid' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
for i in abs_resid_top3.index:
    fig_1.axes[0].annotate(i, 
                               xy = (regr.fittedvalues[i], 
                                   regr.resid[i]))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'abs_resid_top3' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```



```python
norm_residuals = regr.get_influence().resid_studentized_internal # сохраним стьюдентизированные остатки 

```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
QQ = gf.ProbPlot(norm_residuals)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'norm_residuals' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_2 = QQ.qqplot(line='45', alpha=0.5, color='b', lw=1)

```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'QQ' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_2.axes[0].set_title('Normal Q-Q')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'fig_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_2.axes[0].set_xlabel('Theoretical Quantiles')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'fig_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_2.axes[0].set_ylabel('Standardized Residuals');

#и снова метки
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'fig_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
abs_norm_resid = np.flip(np.argsort(abs(norm_residuals)), 0)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'norm_residuals' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
abs_norm_resid_top3 = abs_norm_resid[:3]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'abs_norm_resid' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
for r, i in enumerate(abs_norm_resid_top3):
    fig_2.axes[0].annotate(i, 
                               xy=(np.flip(QQ.theoretical_quantiles, 0)[r],
                                   norm_residuals[i]))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'abs_norm_resid_top3' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```



```python
fig_3 = plt.figure(3)

plt.scatter(regr.fittedvalues, np.sqrt(abs(norm_residuals)), alpha=0.5)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
sns.regplot(regr.fittedvalues, np.sqrt(abs(norm_residuals)), 
            scatter=False, 
            ci=False, 
            lowess=True,
            line_kws={'color': 'red', 'lw': 1, 'alpha': 0.6})
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_3.axes[0].set_title('Scale-Location')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_3.axes[0].set_xlabel('Fitted values')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_3.axes[0].set_ylabel('$\sqrt{|Standardized Residuals|}$')

# и еще раз!)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
abs_sq_norm_resid = np.flip(np.argsort(np.sqrt(abs(norm_residuals)), 0))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'norm_residuals' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
abs_sq_norm_resid_top3 = abs_sq_norm_resid[:3]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'abs_sq_norm_resid' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
for i in abs_sq_norm_resid_top3:
    fig_3.axes[0].annotate(i, xy=(regr.fittedvalues[i], 
                                   np.sqrt(abs(norm_residuals)[i])))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'abs_sq_norm_resid_top3' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```


```python
leverage = regr.get_influence().hat_matrix_diag # сохраняем элементы матрицы-шляпницы
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
cook_dist = regr.get_influence().cooks_distance[0] # и расстояние Кука
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_4 = plt.figure(4)

plt.scatter(leverage, norm_residuals, alpha=0.5)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'leverage' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
sns.regplot(leverage, norm_residuals, 
            scatter=False, 
            ci=False, 
            lowess=True,
            line_kws={'color': 'red', 'lw': 1, 'alpha': 0.8})
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'leverage' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_4.axes[0].set_xlim(0, 0.20)  # РАЗВЕСТИ НА ОТДЕЛЬНЫЕ ЧАНКИ ИЛИ MESSAGE = FALSE
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_4.axes[0].set_ylim(-3, 5)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_4.axes[0].set_title('Residuals vs Leverage')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_4.axes[0].set_xlabel('Leverage')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
fig_4.axes[0].set_ylabel('Standardized Residuals')

```

```
Error in py_call_impl(callable, dots$args, dots$keywords): IndexError: list index out of range

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
leverage_top3 = np.flip(np.argsort(cook_dist), 0)[:3]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'cook_dist' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
for i in leverage_top3:
    fig_4.axes[0].annotate(i, 
                               xy=(leverage[i], 
                                   norm_residuals[i]))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'leverage_top3' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.show()
```

![](02-simplereg_files/figure-latex/unnamed-chunk-23-1.pdf)<!-- --> 


## Stata



Загружаем данные. 

```stata
use data/us-return.dta
```

```
r(601);

end of do-file
r(601);
```

Любуемся и даем новые названия столбцам.

```stata
summarize
ren A n
ren B date
```

```
no variables defined
r(111);

end of do-file
r(111);
```

Убираем пропущенные значения и создаем новые переменные.

```stata
drop if n == .
gen y = MOTOR - RKFREE
gen x = MARKET - RKFREE
```

```
r(111);

end of do-file
r(111);
```

Строим модель и проверяем гипотезу об адекватности регрессии. Тут же получаем доверительные интервалы для коэффициентов.

```stata
reg y x
```

```
r(111);

end of do-file
r(111);
```

Проверим гипотезу о равенстве коэффициента при регрессоре единице. 

```stata
test x = 1
```

```
r(301);

end of do-file
r(301);
```

Сделаем предсказание по выборке и сохраним остатки.

```stata
predict u_hat, resid
predict y_hat
```

```
r(301);

end of do-file
r(301);
```

Протестируем остатки регрессии на нормальность с помощью теста Харке-Бера.
На самом деле, это не совсем тест Харке-Бера. Оригинальный вариант ассимптотический и в нем нет поправки на размер выборки. В Stata есть. Подробнее здесь https://www.stata.com/manuals13/rsktest.pdf


```stata
sktest u_hat
```

```
r(111);

end of do-file
r(111);
```

И тест Шапиро-Уилка. Тут все аналогично R.

```stata
swilk u_hat
```

```
r(111);

end of do-file
r(111);
```

Гипотеза о нормальности остатков не отвергается.

QQ - график


```stata
qnorm u_hat 
```
![](qq_plot.png)

График предсказанных значений против остатков.

```stata
rvfplot, yline(0)
```
![](resvsfit.png)

График диагональных элементов матрицы-шляпницы против квадрата остатков (по сравнению с R оси поменялись местами).

```stata
lvr2plot
```
![](resvsh.png)

График предсказанных значений против стандартизиованных остатков. Размер точек на графике зависит от расстояния Кука для данного наблюдения.

```stata
predict D, cooksd
predict standard, rstandard

graph twoway scatter standard y_hat [aweight=D], msymbol(oh) yline(0)
```
![](standardhat.png)


```stata
set seed 7

set obs 120
gen x_new = x+ 0.5 * rnormal()
gen y_hat_new =  .8481496 * x_new + .0052529 
```

```
obs was 0, now 120

x not found
r(111);

end of do-file
r(111);
```


