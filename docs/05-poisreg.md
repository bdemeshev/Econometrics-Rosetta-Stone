# Модели счетных данных {#poisreg}





## R

Загрузим необходимые пакеты.


```r
library(tidyverse) # манипуляции с данными и построение графиков
library(sjPlot) # визуализация моделей
library(skimr) # симпатичное summary
library(rio) # чтение .dta файлов
library(MASS) # отрицательное биномиальное распределение
library(lmtest) # проверка гипотез
library(pscl) # zero-inflation function
library(margins) # для подсчета предельных эффектов
```


Импортируем данные.

```r
df_fish = rio::import(file = "data/fish.dta")
```

```
Error in rio::import(file = "data/fish.dta"): No such file
```
Данные содержат информацию о количестве рыбы, пойманной людьми на отдыхе. 

Camper - наличие/отсутствие палатки.
Child - количество детей, которых взяли на рыбалку.
Persons - количество людей в группе.
Count - количество пойманной рыбы


Посмотрим нам описательные статистики. 


```r
skim(df_fish)
```

```
Error in is.data.frame(data): object 'df_fish' not found
```

Переменная `camper` принимает всего два значения, поэтому превратим ее в факторную переменную.


```r
df_fish = mutate(df_fish, camper = factor(camper))
```

```
Error in mutate(df_fish, camper = factor(camper)): object 'df_fish' not found
```

Наша задача - по имеющимся данным предсказать улов. 
Для начала посмотрим на распределение объясняемой переменной `count`.


```r
ggplot(df_fish, aes(x = count)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = 'count', y = 'frequency', title = 'Distribution of count variable')
```

```
Error in ggplot(df_fish, aes(x = count)): object 'df_fish' not found
```

Предположим, что переменная имеет распределение Пуассона. Будем использовать пуассоновскую регрессию. 
\[
P(y=k)=exp(-\lambda) \lambda^k / k!
\]
где $\lambda=\exp(b_1 +b_2*x)$


```r
poisson_model = glm(count ~ child + camper +  persons, family = "poisson", data = df_fish)
```

```
Error in is.data.frame(data): object 'df_fish' not found
```

```r
summary(poisson_model)
```

```
Error in summary(poisson_model): object 'poisson_model' not found
```

Посчитаем средний предельный эффект для каждой переменной.


```r
m = margins(poisson_model)
```

```
Error in margins(poisson_model): object 'poisson_model' not found
```

```r
summary(m)
```

```
Error in summary(m): object 'm' not found
```

```r
cplot(poisson_model, x = 'persons', what = 'effect', title = 'Предельный эффект переменной camper')
```

```
Error in cplot(poisson_model, x = "persons", what = "effect", title = "Предельный эффект переменной camper"): object 'poisson_model' not found
```

```r
margins(poisson_model, at = list(child = 0:1)) 
```

```
Error in margins(poisson_model, at = list(child = 0:1)): object 'poisson_model' not found
```

```r
plot_model(poisson_model, type = 'pred')
```

```
Error in insight::model_info(model): object 'poisson_model' not found
```

```r
plot_model(poisson_model, type = "pred", terms = c("child [0, 0, 1]", "persons [1,3]"))
```

```
Error in insight::model_info(model): object 'poisson_model' not found
```

Однако, заметим, что дисперсия и среднее значение объясняемой переменной не равны, как это предполагает распределение Пуассона.


```r
df_fish %>% 
  group_by(camper) %>% 
  summarize(var = var(count), mean = mean(count))
```

```
Error in eval(lhs, parent, parent): object 'df_fish' not found
```

Оценим регрессию, предполагая отрицательное биномиальное распределение остатков. В этом случае, дисперсия распределения зависит от некоторого параметра и не равна среднему.


```r
nb1 = glm.nb(count ~ child + camper +  persons, data = df_fish)
```

```
Error in is.data.frame(data): object 'df_fish' not found
```

```r
summary(nb1)
```

```
Error in summary(nb1): object 'nb1' not found
```

Попробуем исключить из модели переменную `camper` и сравним качество двух моделей.


```r
nb2 = update(nb1, . ~ . - camper)
```

```
Error in update(nb1, . ~ . - camper): object 'nb1' not found
```

```r
waldtest(nb1, nb2)
```

```
Error in waldtest(nb1, nb2): object 'nb1' not found
```


Можем посмотреть на результаты модели с "раздутыми нулями" (zero-inflated).
Они предполагают большую частоту нулевых наблюдений.


```r
zero_infl = zeroinfl(count ~  child + camper | persons, data = df_fish, dist = 'negbin')
```

```
Error in is.data.frame(data): object 'df_fish' not found
```

```r
summary(zero_infl)
```

```
Error in summary(zero_infl): object 'zero_infl' not found
```

```r
plot_model(zero_infl, type = 'pred')
```

```
Error in insight::model_info(model): object 'zero_infl' not found
```


## Python

Загрузим ужные пакетики.


```python
import pandas as pd # работа с таблицами
import numpy as np # математика, работа с матрицами
import matplotlib.pyplot as plt # графики
import statsmodels.api as sm
import statsmodels.formula.api as smf
import statsmodels.graphics.gofplots as gf
from statsmodels.stats.outliers_influence import summary_table
import seaborn as sns # еще более классные графики
from scipy.stats import shapiro # еще математика
import statsmodels.discrete.discrete_model
from statsmodels.discrete.count_model import ZeroInflatedPoisson

plt.style.use('ggplot')
```

Загружаем данные и смотрим описательные статистики.


```python
df_fish = pd.read_stata('data/fish.dta')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): FileNotFoundError: [Errno 2] No such file or directory: 'data/fish.dta'

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

Посмотрим, как выглядит распределение объясняемой переменной. 


```python
sns.distplot(df_fish['count'])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df_fish' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.show()
```

![](05-poisreg_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

Превращаем переменную `camper` в категориальную.


```python
df_fish['camper'] = df_fish['camper'].astype('category')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df_fish' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Строим Пуассоновскую регрессию.


```python
regr_pois = smf.glm('count ~ child + camper +  persons', data=df_fish,
                    family=sm.families.Poisson()).fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df_fish' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
regr_pois.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr_pois' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Посмотрим, равны ли среднее значение и дисперсия, как это предполагает распределение Пуассона.

```python
(df_fish
 .filter(['count', 'camper'])
 .groupby('camper')
 .agg(['mean', 'var']))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df_fish' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

И регрессию с остатками, имеющими отрицательное биномиальное распределение.

```python
regr_bin = smf.glm('count ~ child + camper +  persons', data=df_fish,
              family=sm.families.NegativeBinomial()).fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df_fish' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
regr_bin.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr_bin' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
 
Проверим гипотезу о равенстве 0 коэффициента при переменной `camper`. Для этого проведем тест Вальда.


```python
hyp = '(child = 0)'
regr_bin.wald_test(hyp)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr_bin' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Видим, что p-value очень маленькое. Значит, гипотеза о незначимости коэффициента при переменной `camper` отвергается.

Посчитаем средний предельный эффект для каждой переменной.


```python
pred = regr_pois.fittedvalues
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'regr_pois' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mean_mef_child = np.mean([regr_pois.params[1]*p for p in pred])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'pred' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mean_mef_camper = np.mean([regr_pois.params[2]*p for p in pred])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'pred' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
data_1 = pd.DataFrame({'child': df_fish['child'], 'camper': 1, 'persons': df_fish['persons']})
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df_fish' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
data_0 = pd.DataFrame({'child': df_fish['child'], 'camper': 0, 'persons': df_fish['persons']})
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df_fish' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mean_mef_persons = np.mean([(regr_pois.predict(data_1)[i]-regr_pois.predict(data_0)[i]) 
                            for i in range(len(df_fish))])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df_fish' is not defined

Detailed traceback: 
  File "<string>", line 2, in <module>
```


```python
plot_model(regr_pois, type='effect', terms='camper')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'plot_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```


И модель с раздутыми нулями. (которая у меня не получилась :( )


## Stata



Загружаем данные и смотрим описательные статистики.

```stata
use data/fish.dta
summarize
```

```
r(601);

end of do-file
r(601);
```


```stata
hist count
```
![](hist_pois.png)


Строим Пуассоновскую регрессию. 
AIC, который мы видим в описательных статистиках, рассчитан по следующей формуле:
$AIC = -2log(L) + 2k$
$AIC = -2log(L) + klog(N)$

где `L` — значение функции правдоподобия модели, `k` — количество переменных, `N` — число наблюдений.


```stata
glm count camper child persons, family(poisson)
```

```
r(111);

end of do-file
r(111);
```

Можем посчитать AIC и BIC по другой формуле и получить результат, аналогичный выводу в R.
$AIC = \frac {-2log(L) + 2k}{N}$

```stata
estat ic
```

```
r(301);

end of do-file
r(301);
```

Посмотрим, равны ли среднее значение и дисперсия, как это предполагает распределение Пуассона.

```stata
tabstat count, by(camper) stat(mean, variance) nototal
```

```
r(111);

end of do-file
r(111);
```

Предположим, что остатки имеют отрицательное биномиальное распределение.

```stata
nbreg count child camper persons
```

```
r(111);

end of do-file
r(111);
```
 
Проверим гипотезу о равенстве 0 коэффицинта при переменной `camper`. Проведем тест Вальда.

```stata
quietly: nbreg count child i.camper persons 
test i.camper 
```

```
r(111);

end of do-file
r(111);
```

Посчитаем средний предельный эффект для каждой переменной.

```stata
margins, dydx(*)
marginsplot
```
![](margins_plot.png)

И модель с раздутыми нулями.

```stata
zinb count child i.camper, inflate(persons)
```

```
r(111);

end of do-file
r(111);
```
