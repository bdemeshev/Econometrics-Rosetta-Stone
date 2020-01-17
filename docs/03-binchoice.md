# Модель бинарного выбора {#binchoice}




## R

Загрузим необходимы пакеты.


```r
library(rio) # импорт и экспорт данных в разных форматах
library(tidyverse) # графики и манипуляции с данными
library(skimr) # описательные статистики
library(mfx) # нахождение предельных эффектов
library(margins) # визуализация предельных эффектов
library(lmtest) # проведение тестов
library(plotROC) # построение ROC-кривой
library(caret) # confusion-матрица
library(texreg) # вывод результатов регрессии в тех и html
```

Импортируем исследуемые данные.


```r
data = import("data/bwght.dta") 
```

```
Error in import("data/bwght.dta"): No such file
```

Рассмотрим описательные статистики по всем переменным: количество выкуриваемых сигарет, семейный доход, налог на сигареты, цена сигарет, образование отца и матери, паритет, цвет кожи.


```r
skim(data)
```

Заметим существование пропущенных переменных у `fatheduc`, `motheduc`. 
Будем анализировать только те значения, у которых нет пропущенных наблюдений.
Для этого создадим новый dataframe, `data_2`, в котором отсутствуют пропущенные значения. 
Посмотрим на его описательные статистики.


```r
data_2 = filter(data, !is.na(fatheduc), !is.na(motheduc))
```

```
Error in UseMethod("filter_"): нет подходящего метода для 'filter_' применяемого к объекту класса "function"
```

```r
skim(data_2)
```

```
Error in skim(data_2): объект 'data_2' не найден
```

Сгенерируем переменную `smoke`, отражающую состояние отдельного индивида: `smoke = 1`, если индивид курит (то есть количество выкуриваемых им сигарет положительно), `smoke = 0` – если индивид не курит.


```r
data = mutate(data, smoke = (cigs > 0))
```

```
Error in UseMethod("mutate_"): нет подходящего метода для 'mutate_' применяемого к объекту класса "function"
```

Построим модель линейной вероятности. Сохраним результат под `lin_prob_model`. 


```r
lin_prob_model = lm(smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity + white, data = data_2)
```

```
Error in is.data.frame(data): объект 'data_2' не найден
```

```r
summary(lin_prob_model)
```

```
Error in summary(lin_prob_model): объект 'lin_prob_model' не найден
```

Посмотрим на число совпадений прогнозных и исходных значений. 
Для этого оценим предсказанные значения модели линейной вероятности. Сохраним их как переменную `predictions_lin_prob_model`. 
Если прогнозное значение выше 0.5 (порог отсечения, по дефолту, равный 0.5), то классифицируем наблюдаемого индивида как курильщика (`smoke_ols = 1`). 
Посмотрим на число совпадений исходных и прогнозных данных.


```r
predictions_lin_prob_model = predict(lin_prob_model)
```

```
Error in predict(lin_prob_model): объект 'lin_prob_model' не найден
```

Генерируем `smoke_ols` как 1, если вероятность по модели больше 0.5 и 0, если она меньше 0.5.


```r
smoke_ols = 1 * (predictions_lin_prob_model > 0.5)
```

```
Error in eval(expr, envir, enclos): объект 'predictions_lin_prob_model' не найден
```

Число совпадений данных и прогноза модели линейной вероятности:


```r
sum (smoke_ols == data_2$smoke)
```

```
Error in eval(expr, envir, enclos): объект 'smoke_ols' не найден
```

Известно, что модель линейной вероятности обладает значительными недостатками, поэтому оценим `P(smoke=1|x)`, и построим логит– и пробит– модели.

Построим логит-модель и сохраним результат оцененной модели как `logit_model`.


```r
logit_model = glm(smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity + white, x = TRUE, data = data_2, family = binomial(link = "logit"))
```

```
Error in is.data.frame(data): объект 'data_2' не найден
```

```r
summary(logit_model)
```

```
Error in summary(logit_model): объект 'logit_model' не найден
```

Так как коэффициенты логит- и пробит- моделей плохо интерпретируются, поскольку единицы измерения латентной переменной определить сложно, посчитаем предельные эффекты, то есть изменение вероятности решения курить с изменением фактора на 1 единицу. 

Для предельного эффекта в средних значениях факторов:


```r
logitmfx(smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity + white, data = data_2, atmean = TRUE)
```

```
Error in is.data.frame(data): объект 'data_2' не найден
```

```r
margins = margins(logit_model)
```

```
Error in margins(logit_model): объект 'logit_model' не найден
```

```r
plot(margins)
```

```
Error in model[["call"]]: подгруппа выходит за пределы
```

Интерпретация предельных эффектов (на примере переменной семейного дохода): при увеличении семейного дохода в среднем на 1 единицу при остальных неизменных факторах, вероятность стать курильщиком уменьшается в среднем на 0.18%. 

Визуализируем предельный эффект для семейного дохода:


```r
cplot(logit_model, "faminc", what = "effect", main = "Average Marginal Effect of Faminc")
```

```
Error in cplot(logit_model, "faminc", what = "effect", main = "Average Marginal Effect of Faminc"): объект 'logit_model' не найден
```

Для определения качества модели построим классификационную матрицу. 
Для этого сначала вычислим предсказания логит-модели, `predictions_logit_model`. Так как результат не бинарный, то введём порог отсечения, равный 0.5.
Назовём бинарный результат `smoke_logit`:


```r
predictions_logit_model = predict(logit_model)
```

```
Error in predict(logit_model): объект 'logit_model' не найден
```

```r
smoke_logit_model = (predictions_logit_model>0.5)
```

```
Error in eval(expr, envir, enclos): объект 'predictions_logit_model' не найден
```

Построим классификационную матрицу. 
При возникновении ошибок аргументов, в частности, при несовпадении их размера или типа, можно воспользоваться функцией `as.factor()`.


```r
confusionMatrix(as.factor(smoke_logit_model), as.factor(data_2$smoke))
```

```
Error in is.factor(x): объект 'smoke_logit_model' не найден
```

Качество модели также можно проанализировать с помощью ROC-кривой, отражающей зависимость доли верных положительно классифицируемых наблюдений (`sensitivity`) от доли ложных положительно классифицируемых наблюдений `(1-specifity)`. 

Построим ROC-кривую для логит-модели:


```r
basicplot = ggplot(data_2, aes(m = predictions_logit_model, d = data_2$smoke)) + geom_roc()
```

```
Error in ggplot(data_2, aes(m = predictions_logit_model, d = data_2$smoke)): объект 'data_2' не найден
```

```r
basicplot + annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(basicplot)$AUC, 2)))
```

```
Error in eval(expr, envir, enclos): объект 'basicplot' не найден
```

Площадь под кривой обозначается как AUC. 
Она показывает качество классификации. Соответственно, чем выше AUC, тем лучше построенная модель.

Сейчас проанализируем правильность спецификации логит-модели. Может быть, лучше будет убрать какую-нибудь переменную?
Рассмотрим логит-модель, не учитывающую переменную `white`. 
Сохраним эту модель под названием `logit_model_new`. 


```r
logit_model_new = glm(smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity, x = TRUE, data = data_2, family = binomial(link = "logit"))
```

```
Error in is.data.frame(data): объект 'data_2' не найден
```

Сравним модели `logit_model` и `logit_model_new` с помощью теста максимального правдоподобия (likelihood ratio test).


```r
lrtest(logit_model,logit_model_new)
```

```
Error in lrtest(logit_model, logit_model_new): объект 'logit_model' не найден
```

`p-value = 0.08` в LR-тесте. 
Следовательно, основная гипотеза о том, что переменная `white` не влияет на решение стать курильщиком, не отвергается на 5% уровне значимости.

Сейчас посмотрим на пробит-модель. Скрытая переменная в этой модели распределена стандартно нормально: 
\[
f(t) = \frac{1 \cdot e^{\frac{-t^2}{2}}}{\sqrt{2 \cdot \pi}}
\]

Построим пробит-модель.


```r
probit_model = glm(smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity + white, data = data_2, family = binomial(link = "probit"))
```

```
Error in is.data.frame(data): объект 'data_2' не найден
```

```r
summary(probit_model)
```

```
Error in summary(probit_model): объект 'probit_model' не найден
```

Вычисление предельных эффектов и их интерпретация, построение классификационной матрицы и ROC-кривой и LR-тест проводятся аналогично выполненным в логит-модели.

Выведем сравнительную таблицу для построенных моделей.


```r
screenreg(list(lin_prob_model, logit_model, probit_model), 
             custom.model.names = c("Модель линейной   вероятности", "Логит-модель", "Пробит-модель"))
```

```
Error in "list" %in% class(l)[1]: объект 'lin_prob_model' не найден
```


## Python

Попробуем повторить эти шаги, используя **python**.



Импортируем пакеты:


```python
import numpy as np
import pandas as pd # чтение файлов
import matplotlib.pyplot as plt # построение графиков
from statsmodels.formula.api import logit, probit, ols # построение логит-, пробит- и линейной регрессий
import statistics # описательные статистики
import sklearn
from sklearn import metrics # для работы с классификационными матрицами
from sklearn.metrics import roc_curve, auc  # ROC-curve и AUC
from scipy.stats.distributions import chi2 # хи-квадрат-статистика
```

Загрузим данные:


```python
data = pd.read_stata("data/bwght.dta")
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): FileNotFoundError: [Errno 2] No such file or directory: 'data/bwght.dta'

Detailed traceback: 
  File "<string>", line 1, in <module>
  File "C:\Users\Yuliya\AppData\Local\Programs\Python\Python37\lib\site-packages\pandas\util\_decorators.py", line 188, in wrapper
    return func(*args, **kwargs)
  File "C:\Users\Yuliya\AppData\Local\Programs\Python\Python37\lib\site-packages\pandas\util\_decorators.py", line 188, in wrapper
    return func(*args, **kwargs)
  File "C:\Users\Yuliya\AppData\Local\Programs\Python\Python37\lib\site-packages\pandas\io\stata.py", line 186, in read_stata
    chunksize=chunksize)
  File "C:\Users\Yuliya\AppData\Local\Programs\Python\Python37\lib\site-packages\pandas\util\_decorators.py", line 188, in wrapper
    return func(*args, **kwargs)
  File "C:\Users\Yuliya\AppData\Local\Programs\Python\Python37\lib\site-packages\pandas\util\_decorators.py", line 188, in wrapper
    return func(*args, **kwargs)
  File "C:\Users\Yuliya\AppData\Local\Programs\Python\Python37\lib\site-packages\pandas\io\stata.py", line 994, in __init__
    self.path_or_buf = open(path_or_buf, 'rb')
```

Рассмотрим описательные статистики по всем переменным: количество выкуриваемых сигарет, семейный доход, налог на сигареты, цена сигарет, образование отца и матери, паритет, цвет кожи.


```python
data_2.describe()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
У переменных `fatheduc` и `motheduc` имеются пропущенные значения.

Уберём пропущенные данные и назовём новый датафрейм `data_2`. Выведем описательные статистики по новым данным.


```python
data_2 = data.dropna()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
data_2.describe()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Создадим бинарную переменную `smoke`, которая показывает, курит ли индивид: `smoke = 1`, если индивид является курильщиком (то есть количество выкуриваемых сигарет положительно), `smoke = 0` – иначе.


```python
data_2["smoke"] = 1 * (data_2["cigs"]>0)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Построим модель линейной вероятности. Сохраним результат под `lin_prob_model`.


```python
lin_prob_model = ols("smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity + white", data_2).fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
lin_prob_model.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'lin_prob_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Создадим переменную `predictions_lin_prob_model`, равную прогнозным значениям модели линейной вероятности. 
Если прогнозное значение выше 0.5 (порог отсечения, по дефолту, равный 0.5), то классифицируем наблюдаемого индивида как курильщика (`smoke_ols = 1`). 
Посмотрим на число совпадений исходных (`data_2["smoke"]`) и прогнозных (`data_2["smoke_ols"]`) данных.


```python
predictions_lin_prob_model = lin_prob_model.predict(data_2)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'lin_prob_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
data_2["smoke_ols"] = 1 * (predictions_lin_prob_model>0.5)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'predictions_lin_prob_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
sum(data_2["smoke"] == data_2["smoke_ols"])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Ввиду недостатков линейной вероятностной модели, оценим логит- и пробит- модели.

Построим логит-модель:


```python
logit_model = logit("smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity + white", data_2).fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
logit_model.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'logit_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Так как коэффициенты логит- и пробит- моделей плохо интерпретируются, поскольку единицы измерения латентной переменной определить сложно, посчитаем предельные эффекты, то есть изменение вероятности решения курить с изменением фактора на 1 единицу. 

Посчитаем предельные эффекты в средних значениях переменных для логистической регрессии.


```python
me_mean = logit_model.get_margeff(at="mean")
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'logit_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
me_mean.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'me_mean' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Интерпретация предельных эффектов (на примере переменной семейного дохода): при увеличении семейного дохода в среднем на 1 единицу при остальных неизменных факторах, вероятность стать курильщиком уменьшается в среднем на 0.18%.

Посмотрим на точность классификации построенной логит-модели. Для этого вычислим прогнозные значения модели.


```python
predictions_logit_pred = logit_model.predict(data_2) # прогнозирование значений
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'logit_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
data_2["smoke_logit_model"] = 1 * (predictions_logit_pred>0.5)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'predictions_logit_pred' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Построим классификационную матрицу.


```python
sklearn.metrics.confusion_matrix(data_2["smoke"], data_2["smoke_logit_model"])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Точность прогноза и классификации данных.


```python
np.round(sklearn.metrics.accuracy_score(data_2["smoke"],data_2["smoke_logit_model"]), 2)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
sklearn.metrics.classification_report(data_2["smoke"], data_2["smoke_logit_model"])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Качество модели также можно проанализировать с помощью ROC-кривой, отражающей зависимость доли верных положительно классифицируемых наблюдений (`sensitivity`) от доли ложных положительно классифицируемых наблюдений `(1-specifity)`. Площадь под кривой обозначается как AUC; эта метрика показывает качество классификации. Соответственно, чем выше AUC, тем лучше построенная модель.

Построим ROC-кривую для логит-модели.


```python
fpr, tpr, thresholds = metrics.roc_curve(data_2["smoke"], predictions_logit_pred)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
auc = metrics.roc_auc_score(data_2["smoke"], predictions_logit_pred)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.plot(fpr,tpr,label="auc="+str(np.round(auc, 2)))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'fpr' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.legend(loc=4)
plt.xlabel("1-Specifity")
plt.ylabel("Sensitivity")
plt.title("ROC-curve")
plt.show()
```

<img src="03-binchoice_files/figure-html/unnamed-chunk-37-1.png" width="672" />

Теперь проанализируем правильность спецификации логит-модели. Посмотрим, станет ли модель лучше, если уберём какую-нибудь переменную.

Построим новую логит-модель (`logit_model_new`) без учёта переменной `white`.


```python
logit_model_new = logit("smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity ", data_2).fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
logit_model_new.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'logit_model_new' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Теперь сравним построенные модели (`logit_model` и `logit_model_new`) с помощью теста отношения правдоподобия (LR-тест).

Так как на момент написания коана готовой реализации функции теста отношения правдоподобия нет, то сделаем его ручками.


```python
L1 = logit_model.llf
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'logit_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
L2 = logit_model_new.llf
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'logit_model_new' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
def likelihood_ratio(llmin, llmax):
	return(2 * (max(llmax, llmin) - min(llmax, llmin)))
LR = likelihood_ratio (L1, L2)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'L1' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
np.round(chi2.sf(LR, 1), 2) # расчёт p-value для теста
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'LR' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Основная гипотеза о незначимости фактора `white` не отвергается на 5% уровне значимости, так как `p-value = 0.08` в LR-тесте. 

Теперь научимся строить пробит-модель.


```python
probit_model = probit("smoke ~ 1 + faminc + cigtax + cigprice + fatheduc + motheduc + parity + white", data_2).fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_2' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
probit_model.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'probit_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Расчёт предельных эффектов, точности классификации, визуализация ROC-кривой и проведение LR-теста проводятся аналогично операциям с логит-моделью.

Выведем сравнительную таблицу для построенных моделей.


```python
pd.DataFrame(dict(col1=lin_prob_model.params, col2=logit_model.params, col3=probit_model.params))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'lin_prob_model' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```


## Stata

Познакомимся с тем, как **stata** работает с моделями бинарного выбора. 



Импортируем данные. 


```stata
use 03_bwght.dta
```

```
file 03_bwght.dta not found
r(601);

end of do-file
r(601);
```



Рассмотрим описательные статистики данных.


```stata
sum smoke faminc cigtax cigprice fatheduc motheduc parity white
```

```
no variables defined
r(111);

end of do-file
r(111);
```

Заметим, что переменные `fatheduc`, `motheduc` имеют пропущенные значения.
Уберём их и рассмотрим описательные статистики нового датасета без пропущенных наблюдений.


```stata
sum smoke faminc cigtax cigprice fatheduc motheduc parity white if fatheduc != . & motheduc != .
```

```
no variables defined
r(111);

end of do-file
r(111);
```

Сгенерируем переменную `smoke`, которая будет зависимой переменной в наших моделях. Присвоим ей значение, равное 1, если индивид выкуривает положительное количество сигарет, и 0, иначе.


```stata
gen smoke = (cigs>0) if cigs != .
```

```
cigs not found
r(111);

end of do-file
r(111);
```

Построим модель линейной вероятности. Сохраним результат под `lin_prob_model`.


```stata
reg smoke faminc cigtax cigprice fatheduc motheduc parity white if fatheduc != . & motheduc != .
est store lin_prob_model
```

```
fatheduc not found
r(111);

end of do-file
r(111);
```

Посчитаем количество совпадений прогнозов и исходных значений. Для этого оценим предсказанные значения модели линейной вероятности. Сохраним их как `predictions_lin_prob_model`. 
Если прогнозное значение выше 0.5 (порог отсечения, по дефолту, равный 0.5), то классифицируем наблюдаемого индивида как курильщика (`smoke_ols = 1`). 
Посмотрим на число совпадений исходных и прогнозных данных.


```stata
predict predictions_lin_prob_model
gen smoke_ols = (predictions_lin_prob_model>0.5) if predictions_lin_prob_model != .
count if smoke_ols == smoke
tab smoke_ols smoke
```

```
last estimates not found
r(301);

end of do-file
r(301);
```

Ввиду указанных выше недостатков линейной вероятностной модели, есть необходимость оценки логит- и пробит- моделей.

Построим логит-модель и сохраним результат оцененной модели как `logit_model`.


```stata
logit smoke faminc cigtax cigprice fatheduc motheduc parity white if fatheduc != . & motheduc != .
est store logit_model
```

```
fatheduc not found
r(111);

end of do-file
r(111);
```

Рассчитаем предельные эффекты в средних значениях переменных.


```stata
margins, dydx(*) atmeans
```

```
last estimates not found
r(301);

end of do-file
r(301);
```

Интерпретация предельных эффектов (на примере переменной семейного дохода): при увеличении семейного дохода в среднем на 1 единицу при остальных неизменных факторах, вероятность стать курильщиком уменьшается в среднем на 0.18%.

Визуализируем предельные эффекты.


```stata
marginsplot
```

<center>

![](marginsplot1.png)

</center>

Посмотрим на точность классификации построенной логит-модели. Для этого применяется простая команда:


```stata
estat classification
```

```
last estimates not found
r(301);

end of do-file
r(301);
```

Качество модели также можно проанализировать с помощью ROC-кривой, отражающей зависимость доли верных положительно классифицируемых наблюдений (`sensitivity`) от доли ложных положительно классифицируемых наблюдений `(1-specifity)`. Площадь под кривой обозначается как AUC. 
Она показывает качество классификации. Соответственно, чем выше AUC, тем лучше построенная модель.

Построим ROC-кривую, показывающую качество классификации построенной логит-модели.


```stata
lroc
```

<center>

![](lroc.png)

</center>

Теперь проверим правильность спецификации модели. Проведём тест, определяющий, можно ли убрать некоторые переменные из модели.

Попробуем построить ещё одну логит-модель без учёта фактора `white` и сохраним новую модель под именем `logit_model_new`.


```stata
logit smoke faminc cigtax cigprice fatheduc motheduc parity if fatheduc != . & motheduc != .
est store logit_model_new
```

```
fatheduc not found
r(111);

end of do-file
r(111);
```

Сравним `logit_model` и `logit_model_new` с помощью LR-теста (likelihood-ratio test):


```stata
lrtest logit_model logit_model_new
```

```
estimation result logit_model not found
r(111);

end of do-file
r(111);
```

`p-value = 0.08` в LR-тесте. 
Следовательно, основная гипотеза о том, что переменная `white` не влияет на решение стать курильщиком, не отвергается на 5% уровне значимости.

Построим пробит-модель и сохраним результат оцененной модели как `probit_model`.


```stata
probit smoke faminc cigtax cigprice fatheduc motheduc parity white if fatheduc != . & motheduc != .
est store probit_model
```

```
fatheduc not found
r(111);

end of do-file
r(111);
```

Сравним коэффициенты построенных моделей: модели линейной вероятности, логит- и пробит- моделей.


```stata
est tab lin_prob_model logit_model probit_model
```

```
estimation result lin_prob_model not found
r(111);

end of do-file
r(111);
```



Мини-теория:

**Линейная вероятностная модель**
Можно оценить вероятность бинарной зависимой переменной принимать определённое значение (чаще, 1). Линейная вероятностная модель имеет вид:

\[
P(y_i = 1) = x_i^T \cdot \beta + \varepsilon_i 
\]

Однако такой подход обладает существенными недостатками: нереалистичное значение оцененной вероятности, ошибки, распределённые не нормально и гетероскедастичность, поэтому есть необходимость оценивания логит- и пробит- моделей.

**Логит - модель**
Предполагается, что существует скрытая (латентная) переменная, для которой строится модель, $$y^*_i = x_i^T \cdot \beta + \varepsilon_i$$, так, что:
\[
\begin{equation*}
Y_i = 
 \begin{cases}
   1, &\text{если ${y_i}^* \geqslant 0$}\\
   0, &\text{если ${y_i}^* < 0$}
 \end{cases}
\end{equation*}
\]

 $$\varepsilon_i \sim logistic, \\f(t) = \frac{e^{-t}}{(1 + e^{-t})^2}$$
**LR-тест**

В текущем коане будем тестировать $$H_0: \beta_{white} = 0$$ против $$H_a: \beta_{white} \neq 0$$.

Статистика LR-теста имеет вид: $$2 \cdot (\ln(L) - \ln(L_{H_0})) \sim \chi^2_r$$, где $ln(L)$ - логарифм функции правдоподобия, $ln(L_{H_0})$ - логарифм функции правдоподобия со значениями параметров из основной гипотезы, r - количество ограничений в основной гипотезе.

**Пробит-модель**
Также предполагается, что существует скрытая (латентная) переменная, для которой строится модель, $$y^*_i = x_i^T \cdot \beta + \varepsilon_i$$, так, что:
\[
\begin{equation*}
Y_i = 
 \begin{cases}
   1, &\text{если ${y_i}^* \geqslant 0$}\\
   0, &\text{если ${y_i}^* < 0$}
 \end{cases}
\end{equation*}
\]

 $$\varepsilon_i \sim N(0; 1), \\ f(z) = \frac{1}{\sqrt{2 \pi}} \cdot \int_{- \infty}^{z} e^{- \frac{t^2}{2}} dt$$
 
 > Сейчас попробуем подружиться с моделями бинарного выбора на основе данных `bwght.dta`, где зависимая переменная отражает, является индивид курильщиком или нет, а в качестве независимых переменных представлены характеристики индивида: количество выкуриваемых сигарет, семейный доход, налог на сигареты, цена сигарет, образование отца и матери, паритет, цвет кожи.





