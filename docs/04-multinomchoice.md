# Модели множественного выбора {#multchoice}





## R

Загрузим необходимые пакеты.

```r
library(tidyverse) # манипуляции с данными и построени графиков
library(skimr) # симпатичное summary
library(rio) # чтения .dta файлов
library(margins) # расчет предельных эффектов
library(mlogit)
library(nnet)
```

Импортируем датасет. 
В нем находятся данные по клиентам пенсионных фондов. 
Нас интересует переменная `pctstck`, которая принимает три значения: 0, 50, 100 в зависимоcти от ответа респондента на вопрос о предпочтительном способе инвестирования пенсионных накоплений — в облигации, смешанным способом или в акции.  ОЧЕНЬ ДЛИННОЕ ПРЕДЛОЖЕНИЕ!!!!!!


```r
df = import("data/pension.dta")
```

```
Error in import("data/pension.dta"): No such file
```

Начнем с пристального взгляда на описательные статистки. 

```r
skim(df)
```

```
Error in as.data.frame.default(data): cannot coerce class '"function"' to a data.frame
```

Отсюда несложно заметить, что переменная `choice` — бинарная. 
И принимает значение `1`, если индивид в выборке имел право выбора схемы инвестирования. 
Переменнная `wealth98` — чистое богатство пенсионеров на 1989 год. 
Остальные переменные нас пока что не интересуют :)


Для начала разберемся с объясняемой переменной.
Превратим её в факторную и упорядочим категории. 


```r
df = mutate(df, y = factor(pctstck), y = relevel(y, ref = 2)) 
```

```
Error in UseMethod("mutate_"): no applicable method for 'mutate_' applied to an object of class "function"
```

```r
levels(df$y)
```

```
Error in df$y: object of type 'closure' is not subsettable
```

Можно взглянуть на значения объясняемой переменной в разрезе какой-то другой переменной. 


```r
table(df$y, df$educ)
```

```
Error in df$y: object of type 'closure' is not subsettable
```

Построим модель множественного выбора (лог-линейная модель). 


```r
multmodel = multinom(y ~ choice + age + educ + wealth89 + prftshr, 
                     data = df, reflevel = '0')
```

```
Error in as.data.frame.default(data, optional = TRUE): cannot coerce class '"function"' to a data.frame
```

```r
summary(multmodel)
```

```
Error in summary(multmodel): object 'multmodel' not found
```

При необходимости можем построить модельку для подвыборки, например, только для замужних/женатых.


```r
multmodel_married = multinom(y ~ choice + age + educ + wealth89 + prftshr, 
                             subset = married == 1, data = df, reflevel = '0')
```

```
Error in as.data.frame.default(data, optional = TRUE): cannot coerce class '"function"' to a data.frame
```

```r
summary(multmodel_married)
```

```
Error in summary(multmodel_married): object 'multmodel_married' not found
```

Быстренько прикинули значимость коэффициентов.


```r
coef(multmodel)/summary(multmodel)$standard.errors
```

```
Error in coef(multmodel): object 'multmodel' not found
```

Сохраним прогнозы.

```r
fit_values = fitted(multmodel)
```

```
Error in fitted(multmodel): object 'multmodel' not found
```

И посчитаем относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] показывает изменение отношения шансов при выборе альтернативы j вместо базовой альтернативы 1, если x изменился на единицу.


```r
odds.ratio(multmodel) 
```

```
Error in odds.ratio(multmodel): could not find function "odds.ratio"
```

Можем посчитать предельные эффекты в различных квартилях.

```r
summary(marginal_effects(multmodel))
```

```
Error in marginal_effects(multmodel): object 'multmodel' not found
```

Или при заданном значении объясняемых переменных.

```r
margins(multmodel, at = list(age = 69, choice = 1))
```

```
Error in margins(multmodel, at = list(age = 69, choice = 1)): object 'multmodel' not found
```


Теперь посмотрим на модель упорядоченного выбора :) 
Для нее возьмем другие данные. 
Выборку позаимствуем из опроса NLSY (National Longitudinal Survey of Youth). 
В ней представлены данные о 3705 молодых белых женщинах из США.
Зависимая переменная tradrole — степень согласия с утверждением «Место женщины дома, а не на работе» по четырехбалльной шкале (1 – категорически не согласна, 2 – не согласна, 3 – согласна, 4 – совершенно согласна).


```r
data_nlsy = import('data/tradrole.dta')
```

```
Error in import("data/tradrole.dta"): No such file
```

```r
#skim(data_nlsy)
```


```r
qplot(data_nlsy, x = tradrole) + 
  xlab('Ответы респонденток') +
  ggtitle('Вот такие дела, джентельмены :)')
```

```
Error in FUN(X[[i]], ...): object 'tradrole' not found
```

![](04-multinomchoice_files/figure-latex/hist tradrole r-1.pdf)<!-- --> 

Посмотрим, как влияет религиозное воспитание (`cath` — католичество и `fpro` — протестанство), число лет образования матери — `meduc` и проживание в крупном городе `urb` на объясняемую переменную.


```r
#сначала в факторные, потом регрессия
oprobit = polr(as.factor(tradrole) ~  as.factor(cath) + as.factor(fpro) + meduc + as.factor(urb), data = data_nlsy, method = "probit", na.action = na.omit)
```

```
Error in polr(as.factor(tradrole) ~ as.factor(cath) + as.factor(fpro) + : could not find function "polr"
```

```r
summary(oprobit)
```

```
Error in summary(oprobit): object 'oprobit' not found
```

В summary видим коэффициенты при регрессорах и коэффициенты при константах для каждой из упорядоченных альтернатив.


```r
summary(oprobit)
```

```
Error in summary(oprobit): object 'oprobit' not found
```


## python


```python
import numpy as np
import pandas as pd
import statsmodels.api as st
import matplotlib.pyplot as plt

plt.style.use('ggplot')
```



```python
df = pd.read_stata('data/pension.dta')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): FileNotFoundError: [Errno 2] No such file or directory: 'data/pension.dta'

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


```python
df.describe()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```



```python
df.rename(columns = {'pctstck':'y'}, inplace = True)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Подготовим данные для построения модели множественного выбора. Избавимся от пропусков в интересующих нас переменных и добавим вектор констант. 


```python
sub = df[['y', 'choice', 'age', 'wealth89', 'prftshr', 'educ', 'married']].dropna()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
y = sub['y']
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
X = sub[['choice', 'age', 'wealth89', 'prftshr']]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
X = st.add_constant(X, prepend=False)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'X' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Кросс - табличка для объясняемой переменной и числа лет образования.

```python
pd.crosstab(sub['y'], sub['educ'])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Строим модель.

```python
multmodel = st.MNLogit(y, X, )
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'y' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mm_fit = multmodel.fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'multmodel' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mm_fit.summary() ### сразу же можно проверить значимость коэффициентов
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```


```python
fitted_values = mm_fit.predict()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Отдельно можем извлечь параметры.

```python
mm_fit.params
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Для того, чтобы построить модельку по подвыборке, её (подвыборку) нужно создать :)

```python
data_m = sub[(sub.married == 1)]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
y_m = data_m['y']
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
X_m = data_m[['choice', 'age', 'wealth89', 'prftshr']]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
X_m = st.add_constant(X_m, prepend = False)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'X_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Дальше всё аналогично :)

```python
multmodel_m = st.MNLogit(y_m, X_m)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'y_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mm_fit_m = multmodel_m.fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'multmodel_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mm_fit_m.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

C пределными эффектами в питоне беда!!!!

```python
margeff = mm_fit.get_margeff()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
np.round(margeff.margeff, 3)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'margeff' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
 
Или все-таки беда с отношением шансов?

```python
y50_data = sub[sub['y'] == 50][sub.columns.difference(['y', 'married'])]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
y100_data = sub[sub['y'] == 100][sub.columns.difference(['y', 'married'])]
#np.exp(mm_fit.params[0]*y100_data) # кажется, это придется считать вручную :(
#np.exp(mm_fit.params[0]*y100_data) # не уверена, что так, но пусть пока будет
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

И вернемся к ~~сильным и независимым~~ моделькам упорядоченного выбора :) 

```python
data_nlsy = pd.read_stata('data/tradrole.dta')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): FileNotFoundError: [Errno 2] No such file or directory: 'data/tradrole.dta'

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


```python
plt.hist(data_nlsy['tradrole'])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_nlsy' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.title('')
plt.xlabel('Ответы респонденток')
plt.show('Вот такие дела, джентельмены :)')
```

![](04-multinomchoice_files/figure-latex/hist tradrole py-1.pdf)<!-- --> 

Дальше тоже пока печаль :(



## Stata




```stata
use data/pension.dta
```

```
r(601);

end of do-file
r(601);
```


```stata
sum
```

```
end of do-file
```


```stata
ren pctstck y
```

```
r(111);

end of do-file
r(111);
```

Построим модель множественного выбора (лог-линейная модель). 

```stata
mlogit y choice age educ wealth89 prftshr,  baseoutcome(0) 
```

```
r(111);

end of do-file
r(111);
```

Кросс - табличка для объясняемой переменной и числа лет образования.

```stata
table y educ
```

```
r(111);

end of do-file
r(111);
```

Можем получить прогнозы вероятностей.

```stata
predict p1 p2 p3, p
```

```
r(301);

end of do-file
r(301);
```

И посчитать относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] - показывает изменение отношения шансов при выборе альтернативы j вместо альтернативы 0, если x изменился на единицу.
В stata, в отличие от R, отношение шансов называется relative-risk ratio.


```stata
mlogit, rrr
```

```
r(301);

end of do-file
r(301);
```


Можем посчитать предельные эффекты в разных точках.

```stata
margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) atmeans 

margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) at((p25) *)

margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) at(age = 69 choice = 0)
```

```
r(301);

end of do-file
r(301);
```



И вернемся к ~~сильным и независимым~~ моделькам упорядоченного выбора :) 

```stata
use data/tradrole.dta

sum
```

```
r(601);

end of do-file
r(601);
```

!Нужно добавить название к графику

```stata
hist tradrole
```

```
r(111);

end of do-file
r(111);
```

Посмотрим, как влияет религиозное воспитание (`cath` - католичество и `fpro` - протестанство), число лет образования матери - `meduc` и проживание в крупном городе `urb` на объясняемую переменную.


```stata
oprobit tradrole i.cath i.fpro meduc i.urb
```

```
r(111);

end of do-file
r(111);
```

