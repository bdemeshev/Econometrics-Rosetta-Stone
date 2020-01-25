# Коан о панельных данных {#paneldata}








##R




<!-- Пробелы после диезов -->
<!-- Без "для" -->
<!-- С маленькой буквы -->

Загрузим необходимые библиотеки.

```r
library(plm) # Работа с панельными данными
library(lmtest) # Оценка регрессий и ковариационных матриц параметров
library(skimr) # Красивый summary
library(car) # Линейные модели
library(gplots) # Графики гетерогенности
library(rio) # Чтение данных
library(tidyverse) # Обработка данных
```

<!-- Везде ggplot -->
Загрузим данные и преобразуем нужные переменные в факторные. В данном разделе все визуализации будут построены на подмножестве данных из шести индивидов и нескольких временных точек. Это позволит не перегружать графики. Все модели будут оценены на большом массиве данных.

```r
panel = import('lwage_panel_small.csv')
```

```
Error in import("lwage_panel_small.csv"): No such file
```

```r
panel = mutate(panel, black = factor(black), id = factor(id))
```

```
Error in mutate(panel, black = factor(black), id = factor(id)): object 'panel' not found
```

Изобразим наши панельные данные на диаграмме рассеяния. Дополнительно установим параметр сглаживания, чтобы получить кривые временных рядов. Ненежным элементам графика поставим в соответствие значение "FALSE"


```r
ggplot(data = panel, aes(y = lwage, x = year, color = id)) + geom_smooth(aes(group = id), se = FALSE)  +
        geom_point(aes(color = id)) + ylab("Log(wage)") + xlab("Year")
```

```
Error in ggplot(data = panel, aes(y = lwage, x = year, color = id)): object 'panel' not found
```





Можно сгруппировать данные по различным признакам. Например, в зависимости от расы индивидов. 


```r
ggplot(data = panel) + geom_point(aes(x = year, y = lwage)) + geom_smooth(aes(x = year, y = lwage), se = FALSE) + facet_wrap(~black) + ylab("Log(wage)") + xlab("Year")
```

```
Error in ggplot(data = panel): object 'panel' not found
```

Импортируем основной датасет.

```r
Panel = import('lwage_panel_large.csv')
```

```
Error in import("lwage_panel_large.csv"): No such file
```

Визуализируем гетерогенный эффект. Можно визуализировать по годам или по индивидам. Здесь уже можно использовать полный датасет. Так как доверительные интервалы с интервалом в год не пересекаются, можно увидеть явную гетерогенность.


```r
plotmeans(lwage ~ year, main = "Heterogeineity across years", data=Panel)
```

```
Error in eval(mf$data, parent.frame()): object 'Panel' not found
```

Модель панельных данных будет выглядеть следующим образом:

\begin{equation}
y_{i t}=\alpha+x_{i t}^{\prime} \beta+z_{i}^{\prime} \gamma+c_{i}+u_{i t}
\end{equation}

<!-- Заменить тире, раскладка Ильи Бирмана -->

где $\alpha$ — константа, $c_{i}$ — индивидуальные эффекты индивидов, а $z_i$ — независимые от времени переменные. Следовательно, матрица $X$ — матрица зависимых от времени регрессов, $Z$ — матрица независимых от времени регрессоров. Дополнительно обозначим как $l_n$ вектор из единиц.

Оценим простую модель с фиксированными эффектами через within-оценку. Вычитая $\overline{y}_{i}=1 / T \sum_{t} y_{i t}$  из исходной модели, получим within-модель:

\begin{equation}
\ddot{y}_{i t}=\ddot{x}_{i t}^{\prime} \beta+\ddot{u}_{i t}
\end{equation}

где $\ddot{y}_{i t}=y_{i t}-\overline{y}_{i}, \ddot{x}_{i t k}=x_{i t k}-\overline{x}_{i k}$ and $\ddot{u}_{i t}=u_{i t}-\overline{u}_{i}$. Следует заметить, что константа $\alpha$, индивидуальные эффекты $c_i$ и инвариантные ко времени регрессоры $z_i$ исчезают из модели.

\begin{equation}
\widehat{\beta}_{F E}=\left(\ddot{X}^{\prime} \ddot{X}\right)^{-1} \ddot{X}^{\prime} \ddot{y}
\end{equation}


```r
ffe = plm(lwage ~ hours, model = "within", data = Panel)
```

```
Error in row.names(data): object 'Panel' not found
```

```r
summary(ffe)
```

```
Error in summary(ffe): object 'ffe' not found
```

Проверим значимость коэффициентов, используя ковариационную матрицу ошибок Хубера – Уайта.


```r
coeftest(ffe, vcov=vcovHC(ffe, cluster="group"))
```

```
Error in coeftest(ffe, vcov = vcovHC(ffe, cluster = "group")): object 'ffe' not found
```


Оценим модель со случайными эффектами, используя достижимый обобщённый МНК (FGLS).

\begin{equation}
\left(\begin{array}{c}{\widehat{\alpha}_{R E}} \\ {\widehat{\beta}_{R E}} \\ {\widehat{\gamma}_{R E}}\end{array}\right)=\left(W^{\prime} \widehat{\Omega}_{v}^{-1} W\right)^{-1} W^{\prime} \widehat{\Omega}_{v}^{-1} y
\end{equation}

где

$W=\left[\iota_{N T} X Z\right] \text { и } \iota_{N T} \text { это вектор из единиц размерности } N T \times 1$




```r
fre = plm(lwage ~ hours, model = "random", data = Panel)
```

```
Error in row.names(data): object 'Panel' not found
```

```r
summary(fre)
```

```
Error in summary(fre): object 'fre' not found
```

Проверим значимость коэффициентов, используя ковариационную матрицу ошибок Хубера – Уайта.


```r
coeftest(fre, vcov = vcovHC(ffe, cluster = "group"))
```

```
Error in coeftest(fre, vcov = vcovHC(ffe, cluster = "group")): object 'fre' not found
```

Проведём тест Хаусмана.
<!-- Кто лучше? -->


```r
phtest(ffe, fre)
```

```
Error in phtest(ffe, fre): object 'ffe' not found
```

Построим FD-оценку.

\begin{equation}
\dot{y}_{i t}=\dot{x}_{i t}^{\prime} \beta+\dot{u}_{i t}
\end{equation}

$\dot{y}_{i t}=y_{i t}-y_{i, t-1}, \dot{x}_{i t}=x_{i t}-x_{i, t-1}$ и $\dot{u}_{i t}=u_{i t}-u_{i, t-1}$



```r
fd = plm(lwage ~ hours - 1, model = "fd", data = Panel)
```

```
Error in row.names(data): object 'Panel' not found
```

```r
summary(fd)
```

```
Error in summary(fd): object 'fd' not found
```

Построим МНК-оценку с дамми-переменными по каждому индивиду (LSDV). Видим, что численно её результаты идентичны withih-регрессии, как и должно быть.
<!-- Скрыть результаты -->


```r
lsdv = lm(lwage ~ hours + factor(id) - 1, data = Panel)
```

```
Error in is.data.frame(data): object 'Panel' not found
```

```r
summary(lsdv)
```

```
Error in summary(lsdv): object 'lsdv' not found
```

Построим оценку сквозного МНК. Проверим значимость коэффициентов, используя ковариационную матрицу ошибок Хубера – Уайта.
Покажем, что эта модель игнорирует гетерогенный эффект.


```r
fpo = plm(lwage ~ hours, model = "pooling",data = Panel)
```

```
Error in row.names(data): object 'Panel' not found
```

```r
coeftest(fpo, vcov = vcovHC(fpo, cluster = "group"))
```

```
Error in coeftest(fpo, vcov = vcovHC(fpo, cluster = "group")): object 'fpo' not found
```

```r
summary(fpo)
```

```
Error in summary(fpo): object 'fpo' not found
```


```r
panel = import("lwage_panel_small.csv")
```

```
Error in import("lwage_panel_small.csv"): No such file
```

```r
panel = mutate(panel, black = factor(black), id = factor(id))
```

```
Error in mutate(panel, black = factor(black), id = factor(id)): object 'panel' not found
```

```r
lsdv_small = lm(lwage ~ hours + factor(id) - 1, data = panel)
```

```
Error in is.data.frame(data): object 'panel' not found
```

```r
yhat_lsdv = lsdv_small$fitted.values
```

```
Error in eval(expr, envir, enclos): object 'lsdv_small' not found
```

```r
g = ggplot(panel, aes(hours, yhat_lsdv, col = id))
```

```
Error in ggplot(panel, aes(hours, yhat_lsdv, col = id)): object 'panel' not found
```

```r
g + geom_point() + 
  geom_smooth(aes(group = id, col = id), method = "lm") + 
  geom_smooth(aes(col = "Pooled OLS"),method = "lm", se = FALSE) + 
  labs(title = "Ignoring of heterogeneous effect")
```

```
Error in eval(expr, envir, enclos): object 'g' not found
```


##Python 



```python
import numpy as np
import pandas as pd
```

Подгрузим данные и для обозначения панельных данных присвоим соответствующие индексы.
Зададим соответствующие зависимые и независимые переменные, а также регрессионную формулу.
Переменная "Entity effects" (Фиксированные эффекты) обязательна для включения для корректного распознавания панельных данных. 
Если её не включить, результат будет отличаться от R и STATA.


```python
df = pd.read_csv('lwage_panel_large.csv')
```

```
FileNotFoundError: [Errno 2] File b'lwage_panel_large.csv' does not exist: b'lwage_panel_large.csv'

Detailed traceback: 
  File "<string>", line 1, in <module>
  File "C:\Users\The_sun\ANACON~1\lib\site-packages\pandas\io\parsers.py", line 685, in parser_f
    return _read(filepath_or_buffer, kwds)
  File "C:\Users\The_sun\ANACON~1\lib\site-packages\pandas\io\parsers.py", line 457, in _read
    parser = TextFileReader(fp_or_buf, **kwds)
  File "C:\Users\The_sun\ANACON~1\lib\site-packages\pandas\io\parsers.py", line 895, in __init__
    self._make_engine(self.engine)
  File "C:\Users\The_sun\ANACON~1\lib\site-packages\pandas\io\parsers.py", line 1135, in _make_engine
    self._engine = CParserWrapper(self.f, **self.options)
  File "C:\Users\The_sun\ANACON~1\lib\site-packages\pandas\io\parsers.py", line 1917, in __init__
    self._reader = parsers.TextReader(src, **kwds)
  File "pandas\_libs\parsers.pyx", line 382, in pandas._libs.parsers.TextReader.__cinit__
  File "pandas\_libs\parsers.pyx", line 689, in pandas._libs.parsers.TextReader._setup_parser_source
```

```python
df = df.set_index(['id', 'year'])
```

```
NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
formula = 'lwage ~ 1 + hours + EntityEffects'
dependent = df.lwage
```

```
NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
regressors = df[['hours']]
```

```
NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
print(df.head())
```

```
NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Оценим FE-модель, используя within-оценку.

```python
from linearmodels import PanelOLS
```

```
C:\Users\The_sun\ANACON~1\lib\site-packages\linearmodels\panel\data.py:10: FutureWarning: The Panel class is removed from pandas. Accessing it from the top-level namespace will also be removed in the next version
  from pandas import (Categorical, DataFrame, Index, MultiIndex, Panel, Series,
```

```python
model_fe = PanelOLS.from_formula(formula, df)
```

```
NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
model_fe_fitted = model_fe.fit(cov_type='clustered', cluster_entity=True)
```

```
NameError: name 'model_fe' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
print(model_fe_fitted)
```

```
NameError: name 'model_fe_fitted' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Оценим RE-модель, используя FGLS-оценку.


```python
from linearmodels.panel import RandomEffects
model_re = RandomEffects.from_formula(formula, df)
```

```
NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
model_re_fitted = model_re.fit(cov_type='clustered', cluster_entity=True)
```

```
NameError: name 'model_re' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
dir(model_re_fitted)
```

```
NameError: name 'model_re_fitted' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
print(model_re_fitted)
```

```
NameError: name 'model_re_fitted' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Тест Хаусмана в соответствующем пакете на данный момент не реализован.
<!-- Поискать тест Хаусмана -->

Построим оценку Pooled OLS


```python
from linearmodels.panel import PooledOLS
model_pool = PooledOLS.from_formula(formula, df)
```

```
NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
model_pool_fitted = model_pool.fit(cov_type='clustered', cluster_entity=True)
```

```
NameError: name 'model_pool' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
print(model_pool_fitted)
```

```
NameError: name 'model_pool_fitted' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Построим LSDV-оценку.


```python
model_lsdv = PanelOLS.from_formula(formula, df)
model_lsdv_fitted = model_lsdv.fit(cov_type='clustered', cluster_entity=True, use_lsdv=True)
print(model_lsdv_fitted)
```

Построим FD-оценку. Здесь необходимо убрать константный признак, так как данная модель начинает выдавать ошибку. Логически, конечно, он автоматически должен исчезнуть по построению модели, но в данной реализации это требуется задать на уровне пользователя.


```python
from linearmodels.panel import FirstDifferenceOLS
formula_fd = 'lwage ~ hours + EntityEffects'
model_fd = FirstDifferenceOLS.from_formula(formula_fd, df)
```

```
NameError: name 'df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
model_fd_fitted = model_fd.fit(cov_type='clustered', cluster_entity=True)
```

```
NameError: name 'model_fd' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
print(model_fd_fitted)
```

```
NameError: name 'model_fd_fitted' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```





## Stata


Теперь то же самое в Stata

Для начала подгрузим данные и посмотрим на них. Сперва визуализируем малый датасет.



```stata
use lwage_panel_small
summarize
```

```
file lwage_panel_small.dta not found
r(601);

end of do-file
r(601);
```


```stata
xtset id year
xtline hours, overlay
clear
```

```
no variables defined
r(111);

end of do-file
r(111);
```



```stata
use lwage_panel_large
xtset id year
summarize

```

```
file lwage_panel_large.dta not found
r(601);

end of do-file
r(601);
```
Визуализируем данные. Если необходимо разнести линии на разные графики, следует убрать прараметр "overlay".

Сгенерируем новую переменную и оценим модель с фиксированными эффектами. Последний аргумент произведёт оценку стандартных ошибок переменных в форме Хубера - Уайта


```stata

xtreg lwage hours, fe vce(robust)
```

```
variable lwage not found
r(111);

end of do-file
r(111);
```


Сделаем то же самое для модели со случайными эффектами.


```stata
xtreg lwage hours, re vce(robust)
```

```
variable lwage not found
r(111);

end of do-file
r(111);
```

Тест Хаусмана.



```stata
xtreg lwage hours, re
estimates store b_re
xtreg lwage hours, fe
estimates store b_fe
hausman b_fe b_re, sigmamore
```

```
variable lwage not found
r(111);

end of do-file
r(111);
```

<!-- Вывод -->
Оценим FD-модель.

```stata
reg D.(lwage hours), vce(robust) nocon
```

```
no variables defined
r(111);

end of do-file
r(111);
```

Аналогично оцениваем модель pooled OLS.


```stata
reg lwage hours, vce(robust)
```

```
no variables defined
r(111);

end of do-file
r(111);
```

Оценим LSDV-модель.


```stata
areg lwage hours, absorb(id)
```

```
no variables defined
(error in option absorb())
r(111);

end of do-file
r(111);
```
