# Модели упорядоченного выбора и условный логит {#ordchoice}




Загрузим необходимые пакеты.

```r
library(tidyverse) # для манипуляций с данными и построения графиков
library(skimr) #для красивого summary
library(rio) # для чтения .dta файлов
library(margins)
```

```
Error in library(margins): there is no package called 'margins'
```

```r
library(mlogit)
```

```
Error in library(mlogit): there is no package called 'mlogit'
```

```r
library(nnet)
library(questionr)
```

```
Error in library(questionr): there is no package called 'questionr'
```

```r
library(MASS)
library(survival)

log(6)
```

Импортируем датасет. В нем находятся данные по клиентам пенсионных фондов. Нас интересует переменная `pctstck`, которая принимает три значения: 0, 50, 100 - в зависимоcти от ответа респондента на вопрос о предпочтительном способе инвестирования пенсионных накоплений.   

```r
df = rio::import("pension.dta")
```


```r
skim_with(numeric = list(hist = NULL, p25 = NULL, p75 = NULL)) #посмотрим на данные
#skim(df)
```


Создадим факторную перменную и упорядочим категории. 


```r
df = rename(df,  alloc = pctstck) # переименуем 
df = mutate(df, alloc_factor = factor(alloc)) # факторная переменная
df = mutate(df, y = relevel(df$alloc_factor, ref = 1)) # сменить базовую категорию
levels(df$y)
```

```
[1] "0"   "50"  "100"
```

Построим модель множественного выбора (лог-линейная модель). 

```r
multmodel = multinom(y ~ choice+age+educ+wealth89+prftshr, data = df)
```

```
# weights:  21 (12 variable)
initial  value 220.821070 
iter  10 value 207.012642
iter  20 value 204.507792
final  value 204.507779 
converged
```

```r
summary(multmodel)
```

```
Call:
multinom(formula = y ~ choice + age + educ + wealth89 + prftshr, 
    data = df)

Coefficients:
    (Intercept)    choice         age       educ      wealth89    prftshr
50     3.777686 0.6269410 -0.10621691 0.18518113 -0.0003716626 -0.2717872
100    4.492971 0.6244954 -0.09482129 0.04644315 -0.0003548369  0.9809245

Std. Errors:
    (Intercept)    choice        age       educ     wealth89   prftshr
50     1.581691 0.3701263 0.02826469 0.06725443 0.0007365833 0.4988234
100    1.385291 0.3851273 0.02530600 0.07203058 0.0007896235 0.4396202

Residual Deviance: 409.0156 
AIC: 433.0156 
```

Сохраним прогнозы.

```r
fit_values = fitted(multmodel)
head(fit_values)
```

```
          0        50       100
1 0.4040703 0.3308134 0.2651163
2 0.1534943 0.2619464 0.5845593
3 0.1651913 0.2342525 0.6005562
4 0.4300671 0.1504960 0.4194370
5 0.4878942 0.2797337 0.2323721
6 0.4642700 0.1265789 0.4091510
```

И посчитать относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] - показывает изменение отношения шансов при выборе альтернативы j вместо альтернативы 0, если x изменился на единицу

```r
odds.ratio(multmodel) # отношение шансов в stata называется relative-risk ratio
```

```
Error in odds.ratio(multmodel): could not find function "odds.ratio"
```


Можем посчитать предельные эффекты в различных квартилях. 

```r
summary(marginal_effects(multmodel)) # mean как в стате
```

```
Error in marginal_effects(multmodel): could not find function "marginal_effects"
```



Допустим, мы можем упорядочить наши альтернативы (например, от более рискованного способа распределения ресурсов до менее)

```r
ordered_logit = polr(y ~ choice+age+educ+wealth89+prftshr , data = df)
ordered_probit = polr(y ~ choice+age+educ+wealth89+prftshr , data = df, method = 'probit') 

fit_prob = fitted(ordered_probit)
fit_log = fitted(ordered_logit)
ordered_probit
```

```
Call:
polr(formula = y ~ choice + age + educ + wealth89 + prftshr, 
    data = df, method = "probit")

Coefficients:
       choice           age          educ      wealth89       prftshr 
 0.2932276690 -0.0453064786  0.0269376562 -0.0001693805  0.4864824791 

Intercepts:
     0|50    50|100 
-2.578050 -1.561799 

Residual Deviance: 425.7763 
AIC: 439.7763 
(25 observations deleted due to missingness)
```

```r
ln(5)
```

```
Error in ln(5): could not find function "ln"
```



```r
cond_logit = clogit(y ~ choice+age+strata(educ)+wealth89+prftshr , data = df)
```

```
Error in coxph(formula = Surv(rep(1, 226L), y) ~ choice + age + strata(educ) + : Cox model doesn't support "mright" survival data
```

### То же самое в стате



```stata
use pension.dta
```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```


```stata
sum
```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```



```stata
ren pctstck alloc
```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```

Построим модель множественного выбора (лог-линейная модель). 

```stata
mlogit alloc choice age educ wealth89 prftshr,  baseoutcome(0) #маленькое отличие с R
```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```

Можем посмотреть на прогнозы.

```stata
predict p1 p2 p3, p
```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```

И посчитать относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] - показывает изменение отношения шансов при выборе альтернативы j вместо альтернативы 0, если x изменился на единицу


```stata
mlogit, rrr #relative-risk ratio
```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```


Можем посчитать предельные эффекты в разных точках.

```stata
margins, predict(outcome(50)) dydx( choice age educ wealth89 prftshr) atmeans 

margins, predict(outcome(50)) dydx( choice age educ wealth89 prftshr) at((p25) *)
```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```


```stata
oprobit alloc choice age educ wealth89 prftshr

ologit alloc choice age educ wealth89 prftshr
```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```



Посмотрим на conditional logit

ПОКА ЗАБИЛА


```stata

use crackers.dta


egen resp = group(id occ)

tabulate brand, generate(br)
rename br1 Sunshine
rename br2 Keebler
rename br3 Nabisco

clogit choice Sunshine Keebler Nabisco display feature price, group(resp)

```

```
Error in running command C:/Program Files (x86)/Stata13/StataMP-64.
```
