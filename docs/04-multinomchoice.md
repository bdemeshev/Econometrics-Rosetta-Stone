# Модели множественного выбора {#multchoice}








<<<<<<< HEAD
=======
Импортируем датасет. 
В нем находятся данные по клиентам пенсионных фондов. 
Нас интересует переменная `pctstck`, которая принимает три значения: 0, 50, 100 в зависимоcти от ответа респондента на вопрос о предпочтительном способе инвестирования пенсионных накоплений — в облигации, смешанным способом или в акции.  ОЧЕНЬ ДЛИННОЕ ПРЕДЛОЖЕНИЕ!!!!!!


```r
df = import("../data/04_pension.dta")
```

Начнем с пристального взгляда на описательные статистки. 

```r
skim(df)
```


Table: (\#tab:skim)Data summary

                                
-------------------------  -----
Name                       df   
Number of rows             226  
Number of columns          19   
_______________________         
Column type frequency:          
numeric                    19   
________________________        
Group variables            None 
-------------------------  -----


**Variable type: numeric**

skim_variable    n_missing   complete_rate      mean        sd     p0      p25       p50       p75   p100  hist  
--------------  ----------  --------------  --------  --------  -----  -------  --------  --------  -----  ------
id                       0            1.00   2445.09   1371.27     38   1312.5   2377.50   3804.25   5014  ▇▇▇▇▅ 
pyears                   8            0.96     11.39      9.61      0      4.0      9.00     16.00     45  ▇▅▂▁▁ 
prftshr                 20            0.91      0.21      0.41      0      0.0      0.00      0.00      1  ▇▁▁▁▂ 
choice                   0            1.00      0.62      0.49      0      0.0      1.00      1.00      1  ▅▁▁▁▇ 
female                   0            1.00      0.60      0.49      0      0.0      1.00      1.00      1  ▅▁▁▁▇ 
married                  0            1.00      0.73      0.44      0      0.0      1.00      1.00      1  ▃▁▁▁▇ 
age                      0            1.00     60.70      4.29     53     57.0     60.00     64.00     73  ▇▇▇▂▁ 
educ                     7            0.97     13.52      2.55      8     12.0     12.00     16.00     18  ▁▇▂▂▂ 
finc25                  10            0.96      0.21      0.41      0      0.0      0.00      0.00      1  ▇▁▁▁▂ 
finc35                  10            0.96      0.19      0.39      0      0.0      0.00      0.00      1  ▇▁▁▁▂ 
finc50                  10            0.96      0.25      0.43      0      0.0      0.00      0.00      1  ▇▁▁▁▂ 
finc75                  10            0.96      0.12      0.33      0      0.0      0.00      0.00      1  ▇▁▁▁▁ 
finc100                 10            0.96      0.12      0.33      0      0.0      0.00      0.00      1  ▇▁▁▁▁ 
finc101                 10            0.96      0.06      0.25      0      0.0      0.00      0.00      1  ▇▁▁▁▁ 
wealth89                 0            1.00    197.91    242.09   -580     52.0    127.85    247.50   1485  ▁▇▂▁▁ 
black                    0            1.00      0.12      0.33      0      0.0      0.00      0.00      1  ▇▁▁▁▁ 
stckin89                 0            1.00      0.32      0.47      0      0.0      0.00      1.00      1  ▇▁▁▁▃ 
irain89                  0            1.00      0.50      0.50      0      0.0      0.50      1.00      1  ▇▁▁▁▇ 
pctstck                  0            1.00     46.68     39.44      0      0.0     50.00    100.00    100  ▇▁▇▁▆ 

Отсюда несложно заметить, что переменная `choice` — бинарная. 
И принимает значение `1`, если индивид в выборке имел право выбора схемы инвестирования. 
Переменнная `wealth98` — чистое богатство пенсионеров на 1989 год. 
Остальные переменные нас пока что не интересуют :)


Для начала разберемся с объясняемой переменной.
Превратим её в факторную и упорядочим категории. 


```r
df = mutate(df, y = factor(pctstck), y = relevel(y, ref = 2)) 
levels(df$y)
```

```
[1] "50"  "0"   "100"
```

Можно взглянуть на значения объясняемой переменной в разрезе какой-то другой переменной. 


```r
table(df$y, df$educ)
```

```
     
       8  9 10 11 12 13 14 15 16 17 18
  50   1  1  0  3 34  4  6  2 14  5 14
  0    5  3  0  3 31  4  7  0 11  1  7
  100  0  2  1  1 36  1  5  4  5  4  4
```

Построим модель множественного выбора (лог-линейная модель). 


```r
multmodel = multinom(y ~ choice + age + educ + wealth89 + prftshr, 
                     data = df, reflevel = '0')
```

```
# weights:  21 (12 variable)
initial  value 220.821070 
iter  10 value 208.003694
iter  20 value 204.508245
final  value 204.507779 
converged
```

```r
summary(multmodel)
```

```
Call:
multinom(formula = y ~ choice + age + educ + wealth89 + prftshr, 
    data = df, reflevel = "0")

Coefficients:
    (Intercept)       choice        age       educ     wealth89   prftshr
0    -3.7778032 -0.626950541 0.10621896 -0.1851817 3.716695e-04 0.2718066
100   0.7155771 -0.002461399 0.01139191 -0.1387427 1.684637e-05 1.2527158

Std. Errors:
    (Intercept)    choice        age       educ     wealth89   prftshr
0      1.577870 0.3701450 0.02706225 0.06827224 0.0007353231 0.4980872
100    1.368662 0.3876881 0.02549556 0.06954044 0.0007948194 0.4622377

Residual Deviance: 409.0156 
AIC: 433.0156 
```

При необходимости можем построить модельку для подвыборки, например, только для замужних/женатых.


```r
multmodel_married = multinom(y ~ choice + age + educ + wealth89 + prftshr, 
                             subset = married == 1, data = df, reflevel = '0')
```

```
# weights:  21 (12 variable)
initial  value 165.890456 
iter  10 value 152.140783
iter  20 value 149.611135
final  value 149.611069 
converged
```

```r
summary(multmodel_married)
```

```
Call:
multinom(formula = y ~ choice + age + educ + wealth89 + prftshr, 
    data = df, subset = married == 1, reflevel = "0")

Coefficients:
    (Intercept)     choice       age       educ     wealth89    prftshr
0    -4.9076771 -1.0040922 0.1279089 -0.1905437 0.0006204087 -0.1901239
100   0.2276417 -0.5382183 0.0133528 -0.1000726 0.0004076403  1.0692773

Std. Errors:
    (Intercept)    choice        age       educ     wealth89   prftshr
0      1.850782 0.4463254 0.03189365 0.07925101 0.0008454956 0.5621150
100    1.603713 0.4540907 0.02965149 0.07925338 0.0008825075 0.4947089

Residual Deviance: 299.2221 
AIC: 323.2221 
```

Быстренько прикинули значимость коэффициентов.


```r
coef(multmodel)/summary(multmodel)$standard.errors
```

```
    (Intercept)       choice       age      educ   wealth89   prftshr
0    -2.3942416 -1.693797014 3.9249859 -2.712401 0.50545058 0.5457008
100   0.5228296 -0.006348915 0.4468195 -1.995136 0.02119521 2.7101114
```

Сохраним прогнозы.

```r
fit_values = fitted(multmodel)
```

И посчитаем относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] 
показывает изменение отношения шансов при выборе альтернативы j вместо базовой альтернативы 1, если x изменился на единицу.


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
  dydx_choice         dydx_age           dydx_educ      
 Min.   :0.02341   Min.   :-0.019966   Min.   :0.01298  
 1st Qu.:0.05924   1st Qu.:-0.015237   1st Qu.:0.03141  
 Median :0.07324   Median :-0.013644   Median :0.03791  
 Mean   :0.06945   Mean   :-0.012893   Mean   :0.03468  
 3rd Qu.:0.08402   3rd Qu.:-0.011486   3rd Qu.:0.03905  
 Max.   :0.11414   Max.   :-0.005686   Max.   :0.04223  
 dydx_wealth89         dydx_prftshr     
 Min.   :-6.855e-05   Min.   :-0.25602  
 1st Qu.:-5.119e-05   1st Qu.:-0.19363  
 Median :-4.509e-05   Median :-0.16877  
 Mean   :-4.275e-05   Mean   :-0.15863  
 3rd Qu.:-3.729e-05   3rd Qu.:-0.13454  
 Max.   :-1.675e-05   Max.   :-0.03909  
```

Или при заданном значении объясняемых переменных.

```r
margins(multmodel, at = list(age = 69, choice = 1))
```

```
 at(age) at(choice)  choice      age    educ   wealth89 prftshr
      69          1 0.08024 -0.01431 0.03263 -4.857e-05 -0.1158
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
# skim(data_nlsy)
```


```r
qplot(data_nlsy, x = tradrole) + 
  xlab('Ответы респонденток') +
  ggtitle('Вот такие дела, джентельмены :)')
```

```
Error in FUN(X[[i]], ...): object 'tradrole' not found
```

![](04-multinomchoice_files/figure-epub3/hist tradrole r-1.png)<!-- -->

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



>>>>>>> 2d5797527741ec10cca1ef2ca525a6bab26d4734

