# Коан о простой линейной регрессии {#simplereg}




Построим простую линейную регрессию в R и проведем несложные тесты. 

Загрузим необходимые пакеты и импортируем данные.

```r
library(tidyverse) # для манипуляций с данными и построения графиков
library(skimr) #для красивого summary
library(rio) # для чтения .dta файлов
library(car) # для линейных гипотез
library(tseries)# для теста на нормальность
df = import(file = "us-return.dta")
```

Исследуем наш датасет.


```r
#skim_with(numeric = list(hist = NULL))
skim(df) #посмотрим на наши данные 
df = na.omit(df) # избавимся от пропущенных значений
df = rename(df, n = A, date = B) # дадим столбцам более осмысленные названия :)
```

Будем верить в CAPM :) Оценим параметры модели для компании MOTOR. Соответственно, зависимая переменная - разница доходностей акций MOTOR и безрискового актива, а регрессор - рыночная премия.

```r
#создаем новые переменные и добавляем их к набору данных
df <- mutate(df, y = MOTOR - RKFREE, x = MARKET - RKFREE) 
```

Визуализируем зависимость регрессора и зависимой переменной.

```r
ggplot(df, aes(x = x, y = y)) + geom_point() + geom_smooth(method=lm) +
labs(x = "risk premium", y = "return")
```

<img src="02-simplereg_files/figure-html/plot-1.png" width="672" />

Строим нашу модель и проверяем гипотезу об адекватности регрессии.

```r
ols <- lm(y ~ x, data = df) 
summary(ols)
```

```

Call:
lm(formula = y ~ x, data = df)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.168421 -0.059381 -0.003399  0.061373  0.182991 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.005253   0.007200   0.730    0.467    
x           0.848150   0.104814   8.092 5.91e-13 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.07844 on 118 degrees of freedom
Multiple R-squared:  0.3569,	Adjusted R-squared:  0.3514 
F-statistic: 65.48 on 1 and 118 DF,  p-value: 5.913e-13
```

Проверим гипотезу о равенстве коэффициента при регрессии единице. 

```r
linearHypothesis(ols, c("(Intercept) = 0", "x = 1"))
```

```
Linear hypothesis test

Hypothesis:
(Intercept) = 0
x = 1

Model 1: restricted model
Model 2: y ~ x

  Res.Df     RSS Df Sum of Sq      F Pr(>F)
1    120 0.74108                           
2    118 0.72608  2  0.014998 1.2187 0.2993
```

Сделаем предсказание по выборке.

```r
df <- mutate(df, u_hat = resid(ols), 
             y_hat = predict(ols), 
             n = seq(dim(df)[1]))
```

Посмотрим на остатки :) Протестируем остатки регрессии на нормальность с помощью теста Харке-Бера.

```r
jarque.bera.test(df$u_hat) ##в R нет поправки на размер выборки
```

```

	Jarque Bera Test

data:  df$u_hat
X-squared = 1.7803, df = 2, p-value = 0.4106
```

И тест Шапиро-Уилка.

```r
shapiro.test(df$u_hat)
```

```

	Shapiro-Wilk normality test

data:  df$u_hat
W = 0.99021, p-value = 0.5531
```

Оба теста указывают на нормальность распределения остатков регрессии.


#### То же самое в стате

Загружаем данные, любуемся и даем новые названия столбцам.

```stata
use us-return.dta
summarize
ren A n
ren B date
```

Убраем пропущенные значения и создаем новые переменные.

```stata
drop if n==.
gen y=MOTOR-RKFREE
gen x=MARKET-RKFREE
```

Визуализируем зависимость.

```r
graph twoway (lfit y x) (scatter y x)
```

Строим модель и проверяем гипотезу об адекватности регрессии.

```r
reg y x
```

Проверим гипотезу ....

```r
test _cons=0 x=1
```

Сделаем предсказание по выборке.

```r
predict u_hat, resid
predict y_hat
```

Протестируем остатки регрессии на нормальность с помощью теста Харке-Бера.

```r
sktest u_hat, noadjust #но все равно не как в R
```

И тест Шапиро-Уилка.

```r
swilk u_hat
```
