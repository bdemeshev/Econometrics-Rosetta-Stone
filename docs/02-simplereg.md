# Коан о простой линейной регрессии {#simplereg}

<!-- Цвета для кусков кода 
<style>
pre.r {
    background-color: #81BEF7 !important;
}
pre.stata {
    background-color: #BDBDBD !important;
}
</style> -->



Построим простую линейную регрессию в R. 

Загрузим необходимые пакеты и импортируем данные.

```r
library(tidyverse) # для манипуляций с данными и построения графиков
library(rio) # для чтения .dta файлов
library(car) # для линейных гипотез
df = import(file = "us-return.dta")
```

Исследуем наш датасет.


```r
head(df) # первые 6 наблюдений
df = rename(df, n = A, date = B) # дадим столбцам осмысленные названия :)
# sum(is.na(df)) # проверим наличие пропусков skimr::skim
df = na.omit(df) # и избавмся от них
```

Будем верить в CAPM :) Оценим параметры модели для компании MOTOR. Соответсвенно, зависимая переменная - разница доходностей акций MOTOR и безрискового актива, а регрессор - рыночная премия.

```r
df <- mutate(df, y = MOTOR - RKFREE, x = MARKET - RKFREE)
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


Визуализируем зависимость регрессора и зависимой переменной на графике.

```r
ggplot(df, aes(x, y)) +  geom_point(shape=1) + 
  geom_smooth(method=lm)
```

![](02-simplereg_files/figure-epub3/plot-1.png)<!-- -->


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
