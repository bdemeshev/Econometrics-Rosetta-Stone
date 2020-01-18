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
# srRate и infl оставляем в качестве инструментов

#re_t = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
#           model = "random", effect = "twoways")
#re = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
#         model = "random")

fe_tw = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
            model = "within", effect = "twoways")


# Модель FE с инструментами
fe_iv  = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth | 
               profit + profit2 + size + leverage + srRate + infl,
             model = "within", effect = "twoways")

texreg(list(fe_tw, fe_iv))

############################## Тест на релевантность инструментов ##############################
gdp_growth_rel_fe_long = plm(data = data, gdpGrowth ~ srRate + infl + profit + 
                          profit2 + size + leverage, model = "within")
gdp_growth_rel_fe_short = plm(data = data, gdpGrowth ~ srRate + infl, model = "within")
summary(gdp_growth_rel_fe_long) # 2.2e-16 *** => инструмент релевантен
summary(gdp_growth_rel_fe_short)

############################## Тест на валидность инструментов ##############################
# Тест Саргана
resid = fe_iv$residuals
sigma2 = sum(resid^2)/(690*12 - 690 - 6)
Z = as.matrix(select(data, profit, profit2, size, leverage, srRate, infl))
J = 1/sigma2 * t(resid) %*% Z %*% solve(t(Z) %*% Z) %*% t(Z) %*% resid

pchisq(J, 1, lower.tail = FALSE)

############################## Тест Хаусмана на эндогенность ##############################
phtest(fe_iv, fe_tw) # chisq = 1.3561e-13, df = 4, p-value = 1

re = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
           model = "random")
re_iv = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth | 
              profit + profit2 + size + leverage + srRate + infl,
            model = "random")

fe_iv = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth | 
              profit + profit2 + size + leverage + srRate + infl,
            model = "within", effect = "twoways")

phtest(re_iv, re)

####  Переоценка всего-всего
pool = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
          model = "pooling")
re = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
        model = "random")
fe = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
         model = "within")
re_tw = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
            model = "random", effect = "twoways")
fe_tw = plm(data = data, liqSh ~ profit + profit2 + size + leverage + gdpGrowth, 
            model = "within", effect = "twoways")

calculate_F <- function(model_UR, model_R) {
  rss_UR = sum(model_UR$residuals^2)
  n_UR = nobs(model_UR)
  df_UR = model_UR$df.residual
  
  rss_R = sum(model_R$residuals^2)
  n_R = nobs(model_R)
  df_R = model_R$df.residual
  
  f_stat = ((rss_R - rss_UR) / (df_R - df_UR)) / (rss_UR / df_UR)
  pval = pf(q = f_stat, df1 = df_R - df_UR, df2 = df_UR, lower.tail = FALSE)
  
  print(df_UR)
  print(df_R)
  print(f_stat)
  print(pval)
}

# RE vs RE_TW
calculate_F(fe_tw, fe) # FE TW is better
calculate_F(re_tw, re) # NAN

# RE vs FE
phtest(fe, re) # FE is beter
phtest(fe_tw, re_tw) # FE_tw is better
