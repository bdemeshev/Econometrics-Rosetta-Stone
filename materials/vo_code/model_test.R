## Импорт библиотек
library(tidyverse)
library(plm)
library(texreg)

## Загрузка данных
data = read_csv("../data/final_data/data_final_clear.csv") %>% arrange(bank, month)
## Оценка pooled-модели
## y_it = \beta_0 + \beta_1 * X_it + u_it

pooled = lm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth)
rss_pooled = sum(pooled$residuals^2)
n_pooled = nobs(pooled)
df_pooled = pooled$df.residual

rss_pooled
n_pooled
df_pooled

summary(pooled)

## Оценка модели FE с индивидуальным эффектом банка
## y_it = \alpha_i + \betaX_it + u_it

fe = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth, 
         model = "within")
rss_fe = sum(fe$residuals^2)
n_fe = nobs(fe)
df_fe = fe$df.residual
summary(fe)

## Оценка модели RE с индивидуальным эффектом банка
re = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth, 
         model = "random", effect = 'individual')
rss_re = sum(re$residuals^2)
n_re = nobs(re)
df_re = re$df.residual
summary(re)

texreg(list(pooled, fe, re)) # представление

## Сравнение моделей

## Pooled vs FE
f_pfe = ((rss_pooled - rss_fe) / (df_pooled - df_fe)) / (rss_fe / df_fe)
pval_pfe = pf(q = f_pfe, df1 = df_pooled - df_fe, df2 = df_fe, lower.tail = FALSE)
pval_pfe # 0 => FE

## Pooled vs RE
betw = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth, 
           model = "between")
rss_betw = sum(betw$residuals^2)
df_betw = betw$df.residual

f_pre = (rss_betw / df_betw) / (rss_fe / df_fe) # из лекции
pval_pre = pf(q = f_pre, df1 = df_betw, df2 = df_fe, lower.tail = FALSE)
pval_pre # 0 => RE

## FE vs RE
phtest(re, fe) ## не работает, используем регрессию Мундлака

# Between-трансформация
calc_betw <- function(vect) {
  vect_betw = c()
  for (i in seq(1, 8269, 12)) {
    mean_t = mean(vect[i:(i + 11)])
    vect_betw = c(vect_betw, vect[i:(i + 11)] + mean_t - vect[i:(i + 11)])
  }
  return(vect_betw)
}

data_betw = select(data, crRate, profit, profit2, size, leverage, cbSupp, gdpGrowth) %>%
  apply(2, calc_betw)
colnames(data_betw) = c("crRate_betw", "profit_betw", "profit2_betw", "size_betw", "leverage_betw", 
                        "cbSupp_betw", "gdpGrowth_betw")
data_mund = cbind(data, data_betw)

mundlak = plm(data = data_mund, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth +
                crRate_betw + profit_betw + profit2_betw + size_betw + leverage_betw + cbSupp_betw + 
                gdpGrowth_betw, 
              model = "random")
summary(mundlak) # 12 df, 33.791 RSS

df_mundlak = mundlak$df.residual
n_mundlak = nobs(mundlak)
rss_mundlak = sum(mundlak$residuals^2)

f_re_mund = ((rss_re - rss_mundlak)/(df_re - df_mundlak)) / (rss_mundlak / df_mundlak)
pval_re_mund = pf(q = f_re_mund, df1 = df_re - df_mundlak, df2 = df_mundlak, lower.tail = FALSE)
pval_re_mund # 6.09995e-50 => H0 rejected => gamma != 0 => FE

## FE с временным эффектом
fe_t = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth,
           model = "within", effect = "twoways")
rss_fe_t = sum(fe_t$residuals^2)
n_fe_t = nobs(fe_t)
df_fe_t = fe_t$df.residual
summary(fe_t)

## Pool с временным эффектом
pooled_t = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth,
               model = "pooling", effect = "time")
rss_pooled_t = sum(pooled_t$residuals^2)
n_pooled_t = nobs(pooled_t)
df_pooled_t = pooled_t$df.residual

## RE с временным эффектом
re_t = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth, 
           model = "random", effect = "twoways")
rss_re_t = sum(re_t$residuals^2)
n_re_t = nobs(re_t)
df_re_t = re_t$df.residual

re_t = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth +
             factor(month), 
           model = "random", effect = "individual")

texreg(re_t)

texreg(list(pooled_t, fe_t, re_t)) # Представление в LaTex

## Сравнение моделей

## RE vs RE (two-way)
f_re_ret = ((rss_re - rss_re_t) / (df_re - (df_re_t))) / (rss_re_t / (df_re_t))
pval_f_re_ret = pf(q = f_re_ret, df1 = df_re - (df_re_t), df2 = df_re_t, lower.tail = FALSE)
pval_f_re_ret # 1.237554e-28 => RE с временным эффектом

## FE vs FE (two-way)
f_fe_fet = ((rss_fe - rss_fe_t) / (df_fe - (df_fe_t))) / (rss_fe_t / (df_fe_t))
pval_f_fe_fet = pf(q = f_fe_fet, df1 = df_fe - df_fe_t, df2 = df_fe_t, lower.tail = FALSE)
pval_f_fe_fet # 4.91916e-19 => FE с временным эффектом

## Pooled vs FE с временным эффектом
f_pfe_t = ((rss_pooled_t - rss_fe_t) / (df_pooled_t - df_fe_t)) / (rss_fe_t / df_fe_t)
pval_pfe_t = pf(q = f_pfe_t, df1 = df_pooled_t - df_fe_t, df2 = df_fe_t, lower.tail = FALSE)
pval_pfe_t # 0 => FE с временным эффектом

## Pooled vs RE с временным эффектом
betw_t = plm(data = data, liqSh ~ crRate + profit + profit2 + size + leverage + cbSupp + gdpGrowth 
             + factor(month), 
           model = "between")
rss_betw_t = sum(betw_t$residuals^2)
df_betw_t = betw_t$df.residual

f_pre_t = (rss_betw_t / df_betw_t) / (rss_fe_t / df_fe_t) # из лекции
pval_pre_t = pf(q = f_pre_t, df1 = df_betw_t, df2 = df_fe_t, lower.tail = FALSE)
pval_pre_t # 0 => RE с временным эффектом

## FE с временным эффектом vs RE с временным эффектом
phtest(fe_t, re_t) 
# Снова не работает, используем регрессию Мундлака

f_re_t_mund = ((rss_re_t - rss_mundlak)/(df_re_t - df_mundlak)) / (rss_mundlak / df_mundlak)
pval_re_t_mund = pf(q = f_re_t_mund, df1 = df_re_t - df_mundlak, df2 = df_mundlak, lower.tail = FALSE)
pval_re_t_mund

