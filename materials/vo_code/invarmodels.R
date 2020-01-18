# Импорт библиотек
library(plm)
library(texreg)
library(tidyverse)

## Импорт и обработка данных
data_iv = read_csv("../data/final_data/data_final_clear_iv.csv")

ggplot(data = data_iv, aes(y=cb,x=month)) +
  geom_bar(stat = "identity") +
  ylab('cb') +
  scale_x_continuous(name="Месяц", limits=c(0, 13))

# Усреднение выбранной переменной по времени
calc_mean = function(vect) {
  vect_mean = c()
  for (i in seq(1, 8280, 12)) {
    mean_t = mean(vect[i:(i+11)])
    vect_mean = c(vect_mean, mean_t)
  }
  return(vect_mean)
}

data_invar = select(data_iv, cb) %>% apply(2, calc_mean)
colnames(data_invar) = ('cbmean')
data_iv = cbind(data_iv, data_invar)

# Hausman test
re_iv = plm(data = data_iv, liqSh ~ profit + profit2 + size + leverage + cbmean + gdpGrowth | 
              profit + profit2 + size + leverage + cbmean + srRate + infl,
            model = "random")
re =  plm(data = data_iv, liqSh ~ profit + profit2 + size + leverage + cbmean + gdpGrowth,
          model = "random")
phtest(re_iv, re) # H0 не отвергается, эндогенности нет

## Модели с инвариантной переменной 

# Hausman-Taylor
ht2 = plm(data = data_iv, liqSh ~ profit + profit2 + size + leverage + gdpGrowth + cbmean, 
          random.method = "ht", model = "random", inst.method = "baltagi")
summary(ht2)
texreg(list(ht2)) # Представление в LaTex

# RE
re_inv2 = plm(data = data_iv, liqSh ~ profit + profit2 + size + leverage + gdpGrowth + cbmean, 
             model = "random")
summary(re_inv2)
texreg(list(re_inv2)) # Представление в LaTex

# FE + BE
fe = plm(data = data_iv, liqSh ~ profit + profit2 + size + leverage + gdpGrowth + cbmean, 
         model = "within")
u = fe$residuals
summary(fe)
texreg(list(fe)) # Представление в LaTex
be = plm(data = data_iv, u ~ cbmean, model = "between")
summary(be)
texreg(list(be)) # Представление в LaTex