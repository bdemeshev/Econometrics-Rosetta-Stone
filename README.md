# Книжка будет! :)

https://bdemeshev.github.io/Econometrics-Rosetta-Stone/


# Рабочий процесс

* Соавторы редактируют каждый свои подпапки 01, 02, 03... и общие папки data/, images/, plots/
* В типичной папке 02 сидят находятся:
  - 02_r.Rmd — файл с R-частью главы, начинается с ##
  - 02_py.Rmd — файл с py-частью главы, начинается с ##
  - 02_stata.Rmd — файл с stata-частью главы, начинается с ##
  - возможно что-то ещё, но тогда лучше оговорить в каком-нибудь README.md зачем это нужно
* Общие папки:
  - images/ — иллюстрации, не создаваемые автоматически
  - plots/ — иллюстрации, создаваемые автоматически, как правило из статы
  - data/ — файлы с данными
Чтобы не возникало конфликтов, файлы нужно называть с номера главы, например, 02_cars.dta или
02_histogram.png.
  - 02_simplereg.Rmd — файл, запускающий детей 02_r, 02_py, 02_stata. Начинается с ##
* В папке setup, найдя свой setup_negodiay.R каждый может выбрать компилируемые главы,
отредактировав табличку child
* Книжка должна рендериться в docs сама
* Коммитим на гитхаб и ура!
* Вся книжка рендерится в pdf/html/epub командой `rmarkdown::render_site(encoding = 'UTF-8')`



# Правила Виноделов!

1. После ### в заголовках разделов должен быть пробел.

### Глава о Главном — верно
###Глава о Главном — неверно

И в комментах к коду аналогично, после креста Cum Deo # — пробел!

2. Пути к файлам — только относительные.

Скажем, в вашей negodyay_work есть подпапка images.
Тогда ссылайтейсь на неё как на images/best_picture.png. 
По дефолту R все пути будет измерять от папки, в которой находится главный .Rmd
А не полным путём /Users/Negodyay/Desktop/...

3. Подпапки и файлы — без веских причин лучше заглавных букв не делать.

Например, images лучше, чем Images.
Давайте договоримся, что у каждого могут быть подпапки:

images — с готовыми (не создаваемыми из r/python/stata) картинками.
plots — с создаваемыми в stata/r/python картинками. Если нужно :)
data — с файлами данных

Если нужны ещё какие, заявку в трёх экземплярах :) За три дня, сначала у Букина одобрить! :)

4. Каждый коан должен начинаться с заголовка первого уровня

# Коан о Дружбе Енотов и Поползней


1. В рамках борьбе с дискриминацией названия всех программ пишем с маленькой буквы :)

2. В формулах не должно быть переноса строки \\. Кроме систем уравнений или матриц.

\[
a^2 + b^2 = c^2,
\]
где $a$ — коэффициент Величия!

Пример системы:
\[
\begin{cases}
a^2 = b \\
c^2 = d \\
\end{cases}
\]

7. В корневой папке будут удаляться 

  * *.Rds, *.rds, *.do 
  * *.md кроме README.md
  * Rosetta_Stone.*


8. Stata на linux не позволяет сохранять png-файлы.

Решение: сохраняем в eps, экспортируем eps в png утилитой convert из ImageMagick
```bash
convert xxx.eps xxx.png
```


# Прежние заметки

### Темы
1. Простая регрессия
2. Модели бинарного выбора
3. Модели упорядоченного выбора
4. Пуассоновская регрессия
5. Модель неупорядоченного выбора
6. Инструменты для простой регрессии
7. ARMA
8. Простые модели панельных данных (Random effect, fixed effect, pooled, difference/indiffence)
9. Гетероскедастичность в простой регрессии
10. МГК

**Продвинутые сюжеты**

1. Динамические панели (Arellano-Bond, Blundell-Bond)
2. TOBIT, HECKIT
3. Treatment effects

**И еще кое-что:**

+ раздел "Шишки и грабли"

+ Коан про русских панд (русские переменные, чтобы работали и на линухе, и на маке, и на винде). По-разному сохранять эксель, чтобы везде открылось

+ **Словарик!!!** с командами из статы в питон и в r + ссылки на примеры использования в буке

___

### План

1 шаг: сотавить схему хаоса, список статовских команд, в каких они файлах, что предложено в виде решения

2 шаг: разнести команды по коанам

3 шаг: начать писать коаны

**Прикольно называем коаны!**. Например, Притча о простой регрессии/коан

**Пишем стильно!** В R поможет *styler.*


Нужные пакеты в питоне - statsmodels, linearmodels.


___

### Хотим

1. Книжечка (букдаун)

2. Самый лучший коан сделать в формате плаката a1 и повесить в качестве агитки


