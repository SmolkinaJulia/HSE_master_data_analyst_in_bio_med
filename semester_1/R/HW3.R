#HW3 Smolkina
getwd()
setwd('C:/Users/j.smolkina/OneDrive - CRITEO/Desktop/HSE/R/class3')
getwd()


######1
?mtcars
#A data frame with 32 observations on 11 (numeric) variables.
typeof(mtcars)
class(mtcars)
#[, 2]	 cyl	 Number of cylinders
typeof(mtcars[,2]) #double
mtcars['Fiat 128','cyl'] #4
index_cyl_4 <- which(mtcars$cyl==4, arr.ind=TRUE) #index of row
#mtcars[index_cyl_4,]
mtcars[mtcars$cyl==4,]
min_cyl = min(mtcars$cyl) # min = 4
#Отберите все машины с минимальным числом цилиндров
mtcars[index_cyl_4,]
#Посчитайте корреляцию между всеми свойствами машин при помощи cor(mtcars) 
cor_mtcars = cor(mtcars)
typeof(cor(mtcars))#"double"
class(cor(mtcars))# "matrix"
#с какими свойствами у потребления бензина наблюдается
                          #коэффициент корреляции менее -0.7?
# cyl disp   hp  wt 
col_cor_mtcars = which(cor(mtcars$mpg,mtcars)<(-0.7),arr.ind=TRUE)
col_cor_mtcars
#cor_mtcars[1,which(cor(mtcars$mpg,mtcars)<(-0.7),arr.ind=TRUE)]
cor_mtcars[1,col_cor_mtcars[,2]]

#######2
#Создайте вектор длины 100 заполненный случайными нормально распределенными
#значениями со средним 40 и дисперсией 10
vector = round(rnorm(100, mean = 40, sd = 10),digits = 0)
vector
sub_vec1 = vector[seq(from = 1, to = length(vector), by = 3)]
#Возьмите подвектор не содержащий каждое пятое значение исходного вектора
remove = c(seq(from = 1, to = length(vector), by = 5))
vector
sub_vec2 = vector[-remove]
#Возьмите подвектор содержащий только числа с четной целой частью
sub_vec3 = vector[which(((vector%%2)==0),arr.ind=TRUE)]
sub_vec3

######3
#Придумайте структуру (на основе list) для хранения информации (взаимное
#расположение листьев дерева ) о представленном ниже дереве. Получите из этой
#структуры вектор с именами всех листьев. Извлеките левое поддерево. Извлеките
#узел с листьями b и с.
tr = list()
tr$left = list('a','b c')
tr$right = list('d','e')
str(tr)
tr$left
tr$left[2]

