#practika 3 SMolkina

#1 Создайте структуру для хранения имени, фамилии, возраста и пола людей
#и заполните ее данными для трех человек. Как сделать возможность легко
#находить людей по фамилии (фамилии уникальны)? Найдите всех мужчин.
#Напечатайте все имена. Напечатайте данные второго человека.

#2 Какая структура подойдёт для хранения значений температуры
#измеряемой ежечасно в течении суток у пяти больных. Создайте такую
#структуру и заполните ее случайными данными. Когда (в какие часы) у
#второго пациента температура была выше 40 градусов?   matrix

#3  Создайте вектор содержащий 100 случайных чисел равномерно
#распределенных от -1 до 3. Найдите те из них для которых значение
#синуса больше нуля. Превратите вектор в двухмерную таблицу (10*10),
#возьмите подтаблицу
#– содержащую строки, в которых в 6ой колонке стоят числа больше 1
#– и столбцы, в которых есть хотя бы одно значение меньше -0.5.
# turn vector in matrix

#######1
people = data.frame(name=c('Julia','Igor','Misha'),
                    last_name=c('Smolkina','Petrov','Sidorov'),
                    age=c(22,5,69),
                    sex=c('f','m','m'))
#colnames(x)=c('name','last_name','age','sex')
rownames(people)=c('person_1','person_2','person_3')
people
#Как сделать возможность легко находить людей по фамилии (фамилии уникальны)?
index_last_name <- which(people=='Smolkina', arr.ind=TRUE)
index_last_name
people[index_last_name[1],]
#Найдите всех мужчин.
people[people$sex=='m',]
#Напечатайте все имена. Напечатайте данные второго человека.
people$name
people[2,]

######2
temp = matrix(round(runif(120, 35,47),digits = 2), ncol = 5, nrow = 24)
colnames(temp) = c('person_1','person_2','person_3','person_4','person_5')
rownames(temp) = c('00','01','02','03','04','05','06','07','08','09','10','11',
                '12','13','14','15',
                '16','17','18','19',
                '20','21','22','23')
temp
#Когда (в каие часы) у второго пациента температура была выше 40 градусов?
t = temp[,2,drop =F]>40
temp[t == TRUE,2]
############ 3
n = 100
vector = round(runif(n, min = -1, max = 3), digits = 2)
vector[sin(vector)>0]
matrix_vector = matrix(vector,ncol = 10,nrow = 10, byrow = T)
matrix_vector
#возьмите подтаблицу
#– содержащую строки, в которых в 6ой колонке стоят числа больше 1
#– и столбцы, в которых есть хотя бы одно значение меньше -0.5.
row_6 = matrix_vector[,6,drop =F]>1
row_6
sum(as.numeric(row_6)) # кол-во строк в которых в 6ой колонке стоят числа больше 1
#matrix_row_6 = matrix_vector[col_6 == TRUE,6,drop =F]# elements of rows in col = 6 which >1
#matrix_row_6
new_indexes = which(matrix_vector[row_6 == TRUE,]< (-0.5), arr.ind = TRUE)#indexes from old matrix 
sum(as.numeric(matrix_vector[row_6 == TRUE,]< (-0.5)))# кол-во элементов новой матр
new_indexes
new_matrix = matrix_vector[new_indexes]
new_matrix



