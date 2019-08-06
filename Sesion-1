## Introducción a R
#Algunos comandos básicos
boxplot(sleep$extra ~ sleep$group, col = "gray",
        main = "Diferencias por grupo")

t.test(sleep$extra ~ sleep$group)


# Suma
1 + 1

# Multiplicación
10 * 10

# Logaritmo
log10(100)

# Print Text
"Hello World"
'Hello World'
# Histograma
hist(npk$yield)

# Asignación
test <- 1

2->test2

# or

test = 1

x <- 3
x + 5

x<-x+5

# CRAN (static version)
install.packages(c("aqp", "soilDB", "soilReports", "soiltexture"))

library(soiltexture)

help(package = "soiltexture")

# https://www.youtube.com/watch?time_continue=1&v=s3JldKoA0zw

# Vector
x<-c(1,3)

# Operadores lógicos
# ==  >   >=   <   <=   !=   & (and)   | (or)


# Functions
add_two_numbers <- function(num1, num2) {
  final=log(num1+num2)
  return(final)
}
add_two_numbers(4,5)

# Can you write a function that calculates the mean of 3 numbers?
Media3Numeros<-function(numero1,numero2,numero3=1000){
  Media<-(numero1+numero2+numero3)/3
  #Media<-mean(c(numero1,numero2,numero3))
  return(Media)
}


# Vectors
weight_g <- c(50, 60, 65, 82) # Concatenate/Combine values into a vector
weight_g

seq(0, 30) # This is the same as just `0:30`

x<-seq(0, 30, 2) # Every third number

## Función Módulo
seq(2,20)%%2

## Vectores de Strings
animals <- c('mouse', 'rat', 'dog')
animals # Characters

## Conocer el tipo del vector
class(weight_g)
class(animals)

## Conocer más del vector
str(weight_g)
str(animals)

## Agregar elementos a un vector
weight_g <- c(weight_g, 90) # add to the end of the vector
weight_g <- c(30, weight_g) # add to the beginning of the vector
weight_g


#Pregunta: Que tipo de vector será?
num_char <- c(1, 2, 3, 'a')
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c('a', 'b', 'c', TRUE)
char_logical <- c('a', 'b', 'c', TRUE)
tricky <- c(1, 2, 3, '4')


#class(num_char)
#class(num_logical)
#class(char_logical)
#class(tricky)


### Loops and vectorization

v <- c(2, 4, 6)
for (num in v) {
  print(seq(1,20,by=num))
}

w <- 0
for (num in v) {
  w <- w + num
}
w

## Ejemplo de una función que suma elementos en un vector
my_sum <- function(input_vector) {
  vector_sum <- 0
  for (num in input_vector){
    vector_sum <- vector_sum + num
  }
  return(vector_sum)
}

my_sum(v)

## Diferencias entre un string y un booleano
sum(c("TRUE"))
sum(c(TRUE))

## Paquete para leer tablas a dataframe
install.packages('readr') 

library("readr")

surveys <- readr::read_csv('https://ndownloader.figshare.com/files/2292169')

## Descargar el archivo
#download.file("https://ndownloader.figshare.com/files/2292169",
#              "portal_data.csv") # Saves this name in the current directory

## Si el archivo estuviera de forma local se podría leer como: 
#surveys <- readr::read_csv('portal_data.csv')

# A data frame is the representation of data in the format of a 
# table where the columns are vectors that all have the same length. 
# Because the columns are vectors, they all contain the same type of 
# data as we discussed before the break (e.g., characters, integers, factors). 
# We can see this when inspecting the structure of a data frame with the function str()

# Dimensiones de un dataframe
dim(surveys)

# Número de filas del dataframe
nrow(surveys)
# Número de columnas del dataframe
ncol(surveys)

# Mostrar algunos elementos del dataframe
head(surveys)
tail(surveys)

names(surveys) 
colnames(surveys)

rownames(surveys)
str(surveys)

summary(surveys)


# Indexing, el primer elemento es la fila
surveys[1, 1]

surveys[1, 6] 

surveys[, 1]

surveys[1]

surveys[1:3, 7] # first three elements in the 7th

surveys[3, ]    # the 3rd element for all columns

surveys[1:6, ]  # equivalent to head(surveys)

surveys[,-1]    # All columns, except the first

surveys[-c(7:34786),]

surveys["species_id"]

surveys[, "species_id"]

head(surveys$species_id)          # Result is a vector


# Ejemplo de while
numero=1
while(numero<200){
  numero<-numero+1}
