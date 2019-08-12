library("dplyr")
library("tidyverse")
library("ggplot2")
library("broom")

cuestionario<-
  readr::read_csv(
    "https://raw.githubusercontent.com/said3427/XAL2_2019/dev/datos/cuestionario.csv")

head(cuestionario)

str(cuestionario)

summary(cuestionario)

# Arreglando Timestamp

cuestionario <- 
  cuestionario %>% mutate(Timestamp= as.chron(Timestamp,format = "%m/%d/%Y %H:%M"))

# Arreglando Deportes opcionales

cuestionario %>% distinct(DeporteOpcional)

# Contar las opciones
cuestionario %>% group_by(DeporteOpcional) %>% summarise(NumeroPosibilidades=n())

## Volleybol
cuestionario <-
  cuestionario %>% 
    mutate(DeporteOpcional= 
             ifelse(
               str_detect(DeporteOpcional, "Volleyb"), "Volleybol", DeporteOpcional)) 


cuestionario %>% 
  group_by(DeporteOpcional) %>% 
  summarise(NumeroPosibilidades=n())

## Basquetball
cuestionario <-
  cuestionario %>% 
  mutate(DeporteOpcional= 
           ifelse(
             DeporteOpcional %in% c("basketball","Basquetball","Basquetbol","Básquetbol"), 
             "Basquetbol", 
             DeporteOpcional)) 

cuestionario %>% 
  group_by(DeporteOpcional) %>% 
  summarise(NumeroPosibilidades=n())


# Ninguno

cuestionario <-
  cuestionario %>% 
  mutate(DeporteOpcional= 
           ifelse(
             DeporteOpcional %in% c("Ninguno","No"), 
             "Ninguno", 
             DeporteOpcional)) 

cuestionario %>% 
  group_by(DeporteOpcional) %>% 
  summarise(NumeroPosibilidades=n())

# Grado académico

cuestionario %>% group_by(GradoAcademico) %>% summarise(NumeroPosibilidades=n())

cuestionario %>% 
  group_by(GradoAcademico) %>% 
  summarise(NumeroPosibilidades=n())

cuestionario <-
  cuestionario %>% 
   mutate(GradoAcademico=tolower(GradoAcademico))


# Preparatoria

cuestionario <-
  cuestionario %>% 
   mutate(GradoAcademico= 
           ifelse(
             str_detect(GradoAcademico, "preparatoria"), "preparatoria", GradoAcademico)) %>%
  mutate(GradoAcademico= 
           ifelse(
             str_detect(GradoAcademico, "bachillerato"), "preparatoria", GradoAcademico))

cuestionario %>% 
  group_by(GradoAcademico) %>% 
  summarise(NumeroPosibilidades=n())

# Universidad
cuestionario <-
  cuestionario %>% 
  mutate(GradoAcademico= 
           ifelse(
             str_detect(GradoAcademico, "universi"), "universidad", GradoAcademico)) %>%
  mutate(GradoAcademico= 
           ifelse(
             str_detect(GradoAcademico, "lic"), "universidad", GradoAcademico))

cuestionario %>% 
  group_by(GradoAcademico) %>% 
  summarise(NumeroPosibilidades=n())

# Signo zodiacal

cuestionario <-
  cuestionario %>% 
  mutate(SignoZodiacal=tolower(SignoZodiacal))

cuestionario %>% 
  group_by(SignoZodiacal) %>% 
  summarise(NumeroPosibilidades=n())

## Cambiar Escorpion

cuestionario <-
  cuestionario %>% 
  mutate(SignoZodiacal= 
           ifelse(
             str_detect(SignoZodiacal, "escorpio"), 
             "escorpión", 
             SignoZodiacal)) 


## Géminis

cuestionario <-
  cuestionario %>% 
  mutate(SignoZodiacal= 
           ifelse(
             SignoZodiacal %in% c("geminis","géminis"), 
             "géminis", 
             SignoZodiacal)) 

cuestionario %>% 
  group_by(SignoZodiacal) %>% 
  summarise(NumeroPosibilidades=n())


## Eliminar los que no especifican alguno
cuestionario <-
  cuestionario %>% 
  mutate(SignoZodiacal= 
           ifelse(
             !SignoZodiacal %in% c("acuario","aries","cáncer","capricornio","escorpión","géminis","leo","piscis"), 
             NA, 
             SignoZodiacal)) 


# Analizar rápidamente los datos

cuestionarioColumnasNumericas<-
  select_if(cuestionario,is.numeric) %>% 
  melt %>%
  filter(variable!="Timestamp")

ggplot(cuestionarioColumnasNumericas,aes(y=value,fill=variable)) + 
  geom_boxplot() +
  facet_wrap(~variable,scales = "free_y") +
  theme(legend.position = 'bottom')

## Se observa que hay un problema en peso (es dificil que alguien pese 1000 kg)

cuestionario<-
  cuestionario %>% mutate(Peso=ifelse(Peso>200,NA,Peso))

cuestionarioColumnasNumericas<-
  select_if(cuestionario,is.numeric) %>% 
  melt %>%
  filter(variable!="Timestamp")

ggplot(cuestionarioColumnasNumericas,aes(y=value,fill=variable)) + 
  geom_boxplot() +
  facet_wrap(~variable,scales = "free_y") +
  theme(legend.position = 'bottom')

# Ahora están limpios los datos

## Algunas cosas que pueden hacer después
cuestionario<- cuestionario %>% mutate(IMC=Peso/Estatura^2)