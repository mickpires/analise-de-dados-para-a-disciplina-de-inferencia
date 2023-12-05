{
library(tidyverse)
library(ggplot2)
library(fdth)
library(dplyr)
}

dados2022 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2022.CSV',sep=';',encoding = 'latin1')

fisica2022 <- dados2022 %>% filter(NO_CURSO =='Física' & TP_GRAU_ACADEMICO == 1)
view(fisica2022)

dados2021 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2021.CSV',sep=';',encoding='latin1')
fisica2021 <- dados2021 %>% filter(NO_CURSO == 'Física' & TP_GRAU_ACADEMICO == 1)

dados2020 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2020.CSV',sep=';',encoding='latin1')
fisica2020 <- dados2020 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)

dados2019 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2019.CSV',sep=';',encoding='latin1')
fisica2019 <- dados2019 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)

dados2018 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2018.CSV',sep=';',encoding='latin1')
fisica2018 <- dados2018 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)

dados2017 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2017.CSV',sep=';',encoding='latin1')
fisica2017 <- dados2017 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)

dados2016 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2016.CSV',sep=';',encoding='latin1')
fisica2016 <- dados2016 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)

dados2015 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2015.CSV',sep=';',encoding='latin1')
fisica2015 <- dados2015 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)

