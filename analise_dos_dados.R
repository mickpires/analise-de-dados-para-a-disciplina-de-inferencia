{
library(tidyverse)
library(ggplot2)
library(fdth)
library(dplyr)
}

dados2022 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2022.CSV',sep=';',encoding = 'latin1')

fisica2022 <- dados2022 %>% filter(NO_CURSO =='Física' & TP_GRAU_ACADEMICO == 1)
view(fisica2022)

dados2019 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2019.CSV',sep=';',encoding='latin1')
fisica2019 <- dados2019 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)
view(fisica2019)
