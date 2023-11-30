library(tidyverse)
library(ggplot2)
library(fdth)
library(dplyr)

dados <- read.csv('MICRODADOS_CADASTRO_CURSOS_2022.CSV',sep=';',encoding = 'latin1')

fisica <- dados %>% filter(NO_CURSO =='Física')
view(fisica)
