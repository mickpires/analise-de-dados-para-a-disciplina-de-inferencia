{
library(tidyverse)
library(ggplot2)
library(fdth)
library(dplyr)
}
#carrego os dados aqui
dados2022 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2022.CSV',sep=';',encoding = 'latin1')

dados2021 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2021.CSV',sep=';',encoding='latin1')

dados2020 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2020.CSV',sep=';',encoding='latin1')

dados2019 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2019.CSV',sep=';',encoding='latin1')

dados2018 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2018.CSV',sep=';',encoding='latin1')

dados2017 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2017.CSV',sep=';',encoding='latin1')

dados2016 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2016.CSV',sep=';',encoding='latin1')

dados2015 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2015.CSV',sep=';',encoding='latin1')

# separar em presencial com ead
fisica2015 <- dados2015 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)
fisica2016 <- dados2016 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)
fisica2017 <- dados2017 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)
fisica2018 <- dados2018 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)
fisica2019 <- dados2019 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)
fisica2020 <- dados2020 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1)
fisica2021 <- dados2021 %>% filter(NO_CURSO == 'Física' & TP_GRAU_ACADEMICO == 1)
fisica2022 <- dados2022 %>% filter(NO_CURSO =='Física' & TP_GRAU_ACADEMICO == 1)

fisica2015_presencial <- fisica2015 %>% filter(TP_MODALIDADE_ENSINO == 1)
fisica2015_EAD <- fisica2015 %>% filter(TP_MODALIDADE_ENSINO == 2)

fisica2016_presencial <- fisica2016 %>% filter(TP_MODALIDADE_ENSINO == 1)
fisica2016_EAD <- fisica2016 %>% filter(TP_MODALIDADE_ENSINO == 2)

fisica2017_presencial <- fisica2017 %>% filter(TP_MODALIDADE_ENSINO ==1)
fisica2017_EAD <- fisica2017 %>% filter(TP_MODALIDADE_ENSINO == 2)

fisica2018_presencial <- fisica2018 %>% filter(TP_MODALIDADE_ENSINO == 1)
fisica2018_EAD <- fisica2018 %>% filter(TP_MODALIDADE_ENSINO == 2)

fisica2019_presencial <- fisica2019 %>% filter(TP_MODALIDADE_ENSINO == 1)
fisica2019_EAD <- fisica2019 %>% filter(TP_MODALIDADE_ENSINO == 2)

fisica2020_presencial <- fisica2020 %>% filter(TP_MODALIDADE_ENSINO == 1)
fisica2020_EAD <- fisica2020 %>% filter(TP_MODALIDADE_ENSINO == 2)

fisica2021_presencial <- fisica2021 %>% filter(TP_MODALIDADE_ENSINO == 1)
fisica2021_EAD <- fisica2021 %>% filter(TP_MODALIDADE_ENSINO == 2)

fisica2022_presencial <- fisica2022 %>% filter(TP_MODALIDADE_ENSINO == 1)
fisica2022_EAD <- fisica2022 %>% filter(TP_MODALIDADE_ENSINO == 2)



#quantidade de vagas ead
sum(fisica2015_EAD$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2016_EAD$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2017_EAD$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2018_EAD$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2019_EAD$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2020_EAD$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2021_EAD$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2022_EAD$QT_VG_TOTAL,na.rm=TRUE)

#quantidade de vagas presencial

sum(fisica2015_presencial$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2016_presencial$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2017_presencial$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2018_presencial$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2019_presencial$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2020_presencial$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2021_presencial$QT_VG_TOTAL,na.rm=TRUE)
sum(fisica2022_presencial$QT_VG_TOTAL,na.rm=TRUE)