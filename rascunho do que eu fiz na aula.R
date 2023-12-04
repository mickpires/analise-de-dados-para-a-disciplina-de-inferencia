{
library(tidyverse)
library(ggplot2)
library(fdth)
library(dplyr)
}

dados2022 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2022.CSV',sep=';',encoding = 'latin1')

fisica2022_presencial <- dados2022 %>% filter(NO_CURSO =='Física' & TP_GRAU_ACADEMICO == 1  & TP_MODALIDADE_ENSINO==1)
fisica2022_EAD <- dados2022 %>% filter(NO_CURSO == 'Física' & TP_GRAU_ACADEMICO == 1 & TP_MODALIDADE_ENSINO == 2)
view(fisica2022_presencial)

#quantidade de vagas presencial de 2022
sum(fisica2022_presencial$QT_VG_TOTAL,na.rm=TRUE)
#quantidade de vagas ead de 2022
sum(fisica2022_EAD$QT_VG_TOTAL,na.rm=TRUE)
#quantidade de vagas ociosas presencial

sum(fisica2022_presencial$QT_VG_REMANESC,na.rm=TRUE)

#quantidade e vagas remanescente ead

sum(fisica2022_EAD$QT_VG_REMANESC,na.rm=TRUE)

#quantidae de concluintes presencial

sum(fisica2022_presencial$QT_CONC,na.rm=TRUE)

#quantidade de vagas concluintes ead

sum(fisica2022_EAD$QT_CONC,na.rm=TRUE)

dados2019 <- read.csv('MICRODADOS_CADASTRO_CURSOS_2019.CSV',sep=';',encoding='latin1')
fisica2019_presencial <- dados2019 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1 & TP_MODALIDADE_ENSINO == 1) #presencial
fisica2019_EAD <- dados2019 %>% filter(NO_CURSO == 'FÍSICA' & TP_GRAU_ACADEMICO == 1 & TP_MODALIDADE_ENSINO ==2) #EAD
view(fisica2019)
num_vagas_total_sem_ead_19 <- sum(fisica2019$QT_VG_TOTAL,na.rm = TRUE) - sum(fisica2019$QT_VG_TOTAL_EAD,na.rm = TRUE)
num_vagas_total_sem_ead_19
sum(fisica2019$QT_VG_REMANESC,na.rm=TRUE)
sum(fisica2019$QT_CONC,na.rm=TRUE)

#quantidade de vagas presencial de 2022
sum(fisica2019_presencial$QT_VG_TOTAL,na.rm=TRUE)
#quantidade de vagas ead de 2022
sum(fisica2019_EAD$QT_VG_TOTAL,na.rm=TRUE)
#quantidade de vagas ociosas presencial

sum(fisica2019_presencial$QT_VG_REMANESC,na.rm=TRUE)

#quantidade e vagas remanescente ead

sum(fisica2019_EAD$QT_VG_REMANESC,na.rm=TRUE)

#quantidae de concluintes presencial

sum(fisica2019_presencial$QT_CONC,na.rm=TRUE)

#quantidade de vagas concluintes ead

sum(fisica2019_EAD$QT_CONC,na.rm=TRUE)
