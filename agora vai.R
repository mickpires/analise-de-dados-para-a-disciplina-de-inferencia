{
  library(tidyverse)
  library(ggplot2)
  library(fdth)
  library(dplyr)
}

somar <- function(lista_de_nomes,nome_parametro){
  for (i in lista_de_nomes){
    variavel <- get(i)
    print(sum(variavel[[nome_parametro]],na.rm=TRUE))
  }
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

# pegando os dados de fisica de cada ano
# geral
fisica2015 <- dados2015 %>% filter(NO_CURSO == 'FÍSICA' & TP_ORGANIZACAO_ACADEMICA == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1)
fisica2016 <- dados2016 %>% filter(NO_CURSO == 'FÍSICA' & TP_ORGANIZACAO_ACADEMICA == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1)
fisica2017 <- dados2017 %>% filter(NO_CURSO == 'FÍSICA' & TP_ORGANIZACAO_ACADEMICA == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1)
fisica2018 <- dados2018 %>% filter(NO_CURSO == 'FÍSICA' & TP_ORGANIZACAO_ACADEMICA == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1)
fisica2019 <- dados2019 %>% filter(NO_CURSO == 'FÍSICA' & TP_ORGANIZACAO_ACADEMICA == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1)
fisica2020 <- dados2020 %>% filter(NO_CURSO == 'FÍSICA' & TP_ORGANIZACAO_ACADEMICA == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1)
fisica2021 <- dados2021 %>% filter(NO_CURSO == 'Física' & TP_ORGANIZACAO_ACADEMICA == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1)
fisica2022 <- dados2022 %>% filter(NO_CURSO =='Física' & TP_ORGANIZACAO_ACADEMICA == 1 & TP_CATEGORIA_ADMINISTRATIVA == 1)

# Bacharelado

fisica2015_bac = fisica2015 %>% filter(TP_GRAU_ACADEMICO == 1)
fisica2016_bac = fisica2016 %>% filter(TP_GRAU_ACADEMICO == 1)
fisica2017_bac = fisica2017 %>% filter(TP_GRAU_ACADEMICO == 1)
fisica2018_bac = fisica2018 %>% filter(TP_GRAU_ACADEMICO == 1)
fisica2019_bac = fisica2019 %>% filter(TP_GRAU_ACADEMICO == 1)
fisica2020_bac = fisica2020 %>% filter(TP_GRAU_ACADEMICO == 1)
fisica2021_bac = fisica2021 %>% filter(TP_GRAU_ACADEMICO == 1)
fisica2022_bac = fisica2022 %>% filter(TP_GRAU_ACADEMICO == 1)
bacharel <- c("fisica2015_bac", 
              "fisica2016_bac", 
              "fisica2017_bac", 
              "fisica2018_bac", 
              "fisica2019_bac", 
              "fisica2020_bac", 
              "fisica2021_bac", 
              "fisica2022_bac")
view(bacharel[1])
# Licenciatura
fisica2015_lic = fisica2015 %>% filter(TP_GRAU_ACADEMICO == 2)
fisica2016_lic = fisica2016 %>% filter(TP_GRAU_ACADEMICO == 2)
fisica2017_lic = fisica2017 %>% filter(TP_GRAU_ACADEMICO == 2)
fisica2018_lic = fisica2018 %>% filter(TP_GRAU_ACADEMICO == 2)
fisica2019_lic = fisica2019 %>% filter(TP_GRAU_ACADEMICO == 2)
fisica2020_lic = fisica2020 %>% filter(TP_GRAU_ACADEMICO == 2)
fisica2021_lic = fisica2021 %>% filter(TP_GRAU_ACADEMICO == 2)
fisica2022_lic = fisica2022 %>% filter(TP_GRAU_ACADEMICO == 2)

licenciatura <- c("fisica2015_lic", 
              "fisica2016_lic", 
              "fisica2017_lic", 
              "fisica2018_lic", 
              "fisica2019_lic", 
              "fisica2020_lic", 
              "fisica2021_lic", 
              "fisica2022_lic")

#quantidade de vagas oferecidas pro bacharel

somar(bacharel,'QT_VG_TOTAL')

# quantidade de vagas oferecidas pra licenciatura

somar(licenciatura,'QT_VG_TOTAL')

# quantidade de ingressantes bacharel
somar(bacharel,"QT_ING")

# quantidade de ingressantes licenciatura
somar(licenciatura,"QT_ING")

# quantidade de concluintes bacharel

somar(bacharel,'QT_CONC')

# quantidade de concluintes licenciatura

somar(licenciatura,'QT_CONC')

# quantidade de inscritos totais bacharel

somar(bacharel,'QT_INSCRITO_TOTAL')

# quantidade de inscritos totais bacharel

somar(licenciatura,'QT_INSCRITO_TOTAL')

# fração de concluintes/formantes

for (i in bacharel){
  variavel <- get(i)
  print(sum(variavel$QT_CONC,na.rm=TRUE)/sum(variavel$QT_ING,na.rm = TRUE))
}

for (i in licenciatura){
  variavel <- get(i)
  print(sum(variavel$QT_CONC,na.rm=TRUE)/sum(variavel$QT_ING,na.rm = TRUE))
}


resultados_bacharel <- vector("numeric", length(bacharel))
resultados_licenciatura <- vector("numeric", length(licenciatura))

for (i in seq_along(bacharel)){
  variavel <- get(bacharel[i])
  resultados_bacharel[i] <- sum(variavel$QT_CONC,na.rm=TRUE)/sum(variavel$QT_ING,na.rm = TRUE)
}

for (i in seq_along(licenciatura)){
  variavel <- get(licenciatura[i])
  resultados_licenciatura[i] <- sum(variavel$QT_CONC,na.rm=TRUE)/sum(variavel$QT_ING,na.rm = TRUE)
}

diferencas <- (resultados_bacharel - resultados_licenciatura) * 100
print(diferencas)
