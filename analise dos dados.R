{
  library(tidyverse)
  library(ggplot2)
  library(fdth)
  library(dplyr)
  library(xtable)
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
view(fisica2022_bac)
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

#---- raquel fez ----------

Bacharelado_2015<-fisica2015_bac %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Bacharelado_2015$QT_CONC/Bacharelado_2015$QT_ING)
tx_bacharelado_2015<-Bacharelado_2015$QT_CONC/Bacharelado_2015$QT_ING

Licenciatura_2015<-fisica2015_lic %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Licenciatura_2015$QT_CONC/Licenciatura_2015$QT_ING) #teste de normalidade 
tx_licenciatura_2015<-Licenciatura_2015$QT_CONC/Licenciatura_2015$QT_ING

wilcox.test(tx_bacharelado_2015,tx_licenciatura_2015) # ver se a diferença é significativa
summary(tx_bacharelado_2015)
sd(tx_bacharelado_2015)

summary(tx_licenciatura_2015)
sd(tx_licenciatura_2015)


Bacharelado_2015[10:15,]

# obter a normalidade de cada ano do bacharel e licenciatura
Bacharelado_2016<-fisica2016_bac %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Bacharelado_2016$QT_CONC/Bacharelado_2016$QT_ING)
tx_bacharelado_2016<-Bacharelado_2016$QT_CONC/Bacharelado_2016$QT_ING

Licenciatura_2016<-fisica2016_lic %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Licenciatura_2016$QT_CONC/Licenciatura_2016$QT_ING) #teste de normalidade 
tx_licenciatura_2016<-Licenciatura_2016$QT_CONC/Licenciatura_2016$QT_ING

#wilcox teste do ano
wilcox.test(tx_bacharelado_2016,tx_licenciatura_2016)
summary(tx_bacharelado_2016)
sd(tx_bacharelado_2016)

summary(tx_licenciatura_2016)
sd(tx_licenciatura_2016)

#---------------------------------------------------------

Bacharelado_2017<-fisica2017_bac %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Bacharelado_2017$QT_CONC/Bacharelado_2017$QT_ING)
tx_bacharelado_2017<-Bacharelado_2017$QT_CONC/Bacharelado_2017$QT_ING

Licenciatura_2017<-fisica2017_lic %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Licenciatura_2017$QT_CONC/Licenciatura_2017$QT_ING) #teste de normalidade 
tx_licenciatura_2017<-Licenciatura_2017$QT_CONC/Licenciatura_2017$QT_ING

#wilcox teste do ano
wilcox.test(tx_bacharelado_2017,tx_licenciatura_2017)
summary(tx_bacharelado_2017)
sd(tx_bacharelado_2017)
summary(tx_licenciatura_2017)
sd(tx_licenciatura_2017)

#--------------------------------------------------------

Bacharelado_2018<-fisica2018_bac %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Bacharelado_2018$QT_CONC/Bacharelado_2018$QT_ING)
tx_bacharelado_2018<-Bacharelado_2018$QT_CONC/Bacharelado_2018$QT_ING

Licenciatura_2018<-fisica2018_lic %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Licenciatura_2018$QT_CONC/Licenciatura_2018$QT_ING) #teste de normalidade 
tx_licenciatura_2018<-Licenciatura_2018$QT_CONC/Licenciatura_2018$QT_ING

#wilcox teste do ano
wilcox.test(tx_bacharelado_2018,tx_licenciatura_2018)
summary(tx_bacharelado_2018)
summary(tx_licenciatura_2018)

#--------------------------------------------------------

Bacharelado_2019<-fisica2019_bac %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Bacharelado_2019$QT_CONC/Bacharelado_2019$QT_ING)
tx_bacharelado_2019<-Bacharelado_2019$QT_CONC/Bacharelado_2019$QT_ING

Licenciatura_2019<-fisica2019_lic %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Licenciatura_2019$QT_CONC/Licenciatura_2019$QT_ING) #teste de normalidade 
tx_licenciatura_2019<-Licenciatura_2019$QT_CONC/Licenciatura_2019$QT_ING

#wilcox teste do ano
wilcox.test(tx_bacharelado_2019,tx_licenciatura_2019)
summary(tx_bacharelado_2019)
summary(tx_licenciatura_2019)

#--------------------------------------------------------

Bacharelado_2020<-fisica2020_bac %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Bacharelado_2020$QT_CONC/Bacharelado_2020$QT_ING)
tx_bacharelado_2020<-Bacharelado_2020$QT_CONC/Bacharelado_2020$QT_ING


Licenciatura_2020<-fisica2020_lic %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Licenciatura_2020$QT_CONC/Licenciatura_2020$QT_ING) #teste de normalidade 
tx_licenciatura_2020<-Licenciatura_2020$QT_CONC/Licenciatura_2020$QT_ING

#wilcox teste do ano
wilcox.test(tx_bacharelado_2020,tx_licenciatura_2020)
summary(tx_bacharelado_2020)
summary(tx_licenciatura_2020)

#--------------------------------------------------------

Bacharelado_2021<-fisica2021_bac %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Bacharelado_2021$QT_CONC/Bacharelado_2021$QT_ING)
tx_bacharelado_2021<-Bacharelado_2021$QT_CONC/Bacharelado_2021$QT_ING

Licenciatura_2021<-fisica2021_lic %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Licenciatura_2021$QT_CONC/Licenciatura_2021$QT_ING) #teste de normalidade 
tx_licenciatura_2021<-Licenciatura_2021$QT_CONC/Licenciatura_2021$QT_ING

#wilcox teste do ano
wilcox.test(tx_bacharelado_2021,tx_licenciatura_2021)
summary(tx_bacharelado_2021)
summary(tx_licenciatura_2021)

#--------------------------------------------------------

Bacharelado_2022<-fisica2022_bac %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Bacharelado_2022$QT_CONC/Bacharelado_2022$QT_ING)
tx_bacharelado_2022<-Bacharelado_2022$QT_CONC/Bacharelado_2022$QT_ING

Licenciatura_2022<-fisica2022_lic %>% filter((QT_CONC!='NA'|QT_ING!='NA')& QT_ING!=0)
shapiro.test(Licenciatura_2022$QT_CONC/Licenciatura_2022$QT_ING) #teste de normalidade
tx_licenciatura_2022<-Licenciatura_2022$QT_CONC/Licenciatura_2022$QT_ING

#wilcox teste do ano
wilcox.test(tx_bacharelado_2022,tx_licenciatura_2022)
summary(tx_bacharelado_2022)
summary(tx_licenciatura_2022)

#--------------------------------------------------------

# Vou criar um df com as medias

mediasbac <- c(paste(round(mean(tx_bacharelado_2015), 2), ' \\pm ', round(sd(tx_bacharelado_2015), 2)),
               paste(round(mean(tx_bacharelado_2016), 2), ' \\pm ', round(sd(tx_bacharelado_2016), 2)),
               paste(round(mean(tx_bacharelado_2017), 2), ' \\pm ', round(sd(tx_bacharelado_2017), 2)),
               paste(round(mean(tx_bacharelado_2018), 2), ' \\pm ', round(sd(tx_bacharelado_2018), 2)),
               paste(round(mean(tx_bacharelado_2019), 2), ' \\pm ', round(sd(tx_bacharelado_2019), 2)),
               paste(round(mean(tx_bacharelado_2020), 2), ' \\pm ', round(sd(tx_bacharelado_2020), 2)),
               paste(round(mean(tx_bacharelado_2021), 2), ' \\pm ', round(sd(tx_bacharelado_2021), 2)),
               paste(round(mean(tx_bacharelado_2022), 2), ' \\pm ', round(sd(tx_bacharelado_2022), 2)))

mediaslic <- c(paste(round(mean(tx_licenciatura_2015), 2), ' \\pm ', round(sd(tx_licenciatura_2015), 2)),
               paste(round(mean(tx_licenciatura_2016), 2), ' \\pm ', round(sd(tx_licenciatura_2016), 2)),
               paste(round(mean(tx_licenciatura_2017), 2), ' \\pm ', round(sd(tx_licenciatura_2017), 2)),
               paste(round(mean(tx_licenciatura_2018), 2), ' \\pm ', round(sd(tx_licenciatura_2018), 2)),
               paste(round(mean(tx_licenciatura_2019), 2), ' \\pm ', round(sd(tx_licenciatura_2019), 2)),
               paste(round(mean(tx_licenciatura_2020), 2), ' \\pm ', round(sd(tx_licenciatura_2020), 2)),
               paste(round(mean(tx_licenciatura_2021), 2), ' \\pm ', round(sd(tx_licenciatura_2021), 2)),
               paste(round(mean(tx_licenciatura_2022), 2), ' \\pm ', round(sd(tx_licenciatura_2022), 2)))


medias <- data.frame(Anos = c('2015','2016','2017','2018','2019','2020','2021','2022'),
                     Bacharelado = mediasbac,
                     Licenciatura = mediaslic)
xtable(medias)
# ---------------------------------------------------------------------
# do ano de 2015
boxplot(tx_bacharelado_2015,tx_licenciatura_2015,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes do Ano de 2015")

# do ano de 2016

boxplot(tx_bacharelado_2016,tx_licenciatura_2016,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes do Ano de 2016")

# do ano de 2017

boxplot(tx_bacharelado_2017,tx_licenciatura_2017,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes do Ano de 2017")


boxplot(tx_bacharelado_2018,tx_licenciatura_2018,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes do Ano de 2018")

boxplot(tx_bacharelado_2016,tx_licenciatura_2019,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes do Ano de 2019")

boxplot(tx_bacharelado_2020,tx_licenciatura_2020,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes do Ano de 2020")


boxplot(tx_bacharelado_2021,tx_licenciatura_2021,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes do Ano de 2021")

boxplot(tx_bacharelado_2022,tx_licenciatura_2022,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes do Ano de 2022")

names(Bacharelado_2022) <- names(Bacharelado_2016)
fisicaBachareladoGeral <- rbind(Bacharelado_2015,Bacharelado_2016,Bacharelado_2017,Bacharelado_2018,Bacharelado_2019,Bacharelado_2020,Bacharelado_2021,Bacharelado_2022)
view(fisicaBachareladoGeral)
tx_fisicaBachareladoGeral <- fisicaBachareladoGeral$QT_CONC/fisicaBachareladoGeral$QT_ING

names(Licenciatura_2022) <- names(Licenciatura_2016)
fisicaLicenciaturaGeral <- rbind(Licenciatura_2015,Licenciatura_2016,Licenciatura_2017,Licenciatura_2018,Licenciatura_2019,Licenciatura_2020,Licenciatura_2021,Licenciatura_2022)
tx_fisicaLicenciaturaGeral <- fisicaLicenciaturaGeral$QT_CONC/fisicaLicenciaturaGeral$QT_ING

# Analise de verosimilhança 
wilcox.test(tx_fisicaBachareladoGeral,tx_fisicaLicenciaturaGeral)
boxplot(tx_fisicaBachareladoGeral,tx_fisicaLicenciaturaGeral,outline = FALSE,names=c("Bacharelado","Licenciatura"))
title(main="Taxas de Concluintes Geral")

shapiro.test(tx_fisicaBachareladoGeral)
shapiro.test(tx_fisicaLicenciaturaGeral)$p.value

# Criando a tabela
wilcoxteste <- c(round(wilcox.test(tx_bacharelado_2015,tx_licenciatura_2015)$p.value, 2),
                 round(wilcox.test(tx_bacharelado_2016,tx_licenciatura_2016)$p.value, 2),
                 round(wilcox.test(tx_bacharelado_2017,tx_licenciatura_2017)$p.value, 2),
                 round(wilcox.test(tx_bacharelado_2018,tx_licenciatura_2018)$p.value, 2),
                 round(wilcox.test(tx_bacharelado_2019,tx_licenciatura_2019)$p.value, 2),
                 round(wilcox.test(tx_bacharelado_2020,tx_licenciatura_2020)$p.value, 2),
                 round(wilcox.test(tx_bacharelado_2021,tx_licenciatura_2021)$p.value, 2),
                 round(wilcox.test(tx_bacharelado_2022,tx_licenciatura_2022)$p.value, 2))


tab_pvalor <- data.frame(Anos = c('2015','2016','2017','2018','2019','2020','2021','2022'),
                         "Valor p" = wilcoxteste)
xtable(tab_pvalor)

hist(tx_fisicaBachareladoGeral, xlab="Bacharelado",main="Histograma da taxa de conclusão do bacharelado")
hist(tx_fisicaLicenciaturaGeral,xlab="Licenciatura",main="Histograma da taxa de conclusão da licenciatura")

# transformando os dados obtidos em dataframe para assim transformar em csv

taxas_bacharel_2015 <- data.frame("2015" = tx_bacharelado_2015)
taxas_bacharel_2016 <- data.frame("2016" = tx_bacharelado_2016)
taxas_bacharel_2017 <- data.frame("2017" = tx_bacharelado_2017)
taxas_bacharel_2018 <- data.frame("2018" = tx_bacharelado_2018)
taxas_bacharel_2019 <- data.frame("2019" = tx_bacharelado_2019)
taxas_bacharel_2020 <- data.frame("2020" = tx_bacharelado_2020)
taxas_bacharel_2021 <- data.frame("2021" = tx_bacharelado_2021)
taxas_bacharel_2022 <- data.frame("2022" = tx_bacharelado_2022)

write.csv(taxas_bacharel_2015,"taxas do bacharelado do ano de 2015.csv",row.names = FALSE)
write.csv(taxas_bacharel_2016,"taxas do bacharelado do ano de 2016.csv",row.names = FALSE)
write.csv(taxas_bacharel_2017,"taxas do bacharelado do ano de 2017.csv",row.names=FALSE)
write.csv(taxas_bacharel_2018,'taxas do bacharelado do ano de 2018.csv',row.names = FALSE)
write.csv(taxas_bacharel_2019,'taxas do bacharelado do ano de 2019.csv',row.names = FALSE)
write.csv(taxas_bacharel_2020,'taxas do bacharelado do ano de 2020.csv',row.names = FALSE)
write.csv(taxas_bacharel_2021,'taxas do bacharelado do ano de 2021.csv',row.names = FALSE)
write.csv(taxas_bacharel_2022,'taxas do bacharelado do ano de 2022.csv',row.names = FALSE)

taxas_lic_2015 <- data.frame("2015" = tx_licenciatura_2015)
taxas_lic_2016 <- data.frame("2016" = tx_licenciatura_2016)
taxas_lic_2017 <- data.frame("2017" = tx_licenciatura_2017)
taxas_lic_2018 <- data.frame("2018" = tx_licenciatura_2018)
taxas_lic_2019 <- data.frame("2019" = tx_licenciatura_2019)
taxas_lic_2020 <- data.frame("2020" = tx_licenciatura_2020)
taxas_lic_2021 <- data.frame("2021" = tx_licenciatura_2021)
taxas_lic_2022 <- data.frame("2022" = tx_licenciatura_2022)

write.csv(taxas_lic_2015,"taxas da licenciatura do ano de 2015.csv",row.names = FALSE)
write.csv(taxas_lic_2016,"taxas da licenciatura do ano de 2016.csv",row.names = FALSE)
write.csv(taxas_lic_2017,"taxas da licenciatura do ano de 2017.csv",row.names=FALSE)
write.csv(taxas_lic_2018,'taxas da licenciatura do ano de 2018.csv',row.names = FALSE)
write.csv(taxas_lic_2019,'taxas da licenciatura do ano de 2019.csv',row.names = FALSE)
write.csv(taxas_lic_2020,'taxas da licenciatura do ano de 2020.csv',row.names = FALSE)
write.csv(taxas_lic_2021,'taxas da licenciatura do ano de 2021.csv',row.names = FALSE)
write.csv(taxas_lic_2022,'taxas da licenciatura do ano de 2022.csv',row.names = FALSE) 


write.csv(Bacharelado_2015,"dados filtrados/Dados do bacharelado do ano de 2015.csv",fileEncoding = 'latin1')
write.csv(Bacharelado_2016,"dados filtrados/Dados do bacharelado do ano de 2016.csv",fileEncoding = 'latin1')
write.csv(Bacharelado_2017,"dados filtrados/Dados do bacharelado do ano de 2017.csv",fileEncoding = 'latin1')
write.csv(Bacharelado_2018,"dados filtrados/Dados do bacharelado do ano de 2018.csv",fileEncoding = 'latin1')
write.csv(Bacharelado_2019,"dados filtrados/Dados do bacharelado do ano de 2019.csv",fileEncoding = 'latin1')
write.csv(Bacharelado_2020,"dados filtrados/Dados do bacharelado do ano de 2020.csv",fileEncoding = 'latin1')
write.csv(Bacharelado_2021,"dados filtrados/Dados do bacharelado do ano de 2021.csv",fileEncoding = 'latin1')
write.csv(Bacharelado_2022,"dados filtrados/Dados do bacharelado do ano de 2022.csv",fileEncoding = 'latin1')

write.csv(Licenciatura_2015,"dados filtrados/Dados da licenciatura do ano de 2015.csv",fileEncoding = 'latin1')
write.csv(Licenciatura_2016,"dados filtrados/Dados da licenciatura do ano de 2016.csv",fileEncoding = 'latin1')
write.csv(Licenciatura_2017,"dados filtrados/Dados da licenciatura do ano de 2017.csv",fileEncoding = 'latin1')
write.csv(Licenciatura_2018,"dados filtrados/Dados da licenciatura do ano de 2018.csv",fileEncoding = 'latin1')
write.csv(Licenciatura_2019,"dados filtrados/Dados da licenciatura do ano de 2019.csv",fileEncoding = 'latin1')
write.csv(Licenciatura_2020,"dados filtrados/Dados da licenciatura do ano de 2020.csv",fileEncoding = 'latin1')
write.csv(Licenciatura_2021,"dados filtrados/Dados da licenciatura do ano de 2021.csv",fileEncoding = 'latin1')
write.csv(Licenciatura_2022,"dados filtrados/Dados da licenciatura do ano de 2022.csv",fileEncoding = 'latin1')
