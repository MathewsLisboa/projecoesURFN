library(read.dbc);library(tidyverse)

library(ggplot2);library(lubridate)

library(stringr);library(xtable);library(parallel);library(tictoc)

library(doParallel);library(foreach)


########## PARTE 1 CORRIGINDO LEITURA #############
setwd("D:\\Users\\Public\\Documents\\GIAPS\\Mortalidades\\SP1")

DOSP2000 <- read.dbc('DOSP2000.DBC')
DOSP2001 <- read.dbc('DOSP2001.DBC')
DOSP2002 <- read.dbc('DOSP2002.DBC')
DOSP2003 <- read.dbc('DOSP2003.DBC')
DOSP2004 <- read.dbc('DOSP2004.DBC')
DOSP2005 <- read.dbc('DOSP2005.DBC')
DOSP2006 <- read.dbc('DOSP2006.DBC')
DOSP2007 <- read.dbc('DOSP2007.DBC')
DOSP2008 <- read.dbc('DOSP2008.DBC')
DOSP2009 <- read.dbc('DOSP2009.DBC')
DOSP2010 <- read.dbc('DOSP2010.DBC')

setwd("D:\\Users\\Public\\Documents\\GIAPS\\Mortalidades\\SP")

DOSP2011 <- read.dbc('DOSP2011.DBC')
DOSP2012 <- read.dbc('DOSP2012.DBC')
DOSP2013 <- read.dbc('DOSP2013.DBC')
DOSP2014 <- read.dbc('DOSP2014.DBC')
DOSP2015 <- read.dbc('DOSP2015.DBC')
DOSP2016 <- read.dbc('DOSP2016.DBC')
DOSP2017 <- read.dbc('DOSP2017.DBC')
DOSP2018 <- read.dbc('DOSP2018.DBC')

tic('Transformando')

objs = mget(ls()[str_detect(ls(),'.DBC')])

names(objs) <-  gsub(".DBC",'',names(objs))

rm(list = ls()[str_detect(ls(),'.DBC')])

list2env(objs, globalenv())

objs=mget(ls(pattern = 'DO'))



for(i in 1:length(objs)){
  
  names(objs[[i]]) <- toupper(names(objs[[i]]))
  objs[[i]] <-  objs[[i]] %>% select(DTOBITO,IDADE,DTNASC,SEXO) %>%  
    mutate(UF=str_sub(names(objs[i]),start = 3, end=4),
           DTOBITO=dmy(DTOBITO))
}

toc(log=T)

rm(list=ls()[ls()!='objs'])

tic('Formatando em Dataframe')

aux <- data.frame()


doParallel::registerDoParallel(cl)

aux <- foreach(i=1:length(objs), .combine = rbind )%dopar% {
  objs[[i]]
}



sum(sapply(objs,nrow))==nrow(aux)

toc(log=T)


DOSP <- aux

# rm(list=ls()[ls()!='DOSP'])

DOSP$DTOBITO <- as.Date(DOSP$DTOBITO)



no_core <- detectCores()

cl <- makeCluster(no_core)

Medidas <- sapply(DOSP$DTNASC,str_length)

mean(Medidas, na.rm = T)

# DOSP <- DOSP[-which(Medidas<8),]


DOSP <- DOSP %>% filter(is.na(DTNASC)==F & IDADE!='999')



DOSP$DTNASC <- dmy(DOSP$DTNASC)

DOSP <- DOSP %>% drop_na(DTNASC)


DOSP$DiasVividos <- difftime(DOSP$DTOBITO,DOSP$DTNASC, units = 'days')

DOSP[DOSP$DiasVividos==0,]


fazer_idade2 <- function(x){
  if(str_sub(x,1,1)==0)
    x <- as.numeric(str_sub(x,2,3)) /(24*60)
  else
    if (str_sub(x,1,1)==1)
      x <-  as.numeric(str_sub(x,2,3))/(24)
    else
      x <- NA
    return(x)
}


fazer_idade <- function(x){
  if(str_sub(x,1,1)<4)
    x <- 0
  else
    if (str_sub(x,1,1)==4)
      x <-  0+as.numeric(str_sub(x,2))
    else
      if (str_sub(x,1,1)==5)
        x <- 100+as.numeric(str_sub(x,2))
      else
        x <- NA
      
      return(x)
}



DOSP$IDADE <- DOSP$IDADE %>% as.character()


aux <- DOSP %>% mutate(ParteDias=fazer_idade2(IDADE))

aux <- NA

for(i in 1:nrow(DOSP)){
  aux[i] <- fazer_idade2(DOSP$IDADE[i])
}


DOSP$Partedias <- aux

aux2 <- DOSP

DOSP$DiasVividos[is.na(DOSP$Partedias)==F] <- DOSP$Partedias[is.na(DOSP$Partedias)==F]

which(is.na(DOSP$Partedias)==T) %>% length()


DOSP<- DOSP [,-7]



DOSP$IDADE[is.na(DOSP$IDADE)] <- '999'



DOSP <- DOSP %>% drop_na(IDADE)

tic('Fazer Idades')
idades <- sapply(DOSP$IDADE,fazer_idade)
toc(log=T)


DOSP$NovaIdade <- idades

DOSP$NovaIdade[DOSP$NovaIdade>=90] <- 94

DOSP <- DOSP %>% drop_na(IDADE)
DOSP <- DOSP %>% drop_na(NovaIdade)


rm(list = ls()[ls()!='DOSP'])


V <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)
V <- V*365


GG <- seq(from=0, to=100, by=5)

GG[21] <- 1

GG <- sort(GG)

Cortes <- cut(DOSP$NovaIdade, GG, right = F,V)

DOSP$LI <- Cortes

DOSP$LI <- as.numeric(as.character(DOSP$LI))

DOSP$DifTempo <- DOSP$DiasVividos-DOSP$LI


DOSP$TEMPOMEDIO <- DOSP$DifTempo/365.6



DOSP <- DOSP[-which(DOSP$TEMPOMEDIO<0),]


Cortes <- cut(DOSP$NovaIdade, GG, right = F)

DOSP$CLASSE <- Cortes

which(is.na(DOSP$CLASSE))

DOSP <- DOSP %>% drop_na(TEMPOMEDIO)


load(file='D:\\Users\\Public\\Documents\\GIAPS\\Tabelas de Apoio\\DOBR.RDATA')

####### PARTE 2 CORRIGINDO TABELA FINAL 1 ###########
DOSP_NA <- DOSP[is.na(DOSP$DTNASC),]

DOSP$DTOBITO <- ymd(DOSP$DTOBITO)

DOSP$DTNASC <- ymd(DOSP$DTNASC)

DOSP <- DOSP %>% drop_na(DTOBITO)

DOSP <- DOSP %>% drop_na(DTNASC)

fazer_anos <- function(x){
  ano <- c()
  for(i in 1:length(x)){
    if(x[i]<= paste0(year(x[i]),'-08-01')){
      ano[i]=year(x[i])}
    else
      ano[i]=year(x[i])+1
  }
  return(ano)
}

DOSP$ANO_NASC <- fazer_anos(DOSP$DTNASC)
DOSP$ANO_OBITO <- fazer_anos(DOSP$DTOBITO)
###### Criando tabela final TESTE ######

tic()
Teste_M <- DOSP %>%
  mutate(
    IDOBITO= as.numeric(DiasVividos)/365.6,
    id_ob_anos_comp=floor(IDOBITO)) %>%
  group_by(id_ob_anos_comp, ANO_NASC, ANO_OBITO,UF,SEXO) %>%
  mutate(quantidade=n())%>% 
  ungroup() %>%
  distinct(id_ob_anos_comp,ANO_NASC, ANO_OBITO,UF, SEXO,.keep_all = T) %>%
  select(id_ob_anos_comp, ANO_NASC, ANO_OBITO, quantidade,UF,SEXO) %>%
  arrange(UF,SEXO,ANO_OBITO, desc(ANO_NASC), id_ob_anos_comp)
toc()


Teste_M$SEXO <- as.character(Teste_M$SEXO)

Teste_M$SEXO[Teste_M$SEXO%in%c('0','9')] <- NA

Teste_M <- Teste_M %>% drop_na(SEXO)


Teste_M$SEXO[Teste_M$SEXO=='1'] <- 'M'
Teste_M$SEXO[Teste_M$SEXO=='2'] <- 'F'

ncores <- detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)

sigla_UF <- c('AC','AL','AP','AM','BA','CE','DF','ES','GO',
              'MA','MT','MS','MG','PA','PB','PE','PI','PR','RJ',
              'RN','RS','RO','RR','SC','SE','SP','TO')

anos <- c(2000:2019)

Teste_M$id_ob_anos_comp[Teste_M$id_ob_anos_comp>100] <- 100

tic('Tempo de Arrumar idade 100')


for(k in 1:20){
  if(100%in%Teste_M$id_ob_anos_comp[Teste_M$UF=='SP' & Teste_M$ano_obito==anos[k]]==T){
    Teste_M$ano_nasc[Teste_M$UF=='SP'&Teste_M$ano_obito==anos[k] & Teste_M$id_ob_anos_comp==100]=
      Teste_M$ano_obito[Teste_M$UF=='SP' & Teste_M$ano_obito==anos[k] & Teste_M$id_ob_anos_comp==100]-101
    
  }
  
  
}



toc(log=T)

names(Teste_M) <- c('IDADE','ANO_NASC','ANO_OBITO','QTD','UF','SEXO')


Teste_M <- Teste_M %>% group_by(ANO_NASC,ANO_OBITO,SEXO,IDADE,UF) %>% summarise(sum(QTD)) %>% 
  arrange(UF,ANO_OBITO,SEXO, desc(ANO_NASC), IDADE)

names(Teste_M) <- c('ANO_NASC','ANO_OBITO','SEXO','IDADE','UF','QTD')


Teste_M <- Teste_M %>% select('UF','ANO_NASC','ANO_OBITO','SEXO','IDADE','QTD')


load(file='D:\\Users\\Public\\Documents\\GIAPS\\Tabelas de Apoio\\TabelaFinal_1.RDATA')


aux2 <- TabelaFinal_1[which(TabelaFinal_1$UF=='SP'),]

aux <- rbind(aux,Teste_M)

save(TabelaFinal_1, file='D:\\Users\\Public\\Documents\\GIAPS\\Tabelas de Apoio\\TabelaFinal_1.RDATA')


TabelaFinal_1 <- TabelaFinal_1 %>% arrange(UF,ANO_OBITO,SEXO, desc(ANO_NASC), IDADE) %>% 
  select('UF','ANO_NASC','ANO_OBITO','SEXO','IDADE','QTD')
############ PARTE 3 CORRIGNDO TABUA DE VIDA ##############

