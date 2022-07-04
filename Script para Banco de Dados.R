{
setwd("C:/Users/AllanaNunes/Desktop/MARINAUTA/TECNICO/CIA AMBIENTAL/PescaBase")

#Script criado para manipular dados provenientes dos app

#Pacotes usados

library(tidyr)
library(stringr)
library(dplyr)
#library(xlsx)
library(lubridate)
library(chron)
  library(abjutils)
  # Dados utilizados são provenientes do servidor KoBoToolBox, baixados no formato xls.
# Na sequência, cada aba do arquivo é salvo como .csv, com a seguinte nomenclatura:
# Primeira Aba: f_1.csv / Segunda Aba: f_2.csv

#Leitura dos dados
  
df1<-read.csv("f1.csv", h=T, sep=";")
names(df1)




df2<-read.csv("f2.csv", h=T, sep=";")


pesq<-read.csv("pesq.csv", h=T, sep=";") # Lista de todos os Pesqueiros
recurso<-read.csv("recurso.csv", h=T, sep=";") # Lista dos recursos e respectivos nomes cientidicos
barco<-read.csv("barco0222a.csv", h=T, sep=";") # Lista dos recursos e respectivos nomes cientidicos
barco$Barco<-str_to_upper(barco$Barco)
barco$Barco<-str_trim(barco$Barco)


## Manipulação de dados

## Trabalha na coluna de dados gerais do registro (f_1.csv)
names(df1)
df1<-df1%>%unite("comunidade", 17:25, na.rm=T, remove = T)
df1<-df1%>%unite("Tipo", 14: 15, na.rm=T, remove = T)
df1<-df1%>% unite("Pesqueiro", 18:25, na.rm=T, remove=T)

df1<-df1[-c(7,10,11,20)]
names(df1)<-c("ID_coletor", "ID_dispositivo", "Foto", "Data", "Hora","Entreposto", "Latitude", "Longitude","Pescador", "Barco", "Tipo","Municipio",
              "Comunidade", "Duracao","Pesqueiro", "OBS", "ID_registro", "Verificador", "Submetido", "Status", "Indice")
df1<-df1[-c(22:25)]
df1
### Trabalha na  planilha de recursos (f_2.csv)
names(df2)
df2<-df2%>% unite("Recurso", 2:6, na.rm=T, remove=T)
df2<-df2%>% unite("Arte", 9:10, na.rm=T, remove=T)
df2<-df2[-c(3,10:24,26:32)]
names(df2)<-c("Tipo_recurso","Pescado","Peso","Duzia","Preco","Beneficiamento","Producao","Arte","ID_registro")


### UnIr as duas tabelas
df<-merge(x=df1,y=df2,by="ID_registro",all.y=T) # mescla as duas tabelas provenientes do App

df<-df %>% mutate_all(na_if,"") # Transtorma todas as celulas vazias em "NA"

names(df)

#conjunto de scripts para arrumar as strings


df$Data<-as.Date(df$Data)
df$Mes<-format(df$Data, "%m")

df$Ano<-format(df$Data, "%Y")
df$Data<-format(df$Data, "%d/%m/%Y")

df$Hora<-str_sub(df$Hora, 1,5)


df$ID_dispositivo<-as.factor(df$ID_dispositivo)
df$ID_coletor<-as.factor(df$ID_coletor)
class(df$Hora)

df$Tipo<-str_replace_all(df$Tipo, "NA+", "");
df$Tipo<-str_replace_all(df$Tipo, "_+", " ")
df$Tipo<-str_trim(df$Tipo)
df<-lapply(df, rm_accent)

df$Comunidade<-str_replace_all(df$Comunidade, "NA+", "");df$Comunidade<-str_replace_all(df$Comunidade, "outro+", "");
df$Comunidade<-str_replace_all(df$Comunidade, "_+", " ")
df$Comunidade<-str_trim(df$Comunidade)

df$Municipio<-str_replace_all(df$Municipio, "_+", " ")
df$Municipio<-str_trim(df$Municipio)

df$Pescado<-str_replace_all(df$Pescado, "NA+", "");df$Pescado<-str_replace_all(df$Pescado, "outro+", "");
df$Pescado<-str_replace_all(df$Pescado, "_+", " ")
df$Pescado<-str_trim(df$Pescado)

df$Pesqueiro<-str_replace_all(df$Pesqueiro, "NA+", "");df$Pesqueiro<-str_replace_all(df$Pesqueiro, "outro+", "");
df$Pesqueiro<-str_replace_all(df$Pesqueiro, "_+", " ")
df$Pesqueiro<-str_trim(df$Pesqueiro)

df$Arte<-str_replace_all(df$Arte, "NA+", "");df$Arte<-str_replace_all(df$Arte, "outro+", "");
df$Arte<-str_replace_all(df$Arte, "_+", " ")
df$Arte<-str_trim(df$Arte)

df$Beneficiamento<-str_replace_all(df$Beneficiamento, "NA+", "");df$Beneficiamento<-str_replace_all(df$Beneficiamento, "outro+", "");

df$Beneficiamento<-str_replace_all(df$Beneficiamento, "_+", " ")
df$Beneficiamento<-str_trim(df$Beneficiamento)

df$Peso<-as.numeric(df$Peso)
df$Duzia<-as.numeric(df$Duzia)
df$Preco<-as.numeric(df$Preco)
df$Receita<-ifelse(is.na(df$Peso),df$Duzia*df$Preco,df$Peso*df$Preco)

df$Pescador<-chartr("ÁáéíóãõçÇúôê", "AaeioaocCuoe", df$Pescador)
df$Barco<-chartr("ÁáéíóãõçÇúôê", "AaeioaocCuoe", df$Barco)

df$Pescador<-str_to_upper(df$Pescador)
df$Pescador<-str_trim(df$Pescador)

df$Barco<-str_trim(df$Barco)
df$Barco<-str_to_upper(df$Barco)

df

##Completar a tabela com outras informações  (Regiao de pesca e Nomes cientificos)


levels(as.factor(df$Pescado))
df$Pescado

#antes de combinar, atualizar a ortografia das colunas de refernecia
barco<-lapply(barco, rm_accent)
barco$Barco<-str_to_upper(barco$Barco)
df<-merge(x=df,y=pesq,by=c("Pesqueiro"),all.x=T, all.y=F) # mescla as duas tabelas provenientes do App
df<-merge(x=df,y=recurso,by=c("Pescado", "Tipo_recurso"),all.x=T, all.y=F) # mescla as duas tabelas provenientes do App
df<-merge(x=df,y=barco,by=c("Barco", "Comunidade","Municipio" ),all.x=T, all.y=F) # mescla as duas tabelas provenientes do App, completando a a tabela df1 com dados da df2

recurso$Pescado<-str_trim(recurso$Pescado)

names(df)

library(writexl)
#write.xlsx(df, file = paste("BD_PMAP", Sys.Date(),".xlsx"),  col.names=TRUE, row.names = F) pacote nao esta funcionando na versao mais nova do r
write_xlsx(df, path = paste("BD_PMAP", Sys.Date(),".xlsx"),  col_names=TRUE )



  }
  


