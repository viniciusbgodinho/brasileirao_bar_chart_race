
##Fonte: "https://github.com/adaoduque/Brasileirao_Dataset"

setwd('E:\\arquivos rstudio\\campeonato brasileiro')


df <- read.csv2("campeonato-brasileiro-full.csv", head = TRUE, sep= ';')

library(dplyr)


df_1 <- df %>%
  filter(Data >= "2003-01-01")


table_1 <- df_1 %>%
   select(Data,Rodada,Clube.1,Vencedor)


table_1 <- table_1 %>%
    mutate(Pontos =ifelse(Vencedor == Clube.1, "3",Vencedor))


table_1$Pontos <- ifelse(table_1$Pontos == "-", "1",table_1$Pontos)
  
table_1$Pontos <- ifelse(table_1$Pontos == table_1$Vencedor, "0",table_1$Pontos)




table_2 <- df_1 %>%
  select(Data,Rodada,Clube.2,Vencedor)


table_2 <- table_2 %>%
  mutate(Pontos =ifelse(Vencedor == Clube.2, "3",Vencedor))


table_2$Pontos <- ifelse(table_2$Pontos == "-", "1",table_2$Pontos)

table_2$Pontos <- ifelse(table_2$Pontos == table_2$Vencedor, "0",table_2$Pontos)


colnames(table_1)[3] <-'Clube'
colnames(table_2)[3] <-'Clube'



tabela <- rbind.data.frame(table_1,table_2)


tabela <- tabela %>%
  arrange(Data)


##arrumando as rodadas


library('stringr')


ano_2003 <- tabela %>%
    filter(Data >= "2003-01-01" & Data <= "2004-01-01")


ano_2003  <-str_c("2003 - ",ano_2003$Rodada)


ano_2004 <- tabela %>%
  filter(Data >= "2004-01-01" & Data <= "2005-01-01")


ano_2004  <-str_c("2004 - ",ano_2004$Rodada)

ano_2005 <- tabela %>%
  filter(Data >= "2005-01-01" & Data <= "2006-01-01")


ano_2005  <-str_c("2005 - ",ano_2005$Rodada)

ano_2006 <- tabela %>%
  filter(Data >= "2006-01-01" & Data <= "2007-01-01")


ano_2006  <-str_c("2006 - ",ano_2006$Rodada)

ano_2007 <- tabela %>%
  filter(Data >= "2007-01-01" & Data <= "2008-01-01")


ano_2007  <-str_c("2007 - ",ano_2007$Rodada)

ano_2008 <- tabela %>%
  filter(Data >= "2008-01-01" & Data <= "2009-01-01")


ano_2008  <-str_c("2008 - ",ano_2008$Rodada)

ano_2009 <- tabela %>%
  filter(Data >= "2009-01-01" & Data <= "2010-01-01")


ano_2009  <- str_c("2009 - ",ano_2009$Rodada)

ano_2010 <- tabela %>%
  filter(Data >= "2010-01-01" & Data <= "2011-01-01")


ano_2010  <- str_c("2010 - ",ano_2010$Rodada)

ano_2011 <- tabela %>%
  filter(Data >= "2011-01-01" & Data <= "2012-01-01")


ano_2011  <- str_c("2011 - ",ano_2011$Rodada)

ano_2012 <- tabela %>%
  filter(Data >= "2012-01-01" & Data <= "2013-01-01")


ano_2012  <- str_c("2012 - ",ano_2012$Rodada)

ano_2013 <- tabela %>%
  filter(Data >= "2013-01-01" & Data <= "2014-01-01")


ano_2013  <- str_c("2013 - ",ano_2013$Rodada)

ano_2014 <- tabela %>%
  filter(Data >= "2014-01-01" & Data <= "2015-01-01")


ano_2014  <- str_c("2014 - ",ano_2014$Rodada)

ano_2015 <- tabela %>%
  filter(Data >= "2015-01-01" & Data <= "2016-01-01")


ano_2015  <- str_c("2015 - ",ano_2015$Rodada)

ano_2016 <- tabela %>%
  filter(Data >= "2016-01-01" & Data <= "2017-01-01")

ano_2016  <- str_c("2016 - ",ano_2016$Rodada)

ano_2017 <- tabela %>%
  filter(Data >= "2017-01-01" & Data <= "2018-01-01")


ano_2017  <- str_c("2017 - ",ano_2017$Rodada)

ano_2018 <- tabela %>%
  filter(Data >= "2018-01-01" & Data <= "2019-01-01")


ano_2018  <- str_c("2018 - ",ano_2018$Rodada)

ano_2019 <- tabela %>%
  filter(Data >= "2019-01-01" & Data <= "2020-01-01") 


ano_2019  <- str_c("2019 - ",ano_2019$Rodada)


Ano_rodada <- c(ano_2003,ano_2004,ano_2005,ano_2006,ano_2007,ano_2008,
                    ano_2009,ano_2010,ano_2011,ano_2012,ano_2013,ano_2014,
                    ano_2015,ano_2016,ano_2017,ano_2018,ano_2019)




tabela <- cbind.data.frame(tabela, Ano_rodada)


tabela$Clube <- str_replace(tabela$Clube, "Grêmio Prudente", "Barueri") 
tabela$Clube <- str_replace(tabela$Clube, "Botafogo-rj", "Botafogo-RJ")

###incluir todos no gráfico



tabela_2 <- cbind.data.frame(rep(c("América-RN","América-MG","Athlético-PR","Atlético-GO",
                                   "Atlético-MG","Avaí",
           "Bahia","Barueri","Botafogo","Brasiliense","Ceará",
           "Chapecoense","Corinthians","Coritiba","Criciúma","Cruzeiro","CSA","Figueirense",
           "Flamengo","Fluminense","Fortaleza","Goiás","Grêmio",
           "Guarani","Internacional","Ipatinga","Joinville","Juventude","Náutico",
           "Palmeiras","Paraná","Paysandu","Ponte Preta","Portuguesa",
           "Santa Cruz","Santo André","Santos","Sport","São Caetano","São Paulo","Vasco",
           "Vitória"),662),rep(0,27804),c(rep("2003 - 1ª Rodada",42),rep("2003 - 2ª Rodada",42),
                                          rep("2003 - 3ª Rodada",42),rep("2003 - 4ª Rodada",42),
                                          rep("2003 - 5ª Rodada",42),rep("2003 - 6ª Rodada",42),
                                          rep("2003 - 7ª Rodada",42),rep("2003 - 8ª Rodada",42),
                                          rep("2003 - 9ª Rodada",42),rep("2003 - 10ª Rodada",42),
                                          rep("2003 - 11ª Rodada",42),rep("2003 - 12ª Rodada",42),
                                          rep("2003 - 13ª Rodada",42),rep("2003 - 14ª Rodada",42),
                                          rep("2003 - 15ª Rodada",42),rep("2003 - 16ª Rodada",42),
                                          rep("2003 - 17ª Rodada",42),rep("2003 - 18ª Rodada",42),
                                          rep("2003 - 19ª Rodada",42),rep("2003 - 20ª Rodada",42),
                                          rep("2003 - 21ª Rodada",42),rep("2003 - 22ª Rodada",42),
                                          rep("2003 - 23ª Rodada",42),rep("2003 - 24ª Rodada",42),
                                          rep("2003 - 25ª Rodada",42),rep("2003 - 26ª Rodada",42),
                                          rep("2003 - 27ª Rodada",42),rep("2003 - 28ª Rodada",42),
                                          rep("2003 - 29ª Rodada",42),rep("2003 - 30ª Rodada",42),
                                          rep("2003 - 31ª Rodada",42),rep("2003 - 32ª Rodada",42),
                                          rep("2003 - 33ª Rodada",42),rep("2003 - 34ª Rodada",42),
                                          rep("2003 - 35ª Rodada",42),rep("2003 - 36ª Rodada",42),
                                          rep("2003 - 37ª Rodada",42),rep("2003 - 38ª Rodada",42),
                                          rep("2003 - 39ª Rodada",42),rep("2003 - 40ª Rodada",42),
                                          rep("2003 - 41ª Rodada",42),rep("2003 - 42ª Rodada",42),
                                          rep("2003 - 43ª Rodada",42),rep("2003 - 44ª Rodada",42),
                                          rep("2003 - 45ª Rodada",42),rep("2003 - 46ª Rodada",42),  
                                          rep("2004 - 1ª Rodada",42),rep("2004 - 2ª Rodada",42),
                                          rep("2004 - 3ª Rodada",42),rep("2004 - 4ª Rodada",42),
                                          rep("2004 - 5ª Rodada",42),rep("2004 - 6ª Rodada",42),
                                          rep("2004 - 7ª Rodada",42),rep("2004 - 8ª Rodada",42),
                                          rep("2004 - 9ª Rodada",42),rep("2004 - 10ª Rodada",42),
                                          rep("2004 - 11ª Rodada",42),rep("2004 - 12ª Rodada",42),
                                          rep("2004 - 13ª Rodada",42),rep("2004 - 14ª Rodada",42),
                                          rep("2004 - 15ª Rodada",42),rep("2004 - 16ª Rodada",42),
                                          rep("2004 - 17ª Rodada",42),rep("2004 - 18ª Rodada",42),
                                          rep("2004 - 19ª Rodada",42),rep("2004 - 20ª Rodada",42),
                                          rep("2004 - 21ª Rodada",42),rep("2004 - 22ª Rodada",42),
                                          rep("2004 - 23ª Rodada",42),rep("2004 - 24ª Rodada",42),
                                          rep("2004 - 25ª Rodada",42),rep("2004 - 26ª Rodada",42),
                                          rep("2004 - 27ª Rodada",42),rep("2004 - 28ª Rodada",42),
                                          rep("2004 - 29ª Rodada",42),rep("2004 - 30ª Rodada",42),
                                          rep("2004 - 31ª Rodada",42),rep("2004 - 32ª Rodada",42),
                                          rep("2004 - 33ª Rodada",42),rep("2004 - 34ª Rodada",42),
                                          rep("2004 - 35ª Rodada",42),rep("2004 - 36ª Rodada",42),
                                          rep("2004 - 37ª Rodada",42),rep("2004 - 38ª Rodada",42),
                                          rep("2004 - 39ª Rodada",42),rep("2004 - 40ª Rodada",42),
                                          rep("2004 - 41ª Rodada",42),rep("2004 - 42ª Rodada",42),
                                          rep("2005 - 1ª Rodada",42),rep("2005 - 2ª Rodada",42),
                                          rep("2005 - 3ª Rodada",42),rep("2005 - 4ª Rodada",42),
                                          rep("2005 - 5ª Rodada",42),rep("2005 - 6ª Rodada",42),
                                          rep("2005 - 7ª Rodada",42),rep("2005 - 8ª Rodada",42),
                                          rep("2005 - 9ª Rodada",42),rep("2005 - 10ª Rodada",42),
                                          rep("2005 - 11ª Rodada",42),rep("2005 - 12ª Rodada",42),
                                          rep("2005 - 13ª Rodada",42),rep("2005 - 14ª Rodada",42),
                                          rep("2005 - 15ª Rodada",42),rep("2005 - 16ª Rodada",42),
                                          rep("2005 - 17ª Rodada",42),rep("2005 - 18ª Rodada",42),
                                          rep("2005 - 19ª Rodada",42),rep("2005 - 20ª Rodada",42),
                                          rep("2005 - 21ª Rodada",42),rep("2005 - 22ª Rodada",42),
                                          rep("2005 - 23ª Rodada",42),rep("2005 - 24ª Rodada",42),
                                          rep("2005 - 25ª Rodada",42),rep("2005 - 26ª Rodada",42),
                                          rep("2005 - 27ª Rodada",42),rep("2005 - 28ª Rodada",42),
                                          rep("2005 - 29ª Rodada",42),rep("2005 - 30ª Rodada",42),
                                          rep("2005 - 31ª Rodada",42),rep("2005 - 32ª Rodada",42),
                                          rep("2005 - 33ª Rodada",42),rep("2005 - 34ª Rodada",42),
                                          rep("2005 - 35ª Rodada",42),rep("2005 - 36ª Rodada",42),
                                          rep("2005 - 37ª Rodada",42),rep("2005 - 38ª Rodada",42),
                                          rep("2005 - 39ª Rodada",42),rep("2005 - 40ª Rodada",42),
                                          rep("2005 - 41ª Rodada",42),rep("2005 - 42ª Rodada",42),
                                          rep("2006 - 1ª Rodada",42),rep("2006 - 2ª Rodada",42),
                                          rep("2006 - 3ª Rodada",42),rep("2006 - 4ª Rodada",42),
                                          rep("2006 - 5ª Rodada",42),rep("2006 - 6ª Rodada",42),
                                          rep("2006 - 7ª Rodada",42),rep("2006 - 8ª Rodada",42),
                                          rep("2006 - 9ª Rodada",42),rep("2006 - 10ª Rodada",42),
                                          rep("2006 - 11ª Rodada",42),rep("2006 - 12ª Rodada",42),
                                          rep("2006 - 13ª Rodada",42),rep("2006 - 14ª Rodada",42),
                                          rep("2006 - 15ª Rodada",42),rep("2006 - 16ª Rodada",42),
                                          rep("2006 - 17ª Rodada",42),rep("2006 - 18ª Rodada",42),
                                          rep("2006 - 19ª Rodada",42),rep("2006 - 20ª Rodada",42),
                                          rep("2006 - 21ª Rodada",42),rep("2006 - 22ª Rodada",42),
                                          rep("2006 - 23ª Rodada",42),rep("2006 - 24ª Rodada",42),
                                          rep("2006 - 25ª Rodada",42),rep("2006 - 26ª Rodada",42),
                                          rep("2006 - 27ª Rodada",42),rep("2006 - 28ª Rodada",42),
                                          rep("2006 - 29ª Rodada",42),rep("2006 - 30ª Rodada",42),
                                          rep("2006 - 31ª Rodada",42),rep("2006 - 32ª Rodada",42),
                                          rep("2006 - 33ª Rodada",42),rep("2006 - 34ª Rodada",42),
                                          rep("2006 - 35ª Rodada",42),rep("2006 - 36ª Rodada",42),
                                          rep("2006 - 37ª Rodada",42),rep("2006 - 38ª Rodada",42),
                                          rep("2007 - 1ª Rodada",42),rep("2007 - 2ª Rodada",42),
                                          rep("2007 - 3ª Rodada",42),rep("2007 - 4ª Rodada",42),
                                          rep("2007 - 5ª Rodada",42),rep("2007 - 6ª Rodada",42),
                                          rep("2007 - 7ª Rodada",42),rep("2007 - 8ª Rodada",42),
                                          rep("2007 - 9ª Rodada",42),rep("2007 - 10ª Rodada",42),
                                          rep("2007 - 11ª Rodada",42),rep("2007 - 12ª Rodada",42),
                                          rep("2007 - 13ª Rodada",42),rep("2007 - 14ª Rodada",42),
                                          rep("2007 - 15ª Rodada",42),rep("2007 - 16ª Rodada",42),
                                          rep("2007 - 17ª Rodada",42),rep("2007 - 18ª Rodada",42),
                                          rep("2007 - 19ª Rodada",42),rep("2007 - 20ª Rodada",42),
                                          rep("2007 - 21ª Rodada",42),rep("2007 - 22ª Rodada",42),
                                          rep("2007 - 23ª Rodada",42),rep("2007 - 24ª Rodada",42),
                                          rep("2007 - 25ª Rodada",42),rep("2007 - 26ª Rodada",42),
                                          rep("2007 - 27ª Rodada",42),rep("2007 - 28ª Rodada",42),
                                          rep("2007 - 29ª Rodada",42),rep("2007 - 30ª Rodada",42),
                                          rep("2007 - 31ª Rodada",42),rep("2007 - 32ª Rodada",42),
                                          rep("2007 - 33ª Rodada",42),rep("2007 - 34ª Rodada",42),
                                          rep("2007 - 35ª Rodada",42),rep("2007 - 36ª Rodada",42),
                                          rep("2007 - 37ª Rodada",42),rep("2007 - 38ª Rodada",42),
                                          rep("2008 - 1ª Rodada",42),rep("2008 - 2ª Rodada",42),
                                          rep("2008 - 3ª Rodada",42),rep("2008 - 4ª Rodada",42),
                                          rep("2008 - 5ª Rodada",42),rep("2008 - 6ª Rodada",42),
                                          rep("2008 - 7ª Rodada",42),rep("2008 - 8ª Rodada",42),
                                          rep("2008 - 9ª Rodada",42),rep("2008 - 10ª Rodada",42),
                                          rep("2008 - 11ª Rodada",42),rep("2008 - 12ª Rodada",42),
                                          rep("2008 - 13ª Rodada",42),rep("2008 - 14ª Rodada",42),
                                          rep("2008 - 15ª Rodada",42),rep("2008 - 16ª Rodada",42),
                                          rep("2008 - 17ª Rodada",42),rep("2008 - 18ª Rodada",42),
                                          rep("2008 - 19ª Rodada",42),rep("2008 - 20ª Rodada",42),
                                          rep("2008 - 21ª Rodada",42),rep("2008 - 22ª Rodada",42),
                                          rep("2008 - 23ª Rodada",42),rep("2008 - 24ª Rodada",42),
                                          rep("2008 - 25ª Rodada",42),rep("2008 - 26ª Rodada",42),
                                          rep("2008 - 27ª Rodada",42),rep("2008 - 28ª Rodada",42),
                                          rep("2008 - 29ª Rodada",42),rep("2008 - 30ª Rodada",42),
                                          rep("2008 - 31ª Rodada",42),rep("2008 - 32ª Rodada",42),
                                          rep("2008 - 33ª Rodada",42),rep("2008 - 34ª Rodada",42),
                                          rep("2008 - 35ª Rodada",42),rep("2008 - 36ª Rodada",42),
                                          rep("2008 - 37ª Rodada",42),rep("2008 - 38ª Rodada",42),
                                          rep("2009 - 1ª Rodada",42),rep("2009 - 2ª Rodada",42),
                                          rep("2009 - 3ª Rodada",42),rep("2009 - 4ª Rodada",42),
                                          rep("2009 - 5ª Rodada",42),rep("2009 - 6ª Rodada",42),
                                          rep("2009 - 7ª Rodada",42),rep("2009 - 8ª Rodada",42),
                                          rep("2009 - 9ª Rodada",42),rep("2009 - 10ª Rodada",42),
                                          rep("2009 - 11ª Rodada",42),rep("2009 - 12ª Rodada",42),
                                          rep("2009 - 13ª Rodada",42),rep("2009 - 14ª Rodada",42),
                                          rep("2009 - 15ª Rodada",42),rep("2009 - 16ª Rodada",42),
                                          rep("2009 - 17ª Rodada",42),rep("2009 - 18ª Rodada",42),
                                          rep("2009 - 19ª Rodada",42),rep("2009 - 20ª Rodada",42),
                                          rep("2009 - 21ª Rodada",42),rep("2009 - 22ª Rodada",42),
                                          rep("2009 - 23ª Rodada",42),rep("2009 - 24ª Rodada",42),
                                          rep("2009 - 25ª Rodada",42),rep("2009 - 26ª Rodada",42),
                                          rep("2009 - 27ª Rodada",42),rep("2009 - 28ª Rodada",42),
                                          rep("2009 - 29ª Rodada",42),rep("2009 - 30ª Rodada",42),
                                          rep("2009 - 31ª Rodada",42),rep("2009 - 32ª Rodada",42),
                                          rep("2009 - 33ª Rodada",42),rep("2009 - 34ª Rodada",42),
                                          rep("2009 - 35ª Rodada",42),rep("2009 - 36ª Rodada",42),
                                          rep("2009 - 37ª Rodada",42),rep("2009 - 38ª Rodada",42),
                                          rep("2010 - 1ª Rodada",42),rep("2010 - 2ª Rodada",42),
                                          rep("2010 - 3ª Rodada",42),rep("2010 - 4ª Rodada",42),
                                          rep("2010 - 5ª Rodada",42),rep("2010 - 6ª Rodada",42),
                                          rep("2010 - 7ª Rodada",42),rep("2010 - 8ª Rodada",42),
                                          rep("2010 - 9ª Rodada",42),rep("2010 - 10ª Rodada",42),
                                          rep("2010 - 11ª Rodada",42),rep("2010 - 12ª Rodada",42),
                                          rep("2010 - 13ª Rodada",42),rep("2010 - 14ª Rodada",42),
                                          rep("2010 - 15ª Rodada",42),rep("2010 - 16ª Rodada",42),
                                          rep("2010 - 17ª Rodada",42),rep("2010 - 18ª Rodada",42),
                                          rep("2010 - 19ª Rodada",42),rep("2010 - 20ª Rodada",42),
                                          rep("2010 - 21ª Rodada",42),rep("2010 - 22ª Rodada",42),
                                          rep("2010 - 23ª Rodada",42),rep("2010 - 24ª Rodada",42),
                                          rep("2010 - 25ª Rodada",42),rep("2010 - 26ª Rodada",42),
                                          rep("2010 - 27ª Rodada",42),rep("2010 - 28ª Rodada",42),
                                          rep("2010 - 29ª Rodada",42),rep("2010 - 30ª Rodada",42),
                                          rep("2010 - 31ª Rodada",42),rep("2010 - 32ª Rodada",42),
                                          rep("2010 - 33ª Rodada",42),rep("2010 - 34ª Rodada",42),
                                          rep("2010 - 35ª Rodada",42),rep("2010 - 36ª Rodada",42),
                                          rep("2010 - 37ª Rodada",42),rep("2010 - 38ª Rodada",42),
                                          rep("2011 - 1ª Rodada",42),rep("2011 - 2ª Rodada",42),
                                          rep("2011 - 3ª Rodada",42),rep("2011 - 4ª Rodada",42),
                                          rep("2011 - 5ª Rodada",42),rep("2011 - 6ª Rodada",42),
                                          rep("2011 - 7ª Rodada",42),rep("2011 - 8ª Rodada",42),
                                          rep("2011 - 9ª Rodada",42),rep("2011 - 10ª Rodada",42),
                                          rep("2011 - 11ª Rodada",42),rep("2011 - 12ª Rodada",42),
                                          rep("2011 - 13ª Rodada",42),rep("2011 - 14ª Rodada",42),
                                          rep("2011 - 15ª Rodada",42),rep("2011 - 16ª Rodada",42),
                                          rep("2011 - 17ª Rodada",42),rep("2011 - 18ª Rodada",42),
                                          rep("2011 - 19ª Rodada",42),rep("2011 - 20ª Rodada",42),
                                          rep("2011 - 21ª Rodada",42),rep("2011 - 22ª Rodada",42),
                                          rep("2011 - 23ª Rodada",42),rep("2011 - 24ª Rodada",42),
                                          rep("2011 - 25ª Rodada",42),rep("2011 - 26ª Rodada",42),
                                          rep("2011 - 27ª Rodada",42),rep("2011 - 28ª Rodada",42),
                                          rep("2011 - 29ª Rodada",42),rep("2011 - 30ª Rodada",42),
                                          rep("2011 - 31ª Rodada",42),rep("2011 - 32ª Rodada",42),
                                          rep("2011 - 33ª Rodada",42),rep("2011 - 34ª Rodada",42),
                                          rep("2011 - 35ª Rodada",42),rep("2011 - 36ª Rodada",42),
                                          rep("2011 - 37ª Rodada",42),rep("2011 - 38ª Rodada",42),
                                          rep("2012 - 1ª Rodada",42),rep("2012 - 2ª Rodada",42),
                                          rep("2012 - 3ª Rodada",42),rep("2012 - 4ª Rodada",42),
                                          rep("2012 - 5ª Rodada",42),rep("2012 - 6ª Rodada",42),
                                          rep("2012 - 7ª Rodada",42),rep("2012 - 8ª Rodada",42),
                                          rep("2012 - 9ª Rodada",42),rep("2012 - 10ª Rodada",42),
                                          rep("2012 - 11ª Rodada",42),rep("2012 - 12ª Rodada",42),
                                          rep("2012 - 13ª Rodada",42),rep("2012 - 14ª Rodada",42),
                                          rep("2012 - 15ª Rodada",42),rep("2012 - 16ª Rodada",42),
                                          rep("2012 - 17ª Rodada",42),rep("2012 - 18ª Rodada",42),
                                          rep("2012 - 19ª Rodada",42),rep("2012 - 20ª Rodada",42),
                                          rep("2012 - 21ª Rodada",42),rep("2012 - 22ª Rodada",42),
                                          rep("2012 - 23ª Rodada",42),rep("2012 - 24ª Rodada",42),
                                          rep("2012 - 25ª Rodada",42),rep("2012 - 26ª Rodada",42),
                                          rep("2012 - 27ª Rodada",42),rep("2012 - 28ª Rodada",42),
                                          rep("2012 - 29ª Rodada",42),rep("2012 - 30ª Rodada",42),
                                          rep("2012 - 31ª Rodada",42),rep("2012 - 32ª Rodada",42),
                                          rep("2012 - 33ª Rodada",42),rep("2012 - 34ª Rodada",42),
                                          rep("2012 - 35ª Rodada",42),rep("2012 - 36ª Rodada",42),
                                          rep("2012 - 37ª Rodada",42),rep("2012 - 38ª Rodada",42),
                                          rep("2013 - 1ª Rodada",42),rep("2013 - 2ª Rodada",42),
                                          rep("2013 - 3ª Rodada",42),rep("2013 - 4ª Rodada",42),
                                          rep("2013 - 5ª Rodada",42),rep("2013 - 6ª Rodada",42),
                                          rep("2013 - 7ª Rodada",42),rep("2013 - 8ª Rodada",42),
                                          rep("2013 - 9ª Rodada",42),rep("2013 - 10ª Rodada",42),
                                          rep("2013 - 11ª Rodada",42),rep("2013 - 12ª Rodada",42),
                                          rep("2013 - 13ª Rodada",42),rep("2013 - 14ª Rodada",42),
                                          rep("2013 - 15ª Rodada",42),rep("2013 - 16ª Rodada",42),
                                          rep("2013 - 17ª Rodada",42),rep("2013 - 18ª Rodada",42),
                                          rep("2013 - 19ª Rodada",42),rep("2013 - 20ª Rodada",42),
                                          rep("2013 - 21ª Rodada",42),rep("2013 - 22ª Rodada",42),
                                          rep("2013 - 23ª Rodada",42),rep("2013 - 24ª Rodada",42),
                                          rep("2013 - 25ª Rodada",42),rep("2013 - 26ª Rodada",42),
                                          rep("2013 - 27ª Rodada",42),rep("2013 - 28ª Rodada",42),
                                          rep("2013 - 29ª Rodada",42),rep("2013 - 30ª Rodada",42),
                                          rep("2013 - 31ª Rodada",42),rep("2013 - 32ª Rodada",42),
                                          rep("2013 - 33ª Rodada",42),rep("2013 - 34ª Rodada",42),
                                          rep("2013 - 35ª Rodada",42),rep("2013 - 36ª Rodada",42),
                                          rep("2013 - 37ª Rodada",42),rep("2013 - 38ª Rodada",42),
                                          rep("2014 - 1ª Rodada",42),rep("2014 - 2ª Rodada",42),
                                          rep("2014 - 3ª Rodada",42),rep("2014 - 4ª Rodada",42),
                                          rep("2014 - 5ª Rodada",42),rep("2014 - 6ª Rodada",42),
                                          rep("2014 - 7ª Rodada",42),rep("2014 - 8ª Rodada",42),
                                          rep("2014 - 9ª Rodada",42),rep("2014 - 10ª Rodada",42),
                                          rep("2014 - 11ª Rodada",42),rep("2014 - 12ª Rodada",42),
                                          rep("2014 - 13ª Rodada",42),rep("2014 - 14ª Rodada",42),
                                          rep("2014 - 15ª Rodada",42),rep("2014 - 16ª Rodada",42),
                                          rep("2014 - 17ª Rodada",42),rep("2014 - 18ª Rodada",42),
                                          rep("2014 - 19ª Rodada",42),rep("2014 - 20ª Rodada",42),
                                          rep("2014 - 21ª Rodada",42),rep("2014 - 22ª Rodada",42),
                                          rep("2014 - 23ª Rodada",42),rep("2014 - 24ª Rodada",42),
                                          rep("2014 - 25ª Rodada",42),rep("2014 - 26ª Rodada",42),
                                          rep("2014 - 27ª Rodada",42),rep("2014 - 28ª Rodada",42),
                                          rep("2014 - 29ª Rodada",42),rep("2014 - 30ª Rodada",42),
                                          rep("2014 - 31ª Rodada",42),rep("2014 - 32ª Rodada",42),
                                          rep("2014 - 33ª Rodada",42),rep("2014 - 34ª Rodada",42),
                                          rep("2014 - 35ª Rodada",42),rep("2014 - 36ª Rodada",42),
                                          rep("2014 - 37ª Rodada",42),rep("2014 - 38ª Rodada",42),
                                          rep("2015 - 1ª Rodada",42),rep("2015 - 2ª Rodada",42),
                                          rep("2015 - 3ª Rodada",42),rep("2015 - 4ª Rodada",42),
                                          rep("2015 - 5ª Rodada",42),rep("2015 - 6ª Rodada",42),
                                          rep("2015 - 7ª Rodada",42),rep("2015 - 8ª Rodada",42),
                                          rep("2015 - 9ª Rodada",42),rep("2015 - 10ª Rodada",42),
                                          rep("2015 - 11ª Rodada",42),rep("2015 - 12ª Rodada",42),
                                          rep("2015 - 13ª Rodada",42),rep("2015 - 14ª Rodada",42),
                                          rep("2015 - 15ª Rodada",42),rep("2015 - 16ª Rodada",42),
                                          rep("2015 - 17ª Rodada",42),rep("2015 - 18ª Rodada",42),
                                          rep("2015 - 19ª Rodada",42),rep("2015 - 20ª Rodada",42),
                                          rep("2015 - 21ª Rodada",42),rep("2015 - 22ª Rodada",42),
                                          rep("2015 - 23ª Rodada",42),rep("2015 - 24ª Rodada",42),
                                          rep("2015 - 25ª Rodada",42),rep("2015 - 26ª Rodada",42),
                                          rep("2015 - 27ª Rodada",42),rep("2015 - 28ª Rodada",42),
                                          rep("2015 - 29ª Rodada",42),rep("2015 - 30ª Rodada",42),
                                          rep("2015 - 31ª Rodada",42),rep("2015 - 32ª Rodada",42),
                                          rep("2015 - 33ª Rodada",42),rep("2015 - 34ª Rodada",42),
                                          rep("2015 - 35ª Rodada",42),rep("2015 - 36ª Rodada",42),
                                          rep("2015 - 37ª Rodada",42),rep("2015 - 38ª Rodada",42),
                                          rep("2016 - 1ª Rodada",42),rep("2016 - 2ª Rodada",42),
                                          rep("2016 - 3ª Rodada",42),rep("2016 - 4ª Rodada",42),
                                          rep("2016 - 5ª Rodada",42),rep("2016 - 6ª Rodada",42),
                                          rep("2016 - 7ª Rodada",42),rep("2016 - 8ª Rodada",42),
                                          rep("2016 - 9ª Rodada",42),rep("2016 - 10ª Rodada",42),
                                          rep("2016 - 11ª Rodada",42),rep("2016 - 12ª Rodada",42),
                                          rep("2016 - 13ª Rodada",42),rep("2016 - 14ª Rodada",42),
                                          rep("2016 - 15ª Rodada",42),rep("2016 - 16ª Rodada",42),
                                          rep("2016 - 17ª Rodada",42),rep("2016 - 18ª Rodada",42),
                                          rep("2016 - 19ª Rodada",42),rep("2016 - 20ª Rodada",42),
                                          rep("2016 - 21ª Rodada",42),rep("2016 - 22ª Rodada",42),
                                          rep("2016 - 23ª Rodada",42),rep("2016 - 24ª Rodada",42),
                                          rep("2016 - 25ª Rodada",42),rep("2016 - 26ª Rodada",42),
                                          rep("2016 - 27ª Rodada",42),rep("2016 - 28ª Rodada",42),
                                          rep("2016 - 29ª Rodada",42),rep("2016 - 30ª Rodada",42),
                                          rep("2016 - 31ª Rodada",42),rep("2016 - 32ª Rodada",42),
                                          rep("2016 - 33ª Rodada",42),rep("2016 - 34ª Rodada",42),
                                          rep("2016 - 35ª Rodada",42),rep("2016 - 36ª Rodada",42),
                                          rep("2016 - 37ª Rodada",42),rep("2016 - 38ª Rodada",42),
                                          rep("2017 - 1ª Rodada",42),rep("2017 - 2ª Rodada",42),
                                          rep("2017 - 3ª Rodada",42),rep("2017 - 4ª Rodada",42),
                                          rep("2017 - 5ª Rodada",42),rep("2017 - 6ª Rodada",42),
                                          rep("2017 - 7ª Rodada",42),rep("2017 - 8ª Rodada",42),
                                          rep("2017 - 9ª Rodada",42),rep("2017 - 10ª Rodada",42),
                                          rep("2017 - 11ª Rodada",42),rep("2017 - 12ª Rodada",42),
                                          rep("2017 - 13ª Rodada",42),rep("2017 - 14ª Rodada",42),
                                          rep("2017 - 15ª Rodada",42),rep("2017 - 16ª Rodada",42),
                                          rep("2017 - 17ª Rodada",42),rep("2017 - 18ª Rodada",42),
                                          rep("2017 - 19ª Rodada",42),rep("2017 - 20ª Rodada",42),
                                          rep("2017 - 21ª Rodada",42),rep("2017 - 22ª Rodada",42),
                                          rep("2017 - 23ª Rodada",42),rep("2017 - 24ª Rodada",42),
                                          rep("2017 - 25ª Rodada",42),rep("2017 - 26ª Rodada",42),
                                          rep("2017 - 27ª Rodada",42),rep("2017 - 28ª Rodada",42),
                                          rep("2017 - 29ª Rodada",42),rep("2017 - 30ª Rodada",42),
                                          rep("2017 - 31ª Rodada",42),rep("2017 - 32ª Rodada",42),
                                          rep("2017 - 33ª Rodada",42),rep("2017 - 34ª Rodada",42),
                                          rep("2017 - 35ª Rodada",42),rep("2017 - 36ª Rodada",42),
                                          rep("2017 - 37ª Rodada",42),rep("2017 - 38ª Rodada",42),
                                          rep("2018 - 1ª Rodada",42),rep("2018 - 2ª Rodada",42),
                                          rep("2018 - 3ª Rodada",42),rep("2018 - 4ª Rodada",42),
                                          rep("2018 - 5ª Rodada",42),rep("2018 - 6ª Rodada",42),
                                          rep("2018 - 7ª Rodada",42),rep("2018 - 8ª Rodada",42),
                                          rep("2018 - 9ª Rodada",42),rep("2018 - 10ª Rodada",42),
                                          rep("2018 - 11ª Rodada",42),rep("2018 - 12ª Rodada",42),
                                          rep("2018 - 13ª Rodada",42),rep("2018 - 14ª Rodada",42),
                                          rep("2018 - 15ª Rodada",42),rep("2018 - 16ª Rodada",42),
                                          rep("2018 - 17ª Rodada",42),rep("2018 - 18ª Rodada",42),
                                          rep("2018 - 19ª Rodada",42),rep("2018 - 20ª Rodada",42),
                                          rep("2018 - 21ª Rodada",42),rep("2018 - 22ª Rodada",42),
                                          rep("2018 - 23ª Rodada",42),rep("2018 - 24ª Rodada",42),
                                          rep("2018 - 25ª Rodada",42),rep("2018 - 26ª Rodada",42),
                                          rep("2018 - 27ª Rodada",42),rep("2018 - 28ª Rodada",42),
                                          rep("2018 - 29ª Rodada",42),rep("2018 - 30ª Rodada",42),
                                          rep("2018 - 31ª Rodada",42),rep("2018 - 32ª Rodada",42),
                                          rep("2018 - 33ª Rodada",42),rep("2018 - 34ª Rodada",42),
                                          rep("2018 - 35ª Rodada",42),rep("2018 - 36ª Rodada",42),
                                          rep("2018 - 37ª Rodada",42),rep("2018 - 38ª Rodada",42),
                                          rep("2019 - 1ª Rodada",42),rep("2019 - 2ª Rodada",42),
                                          rep("2019 - 3ª Rodada",42),rep("2019 - 4ª Rodada",42),
                                          rep("2019 - 5ª Rodada",42),rep("2019 - 6ª Rodada",42),
                                          rep("2019 - 7ª Rodada",42),rep("2019 - 8ª Rodada",42),
                                          rep("2019 - 9ª Rodada",42),rep("2019 - 10ª Rodada",42),
                                          rep("2019 - 11ª Rodada",42),rep("2019 - 12ª Rodada",42),
                                          rep("2019 - 13ª Rodada",42),rep("2019 - 14ª Rodada",42),
                                          rep("2019 - 15ª Rodada",42),rep("2019 - 16ª Rodada",42),
                                          rep("2019 - 17ª Rodada",42),rep("2019 - 18ª Rodada",42),
                                          rep("2019 - 19ª Rodada",42),rep("2019 - 20ª Rodada",42),
                                          rep("2019 - 21ª Rodada",42),rep("2019 - 22ª Rodada",42),
                                          rep("2019 - 23ª Rodada",42),rep("2019 - 24ª Rodada",42),
                                          rep("2019 - 25ª Rodada",42),rep("2019 - 26ª Rodada",42),
                                          rep("2019 - 27ª Rodada",42),rep("2019 - 28ª Rodada",42),
                                          rep("2019 - 29ª Rodada",42),rep("2019 - 30ª Rodada",42),
                                          rep("2019 - 31ª Rodada",42),rep("2019 - 32ª Rodada",42),
                                          rep("2019 - 33ª Rodada",42),rep("2019 - 34ª Rodada",42),
                                          rep("2019 - 35ª Rodada",42),rep("2019 - 36ª Rodada",42),
                                          rep("2019 - 37ª Rodada",42),rep("2019 - 38ª Rodada",42)
                                          ))



colnames(tabela_2)[1] <- "Clube"
colnames(tabela_2)[2] <- "Pontos"
colnames(tabela_2)[3] <- "Ano_rodada"



tabela <- tabela %>%
     select(Clube,Pontos,Ano_rodada)


  
tabela <- rbind.data.frame(tabela,tabela_2)

tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 1ª Rodada", "- 01ª Rodada")
tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 2ª Rodada", "- 02ª Rodada")
tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 3ª Rodada", "- 03ª Rodada")
tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 4ª Rodada", "- 04ª Rodada")
tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 5ª Rodada", "- 05ª Rodada")
tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 6ª Rodada", "- 06ª Rodada")
tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 7ª Rodada", "- 07ª Rodada")
tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 8ª Rodada", "- 08ª Rodada")
tabela$Ano_rodada <- str_replace(tabela$Ano_rodada, "- 9ª Rodada", "- 09ª Rodada")





tabela <- tabela %>%
  arrange(Ano_rodada) 


  

tabela$Pontos <- as.numeric(tabela$Pontos)





tabela_teste <- group_by(tabela,Clube) %>%
     summarise(PontosAcumulados =cumsum(Pontos), Ano_rodada = Ano_rodada)


tabela_final <- tabela_teste %>%
  arrange(Ano_rodada)

tabela_final <- distinct(tabela_final)

# generate top n ranking by year group

anim_tabela_brasileiro <- tabela_final %>%
  dplyr::group_by(Ano_rodada) %>%
  dplyr::mutate(
    rank = row_number(-PontosAcumulados),
    Value_rel = PontosAcumulados / PontosAcumulados,
    Value_lbl = paste0(" ", PontosAcumulados)
  ) %>%
  dplyr::filter(rank <= 30) 



# create animated bar chart
library("ggplot2")
library("gganimate")
library("av")


p <- ggplot2::ggplot(anim_tabela_brasileiro, aes(rank)) +
  ggplot2::geom_tile(aes(
    y = PontosAcumulados / 2,
    height = PontosAcumulados,
    width = 0.9, fill = "darkred"
  ), alpha = 0.8, color = NA) +
  ggplot2::geom_text(aes(y = 0, label = paste(Clube, " ")), size = 8, vjust = 0.2, hjust = 1) +
  ggplot2::geom_text(aes(y = PontosAcumulados, label = Value_lbl, hjust = 0), size = 8) +
  ggplot2::coord_flip(clip = "off", expand = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_reverse() +
  ggplot2::guides(color = FALSE, fill = FALSE) +
  ggplot2::labs(
    title = "Pontos Acumulados Brasileirão Série A (2003-2019)", x = "", y = "",
    subtitle = "{closest_state}",
    caption = "Tabela: https://github.com/adaoduque/Brasileirao_Dataset. Elaborado: Vinícius Godinho"
  ) +
  ggplot2::theme(title = element_text(size=30),
                 axis.ticks.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.margin = margin(2, 2, 1, 16, "cm")
  ) +
  gganimate::transition_states(Ano_rodada, transition_length = 4, state_length = 1) +
  gganimate::ease_aes()

# save as preferred rendered format

gganimate::animate(p, 200, fps = 12, duration = 140, width = 2000, height = 1200, renderer = av_renderer("anim_brasileiro.mp4"))



