#########LAPOP2023#####


# Retirando os labels de todas as variáveis do dataframe
BASECJLAPOP2023 <- zap_labels(BASECJLAPOP2023)


#Pacotes####
library(haven)
library(readstata13)
library(tidyverse)
library(survey)
library(Rmisc)
library(broom)
library(psych)
library(writexl)
library(sjPlot)
library(olsrr)
library(coefplot)

BRA_2023_LAPOP_AmericasBarometer_v1_0_w
str(BRA_2023_LAPOP_AmericasBarometer_v1_0_w)
colnames(BRA_2023_LAPOP_AmericasBarometer_v1_0_w)

Lapop <- BRA_2023_LAPOP_AmericasBarometer_v1_0_w

table(Lapop$m1)
# 1   2   3   4   5 
# 165 506 523 107 213 

#Labels:
#   value                      label
# 1                  Muito bom
# 2                        Bom
# 3 Nem bom, nem mau (regular)
# 4                        Mau
# 5        Muito mau (péssimo)
# NA(a)                   Não sabe
# NA(b)               Não responde

# br21 <- read.dta13("BRA_2021_LAPOP_AmericasBarometer_v1.2_w.dta", #readstata13
#                    convert.factors = FALSE)
# BRA_2023_LAPOP_AmericasBarometer_v1_0_w

lapop2 <- read.dta13("BRA_2023_LAPOP_AmericasBarometer_v1.0_w.dta",
                     convert.factors = TRUE)

BASECJLAPOP2023 <- bind_cols(Lapop, lapop2)


############################
#IDEIA NAIARA
#PRIMEIRO FAZER O TESTE DE REGRESSÃO 
#DEPOIS SOMAR AVALIAÇÃO PRESIDENCIAL COM RELIGIÃO
#OU FAZER UM CRUZAMENTO ENTRE AVALIAÇÃO PRESIDENCIAL E VOTAÇÃO
#######################################################


#salvando

writexl::write_xlsx(BASECJLAPOP2023, path = "BASECJLAPOP2023.xlsx")


######################################
#INÍCIO DAS ANÁLISES 

#VD#####
#2023
# VARIÁVEIS DEPENDENTE 
# VB2 O(A) sr./sra. votou no primeiro turno nas últimas eleições 
#presidenciais de 2022?
#   1 Sim, votou [Continue]
# 2 Não votou [Vá para VB10]
# 888888 Não sabe [NÃO LER] [Vá para VB10]
# 988888 Não responde [NÃO LER] [Vá para VB10]
# 
# C.O.X.CC.1500.X.18 
# VB3N Em qual dos candidatos o(a) sr./sra. votou para 
#presidente no primeiro turno das eleições presidenciais de 2022?
#   [NÃO LER ALTERNATIVAS] 
# 
# 00 Nenhum (foi votar, mas votou em branco)
# 97 Nenhum (foi votar, mas anulou seu voto)
# 1501 Luiz Inácio Lula da Silva – PT
# 1502 Jair Bolsonaro – PL
# 1503 Simone Tebet – MDB
# 1504 Ciro Gomes – PDT
# 1577 Outro
# 888888 Não sabe [NÃO LER] 
# 988888 Não responde [NÃO LER] 
# 999999 Não se aplica [NÃO LER] 
# 
# 

table(BASECJLAPOP2023$vb2...297)
# Sim, votou    Não votou     Não sabe Não responde 
# 1199          325            0            0 

BASECJLAPOP2023$vb2...93 <- as.character(BASECJLAPOP2023$vb2...93)
table(BASECJLAPOP2023$vb2...93)
# 1    2 
# 1199  325 

table(BASECJLAPOP2023$vb3n...298)
# Nenhum (foi votar, mas votou em branco) Nenhum (foi votar, mas anulou seu voto) 
# 28                                      23 
# Luiz Inácio Lula da Silva - PT                     Jair Bolsonaro - PL 
# 541                                     405 
# Simone Tebet - MDB                        Ciro Gomes - PDT 
# 17                                      26 
# Outro                                Não sabe 
# 14                                       0 
# Não responde                           Não se aplica 
# 0                                       0 
# > 

table(BASECJLAPOP2023$vb3n...94)
#  0   97 1501 1502 1503 1504 1577 
# 28   23  541  405   17   26   14 

#recodificação
# As duas transformar em 
# 0 não votou (2)
# 1 Anulou (97 ) ou votou em branco (0)
# 2 votou em outro candidato (1577, 1503, 1504 )
# 3 NS/NR
# 4 votou em lula (1501)
# 5 votou em bolsonaro


BASECJLAPOP2023 <- BASECJLAPOP2023 %>% 
  mutate(Votacao = case_when(
    vb2...93 == 2 ~ 0,
    vb3n...94 %in% c(97, 0) ~ 1,
    vb3n...94 %in% c(1577, 1503, 1504)  ~ 2,
    vb3n...94 == 1501 ~ 4,
    vb3n...94 == 1502 ~ 5,
    TRUE ~ 3
  ))
table(BASECJLAPOP2023$Votacao)
#  0   1   2   3   4   5 
325+ 51 +57+147+541 +405
#[1] 1526

#recategorizar nominalmente
BASECJLAPOP2023 <- BASECJLAPOP2023 %>% 
  mutate(VotacaoCat = case_when(
    Votacao == 0 ~ "Não votou",
    Votacao == 1 ~ "Nulo/branco",
    Votacao == 2 ~ "Outro",
    Votacao == 3 ~ "NS/NR",
    Votacao == 4 ~ "Lula",
    Votacao == 5 ~ "Bolsonaro"
  ))

table(BASECJLAPOP2023$VotacaoCat)
# Bolsonaro        Lula   Não votou       NS/NR Nulo/branco       Outro 
# 405         541         325         147          51          57 



##################################
#FAZER A DESCRIÇÃO DESSAS VARIÁVEIS############

########################
#VI####
#AVALIAÇÃO PRESIDENCIAL####
table(BASECJLAPOP2023$m1...256) #Avaliação Lula 2023
# 
# Muito bom                  Bom              Nem bom, nem mau (regular) 
# 165                        506                        523 
# Mau        Muito mau (péssimo)                   Não sabe 
# 107                        213                          0 
# Não responde 
# 0 

table(BASECJLAPOP2023$m1...52)
# 1   2   3   4   5 
# 165 506 523 107 213 

#recategorizar nominalmente
BASECJLAPOP2023 <- BASECJLAPOP2023 %>% 
  mutate(AvaliacaoPres. = case_when(
    m1...52 %in% c(1,2,3) ~ 0,
    m1...52 %in% c(4, 5) ~ 1
  ))

table(BASECJLAPOP2023$AvaliacaoPres.) #bom #ruim
# 0    1 
# 1194  320 

#1Análise fatorial##### 

#ANÁLISE FATORIAL DAS VARIÁVEIS DE TOLERÂNCIA
#PARA CRIAR UM ÍNDICE DE TOLERÂNCIA todos são escalas de 1 a 10
# BASECJLAPOP2023$e5...69 #Aprovação de participação em manifestações legais
# BASECJLAPOP2023$e17a...70#Aprovação de protestos por grupos que apoiam direitos das mulheres
# BASECJLAPOP2023$e17b...71#Que grupos feministas têm o direito de participar de protestos
# 
# BASECJLAPOP2023$d3...72#Aprovação de críticos do governo para se candidatar
# BASECJLAPOP2023$d4...73#Aprovação de críticos do governo para discursar
# BASECJLAPOP2023$d5...74*#Aprovação dos direitos dos homosexuais a se candidatarem
# 
# BASECJLAPOP2023$d6...75*#Aprovação do casamento entre pessoas homosexuais
# BASECJLAPOP2023$d5newa...76*#Aprovação de direitos iguais para minorias sexuais
# BASECJLAPOP2023$d7a...77*#Aprovação de adoção de crianças por casais homosexuais
# 
# BASECJLAPOP2023$d7b...79*#Aprovação de adoção de crianças por minorias de gênero
# BASECJLAPOP2023$d5newb...78*#Aprovação de direitos iguais para minorias de gênero


#Análise fatorial 
fa <- BASECJLAPOP2023[c("e5...69","e17a...70", "e17b...71", 
                        "d3...72","d4...73", "d5...74", 
                        "d6...75", "d5newa...76","d7a...77", 
                        "d7b...79", "d5newb...78")]

summary(fa)


# Instale o pacote missMDA se ainda não estiver instalado
#install.packages("missMDA")

# Carregue o pacote missMDA
library(missMDA)

# Impute os valores ausentes usando a função imputePCA
fa_imputado <- imputePCA(fa)

# Realize a análise fatorial no conjunto de dados imputado
resultado_fatorial <- fa.parallel(fa_imputado$completeObs)


# Realizar a análise fatorial
resultado_fatorial <- fa(fa_imputado$completeObs)

# Extrair as cargas fatoriais
cargas_fatoriais <- resultado_fatorial$loadings

# Visualizar as cargas fatoriais
print(cargas_fatoriais)

# 
# Loadings:
#   MR1  
# e5...69     0.398---
# e17a...70   0.652---
# e17b...71   0.665---
# d3...72     0.270---
# d4...73     0.284---
# d5...74     0.798
# d6...75     0.776
# d5newa...76 0.854
# d7a...77    0.825
# d7b...79    0.825
# d5newb...78 0.875
# 
# MR1
# SS loadings    5.275
# Proportion Var 0.480
# Criar um índice com base nas variáveis com cargas fatoriais elevadas no fator MR1
Ind.TolSex <- fa_imputado$completeObs[, c("d5...74", "d6...75", 
                                      "d5newa...76", "d7a...77", 
                                      "d7b...79", "d5newb...78")]

# Calcular a média das variáveis para criar o índice
Ind.TolSex <- rowMeans(Ind.TolSex, na.rm = TRUE)

summary(Ind.TolSex)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.088   4.940   7.464   6.974   9.628  10.149 

# Criar um índice com base nas variáveis com 
#cargas fatoriais elevadas no fator MR1
Ind.TolSex <- rowMeans(fa_imputado$completeObs[, c("d5...74", "d6...75", 
                                               "d5newa...76", "d7a...77", 
                                               "d7b...79", "d5newb...78")], 
                                                   na.rm = TRUE)



# Adicionar o índice como uma nova variável à base de dados
BASECJLAPOP2023$Ind.TolSex <- Ind.TolSex

# Selecione as variáveis que não foram incluídas no primeiro índice
Ind.toldemocratica <- fa_imputado$completeObs[, c("e5...69", "e17a...70",
                                                "e17b...71", "d3...72", 
                                                "d4...73")]

# Calcular o segundo índice
Ind.toldemocratica <- rowMeans(Ind.toldemocratica, na.rm = TRUE)

# Adicionar o segundo índice como uma nova variável à base de dados
BASECJLAPOP2023$Ind.toldemocratica <- Ind.toldemocratica


summary(BASECJLAPOP2023$Ind.Tol.Geral)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.8674  4.9988  6.3057  6.2940  7.7033 10.1304

##################

#2Análise fatorial##### 
fabd <- BASECJLAPOP2023[c("b47a...255", "b37...254","b12...245", 
             "b13...246", "b18...247", "b20...248", 
             "b20a...249", "b21...250", "b21a...251",
             "b31...252", "b32...253")]

fa.parallel(fabd)

#Parallel analysis suggests that the number of factors =  4  and the number of components =  3 

fa.r <- principal(fabd, nfactors =3 ,n.obs = 1526)

print(fa.r$loadings, cutoff = 0.45)

summary(BASECJLAPOP2023$b47a...255)

# Loadings:
#                RC1    RC2    RC3   
# b47a...255  0.761              
# b37...254          0.529       
# b12...245          0.750       
# b13...246   0.625  0.474       
# b18...247          0.684       
# b20...248                 0.854
# b20a...249                0.621
# b21...250   0.622              
# b21a...251  0.877              
# b31...252   0.834              
# b32...253          0.539       
# 
# RC1   RC2   RC3
# SS loadings    3.231 2.233 1.322
# Proportion Var 0.294 0.203 0.120
# Cumulative Var 0.294 0.497 0.617

#Inserir esse Índice de Confiança Institucional 
#na base de dados


alpha(fabd[c("b47a...255", "b13...246", 
             "b21...250", "b21a...251",
             "b31...252")], cumulative = F)


#####Recodificação de Variáveis####
######Índice de conf institucional########

BASECJLAPOP2023$Ind.ConfInts <- BASECJLAPOP2023$b47a...255 + 
  BASECJLAPOP2023$b13...246 + BASECJLAPOP2023$b21...250 + 
  BASECJLAPOP2023$b21a...251 +
  BASECJLAPOP2023$b31...252


# Verificar o resumo estatístico dos valores do índice
summary(BASECJLAPOP2023$Ind.ConfInts)

# Remover valores ausentes
cleaned_values <- na.omit(BASECJLAPOP2023$Ind.ConfInts)


# Normalizar os valores do índice para a faixa de 0 a 1
min_val <- min(BASECJLAPOP2023$Ind.ConfInts, na.rm = TRUE)
max_val <- max(BASECJLAPOP2023$Ind.ConfInts, na.rm = TRUE)
scaled_values <- scale(cleaned_values, center = min_val, 
                       scale = max_val - min_val)

# Reescalar os valores normalizados para a faixa de 1 a 10
min_range <- 0
max_range <- 10


# Substituir os valores originais pelo índice normalizado e reescalado
BASECJLAPOP2023$Ind.ConfInts[!is.na(BASECJLAPOP2023$Ind.ConfInts)] <- 
  rescaled_values

# Verificar o resumo estatístico dos valores do índice
summary(BASECJLAPOP2023$Ind.ConfInts)

######Índice de Confiança nos Meios de Comunicação:########

BASECJLAPOP2023$Ind.ConfMeiosCom <- BASECJLAPOP2023$b37...254 + 
  BASECJLAPOP2023$b12...245 + BASECJLAPOP2023$b18...247 + 
  BASECJLAPOP2023$b32...253


# Verificar o resumo estatístico dos valores do índice
summary(BASECJLAPOP2023$Ind.ConfMeiosCom)

# Remover valores ausentes
cleaned_values <- na.omit(BASECJLAPOP2023$Ind.ConfMeiosCom)


# Normalizar os valores do índice para a faixa de 0 a 1
min_val <- min(BASECJLAPOP2023$Ind.ConfMeiosCom, na.rm = TRUE)
max_val <- max(BASECJLAPOP2023$Ind.ConfMeiosCom, na.rm = TRUE)
scaled_values <- scale(cleaned_values, center = min_val, 
                       scale = max_val - min_val)

# Reescalar os valores normalizados para a faixa de 1 a 10
min_range <- 0
max_range <- 10


# Substituir os valores originais pelo índice normalizado e reescalado
BASECJLAPOP2023$Ind.ConfMeiosCom[!is.na(BASECJLAPOP2023$Ind.ConfMeiosCom)] <- 
  rescaled_values

# Verificar o resumo estatístico dos valores do índice
summary(BASECJLAPOP2023$Ind.ConfInts)
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   2.000   4.333   4.290   6.333  10.000      59 



######Índice de Confiança Religiosa:########

BASECJLAPOP2023$Ind.ConfRelig <- BASECJLAPOP2023$b20...248 + 
  BASECJLAPOP2023$b20a...249 


# Verificar o resumo estatístico dos valores do índice
summary(BASECJLAPOP2023$Ind.ConfRelig)

# Remover valores ausentes
cleaned_values <- na.omit(BASECJLAPOP2023$Ind.ConfRelig)


# Normalizar os valores do índice para a faixa de 0 a 1
min_val <- min(BASECJLAPOP2023$Ind.ConfRelig, na.rm = TRUE)
max_val <- max(BASECJLAPOP2023$Ind.ConfRelig, na.rm = TRUE)
scaled_values <- scale(cleaned_values, center = min_val, 
                       scale = max_val - min_val)

# Reescalar os valores normalizados para a faixa de 1 a 10
min_range <- 0
max_range <- 10


# Substituir os valores originais pelo índice normalizado e reescalado
BASECJLAPOP2023$Ind.ConfRelig[!is.na(BASECJLAPOP2023$Ind.ConfRelig)] <- 
  rescaled_values

# Verificar o resumo estatístico dos valores do índice
summary(BASECJLAPOP2023$Ind.ConfRelig)
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.000   2.000   4.333   4.285   6.333  10.000      41 




# A FAZER  

# *Depois disso tenho recortar o banco de dados 
# *Escrever no trabalho como foi feito a criação dos índices 
# *Colocar as análises descritivas
# *ROdar o teste de regressão




################

# C.O.X.SW.1500.X.21 
# B47A Até que ponto o(a) sr./sra. tem confiança nas eleições neste 
# país?
# 1 Nada
# 2 
# 3 
# 4 
# 5 
# 6 
# 7 Muito
# 888888 Não sabe [NÃO LER] 
# 988888 Não responde [NÃO LER] 
# 
# C.O.X.SW.1500.X.21 
# B37 Até que ponto o(a) sr./sra. tem confiança nos meios de comunicação?
#   1 Nada
# 2 
# 3 
# 4 
# 5 
# 6 
# 7 Muito
# 888888 Não sabe [NÃO LER] 
# 988888 Não responde [NÃO LER] 
# 
# 
# MONTAR UM ÍNDICE DE CONFIANÇA NAS INSTITUIÇÕES
# B12 Até que ponto o(a) sr./sra. tem confiança nas Forças Armadas 
# [o Exército]?
#   B13 Até que ponto o(a) sr./sra. tem confiança no Congresso Nacional?
#   B18 Até que ponto o(a) sr./sra. tem confiança na Polícia Militar?
#   B20 Até que ponto o(a) sr./sra. tem confiança na Igreja Católica?
#   B20A Até que ponto o(a) sr./sra. tem confiança na Igreja Evangélica?
#   B21 Até que ponto o(a) sr./sra. tem confiança nos partidos políticos?
#   #
#   ÍNDICE DE CONFIANÇA NOS PODERES
# B21A Até que ponto o(a) sr./sra. tem confiança no Presidente da República?
#   B31 Até que ponto o(a) sr./sra. tem confiança no Supremo Tribunal Federal?
#   B32 
# Até que ponto o(a) sr./sra. tem confiança na Prefeitura do seu município ? 
#   #
#   B37 Até que ponto o(a) sr./sra. tem confiança nos meios de 
# comunicação?
#   B47A Até que ponto o(a) sr./sra. tem confiança nas eleições neste 
# país?
  
#####

#Desafeição partidária####

table(BASECJLAPOP2023$vb10...95)
# 1    2 
# 344 1173 

344 + 1173 #[1] 1517

# VB10 Atualmente o(a) sr./sra. 
# simpatiza com algum partido 
# político?

table(BASECJLAPOP2023$vb10...299)
# Sim          Não     Não sabe Não responde 
# 344         1173            0            0 

BASECJLAPOP2023$DesafPart. <- BASECJLAPOP2023$vb10...299

#CONTROLE####

#Idade####
summary(BASECJLAPOP2023$q2...16)
BASECJLAPOP2023$Idade <- BASECJLAPOP2023$q2...16

#Gênero####
table(BASECJLAPOP2023$q1tc_r...219)
# 1masc.   2fem   3outro. 
# 706 718  11


#recategorizar nominalmente
table(BASECJLAPOP2023$q1tc_r...15)

BASECJLAPOP2023 <- BASECJLAPOP2023 %>% 
  mutate(Sexo = case_when(
    q1tc_r...15 == 3 ~ 0,
    q1tc_r...15 == 2 ~ 2,
    q1tc_r...15 == 1 ~ 1
  ))

table(BASECJLAPOP2023$Sexo)
# 0   1   2 
# 11 706 718 



#Escolaridade####
table(BASECJLAPOP2023$edre...158)
# 0   1   2   3   4   5   6 
# 43 302 158 233 509 115 161 

BASECJLAPOP2023$Escolaridade <- BASECJLAPOP2023$edre...158

#Renda#### 
table(BASECJLAPOP2023$q10inc...368)
table(BASECJLAPOP2023$q10inc...164)
BASECJLAPOP2023$RendaNum <- BASECJLAPOP2023$q10inc...164
BASECJLAPOP2023$Renda <- BASECJLAPOP2023$q10inc...368
#Testando para maior renda


#Religião ####
#DENOMINAÇAO Q3CN
#Q3CN Qual é sua religião, se tiver?
# 01 Católico 
# 02 Protestante Tradicional ou 
# Evangélica não pentecostal 
# 03 Outra religião oriental não cristã 
# 05 Evangélica pentecostal
# 07 Religiões Tradicionais ou 
# nativas
# 04 
# Nenhuma (Acredita em uma 
#          entidade suprema mas não 
#          pertence à religião nenhuma)
# 11 Agnóstico ou ateu/não acredita 
# em Deus 
# 1501 Espírita Kardecista 
# 77 Outra 
# 888888 Não sabe [NÃO LER] 
# 988888 Não responde

table(BASECJLAPOP2023$q3cn...363)
# Católico 
# 738 
# Protestante Tradicional ou Evangélica não pentecostal 
# 165 
# Outra religião oriental não cristã 
# 10 
# Nenhuma (Acredita em uma entidade suprema mas não pertence à religião nenhuma) 
# 163 
# Evangálica pentecostal 
# 275 
# Religioães Tradicionais ou nativas 
# 20 
# Agnóstico ou ateu/não acredita em Deus 
# 44 
# Outra 
# 57 
# Espírita Kardecista 
# 19 
# Não sabe 
# 0 
# Não responde 
# 0 

#Recategorizar 
# Católica
# Protestante tradicional
# Evangélico pentecostal 
# Outras denominações
# Ateu/Agnóstico

table(BASECJLAPOP2023$q3cn...159)

#   1    2    3    4    5    7   11   77 1501 
# 738  165   10  163  275   20   44   57   19 

#recategorizar 
BASECJLAPOP2023 <- BASECJLAPOP2023 %>% 
  mutate(Religiao = case_when(
    q3cn...159 == 1 ~ "Católica",
    q3cn...159 == 2 ~ "Protestante tradicional",
    q3cn...159 %in% c(3, 7, 77, 1501) ~ "Outras denominações",
    q3cn...159 == 5 ~ "Evangélico pentecostal",
    q3cn...159%in% c(4,11) ~ "Nenhum/Ateu/Agnóstico"
  ))

table(BASECJLAPOP2023$Religiao)
# Católica  Evangélico pentecostal   Nenhum/Ateu/Agnóstico     Outras denominações 
# 738                     275                     207                     106 
# Protestante tradicional 
# 165 

# falta alterar o levels pra testar pra católico 
# levels(BASE_SEN_2022$DS_COR_RACA)
# 
# 
# BASE_SEN_2022$DS_GENERO <- 
#   factor(BASE_SEN_2022$DS_GENERO, 
#          levels = c("FEMININO",  "MASCULINO"))


#Importância religiosa
#IMPORTÂNCIAQ5B####

#Organizar a importância religiosa 



#SALVANDO####

save(BASECJLAPOP2023, file = "BASECJLAPOP2023.RData")



#Reduzindo a base de dados#### 
BaseLAPOP2023F <- subset(BASECJLAPOP2023,
                         select = c("Votacao", "VotacaoCat",
                                    "AvaliacaoPres.", "Ind.TolSex",
"Ind.toldemocratica",
"Ind.ConfInts",
"Ind.ConfMeiosCom",
"Ind.ConfRelig",
"Ind.TolSex",
"Sexo",
"Idade",
"Escolaridade",
"Renda", #Não usei
"Religiao",
"RendaNum", 
"DesafPart."))


save(BaseLAPOP2023F, file = "BaseLAPOP2023F.RData")



#Regressão voto em bolsonaro####

table(BaseLAPOP2023F$Votacao)
table(BaseLAPOP2023F$VotacaoCat)
#Testando para voto em bolsonaro 
BaseLAPOP2023F$VotacaoCat <- as.factor(BaseLAPOP2023F$VotacaoCat)
BaseLAPOP2023F$Religiao <- as.factor(BaseLAPOP2023F$Religiao)
levels(BaseLAPOP2023F$Religiao)

levels(BaseLAPOP2023F$DesafPart.)


BaseLAPOP2023F$DesafPart. <-  
  factor(BaseLAPOP2023F$DesafPart.,
         levels = c("Não sabe","Não responde","Sim","Não"))

Model <- lm(Votacao ~ DesafPart. + Religiao + Sexo + Idade +
              Escolaridade  + Religiao + RendaNum + DesafPart. +
              Ind.TolSex + Ind.toldemocratica + Ind.ConfInts +
              Ind.ConfMeiosCom + Ind.ConfRelig, 
            data = BaseLAPOP2023F)

summary(Model)

# Call:
#   lm(formula = Votacao ~ DesafPart. + Religiao + Sexo + Idade + 
#        Escolaridade + Religiao + RendaNum + DesafPart., data = BaseLAPOP2023F)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.5181 -1.5667  0.6537  1.3600  3.0806 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                     -41.941046  21.208790  -1.978   0.0482 *  
#   DesafPart.Não                    -0.258332   0.117866  -2.192   0.0286 *  
#   ReligiaoEvangélico pentecostal    0.119918   0.134976   0.888   0.3745    
# ReligiaoNenhum/Ateu/Agnóstico    -0.332946   0.151200  -2.202   0.0278 *  
#   ReligiaoOutras denominações      -0.425586   0.200629  -2.121   0.0341 *  
#   ReligiaoProtestante tradicional  -0.142745   0.164534  -0.868   0.3858    
# Sexo                              0.231477   0.098806   2.343   0.0193 *  
#   Idade                             0.020836   0.003286   6.340 3.15e-10 ***
#   Escolaridade                      0.216635   0.035711   6.066 1.71e-09 ***
#   RendaNum                          0.028900   0.014107   2.049   0.0407 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.801 on 1319 degrees of freedom
# (197 observations deleted due to missingness)
# Multiple R-squared:  0.07668,	Adjusted R-squared:  0.07038 
# F-statistic: 12.17 on 9 and 1319 DF,  p-value: < 2.2e-16


tab_model(Model, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")

#Condionantes para o teste de regressão####
#VIF: Variance Inflation Factors

library(olsrr) # Pacote para VIF


ols_vif_tol(Model)
ols_eigen_cindex(Model)

#Coefplot 


obj1 <- coefplot(Model, title = "Coeficientes voto Bolsonaro",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 2,
                 intercept = F)

obj2 <- obj1 + theme_classic() + geom_point(size=3, pch=21, fill="dodgerblue"  ,
                                            alpha=105) 
obj2 + formatacao 


formatacao <- theme(text = element_text(family = "serif", size = 16),
                    title = element_text(color = "black"),
                    axis.line = element_line(color = "black")) +
  theme(legend.position="none") 


#Regressão avaliação negativa de Lula####

# 0    1 
# 1194  320 

table(BaseLAPOP2023F$AvaliacaoPres.)


#Testando para avaliação negativa em lula 
Model2 <- glm(AvaliacaoPres. ~ DesafPart.+ Religiao + Idade + Sexo +
                Escolaridade  + Religiao + RendaNum +
                Ind.TolSex + Ind.toldemocratica + Ind.ConfInts +
                Ind.ConfMeiosCom + Ind.ConfRelig, 
              data = BaseLAPOP2023F, family = binomial(link = "probit"))

summary(Model2)
# Call:
#   glm(formula = AvaliacaoPres. ~ DesafPart. + Religiao + Idade + 
#         Sexo + Escolaridade + Religiao + RendaNum, family = binomial(link = "probit"), 
#       data = BaseLAPOP2023F)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -99.158787  16.595408  -5.975 2.30e-09 ***
#   DesafPart.Não                    -0.082597   0.094062  -0.878   0.3799    
# ReligiaoEvangélico pentecostal    0.614471   0.104717   5.868 4.41e-09 ***
#   ReligiaoNenhum/Ateu/Agnóstico     0.182037   0.123089   1.479   0.1392    
# ReligiaoOutras denominações       0.191119   0.158667   1.205   0.2284    
# ReligiaoProtestante tradicional   0.201010   0.133986   1.500   0.1336    
# Idade                             0.002241   0.002653   0.845   0.3982    
# Sexo                             -0.181295   0.079589  -2.278   0.0227 *  
#   Escolaridade                      0.066473   0.028818   2.307   0.0211 *  
#   RendaNum                          0.065185   0.011036   5.907 3.49e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1388.5  on 1321  degrees of freedom
# Residual deviance: 1286.7  on 1312  degrees of freedom
# (204 observations deleted due to missingness)
# AIC: 1306.7
# 
# Number of Fisher Scoring iterations: 4

tab_model(Model2, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


Model3 <- glm(AvaliacaoPres. ~ DesafPart.+ Religiao + Idade + Sexo +
                Escolaridade  + Religiao + RendaNum +
                Ind.TolSex + Ind.toldemocratica + Ind.ConfInts +
                Ind.ConfMeiosCom + Ind.ConfRelig, 
              data = BaseLAPOP2023F, family = binomial(link = "logit"))

tab_model(Model3, show.ci = F, auto.label = T, show.se =  T,
          collapse.se = T, wrap.labels = 60, p.style = "stars")


#Coefplot 


obj1 <- coefplot(Model3, title = "Coeficientes da avaliação negativa em Lula",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 2,
                 intercept = F)

obj2 <- obj1 + theme_classic() + geom_point(size=3, pch=21, fill="aquamarine2"  ,
                                            alpha=105) 
obj2 + formatacao 


formatacao <- theme(text = element_text(family = "serif", size = 16),
                    title = element_text(color = "black"),
                    axis.line = element_line(color = "black")) +
  theme(legend.position="none") 


#colors()#####
[1] "white"                "aliceblue"            "antiquewhite"        
[4] "antiquewhite1"        "antiquewhite2"        "antiquewhite3"       
[7] "antiquewhite4"        "aquamarine"           "aquamarine1"         
[10] "aquamarine2"          "aquamarine3"          "aquamarine4"         
[13] "azure"                "azure1"               "azure2"              
[16] "azure3"               "azure4"               "beige"               
[19] "bisque"               "bisque1"              "bisque2"             
[22] "bisque3"              "bisque4"              "black"               
[25] "blanchedalmond"       "blue"                 "blue1"               
[28] "blue2"                "blue3"                "blue4"               
[31] "blueviolet"           "brown"                "brown1"              
[34] "brown2"               "brown3"               "brown4"              
[37] "burlywood"            "burlywood1"           "burlywood2"          
[40] "burlywood3"           "burlywood4"           "cadetblue"           
[43] "cadetblue1"           "cadetblue2"           "cadetblue3"          
[46] "cadetblue4"           "chartreuse"           "chartreuse1"         
[49] "chartreuse2"          "chartreuse3"          "chartreuse4"         
[52] "chocolate"            "chocolate1"           "chocolate2"          
[55] "chocolate3"           "chocolate4"           "coral"               
[58] "coral1"               "coral2"               "coral3"              
[61] "coral4"               "cornflowerblue"       "cornsilk"            
[64] "cornsilk1"            "cornsilk2"            "cornsilk3"           
[67] "cornsilk4"            "cyan"                 "cyan1"               
[70] "cyan2"                "cyan3"                "cyan4"               
[73] "darkblue"             "darkcyan"             "darkgoldenrod"       
[76] "darkgoldenrod1"       "darkgoldenrod2"       "darkgoldenrod3"      
[79] "darkgoldenrod4"       "darkgray"             "darkgreen"           
[82] "darkgrey"             "darkkhaki"            "darkmagenta"         
[85] "darkolivegreen"       "darkolivegreen1"      "darkolivegreen2"     
[88] "darkolivegreen3"      "darkolivegreen4"      "darkorange"          
[91] "darkorange1"          "darkorange2"          "darkorange3"         
[94] "darkorange4"          "darkorchid"           "darkorchid1"         
[97] "darkorchid2"          "darkorchid3"          "darkorchid4"         
[100] "darkred"              "darksalmon"           "darkseagreen"        
[103] "darkseagreen1"        "darkseagreen2"        "darkseagreen3"       
[106] "darkseagreen4"        "darkslateblue"        "darkslategray"       
[109] "darkslategray1"       "darkslategray2"       "darkslategray3"      
[112] "darkslategray4"       "darkslategrey"        "darkturquoise"       
[115] "darkviolet"           "deeppink"             "deeppink1"           
[118] "deeppink2"            "deeppink3"            "deeppink4"           
[121] "deepskyblue"          "deepskyblue1"         "deepskyblue2"        
[124] "deepskyblue3"         "deepskyblue4"         "dimgray"             
[127] "dimgrey"              "dodgerblue"           "dodgerblue1"         
[130] "dodgerblue2"          "dodgerblue3"          "dodgerblue4"         
[133] "firebrick"            "firebrick1"           "firebrick2"          
[136] "firebrick3"           "firebrick4"           "floralwhite"         
[139] "forestgreen"          "gainsboro"            "ghostwhite"          
[142] "gold"                 "gold1"                "gold2"               
[145] "gold3"                "gold4"                "goldenrod"           
[148] "goldenrod1"           "goldenrod2"           "goldenrod3"          
[151] "goldenrod4"           "gray"                 "gray0"               
[154] "gray1"                "gray2"                "gray3"               
[157] "gray4"                "gray5"                "gray6"               
[160] "gray7"                "gray8"                "gray9"               
[163] "gray10"               "gray11"               "gray12"              
[166] "gray13"               "gray14"               "gray15"              
[169] "gray16"               "gray17"               "gray18"              
[172] "gray19"               "gray20"               "gray21"              
[175] "gray22"               "gray23"               "gray24"              
[178] "gray25"               "gray26"               "gray27"              
[181] "gray28"               "gray29"               "gray30"              
[184] "gray31"               "gray32"               "gray33"              
[187] "gray34"               "gray35"               "gray36"              
[190] "gray37"               "gray38"               "gray39"              
[193] "gray40"               "gray41"               "gray42"              
[196] "gray43"               "gray44"               "gray45"              
[199] "gray46"               "gray47"               "gray48"              
[202] "gray49"               "gray50"               "gray51"              
[205] "gray52"               "gray53"               "gray54"              
[208] "gray55"               "gray56"               "gray57"              
[211] "gray58"               "gray59"               "gray60"              
[214] "gray61"               "gray62"               "gray63"              
[217] "gray64"               "gray65"               "gray66"              
[220] "gray67"               "gray68"               "gray69"              
[223] "gray70"               "gray71"               "gray72"              
[226] "gray73"               "gray74"               "gray75"              
[229] "gray76"               "gray77"               "gray78"              
[232] "gray79"               "gray80"               "gray81"              
[235] "gray82"               "gray83"               "gray84"              
[238] "gray85"               "gray86"               "gray87"              
[241] "gray88"               "gray89"               "gray90"              
[244] "gray91"               "gray92"               "gray93"              
[247] "gray94"               "gray95"               "gray96"              
[250] "gray97"               "gray98"               "gray99"              
[253] "gray100"              "green"                "green1"              
[256] "green2"               "green3"               "green4"              
[259] "greenyellow"          "grey"                 "grey0"               
[262] "grey1"                "grey2"                "grey3"               
[265] "grey4"                "grey5"                "grey6"               
[268] "grey7"                "grey8"                "grey9"               
[271] "grey10"               "grey11"               "grey12"              
[274] "grey13"               "grey14"               "grey15"              
[277] "grey16"               "grey17"               "grey18"              
[280] "grey19"               "grey20"               "grey21"              
[283] "grey22"               "grey23"               "grey24"              
[286] "grey25"               "grey26"               "grey27"              
[289] "grey28"               "grey29"               "grey30"              
[292] "grey31"               "grey32"               "grey33"              
[295] "grey34"               "grey35"               "grey36"              
[298] "grey37"               "grey38"               "grey39"              
[301] "grey40"               "grey41"               "grey42"              
[304] "grey43"               "grey44"               "grey45"              
[307] "grey46"               "grey47"               "grey48"              
[310] "grey49"               "grey50"               "grey51"              
[313] "grey52"               "grey53"               "grey54"              
[316] "grey55"               "grey56"               "grey57"              
[319] "grey58"               "grey59"               "grey60"              
[322] "grey61"               "grey62"               "grey63"              
[325] "grey64"               "grey65"               "grey66"              
[328] "grey67"               "grey68"               "grey69"              
[331] "grey70"               "grey71"               "grey72"              
[334] "grey73"               "grey74"               "grey75"              
[337] "grey76"               "grey77"               "grey78"              
[340] "grey79"               "grey80"               "grey81"              
[343] "grey82"               "grey83"               "grey84"              
[346] "grey85"               "grey86"               "grey87"              
[349] "grey88"               "grey89"               "grey90"              
[352] "grey91"               "grey92"               "grey93"              
[355] "grey94"               "grey95"               "grey96"              
[358] "grey97"               "grey98"               "grey99"              
[361] "grey100"              "honeydew"             "honeydew1"           
[364] "honeydew2"            "honeydew3"            "honeydew4"           
[367] "hotpink"              "hotpink1"             "hotpink2"            
[370] "hotpink3"             "hotpink4"             "indianred"           
[373] "indianred1"           "indianred2"           "indianred3"          
[376] "indianred4"           "ivory"                "ivory1"              
[379] "ivory2"               "ivory3"               "ivory4"              
[382] "khaki"                "khaki1"               "khaki2"              
[385] "khaki3"               "khaki4"               "lavender"            
[388] "lavenderblush"        "lavenderblush1"       "lavenderblush2"      
[391] "lavenderblush3"       "lavenderblush4"       "lawngreen"           
[394] "lemonchiffon"         "lemonchiffon1"        "lemonchiffon2"       
[397] "lemonchiffon3"        "lemonchiffon4"        "lightblue"           
[400] "lightblue1"           "lightblue2"           "lightblue3"          
[403] "lightblue4"           "lightcoral"           "lightcyan"           
[406] "lightcyan1"           "lightcyan2"           "lightcyan3"          
[409] "lightcyan4"           "lightgoldenrod"       "lightgoldenrod1"     
[412] "lightgoldenrod2"      "lightgoldenrod3"      "lightgoldenrod4"     
[415] "lightgoldenrodyellow" "lightgray"            "lightgreen"          
[418] "lightgrey"            "lightpink"            "lightpink1"          
[421] "lightpink2"           "lightpink3"           "lightpink4"          
[424] "lightsalmon"          "lightsalmon1"         "lightsalmon2"        
[427] "lightsalmon3"         "lightsalmon4"         "lightseagreen"       
[430] "lightskyblue"         "lightskyblue1"        "lightskyblue2"       
[433] "lightskyblue3"        "lightskyblue4"        "lightslateblue"      
[436] "lightslategray"       "lightslategrey"       "lightsteelblue"      
[439] "lightsteelblue1"      "lightsteelblue2"      "lightsteelblue3"     
[442] "lightsteelblue4"      "lightyellow"          "lightyellow1"        
[445] "lightyellow2"         "lightyellow3"         "lightyellow4"        
[448] "limegreen"            "linen"                "magenta"             
[451] "magenta1"             "magenta2"             "magenta3"            
[454] "magenta4"             "maroon"               "maroon1"             
[457] "maroon2"              "maroon3"              "maroon4"             
[460] "mediumaquamarine"     "mediumblue"           "mediumorchid"        
[463] "mediumorchid1"        "mediumorchid2"        "mediumorchid3"       
[466] "mediumorchid4"        "mediumpurple"         "mediumpurple1"       
[469] "mediumpurple2"        "mediumpurple3"        "mediumpurple4"       
[472] "mediumseagreen"       "mediumslateblue"      "mediumspringgreen"   
[475] "mediumturquoise"      "mediumvioletred"      "midnightblue"        
[478] "mintcream"            "mistyrose"            "mistyrose1"          
[481] "mistyrose2"           "mistyrose3"           "mistyrose4"          
[484] "moccasin"             "navajowhite"          "navajowhite1"        
[487] "navajowhite2"         "navajowhite3"         "navajowhite4"        
[490] "navy"                 "navyblue"             "oldlace"             
[493] "olivedrab"            "olivedrab1"           "olivedrab2"          
[496] "olivedrab3"           "olivedrab4"           "orange"              
[499] "orange1"              "orange2"              "orange3"             
[502] "orange4"              "orangered"            "orangered1"          
[505] "orangered2"           "orangered3"           "orangered4"          
[508] "orchid"               "orchid1"              "orchid2"             
[511] "orchid3"              "orchid4"              "palegoldenrod"       
[514] "palegreen"            "palegreen1"           "palegreen2"          
[517] "palegreen3"           "palegreen4"           "paleturquoise"       
[520] "paleturquoise1"       "paleturquoise2"       "paleturquoise3"      
[523] "paleturquoise4"       "palevioletred"        "palevioletred1"      
[526] "palevioletred2"       "palevioletred3"       "palevioletred4"      
[529] "papayawhip"           "peachpuff"            "peachpuff1"          
[532] "peachpuff2"           "peachpuff3"           "peachpuff4"          
[535] "peru"                 "pink"                 "pink1"               
[538] "pink2"                "pink3"                "pink4"               
[541] "plum"                 "plum1"                "plum2"               
[544] "plum3"                "plum4"                "powderblue"          
[547] "purple"               "purple1"              "purple2"             
[550] "purple3"              "purple4"              "red"                 
[553] "red1"                 "red2"                 "red3"                
[556] "red4"                 "rosybrown"            "rosybrown1"          
[559] "rosybrown2"           "rosybrown3"           "rosybrown4"          
[562] "royalblue"            "royalblue1"           "royalblue2"          
[565] "royalblue3"           "royalblue4"           "saddlebrown"         
[568] "salmon"               "salmon1"              "salmon2"             
[571] "salmon3"              "salmon4"              "sandybrown"          
[574] "seagreen"             "seagreen1"            "seagreen2"           
[577] "seagreen3"            "seagreen4"            "seashell"            
[580] "seashell1"            "seashell2"            "seashell3"           
[583] "seashell4"            "sienna"               "sienna1"             
[586] "sienna2"              "sienna3"              "sienna4"             
[589] "skyblue"              "skyblue1"             "skyblue2"            
[592] "skyblue3"             "skyblue4"             "slateblue"           
[595] "slateblue1"           "slateblue2"           "slateblue3"          
[598] "slateblue4"           "slategray"            "slategray1"          
[601] "slategray2"           "slategray3"           "slategray4"          
[604] "slategrey"            "snow"                 "snow1"               
[607] "snow2"                "snow3"                "snow4"               
[610] "springgreen"          "springgreen1"         "springgreen2"        
[613] "springgreen3"         "springgreen4"         "steelblue"           
[616] "steelblue1"           "steelblue2"           "steelblue3"          
[619] "steelblue4"           "tan"                  "tan1"                
[622] "tan2"                 "tan3"                 "tan4"                
[625] "thistle"              "thistle1"             "thistle2"            
[628] "thistle3"             "thistle4"             "tomato"              
[631] "tomato1"              "tomato2"              "tomato3"             
[634] "tomato4"              "turquoise"            "turquoise1"          
[637] "turquoise2"           "turquoise3"           "turquoise4"          
[640] "violet"               "violetred"            "violetred1"          
[643] "violetred2"           "violetred3"           "violetred4"          
[646] "wheat"                "wheat1"               "wheat2"              
[649] "wheat3"               "wheat4"               "whitesmoke"          
[652] "yellow"               "yellow1"              "yellow2"             
[655] "yellow3"              "yellow4"              "yellowgreen"
