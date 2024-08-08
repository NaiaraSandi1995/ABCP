#Artigo ANPOCS#######

#PACOTES####
library(haven)
library(sjPlot)
library(dplyr)
library(tidyverse)
library(tidyr)
library(scales)

ESEB2018 <- read_sav("ESEB 2018/ESEB2018.sav")

#renomeação 2018####

##VD####

#Confiança#####

# P05. De forma geral, você acredita que as 
#eleições no Brasil são confiáveis ou são objeto de fraude? 
#   (ESTIMULADA E RU)
# 1.As eleições são confiáveis 
# 2.São objetos de fraude 
# 8.NÃO SABE 
# 9.NÃO RESPONDEU

table(ESEB2018$P5)

# 1    2    8    9 
# 991 1312  167   36 



ESEB2018$Confiança <- 
  ifelse(ESEB2018$P5 == 1, "Confia",
         ifelse(ESEB2018$P5 == 2, "Desconfia",
                ifelse(ESEB2018$P5  %in%  c(8, 9), NA,
                       NA)))

table(ESEB2018$Confiança)
# Confia Desconfia 
# 991      1312 

ESEB2018$ConfiançaN <- 
  ifelse(ESEB2018$P5 == 1, 0,
         ifelse(ESEB2018$P5 == 2, 1,
                ifelse(ESEB2018$P5  %in%  c(8, 9), NA,
                       NA)))

table(ESEB2018$ConfiançaN)



#Presidente####

# Labels:
#   value                       label
# 1        Fernando Haddad (PT)
# 2        Jair Bolsonaro (PSL)
# 50        Anulou o voto (Esp.)
# 60      Votou em branco (Esp.)
# 97        Não respondeu (Esp.)
# 98 Não sabe/ Não lembra (Esp.)
# 99                     Missing

table(ESEB2018$Q12P2_B)

#  1    2   50   60   97   98   99 
# 711 1023  125   64   52   16  515 

ESEB2018$VotoPres18 <- ESEB2018$Q12P2_B

ESEB2018$VotoPres18 <- 
  ifelse(ESEB2018$Q12P2_B == 1, "Perdeu-N.Constestou",
  ifelse(ESEB2018$Q12P2_B == 2, "Ganhou-Constestou",
  ifelse(ESEB2018$Q12P2_B  %in%  c(50, 60, 97, 98), "NS/NR",
  "Não votou")))

table(ESEB2018$VotoPres18)

length(ESEB2018$VotoPres18) #[1] 2506

#redes sociais####
table(ESEB2018$P25)

#P25. Qual destes 
#meios o(a) sr(a) mais utiliza para se informar sobre política?

# Labels:
# value                                      label
# 1                         Jornais e Revistas
# 2                           Noticiário da TV
# 3                                      Rádio
# 4                          Blogs da internet #digital
# 5 Redes sociais, Facebook, twitter, whatsapp #digital
# 6           Conversa com amigos e familiares
# 7           Conversa com colegas de trabalho
# 8                            Busca no Google #digital
# 9       Não se informa sobre política (Esp.)
# 10                       Nenhum destes (Esp.)
# 88                                   Não sabe
# 99                              Não respondeu


table(ESEB2018$P25)

# 1    2    3    4    5    6    7    8    9   10   88   99 
# 223 1036  111  100  471  160   46  116   56  149   27   11 

length(ESEB2018$P25) #[1] 2506

table(ESEB2018$P26)#Não adianta usar essa questão, pq em 2022 
# as redes sociais estão agregadas 

#utiliza meio digital
ESEB2018$InfoPol <- ifelse(
(ESEB2018$P25 >= 4 & ESEB2018$P25 <= 5) |
(ESEB2018$P25 == 8),
  "Digital",
  "Não"
)


ESEB2018$InfoPol <-  
  factor(ESEB2018$InfoPol,
         levels = c("Não", "Digital"))

table(ESEB2018$InfoPol)

#Utiliza meio analógico
ESEB2018$InfoPol2 <- ifelse(
  (ESEB2018$P25 >= 1 & ESEB2018$P25 <= 3) |
  (ESEB2018$P25 >= 6 & ESEB2018$P25 <= 7),
  "Analógico",
  "Não"
)

ESEB2018$InfoPol2 <-  
  factor(ESEB2018$InfoPol2,
         levels = c("Não", "Analógico"))

table(ESEB2018$InfoPol2)

table(ESEB2018$InfoPol)
# Digital     Não 
# 687        1819 
table(ESEB2018$InfoPol2)
# Analógico       Não 
# 1576       930 

#Idade#### 


summary(ESEB2018$D1A_ID)

ESEB2018$Idade <- ESEB2018$D1A_ID

#Sexo####
ESEB2018$D2_SEXO


# Labels:
#   value     label
# 1  Masculino
# 2  Feminino

table(ESEB2018$D2_SEXO)

ESEB2018$Sexo <- ifelse(ESEB2018$D2_SEXO==1, "Masculino",
                 ifelse(ESEB2018$D2_SEXO==2, "Feminino", NA))

table(ESEB2018$Sexo)

ESEB2018$Sexo <-  
  factor(ESEB2018$Sexo,
         levels = c("Masculino", "Feminino"))


levels(ESEB2018$Sexo)

#Escolaridade####
#Testando para ensino superior ou mais
table(ESEB2018$D3_ESCOLA)
summary(ESEB2018$D3_ESCOLA)


ESEB2018$Escolaridade <- ESEB2018$D3_ESCOLA


#InteressePolítica####

ESEB2018$Q1

# 
# Labels:
#   value                label
# 1 Muito interessado(a)
# 2       Interessado(a)
# 3 Pouco interessado(a)
# 4  Nada interessado(a)
# 7        Não respondeu
# 8             Não sabe
# 9              Missing

table(ESEB2018$Q1)
# 1   2   3   4   7   8 
# 322 430 985 740  18  11 

#
ESEB2018$InterPolitica <- ifelse(ESEB2018$Q1 == 1, 4,
                                         ifelse(ESEB2018$Q1 == 2, 3,
                                                ifelse(ESEB2018$Q1 == 3, 2,
                                                       ifelse(ESEB2018$Q1 == 4, 1,
                                                              ifelse(ESEB2018$Q1 == 7, NA,
                                                                     ifelse(ESEB2018$Q1 == 8, NA, NA))))))


#renomeação 2022####

ESEB2022 <- read_sav("ESEB 2022/ESEB2022.sav")

##VD####

#Confiança####
# Q13.  EM ALGUNS PAÍSES, AS PESSOAS 
# ACHAM QUE AS ELEIÇÕES SÃO REALIZADAS DE MANEIRA CONFIÁVEL. EM OUTROS 
# PAÍSES, AS PESSOAS ACHAM QUE AS ELEIÇÕES NÃO SÃO REALIZADAS DE MANEIRA 
# CONFIÁVEL.  
# PENSANDO NESTA ELEIÇÃO, EM UMA ESCALA DE 1 A 5, ONDE 1 SIGNIFICA QUE A 
# ELEIÇÃO FOI REALIZADA DE MANEIRA CONFIÁVEL E 5 SIGNIFICA QUE A ELEIÇÃO 
# NÃO FOI REALIZADA DE MANEIRA CONFIÁVEL, COMO O(A) SR. (A) CONSIDERA 
# ESTA ELEIÇÃO? (RU) 
# 1. AS ELEIÇÕES FORAM REALIZADAS DE MANEIRA CONFIÁVEL  
# 2.  
# 3.  
# 4.  
# 5. AS ELEIÇÕES NÃO FORAM REALIZADAS DE MANEIRA CONFIÁVEl  
# 97. NÃO SABE (NÃO LER) 
# 98. NÃO RESPONDEU (NÃO LER

table(ESEB2022$Q13)

 # 1   2   3   4   5  97  98 
# 841 113 208 157 654  26   2  


ESEB2022$Confiança <- 
  ifelse(ESEB2022$Q13 %in% c(1,2), "Confia",
         ifelse(ESEB2022$Q13 %in% c(3,4,5), "Desconfia",
                ifelse(ESEB2022$Q13  %in%  c(97,98), NA,
                       NA)))

table(ESEB2022$Confiança)
# Confia Desconfia 
# 954      1019 


ESEB2022$ConfiançaN <- 
  ifelse(ESEB2022$Q13 %in% c(1,2), 0,
         ifelse(ESEB2022$Q13 %in% c(3,4,5), 1,
                ifelse(ESEB2022$Q13  %in%  c(97,98), NA,
                       NA)))

table(ESEB2022$ConfiançaN)

##VI####

#Presidente####
table(ESEB2022$Q10P2b)
# Labels:
#   value                label
# 1       Jair Bolsonaro
# 2                 Lula
# 50        Anulou o voto
# 60      Votou em branco
# 97 Não sabe/ Não lembra
# 98        Não respondeu

length(ESEB2022$Q10P2a)
table(ESEB2022$Q10P2a) #votou no 2º turno

#     1    2    3    4    5    6    7   97   98 
# 1717   24    2  103  116   11   19    1    8 


table(ESEB2022$Q10P2b)
ESEB2022$Q10P2b

#  1   2  50  60  97  98 
# 718 835  63  40   6  55 


ESEB2022$VotoPres2022 <- 
  ifelse(ESEB2022$Q10P2b == 1, "Perdeu-Constestou",
  ifelse(ESEB2022$Q10P2b == 2, "Ganhou-N.Constestou",
  ifelse(ESEB2022$Q10P2b  %in% 
           c(50, 60, 97, 98), "NS/NR",
  "Não votou")))

ESEB2022$VotoPres2022 <- ifelse(is.na(ESEB2022$Q10P2b), "Não votou", ESEB2022$VotoPres2022)


table(ESEB2018$VotoPres18)
# Ganhou-N.Constestou           Não votou               NS/NR Perdeu-N.Constestou 
# 1023                 515                 257                 711 

table(ESEB2022$VotoPres2022)
# Ganhou-N.Constestou           Não votou               NS/NR   Perdeu-Constestou 
# 835                 284                 164                 718 

#redes sociais####
ESEB2022$Q02f

# Q02f.  Durante a campanha eleitoral passada, em uma semana 
#normal, quantos dias o(a) sr(a): Usou redes sociais, 
#como Facebook, Twitter, Whatsapp ou Telegram, 
# para acompanhar as notícias sobre partidos e candidatos?


# Labels:
#   value             label
# 0 Nenhum/ zero dias
# 1            Um dia
# 2         Dois dias
# 3         Três dias
# 4       Quatro dias
# 5        Cinco dias
# 6         Seis dias
# 7         Sete dias
# 97          Não sabe
# 98     Não respondeu


table(ESEB2022$Q02f)

#   0*   1   2   3   4   5   6   7  97*  98* 
# 708  47  83  92  51  73  44 896   5   2 

length(ESEB2022$Q02f) #[1] 2001

#vou recodificar com "sim" para redes sociais quem respondeu 4 e 5 
# e não para todos os outros 1 a 3, 6, 7, 98,99 e 999 

#Q02e. VISITOU SITES DE NOTÍCIAS NA INTERNET 
table(ESEB2022$Q02e)

#   0   1   2   3   4   5   6   7  97  98 
# 780 100 148 145  82  84  36 609  16   1 

ESEB2022$InfoPol <- ifelse(
  (ESEB2022$Q02e >= 1 & ESEB2022$Q02e <= 7) |
  (ESEB2022$Q02f >= 1 & ESEB2022$Q02f <= 7),
  "Digital",
  "Não"
)

ESEB2022$InfoPol <-  
  factor(ESEB2022$InfoPol,
         levels = c("Não", "Digital"))

table(ESEB2022$InfoPol)

# Q02a. ASSISTIU AS NOTÍCIAS EM UMA EMISSORA DE TELEVISÃO PÚBLICA, COMO A 
# TV BRASIL OU A TV CULTURA 
# 
# Q02b. ASSISTIU AS NOTÍCIAS EM UMA EMISSORA DE TELEVISÃO PRIVADA, COMO 
# A REDE GLOBO OU A TV RECORD 
#  
# Q02c. OUVIU AS NOTÍCIAS NO RÁDIO 

ESEB2022$InfoPol2 <- ifelse(
  (ESEB2022$Q02a >= 1 & ESEB2022$Q02a <= 7) |
  (ESEB2022$Q02b >= 1 & ESEB2022$Q02b <= 7) |
  (ESEB2022$Q02c >= 1 & ESEB2022$Q02c <= 7),
  "Analógico",
  "Não"
)

table(ESEB2022$InfoPol2)
# Analógico       Não 
# 1610       391 

ESEB2022$InfoPol2 <-  
  factor(ESEB2022$InfoPol2,
         levels = c("Não", "Analógico"))

table(ESEB2022$InfoPol2)
# Analógico       Não 
#     1610       391



#Idade#### 

ESEB2022$D01A_IDADE

summary(ESEB2022$D01A_IDADE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 16.00   29.00   41.00   42.23   54.00   90.00 

ESEB2022$Idade <- ESEB2022$D01A_IDADE

#Sexo####
ESEB2022$D02

# Labels:
#  value     label
#      1 MASCULINO
#      2  FEMININO

table(ESEB2022$D02)
# 1    2 
# 978 1023 

ESEB2022$Sexo <- ifelse(ESEB2022$D02==1, "Masculino",
                 ifelse(ESEB2022$D02==2, "Feminino", NA))

table(ESEB2022$Sexo)

ESEB2022$Sexo <-  
  factor(ESEB2022$Sexo,
         levels = c("Masculino", "Feminino"))


levels(ESEB2022$Sexo)

#Escolaridade####
#Testando para ensino superior ou mais
table(ESEB2022$D03)

ESEB2022$D03[ESEB2022$D03 == 99] <- NA #1 único caso de NA

ESEB2022$Escolaridade <- ESEB2022$D03

length(ESEB2022$Escolaridade)

#InteressePolítica####

ESEB2022$Q01

# Labels:
#   value                label
# 1   Muito interessado,
# 2         Interessado,
# 3 Pouco interessado ou
# 4    Nada interessado?
#   97             Não sabe
# 98        Não respondeu

table(ESEB2022$Q01)
#  1   2   3   4  97  98 
# 358 414 822 396   9   2 


#
ESEB2022$InterPolitica <- ifelse(ESEB2022$Q01 == 1, 4,
                                 ifelse(ESEB2022$Q01 == 2, 3,
                                        ifelse(ESEB2022$Q01 == 3, 2,
                                               ifelse(ESEB2022$Q01 == 4, 1,
                                                      ifelse(ESEB2022$Q01 == 97, NA,
                                                             ifelse(ESEB2022$Q01 == 98, NA, NA))))))


table(ESEB2022$InterPolitica)

#Separação da base#####
#2018####

#VD: Confiança 
ESEB2018$Confiança
ESEB2018$ConfiançaN

#VI: 
ESEB2018$VotoPres18
ESEB2018$InfoPol
ESEB2018$InfoPol2
ESEB2018$InterPolitica
ESEB2018$Idade
ESEB2018$Sexo
ESEB2018$Escolaridade


ESEB2018Menor <-  subset(ESEB2018, select = 
                           c("Confiança",
                             "ConfiançaN",
                             "VotoPres18",
                             "InfoPol",
                             "InfoPol2",
                             "InterPolitica",
                             "Idade",
                             "Sexo",
                             "Escolaridade"))


save(ESEB2018Menor, file = "ESEB2018Menor.RData")

#2022####

#VD: Confiança 
ESEB2022$Confiança
ESEB2022$ConfiançaN

#VI: 
ESEB2022$VotoPres2022
ESEB2022$InfoPol
ESEB2022$InfoPol2
ESEB2022$InterPolitica
ESEB2022$Idade
ESEB2022$Sexo
ESEB2022$Escolaridade

ESEB2022Menor <-  subset(ESEB2022, select = 
                           c("Confiança",
                             "ConfiançaN",
                             "VotoPres2022",
                             "InfoPol",
                             "InfoPol2",
                             "InterPolitica",
                             "Idade",
                             "Sexo",
                             "Escolaridade"))

save(ESEB2022Menor, file = "ESEB2022Menor.RData")

#modelos####
#2018####


table(ESEB2018Menor$VotoPres18)

# Ganhou-Constestou           Não votou               NS/NR 
#         1023                 515                 257 
# Perdeu-N.Constestou 
# 711 

ESEB2018Menor$VotoPres18 <-  
  factor(ESEB2018Menor$VotoPres18,
         levels = c("Ganhou-Constestou", "Perdeu-N.Constestou",
                    "Não votou", "NS/NR"))

m2018 <- glm(ConfiançaN ~ VotoPres18 + InfoPol +
               InfoPol2 + InterPolitica + Idade +
               Sexo + Escolaridade, 
             data = ESEB2018Menor,
             family = binomial(link = "logit"))
  

summary(m2018)

obj1 <-  tab_model(m2018, show.ci = F, auto.label = F, 
          p.style = "star")

# A tibble: 10 × 3
# term                          `Odds Ratio` `Coeficiente Logit`
# <chr>                                <dbl>               <dbl>
#   1 (Intercept)                          8.04              2.08   
# 2 VotoPres18Não votou                  1.45              0.370  
# 3 VotoPres18NS/NR                      0.895            -0.111  
# 4 VotoPres18Perdeu-N.Constestou        0.967            -0.0334 
# 5 InfoPolDigital                       0.589            -0.529  
# 6 InfoPol2Analógico                    0.453            -0.793  
# 7 InterPolitica                        0.747            -0.292  
# 8 Idade                                0.991            -0.00887
# 9 SexoFeminino                         1.50              0.402  
# 10 Escolaridade                         0.923            -0.0803 


#2022####
table(ESEB2022Menor$VotoPres2022)
# Ganhou-N.Constestou           Não votou               NS/NR 
# 835                 284                 164 
# Perdeu-Constestou 
# 718 

ESEB2022Menor$VotoPres2022 <-  
  factor(ESEB2022Menor$VotoPres2022,
         levels = c("Ganhou-N.Constestou", "Perdeu-Constestou",
                    "Não votou", "NS/NR"))

m2022 <- glm(ConfiançaN ~ VotoPres2022 + InfoPol +
               InfoPol2 + InterPolitica + Idade +
               Sexo + Escolaridade, 
             data = ESEB2022Menor,
             family = binomial(link = "logit"))


summary(m2022)

obj2 <- tab_model(m2022, show.ci = F, auto.label = F, 
          p.style = "star")

# A tibble: 9 × 3
# term                          `Odds Ratio` `Coeficiente Logit`
# <chr>                                <dbl>               <dbl>
#   1 (Intercept)                          0.321            -1.14   
# 2 VotoPres2022NS/NR                    3.00              1.10   
# 3 VotoPres2022Perdeu-Constestou       15.7               2.76   
# 4 InfoPolDigital                       1.33              0.284  
# 5 InfoPol2Analógico                    0.763            -0.271  
# 6 InterPolitica                        0.940            -0.0616 
# 7 Idade                                1.01              0.00618
# 8 SexoFeminino                         1.09              0.0816 
# 9 Escolaridade                         0.979            -0.0216 

#Merge####

ESEB2018Menor$ANO <- 2018
ESEB2022Menor$ANO <- 2022

colnames(ESEB2018Menor)[colnames(ESEB2018Menor) == 
                          "VotoPres18"] <- "VotoPresidente"

colnames(ESEB2022Menor)[colnames(ESEB2022Menor) == 
                          "VotoPres2022"] <- "VotoPresidente"

colnames(ESEB2018Menor)
colnames(ESEB2022Menor)

BaseUnida <- bind_rows(ESEB2018Menor, ESEB2022Menor)

length(BaseUnida$Escolaridade)

save(ESEB2018Menor, file = "ESEB2018Menor.RData")
save(ESEB2022Menor, file = "ESEB2022Menor.RData")

save(BaseUnida, file = "BaseUnida.RData")

library(writexl)
#salvando
writexl::write_xlsx(ESEB2018Menor, path = "ESEB2018Menor.xlsx")
writexl::write_xlsx(ESEB2022Menor, path = "ESEB2022Menor.xlsx")


#ModeloFinal####
BaseUnida$VotoPresidente <-  
  factor(BaseUnida$VotoPresidente,
         levels = c("Ganhou-N.Constestou", "Perdeu-N.Constestou",
                    "Ganhou-Constestou", "Perdeu-Constestou", 
                    "Não votou", "NS/NR"))


table(BaseUnida$VotoPresidente)
# Ganhou-N.Constestou Perdeu-N.Constestou   Ganhou-Constestou 
# 835                 711                1023 
# Perdeu-Constestou           Não votou               NS/NR 
# 718                 799                 421


table(BaseUnida$Sexo)

BaseUnida$InfoPol <-  
  factor(BaseUnida$InfoPol,
         levels = c("Não", "Digital"))

table(BaseUnida$InfoPol)

BaseUnida$InfoPol2 <-  
  factor(BaseUnida$InfoPol2,
         levels = c("Não", "Analógico"))

table(BaseUnida$InfoPol2)


mUnido <- glm(ConfiançaN ~ VotoPresidente + InfoPol +
               InfoPol2 + InterPolitica + Idade +
               Sexo + Escolaridade, 
             data = BaseUnida,
             family = binomial(link = "logit"))


summary(mUnido)



obj3 <- tab_model(mUnido, show.ci = F, auto.label = F, 
          p.style = "star")


tab_model(m2018, m2022, mUnido,
          show.ci = F,
          show.se = T,
          auto.label = T,
          collapse.se = T,
          p.style = "star")


#transform = NULL

Base

#GRÁFICOS####

plot_model(mUnido)

# BaseUnida$Escolaridade <- as.numeric(BaseUnida$Escolaridade)
# plot_model(mUnido, terms ="Escolaridade", type = "pred")
# Obj <-  plot_model(mUnido, terms ="Escolaridade", type = "pred")
# 
# Obj + labs(x= "Predicted", y= "Escolaridade") +
#   theme_classic() +Fonte
#grey90

#Fonte####
Fonte <-  theme(text = element_text(family = "serif", size = 13),
                title = element_text(color = "black"),
                axis.line = element_line(color = "black"), 
                axis.text = element_text(colour = "black", size = rel(0.7))) 

#voto

plot_model(mUnido, terms ="VotoPresidente", type = "pred")

Obj2 <- plot_model(mUnido, terms ="VotoPresidente", 
                   type = "pred",
                   colors = 'steelblue1')

Obj2 + coord_flip() +
  labs(x= "VotoPresidente", y= "Predicted",
       title = " ") +
  theme_classic() +Fonte 

colnames(Obj2$data)


Obj2  +
  labs(x= "VotoPresidente", y= "Probabilidades Previstas",
       title = " ") +
  theme_classic() +Fonte +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#y"Probabilidades Previstas"

table(BaseUnida$VotoPresidente)


#GRÁFICOS FINAIS

BaseUnida <- drop_na(BaseUnida, Confiança)

BaseUnida$ANO <- as.factor(BaseUnida$ANO)

GraConf <- ggplot(BaseUnida, aes(x= Confiança, 
                                     y= (..count..)/sum(..count..), 
                                 fill= ANO)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette="Greys", type = "div")+
  theme_classic() +
  labs(title = "Confiança nas instituições", y="Contagem", x= "Confiança") 
  

GraConf + 
  theme(text = element_text(family = "serif", size = 13))

GraConf + coord_flip() + Fonte


Fonte <-  theme(plot.background = element_rect(fill = "grey90", 
                                               colour = "black", 
                                               linewidth = 1)) 


GraConf + coord_flip() + theme_dark()

#####################
#TESTES#########
#CONFIANÇA- VC####
#VD 2022  - primeiro corte foi 1+2 = confia e 
#3+4+5 = DESCONFIA
table(ESEB2022$Q13) 
#   1   2   3   4   5  97  98 
# 841 113 208 157 654  26   2 

# Exemplo de corte da variável dependente
ESEB2022$ConfiancaCategoria <- 
  cut(ESEB2022$Q13, breaks = c(-Inf, 3, 5, Inf), 
      labels = c("Confia", "Desconfia", NA))
table(ESEB2022$ConfiancaCategoria)

#INserindo na base de dados menor a original e recodificada
ESEB2022Menor$ConfiancaCategoria <- 
  ESEB2022$ConfiancaCategoria

ESEB2022Menor$Q13 <- 
  ESEB2022$Q13

# Ajuste de modelos para cada categoria
modelos_por_categoria <- list() #nicializa uma lista chamada modelos_por_categoria, que será usada para armazenar os modelos ajustados para cada categoria da variável
categorias <- levels(ESEB2022Menor$ConfiancaCategoria) #Obtém os níveis únicos da variável ConfiancaCategoria na base de dados menor (ESEB2022Menor). Cada nível representa uma categoria dessa variável.

for (categoria in categorias) { ##Inicia um loop for que percorre cada categoria na lista de categorias obtidas anteriormente
  subset_data <- subset(ESEB2022Menor, #isolando os dados específicos para a categoria em que o loop está atualmente.
                        ConfiancaCategoria == categoria) 
  #Teste
#>Basicamente criei um subconjuntos de
#>dos dados para cada categoria presente na variável ConfiancaCategoria. 
#>Esses subconjuntos serão usados para ajustar modelos logísticos 
#>binomiais específicos para cada categoria na sequência do código.  

# Verificar se a variável dependente tem pelo menos 
  #dois níveis únicos
unique_levels <- unique(subset_data$ConfiançaN) #Obtém os níveis únicos da variável dependente (ConfiançaN) nos dados específicos para a categoria atual (armazenados em subset_data).
  if (length(unique_levels) >= 2) { #Verifica se há pelo menos dois níveis únicos na variável dependente para a categoria atual.
    #Não há, portanto o cód. if não é executado.
    modelo <- glm(ConfiançaN ~ VotoPres2022 + InfoPol + InfoPol2 + InterPolitica + Idade + Sexo + Escolaridade, 
                  data = subset_data,
                  family = binomial(link = "logit"),
                  control = list(maxit = 100))  # Aumentar o número máximo de iterações
    
    modelos_por_categoria[[categoria]] <- modelo
  } else {
    cat(paste("A categoria", categoria, 
              "da variável dependente tem menos de dois níveis únicos. Ignorando.\n"))
  }
}

# Comparação de modelos usando AIC
aic_values <- sapply(modelos_por_categoria, AIC)

# Imprima os valores do AIC para comparação
print(aic_values)

# Escolha do melhor modelo com base no menor AIC
if (length(modelos_por_categoria) > 0) {
  melhor_modelo <- modelos_por_categoria[[which.min(aic_values)]]
  
  # Resumo do melhor modelo
  summary(melhor_modelo)
} else {
  cat("Nenhum modelo pôde ser ajustado devido à falta de variação na variável dependente nas categorias.\n")
}


# Call:
#   glm(formula = ConfiançaN ~ VotoPres2022 + InfoPol + InfoPol2 + 
#         InterPolitica + Idade + Sexo + Escolaridade, family = binomial(link = "logit"), 
#       data = subset_data, control = list(maxit = 100))
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   -2.072906   0.496376  -4.176 2.97e-05 ***
#   VotoPres2022Não votou          0.990624   0.237909   4.164 3.13e-05 ***
#   VotoPres2022NS/NR              1.348876   0.251786   5.357 8.45e-08 ***
#   VotoPres2022Perdeu-Constestou  1.892971   0.202615   9.343  < 2e-16 ***
#   InfoPolDigital                 0.455955   0.218837   2.084   0.0372 *  
#   InfoPol2Analógico             -0.125243   0.214670  -0.583   0.5596    
# InterPolitica                 -0.292189   0.098465  -2.967   0.0030 ** 
#   Idade                         -0.001798   0.005940  -0.303   0.7622    
# SexoFeminino                  -0.303536   0.166158  -1.827   0.0677 .  
# Escolaridade                   0.065719   0.046095   1.426   0.1540    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1087.0  on 1156  degrees of freedom
# Residual deviance:  961.8  on 1147  degrees of freedom
# (5 observations deleted due to missingness)
# AIC: 981.8
# 
# Number of Fisher Scoring iterations: 5


tab_model(melhor_modelo)
#################
#JUNÇÃO COM O NOVO MODELO########
table(ESEB2022Menor$Confiança)
# Confia Desconfia 
# 954      1019 


table(ESEB2022Menor$ConfiancaCategoria)
# Confia Desconfia      <NA> 
#   1162       811        28  

ESEB2022Menor$Confiança <- ESEB2022Menor$ConfiancaCategoria

table(ESEB2022Menor$Confiança)
# Confia Desconfia      <NA> 
#   1162       811        28 


table(ESEB2022Menor$ConfiançaN)
# 0    1 
# 954 1019 

ESEB2022Menor$ConfiançaN <- 
  ifelse(ESEB2022Menor$Confiança == "Confia", 0,
         ifelse(ESEB2022Menor$Confiança == "Desconfia", 1, NA))

table(ESEB2022Menor$ConfiançaN)
# 0    1 
# 1162  811 

ESEB2022Menor <- ESEB2022Menor[ ,-c(11,12)]

######
colnames(ESEB2018Menor)[colnames(ESEB2018Menor) == 
                          "VotoPres18"] <- "VotoPresidente"

colnames(ESEB2022Menor)[colnames(ESEB2022Menor) == 
                          "VotoPres2022"] <- "VotoPresidente"

colnames(ESEB2018Menor)
colnames(ESEB2022Menor)

library(dplyr)
BaseUnida <- bind_rows(ESEB2018Menor, ESEB2022Menor)


BaseUnida$VotoPresidente <-  
  factor(BaseUnida$VotoPresidente,
         levels = c("Ganhou-N.Constestou", "Perdeu-N.Constestou",
                    "Ganhou-Constestou", "Perdeu-Constestou", 
                    "Não votou", "NS/NR"))

#MODELO FINAL 2##########
mUnido2 <- glm(ConfiançaN ~ VotoPresidente + InfoPol +
                InfoPol2 + InterPolitica + Idade +
                Sexo + Escolaridade, 
              data = BaseUnida,
              family = binomial(link = "logit"))


#Fonte####
Fonte <-  theme(text = element_text(family = "serif", size = 14),
                title = element_text(color = "black"),
                axis.line = element_line(color = "black"), 
                axis.text = element_text(colour = "black", size = rel(1))) 

#voto

plot_model(mUnido2, terms ="VotoPresidente", type = "pred")

Obj2 <- plot_model(mUnido, terms ="VotoPresidente", 
                   type = "pred",
                   colors = 'steelblue1')

Obj2 + coord_flip() +
  labs(x= "VotoPresidente", y= "Predicted",
       title = " ") +
  theme_classic() +Fonte 

colnames(Obj2$data)


Obj2  +
  labs(x= "VotoPresidente", y= "Probabilidades Previstas",
       title = " ") +
  theme_classic() +Fonte +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#GRÁFICOS RELAÇÃO DE CONFIANÇA 2018X 2022###########
BaseUnida <- drop_na(BaseUnida, Confiança)

BaseUnida$ANO <- as.factor(BaseUnida$ANO)

GraConf <- ggplot(BaseUnida, aes(x= Confiança, 
                                 y= (..count..)/sum(..count..), 
                                 fill= ANO)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette="Greys", type = "div")+
  theme_classic() +
  labs(title = "Confiança nas instituições", y="Contagem", x= "Confiança") 


GraConf + 
  theme(text = element_text(family = "serif", size = 13))

GraConf + coord_flip() + Fonte


Fonte <-  theme(plot.background = element_rect(fill = "grey90", 
                                               colour = "black", 
                                               linewidth = 1)) 


GraConf + coord_flip() + theme_dark()
