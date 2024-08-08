#Artigo para workshop de CP UEM

library(tidyverse)
library(haven)
LAPOP2019 <- read_dta("Bra2019.dta")

  ### Recodificação para criação dos perfis de desafeição.
summary(LAPOP2019$braantip1)  

LAPOP2019$AntPT <- as.numeric(LAPOP2019$braantip1)
LAPOP2019$AntPT <- memisc::recode(LAPOP2019$AntPT, 1<-1, 0 <- c(2:10))
  
summary(LAPOP2019$AntPT)
  
summary(LAPOP2019$braantip2) 
LAPOP2019$AntPSDB <- as.numeric(LAPOP2019$braantip2)
LAPOP2019$AntPSDB <- memisc::recode(LAPOP2019$AntPSDB, 10<-1, 0 <- c(2:10))
  
summary(LAPOP2019$AntPSDB)
  
LAPOP2019$AntPMDB <- as.numeric(LAPOP2019$braantip3)
LAPOP2019$AntPMDB <- memisc::recode(LAPOP2019$AntPMDB, 100<-1, 0 <- c(2:10))
  
summary(LAPOP2019$AntPMDB)
  
LAPOP2019$AntPartido <- (LAPOP2019$AntPT + LAPOP2019$AntPSDB + 
                             LAPOP2019$AntPMDB)
  
memisc::Table(LAPOP2019$AntPartido, percent=T) 
  
freq(LAPOP2019$AntPartido)
  
table(LAPOP2019$AntPT, LAPOP2019$AntPMDB, LAPOP2019$AntPSDB)

##########
### Recodificação para criação dos perfis de desafeição.
#AntPT, 1<-1, 0 <- c(2:10))
#AntPMDB, 100<-1, 0 <- c(2:10))

#desafeto misto#####

#desafeto generalizado#####

#Desafeto partidário
#Como não estava entendendo como foi a recategorização fiz no excel
# Rótulos de Linha	Contagem de Generalizado
# Desafeto PMDB	9,8%
# Desafeto PSDB	5,6%
# Desafeto PT	32,4%
# Desafeto_misto	23,6%
# Generalizado	28,6%


########
#gráfico

### Recodificação para criação da medida de tolerância.
  
summary(LAPOP2019$braparap1)
  LAPOP2019$IntPT <- as.numeric(LAPOP2019$braparap1)
  LAPOP2019$IntPT <- memisc::recode(LAPOP2019$IntPT, 5<-1, 0 <- c(2:10))
  summary(LAPOP2019$IntPT)
  
  LAPOP2019$IntPSDB <- as.numeric(LAPOP2019$braparap2)
  LAPOP2019$IntPSDB <- memisc::recode(LAPOP2019$IntPSDB, 50<-1, 0 <- c(2:10))
  summary(LAPOP2019$IntPSDB)
  
  LAPOP2019$IntPMDB <- as.numeric(LAPOP2019$braparap3)
  LAPOP2019$IntPMDB <- memisc::recode(LAPOP2019$IntPMDB, 500<-1, 0 <- c(2:10))
  summary(LAPOP2019$IntPMDB)
  
  LAPOP2019$Intoler <- (LAPOP2019$IntPT + LAPOP2019$IntPSDB + LAPOP2019$IntPMDB)
  memisc::Table(LAPOP2019$Intoler, percent=T)
  
  #### Variáveis controles
  
  #Sexo
LAPOP2019$Sexo <- Bra2019$sex
LAPOP2019$Sexo <- as.numeric(LAPOP2019$Sexo) 


  #LAPOP2019$Sexo <- as.numeric(LAPOP2019$sex)
  LAPOP2019$Sexo <- memisc::recode(LAPOP2019$Sexo, 1<-1, 0<-2) # Alteração de masculino para 1 e feminino para 0.
  LAPOP2019$Sexo <- as.factor(LAPOP2019$Sexo) # conversão para fator.
  
  #Idade
  LAPOP2019$Idade <-as.numeric(LAPOP2019$q2) # conversão em numérica.
  
  #Escolaridade
  LAPOP2019$Escolaridade <- Bra2019$ed
  
  LAPOP2019$ed # sem recodificações.
  
  ### Variável dependente.
LAPOP2019$Voto <- Bra2019$vb3n
  
  LAPOP2019$Voto <- as.numeric(LAPOP2019$Voto)# Conversão em numérica.
  LAPOP2019$Voto <- memisc::recode(LAPOP2019$Voto, 1<-1501, 2<-1502, 
                           3<-1503, 4<-c(0,97,1504,1504,1506,
                                         1508,1509,1510, 1577)) # redução agrupando "outros".
  LAPOP2019$VotoFator <- factor(LAPOP2019$Voto, 
                                labels = c("Bolsonaro", 
                                           "Haddad","Ciro","Outros")) # conversão em fator atribuindo rótulos.
  
  LAPOP2019$VotoDic <- memisc::recode(LAPOP2019$Voto, 1<-1, 0 <- c(2:4)) #Dicotomização para voto em Bolsonaro.
###ok o código  
colnames(LAPOP2019)
LAPOP2019$`Antipartidário generalizado intolerante` <- as.numeric(
  LAPOP2019$`Antipartidário generalizado intolerante`
)

summary(LAPOP2019$`Antipartidário generalizado intolerante`)

LAPOP2019$`Antipartidário generalizado tolerante` <- as.numeric(
  LAPOP2019$`Antipartidário generalizado tolerante`
)

LAPOP2019$`Antipetista intolerante` <- as.numeric(
  LAPOP2019$`Antipetista intolerante`
)

LAPOP2019$`Antipetista tolerante` <- as.numeric(
  LAPOP2019$`Antipetista tolerante`
)


modelo <- glm(VotoDic ~ `Antipetista tolerante` + `Antipetista intolerante` +
                `Antipartidário generalizado tolerante` + 
                `Antipartidário generalizado intolerante` +
                  Sexo + 
                Idade +
                Escolaridade, 
                data = LAPOP2019,
                family = binomial(link = "logit"))
  
  
  summary(mUnido)
  
  
  
  obj3 <- tab_model(mUnido, show.ci = F, auto.label = F, 
                    p.style = "star")
  
  
  library(writexl)
  #salvando
  writexl::write_xlsx(LAPOP2019, path = "LAPOP2019.xlsx")
  