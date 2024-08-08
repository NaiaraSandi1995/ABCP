############DESCRITIVAS

#GRÁFICO1###### 
table1 <-  data.frame(
Antipartidarismo = c("Desafeto ao PT", "Desafeto ao PSDB",
                       "Desfaeto ao MDB","Desafeto Misto",
                       "Desafeto Generalizado"),
Porcentagens = c(31.7, 5.4, 9.9, 23.7, 29.2))

library(ggplot2)
grafico1 <- ggplot(table1, aes(x = Antipartidarismo, 
                              y = Porcentagens, 
                              fill = Antipartidarismo))+
    geom_bar(stat = "identity") +
  labs(title = "Porcentagem de Desafetos por Categoria",
       x = "Antipartidarismo",
       y = "Porcentagens") +
  theme_minimal()

grafico17

library(scales) 
library(tidyverse)

LAPOP2019$AntPartido3C <- as.factor(LAPOP2019$AntPartido3)
LAPOP2019 <- drop_na(LAPOP2019, AntPartido3C)

Grafico2 <-  ggplot(LAPOP2019, 
                    aes(x= AntPartido3C, 
                        y=(..count..)/sum(..count..), 
                        fill= AntPartido3C),
       ) +
  geom_bar()+
  scale_x_discrete(labels = c("Desafeto ao PT", "Desafeto ao PSDB",
                              "Desfaeto ao MDB","Desafeto Misto",
                              "Desafeto Generalizado"))+
  scale_y_continuous(labels = percent) +
theme_test(base_size = 10) +
  geom_text(aes(y = ((..count..)/sum(..count..)),
                label = percent(round((..count..)/sum(..count..),3))),
            stat = "count", vjust = -0.25, 
            color="black",size=3.5,
            family= "serif")

Gráfico3 <- Grafico2 +  scale_fill_brewer(palette="Blues",
                                          type = "qua")+
  
  labs(title = expression(bold("Porcentagem de Antipartidarismos"))  ,
       x = "Antipartidarismo",
       y = "Porcentagens")+ 
  theme(legend.position = "none")

Gráfico3 + facet_wrap( ~ LAPOP2019$sex)

#####
GraficoFinal <- 
  LAPOP2019 %>%
  count(AntPartido3C) %>%
  arrange(desc(n)) %>%
  mutate(AntPartido3C = factor(AntPartido3C, levels = .$AntPartido3C)) %>%
  ggplot(aes(x = AntPartido3C, y = n/sum(n), fill = AntPartido3C)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Desafeto ao PT","Desafeto Generalizado",
                              "Desafeto Misto","Desfaeto ao MDB" ,
                              "Desafeto ao PSDB"
                              )) +
  scale_y_continuous(labels = scales::percent_format(accuracy =1)) +
  theme_minimal(base_size = 10) 

  GraficoFinal <- GraficoFinal +  scale_fill_brewer(palette = "Blues", type = "qua") +
  labs(title = expression(bold("Porcentagem de Antipartidarismos")),
       x = "Antipartidarismo",
       y = "Porcentagens") +
  theme_classic() +
  theme(legend.position = "none") +
  Fonte +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# +
#   geom_text(aes(y = n/sum(n), label = scales::percent(n/sum(n), accuracy = 0.1)),
#             stat = "identity", vjust = -0.25, 
#             color = "black", size = 3.5, family = "serif")

############################

#GRÁFICO2#########

LAPOP2019$Intoler3C <-  as.factor(LAPOP2019$Intoler3)

LAPOP2019 <- drop_na(LAPOP2019, Intoler3C)


GraficoFinal2 <- 
  LAPOP2019 %>%
  count(Intoler3C) %>%
  arrange(desc(n)) %>%
  mutate(Intoler3C = factor(Intoler3C, levels = .$Intoler3C)) %>%
  ggplot(aes(x = Intoler3C, y = n/sum(n), fill = Intoler3C)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Intolerância ao PT", "Intolerância Generalizada",
                              "Intolerância Mista",
                              "Intolerância ao MDB",
                              "Intolerância ao PSDB")) +
  scale_y_continuous(labels = scales::percent_format(accuracy =2)) +
  theme_minimal(base_size = 10) 


  GraficoFinal2 <- GraficoFinal2 +  scale_fill_brewer(palette = "Blues", type = "qua") +
  labs(title = expression(bold("Porcentagem de Intolerância partidária")),
       x = "Intolerância partidária",
       y = "Porcentagens") +
  theme_classic() +
  theme(legend.position = "none")+
  Fonte +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  Fonte <-  theme(text = element_text(family = "serif", size = 11),
                  title = element_text(color= "black"),
                  axis.line = element_line(color = "black"), 
                  axis.text = element_text(colour = "black", size = rel(1))) 

  
  
library(gridExtra)
  
  GraficoFinal2
  
figura_completa <- grid.arrange(GraficoFinal, GraficoFinal2,  nrow=1)


############
library(tidyverse)

# Criar o data frame com os dados fornecidos
dados <- data.frame(
  Categoria = c("Antipetista tolerante", "Antipetista intolerante", 
                "Antipartidário generalizado tolerante", "Antipartidário generalizado intolerante"),
  Percentual = c(12.5, 39.5, 26.7, 21.3)
)

# Ajustar o nome da coluna de porcentagem
dados <- dados %>%
  rename(n = Percentual)

# Criar o gráfico
GraficoFinal3 <- 
  dados %>%
  arrange(desc(n)) %>%
  mutate(Categoria = factor(Categoria, levels = Categoria)) %>%
  ggplot(aes(x = Categoria, y = n/sum(n), fill = Categoria)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Antipetista intolerante",
                              "Antipartidário Gen. tolerante",
                              "Antipartidário Gen. intolerante",
                              "Antipetista tolerante")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 2)) +
  scale_fill_brewer(palette = "Blues", type = "qua") +
  labs(title = expression(bold("Porcentagem de Antipartidarismo somado a tolerância")),
       x = "Antipartismo e Tolerância",
       y = "Porcentagens") +
  theme_minimal(base_size = 10) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exibir o gráfico
GraficoFinal3 + Fonte

