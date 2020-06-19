library(dplyr)

#importar crimes e filtrar para homicídios
library(readxl)
indicadoressegurancapublicauffev20 <- read_excel("indicadoressegurancapublicauffev20.xlsx") #retirado do Ministério da Justiça
View(indicadoressegurancapublicauffev20)
dados<- indicadoressegurancapublicauffev20
colnames(dados) <- c("UF","Crime","Ano","Mês","Vítimas")
dadoshom<- filter(dados, Ano == "2019" & Crime == "Homicídio doloso")

#importar pop e regiões e abb 
library(readxl)
POP <- read_excel("POP.xlsx") #retirado do IBGE
View(POP)
colnames(POP) <- c("Sigla", "UF", "Pop", "Região")


#mesclar database
dados19 <- merge(dadoshom, POP, by = "UF")
dados19[2:4] <- NULL

#agrupar todas entradas diferentes de Estado em um só
df_br<- dados19 %>% group_by(UF, Sigla, Pop, Região) %>% summarise(Homicídios = sum(Vítimas))

#taxa de homicídio por 100k
df_br<- mutate(df_br, taxa = Homicídios/Pop*100000)

#média da taxa do país
r<- summarize(df_br, rate = sum(df_br$Homicídios) / sum(df_br$Pop) * 10^6) %>% .$rate


#incluindo taxa média americana
library(dslabs)
rUS <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

#criando o gráfico
library(ggthemes)
library(ggrepel)
library(ggplot2)


df_br %>% 
  ggplot(aes(Pop/10^6, Homicídios, label = Sigla)) + #defines the variables
  geom_abline(intercept = log10(r), lty = 2, color = "darkred") + #creates the rate line
  #geom_abline(intercept = log10(rUS), lty = 2, color = "darkblue") + #creates the rate line for USA
  geom_point(aes(col = Região), size = 3) + #defines the colors according to Region (col and color are the same)
  geom_text_repel()+ #stops letters overlapping
  scale_x_log10()+ #turns x into log10
  scale_y_log10()+ #turns y into log10
  xlab("População em milhão (escala log)")+ #changes the label of x
  ylab("Total de homicídios (escala log)")+ #changes the label of y
  ggtitle("Homicídios Dolosos no Brasil em 2019")+ #gives a title to the graph
  theme_economist() #uses the theme of The Economist
