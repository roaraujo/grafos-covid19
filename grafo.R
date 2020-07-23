library(visNetwork)
library(igraph)


data <- read.table('C:/Users/Rodrigo Araujo/Documents/Pesquisa e Planejamento 1/Trabalho final/pesquisa_covid.csv', sep = ',', header=TRUE, fileEncoding = "UTF-8")

data <- data[1:30, ]

data$dia_nasc <- as.factor(data$dia_nasc)
data$genero <- as.factor(data$genero)
data$escolaridade <- as.factor(data$escolaridade)
data$instituto <- as.factor(data$instituto)
data$conhece_oms <- as.factor(data$conhece_oms)
data$conhece_mandetta <- as.factor(data$conhece_mandetta)
data$conhece_terra <- as.factor(data$conhece_terra)
data$area <- as.factor(data$area)




######## Separando os dados em par e impar


data_par <- data %>% filter(dia_nasc == 'Par') 

data_impar <- data %>% filter(dia_nasc == 'Impar')


########### Tratamento para os dados da manchete 'mascara' ########################


data_manchete_mascara_1 <- data_par %>% dplyr::select(mascara_oms, genero, acompanha_noticia, dia_nasc, escolaridade, instituto, conhece_oms, conhece_terra, area)
names(data_manchete_mascara_1)[1] <- c("manchete_mascara")
data_manchete_mascara_1['autor'] <- 'Adhanom'


data_manchete_mascara_2 <- data_impar %>% dplyr::select(mascara_terra, genero, acompanha_noticia, dia_nasc, escolaridade, instituto, conhece_oms,conhece_terra, area)
names(data_manchete_mascara_2)[1] <- c("manchete_mascara")
data_manchete_mascara_2['autor'] <- 'Terra'

data_manchete_mascara <- rbind(data_manchete_mascara_1, data_manchete_mascara_2)

rm(data_manchete_mascara_1, data_manchete_mascara_2, data_impar, data_par, data)




data_manchete_mascara$manchete_mascara <- as.factor(data_manchete_mascara$manchete_mascara)
data_manchete_mascara$acompanha_noticia <- as.factor(data_manchete_mascara$acompanha_noticia)

nodes1 <- data_manchete_mascara %>% distinct(escolaridade)

nodes2 <- data_manchete_mascara %>% distinct(autor)

nodes3 <- data_manchete_mascara %>% distinct(area)

nodes4 <- data_manchete_mascara %>% distinct(manchete_mascara)


names(nodes1)[1] <- c("id")
names(nodes2)[1] <- c("id")
names(nodes3)[1] <- c("id")
names(nodes4)[1] <- c("id")

nodes <- rbind(nodes1, nodes2, nodes3, nodes4, fill = TRUE)

nodes <- nodes %>% filter(id != "NA")
nodes <- nodes %>% filter(id != "")




autor_area <- data.frame(from = data_manchete_mascara$autor, to = data_manchete_mascara$area)

area_escolaridade <- data.frame(from = data_manchete_mascara$area, to = data_manchete_mascara$escolaridade)

escolaridade_acompanha_noticia <- data.frame(from = data_manchete_mascara$escolaridade, to = data_manchete_mascara$manchete_mascara)


g_mascara <- rbind(autor_area, area_escolaridade, escolaridade_acompanha_noticia, fill = TRUE)

g_mascara <- g_mascara %>% filter(from != "NA")
g_mascara <- g_mascara %>% filter(from != "")




nodes_ <- data.frame(nodes, group =  c(rep("escolaridade", 3), rep("autor", 2), rep("area", 3), rep("manchete_mascara", 5)), 
                        
                        shape = c(rep("dot", 5), rep("dot", 3), rep("dot", 2), rep("dot", 3))
                        
)


a <- visNetwork(nodes_, g_mascara, height = "900px", width = "90%") %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>% 
  
  visGroups(groupname = "manchete_mascara", color = "orange") %>% 
  visPhysics(stabilization = TRUE)

visLegend(a, main="Legend", position="right", ncol=2) 







