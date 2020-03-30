library(ggplot2)
require(scales)
library(plyr)

source("util.R")


#---------------------------------
#Scenario without social isolation
#---------------------------------
df <- get_df_prepared()
df$DOMICILIO_VULNERAVEL <- df$POSSUI_GRUPO_RISCO
df <- ddply(df,.(GRUPO_VINCULO,DOMICILIO_VULNERAVEL),summarise,TOTAL=length(GRUPO_VINCULO))
# Compute percentages
df$PORCENTAGEM[df$GRUPO_VINCULO == "Alunos"] <- df$TOTAL[df$GRUPO_VINCULO == "Alunos"] / sum(df$TOTAL[df$GRUPO_VINCULO == "Alunos"])
df$PORCENTAGEM[df$GRUPO_VINCULO == "Servidores"] <- df$TOTAL[df$GRUPO_VINCULO == "Servidores"] / sum(df$TOTAL[df$GRUPO_VINCULO == "Servidores"])
df_sem_isolamento <- df[df$DOMICILIO_VULNERAVEL == TRUE, ]
df_sem_isolamento$CENARIO <- "Isolamento vertical"
#---------------------------------

#---------------------------------
#Scenario without classes and with support to workers
#---------------------------------
df <- get_df_prepared()
df$DOMICILIO_VULNERAVEL <- df$VULNERAVEL_SUSPENSAO_AULAS_E_COM_APOIO_TRABALHADORES
df <- ddply(df,.(GRUPO_VINCULO,DOMICILIO_VULNERAVEL),summarise,TOTAL=length(GRUPO_VINCULO))
# Compute percentages
df$PORCENTAGEM[df$GRUPO_VINCULO == "Alunos"] <- df$TOTAL[df$GRUPO_VINCULO == "Alunos"] / sum(df$TOTAL[df$GRUPO_VINCULO == "Alunos"])
df$PORCENTAGEM[df$GRUPO_VINCULO == "Servidores"] <- df$TOTAL[df$GRUPO_VINCULO == "Servidores"] / sum(df$TOTAL[df$GRUPO_VINCULO == "Servidores"])
df_sem_aulas_e_com_apoio_trablhadores <- df[df$DOMICILIO_VULNERAVEL == TRUE, ]
df_sem_aulas_e_com_apoio_trablhadores$CENARIO <- "Isolamento horizontal com ajuda financeira aos trabalhadores informais"
#---------------------------------

#---------------------------------
#Scenario without classes and without support to workers
#---------------------------------
df <- get_df_prepared()
df$DOMICILIO_VULNERAVEL <- df$VULNERAVEL_SUSPENSAO_AULAS_E_SEM_APOIO_TRABALHADORES
df <- ddply(df,.(GRUPO_VINCULO,DOMICILIO_VULNERAVEL),summarise,TOTAL=length(GRUPO_VINCULO))
# Compute percentages
df$PORCENTAGEM[df$GRUPO_VINCULO == "Alunos"] <- df$TOTAL[df$GRUPO_VINCULO == "Alunos"] / sum(df$TOTAL[df$GRUPO_VINCULO == "Alunos"])
df$PORCENTAGEM[df$GRUPO_VINCULO == "Servidores"] <- df$TOTAL[df$GRUPO_VINCULO == "Servidores"] / sum(df$TOTAL[df$GRUPO_VINCULO == "Servidores"])
df_sem_aulas_e_sem_apoio_trablhadores <- df[df$DOMICILIO_VULNERAVEL == TRUE, ]
df_sem_aulas_e_sem_apoio_trablhadores$CENARIO <- "Isolamento horizontal sem ajuda financeira aos trabalhadores informais"
#---------------------------------

df <- rbind(df_sem_isolamento, df_sem_aulas_e_com_apoio_trablhadores, df_sem_aulas_e_sem_apoio_trablhadores)

df$LABEL <- paste0(round(df$PORCENTAGEM * 100), "%")

cbbPalette <- c("#abdda4", "#2b83ba", "#d7191c")

df$GRUPO_VINCULO[df$GRUPO_VINCULO == "Alunos"] <- "Domicílios dos alunos"
df$GRUPO_VINCULO[df$GRUPO_VINCULO == "Servidores"] <- "Domicílios dos servidores"

#---------------------------------
# Plot chart: alunos e servidores
#---------------------------------
ggplot(df, aes(x=CENARIO, y=PORCENTAGEM, fill=CENARIO)) +
  facet_wrap(facets = vars(GRUPO_VINCULO), ncol = 2) +
  geom_bar(stat="identity") +
  coord_flip()+
  theme(text = element_text(size=23), 
        legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        axis.text.y=element_blank())+
  guides(fill=guide_legend(title="Cenários: ",nrow=3,byrow=TRUE)) +
  labs(y="Porcentagem de domicílios vulneráveis", x="Cenários") +
  scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("Domicílios de alunos e servidores do\nIFNMG vulneráveis à Covid-19\n") +
  scale_fill_manual(values=cbbPalette) +
  #geom_text(position = position_stack(vjust = 0.5))
  geom_label(position = position_stack(vjust = 0.5), aes(label=LABEL), size=6, show.legend = FALSE) 
ggsave("domicilios_vulneraveis.jpg", width = 12, height = 7)

#---------------------------------
# Plot chart: alunos
#---------------------------------

df <- df[df$GRUPO_VINCULO == "Domicílios dos alunos", ] 

#Domicílios de alunos do IFNMG vulneráveis à Covid-19
ggplot(df, aes(x=CENARIO, y=PORCENTAGEM, fill=CENARIO)) +
  geom_bar(stat="identity") +
  coord_flip()+
  theme(text = element_text(size=23), 
        legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        axis.text.y=element_blank())+
  guides(fill=guide_legend(title="Cenários: ",nrow=3,byrow=TRUE)) +
  labs(y="Porcentagem de domicílios vulneráveis", x="Cenários") +
  scale_y_continuous(labels = percent, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_fill_manual(values=cbbPalette) +
  #geom_text(position = position_stack(vjust = 0.5))
  geom_label(position = position_stack(vjust = 0.5), aes(label=LABEL), size=6, show.legend = FALSE) 
ggsave("domicilios_vulneraveis_alunos.jpg", width = 11, height = 7)