library(ggplot2)
library(plyr)

source("util.R")

#---------
#estudante
#---------
df_estudante <- get_df_prepared()
df_estudante <- df_estudante[df_estudante$GRUPO_VINCULO == "Alunos", ] 
df_estudante$ELO_CONTAMINACAO <- "Estudante"
df_estudante$POSSUI_ELO_CONTAMINACAO <- TRUE
df_estudante <- ddply(df_estudante,.(ELO_CONTAMINACAO,POSSUI_ELO_CONTAMINACAO),summarise,TOTAL=length(ELO_CONTAMINACAO))
df_estudante$fraction <- df_estudante$TOTAL / sum(df_estudante$TOTAL)
df_estudante$ymax <- cumsum(df_estudante$fraction)
df_estudante$ymin <- c(0, head(df_estudante$ymax, n=-1))
df_estudante$labelPosition <- (df_estudante$ymax + df_estudante$ymin) / 2
df_estudante$label <- paste0(round(df_estudante$fraction*100), "%")
#---------

#---------
#trabalhador essencial
#---------
df_trabalhador_essencial <- get_df_prepared()
df_trabalhador_essencial <- df_trabalhador_essencial[df_trabalhador_essencial$GRUPO_VINCULO == "Alunos", ] 
df_trabalhador_essencial$ELO_CONTAMINACAO <- "Trabalhador de serviço essencial"
df_trabalhador_essencial$POSSUI_ELO_CONTAMINACAO <- df_trabalhador_essencial$SERVICOS_ESSENCIAIS > 0
df_trabalhador_essencial <- ddply(df_trabalhador_essencial,.(ELO_CONTAMINACAO,POSSUI_ELO_CONTAMINACAO),summarise,TOTAL=length(ELO_CONTAMINACAO))
df_trabalhador_essencial$fraction <- df_trabalhador_essencial$TOTAL / sum(df_trabalhador_essencial$TOTAL)
df_trabalhador_essencial$ymax <- cumsum(df_trabalhador_essencial$fraction)
df_trabalhador_essencial$ymin <- c(0, head(df_trabalhador_essencial$ymax, n=-1))
df_trabalhador_essencial$labelPosition <- (df_trabalhador_essencial$ymax + df_trabalhador_essencial$ymin) / 2
df_trabalhador_essencial$label <- paste0(round(df_trabalhador_essencial$fraction*100), "%")
#---------

#---------
#trabalhador informal
#---------
df_trabalhador_informal <- get_df_prepared()
df_trabalhador_informal <- df_trabalhador_informal[df_trabalhador_informal$GRUPO_VINCULO == "Alunos", ] 
df_trabalhador_informal$ELO_CONTAMINACAO <- "Trabalhador informal"
df_trabalhador_informal$POSSUI_ELO_CONTAMINACAO <- df_trabalhador_informal$INFORMAIS_AUTONOMOS > 0
df_trabalhador_informal <- ddply(df_trabalhador_informal,.(ELO_CONTAMINACAO,POSSUI_ELO_CONTAMINACAO),summarise,TOTAL=length(ELO_CONTAMINACAO))
df_trabalhador_informal$fraction <- df_trabalhador_informal$TOTAL / sum(df_trabalhador_informal$TOTAL)
df_trabalhador_informal$ymax <- cumsum(df_trabalhador_informal$fraction)
df_trabalhador_informal$ymin <- c(0, head(df_trabalhador_informal$ymax, n=-1))
df_trabalhador_informal$labelPosition <- (df_trabalhador_informal$ymax + df_trabalhador_informal$ymin) / 2
df_trabalhador_informal$label <- paste0(round(df_trabalhador_informal$fraction*100), "%")
#---------

df <- rbind(df_estudante, df_trabalhador_essencial, df_trabalhador_informal)

cbbPalette <- c("#abdda4", "#d7191c")

df$POSSUI_ELO_CONTAMINACAO[df$POSSUI_ELO_CONTAMINACAO == TRUE] = "Possui morador classificado potencial vetor de transmissão"
df$POSSUI_ELO_CONTAMINACAO[df$POSSUI_ELO_CONTAMINACAO == FALSE] = "Não possui morador classificado potencial vetor de transmissão"

#---------------------------------
# Plot chart: alunos e servidores
#---------------------------------
#Proporção de domicílios de alunos do IFNMG com potenciais vetores de transmissão da Covid-19
ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=POSSUI_ELO_CONTAMINACAO)) +
  facet_wrap(facets = vars(ELO_CONTAMINACAO), ncol = 3) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6, show.legend = FALSE) +
  scale_fill_manual(values=cbbPalette) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  guides(fill=guide_legend(title="Domicílio: ",nrow=2,byrow=TRUE)) +
  theme(text = element_text(size=23), 
        legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
ggsave("vetores_transmissao.jpg", width = 12, height = 5.5)