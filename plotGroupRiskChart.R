library(ggplot2)
library(plyr)

source("util.R")

df <- get_df_prepared()

df <- ddply(df,.(GRUPO_VINCULO,POSSUI_GRUPO_RISCO),summarise,TOTAL=length(GRUPO_VINCULO))

df$POSSUI_GRUPO_RISCO[df$POSSUI_GRUPO_RISCO == TRUE] = "Possui morador no grupo de risco"
df$POSSUI_GRUPO_RISCO[df$POSSUI_GRUPO_RISCO == FALSE] = "Não possui morador no grupo de risco"


# Compute percentages
df$fraction[df$GRUPO_VINCULO == "Alunos"] <- df$TOTAL[df$GRUPO_VINCULO == "Alunos"] / sum(df$TOTAL[df$GRUPO_VINCULO == "Alunos"])
df$fraction[df$GRUPO_VINCULO == "Servidores"] <- df$TOTAL[df$GRUPO_VINCULO == "Servidores"] / sum(df$TOTAL[df$GRUPO_VINCULO == "Servidores"])

# Compute the cumulative percentages (top of each rectangle)
df$ymax[df$GRUPO_VINCULO == "Alunos"] <- cumsum(df$fraction[df$GRUPO_VINCULO == "Alunos"])
df$ymax[df$GRUPO_VINCULO == "Servidores"] <- cumsum(df$fraction[df$GRUPO_VINCULO == "Servidores"])


# Compute the bottom of each rectangle
df$ymin[df$GRUPO_VINCULO == "Alunos"] <- c(0, head(df$ymax[df$GRUPO_VINCULO == "Alunos"], n=-1))
df$ymin[df$GRUPO_VINCULO == "Servidores"] <- c(0, head(df$ymax[df$GRUPO_VINCULO == "Servidores"], n=-1))

# Compute label position
df$labelPosition[df$GRUPO_VINCULO == "Alunos"] <- (df$ymax[df$GRUPO_VINCULO == "Alunos"] + df$ymin[df$GRUPO_VINCULO == "Alunos"]) / 2
df$labelPosition[df$GRUPO_VINCULO == "Servidores"] <- (df$ymax[df$GRUPO_VINCULO == "Servidores"] + df$ymin[df$GRUPO_VINCULO == "Servidores"]) / 2

# Compute a good label
df$label[df$GRUPO_VINCULO == "Alunos"] <- paste0(round(df$fraction[df$GRUPO_VINCULO == "Alunos"]*100), "%")
df$label[df$GRUPO_VINCULO == "Servidores"] <- paste0(round(df$fraction[df$GRUPO_VINCULO == "Servidores"]*100), "%")

cbbPalette <- c("#abdda4", "#d7191c")

df$GRUPO_VINCULO[df$GRUPO_VINCULO == "Alunos"] <- "Domicílios dos alunos"
df$GRUPO_VINCULO[df$GRUPO_VINCULO == "Servidores"] <- "Domicílios dos servidores"
#---------------------------------
# Plot chart: alunos e servidores
#---------------------------------
ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=POSSUI_GRUPO_RISCO)) +
  facet_wrap(facets = vars(GRUPO_VINCULO), ncol = 2) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6, show.legend = FALSE) +
  scale_fill_manual(values=cbbPalette) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  guides(fill=guide_legend(title="Possui alguma pessoa do grupo de risco em seu domicílio: ",nrow=2,byrow=TRUE)) +
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
        panel.grid.minor = element_blank()) +
  ggtitle("Alunos e servidores do IFNMG que possuem alguma pessoa\ndo grupo de risco da Covid-19 em seu domicílio\n")
ggsave("grupo_risco.jpg", width = 12, height = 7)

#---------------------------------
# Plot chart: alunos -------------
#---------------------------------

df <- df[df$GRUPO_VINCULO == "Domicílios dos alunos", ] 

#Proporção de domicílios de alunos do IFNMG com moradores que pertencem ao grupo de risco da Covid-19
ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=POSSUI_GRUPO_RISCO)) +
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
ggsave("grupo_risco_alunos.jpg", width = 11, height = 7)