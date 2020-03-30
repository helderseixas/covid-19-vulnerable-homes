library(ggplot2)
library(plyr)

source("util.R")

df <- get_df_prepared()

df <- df[df$GRUPO_VINCULO == "Alunos", ] 

df$POSSUI_ENFERMO <- df$DOENCA_CRONICA > 0

df <- ddply(df,.(POSSUI_ENFERMO),summarise,TOTAL=length(POSSUI_ENFERMO))

df$POSSUI_ENFERMO[df$POSSUI_ENFERMO == TRUE] = "Possui morador com enfermidade de risco"
df$POSSUI_ENFERMO[df$POSSUI_ENFERMO == FALSE] = "Não possui morador com enfermidade de risco"


# Compute percentages
df$fraction <- df$TOTAL / sum(df$TOTAL)

# Compute the cumulative percentages (top of each rectangle)
df$ymax <- cumsum(df$fraction)

# Compute the bottom of each rectangle
df$ymin <- c(0, head(df$ymax, n=-1))

# Compute label position
df$labelPosition <- (df$ymax + df$ymin) / 2

# Compute a good label
df$label <- paste0(round(df$fraction*100), "%")

cbbPalette <- c("#abdda4", "#d7191c")

#---------------------------------
# Plot chart: alunos -------------
#---------------------------------
#Proporção de domicílios de alunos do IFNMG com moradores portadores de enfermidade de risco à Covid-19
ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=POSSUI_ENFERMO)) +
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
ggsave("enfermos.jpg", width = 11, height = 7)