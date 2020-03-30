get_df_base<-function()
{
  df_base <- base_original
  df_base$TOTAL_MORADORES <- df_base$IDADE_0_9 + df_base$IDADE_10_39 + df_base$IDADE_40_59 + df_base$IDADE_MAIS_59
  df_base$POSSUI_GRUPO_RISCO <- (df_base$IDADE_MAIS_59 > 0 | df_base$DOENCA_CRONICA > 0)
  df_base$VULNERAVEL_SUSPENSAO_AULAS_E_COM_APOIO_TRABALHADORES <- (df_base$POSSUI_GRUPO_RISCO & df_base$SERVICOS_ESSENCIAIS > 0)
  df_base$VULNERAVEL_SUSPENSAO_AULAS_E_SEM_APOIO_TRABALHADORES <- (df_base$POSSUI_GRUPO_RISCO & (df_base$SERVICOS_ESSENCIAIS > 0 | df_base$INFORMAIS_AUTONOMOS > 0))  
  df_base$GRUPO_VINCULO[df_base$VINCULO == "Professor" | df_base$VINCULO == "Tecnico administrativo"] = "Servidores"
  df_base$GRUPO_VINCULO[df_base$VINCULO == "Aluno"] = "Alunos"
  df_base
}

get_df_prepared<-function()
{
  df <- get_df_base()
  df <- clear_base(df)
  df
}

clear_base<-function(df)
{
  df_temp <- remove_duplicated_email(df)
  df_temp <- remove_invalid_vinculo(df_temp)
  df_temp <- remove_invalid_total_moradores(df_temp)
  df_temp <- remove_invalid_estudante(df_temp)
  df_temp <- remove_invalid_professor(df_temp)
  df_temp <- remove_outliers(df_temp)
  df_temp
}

remove_duplicated_email<-function(df)
{
  df_temp <- df[!duplicated(df$EMAIL) | df$EMAIL == "", ] 
  df_temp
}

remove_invalid_total_moradores<-function(df)
{
  df_temp <- df[df$TOTAL_MORADORES >= df$DOENCA_CRONICA & df$TOTAL_MORADORES >= df$ESTUDANTES & df$TOTAL_MORADORES >= df$PROFESSORES & df$TOTAL_MORADORES >= df$SERVICOS_ESSENCIAIS & df$TOTAL_MORADORES >= df$INFORMAIS_AUTONOMOS, ] 
  df_temp <- df_temp[df_temp$TOTAL_MORADORES > 0, ] 
  df_temp
}

remove_outliers<-function(df)
{
  df_temp <- df
  
  summary(df_temp$TOTAL_MORADORES)
  #boxplot(df_temp$TOTAL_MORADORES)
  df_temp <- df_temp[df_temp$TOTAL_MORADORES <= 25, ] 
  summary(df_temp$TOTAL_MORADORES)
  #boxplot(df_temp$TOTAL_MORADORES)
  
  summary(df_temp$QUARTOS_DOMICILIO)
  #boxplot(df_temp$QUARTOS_DOMICILIO)
  
  df_temp
}

remove_invalid_estudante<-function(df)
{
  df_temp <- df[(df$VINCULO == "Aluno" & df$ESTUDANTES >= 1) | (df$VINCULO != "Aluno"), ]
  df_temp
}

remove_invalid_professor<-function(df)
{
  df_temp <- df[(df$VINCULO == "Professor" & df$PROFESSORES >= 1) | (df$VINCULO != "Professor"), ]
  df_temp
}

remove_invalid_vinculo<-function(df)
{
  df_temp <- df[df$VINCULO != "Nenhum", ]
  df_temp
}