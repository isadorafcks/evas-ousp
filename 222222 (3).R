#Isadora russo friedericks

#PROJETO PUB 2023/2024

##bases de dados iniciais

cult <- read.csv("C:/Users/isado/cult.csv")
pontupapfe <- read.csv("C:/Users/isado/pontupapfe.csv")

### bibliotecas##

#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("lmtest")
#install.packages("openxlsx")

library(openxlsx)
library(lmtest)
library(stringr)
library(readxl)
library(dplyr)
library(tidyverse)


########################## Filtrar dados de cult #########################

cult_filtrado <- cult %>%
  
  filter(Ano.de.Ingresso >= 2010 & Ano.de.Ingresso <= 2015) %>%
  
  mutate(Valor = as.numeric(Valor)) %>%
  
  group_by(ID, Ano.Pagamento, Tipo.Bonifica‡Æo.Aluno) %>%
  
  summarize(soma_valor = sum(Valor, na.rm = TRUE)) %>%
  
  group_by(ID, Ano.Pagamento, soma_valor) %>%
  
  summarise(Tipos_Agregados = toString(unique(Tipo.Bonifica‡Æo.Aluno)))

###################### Filtrar dados de papfe #############################

papfe <- pontupapfe %>%
  
  filter(Ano.de.ingresso >= 2011 & Ano.de.ingresso <= 2015) %>%
  
  rename(Ano.de.Ingresso = Ano.de.ingresso) %>%
  
  merge(cult_filtrado, by = "ID", all = TRUE) %>%
  
  select(-Ano.PAPFE) %>%
  
  mutate(Bolsa = ifelse(is.na(soma_valor) | soma_valor < 1, 0, 1)) %>%
  
  filter(is.na(Ano.Pagamento) | (Ano.Pagamento >= 2011 & Ano.Pagamento <= 2022))



################# Carregar nova base de dados (informa‡äes sobre os estudantes ) #############

PET_FEA_V2_ <- read_excel("C:/Users/isado/Downloads/PET_FEA_V2_.xlsx")

estudante <- PET_FEA_V2_ 

# Ajustar nome das colunas

colnames(estudante) <- estudante[1, ]
estudante <- estudante[-1, ]
estudante <- estudante %>%
  select(-49) %>%
  rename(PS = `Processo Seletivo`) %>%
  filter(`PS` != "SiSU") %>%
  mutate(`Ano de Ingresso` = as.numeric(`Ano de Ingresso`)) %>%
  
  
  filter(between(`Ano de Ingresso`, 2011, 2015)) %>%
  select(-8, -9) %>%
  select(-6, -2)


# Excluir colunas especificadas

colunas_a_excluir <- c(92,91,90,89,87,88,86,85,84,83,82,81,79,78,77,76,75,74,73,72,71,70,69,67,66,63,62,61,60,59,58,56,55,52,49,46,44,41,38,35,32,29,26,23,20,17,14,9,8,6)
estudante <- estudante[, -colunas_a_excluir]




##################################### VARIAVEIS PARA A REGRESSÇO ###########################################


#################  evasÆo (variavel dependente) ###########################


# Crie a nova coluna 'dummy_evasao' usando ifelse aninhado

estudante$dummy_evasao <- ifelse(
  estudante[, 8] == "Ingressante sem Frequˆncia" | 
    estudante[, 8] == "Desistˆncia a pedido" | 
    estudante[, 8] == "Cancelamento trancamento 4 semestres" | 
    estudante[, 8] == "Cancelamento 0 cr‚dito em dois semestres" | 
    estudante[, 8] == "Abandono 2 semestres sem matr¡cula" | 
    estudante[, 8] == "Cancelamento 0 cr‚dito" | 
    estudante[, 8] == "Transferˆncia Interna" | 
    estudante[, 8] == "Encerramento novo ingresso" | 
    estudante[, 8] == "Cancelamento ultrapassou prazo m ximo" | 
    estudante[, 8] == "Transferˆncia USP" | 
    estudante[, 8] == "Cancelamento trancamento 6 semestres" | 
    estudante[, 8] == "Cancelamento menos 20% cr‚d 2 semestres" | 
    estudante[, 8] == "Abandono 3 semestres sem matr¡cula" | 
    estudante[, 8] == "Transferˆncia externa" | 
    estudante[, 8] == "Cancelamento trancamento 5 semestres" | 
    estudante[, 8] == "Cancelamento menos 20% dos cr‚ditos" | 
    estudante[, 8] == "Cancelamento trancamento 10 semestres", 
  1,
  ifelse(
    estudante[, 8] == "NÆo cumpriu normas de retorno ao Curso", 0,
    ifelse(
      estudante[, 8] == "Falecimento", 0,
      ifelse(
        estudante[, 8] == "Cancelamento outras IES (Lei Federal)", 0,
        ifelse(
          estudante[, 8] == "ExpulsÆo", 0,
          ifelse(
            estudante[, 8] == "T‚rmino", 0,
            ifelse(
              estudante[, 8] == "ConclusÆo", 0,
              ifelse(
                estudante[, 8] == "Op‡Æo Curr¡culo", 0,
                NA
              )
            )
          )
        )
      )
    )
  )
)

# Remover linhas com NA na coluna 8
estudante <- estudante[!is.na(estudante[, 8]), ]
estudante <- estudante[!is.na(estudante[, 42]), ]




############################ gˆnero ##################################


### em dummy ###

estudante[[3]] <- ifelse(estudante[[3]] == "M", 0, ifelse(estudante[[3]] == "F", 1, NA))



######################  cor ( PRETA, PARDO, AMARELO E BRANCO ) ######################################

# Verificar se h  "NÆo informada" na coluna 4 (assumindo que a coluna cor est  na posi‡Æo 4)
indices_na <- estudante[, 4] == "NÆo informada"

# Substituir os valores na coluna 4 pela coluna 38 quando a condi‡Æo "NÆo informada" for atendida
estudante[indices_na, 4] <- estudante[indices_na, 38]

# Remova NA, "-", "NÆo informada" e "Sem informa‡Æo" da coluna n£mero 4
estudante <- estudante[!(is.na(estudante[, 4]) | estudante[, 4] == "-" | estudante[, 4] == "NÆo informada" | estudante[, 4] == "Sem informa‡Æo"), ]

# Criar a vari vel dummy PPI (Preta, Parda e Ind¡gena)

estudante[[4]] <- ifelse(estudante[[4]] %in% c("Preta / negra", "Parda", "Ind¡gena"), 1, 0)




######################## desenpenho academico ( NOTAS AO LONGO DO PERIODO DE PERMANENCIA NA USP) ##########################


# vamos fazer um loop para calcular a media artitmetica de cada ID


# Selecione apenas as colunas da 11¦ … 37¦
colunas_a_somar <- estudante[, 11:37, drop = FALSE]

# Substitua "-" por NA para facilitar os c lculos
colunas_a_somar[colunas_a_somar == "-"] <- NA

# Converta as colunas para num‚rico
colunas_a_somar <- apply(colunas_a_somar, 2, as.numeric)

# Crie um vetor para armazenar os resultados
resultados <- numeric(nrow(estudante))

# Loop atrav‚s de cada linha
for (i in 1:nrow(estudante)) {
  # Selecione apenas os valores num‚ricos e nÆo NA
  valores_numericos <- colunas_a_somar[i, !is.na(colunas_a_somar[i,])]
  
  # Se houver valores num‚ricos, calcule a m‚dia e armazene no vetor de resultados
  if (length(valores_numericos) > 0) {
    resultados[i] <- sum(valores_numericos) / length(valores_numericos)
  } else {
    resultados[i] <- NA
  }
}

# Adicione os resultados ao dataframe
estudante$soma_media <- resultados


# Remova as colunas da 11¦ … 37¦
estudante <- estudante[, -c(11:37)]
estudante <- estudante[, -c(5,7,8,11)]


#################### renda per capita  de cada ID ##########################


# psso 1: transformar a coluna 11 em numerica - isto ‚ transformar 3 SM em 3 
# passo 2: multiplicar dado o ano de ingresso da pessoa , o valor numerico encontrado na coluna 11 pelo valor do salario minimo correspondente ao periodo

#salario_minimo_por_ano <- c(`2010` = 1069.67,
#                            `2011` = 1077.56,
#                            `2012` = 1158.04,
#                           `2013` = 1195.78,
#                           `2014` = 1202.04,
#                           `2015` = 1175.72)

# passo 3: transformar a coluna 12 tamb‚m em numerica

# pass0 4: dividir o valor encontrado pela nova coluna de SM criada apos o passo 2 pelo valor correspondente a coluna 12



# Obtenha as respostas £nicas na coluna 11
respostas_unicas <- unique(estudante[, 8])

# Imprima as respostas £nicas
print(respostas_unicas)

estudante <- estudante %>%
  filter(estudante[, 8] != "-")


# Fun‡Æo para calcular SM com base na resposta da coluna 11
calcular_SM <- function(resposta) {
  case_when(
    grepl("De 5 a 6,9 SM.", resposta) ~ 5.95,
    grepl("De 10 a 14,9 SM.", resposta) ~ 12.45,
    grepl("Igual ou superior a 20 SM.", resposta) ~ 20,
    grepl("De 15 a 19,9 SM.", resposta) ~ 17.45,
    grepl("De 7 a 9,9 SM.", resposta) ~ 8.45,
    grepl("Entre 10 e 15 SM.", resposta) ~ 12.5,
    grepl("Entre 03 e 05 SM.", resposta) ~ 4,
    grepl("Acima de 2 at‚ 3 SM.", resposta) ~ 2.5,
    grepl("Entre 05 e 07 SM.", resposta) ~ 6,
    grepl("De 2 a 2,9 SM.", resposta) ~ 2.45,
    grepl("De 3 a 4,9 SM.", resposta) ~ 3.95,
    grepl("Acima de 20 SM.", resposta) ~ 20,
    grepl("Inferior a 1 SM.", resposta) ~ 0.9,
    grepl("Acima de 15 at‚ 20 SM.", resposta) ~ 17.5,
    grepl("Entre 07 e 10 SM.", resposta) ~ 8.5,
    grepl("Acima de 3 at‚ 5 SM.", resposta) ~ 4,
    grepl("Acima de 7 at‚ 10 SM.", resposta) ~ 8.5,
    grepl("De 1 a 1,9 SM.", resposta) ~ 1.45,
    grepl("Entre 15 e 20 SM.", resposta) ~ 17.5,
    grepl("Acima de 7 at‚ 10 SM.", resposta) ~ 8.5,
    grepl("Acima de 15 at‚ 20 SM.", resposta) ~ 17.5,
    grepl("Inferior a 01 SM.", resposta) ~ 0.9,
    grepl("Acima de 20 SM at‚ 30 SM.", resposta) ~ 25,
    grepl("Acima de 5 at‚ 7 SM.", resposta) ~ 6,
    grepl("Acima de 3 at‚ 5 SM.", resposta) ~ 4,
    grepl("Acima de 1 at‚ 2 SM.", resposta) ~ 1.5,
    grepl("Entre 01 e 02 SM.", resposta) ~ 1.5,
    grepl("Acima de 5 at‚ 7 SM.", resposta) ~ 6,
    grepl("Acima de 3 at‚ 5 SM.", resposta) ~ 4,
    grepl("Acima de 7 at‚ 10 SM.", resposta) ~ 8.5,
    grepl("Entre 500 e 1500", resposta) ~ 0.9,
    grepl("Acima de 10 at‚ 15 SM.", resposta) ~ 12.5,
    grepl("At‚ 1 SM - at‚ R$ 1.212,00.", resposta) ~ 0.9,
    grepl("Entre 02 e 03 SM.", resposta) ~ 2.5,
    grepl("Acima de 1 at‚ 2 SM.", resposta) ~ 1.5,
    grepl("Acima de 50 SM - superior a R$ 55.000,00.", resposta) ~ 50,
    grepl("Acima de 15 at‚ 20 SM.", resposta) ~ 12.5,
    grepl("Entre 1500 e 3000", resposta) ~ 2,
    grepl("Acima de 1 at‚ 2 SM - de R$ 1.212,01 at‚ R$ 2.424,00.", resposta) ~ 1.5,
    grepl("Acima de 2 at‚ 3 SM - de R$ 2.090,01 at‚ R$ 3.135,00.", resposta) ~ 2.5,
    grepl("Acima de 50 SM - superior a R$ 60.600,00.", resposta) ~ 50,
    grepl("Acima de 20 SM at‚ 30 SM - de R$ 22.000,01 at‚ R$ 33.000,00.", resposta) ~ 25,
    grepl("At‚ 1 SM - at‚ R$ 1.045,00.", resposta) ~ 1,
    grepl("Acima de 30 SM at‚ 50 SM - de R$ 33.000,01 at‚ R$ 55.000,00.", resposta) ~ 40,
    grepl("Entre 7000 e 10000", resposta) ~ 8,
    grepl("Acima de 5 at‚ 7 SM - de R$ 5.225,01 at‚ R$ 7.315,00.", resposta) ~ 6,
    grepl("Acima de 30 SM at‚ 50 SM - de R$ 36.360,01 at‚ R$ 60.600,00.", resposta) ~ 40,
    grepl("Acima de 10 at‚ 15 SM. - de R$ 11.000,01 at‚ R$ 16.500,00.", resposta) ~ 12.5,
    grepl("Entre 01 e 02 SM", resposta) ~ 1.5,
    grepl("Acima de 20 SM at‚ 30 SM - de R$ 24.240,01 at‚ R$ 36.360,00.", resposta) ~ 25,
    grepl("At‚ 1 SM - at‚ R$ 1.100,00.", resposta) ~ 1,
    grepl("Entre 03 e 05 SM", resposta) ~ 4,
    grepl("Acima de 2 at‚ 3 SM - de R$ 2.424,01 at‚ R$ 3.636,00.", resposta) ~ 2.5,
    grepl("Inferior a 500", resposta) ~ 0.5,
    grepl("Acima de 30 SM at‚ 50 SM - de R$ 31.350,01 at‚ R$ 52.250,00.", resposta) ~ 40,
    grepl("Entre 02 e 03 SM", resposta) ~ 2.5,
    grepl("Entre 05 e 07 SM", resposta) ~ 6,
    grepl("Entre 07 e 10 SM", resposta) ~ 8.5,
    grepl("Acima de 50 SM - superior a R$ 52.250,00.", resposta) ~ 50,
    TRUE ~ NA_real_
  )
}

# Adiciona a coluna SM ao dataframe
estudante <- estudante %>%
  mutate(SM = sapply(estudante[, 8], calcular_SM))

# Mapear os valores do sal rio m¡nimo por ano   

## dados por IPEADATA - salario M¡nimo Real (http://www.ipeadata.gov.br/Default.aspx)

salario_minimo_por_ano <- c(`2010` = 1069.67,
                            `2011` = 1077.56,
                            `2012` = 1158.04,
                            `2013` = 1195.78,
                            `2014` = 1202.04,
                            `2015` = 1175.72)


# Criar o vetor salario_minimo_por_ano
salario_minimo_por_ano <- c(`2010` = 1069.67,
                            `2011` = 1077.56,
                            `2012` = 1158.04,
                            `2013` = 1195.78,
                            `2014` = 1202.04,
                            `2015` = 1175.72)


# Obter valores £nicos da coluna na posi‡Æo 2
valores_unicos <- unique(estudante[, 2])

# Imprimir os valores £nicos
print(valores_unicos)


# Criar um dataframe com a coluna de ano de ingresso e o sal rio m¡nimo correspondente
salario_minimo_df <- data.frame(
  Ano.de.Ingresso = c(2010, 2011, 2012, 2013, 2014, 2015),
  salario_minimo = c(1069.67, 1077.56, 1158.04, 1195.78, 1202.04, 1175.72)
)

# Realizar o merge com base na coluna de ano de ingresso (posi‡Æo 2)
estudante <- merge(estudante, salario_minimo_df, by.x = "Ano de Ingresso", by.y = 1, all.x = TRUE)


#### multiplicar a coluna 21 pela 22

estudante$sm_media <- as.numeric(estudante[, 14]) * as.numeric(estudante[, 15])


# passo 3: vamos transformar a coluna 9 (quantas pessoas vivem dessa renda na casa) em num‚rica


# Obter valores £nicos da coluna 9
respostas_unicas <- unique(estudante[, 9])

# Criar um data frame com os valores £nicos
df_respostas_unicas <- data.frame(Respostas = respostas_unicas)

# Exibir o data frame
print(df_respostas_unicas)


# Excluir linhas com NA ou "-"
estudante <- estudante[!is.na(estudante[, 9]) & estudante[, 9] != "-", ]


# Substituir os valores na coluna 9
estudante <- estudante %>%
  mutate(across(9, ~ifelse(. %in% c("Cinco.", "Cinco"), 5,
                           ifelse(. %in% c("Quatro.", "Quatro"), 4,
                                  ifelse(. %in% c("Trˆs.", "Trˆs"), 3,
                                         ifelse(. %in% c("Duas.", "Duas"), 2,
                                                ifelse(. %in% c("Uma.", "Uma"), 1,
                                                       ifelse(. %in% c("Seis ou mais.", "Seis"), 6,
                                                              as.numeric(.)))))))))


### passo 4:

# Criar nova coluna renda_percapita
estudante$renda_percapita <- estudante[, 16] / estudante[, 9]



######################  curso ( engenharia, letras....) ############################################

#retirar todos os m¢dulos, ciclos basicos, campus externos 


# Obter valores £nicos da coluna 5
respostas_unicas_curso <- unique(estudante[, 5])

# Criar um data frame com os valores £nicos
df_respostas_unicas_curso <- data.frame(Respostas = respostas_unicas_curso)

# Exibir o data frame
print(df_respostas_unicas_curso)

# Identificar ¡ndices das linhas que come‡am com "M¢dulo:"
indices_modulo <- grep("^M¢dulo:", estudante[, 5])

# Remover as linhas correspondentes ao "M¢dulo:"
estudante <- estudante[-indices_modulo, ]


# Obter valores £nicos da coluna 5
respostas_unicas_curso <- unique(estudante[, 5])

# Criar um data frame com os valores £nicos
df_respostas_unicas_curso <- data.frame(Respostas = respostas_unicas_curso)

# Exibir o data frame
print(df_respostas_unicas_curso)

# Cursos a serem removidos
cursos_remover <- c("Licenciatura em Ciˆncias (Polo Piracicaba)",
                    "Licenciatura em Ciˆncias (Polo Ja£)",
                    "Licenciatura em Ciˆncias (Polo Santos)",
                    "Licenciatura em Ciˆncias (Polo Lorena)",
                    "Licenciatura em Ciˆncias (Polo RibeirÆo Preto)",
                    "Licenciatura em Ciˆncias (Polo Piracicaba)",
                    "Licenciatura em Ciˆncias (Polo SÆo Carlos)",
                    "Ciclo B sico")

# Remover as linhas correspondentes aos cursos
estudante <- estudante[!estudante[, 5] %in% cursos_remover, ]


# Obter valores £nicos da coluna 5
respostas_unicas_curso <- unique(estudante[, 5])

# Criar um data frame com os valores £nicos
df_respostas_unicas_curso <- data.frame(Respostas = respostas_unicas_curso)

# Exibir o data frame
print(df_respostas_unicas_curso)


# Categorias a serem removidas
categorias_remover <- c("N£cleo Geral", "Ciclo B sico - RibeirÆo Preto")

# Remover as linhas correspondentes …s categorias
estudante <- estudante[!grepl(paste(categorias_remover, collapse = "|"), estudante[, 5]), ]


# Obter valores £nicos da coluna 5
respostas_unicas_curso <- unique(estudante[, 5])

# Criar um data frame com os valores £nicos
df_respostas_unicas_curso <- data.frame(Respostas = respostas_unicas_curso)

# Exibir o data frame
print(df_respostas_unicas_curso)

# Cria um dicion rio de correspondˆncias

correspondencias<- c(
  "Bacharelado - Habilita‡Æo: Portuguˆs e Lingu¡stica" = "letras bacharelado",
  "F¡sica Bacharelado" = "fisica bacharelado",
  "Bacharelado em Matem tica Aplicada e Computacional" = "matematica bacharelado",
  "Licenciatura - Habilita‡Æo: Portuguˆs" = "letras licenciatura",
  "Licenciatura em Hist¢ria" = "historia licenciatura",
  "Bacharelado - Habilita‡Æo: Grego" = "letras bacharelado",
  "Engenharia - Ciclo B sico" = "engenharia",
  "Bacharelado - Habilita‡Æo: Lingu¡stica" = "letras bacharelado",
  "Licenciatura em Ciˆncias Biol¢gicas" = "biologia licenciatura",
  "Habilita‡Æo: Engenharia de Produ‡Æo" = "engenharia",
  "Matem tica Licenciatura" = "matematica licenciatura",
  "Bacharelado em Ciˆncia da Computa‡Æo" = "computa‡Æo bacharelado",
  "Licenciatura - Habilita‡äes: Portuguˆs e Francˆs" = "letras licenciatura",
  "Habilita‡Æo em Estat¡stica Econ“mica" = "estatistica bacharelado",
  "Bacharelado - Habilita‡Æo: Italiano" = "letras bacharelado",
  "F¡sica Bacharelado - Ciclo B sico" = "fisica bacharelado",
  "F¡sica Licenciatura" = "fisica licenciatura",
  "Bacharelado em Ciˆncias Sociais" = "ciencias_sociais bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs" = "letras bacharelado",
  "Bacharelado em Qu¡mica com Atribui‡äes em Biotecnologia" = "quimica bacharelado",
  "Medicina Veterin ria" = "veterinaria",
  "Bacharelado em Ciˆncias Econ“micas" = "economia bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Italiano" = "letras bacharelado",
  "Habilita‡Æo: Engenharia Mecƒnica" = "engenharia",
  "Bacharelado em Hist¢ria" = "historia bacharelado",
  "Grande µrea - Engenharia Civil" = "engenharia",
  "Letras - Ciclo B sico" = "letras  ciclo basico",
  "Bacharelado em Qu¡mica com Atribui‡äes Tecnol¢gicas" = "quimica bacharelado",
  "Arquitetura e Urbanismo" = "arquitetura",
  "Odontologia" = "odontologia",
  "Bacharelado em Ciˆncias Atuariais" = "atuaria bacharelado",
  "Qu¡mica - N£cleo Geral" = "quimica",
  "Bacharelado" = "aaa",
  "Bacharelado em Administra‡Æo" = "administra‡Æo bacharelado",
  "Bacharelado em Geografia" = "geografia bacharelado",
  "Ciˆncias Biol¢gicas - N£cleo Geral" = "biologia",
  "Bacharelado em Artes Visuais - Habilita‡Æo em Escultura" = "artes bacharelado",
  "Habilita‡Æo: Engenharia Civil" = "engenharia",
  "Turismo" = "turismo",
  "Geologia" = "geologia",
  "Habilita‡Æo: Engenharia El‚trica - Ònfase em Automa‡Æo e Controle" = "engenharia",
  "Licenciatura" = "aaa",
  "Grande µrea - Engenharia Mecƒnica" = "engenharia",
  "Licenciatura em Filosofia" = "filosofia licenciatura",
  "Habilita‡Æo: Engenharia Mecatr“nica" = "engenharia",
  "Habilita‡Æo: Engenharia El‚trica - Ònfase em Computa‡Æo" = "engenharia",
  "Bacharelado em Ciˆncias Cont beis" = "contabilidade bacharelado",
  "Habilita‡Æo: Engenharia de Materiais" = "engenharia",
  "Habilita‡Æo: Engenharia Naval" = "engenharia",
  "Bacharelado em Oceanografia" = "oceanografia bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e µrabe" = "letras bacharelado",
  "Dupla Forma‡Æo FAU - EPUSP" = "dupla_formacao_fau_epusp",
  "Bacharelado em Educa‡Æo F¡sica" = "ed_fisica bacharelado",
  "Licenciatura em Letras" = "letras licenciatura",
  "Habilita‡Æo em Publicidade e Propaganda" = "publicidade_e_propaganda",
  "Grande µrea - Engenharia El‚trica" = "engenharia",
  "Grande µrea - Engenharia Qu¡mica" = "engenharia",
  "Habilita‡Æo em Pesquisa B sica em F¡sica" = "fisica",
  "Licenciatura em Geociˆncias e Educa‡Æo Ambiental" = "geociencias licenciatura",
  "Farm cia - Bioqu¡mica" = "farmacia",
  "Habilita‡Æo: Engenharia de Computa‡Æo" = "engenharia",
  "Bacharelado - Habilita‡Æo: Inglˆs" = "letras bacharelado",
  "Pedagogia" = "pedagogia",
  "N£cleo Comum - Materiais" = "engenharia",
  "Bacharelado - Habilita‡Æo: Latim" = "letras bacharelado",
  "Habilita‡Æo em Mecatr“nica e Sistemas Mecƒnicos" = "engenharia",
  "Bacharelado em Geof¡sica" = "geofisica bacharelado",
  "Bacharelado - Habilita‡Æo: Francˆs" = "letras bacharelado",
  "Bacharelado em Ciˆncias Biol¢gicas" = "biologia bacharelado",
  "Licenciatura - Habilita‡Æo: Latim" = "letras licenciatura",
  "Bacharelado em Esporte" = "ed_fisica bacharelado",
  "Psic¢logo" = "psicologia",
  "Estat¡stica Bacharelado" = "estatistica bacharelado",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Inglˆs" = "letras licenciatura",
  "Biblioteconomia" = "biblioteconomia",
  "Habilita‡Æo: Engenharia Qu¡mica" = "engenharia",
  "Habilita‡Æo: Engenharia El‚trica - Ònfase em Energia e Automa‡Æo El‚tricas" = "engenharia",
  "Bacharelado - Habilita‡Æo: AlemÆo" = "letras bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Hebraico" = "letras bacharelado",
  "Licenciatura em Ciˆncias Sociais" = "ciencias_sociais licenciatura",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Italiano" = "letras licenciatura",
  "Habilita‡Æo em Sa£de Animal" = "veterinaria",
  "Matem tica Bacharelado" = "matematica bacharelado",
  "Bacharelado - Habilita‡Æo: Japonˆs" = "letras bacharelado",
  "Bacharelado - Habilita‡Æo: Chinˆs" = "letras bacharelado",
  "Bacharelado - Habilita‡Æo: Coreano" = "letras bacharelado",
  "Licenciatura em Qu¡mica" = "quimica licenciatura",
  "Bacharelado em Artes Visuais - Habilita‡Æo em Pintura" = "artes bacharelado",
  "Habilita‡Æo: Engenharia Civil - Dupla Forma‡Æo EPUSP - FAU" = "engenharia",
  "Habilita‡Æo: Engenharia de Minas" = "engenharia",
  "Bacharelado em Filosofia" = "filosofia bacharelado",
  "Licenciatura em Geografia" = "geografia licenciatura",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Inglˆs" = "letras bacharelado",
  "Habilita‡Æo em Ciˆncias Biol¢gicas" = "biologia",
  "Bacharelado em Meteorologia" = "meteorologia bacharelado",
  "Habilita‡Æo: Engenharia El‚trica - Ònfase em Sistemas Eletr“nicos" = "engenharia",
  "Habilita‡Æo em Rela‡äes P£blicas" = "relacoes_publicas",
  "Licenciatura - Habilita‡Æo: Russo" = "letras licenciatura",
  "Bacharelado em Rela‡äes Internacionais" = "ri bacharelado",
  "Bacharelado em Qu¡mica com Ònfase em Bioqu¡mica e Biologia Molecular" = "quimica bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Francˆs" = "letras bacharelado",
  "Licenciatura - Habilita‡Æo: Inglˆs" = "letras licenciatura",
  "Psicologia - Licenciatura" = "psicologia licenciatura",
  "Psicologia - Bacharelado" = "psicologia bacharelado",
  "Licenciatura em Educomunica‡Æo" = "educomunicacao",
  "Engenharia Ambiental" = "engenharia",
  "Habilita‡Æo em Jornalismo" = "jornalismo",
  "Habilita‡Æo: Engenharia de Petr¢leo" = "engenharia",
  "Habilita‡Æo em M‚todos Matem ticos" = "matematica bacharelado",
  "Licenciatura em Educa‡Æo F¡sica" = "ed_fisica licenciatura",
  "Bacharelado - Habilita‡Æo: Espanhol" = "letras bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Grego" = "letras bacharelado",
  "Bacharelado em Artes Visuais - Habilita‡Æo em Gravura" = "artes bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Latim" = "letras bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Espanhol" = "letras bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e AlemÆo" = "letras bacharelado",
  "Licenciatura - Habilita‡Æo: Portuguˆs e AlemÆo" = "letras licenciatura",
  "Design" = "design",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Chinˆs" = "letras licenciatura",
  "Habilita‡Æo em Sa£de P£blica" = "saude_publica",
  "Bacharelado em Qu¡mica com Ònfase em Qu¡mica Ambiental" = "quimica bacharelado",
  "Licenciatura - Habilita‡Æo: Portuguˆs e µrabe" = "letras licenciatura",
  "Bacharelado em Qu¡mica Ambiental" = "quimica bacharelado",
  "Habilita‡Æo em Instrumento" = "musica",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Grego" = "letras licenciatura",
  "Licenciatura - Habilita‡Æo: Grego" = "letras licenciatura",
  "Licenciatura em Ciˆncias (Polo SÆo Paulo)" = "ciencias licenciatura",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Lingu¡stica" = "letras licenciatura",
  "Matem tica Aplicada - Bacharelado" = "matematica bacharelado",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Russo" = "letras licenciatura",
  "Bacharelado em Qu¡mica com Ònfase em Qu¡mica Tecnol¢gica" = "quimica bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Chinˆs" = "letras bacharelado",
  "Bacharelado em Astronomia" = "astronomia bacharelado",
  "Licenciatura - Habilita‡Æo: AlemÆo" = "letras licenciatura",
  "Bacharelado - Habilita‡Æo: µrabe" = "letras bacharelado",
  "Licenciatura - Habilita‡äes: Portuguˆs e Latim" = "letras licenciatura",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Japonˆs" = "letras licenciatura",
  "Licenciatura - Habilita‡Æo: Francˆs" = "letras licenciatura",
  "Licenciatura - Habilita‡Æo: Japonˆs" = "letras licenciatura",
  "Habilita‡Æo em Astronomia" = "astronomia",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Espanhol" = "letras licenciatura",
  "Curso Superior do Audiovisual" = "audiovisual",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Russo" = "letras bacharelado",
  "Bacharelado - Habilita‡Æo: Hebraico" = "letras bacharelado",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Hebraico" = "letras licenciatura",
  "Licenciatura em M£sica" = "musica licenciatura",
  "Habilita‡Æo: Engenharia El‚trica - Ònfase em Telecomunica‡äes" = "engenharia",
  "Bacharelado - Habilita‡Æo: Russo" = "letras bacharelado",
  "Habilita‡Æo em Canto e Arte L¡rica" = "musica",
  "Licenciatura - Habilita‡Æo: Italiano" = "letras licenciatura",
  "Licenciatura em Artes Visuais" = "artes licenciatura",
  "Habilita‡Æo: Engenharia Metal£rgica" = "engenharia",
  "Licenciatura - Habilita‡Æo: Coreano" = "letras licenciatura",
  "Licenciatura - Habilita‡Æo: Espanhol" = "letras licenciatura",
  "Habilita‡Æo em Controle e Automa‡Æo" = "engenharia",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Latim" = "letras licenciatura",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Japonˆs" = "letras bacharelado",
  "Habilita‡Æo em Sistemas e Controle" = "engenharia",
  "Licenciatura em Artes Cˆnicas" = "teatro licenciatura",
  "Habilita‡Æo em Composi‡Æo" = "musica",
  "Bacharelado em Artes Visuais - Habilita‡Æo em Multim¡dia e Interm¡dia" = "artes bacharelado",
  "Habilita‡Æo em Editora‡Æo" = "editoracao",
  "Licenciatura - Habilita‡Æo: Lingu¡stica" = "letras licenciatura",
  "Ciclo B sico - Engenharia El‚trica" = "engenharia",
  "Habilita‡Æo: Engenharia El‚trica - Ònfase em Eletr“nica e Sistemas" = "engenharia",
  "Habilita‡Æo em M£sica" = "musica",
  "Habilita‡Æo em Regˆncia" = "musica",
  "Habilita‡Æo em Interpreta‡Æo Teatral" = "teatro",
  "Habilita‡Æo em Dire‡Æo Teatral" = "teatro",
  "Habilita‡Æo em Teoria do Teatro" = "teatro",
  "Habilita‡Æo em Fisiologia e Biof¡sica" = "biologia",
  "Habilita‡Æo em Comunica‡Æo Cient¡fica" = "educomunicacao",
  "Licenciatura - Habilita‡Æo: Chinˆs" = "letras licenciatura",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Coreano" = "letras bacharelado",
  "Licenciatura - Habilita‡Æo: Hebraico" = "letras licenciatura",
  "Bacharelado em Ciˆncias Biom‚dicas" = "biomedicina bacharelado",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Coreano" = "letras licenciatura",
  "Licenciatura - Habilita‡Æo: µrabe" = "letras licenciatura",
  "Habilita‡Æo: Engenharia Ambiental" = "engenharia",
  "Bacharelado em Qu¡mica com Ònfase em Biotecnologia" = "quimica bacharelado",
  "Bacharelado - Habilita‡Æo: Portuguˆs e Armˆnio" = "letras bacharelado",
  "Ciclo B sico - Civil / Ambiental" = "engenharia",
  "Artes Visuais - Bacharelado" = "artes bacharelado",
  "Habilita‡Æo em Cenografia" = "cenografia",
  "Licenciatura - Habilita‡Æo: Portuguˆs e Armˆnio" = "letras licenciatura",
  "Ciclo B sico - Materiais/Metal£rgica/Nuclear" = "engenharia",
  "Habilita‡Æo: Engenharia de Computa‡Æo - Ònfase Sistemas Corporativos" = "engenharia",
  "Artes Visuais - Licenciatura" = "artes licenciatura",
  "Habilita‡Æo em Atu ria" = "atuaria"
)

# Substituir os valores na coluna 5
estudante <- estudante %>%
  mutate(across(5, ~correspondencias[.]))

# Obter valores £nicos da coluna 5
respostas_unicas_curso <- unique(estudante[, 5])

# Criar um data frame com os valores £nicos
df_respostas_unicas_curso <- data.frame(Respostas = respostas_unicas_curso)

# Exibir o data frame
print(df_respostas_unicas_curso)

# Eliminar linhas onde a coluna na posi‡Æo 5 ‚ igual a "aaa"
estudante <- estudante %>%
  filter(!(.[, 5] == "aaa"))


# Criar a dummy exatas
estudante$exatas <- ifelse(estudante[[5]] %in% c(
  "fisica bacharelado", "fisica licenciatura", "fisica", 
  "matematica bacharelado", "matematica licenciatura", 
  "computa‡Æo bacharelado", "estatistica bacharelado", 
  "quimica bacharelado", "quimica licenciatura", 
  "geologia", "oceanografia bacharelado", 
  "geofisica bacharelado", "meteorologia bacharelado", 
  "engenharia", "contabilidade bacharelado", 
  "astronomia bacharelado", "astronomia"),
  1, 0)


# Mapeamento de todos os cursos presentes nas bases de dados

# 0: letras bacharelado, letras licenciatura, letras ciclo basico
# 1: fisica bacharelado, fisica licenciatura, fisica
# 2: matematica bacharelado, matematica licenciatura
# 3: historia licenciatura, historia bacharelado
# 4: engenharia
# 5: biologia licenciatura, biologia, ciencias licenciatura
# 6: computa‡Æo bacharelado
# 7: estatistica bacharelado
# 8: ciencias_sociais bacharelado, ciencias_sociais licenciatura
# 9: quimica bacharelado, quimica licenciatura
# 10: veterinaria
# 11: economia bacharelado
# 12: arquitetura, dupla_formacao_fau_epusp
# 13: odontologia
# 14: atuaria bacharelado, atuaria
# 15: administra‡Æo bacharelado
# 16: geografia bacharelado, geografia licenciatura
# 17: artes bacharelado
# 18: turismo
# 19: geologia
# 20: filosofia licenciatura, filosofia bacharelado
# 21: oceanografia bacharelado
# 22: ed_fisica bacharelado, ed_fisica licenciatura
# 23: publicidade_e_propaganda
# 24: geociencias licenciatura
# 25: farmacia
# 26: pedagogia
# 27: contabilidade bacharelado
# 28: geofisica bacharelado
# 29: psicologia, psicologia licenciatura, psicologia bacharelado
# 30: biblioteconomia
# 31: meteorologia bacharelado
# 32: relacoes_publicas
# 33: ri bacharelado
# 34: educomunicacao
# 35: jornalismo
# 36: design
# 37: saude_publica
# 38: musica, musica licenciatura
# 39: astronomia bacharelado, astronomia
# 40: audiovisual
# 41: artes licenciatura
# 42: teatro licenciatura, teatro
# 43: editoracao
# 44: biomedicina bacharelado
# 45: cenografia


###################################################


# retirar da base de dados as colunas indesejadas
estudante <- estudante %>%
  select(-c(14, 9, 16, 15))


###########################################################################################################3

###### coluna: ‚ bacharelado ou ‚ licenciatura:

estudante <- estudante %>%
  mutate(
    dummy_licenciatura = ifelse(estudante[[5]] %in% c("letras bacharelado", "fisica bacharelado", "matematica bacharelado", "engenharia",
                                                     "computa‡Æo bacharelado", "estatistica bacharelado", "ciencias_sociais bacharelado",
                                                     "quimica bacharelado", "veterinaria", "economia bacharelado", "historia bacharelado",
                                                     "arquitetura", "odontologia", "atuaria bacharelado", "administra‡Æo bacharelado",
                                                     "geografia bacharelado", "artes bacharelado", "turismo", "geologia", 
                                                     "oceanografia bacharelado", "dupla_formacao_fau_epusp", "ed_fisica bacharelado",
                                                     "fisica", "farmacia", "contabilidade bacharelado", "geofisica bacharelado", 
                                                     "biologia bacharelado", "psicologia", "biblioteconomia", "quimica licenciatura",
                                                     "filosofia bacharelado", "geografia licenciatura", "meteorologia bacharelado", 
                                                     "relacoes_publicas", "ri bacharelado", "psicologia licenciatura", "psicologia bacharelado",
                                                     "educomunicacao", "design", "saude_publica", "audiovisual", "musica licenciatura",
                                                     "artes licenciatura", "teatro licenciatura", "editoracao", "biomedicina bacharelado",
                                                     "cenografia", "atuaria"), 0, 1)
  )




estudante <- estudante %>%
  select(-c(8))

############################## nivel de escolaridade mae ##############################################


# Obter valores £nicos da coluna 8
respostas<- unique(estudante[,8])

# Criar um data frame com os valores £nicos
df_respostas<- data.frame(Respostas = respostas)

# Exibir o data frame
print(df_respostas)

# Excluir linhas onde a coluna 8 ‚ igual a "-"
estudante <- estudante %>%
  filter(!(.[, 8] == "-"))


# Obter valores £nicos da coluna 8
respostas<- unique(estudante[,8])

# Criar um data frame com os valores £nicos
df_respostas<- data.frame(Respostas = respostas)

# Exibir o data frame
print(df_respostas)

# Criar a nova coluna dummy_entrada_superior
estudante <- estudante %>%
  mutate(
    dummy_mae_ensinomedio = ifelse(estudante[, 8] %in% c("Ensino superior incompleto.", "Universit rio incompleto.", "P¢s-Gradua‡Æo completa.", "Ensino superior completo.", "Universit rio completo.", "P¢s-Gradua‡Æo completa.", "P¢s-Gradua‡Æo incompleta.", "Ensino m‚dio completo.", "Ensino m‚dio completo"), 1, 0)
  )






########################### nota FUVEST ##################################


#tudo certo

# Transformar v¡rgulas em pontos na coluna 7
estudante <- estudante %>%
  mutate(across(7, ~ as.numeric(str_replace_all(., ",", "."))))


######################  classifica‡Æo do processo seletivo ######################




#tudo certo 

############################# ano dummy ##################################

# Supondo que 'estudante' ‚ o nome do seu DataFrame

# Criar uma coluna dummy para o ano 2011
estudante <- cbind(estudante, "ano_2011" = as.integer(estudante[, 1] == 2011))

# Criar uma coluna dummy para o ano 2012
estudante <- cbind(estudante, "ano_2012" = as.integer(estudante[, 1] == 2012))

# Criar uma coluna dummy para o ano 2013
estudante <- cbind(estudante, "ano_2013" = as.integer(estudante[, 1] == 2013))

# Criar uma coluna dummy para o ano 2014
estudante <- cbind(estudante, "ano_2014" = as.integer(estudante[, 1] == 2014))

# Criar uma coluna dummy para o ano 2015
estudante <- cbind(estudante, "ano_2015" = as.integer(estudante[, 1] == 2015))

# Exiba as primeiras linhas do DataFrame para verificar se as colunas de vari veis dummy foram adicionadas corretamente
head(estudante)



########################   regressÆo ###################################

# EVASÇO = Ano de ingresso ( 2010-2015) | Ra‡a (PPI = 1 / amarela e branca = 0) | Gˆnero ( F = 0 ou M = 1) | Renda per capita | Nota FUVEST | Curso ( dummy de 1 a 45) | tipo de curso ( bacharelado= 0 ou licenciatura= 1 )  | Desempenho Acadˆmico (notas) | Bolsa ( 1 ou 0 ) | Valor recebido da bolsa | Escolaridade Mƒe ( 1-6)


# Remover ".0" da coluna de posi‡Æo 2
estudante[, 2] <- sub("\\.0$", "", estudante[, 2])

# Remover duplicatas pelo ID
df_final1 <- distinct(estudante, ID, .keep_all = TRUE)

# Remover duplicatas pelo ID no dataframe papfe
df_papfe_final <- distinct(papfe, ID, .keep_all = TRUE)

# Excluir colunas 3, 4 e 6 do dataframe papfe
df_papfe_final <- df_papfe_final %>%
  select(-c(3, 4, 6))

# Renomear a coluna na posi‡Æo 2 para "ingresso_ano" no dataframe papfe
df_papfe_final <-df_papfe_final%>%
  rename(ingresso_ano = names(df_papfe_final)[2])

# Renomear a coluna na posi‡Æo 2 para "ingresso_ano" no dataframe df_final

df_final1 <-df_final1%>%
  rename(ingresso_ano = names(df_final1)[1])

# Remover linhas com NA na coluna ingresso_ano
df_final2 <- df_papfe_final[complete.cases(df_papfe_final$ingresso_ano), ]

# Substituir NA por 0 na coluna valor_soma
df_final2$soma_valor[is.na(df_final2$soma_valor)] <- 0

# Juntar os dataframes pelo ID e Ano de Ingresso
df_merged1 <- merge(df_final1, df_final2, by = c("ID"), all = TRUE)


# Remover linhas com valores ausentes
df_merged1 <- na.omit(df_merged1)

# retirar da base de dados as colunas indesejadas
df_merged2 <- df_merged1 %>%
  select(-c(1, 2, 5, 8, 9, 21))


# Ajuste a regressÆo linear

# mudan‡a nomes 

colnames(df_merged2)[1] <- "ano"
colnames(df_merged2)[3] <- "ranking"
colnames(df_merged2)[4] <- "nota fuvest"
colnames(df_merged2)[17] <- "bolsa"
colnames(df_merged2)[16] <- "valor"
colnames(df_merged2)[6] <- "notas"
colnames(df_merged2)[1] <- "feminino"
colnames(df_merged2)[2] <- "ppi"
colnames(df_merged2)[9] <- "licenciatura"
colnames(df_merged2)[10] <- "mae concluiu o ensino m‚dio"
colnames(df_merged2)[5] <- "evasao"

df_merged2 <- df_merged2 %>%
  select(-c(15))


# Remover linhas com valores ausentes
df_merged2 <- na.omit(df_merged2)


# Criar o modelo de regressÆo linear
modelo1 <- lm(evasao ~ ., data = df_merged2)

# Visualizar os resultados
summary(modelo1)

#sem ranking
modelo_sem_ranking <- lm(evasao ~ . - ranking, data = df_merged2)
summary(modelo_sem_ranking)

#agora sem as notas e sem ranking
modelo_sem_ranking_notas <- lm(evasao ~ . - ranking - notas, data = df_merged2)
summary(modelo_sem_ranking_notas)



##################################################################################



#  estat¡sticas descritivas (m‚dia, min e max) das vari veis inclu¡das no estudo 

# Para vari veis num‚ricas
descritivas_numericas <- summary(df_merged2[c("nota fuvest", "notas", "renda_percapita", "valor", "bolsa")])

# Imprimir as estat¡sticas descritivas
print("Estat¡sticas Descritivas para Vari veis Num‚ricas:")
print(descritivas_numericas)


# Lista de vari veis categ¢ricas
variaveis_categoricas <- c("feminino", "ppi", "ranking", "evasao", "exatas", "licenciatura", "mae concluiu o ensino m‚dio", "ano_2011", "ano_2012", "ano_2013", "ano_2014")

# Loop para obter as estat¡sticas descritivas para cada vari vel categ¢rica
for (variavel in variaveis_categoricas) {
  cat(paste("Estat¡sticas descritivas para:", variavel, "\n"))
  print(summary(subset(df_merged2, select = variavel)))
}


# Imprimir as estat¡sticas descritivas
print("Estat¡sticas Descritivas para Vari veis Num‚ricas:")
print(descritivas_numericas)





###############################################################################


#gr fico com a taxa de evasÆo ao longo do tempo

#install.packages("dplyr")
#install.packages("ggplot2")   

library(dplyr)
library(ggplot2)


# Dados fornecidos sobre a porcentagem de evasÆo por ano
ano <- c(2011, 2012, 2013, 2014, 2015)
taxa_evasao <- c(16.95642, 18.86163, 21.27888, 20.74303, 22.16004)

# Criar um dataframe com os dados
df_taxa_evasao <- data.frame(ano, taxa_evasao)

# Verificar os valores do dataframe
print(df_taxa_evasao)

# Criar o gr fico com ggplot2 e ajustar os limites do eixo y
grafico_evasao <- ggplot(df_taxa_evasao, aes(x = ano, y = taxa_evasao)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Taxa de EvasÆo ao Longo do Tempo",
       x = "Ano",
       y = "Taxa de EvasÆo (%)") +
  ylim(0, 40) +  # Ajustar os limites do eixo y
  theme_minimal()

# Exibir o gr fico
print(grafico_evasao)


# Dados sobre a porcentagem de evasÆo
categoria <- c("NÆo evadiram", "Evadiram")
percentual <- c(69.09, 30.9)

# Criar um dataframe com os dados
df_evasao <- data.frame(categoria, percentual)

# Verificar os valores do dataframe
print(df_evasao)

# Criar o gr fico de barras com ggplot2
grafico_barras <- ggplot(df_evasao, aes(x = categoria, y = percentual, fill = categoria)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = "Porcentagem de EvasÆo",
       x = "Categoria",
       y = "Percentual (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(percentual, "%")), vjust = -0.5)

# Exibir o gr fico
print(grafico_barras)



# Dados sobre a m‚dia de evasÆo com e sem bolsa
categoria <- c("Com bolsa", "Sem bolsa")
percentual <- c(27, 34)

# Criar um dataframe com os dados
df_evasao_bolsa <- data.frame(categoria, percentual)

# Verificar os valores do dataframe
print(df_evasao_bolsa)

# Criar o gr fico de barras com ggplot2
grafico_barras_bolsa <- ggplot(df_evasao_bolsa, aes(x = categoria, y = percentual, fill = categoria)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  labs(title = "M‚dia de EvasÆo com e sem Bolsa",
       x = "Categoria",
       y = "Percentual (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0(percentual, "%")), vjust = -0.5)

# Exibir o gr fico
print(grafico_barras_bolsa)

# Supondo que df_merged2 seja o nome do seu dataframe
# Vamos calcular a m‚dia dos valores excluindo os valores iguais a zero
media_valores <- mean(df_merged2$valor[df_merged2$valor != 0])

# Imprime apenas o valor da m‚dia
print(media_valores)

resultado <- 2513.62 / 12
print(resultado)

#209.4683




###################################################################################


# sobre a evasÆo

# Contagem de evasäes por ano
evasoes_por_ano <- colSums(df_merged2[, grepl("^ano_", names(df_merged1))] == 1)

# Contagem total de alunos matriculados por ano
total_alunos_por_ano <- colSums(!is.na(df_merged1[, grepl("^ano_", names(df_merged1))]))

# Calcular a porcentagem de evasÆo por ano
porcentagem_evasao_por_ano <- (evasoes_por_ano / total_alunos_por_ano) * 100

# Exibir os resultados
porcentagem_evasao_por_ano


# resultados evasao em porcentagem 
#2011: 16.95642
#2012: 18.86163
#2013: 21.27888
#2014: 20.74303
#2015: 22.16004


# Verificar os valores £nicos e a contagem de cada valor na vari vel de evasÆo
contagem_evasao <- table(df_merged2$evasao)

# Exibir os resultados
contagem_evasao

#resultado: para uma base de dados com 8,398 linhas temos:

# 5803 que nÆo evadiram
# 2595 que evadiram


proporcao_evasao <- prop.table(contagem_evasao)
proporcao_evasao

# em porcentagem esses valores sÆo:

# 69,09 % de pessoas nao evadiram
# 30,9 % de pessoas que evadiram

proporcao_evasao_total <- table(df_merged2$evasao) / nrow(df_merged2)
proporcao_evasao_total


taxa_evasao <- sum(df_merged2$evasao) / nrow(df_merged2) * 100
taxa_evasao
# 30,9%

qui_quadrado <- chisq.test(df_merged2$evasao, df_merged2$outra_variavel)
qui_quadrado

#nÆo h  evidˆncias significativas de associa‡Æo entre a vari vel de evasÆo e outra vari vel categ¢rica 




###########################################

#bolsa e evasao

correlacao <- cor(df_merged2$bolsa, df_merged2$evasao)
correlacao

#-0.07
#pequena correla‡Æo



media_evasao_com_bolsa <- mean(df_merged2$evasao[df_merged2$bolsa == 1])
media_evasao_sem_bolsa <- mean(df_merged2$evasao[df_merged2$bolsa == 0])
media_evasao_com_bolsa
media_evasao_sem_bolsa


# resultados:
# evasao media com bolsa: 27%
# evasao m‚dia sem bolsa: 34%

teste_t <- t.test(df_merged2$evasao ~ df_merged2$bolsa)
teste_t

#h  uma diferen‡a significativa na m‚dia de evasÆo entre os dois grupos 

#sample estimates:
#mean in group 0 mean in group 1 
#0.3491258       0.2796785 



#################################

# Criar o modelo logit
modelo_logit <- glm(evasao ~ feminino + ppi + `nota fuvest` + renda_percapita + exatas + licenciatura + `mae concluiu o ensino m‚dio` + ano_2011 + ano_2012 + ano_2013 + ano_2014 + valor + bolsa, 
                    data = df_merged2, family = binomial)

# Visualizar o resumo do modelo
summary(modelo_logit)


