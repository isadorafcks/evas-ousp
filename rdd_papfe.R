
### bibliotecas##


# Instalar
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("rdrobust")
install.packages("rddensity")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("modelsummary")
install.packages("vtable")
install.packages("gridExtra")

# Usar
library(gridExtra)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(ggpubr)
library(modelsummary)
library(vtable)

# ###### PARTE 1 - DADOS DE BENEFÍCIO PAPFE ----



# Pontuação Papfe


pontos <- read_excel("P:/David/Papfe/Dados/PET_FEA_BLS_V1.xlsx", 
                     sheet = "Pto PAPFE")



pontos2 <- pontos %>% filter(`Ano de ingresso` == `Ano PAPFE` )  %>% drop_na()


distinct(pontos2)



# Tipo de Bonificação

bonif <- read_excel("P:/David/Papfe/Dados/PET_FEA_BLS_V1.xlsx", 
                    sheet = "Tipo PAPFE")



bonif2 <- bonif  %>% filter(
  str_sub(`Data Inicio concessão`,1,4) == `Ano de Ingresso`
) %>% mutate(ano_benef = year(`Data Inicio concessão`) ) %>% filter(ano_benef == `Ano de Ingresso` )



# Separar um aluno com colunas por benefício

bonif3 <- bonif2  %>% distinct(`Tipo Bonificação Aluno`, ID, `Ano de Ingresso`, .keep_all = T) %>% 
  mutate(across(`Tipo Bonificação Aluno`, ~ifelse(!is.na(.), 1, 0), .names = "Received_{.col}")) %>%
  unnest(cols = starts_with("Received_")) %>%
  pivot_wider(
    id_cols = c("ID", "Ano de Ingresso"),
    names_from = `Tipo Bonificação Aluno`,
    values_from = starts_with("Received_"),
    values_fill = list(Received_ = 0)
  ) %>%
  mutate(across(starts_with("Received_"), as.integer))

bonif4 <- bonif3 %>% mutate_all(~ifelse(is.na(.), 0, .))





# Parcelas e valor pago

valor <- read_excel("P:/David/Papfe/Dados/PET_FEA_BLS_V1.xlsx", 
                    sheet = "Pesquisa CulturaExt")

valor2 <- valor %>%  filter(`Ano de Ingresso` %in% 2011:2015 & 
                              `Ano de Ingresso` == `Ano Pagamento` )

valor3 <- valor2  %>% 
  group_by(`Ano Pagamento`, ID, `Tipo Bonificação Aluno`) %>% summarise(
    valor_total = sum(Valor, na.rm = T),
    parcelas = sum(length(unique(`Mês Pagamento`))), 
    mes_recebimento = paste(min(`Mês Pagamento`), max(`Mês Pagamento`), sep = "-"),
    .groups = "drop")

valor4 <- valor3 %>% pivot_wider(names_from = c(`Tipo Bonificação Aluno`), 
                                 values_from = mes_recebimento) 





# Base joined - Resultados Papfe 



papfe <- right_join(bonif4, pontos2, by = c("ID","Ano de Ingresso" = "Ano de ingresso" )) 




 # ###### PARTE 2 - DADOS GRADUAÇÃO ----




estudante <- read_excel("P:/David/Papfe/Dados/PET_FEA_V2_.xlsx")
colnames(estudante) <- estudante[1,]

estudante <- estudante[,-49]


# Refinando base de Estudantes


estudante <- estudante %>%
  mutate(`Ano de Ingresso` = as.numeric(`Ano de Ingresso`))







estudantes <- estudante %>%  mutate(
  dummy_evad = case_when(
    `Tipo de Encerramento da Habilitação` %in% c("Ingressante sem Frequência", 
                                                 "Desistência a pedido",
                                                 "Cancelamento trancamento 4 semestres",
                                                 "Cancelamento 0 crédito em dois semestres",
                                                 "Abandono 2 semestres sem matrícula",
                                                 "Cancelamento 0 crédito",
                                                 "Transferência Interna",
                                                 "Encerramento novo ingresso",
                                                 "Cancelamento ultrapassou prazo máximo",
                                                 "Transferência USP",
                                                 "Cancelamento trancamento 6 semestres",
                                                 "Cancelamento menos 20% créd 2 semestres",
                                                 "Abandono 3 semestres sem matrícula",
                                                 "Transferência externa",
                                                 "Cancelamento trancamento 5 semestres",
                                                 "Cancelamento menos 20% dos créditos",
                                                 "Término",
                                                 "Cancelamento trancamento 10 semestres") ~ 1,
    `Tipo de Encerramento da Habilitação` %in% c(
      "Não cumpriu normas de retorno ao Curso",
      "Falecimento", "Cancelamento outras IES (Lei Federal)", "Expulsão", 
      "Liminar cassada", "Invalidação da Matrícula") ~ 2, # Exclude Edge Cases
    `Tipo de Encerramento da Habilitação` %in% c( "Conclusão",  NA) ~ 0
  )) %>% filter(dummy_evad != 2)






estudantes <- estudantes %>%  mutate(
  dummy_grad = case_when(
    `Tipo de Encerramento da Habilitação` %in% c( "Conclusão") ~ 1,
    T ~ 0,
  ))







# EVAS?O = Ano de ingresso ( 2010-2015) | Ra?a ( Preto, Pardo, Branco, Amarelo) | G?nero ( F ou M) | Renda per capita | Nota FUVEST | Curso | Desempenho Acad?mico | Bolsa ( 1 ou 0 ) | Valor recebido da bolsa | Escolaridade M?e ( 1-6)


df_final <- estudantes %>% rename(
  habil = `Nome Habilitação`,
  ano_ing = `Ano de Ingresso`,
  curso = `Nome do Curso`
)



# DEFINIR EVADIR:

# Nem todas as habilitações contam como formação
# Importante considerar apenas alunos que encerraram sua graduação 
# sem concluir nenhuma habilitação "válida" (bacharel ou licenciatura)
# como tendo evadido
 

# Calcular Semestres cursados


df_final2 <- df_final %>% mutate(
  ultimo_sem = case_when(
    is.na(`Ano/Semestre de Referência do Fim da Habilitação`) ~ 2023.5,
    T ~ (as.numeric(str_sub(df_final$`Ano/Semestre de Referência do Fim da Habilitação`,1,4))) +
    ( as.numeric(str_sub(df_final$`Ano/Semestre de Referência do Fim da Habilitação`,6,6))*0.5)),
    semestres = (ultimo_sem - ano_ing))





# Remover duplicatas pelo ID

df_final3 <- df_final2 %>% group_by(ID, ano_ing) %>% mutate(
  evad = as.numeric(
    case_when(
      min(dummy_evad) == 1 ~ 1, T ~ 0)),
  grad = as.numeric(
    case_when(
      max(dummy_grad) == 1 ~ 1, T ~ 0))
)



# One Student per Line



df_final4 <- df_final3 %>% group_by(ID, ano_ing) %>%
  mutate(
    product_grad = semestres*dummy_grad,
    product_evad = semestres*dummy_evad,
    anos_cursados =  case_when(
      sum(dummy_grad) > 1 ~ max(product_grad),
      sum(dummy_grad) == 1 ~ max(product_grad),
      sum(dummy_grad) == 0 ~ max(semestres)
      )
  ) %>% distinct(ID, ano_ing, .keep_all = T)






# Definir dummies de evasões em anos específicos

df_final5 <- df_final4 %>% mutate(
 perm_prim_ano = case_when(
    anos_cursados > 1   ~ 1, T ~ 0),
 perm_seg_ano = case_when(
   anos_cursados > 2  ~ 1, T ~ 0),
 perm_terc_ano = case_when(
   anos_cursados > 3  ~ 1, T ~ 0),
 perm_quatro_ano = case_when(
   anos_cursados > 4  ~ 1, T ~ 0)
) %>% filter(!str_detect(habil, "Ribeirão")) %>% 
  filter(!str_detect(habil, "Cênicas - Bacharelado")) # tirando cursos fora de sp





df_final6 <- df_final5 %>% 
  dplyr::mutate(
    ppi = case_when(`Raça/Cor` %in% c("Não informada" ,"Sem informação") ~ NA,
                    `Raça/Cor`  %in% c("Indígena", "Parda", "'Pre'ta / negra") ~ 1,
                    `Raça/Cor` %in% c("Amarela", "Branca") ~ 0 ),
  mulher = case_when(Gênero == "F" ~ 1, T ~ 0))
    

table(df_final8$SM, useNA = "always")

library(dplyr)

df_final7 <- df_final6 %>% 
  rename(resposta = `Somando a renda bruta de todas as pessoas que moram com você, quanto é a renda familiar mensal, em salários mínimos? (some todas as rendas que sus`) %>% 
  mutate(
    SM = 
      case_when(
        grepl("De 5 a 6,9 SM.", resposta) ~ 5.95,
        grepl("De 10 a 14,9 SM.", resposta) ~ 12.45,
        grepl("Igual ou superior a 20 SM.", resposta) ~ 20,
        grepl("De 15 a 19,9 SM.", resposta) ~ 17.45,
        grepl("De 7 a 9,9 SM.", resposta) ~ 8.45,
        grepl("Entre 10 e 15 SM.", resposta) ~ 12.5,
        grepl("Entre 03 e 05 SM.", resposta) ~ 4,
        grepl("Acima de 2 até 3 SM.", resposta) ~ 2.5,
        grepl("Entre 05 e 07 SM.", resposta) ~ 6,
        grepl("De 2 a 2,9 SM.", resposta) ~ 2.45,
        grepl("De 3 a 4,9 SM.", resposta) ~ 3.95,
        grepl("Acima de 20 SM.", resposta) ~ 20,
        grepl("Inferior a 1 SM.", resposta) ~ 0.9,
        grepl("Acima de 15 até 20 SM.", resposta) ~ 17.5,
        grepl("Entre 07 e 10 SM.", resposta) ~ 8.5,
        grepl("Acima de 3 até 5 SM.", resposta) ~ 4,
        grepl("Acima de 7 até 10 SM.", resposta) ~ 8.5,
        grepl("De 1 a 1,9 SM.", resposta) ~ 1.45,
        grepl("Entre 15 e 20 SM.", resposta) ~ 17.5,
        grepl("Acima de 7 até 10 SM.", resposta) ~ 8.5,
        grepl("Acima de 15 até 20 SM.", resposta) ~ 17.5,
        grepl("Inferior a 01 SM.", resposta) ~ 0.9,
        grepl("Acima de 20 SM até 30 SM.", resposta) ~ 25,
        grepl("Acima de 5 até 7 SM.", resposta) ~ 6,
        grepl("Acima de 3 até 5 SM.", resposta) ~ 4,
        grepl("Acima de 1 até 2 SM.", resposta) ~ 1.5,
        grepl("Entre 01 e 02 SM.", resposta) ~ 1.5,
        grepl("Acima de 5 até 7 SM.", resposta) ~ 6,
        grepl("Acima de 3 até 5 SM.", resposta) ~ 4,
        grepl("Acima de 7 até 10 SM.", resposta) ~ 8.5,
        grepl("Entre 500 e 1500", resposta) ~ 0.9,
        grepl("Acima de 10 até 15 SM.", resposta) ~ 12.5,
        grepl("Até 1 SM - até R$ 1.212,00.", resposta) ~ 0.9,
        grepl("Entre 02 e 03 SM.", resposta) ~ 2.5,
        grepl("Acima de 1 até 2 SM.", resposta) ~ 1.5,
        grepl("Acima de 50 SM - superior a R$ 55.000,00.", resposta) ~ 50,
        grepl("Acima de 15 até 20 SM.", resposta) ~ 12.5,
        grepl("Entre 1500 e 3000", resposta) ~ 2,
        grepl("Acima de 1 até 2 SM - de R$ 1.212,01 até R$ 2.424,00.", resposta) ~ 1.5,
        grepl("Acima de 2 até 3 SM - de R$ 2.090,01 até R$ 3.135,00.", resposta) ~ 2.5,
        grepl("Acima de 50 SM - superior a R$ 60.600,00.", resposta) ~ 50,
        grepl("Acima de 20 SM até 30 SM - de R$ 22.000,01 até R$ 33.000,00.", resposta) ~ 25,
        grepl("Até 1 SM - até R$ 1.045,00.", resposta) ~ 1,
        grepl("Acima de 30 SM até 50 SM - de R$ 33.000,01 até R$ 55.000,00.", resposta) ~ 40,
        grepl("Entre 7000 e 10000", resposta) ~ 8,
        grepl("Acima de 5 até 7 SM - de R$ 5.225,01 até R$ 7.315,00.", resposta) ~ 6,
        grepl("Acima de 30 SM até 50 SM - de R$ 36.360,01 até R$ 60.600,00.", resposta) ~ 40,
        grepl("Acima de 10 até 15 SM. - de R$ 11.000,01 até R$ 16.500,00.", resposta) ~ 12.5,
        grepl("Entre 01 e 02 SM", resposta) ~ 1.5,
        grepl("Acima de 20 SM até 30 SM - de R$ 24.240,01 até R$ 36.360,00.", resposta) ~ 25,
        grepl("até 1 SM - até R$ 1.100,00.", resposta) ~ 1,
        grepl("Entre 03 e 05 SM", resposta) ~ 4,
        grepl("Acima de 2 até 3 SM - de R$ 2.424,01 até R$ 3.636,00.", resposta) ~ 2.5,
        grepl("Inferior a 500", resposta) ~ 0.5,
        grepl("Acima de 30 SM até 50 SM - de R$ 31.350,01 até R$ 52.250,00.", resposta) ~ 40,
        grepl("Entre 02 e 03 SM", resposta) ~ 2.5,
        grepl("Entre 05 e 07 SM", resposta) ~ 6,
        grepl("Entre 07 e 10 SM", resposta) ~ 8.5,
        grepl("Acima de 50 SM - superior a R$ 52.250,00.", resposta) ~ 50,
        TRUE ~ NA_real_
      ))  %>% rename(
  num_fam = `Quantas pessoas da família vivem da renda indicada na pergunta anterior?`
) %>% mutate(
  fam_num = case_when(
    num_fam %in% c("Uma", "Uma.") ~ 1,
    num_fam %in% c("Duas", "Duas.") ~ 2,
    num_fam %in% c("Três", "Três.") ~ 3,
    num_fam %in% c("Quatro", "Quatro.") ~ 4,
    num_fam %in% c("Cinco", "Cinco.") ~ 5,
    num_fam %in% c("Seis") ~ 6,
    num_fam %in% c("Seis ou mais.") ~ 6,
    T ~ NA
  ),
  SM_pc = SM / fam_num,
  house_income = 
    case_when(
      ano_ing == 2011 ~ SM*1.14157,
      ano_ing == 2012 ~ SM*1.13465,
      ano_ing == 2013 ~ SM*1.23731,
      ano_ing == 2014 ~ SM*1.26484,
      ano_ing == 2015 ~ SM*1.28316,
      ano_ing == 2016 ~ SM*1.3037,
      ano_ing == 2017 ~ SM*1.30799,
      ano_ing == 2018 ~ SM*1.32091
    ),
  rpc = case_when(
    ano_ing == 2011 ~ SM_pc*1.14157*1000,
    ano_ing == 2012 ~ SM_pc*1.13465*1000,
    ano_ing == 2013 ~ SM_pc*1.23731*1000,
    ano_ing == 2014 ~ SM_pc*1.26484*1000,
    ano_ing == 2015 ~ SM_pc*1.28316*1000,
    ano_ing == 2016 ~ SM_pc*1.3037*1000,
    ano_ing == 2017 ~ SM_pc*1.30799*1000,
    ano_ing == 2018 ~ SM_pc*1.32091*1000,
  )
)







df_final8 <- df_final7 %>% rename(
  mother_educ = `Qual é o nível de instrução de sua mãe ou responsável?`,
  prim_educ = `Qual é o nível de instrução do primeiro responsável por você (mãe, pai ou outra pessoa)?`) %>% 
  mutate(
    mother_education_status = case_when(
      is.na(mother_educ) == F &
      mother_educ %in% c("Não estudou.", "Não frequentou a escola ou tem apenas o primário (1ª a 4ª série do 1º grau) incompleto.", "Não possuo mãe ou mulher responsável.") ~ 1,
      is.na(prim_educ) == F &
        prim_educ %in% c("Não estudou.", "Não frequentou a escola ou tem apenas o primário (1ª a 4ª série do 1º grau) incompleto.", "Não possuo mãe ou mulher responsável.") ~ 1,
      TRUE ~ 0
    ), 
    dummy_fundamental_incompleto = case_when( is.na(mother_educ) == F &
      mother_educ %in% c("Ensino fundamental incompleto.", "Iniciou o Ensino Fundamental, mas abandonou entre a 5ª e a 8ª.") ~ 1,
      is.na(prim_educ) == F &
        prim_educ %in% c("Ensino fundamental incompleto.", "Iniciou o Ensino Fundamental, mas abandonou entre a 5ª e a 8ª.") ~ 1,
      
      TRUE ~ 0
    ),
    dummy_fundamental_completo = case_when( is.na(mother_educ) == F &
      mother_educ %in% c("Ensino fundamental completo.", "Ensino fundamental completo (1ª a 8ª séries).") ~ 1,
      is.na(prim_educ) == F &
        prim_educ %in% c("Ensino fundamental completo.", "Ensino fundamental completo (1ª a 8ª séries).") ~ 1,
      TRUE ~ 0
    ),
    dummy_medio_incompleto = case_when( is.na(mother_educ) == F &
      mother_educ == "Ensino médio incompleto." ~ 1,
      is.na(prim_educ) == F &
        prim_educ == "Ensino médio incompleto." ~ 1,
      TRUE ~ 0
    ),
    dummy_medio_completo = case_when( is.na(mother_educ) == F &
      mother_educ == "Ensino médio completo." ~ 1,
      is.na(prim_educ) == F &
        prim_educ == "Ensino médio completo." ~ 1,
      
      TRUE ~ 0
    ),
    dummy_superior_incompleto = case_when( is.na(mother_educ) == F &
      mother_educ %in% c("Ensino superior incompleto.", "Universitário incompleto.") ~ 1,
      is.na(prim_educ) == F &
        prim_educ %in% c("Ensino superior incompleto.", "Universitário incompleto.") ~ 1,
      TRUE ~ 0
    ),
    dummy_superior_completo = case_when( is.na(mother_educ) == F &
      mother_educ %in% c("Pós-Graduação completa.", "Ensino superior completo.", "Universitário completo.") ~ 1,
      is.na(prim_educ) == F &
        prim_educ %in% c("Pós-Graduação completa.", "Ensino superior completo.", "Universitário completo.") ~ 1,
      TRUE ~ 0
    ),  
    mother_educ_type = case_when(
      mother_educ %in% c("Não estudou.", "Não frequentou a escola ou tem apenas o primário (1ª a 4ª série do 1º grau) incompleto.", "Não possuo mãe ou mulher responsável.") ~ "Did not study",
      mother_educ %in% c("Ensino fundamental incompleto.", "Iniciou o Ensino Fundamental, mas abandonou entre a 5ª e a 8ª.") ~ "Incomplete primary education",
      mother_educ %in% c("Ensino fundamental completo.", "Ensino fundamental completo (1ª a 8ª séries).") ~ "Complete primary education",
      mother_educ == "Ensino médio incompleto." ~ "Incomplete secondary education",
      mother_educ == "Ensino médio completo." ~ "Complete secondary education",
      mother_educ %in% c("Ensino superior incompleto.", "Universitário incompleto.") ~ "Incomplete higher education",
      mother_educ %in% c("Pós-Graduação completa.", "Ensino superior completo.", "Universitário completo.") ~ "Complete higher education",
      TRUE ~ NA_character_
    ),
    mother_college = case_when(
      mother_educ_type %in% c("Complete higher education", "Incomplete higher education") ~ 1,
      is.na(mother_educ_type) ~ NA,
      TRUE ~ 0
    ),
    mother_highschool = case_when(
      mother_educ_type %in% c("Complete secondary education", "Incomplete secondary education",
                              "Complete higher education", "Incomplete higher education") ~ 1,
      is.na(mother_educ_type) ~ NA,
      TRUE ~ 0
    ), 
    mother_no_highschool = case_when(
      mother_educ_type %in% c("Complete secondary education", "Incomplete secondary education",
                              "Complete higher education", "Incomplete higher education") ~ 0,
      is.na(mother_educ_type) ~ NA,
      TRUE ~ 1
    ),
    enter_enem = case_when(
      `Modalidade de Ingresso do Candidato` %in% c("AC", "EP", "PPI") ~ 1, T ~ 0
    ),
    cota_ep = case_when(
      `Modalidade de Ingresso do Candidato` %in% c( "EP") ~ 1, T ~ 0
    ),
    cota_ppi = case_when(
      `Modalidade de Ingresso do Candidato` %in% c("PPI") ~ 1, T ~ 0
    )
  ) %>% mutate(
    ppi = case_when( is.na(ppi) & cota_ppi == 1 ~ 1, T ~ ppi ),
    # Student with ppi affirmative action entry assumed to be ppi
  )    %>% mutate(
    curso2 = case_when(
      curso %in% c("Engenharia", "Música Bacharelado") ~ habil, T ~ curso) 
  ) # Curso 2 Makes Engineering and Music specialties different majors, 
# as they have distinct
# entrance exam score criteria and curriculums                                                        # if racial info is absent from questionnaire 


  
# ###### PARTE 3 - FORMANDO A BASE FINAL ----


install.packages("writexl")
library(write_xl)


  usp_all <- df_final8 %>% dplyr::select(
                                         `Nota Final no Processo Seletivo`,  ano_ing,  SM_pc , rpc, SM,
                                          mulher, grad, curso, habil,
                                         ID, evad,  house_income , ppi,
                                         perm_prim_ano, perm_seg_ano,  perm_terc_ano, perm_quatro_ano,
                                         mother_no_highschool, mother_highschool, mother_college,
                                         enter_enem, cota_ep, cota_ppi
  ) %>% drop_na() %>% 
  mutate(
    nota_fuvest  = as.numeric(gsub(",", ".", gsub("\\s", "", 
               `Nota Final no Processo Seletivo`))),
    
  )


table(filter(df_final9,ano_ing == 2018, is.na(resposta) == F)$SM)

(1+100+242+48)*100 / 
  length(filter(df_final9,ano_ing == 2011, is.na(resposta) == F)$SM)


(149+530+20 )*100 / 
  length(filter(df_final9,ano_ing == 2018, is.na(resposta) == F)$SM)


usp_all_noppi <- df_final9 %>% dplyr::select(
  `Nota Final no Processo Seletivo`,  ano_ing,  SM_pc , rpc, SM,
  mulher, grad, curso, habil,
  ID, evad,  house_income ,
  perm_prim_ano, perm_seg_ano,  perm_terc_ano, perm_quatro_ano,
  mother_no_highschool, mother_highschool, mother_college,
  enter_enem, cota_ep, cota_ppi
) %>% drop_na()


usp_all_cotas <- df_final9 %>% dplyr::select(
  `Nota Final no Processo Seletivo`,  ano_ing,  
  mulher, grad, curso, habil,
  ID, 
  enter_enem, cota_ep, cota_ppi
) %>% drop_na() 



papfe$ID <-   papfe$ID %>% as.character()



joined1 <- left_join(papfe, df_final8,
                  by = c("ID","Ano de Ingresso" = "ano_ing")
)  







usp_papfe <- joined1 %>% dplyr::select(`2018/1`, `2018/2`, `2019/1`, `2019/2`,
                                   `2020/1`, `2020/2`, `2021/1`, `2021/2`, `2022/2`,
  `Nota Final no Processo Seletivo`,  `Ano de Ingresso`,  SM_pc , rpc, SM,
  house_income ,
  ppi, mulher, grad, curso, curso2, habil,
  ID, `Ano de Ingresso`, `Pontuação Total`, anos_cursados,
  `Apoio Moradia-Vaga`, `Apoio Moradia-Auxílio`, evad, `Nº de Disciplinas Optativas Cursadas ao Longo do Curso`,
  perm_prim_ano, perm_seg_ano,  perm_terc_ano, perm_quatro_ano,
  mother_no_highschool, mother_highschool, mother_college,
  enter_enem, cota_ep, cota_ppi
  
) %>% mutate(
  num_cred = case_when(
    
    is.na(`Nº de Disciplinas Optativas Cursadas ao Longo do Curso`) == T ~ 0, T ~
    as.numeric(
    `Nº de Disciplinas Optativas Cursadas ao Longo do Curso`)
  ),
  nota_fuvest  = as.numeric(gsub(",", ".", gsub("\\s", "", 
         `Nota Final no Processo Seletivo`))),
  vaga =  case_when(`Apoio Moradia-Vaga` == 1 ~ 1, T ~ 0), 
             bolsa = 
               case_when(`Apoio Moradia-Auxílio` == 1 ~ 1, T ~ 0),
  aux = case_when(vaga == 1 |bolsa == 1 ~ 1, T ~ 0 )
  ) %>% 
  rename(
    gpa_2018_1 = `2018/1`,
         gpa_2018_2 = `2018/2`,
         gpa_2019_1 = `2019/1`,
         gpa_2019_2 = `2019/2`,
         gpa_2020_1 = `2020/1`,
         gpa_2020_2 = `2020/2`,
         gpa_2021_1 = `2021/1`,
         gpa_2021_2 = `2021/2`,
         gpa_2022_2 = `2022/2`,
    ano_ing = `Ano de Ingresso`,
         papfe = `Pontuação Total`) %>% 
  dplyr::select(-`Apoio Moradia-Vaga`, -`Apoio Moradia-Auxílio`) # Definindo os tratamentos e






# Discontinuiy in Treatment Probability







###### Regressões RDD


usp_papfe_2011 <- usp_papfe %>% filter(ano_ing == 2011) %>% ungroup()
usp_papfe_2012 <- usp_papfe %>% filter(ano_ing == 2012) %>% ungroup()
usp_papfe_2013 <- usp_papfe %>% filter(ano_ing == 2013) %>% ungroup()
usp_papfe_2014 <- usp_papfe %>% filter(ano_ing == 2014) %>% ungroup()
usp_papfe_2015 <- usp_papfe %>% filter(ano_ing == 2015) %>% ungroup()
usp_papfe_2016 <- usp_papfe %>% filter(ano_ing == 2016) %>% ungroup()
usp_papfe_2017 <- usp_papfe %>% filter(ano_ing == 2017) %>% ungroup()
usp_papfe_2018 <- usp_papfe %>% filter(ano_ing == 2018) %>% ungroup()



# Finding the (implicit) cutoff scores

cutoff_2011_vaga <- min(filter( usp_papfe_2011, papfe >90 , vaga == 1)$papfe )
cutoff_2012_vaga <- min(filter( usp_papfe_2012, papfe >90, vaga  == 1)$papfe )
cutoff_2013_vaga <- min(filter( usp_papfe_2013, papfe >90, vaga == 1)$papfe )
cutoff_2014_vaga <- min(filter( usp_papfe_2014, papfe >90, vaga  == 1)$papfe )
cutoff_2015_vaga <- min(filter( usp_papfe_2015, papfe >90, vaga  == 1)$papfe )
cutoff_2016_vaga <- min(filter( usp_papfe_2016, papfe >90, vaga  == 1)$papfe )
cutoff_2017_vaga <- min(filter( usp_papfe_2017, papfe >90, vaga  == 1)$papfe )
cutoff_2018_vaga <- min(filter( usp_papfe_2018, papfe >90, vaga  == 1)$papfe )


cutoff_2011_bolsa <- min(filter( usp_papfe_2011, papfe >80, bolsa == 1)$papfe )
cutoff_2012_bolsa <- min(filter( usp_papfe_2012, papfe >80, bolsa  == 1)$papfe )
cutoff_2013_bolsa <- min(filter( usp_papfe_2013, papfe >80, bolsa == 1)$papfe )
cutoff_2014_bolsa <- min(filter( usp_papfe_2014, papfe >80, bolsa  == 1)$papfe )
cutoff_2015_bolsa <- min(filter( usp_papfe_2015, papfe >80, bolsa  == 1)$papfe )
cutoff_2016_bolsa <- min(filter( usp_papfe_2016, papfe >80, bolsa  == 1)$papfe )
cutoff_2017_bolsa <- min(filter( usp_papfe_2017, papfe >80, bolsa  == 1)$papfe )
cutoff_2018_bolsa <- min(filter( usp_papfe_2018, papfe >80, bolsa  == 1)$papfe )





# Only P2 ans P1-S Students

papfe_rd_reg <- usp_papfe  %>% 
  mutate(cutoff_year_vaga = case_when(
    ano_ing == 2011 ~ cutoff_2011_vaga , 
    ano_ing == 2012 ~ cutoff_2012_vaga, 
    ano_ing == 2013 ~ cutoff_2013_vaga, 
    ano_ing == 2014 ~ cutoff_2014_vaga, 
    ano_ing == 2015 ~ cutoff_2015_vaga,
    ano_ing == 2016 ~ cutoff_2016_vaga, 
    ano_ing == 2017 ~ cutoff_2017_vaga, 
    ano_ing == 2018 ~ cutoff_2018_vaga
  ), norm_cutoff_vaga =  papfe - cutoff_year_vaga,
  cutoff_year_bolsa = case_when(
    ano_ing == 2011 ~ cutoff_2011_bolsa , 
    ano_ing == 2012 ~ cutoff_2012_bolsa, 
    ano_ing == 2013 ~ cutoff_2013_bolsa, 
    ano_ing == 2014 ~ cutoff_2014_bolsa, 
    ano_ing == 2015 ~ cutoff_2015_bolsa,
    ano_ing == 2016 ~ cutoff_2016_bolsa, 
    ano_ing == 2017 ~ cutoff_2017_bolsa, 
    ano_ing == 2018 ~ cutoff_2018_bolsa
  ), norm_cutoff_bolsa =  papfe - cutoff_year_bolsa,
  above_cutoff_bolsa = case_when(norm_cutoff_bolsa > 0 ~ 1, T ~ 0),
  above_cutoff_vaga = case_when(norm_cutoff_vaga > 0 ~ 1, T ~ 0)
  ) %>% ungroup()  %>% filter(ano_ing == 2018) %>% 
  filter(norm_cutoff_vaga < 0) 


# Histograma da Pontuação Papfe e Nota de Corte



papfe_rd  %>%  ggplot() + geom_histogram(
  aes(x = (norm_cutoff_bolsa+95)), , fill = "red", alpha = 0.3) +
 geom_vline(xintercept = 95, lty = "dashed") + theme_minimal() +
  xlim(0, 130) + labs(y = "Frequency", x = "Papfe Score (Dashed Line indicates Cutoff)")


# Plot da Pontuação Papfe e Probabilidade de Bolsa

disc_bolsa <-  filter(papfe_rd  , norm_cutoff_bolsa >= -30 , norm_cutoff_bolsa <= 30,
                      ano_ing %in% c(2018)) %>% ggplot() +
  stat_summary_bin(aes(x = norm_cutoff_bolsa,
                       y = bolsa*100),size = 1, bins = 6, color = "red") +
  coord_cartesian(ylim = c(0,100),
                  xlim = c(-25, 25)) + labs(x = "Normalized Vulnerability Score", 
                                          y = "Stipend Probability (%)") +
  ggtitle("") + theme_minimal() +
  theme(legend.position = "none", 
        text =element_text(size = 18)) + 
  geom_vline(xintercept = 0, linetype = "dashed") 




#- Statistics by Major

papfe_curso_table <- usp_papfe %>% filter(ano_ing == 2018)  %>% 
  group_by(curso2) %>% summarise(
  applicants = n(), 
  stipends = sum(bolsa, na.rm = T),
  porc_evad = 100* mean(evad),
  porc_perm_quatro_ano = 100* mean(perm_quatro_ano),
  ppi = 100*mean(ppi, na.rm = T),
  renda = 1.1648*mean(SM, na.rm = T),
  mulher= 100*mean(mulher),
  nota = mean(nota_fuvest, na.rm = T)
) 

usp_curso_table <- df_final8 %>% mutate(
  nota_fuvest  = as.numeric(gsub(",", ".", gsub("\\s", "", 
                                                `Nota Final no Processo Seletivo`)))
) %>%  filter(ano_ing == 2018)  %>% group_by(curso2) %>% summarise(
  students = n(), 
  porc_evad = 100* mean(evad),
  porc_perm_quatro_ano = 100* mean(perm_quatro_ano),
  ppi = 100*mean(ppi, na.rm = T),
  renda = 1.1648*mean(SM, na.rm = T),
  mulher= 100*mean(mulher, na.rm = T),
  nota = 100*mean(nota_fuvest, na.rm = T)
)




final_curso_table <- left_join(usp_curso_table,
                               select(papfe_curso_table,curso2,
                                      applicants, stipends),
                               by = "curso2")  %>% 
  mutate( applicants = case_when(is.na(applicants) == T ~ 0, 
                                 T ~ applicants),
          porc_apply = case_when(applicants == 0 ~ 0, 
                                 T ~  100*applicants/students),
          porc_stipend = case_when(stipends == 0 ~ 0, 
                                 T ~  100*stipends/students))






# Covariate Characteristics Graphs





year_graph <- 2018

disc_fuvest <- filter(papfe_rd, norm_cutoff_bolsa >= -30 , norm_cutoff_bolsa <= 30,
     ano_ing %in% c(year_graph)) %>% ggplot() +
stat_summary_bin(aes(x = norm_cutoff_bolsa,
                     y = nota_fuvest), bins = 6, color = "#D4AF37") +
geom_vline(xintercept = 0, linetype = "dashed") +
coord_cartesian(ylim = c(0.5*1000, 700),
                xlim = c(-25, 25)) + labs(x = "Normalized Score", 
                                                     y = "Score") +
ggtitle("Admission Test Scores") + theme_minimal() + theme(legend.position = "none")



disc_income <-  filter(papfe_rd  , norm_cutoff_bolsa >= -30 , norm_cutoff_bolsa <= 30,
     ano_ing %in% c(2018)) %>% ggplot() +
stat_summary_bin(aes(x = norm_cutoff_bolsa,
        y = SM*1648), bins = 6, color = "#A6C17B") +
geom_vline(xintercept = 0, linetype = "dashed") +
coord_cartesian(ylim = c(2*1648, 7*1648),
                xlim = c(-25, 25)) + labs(x = "Normalized Score", 
                                       y = "R$ (2023)") +
ggtitle("Household Income") + theme_minimal() + theme(legend.position = "none")


disc_ppi <- filter(papfe_rd, norm_cutoff_bolsa >= -30 , norm_cutoff_bolsa <= 30,
     ano_ing %in% c(year_graph)) %>% ggplot() +
stat_summary_bin(aes(x = norm_cutoff_bolsa,
                     y = 100*ppi, color = "#C9A3A7"), bins = 6) +
geom_vline(xintercept = 0, linetype = "dashed") +
coord_cartesian(ylim = c(0, 70),
                xlim = c(-25, 25)) + labs(x = "Normalized Score", 
                                       y = "Black, Brown or Indigenous (%)") +
ggtitle("Race") + theme_minimal() + theme(legend.position = "none")

disc_gender <- filter(papfe_rd, norm_cutoff_bolsa >= -30 , norm_cutoff_bolsa <= 30,
     ano_ing %in% c(year_graph)) %>% ggplot() +
stat_summary_bin(aes(x = norm_cutoff_bolsa,
                     y = 100*mulher), bins = 6, color = "#6E7E99") +
geom_vline(xintercept = 0, linetype = "dashed") +
coord_cartesian(ylim = c(0, 100),
                xlim = c(-25, 25)) + labs(x = "Normalized Score", 
                                         y = "Female (%)") +
ggtitle("Female") + theme_minimal() + theme(legend.position = "none")


annotate_figure(
ggarrange(plotlist = list(disc_fuvest, disc_gender, 
                        disc_income, disc_ppi), 
        ncol = 2, nrow = 2
),
top = text_grob("", 
              color = "black", face = "bold", size = 24))






# Main Results


papfe_rd_reg <- papfe_rd %>% dplyr::select(
  grad, perm_quatro_ano, norm_cutoff_bolsa, evad,
  norm_cutoff_vaga, vaga, cota_ep, cota_ppi,
  perm_seg_ano, perm_terc_ano, perm_quatro_ano,
  bolsa, habil, curso2, ano_ing)  %>% filter(norm_cutoff_vaga < 0,
                                     ano_ing %in% c(2018))  %>%  drop_na()


state.f = factor(papfe_rd_reg$curso2)

state.d = model.matrix(~state.f+0)


writexl::write_xlsx(papfe_rd_reg,"P:/David/Papfe/Dados/papfe_rd_reg.xlsx")



# Descriptive Statistics


papfe_rd_reg %>% summarise(
  Persistence_2 = 100*mean(perm_seg_ano),
  Persistence_3 = 100*mean(perm_terc_ano),
  Persistence_4 = 100*mean(perm_quatro_ano),
  Drop_Out = 100*mean(evad)
)

papfe_rd_reg %>% summarise(
  cota_ep = 100*mean(cota_ep),
  cota_ppi = 100*mean(cota_ppi)
)




table_data_cov <- papfe_rd_reg <- papfe_rd %>% 
  dplyr::select(papfe, mulher,above_cutoff_bolsa,
       grad, perm_quatro_ano, norm_cutoff_bolsa, evad,
        perm_seg_ano, perm_terc_ano,
        bolsa, habil, ano_ing,
       mulher, cota_ep, cota_ppi,
       ppi ,
       SM, 
       nota_fuvest) %>%
  mutate(cut = case_when(above_cutoff_bolsa == 1 ~ "Eligible", 
                         T ~ "Not Eligible"))  %>%  drop_na(habil) %>% 
  filter(ano_ing %in% c(2018)) %>% dplyr::select(
    mulher,
    ppi , cota_ep, cota_ppi,
    SM,
    nota_fuvest, papfe,cut
  ) %>% mutate(mulher = 100*mulher, ppi = 100*ppi,
               cota_ep = 100*cota_ep,
               cota_ppi= 100*cota_ppi,
               any_cota = case_when(cota_ep+cota_ppi> 0 ~ 100 , T ~ 0)
               )

table_data_outcome <- papfe_rd_reg <- papfe_rd %>% 
  dplyr::select(mulher, above_cutoff_bolsa,
                grad, perm_quatro_ano, norm_cutoff_bolsa, evad,
                perm_seg_ano, perm_terc_ano,
                bolsa, habil, ano_ing,
                mulher,
                ppi ,
                SM,
                nota_fuvest) %>%
  mutate(cut = case_when(above_cutoff_bolsa == 1 ~ "Eligible", 
                         T ~ "Not Eligible"),
         SM2 = SM*1648) %>% select(-SM) %>% 
  drop_na(habil) %>% 
  filter(ano_ing %in% c(2018)) %>% dplyr::select(
    perm_seg_ano, perm_terc_ano,
    perm_quatro_ano,
    evad, cut
  ) %>% mutate(perm_seg_ano = 100*perm_seg_ano, 
               perm_terc_ano = 100*perm_terc_ano, 
               perm_quatro_ano = 100*perm_quatro_ano,
               evad = 100*evad)




sumtable(table_data_cov, group = "cut",
                   skip.format = c(),
                   summ = c("mean(x)", "sd(x)", "notNA(x)"), 
                   digits = 3, 
                   fixed.digits = F)


sumtable(table_data_outcome, group = "cut",
         skip.format = c(),
         summ = c("mean(x)", "sd(x)", "notNA(x)"), 
         digits = 2, 
         fixed.digits = F)




########################## Regs


# RDRobust and Modelsummary Compatibility

tidy.rdrobust <- function(object, ...){
  ret <- data.frame(term = row.names(object$coef), 
                    estimate = object$coef[, 1], 
                    std.error = object$se[, 1], 
                    statistic = object$z[, 1],
                    p.value = object$pv[, 1], 
                    conf.low = object$ci[,1],
                    conf.high = object$ci[, 2])
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(object, ...){
  ret <- data.frame("Obs. Below Cutoff" = object$N[1],
                    "Obs. Above Cutoff" = object$N[2],
                    "Effect. Obs. Below" = object$N_h[1],
                    "Effect. Obs. Above" = object$N_h[2])
  ret
}



# Covariate Tests



# ppi

papfe_rd_ppi <- papfe_rd %>% 
  dplyr::select(ppi, norm_cutoff_bolsa, ano_ing)  %>% 
  filter(ano_ing %in% c(2018))  %>%  drop_na()

rd_ppi <- rdrobust(papfe_rd_ppi$ppi, 
                   papfe_rd_ppi$norm_cutoff_bolsa, c = 0,
                   bws = "msetwo",all = T
) 


# mulher

papfe_rd_mulher <- papfe_rd %>% 
  dplyr::select(mulher, norm_cutoff_bolsa, ano_ing)  %>% 
  filter(ano_ing %in% c(2018))  %>%  drop_na()

rd_mulher <- rdrobust(papfe_rd_mulher$mulher, 
                      papfe_rd_mulher$norm_cutoff_bolsa, c = 0,
                      bws = "msetwo",all = T
)  





# nota_fuvest

papfe_rd_nota_fuvest <- papfe_rd %>% 
  dplyr::select(nota_fuvest, norm_cutoff_bolsa, ano_ing)  %>% 
  filter(ano_ing %in% c(2018))  %>%  drop_na()

rd_nota_fuvest <- rdrobust(papfe_rd_nota_fuvest$nota_fuvest, 
                           papfe_rd_nota_fuvest$norm_cutoff_bolsa, c = 0,
                           bws = "msetwo",all = T
)  


papfe_rd_cota_ppi <- papfe_rd %>% 
  dplyr::select(cota_ppi, norm_cutoff_bolsa, ano_ing)  %>% 
  filter(ano_ing %in% c(2018))  %>%  drop_na()

rd_nota_fuvest <- rdrobust(papfe_rd_cota_ppi$cota_ppi, 
                           papfe_rd_cota_ppi$norm_cutoff_bolsa, c = 0,
                           bws = "msetwo",all = T
) 





# SM_pc

papfe_rd_SM <- papfe_rd %>% 
  dplyr::select(SM_pc, norm_cutoff_bolsa, ano_ing, SM, rpc)  %>% 
  filter(ano_ing %in% c(2018)) %>% 
  mutate(income = SM*1648/1000) %>%  drop_na()

rd_SM <- rdrobust(papfe_rd_SM$income, 
                     papfe_rd_SM$norm_cutoff_bolsa, c = 0,
                     bws = "msetwo",all = T
) 



modelsummary(list("Female" = rd_mulher , 
                  "Black, Brown or Indigenous" = rd_ppi ,
                  "Admission Test Scores" = rd_nota_fuvest ,
                  "Household Income" = rd_SM ),
             stars = c( '*' = .1, '**' = .05, '***' = .01)
)



#### Main Results


# Without Controlling for Major




# Regs

rd_2 <- rdrobust(papfe_rd_reg$perm_seg_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, bws = "msetwo",
                 all = T
)  

rd_3 <- rdrobust(papfe_rd_reg$perm_terc_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, bws = "msetwo",
                 all = T
)   

rd_4 <- rdrobust(papfe_rd_reg$perm_quatro_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, bws = "msetwo",
                 all = T
)   

evad <- rdrobust(papfe_rd_reg$evad, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa,  bws = "msetwo",
                 all = T, 
) 



modelsummary(list("Permanência no 20 ano" = rd_2, 
                  "Permanência até o  3o ano" = rd_3,
                  "Permanência até o 4o ou mais anos" = rd_4,
                  "Evasão" = evad),
             stars = c( '*' = .1, '**' = .05, '***' = .01)
)



# Controlling for Major


rd_2 <- rdrobust(papfe_rd_reg$perm_seg_ano, 
         papfe_rd_reg$norm_cutoff_bolsa, c = 0,
         fuzzy = papfe_rd_reg$bolsa, 
         covs = state.d, bws = "msetwo",
         all = T
)  
 
rd_3 <- rdrobust(papfe_rd_reg$perm_terc_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "msetwo",
                 all = T
) 

rd_4 <- rdrobust(papfe_rd_reg$perm_quatro_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "msetwo",
                 all = T
)  

evad <- rdrobust(papfe_rd_reg$evad, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "msetwo",
                 all = T)




modelsummary(list("Permanência no 20 ano" = rd_2, 
                  "Permanência até o  3o ano" = rd_3,
                  "Permanência até o 4o ou mais anos" = rd_4,
                  "Evasão" = evad),
             stars = c( '*' = .1, '**' = .05, '***' = .01)
             )



#################### Robustness Tests


# For CER Criteria 


cer_rd_2 <- rdrobust(papfe_rd_reg$perm_seg_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "certwo",
                 all = T
)  

cer_rd_3 <- rdrobust(papfe_rd_reg$perm_terc_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "certwo",
                 all = T
) 

cer_rd_4 <- rdrobust(papfe_rd_reg$perm_quatro_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "certwo",
                 all = T
)  

cer_evad <- rdrobust(papfe_rd_reg$evad, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "certwo",
                 all = T)




modelsummary(list("Permanência no 20 ano" = cer_rd_2, 
                  "Permanência até o  3o ano" = cer_rd_3,
                  "Permanência até o 4o ou mais anos" = cer_rd_4,
                  "Evasão" = cer_evad),
             stars = c( '*' = .1, '**' = .05, '***' = .01))




# Half the Bandwidth

rd_2_half <- rdrobust(papfe_rd_reg$perm_seg_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, h = c(rd_2$bws[2]/2, rd_2$bws[4]/2),
                 all = T
)  

rd_3_half <- rdrobust(papfe_rd_reg$perm_terc_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, h = c(rd_3$bws[2]/2, rd_3$bws[4]/2),
                 all = T
) 

rd_4_half <- rdrobust(papfe_rd_reg$perm_quatro_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, h = c(rd_4$bws[2]/2, rd_4$bws[4]/2),
                 all = T
)  

evad_half <- rdrobust(papfe_rd_reg$evad, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, h = c(evad$bws[2]/2, evad$bws[4]/2),
                 all = T, 
) 





modelsummary(list("Permanência no 20 ano" = rd_2_half, 
                  "Permanência até o  3o ano" = rd_3_half,
                  "Permanência até o 4o ou mais anos" = rd_4_half,
                  "Evasão" = evad_half),
             stars = c( '*' = .1, '**' = .05, '***' = .01)
)




# 150% the Bandwidth




rd_2_onehalf <- rdrobust(papfe_rd_reg$perm_seg_ano, 
                      papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                      fuzzy = papfe_rd_reg$bolsa, 
                      covs = state.d, h = c(rd_2$bws[2]*1.5, rd_2$bws[4]*1.5),
                      all = T
)  

rd_3_onehalf <- rdrobust(papfe_rd_reg$perm_terc_ano, 
                      papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                      fuzzy = papfe_rd_reg$bolsa, 
                      covs = state.d, h = c(rd_3$bws[2]*1.5, rd_3$bws[4]*1.5),
                      all = T
) 

rd_4_onehalf <- rdrobust(papfe_rd_reg$perm_quatro_ano, 
                      papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                      fuzzy = papfe_rd_reg$bolsa, 
                      covs = state.d, h = c(rd_4$bws[2]*1.5, rd_4$bws[4]*1.5),
                      all = T
)  

evad_onehalf <- rdrobust(papfe_rd_reg$evad, 
                      papfe_rd_reg$norm_cutoff_bolsa, c = 0,
                      fuzzy = papfe_rd_reg$bolsa, 
                      covs = state.d, h = c(evad$bws[2]*1.5, evad$bws[4]*1.5),
                      all = T, 
) 





modelsummary(list("Permanência no 2o ano" = rd_2_onehalf, 
                "Permanência até o  3o ano" = rd_3_onehalf,
                "Permanência até o 4o ou mais anos" = rd_4_onehalf,
                "Evasão" = evad_onehalf),
           stars = c( '*' = .1, '**' = .05, '***' = .01)
)




# False Cutoff


cutoff_placebo <- -5 # Replace to -5, 0 or 5


rd_false_cutoff <- rdrobust(papfe_rd_reg$perm_seg_ano, 
         papfe_rd_reg$norm_cutoff_bolsa, c = cutoff_placebo,
         fuzzy = papfe_rd_reg$bolsa, 
         covs = state.d, bws = "msetwo",
         all = T
)  
 
rd_false_cutoff <- rdrobust(papfe_rd_reg$perm_terc_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = cutoff_placebo,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "msetwo",
                 all = T
) 

rd_false_cutoff <- rdrobust(papfe_rd_reg$perm_quatro_ano, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = cutoff_placebo,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "msetwo",
                 all = T
)  

evad_false_cutoff <- rdrobust(papfe_rd_reg$evad, 
                 papfe_rd_reg$norm_cutoff_bolsa, c = cutoff_placebo,
                 fuzzy = papfe_rd_reg$bolsa, 
                 covs = state.d, bws = "msetwo",
                 all = T)




modelsummary(list("Permanência no 20 ano" = rd_false_cutoff, 
                  "Permanência até o  3o ano" = rd_false_cutoff,
                  "Permanência até o 4o ou mais anos" = rd_false_cutoff,
                  "Evasão" = evad_false_cutoff),
             stars = c( '*' = .1, '**' = .05, '***' = .01)
             )





# Regression Analysis by Course

reg_curso_evad <- lm(data = final_curso_table,
                     porc_evad ~ porc_apply
)

reg_curso_evad_renda <- lm(data = final_curso_table,
                           porc_evad ~ porc_apply  + nota 
)

reg_curso_evad_cov <- lm(data = final_curso_table,
                         porc_evad ~ porc_apply +   nota + renda 
)

reg_curso_evad_cov_renda <- lm(data = final_curso_table,
                               porc_evad ~ porc_apply + nota + renda + ppi + mulher 
) 


coef_map = c(
  'renda' = 'Mean Household Income (R$)',
  'porc_applicants' = 'Students Applying to Aid (%)',
  'porc_applicants*renda' = 'Income x Application (%)',
  'ppi' = 'BBI(%)',
  'mulher' = 'Female(%)')

library(modelsummary)
modelsummary(list( reg_curso_evad,
                   reg_curso_evad_renda,
                   reg_curso_evad_cov,
                   reg_curso_evad_cov_renda
), 
stars = c( '*' = .1, '**' = .05, '***' = .01))



# Plot % Application vs % Drop Out per course 


ggplot(final_curso_table) + 
  geom_point(aes(x = porc_apply, y = porc_evad, 
                 size = students), shape = 1,  stroke = 1.1) +
  theme_minimal() +
  labs(title = "",
       x = "Applied to Student Aid (%)",
       y = "Drop-out (%)", 
       size = "Students
per Major") + geom_abline(
  intercept = 
    coef(lm(data = final_curso_table, 
            porc_evad ~ porc_apply))[1],
  slope = 
    coef(lm(data = final_curso_table, 
            porc_evad ~ porc_apply))[2],
  lty = "dashed"
)  + annotate("text", label = "Slope = ― 0.26
p-value < 0.01", x = 45, y = 29, fontface =2, size = 5) + 
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    text=element_text(size=24)) +
  xlim(0,50) + ylim(0,50) 





# Density

library(rddensity)
  
rddensity(papfe_rd_reg$norm_cutoff_bolsa, c = 0) %>% summary()


rdplotdensity(rddensity(papfe_rd_reg$norm_cutoff_bolsa, c = 0),
       papfe_rd_reg$norm_cutoff_bolsa)



  ## Plots


install.packages('gplm')
library(gplm)
library(rdrobust)




##### EFFECT OF TREATMENT ON DROPPING OUT


# Base Regression


state.f = factor(papfe_rd_reg$curso2)

state.d = model.matrix(~state.f+0)


evad_reg <- rdrobust(papfe_rd_reg$evad, 
         papfe_rd_reg$norm_cutoff_bolsa, c = 0,
         fuzzy = papfe_rd_reg$bolsa, 
         covs = state.d, bws = "msetwo",
         all = T
) %>% summary()


# Separate Below and Above Cutoff Data


below_cutoff <- papfe_rd_reg %>% filter(norm_cutoff_bolsa < 0, 
                                        norm_cutoff_bolsa > -evad_reg$bws[1]
                                        ) %>% rename(
  x = norm_cutoff_bolsa, y = evad
)

above_cutoff <- papfe_rd_reg %>% filter(norm_cutoff_bolsa >= 0) %>% rename(
  x = norm_cutoff_bolsa, y = evad
)


# Calculating y values for the fitted lines

y_values_below <- evad_reg$beta_T_p_l[1] + below_cutoff$x*evad_reg$beta_T_p_l[2]


y_values_above <- evad_reg$beta_T_p_r[1] + above_cutoff$x*evad_reg$beta_T_p_r[2]

# Plotting
frist_stage_plot <- ggplot() +
  geom_line(aes(x = below_cutoff$x, y = 0), color = "red",
            size = 1.2) +
  geom_line(aes(x = above_cutoff$x, y = y_values_above*100), color = "blue",
            size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "green") +
  labs(title = "Fitted Lines and Discontinuity",
       y = "Drop-Out(%)",
       x = "Vulnerabiliy Score",
       color = "Fitted Polynomial") + theme_minimal() + 
  xlim(-10,10) + ylim(-5,100) 
  
  
  
dots <- rdplot(
    x = papfe_rd_reg$norm_cutoff_bolsa, 
       y = papfe_rd_reg$bolsa*100,
       x.lim = c(-10,10), y.lim = c(-5,100), 
       p = 1, col.lines = "white"
       )
   
dots$rdplot +
  geom_line(aes(x = below_cutoff$x, y = 0), color = "red",
            size = 1.2) +
  geom_line(aes(x = above_cutoff$x, y = y_values_above*100), color = "blue",
            size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "green") +
  labs(title = "",
       y = "Recieves Stipend (%)",
       x = "Vulnerabiliy Score",
       color = "Fitted Polynomial") + theme_minimal() +
  theme(text=element_text(size=24)) +
  xlim(-10,10) + ylim(-5,100) 



###############################################

##### EFFECT OF TREATMENT ON FOURTH YEAR


# Base Regression


# Base Regression


state.f = factor(papfe_rd_reg$curso2)

state.d = model.matrix(~state.f+0)


perm_quatro_ano_reg <- rdrobust(papfe_rd$grad, 
                     papfe_rd$norm_cutoff_bolsa, c = 0,
                     fuzzy = papfe_rd_reg$bolsa, 
                     covs = state.d, bws = "msetwo",
                     all = T
) %>% summary()


# Separate Below and Above Cutoff Data


below_cutoff <- papfe_rd_reg %>% filter(norm_cutoff_bolsa < 0, 
                                        norm_cutoff_bolsa > -perm_quatro_ano_reg$bws[1]
) %>% rename(
  x = norm_cutoff_bolsa, y = perm_quatro_ano
)

above_cutoff <- papfe_rd_reg %>% filter(norm_cutoff_bolsa >= 0) %>% rename(
  x = norm_cutoff_bolsa, y = perm_quatro_ano
)

perm_quatro_ano_reg$beta_T_p_r 

# Coefficients

perm_quatro_ano_reg$beta_Y_p_l


discontinuity <- perm_quatro_ano_reg$coef[3]





intercept_before <- coef(lm( below_cutoff$y ~  below_cutoff$x, data = below_cutoff))[1]
slope_before <- coef(lm( below_cutoff$y ~  below_cutoff$x, data = below_cutoff))[2]


# Calculating y values for the fitted lines

y_values_below <- intercept_before  +
  (slope_before * below_cutoff$x) 

y_values_above <- ( intercept_before * (1 + (discontinuity * perm_quatro_ano_reg$beta_T_p_r[1]) ) ) +
  perm_quatro_ano_reg$beta_Y_p_l[2]*above_cutoff$x

# Plotting
ggplot() +
  geom_line(aes(x = below_cutoff$x, y = y_values_below*100), color = "red",
            size = 1.2) +
  geom_line(aes(x = above_cutoff$x, y = y_values_above*100), color = "blue",
            size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "green") +
  labs(title = "Fitted Lines and Discontinuity",
       y = "Drop-Out(%)",
       x = "Vulnerabiliy Score",
       color = "Fitted Polynomial") + theme_minimal() + 
  xlim(-10,10) + ylim(50,120)




# Plot Curso

ggplot(curso_table) + 
  geom_point(aes(x = porc_bolsa, y = renda*1648/1000, size = students),
             shape = 1) +
  theme_minimal() +
  labs(title = "Recebimento de Bolsa e Permanência entre Candidatos à bolsa - por Curso",
    x = "Obteve Bolsa (%)",
       y = "Renda Domiciliar Mensal (1000 R$)", 
       size = "Candidatos
por Curso") + geom_abline(
  intercept = 
    1648/1000*coef(lm(data = curso_table, renda ~ porc_bolsa ))[1],
  slope = 
    1648/1000*coef(lm(data = curso_table, renda ~ porc_bolsa ))[2],
  lty = "dashed"
)

lm(data = curso_table, renda ~ porc_bolsa ) %>% summary()


# Plot Race

ggplot(curso_table) + 
  geom_point(aes(x = porc_bolsa, y = ppi, size = students)) +
  theme_minimal() +
  labs(title = "Recebimento de Bolsa e Permanência entre Candidatos à bolsa - por Curso",
       x = "Obteve Bolsa (%)",
       y = "Renda Domiciliar Mensal (1000 R$)", 
       size = "Estudantes
por Curso") + geom_abline(
  intercept = 
    coef(lm(data = curso_table, ppi ~ porc_bolsa ))[1],
  slope = 
    coef(lm(data = curso_table, ppi ~ porc_bolsa ))[2],
  lty = "dashed"
)

lm(data = curso_table, renda ~ porc_bolsa ) %>% summary()



# Show neat Table

library(xtable)

# Student Characteristics Table

cov_curso_table <- final_curso_table %>% select(curso,
ppi, mulher, renda, 
) %>% rename(
  "Major" = curso,
  "BBI (%)" = ppi,
  "Female (%)" = mulher,
  "Monthly Household Income" = renda
)
  
  




#### Plot Bin Averages and SEs

# Define the bins for norm_cutoff_bolsa

bin_breaks1 <- c(5*2,10*2,15*2)
bin_breaks2 <- c(-15*2,-10*2,-5*2)




# Create bins for each subset
subset1 <- filter(papfe_rd, norm_cutoff_bolsa >= -30, norm_cutoff_bolsa < 0, 
                  ano_ing %in% c(2018), habil == "Letras - Ciclo Básico"
                  )

subset1$bin <- cut_width(subset1$norm_cutoff_bolsa, width = 5*2, center = 2.5*2)

subset2 <- filter(papfe_rd, norm_cutoff_bolsa < 30, norm_cutoff_bolsa >= 0, 
                  ano_ing %in% c(2018), habil == "Letras - Ciclo Básico")
subset2$bin <- cut_width(subset2$norm_cutoff_bolsa, width = 5*2,  center = 2.5*2)

### Compute mean and standard error for each bin in each subset

# Switch the outcome variable as needed inside the mean() and sd() calls

summary_subset1 <- subset1 %>% 
  group_by(bin) %>%
  summarise(mean = mean(100*evad, na.rm = T), 
            se = sd(100*evad, na.rm = T) / sqrt(n()),
            n = n(),
            upper = mean + 1.96*se/sqrt(n),
            lower = mean - 1.96*se/sqrt(n),
  ) %>% as.data.frame()

summary_subset1 <- summary_subset1 %>% mutate(
  bins =  c(- 10.5*2, -5.5*2, -0.5*2))

summary_subset2 <- subset2 %>% 
group_by(bin) %>%
summarise(mean = mean(100*evad, na.rm = T), 
          se = sd(100*evad, na.rm = T) / sqrt(n()),
          n = n(),
          upper = mean + 1.96*se/sqrt(n),
          lower = mean - 1.96*se/sqrt(n),
          ) %>% as.data.frame()
  
summary_subset2  <- summary_subset2 %>% mutate(
  bins =  c( 10.5*2, 5.5*2, 0.5*2))


# Create separate plots for each subset with error bands
ggplot() +
  geom_point(data = summary_subset1, aes(x = bins, y = mean)) +
  geom_line(data = summary_subset1, aes(x = bins, y = mean), color = "orange") +
  geom_line(data = summary_subset1, 
            aes(x = bins, y = upper, group = 1), 
            linetype = "dotted", color = "brown") +
  geom_line(data = summary_subset1, 
            aes(y = lower, group = 1, x = bins,),
            linetype = "dotted", color = "brown") +
  geom_point(data = summary_subset2, aes(x = bins, y = mean)) +
  geom_line(data = summary_subset2, aes(x = bins, y = mean), color = "blue") +
  geom_line(data = summary_subset2, 
            aes(y = upper, group = 1, x = bins),
            linetype = "dotted", color = "blue") +
  geom_line(data = summary_subset2, 
            aes(y = lower, group = 1, x = bins), 
            linetype = "dotted", color = "blue") +
  theme_minimal() +    
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + xlim(-30,30) +
  xlab("Vulnerability Score (Normalized)") + ylab("Drop-Out (%)") +
  theme(text =element_text(size = 25)) + ylim(0,60)


# Combine plots if needed

combined_plot <- grid.arrange(plot_subset1, plot_subset2, ncol = 1)

# Show the combined plot
print(combined_plot)




?stat_summ

 ggplot() +
   stat_summary_bin(data = 
                      filter(papfe_rd, norm_cutoff_bolsa > -31 , 
                             norm_cutoff_bolsa < 0,
                                  ano_ing %in% c(2018)),
     aes(x = norm_cutoff_bolsa,
                        y = 100*perm_quatro_ano), bins = 6, color = "#D4AF37") +
   stat_summary_bin(data = 
                      filter(papfe_rd, norm_cutoff_bolsa < 31 , 
                             norm_cutoff_bolsa > 0,
                             ano_ing %in% c(2018)),
                    aes(x = norm_cutoff_bolsa,
                        y = 100*perm_quatro_ano), bins = 6, color = "blue") +
   geom_vline(xintercept = 0, linetype = "dashed") +
   coord_cartesian(ylim = c(0, 120)) + labs(x = "Normalized Score", 
                                           y = "Score") +
   ggtitle("Admission Test Scores") + theme_minimal() + 
   theme(legend.position = "none")
 
 
 

###--- Changes in Student Body Composition 

 
 usp_all_table <- df_final9 %>% 
   group_by(ano_ing) %>% summarise(
     porc_cotas = 100*mean(cota_ep, na.rm = T) + 100*mean(cota_ppi, na.rm = T),
     porc_cotas_ppi =  100*mean(cota_ppi, na.rm = T)
   )
 
 
 
 
 usp_all_demogr <- usp_all %>% 
   group_by(ano_ing) %>% summarise(
     students = n(),
     mean_income = mean(SM_pc, na.rm = T),
     porc_mother_college = mean(mother_college, na.rm = T),
     porc_ppi =  100*mean(ppi, na.rm = T)
   )
 
 usp_all_demogr_noppi <- usp_all_noppi %>% 
   group_by(ano_ing) %>% summarise(
     students = n(),
     mean_income = mean(SM_pc, na.rm = T),
     porc_mother_college = mean(mother_college, na.rm = T),
     porc_cotas = 100*mean(cota_ep, na.rm = T) + 100*mean(cota_ppi, na.rm = T),
     porc_cotas_ppi =  100*mean(cota_ppi, na.rm = T),
   )
 
 usp_all_demogr_cotas <- usp_all_cotas %>% 
   group_by(ano_ing) %>% summarise(
     students = n(),
     porc_cotas = 100*mean(cota_ep, na.rm = T) + 100*mean(cota_ppi, na.rm = T),
     porc_cotas_ppi =  100*mean(cota_ppi, na.rm = T),
   )
 
 
 
 
 
plot_cotas_ep <-  usp_all_demogr_cotas %>% ggplot() +
   geom_line(aes(x = ano_ing, y = porc_cotas),
                 color = "blue", linewidth = 0.9) +
  theme_minimal() +
  labs(x = "Year", y = "Public School Slots (%)") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") + 
  theme(text =element_text(size = 15, face = "bold")) 


plot_cotas_ppi <-  usp_all_demogr_cotas %>% ggplot() +
  geom_line(aes(x = ano_ing, y = porc_cotas_ppi), 
                color = "orange", linewidth = 0.9) +
  theme_minimal() +
  labs(x = "Year", y = "Racial Public School Slots (%)") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") + 
  theme(text =element_text(size = 15, face = "bold")) 





plot_ppi <-  usp_all_demogr %>% ggplot() +
  geom_line(aes(x = ano_ing, y = porc_ppi 
                ), color = "darkgreen", linewidth = 0.9) +
  theme_minimal() +
  labs(x = "Year", y = "Black Brown or Indigenous (%)") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") + 
  theme(text =element_text(size = 15, face = "bold")) 



plot_income <-  usp_all_demogr %>% ggplot() +
geom_line(aes(x = ano_ing, y = mean_income*1000, 
), color = "brown", linewidth = 0.9) +
theme_minimal() +
labs(x = "Year", y = "Household Income (R$ 2023)") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") + 
  theme(text =element_text(size = 15, face = "bold")) 


 
 
ggarrange( plot_income, plot_ppi,
  plot_cotas_ep, plot_cotas_ppi, 
           nrow = 2, ncol = 2) 


usp_all_demogr_initial <- usp_all_noppi %>% filter(ano_ing == 2011) %>%  
  group_by(habil) %>% summarise(
    mean_nota = mean(nota_fuvest, na.rm = T),
    students = n(),
    mean_income = mean(rpc, na.rm = T),
    porc_mother_college = mean(mother_college, na.rm = T),
    porc_ppi =  100*mean(ppi, na.rm = T)
  )


usp_all_withppi %>% select(rpc, ano_ing) %>% group_by(ano_ing) %>% 
  summarise(a = mean(rpc, na.rm = T))


(2.35 / 2.56) 


(1-0.9343066)*100
(1-0.9179688)*100


usp_all_ppi %>% select(ppi, ano_ing) %>% group_by(ano_ing) %>% 
  summarise(a = mean(SM_pc, na.rm = T))


# Income and Test Scores

ggplot(usp_all_demogr_initial) + 
  geom_point(aes(x = mean_nota, y = mean_income, 
                 size = students), shape = 1,  stroke = 1.1) +
  theme_minimal() +
  labs(title = "",
       x = "Mean Test Scores in 2011",
       y = "Household Income (R$ 2023)", 
       size = "Students
per Major") + 
  geom_abline(
    intercept = 0,
    slope = 0,
    lty = "dashed"
  )  +
  theme(
    text=element_text(size=15, face = "bold"))   +ylim(1500, 6000)


# Race and Test Scores

ggplot(usp_all_demogr_initial) + 
  geom_point(aes(x = mean_nota, y = porc_ppi, 
                 size = students), shape = 1,  stroke = 1.1) +
  theme_minimal() +
  labs(title = "",
       x = "Mean Test Scores in 2011",
       y = "BBI Students (%)", 
       size = "Students
per Major") + 
  geom_abline(
    intercept = 0,
    slope = 0,
    lty = "dashed"
  )  +
  theme(
    text=element_text(size=15, face = "bold"))  



###--- Changes in Student Body Composition by Major (11-15)

usp_all_demogr_major_notas <- usp_all %>% filter(ano_ing == 2011) %>% 
  group_by(habil) %>% summarise(
    number = n(),
    mean_nota = mean(nota_fuvest, na.rm = T))

usp_all_demogr <- usp_all %>% 
  group_by(ano_ing, habil) %>% summarise(
    students = n(),
    mean_income = mean(rpc, na.rm = T),
    porc_mother_college = mean(mother_college, na.rm = T),
    porc_ppi =  100*mean(ppi, na.rm = T)
  )


major_nota_cov <- left_join(usp_all_demogr, usp_all_demogr_major_notas, by = "habil") %>% 
  filter(ano_ing %in%  c(2011, 2015) ) %>%
  group_by(habil) %>%
  pivot_wider(names_from = ano_ing,
              values_from = c(mean_income, porc_ppi),
              names_sep = "_"
  )


major_nota_cov_11 <- major_nota_cov %>% select(habil, students, mean_nota,
                                               porc_ppi_2011, mean_income_2011)


major_nota_cov_15 <- major_nota_cov %>% select(habil, 
                                               porc_ppi_2015,, mean_income_2015)


major_nota_cov_all <- left_join(major_nota_cov_11, major_nota_cov_15,
                                by = "habil"
) %>% drop_na() %>% mutate(
  diff_income = mean_income_2015 - mean_income_2011,
  diff_ppi = porc_ppi_2015 - porc_ppi_2011,
  var_income = ( (mean_income_2015/mean_income_2011)^(1/4)-1 )*100 ,
  diff_ppi = porc_ppi_2015 - porc_ppi_2011,
)



mean(major_nota_cov_all$porc_ppi_2011)


# Plot



# Income

ggplot(major_nota_cov_all) + 
  geom_point(aes(x = mean_nota, y = var_income, 
                 size = students), shape = 1,  stroke = 1.1) +
  theme_minimal() +
  labs(title = "",
       x = "Mean Test Scores in 2011",
       y = "Yearly Income Variation", 
       size = "Students
per Major") + 
  geom_abline(
    intercept = 0,
    slope = 0,
    lty = "dashed"
  )  +
  theme(
    text=element_text(size=15, face = "bold"))  + ylim(-20, 10) + xlim(180,700)


# PPI

ggplot(major_nota_cov_all) + 
  geom_point(aes(x = mean_nota, y = diff_ppi, 
                 size = students), shape = 1,  stroke = 1.1) +
  theme_minimal() +
  labs(title = "",
       x = "Mean Test Scores in 2011",
       y = "Change in Proportion of BBI Students", 
       size = "Students
per Major") + 
  geom_abline(
    intercept = 0,
    slope = 0,
    lty = "dashed"
  ) +
  theme(
    text=element_text(size=15, face = "bold"))  + ylim(-20,40)



 

###--- Changes in Student Body Composition by Major (15-18)

usp_all_demogr_major_notas <- usp_all %>% filter(ano_ing == 2011) %>% 
  group_by(habil) %>% summarise(
    number = n(),
    mean_nota = mean(nota_fuvest, na.rm = T))

usp_all_demogr <- usp_all %>% 
  group_by(ano_ing, habil) %>% summarise(
    students = n(),
    mean_income = mean(rpc, na.rm = T),
    porc_mother_college = mean(mother_college, na.rm = T),
    porc_ppi =  100*mean(ppi, na.rm = T)
  )


major_nota_cov <- left_join(usp_all_demogr, usp_all_demogr_major_notas, by = "habil") %>% 
  filter(ano_ing %in%  c(2015, 2018) ) %>%
  group_by(habil) %>%
  pivot_wider(names_from = ano_ing,
              values_from = c(mean_income, porc_ppi),
              names_sep = "_"
  )


major_nota_cov_15 <- major_nota_cov %>% select(habil, students, mean_nota,
                                         porc_ppi_2015, mean_income_2015)


major_nota_cov_18 <- major_nota_cov %>% select(habil, 
                                porc_ppi_2018,, mean_income_2018)


major_nota_cov_all <- left_join(major_nota_cov_15, major_nota_cov_18,
                                by = "habil"
                                ) %>% drop_na() %>% mutate(
                       diff_income = mean_income_2018 - mean_income_2015,
                       diff_ppi = porc_ppi_2018 - porc_ppi_2015,
   var_income = ( (mean_income_2018/mean_income_2015)^(1/3)-1 )*100 ,
                       diff_ppi = porc_ppi_2018 - porc_ppi_2015,
                                )



mean(major_nota_cov_all$porc_ppi_2018)


ggplot(major_nota_cov_all) + 
  geom_point(aes(x = mean_nota, y = var_income, 
                 size = students), shape = 1,  stroke = 1.1) +
  theme_minimal() +
  labs(title = "",
       x = "Mean Test Scores in 2011",
       y = "Yearly Income Variation", 
       size = "Students
per Major") + 
  geom_abline(
    intercept = 0,
    slope = 0,
    lty = "dashed"
  )  +
  theme(
    text=element_text(size=15, face = "bold"))  + ylim(-20, 10) + xlim(180,700)



# PPI

ggplot(major_nota_cov_all) + 
  geom_point(aes(x = mean_nota, y = diff_ppi, 
                 size = students), shape = 1,  stroke = 1.1) +
  theme_minimal() +
  labs(title = "",
       x = "Mean Test Scores in 2011",
       y = "Change in Proportion of BBI Students", 
       size = "Students
per Major") + 
  geom_abline(
    intercept = 0,
    slope = 0,
    lty = "dashed"
  ) +
  theme(
    text=element_text(size=15, face = "bold"))  + ylim(-20,40)




####


disc_income <-  filter(papfe_rd  , norm_cutoff_bolsa >= -30 , norm_cutoff_bolsa <= 30,
                       ano_ing %in% c(2018)) %>% ggplot() +
  stat_summary_bin(aes(x = norm_cutoff_bolsa,
                       y = SM*1648), size = 1, bins = 6, color = "blue") +
  coord_cartesian(ylim = c(2*1648, 7*1648)) + labs(x = "Pontuação PAPFE", 
                                                   y = "Renda (R$ de 2023) ") +
  ggtitle("Renda Domiciliar e Pontuação Socioeconômica") + theme_minimal() +
  theme(legend.position = "none",  axis.text.x=element_blank(), 
        text =element_text(size = 18))









