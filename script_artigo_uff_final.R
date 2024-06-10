####
##
# Apresentacao ----

# Script utilizado para gerar os resultado do artigo:

# O impacto de transferências emergenciais nas hospitalizações por síndrome respiratória
# aguda grave (SRAGs) durante a pandemia de COVID-19: evidências de Maricá/RJ

# Link: https://periodicos.uff.br/revistaeconomica/article/view/62632
# Doi: https://doi.org/10.22409/reuff.v24i1/62632

####
##
# Biblioteca ----

library(tidyverse)
library(readr)
library(textclean)
library(vistime)
library(readxl)
library(patchwork)

####
##
# Graficos ----

####
##
## Mapa Marica  ----

library(geobr)
library(ggplot2)
library(sf)
library(dplyr)
library(haven)
library(ggpubr)

munic_rj <- read_municipality(code_muni= "RJ", 
                              year=2010,
                              showProgress = FALSE)

munic_rj$code_muni <- factor(munic_rj$code_muni)

data_censo <- read_dta("banco/base_de_dados_marica_royalties/censo_2010_munic.dta")
data_censo <-rename(data_censo, code_muni = code_munic)
data_censo$code_muni <- factor(data_censo$code_muni)

mapa_RJ <- munic_rj %>% 
  left_join(data_censo, by = "code_muni")

p1 <- ggplot() +
  geom_sf(data=mapa_RJ, aes(fill=mapping), color="gray", size=.15, show.legend = FALSE) +
  labs(subtitle="", size=20) + scale_fill_distiller(palette = "RdGy", direction = 1) +
  theme_minimal() 

p1

#ggsave("imagens_resultados/revista_economica/mapa_marica.jpeg", width = 25, height = 15, units = "cm")

####
##
## Renda e Marica ----

df_rev <- read.csv("banco/base_de_dados_marica_royalties/reveneus_trend.csv", sep =",", header = TRUE)
df_rev$year <- factor(df_rev$year)
df_rev <- filter(df_rev, selection == 1)

# graficando 

ggplot(df_rev, aes(x= as.character(year), y=share, group=Group)) +
  geom_line(aes(linetype=Group)) +
  ylab("Renda per capita") + xlab("") +
  geom_vline(xintercept = "2014", linetype="dotted", color = "#989898", size=0.7) +

  annotate(geom="text", x="2015", y=9800, label="",
           color="darkgray",size=3)+  
  annotate(
    geom = "curve", x = "2015", y = 15000, xend = "2017", yend = 15300, 
    curvature = -0.5, arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "text", x = "2015", y = 14500, label = "Maricá", hjust = "right",size=3) +
  annotate(
    geom = "curve", x = "2018", y = 3700, xend = "2016", yend = 3900, 
    curvature = -0.5, arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "text", x = "2018", y = 3950, label = "Outros", hjust = "left",size=3) +
  
  ggthemes::theme_fivethirtyeight()+

  theme(legend.title = element_text(family = "Times New Roman"), 
        legend.position = "none",
        title = element_text(size = 10, family = "Times New Roman"),
        axis.title.y = element_text(size = 10, family = "Times New Roman"),
        axis.text.y = element_text(size = 10, family = "Times New Roman"),
        axis.text.x = element_text(size = 10, family = "Times New Roman", angle = 90),
        axis.title.x = element_text(size = 10,family = "Times New Roman"),
        legend.text = element_text(size = 10, family = "Times New Roman"),
        plot.caption = element_text(hjust = 0, size = 11, family = "Times New Roman" ),
        panel.background = element_rect(fill = "white", colour = "white", color = "white"),
        plot.background = element_rect(fill = "white", colour = "white", color = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#eceff2", size = 0.7),
        legend.key = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", colour = "white"))

#ggsave("imagens_resultados/revista_economica/marica_renda.jpeg", width = 25, height = 15, units = "cm")

####
##
## Figura -----

# Link: https://cran.r-project.org/web/packages/vistime/vignettes/vistime-vignette.html

read_excel("banco/banco_figura.xlsx",
           col_types = c("text", "text", "date",
                         "date", "text", "text")) %>% 
  vistime(col.event = "Name",
          col.group = "Position"
          #title = "A Pandemia e os Programas Sociais de Maricá"
  )


####
##
## Casos acumulados de SRAG em Marica em comparacao demais municipios do RJ ----

load("banco/srag_long_casos_mes_acumulados_2018_2021.Rda") # banco utilizado

g_casos <- srag_long_casos_mes_acumulados_2018_2021 %>% 
  filter(sigla_uf == "RJ") %>% 
  group_by(`Descrição`, ano_mes_noti, data_numero) %>% 
  summarise(soma_casos = sum(casos_acumulados),
            soma_populacao = sum(pop_2020_siops, na.rm = T), 
            casos_propo = ((soma_casos/soma_populacao)*10000)) %>%   # Casos Acumulados por 10 mil.habitantes
  mutate(data_numero = as.numeric(data_numero),
         Descrição = mgsub(
           Descrição, c(
             "Estado do Rio de Janeiro", # mudar noma variavel
             "Marica"),
           c("Estado do Rio de Janeiro",
             "Maricá"))) %>%
  rename(descricao=`Descrição`)

# calculo da diferenca de casos em Marica

g_casos_vari <- g_casos %>% 
  select(ano_mes_noti, descricao, casos_propo) %>% 
  pivot_wider(names_from = descricao, values_from = casos_propo) %>% 
  mutate(casos_per_rio = (Maricá*100)/ `Estado do Rio de Janeiro`,
         casos_menos = Maricá - `Estado do Rio de Janeiro`,
         casos_menos_per = 100 - casos_per_rio,

  #       descricao = "Maricá",
         descricao_diferenca = "Diferença entre Maricá e o RJ",
         Maricá = NULL,
         `Estado do Rio de Janeiro` = NULL
)

# Medias de casos a menos

# Media (2020-2021)
# media_casos <- g_casos %>% filter(descricao == "Maricá")
# mean(media_casos$casos_menos)
# Media de casos a menos (2020-2021): -8.156832

# Media (2020)

# media_casos_2020 <- g_casos %>% filter(descricao == "Maricá", data_numero %in% c (26:37))
# mean(media_casos_2020$casos_menos)
# Media de casos a menos (2020-2021): -16.61969

# Juntando os dois graficos

g_casos <-  left_join(g_casos, g_casos_vari, 
            by= "ano_mes_noti") 


## criando a funcao de segundo eixo

#  Transformer Function 

transformer_dual_y_axis <- function(data,
                                    primary_column, secondary_column,
                                    include_y_zero = FALSE) {
  
  # PARAMETER SETUP
  params_tbl <- data %>%
    summarise(
      max_primary   = max(!! enquo(primary_column)),
      min_primary   = min(!! enquo(primary_column)),
      max_secondary = max(!! enquo(secondary_column)),
      min_secondary = min(!! enquo(secondary_column))
    )
  
  if (include_y_zero) {
    params_tbl$min_primary   <- 0
    params_tbl$min_secondary <- 0
  }
  
  params_tbl <- params_tbl %>%
    mutate(
      scale = (max_secondary - min_secondary) / (max_primary - min_primary),
      shift = min_primary - min_secondary
    )
  
  # MAKE SCALER FUNCTIONS
  scale_func <- function(x) {
    x * params_tbl$scale - params_tbl$shift
  }
  
  inv_func <- function(x) {
    (x + params_tbl$shift) / params_tbl$scale
  }
  
  # RETURN
  ret <- list(
    scale_func = scale_func,
    inv_func   = inv_func,
    params_tbl = params_tbl
  )
  
  return(ret)
}

# * Make A Y-Axis Transformer --

g_casos <- g_casos %>% 
  mutate(sem_rep = ifelse(  # fazer nova variavel
  descricao == "Maricá" ,
  casos_menos, NA), 
  casos_menos = casos_menos*-1) %>% 
  ungroup()

transformer <- g_casos %>%
  transformer_dual_y_axis(
    primary_column   = casos_propo ,
    secondary_column =casos_menos,
    include_y_zero   = TRUE
  )


g_casos %>%   
  
  ggplot(aes(x = fct_reorder(ano_mes_noti, data_numero)))+
  geom_line(aes(y = casos_propo, 
                col = descricao, 
                group = descricao), size = 1.7,
            alpha = .7)+
  scale_color_manual(values = c("black", "grey50"))+
  geom_vline(xintercept = "03/2020", 
             linetype = "dashed", alpha = 1) +
  geom_point(aes(y = casos_propo,
                 group = descricao,
                 col = descricao)
             ,size = 2,
             alpha = .7)+
  geom_line(aes(y = transformer$inv_func(casos_menos), 
                group = 1),
            size = .7,
            alpha = .5,
            linetype = 4)+
  scale_y_continuous(
    name = "10 mil habitantes",
    sec.axis = sec_axis(
      trans = ~ transformer$scale_func(.),
      name  = "Diferença de casos entre Maricá e o RJ [linha pontinlhada]")) +
  labs(x = NULL,
       col = NULL,
       linetype = NULL,
       title = NULL)+
  theme(legend.title = element_text(family = "Times New Roman"), 
        legend.position = "bottom",
        title = element_text(size = 10, family = "Times New Roman"),
        axis.title.y = element_text(size = 10, family = "Times New Roman"),
        axis.text.y = element_text(size = 10, family = "Times New Roman"),
        axis.text.x = element_text(size = 10, family = "Times New Roman", angle = 90),
        axis.title.x = element_text(size = 10,family = "Times New Roman"),
        legend.text = element_text(size = 10, family = "Times New Roman"),
        plot.caption = element_text(hjust = 0, size = 11, family = "Times New Roman" ),
        panel.background = element_rect(fill = "white", colour = "white", color = "white"),
        plot.background = element_rect(fill = "white", colour = "white", color = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#eceff2", size = 0.7),
        legend.key = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", colour = "white"))


#ggsave("imagens_resultados/revista_economica/g_casos.jpeg", width = 25, height = 15, units = "cm")

####
##
#
## Casos por mes de SRAG em Marica em comparacao demais  municipios do RJ -----

(g_casos_mes <- srag_long_casos_mes_acumulados_2018_2021 %>% 
  filter(sigla_uf == "RJ") %>% 
  group_by(`Descrição`, ano_mes_noti, data_numero) %>% 
  summarise(soma_casos_mes = sum(casos_mes, na.rm = T),
            soma_populacao = sum(pop_2020_siops, na.rm = T), 
            casos_propo = ((soma_casos_mes/soma_populacao)*10000)) %>%  # Casos Acumulados por 10 mil.habitantes
  mutate(data_numero = as.numeric(data_numero),
         `Descrição` = mgsub(
           `Descrição`, c(
                    "Estado do Rio de Janeiro", # mudar noma variavel
                    "Marica"),
                  c("Estado do Rio de Janeiro",
                    "Maricá"))) %>% 
  
  ggplot(aes(x = fct_reorder(ano_mes_noti, data_numero),
             y = casos_propo, 
             col =`Descrição`, 
             group = `Descrição`))+
   geom_line(stat = "identity",
            size = 1.7,
            alpha = .7)+
  geom_vline(xintercept = "03/2020",
             linetype = "dashed", alpha = .7) +
  geom_point(size = 2,
             alpha = .7)+
  annotate(geom = "curve", x = "02/2020", y = 11, xend = "05/2020", yend = 12.2, # indicacao Rio
           curvature = -0.5, arrow = arrow(length = unit(3, "mm"))) + # sao dois anotate, uma para indicao outra para por a palavra
  annotate(geom = "text", x = "02/2020", y = 10.8, label = "RJ", hjust = "right",size=3)+
  
   annotate(geom = "curve", x = "02/2021", y = 18.2, xend = "05/2021", yend = 19.6, # indicacao Rio
           curvature = -0.5, arrow = arrow(length = unit(3, "mm"))) + # sao dois anotate, uma para indicao outra pra 
   annotate(geom = "text", x = "02/2021", y = 18.1, label = "Maricá", hjust = "right",size=3)+
  
  labs(x = NULL,
       y = "10 mil habitantes", 
       col = NULL,
       linetype = NULL,
       title = NULL) +
  ggthemes::theme_fivethirtyeight()+
    scale_color_manual(values = c("black", "grey50"))+
  theme(legend.title = element_text(family = "Times New Roman"), 
        legend.position = "bottom",
        title = element_text(size = 10, family = "Times New Roman"),
        axis.title.y = element_text(size = 10, family = "Times New Roman"),
        axis.text.y = element_text(size = 10, family = "Times New Roman"),
        axis.text.x = element_text(size = 10, family = "Times New Roman", angle = 90),
        axis.title.x = element_text(size = 10,family = "Times New Roman"),
        legend.text = element_text(size = 10, family = "Times New Roman"),
        plot.caption = element_text(hjust = 0, size = 11, family = "Times New Roman" ),
        panel.background = element_rect(fill = "white", colour = "white", color = "white"),
        plot.background = element_rect(fill = "white", colour = "white", color = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#eceff2", size = 0.7),
        legend.key = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", colour = "white")))

#ggsave("imagens_resultados/revista_economica/g_casos_mes.jpeg", width = 25, height = 15, units = "cm")

####
##
## Nivel de mobilidade em Marica  -----

load("banco/brasil_20_21_mob_pop.Rda")

# Marica

# local de trabalho 

marica_trabalho <- brasil_20_21_mob_pop %>%
  filter(id_municipio_6 == 330270) %>% # apenas Marica
  select(municipio, workplaces_percent_change_from_baseline, date) %>%
  mutate(grupo = "local_de_trabalho") |> 
  rename(valor = workplaces_percent_change_from_baseline)

# media residencia 

marica_residencia <- brasil_20_21_mob_pop %>%
  filter(id_municipio_6 == 330270) %>% # apenas Marica
  select(municipio, residential_percent_change_from_baseline, date) %>%
  mutate(grupo = "local_de_residencia") |> 
  rename(valor = residential_percent_change_from_baseline)

# grafico total

marica <- rbind(marica_trabalho, marica_residencia)

# Forma para mudar a legenda no face_grid
grupo <- c("Locais de residência", "Locais de trabalho")
names(grupo) <- c("local_de_residencia", "local_de_trabalho")

marica %>% ggplot() + 
  aes(x = date,
      y = valor) +
  geom_line(stat = "identity",
            size = .7,
            alpha = .8, 
            group = 1) +
  facet_grid(rows = vars(grupo),
             labeller = labeller(grupo = grupo))+
  geom_hline(yintercept= 0, linetype="dashed", size=  0.2,alpha = .6,
             color = "#2B2B2B")+
  geom_vline(xintercept = as.numeric(as.Date("2020-04-01")),
             linetype = "dashed", alpha = .7) +
  labs(x = NULL,
       y = NULL, 
       col = NULL,
       linetype = NULL,
       #title = "Níveis de circulação em Maricá/RJ (2020-2021)"
       )+
  scale_y_continuous(labels=function(x) paste0(x,"%"), limits = c(-60, 40))+
  ggthemes::theme_fivethirtyeight()+
  theme(legend.title = element_text(family = "Times New Roman"), 
        legend.position = "none",
        title = element_text(size = 10, family = "Times New Roman"),
        axis.title.y = element_text(size = 10, family = "Times New Roman"),
        axis.text.y = element_text(size = 10, family = "Times New Roman"),
        axis.text.x = element_text(size = 10, family = "Times New Roman", angle = 90),
        axis.title.x = element_text(size = 10,family = "Times New Roman"),
        legend.text = element_text(size = 10, family = "Times New Roman"),
        plot.caption = element_text(hjust = 0, size = 11, family = "Times New Roman" ),
        panel.background = element_rect(fill = "white", colour = "white", color = "white"),
        plot.background = element_rect(fill = "white", colour = "white", color = "white"),
        legend.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#eceff2", size = 0.7),
        legend.key = element_rect(fill = "white", color = "white"),
        strip.background = element_rect(fill = "white", colour = "white"))

#ggsave("imagens_resultados/revista_economica/mobilidade_marica.jpeg", width = 25, height = 15, units = "cm")

## Calculo antes e depois -----

# Argumento: diferencas das diferencas 

bd_casos_mes <- srag_long_casos_mes_acumulados_2018_2021 %>% 
  filter(sigla_uf == "RJ") %>% 
  group_by(`Descrição`, ano_mes_noti, data_numero) %>% 
  summarise(soma_casos_mes = sum(casos_mes, na.rm = T),
            soma_populacao = sum(pop_2020_siops, na.rm = T), 
            casos_propo = ((soma_casos_mes/soma_populacao)*10000)) %>%  # Casos Acumulados por 10 mil.habitantes
  mutate(data_numero = as.numeric(data_numero),
         `Descrição` = mgsub(
           `Descrição`, c(
             "Estado do Rio de Janeiro", # mudar noma variavel
             "Marica"),
           c("Estado do Rio de Janeiro",
             "Maricá")))

##  Marica 

# Antes:

bd_marica_antes <- bd_casos_mes |> filter (Descrição == "Maricá",
                                           ano_mes_noti %in% c (
                                             "01/2018", "02/2018", "03/2018",
                                             "04/2018", "05/2018", "06/2018",
                                             "07/2018", "08/2018", "09/2018",
                                             "10/2018", "11/2018", "12/2018",
                                             
                                             "01/2019", "02/2019", "03/2019",
                                             "04/2019", "05/2019", "06/2019",
                                             "07/2019", "08/2019", "09/2019",
                                             "10/2019", "11/2019", "12/2019",
                                             
                                             "01/2020", "02/2020"))

mean(bd_marica_antes$casos_propo)
# Marica Media Antes: 0.02624433

# Depois:

bd_marica_20_21_depois <- bd_casos_mes |> filter (Descrição == "Maricá",
                                                  ano_mes_noti %in%  c (
                                                    "03/2020", "04/2020",
                                                    "05/2020", "06/2020", "07/2020",
                                                    "08/2020", "09/2020", "10/2020",
                                                    "11/2020",  "12/2020",
                                                    
                                                    "01/2021", "02/2021", "03/2021",
                                                    "04/2021", "05/2021", "06/2021",
                                                    "07/2021", "08/2021", "09/2021",
                                                    "10/2021", "11/2021", "12/2021"))

bd_marica_20_depois <- bd_casos_mes |> filter (Descrição == "Maricá",
                                               ano_mes_noti %in%  c (
                                                 "03/2020", "04/2020",
                                                 "05/2020", "06/2020", "07/2020",
                                                 "08/2020", "09/2020", "10/2020",
                                                 "11/2020",  "12/2020"
                                                 
                                                 # "01/2021", "02/2021", "03/2021",
                                                 # "04/2021", "05/2021", "06/2021",
                                                 # "07/2021", "08/2021", "09/2021",
                                                 # "10/2021", "11/2021", "12/2021"
                                               ))

mean(bd_marica_20_depois$casos_propo)
mean(bd_marica_20_21_depois$casos_propo)

# Marica Media Depois (20): 3.641281

# Marica Media Depois (20-21): 6.583856

# Antes/depois Marica: 

a_d_marica_20 <- mean(bd_marica_20_depois$casos_propo) - mean(bd_marica_antes$casos_propo) # Valor a mais em 2020

mean(bd_marica_20_21_depois$casos_propo) - mean(bd_marica_antes$casos_propo) # Valor a mais em 2020 - 2021

# Valor a mais apenas 2020: 3.615037

# Valor a mais apenas 2020-2021: 6.557611

## Demais Municipios RJ

# Antes RJ:

bd_rj_antes <- bd_casos_mes |> filter (Descrição != "Maricá",
                                       ano_mes_noti %in% c (
                                         "01/2018", "02/2018", "03/2018",
                                         "04/2018", "05/2018", "06/2018",
                                         "07/2018", "08/2018", "09/2018",
                                         "10/2018", "11/2018", "12/2018",
                                         
                                         "01/2019", "02/2019", "03/2019",
                                         "04/2019", "05/2019", "06/2019",
                                         "07/2019", "08/2019", "09/2019",
                                         "10/2019", "11/2019", "12/2019",
                                         
                                         "01/2020", "02/2020"))

mean(bd_rj_antes$casos_propo)
# Rj Media Antes: 0.09917609

# Depois RJ:

bd_rj_20_21_depois <- bd_casos_mes |> filter (Descrição != "Maricá",
                                              ano_mes_noti %in%  c (
                                                "03/2020", "04/2020",
                                                "05/2020", "06/2020", "07/2020",
                                                "08/2020", "09/2020", "10/2020",
                                                "11/2020",  "12/2020",
                                                
                                                "01/2021", "02/2021", "03/2021",
                                                "04/2021", "05/2021", "06/2021",
                                                "07/2021", "08/2021", "09/2021",
                                                "10/2021", "11/2021", "12/2021"))

bd_rj_20_depois <- bd_casos_mes |> filter (Descrição != "Maricá",
                                           ano_mes_noti %in%  c (
                                             "03/2020", "04/2020",
                                             "05/2020", "06/2020", "07/2020",
                                             "08/2020", "09/2020", "10/2020",
                                             "11/2020",  "12/2020"
                                             # 
                                             # "01/2021", "02/2021", "03/2021",
                                             # "04/2021", "05/2021", "06/2021",
                                             # "07/2021", "08/2021", "09/2021",
                                             # "10/2021", "11/2021", "12/2021"
                                           ))

mean(bd_rj_20_21_depois$casos_propo)
mean(bd_rj_20_depois$casos_propo)

# Rj Media Depois (20-21): 6.804145

# Rj Media Depois (20): 6.768607


# Antes/depois Rj: 

# Valor a mais apenas 2020: 6.669431

a_d_rj_20  <- mean(bd_rj_20_depois$casos_propo) - mean(bd_rj_antes$casos_propo) # Valor a mais

# Valor a mais apenas 2020-2021: 6.704969

mean(bd_rj_20_21_depois$casos_propo) - mean(bd_rj_antes$casos_propo) # Valor a mais


## Antes e depois 

v_m_marica_20 <- a_d_rj_20 -  a_d_marica_20   

round (v_m_marica_20*100 / a_d_rj_20) 



