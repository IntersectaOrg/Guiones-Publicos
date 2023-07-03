#------------------------------------------------------------------------------#
# Proyecto:                   Violencia y discriminación en el trabajo
# Objetivo:                   Análisis de ENDISEG
#
# Encargadas:                 Adriana E. Ortega (INTR)
# Correo:                     aortega@intersecta.org
# Fecha de creación:          20 de junio de 2023
# Última actualización:       --
#------------------------------------------------------------------------------#

# Fuente:            https://www.inegi.org.mx/programas/endiseg/2021/#Microdatos

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(scales, tidyverse, stringi, dplyr, plyr, foreign, readxl, janitor,extrafont,
       beepr, extrafont, treemapify, ggmosaic, srvyr, ggrepel, lubridate, tidyr, magick, add2ggplot)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp         <- "/Volumes/GoogleDrive/My Drive/endiseg/base/"
out_figs    <- "/Users/adriana/Desktop/violencia_lgbt+_work/grafs/"

# Tema para gráficas 
tema <-  theme_linedraw() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot", 
    text                  = element_text(family = "Helvetica", color = "#16213E"),
    plot.title            = element_text(family = "Helvetica", face = "bold", color = "#16213E", size = 10,  margin = margin(10,5,5,5)),
    plot.subtitle         = element_text(family = "Helvetica", face = "italic", color = "#453C67", size = 9,  margin = margin(5, 5, 5, 5)),
    plot.caption          = element_text(family = "Helvetica", face = "italic", color = "#453C67", size = 6,  hjust = 0),
    panel.grid            = element_blank(),
    plot.margin           = margin(0, 0.5, 0, 0.5, "cm"),
    legend.position       = "top",
    panel.border          = element_blank(),
    legend.title          = element_text(size = 8, family = "Helvetica", color = "#453C67", face   = "bold"),
    legend.text           = element_text(size = 8, family = "Helvetica", color = "#453C67"),
    axis.title            = element_text(size = 8, family = "Helvetica", color = "#453C67", hjust = .5, margin = margin(1,1,1,1)),
    axis.text.y           = element_text(size = 7, family = "Helvetica", color = "#453C67", angle=0,  hjust=1),
    axis.text.x           = element_text(size = 7, family = "Helvetica", color = "#453C67", angle=90, hjust=.5, vjust = 0.5),
    strip.text.x          = element_text(size = 8, family = "Helvetica", color = "#453C67", face = "bold"),
    strip.text.y          = element_text(size = 8, family = "Helvetica", face = "bold", color = "#D6CEB4"), 
    strip.background      = element_rect(fill = "white", color = "#16213E"),
    axis.line.x.bottom    = element_line(color="#16213E", size = .3))

# Logo Intersecta
add_intlogo <- function(graf, escala){
  graf_con_logo <- add_logo(
    plot_path = graf,
    logo_path = "/Users/adriana/Desktop/violencia_lgbt+_work/logo/corto_blanco.png",
    logo_position = "bottom right",
    logo_scale = escala)
  
  magick::image_write(graf_con_logo, graf)
}

# Formato para gráficas
v_formato <- ".png"

# Etiquetas
v_caption       <- "Fuente: Encuesta Nacional sobre Diversidad Sexual y de Género (Endiseg) 2021. 
Datos procesados por Intersecta.org
*Se toman en cuenta a las personas que contaron con un trabajo remunerado en el último año (2020-2021)"
v_caption_alt       <- "Fuente: Encuesta Nacional sobre Diversidad Sexual y de Género (Endiseg) 2021. 
Datos procesados por Intersecta.org"
v_percent       <- "\nPorcentaje"
v_empty         <- ""
v_sexo          <- "Por sexo"
v_ori           <- "Por orientación sexual"

# Cargar base y elegir variables
base <- read_csv("/Volumes/GoogleDrive/My Drive/endiseg/base/conjunto_de_datos_tmodulo_endiseg_2021.csv")
base <- clean_names(base)
base <- select(base, folio, n_ren, est_dis, factor, upm_dis, 
               p8_1, p8_1a, p8_1b, p9_1, filtro_9_2, p11_3, p11_4_1, p11_4_2, p11_4_3, p11_4_4, p11_4_5,
               p10_1_3, p10_1_5, p10_2, p10_3, filtro_10_4, p10_4_5, p10_2, p10_3, p4_17c, p4_19, p4_15, p4_7, p4_4)

# Hacer variable de violencia o no violencia en el trabajo
base <- base                                  %>%
  mutate(
    # Incidentes de violencia / discriminación
    violencia = case_when(
      # Sin violencia / discriminación 
      !(p11_4_1 == 1 | p11_4_2 == 1 | p11_4_3 == 1 | p11_4_4 == 1 | p11_4_5 == 1) ~ 0, 
      # Casos donde SÍ hubo violencia / discriminación
      (p11_4_1 == 1 | p11_4_2 == 1 | p11_4_3 == 1 | p11_4_4 == 1 | p11_4_5 == 1) ~ 1, 
      # En todas responde que no sabe o no responde
      (p11_4_1 %in% c(8, 9) & p11_4_2 %in% c(8, 9) & 
        p11_4_3 %in% c(8, 9) & p11_4_4 %in% c(8, 9) & 
        p11_4_5 %in% c(8, 9)) ~ NA_real_))

# Hacer variable de banda LGBT+
base <- base                                  %>%
  mutate(
    # pertenencia a lgbti+
    lgbti = case_when(
      # Sin lgbti
      (p8_1 == 4 | p8_1 == 5 | filtro_9_2 == 2) ~ "Personas no LGBT+", 
      # Con lgbti
      (p8_1 == 1 | p8_1 == 2 | p8_1 == 3 | p8_1 == 6 | filtro_9_2 == 1) ~ "Personas LGBT+")) 
    
# Variable orientación sexual
base <- base                                  %>%
  mutate(orient = case_when(
    p8_1 == 1 ~ "Mujer lesbiana",
    p8_1 == 2 ~ "Hombre gay",
    p8_1 == 3 ~ "Persona bisexual",
    p8_1 == 4 ~ "Mujer heterosexual",
    p8_1 == 5 ~ "Hombre heterosexual",
    p8_1 == 6 ~ "Otra orientación"))

# Variable identidad sexual
base <- base                                  %>%
  mutate(identidad = case_when(
    p9_1 == 1 ~ "Hombre",
    p9_1 == 2 ~ "Mujer",
    p9_1 == 3 ~ "Otro género",
    p9_1 == 4 ~ "Otro género",
    p9_1 == 5 ~ "Otro género"))

# Variable color de piel
base <- base            %>%
  mutate(color = case_when(
   p4_19 == "01" ~ "Tono claro",
   p4_19 == "02" ~ "Tono claro",
   p4_19 == "03" ~ "Tono claro",
   p4_19 == "04" ~ "Tono claro",
   p4_19 == "05" ~ "Tono medio",
   p4_19 == "06" ~ "Tono medio",
   p4_19 == "07" ~ "Tono medio",
   p4_19 == "08" ~ "Tono medio",
   p4_19 == "09" ~ "Tono oscuro",
   p4_19 == "10" ~ "Tono oscuro",
   p4_19 == "11" ~ "Tono oscuro"))

# Aplicar el diseño muestral 
options(survey.lonely.psu="adjust")

base <- base                        %>%
  # Convertir a formato numérico para implementar diseño de encuesta
  # Nota: Como se trata de variables tipo factor, primero se pasa a caracter.
  mutate_at(
    .vars = c("n_ren", "est_dis", "factor"), 
    ~as.numeric(as.character(.)))           %>% 
  # Diseño de encuesta
  as_survey_design(
    ids = n_ren, strata = est_dis, weights = factor, nest=TRUE)  

class(base$variables$n_ren)

# Incidentes de violencia y discriminación por orientación sexual 
v_codes     <- c("p11_4_1", "p11_4_2", "p11_4_3", "p11_4_4", "p11_4_5")
v_eventos   <- c("Recibió comentarios ofensivos o burlas", "Le excluyeron de eventos sociales",
                 "Le molestaron o acosaron", "Recibió un trato desigual respecto a beneficios, prestaciones o ascensos",
                 "Le pegaron, agredieron o amenazaron")

incidentes <- base                                                      %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    p8_1 == 1 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 2 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 3 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 4 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 5 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 6 ~ "Personas que no se identifican\ncomo heterosexuales")) %>%
  rename(respuesta = v_codes[1])                                      %>% 
  select(domi, respuesta)                                             %>%
  srvyr::group_by(domi, respuesta)                                    %>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
  rename(porcentaje = prop)                                         %>%
  mutate(evento = v_eventos[1])   

for(i in 2:length(v_codes)){
  
  print(paste("Vuelta", i, "de", length(v_codes)))
  
  df_data_loop <- base                                                    %>% 
    filter(p11_3 == 1 )%>%
    mutate(domi = case_when(
      p8_1 == 1 ~ "Personas que no se identifican\ncomo heterosexuales",
      p8_1 == 2 ~ "Personas que no se identifican\ncomo heterosexuales",
      p8_1 == 3 ~ "Personas que no se identifican\ncomo heterosexuales",
      p8_1 == 4 ~ "Personas que se identifican\ncomo heterosexuales",
      p8_1 == 5 ~ "Personas que se identifican\ncomo heterosexuales",
      p8_1 == 6 ~ "Personas que no se identifican\ncomo heterosexuales")) %>%
    rename(respuesta = v_codes[i])                                      %>%
    select(domi, respuesta)                                             %>%
    srvyr::group_by(domi, respuesta)                                    %>%
    srvyr::summarise(
      prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
    rename(porcentaje = prop)                                           %>%
    mutate(evento = v_eventos[i]) 
  
  incidentes <- incidentes %>% 
    bind_rows(df_data_loop) %>% 
    filter(respuesta == "1") 
  
}

# Gráfica
v_title     <- "Eventos de discriminación o violencia en el lugar de trabajo"
v_subtitle  <- "En el último año (2020-2021)"

ggplot(incidentes,
       aes(x = porcentaje, y = reorder(evento, porcentaje), fill = evento)) +
  geom_col(fill = "#b627f7") +
  geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = evento),
            position = position_dodge(1), size=2.5, hjust=-.1, vjust=.5, angle = 0,
            color="#b627f7", fontface = "bold") +
  facet_wrap(~domi)+
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  tema +
  theme(strip.background = element_rect(colour="white", fill="white"))+
  #scale_colour_manual(
  #values = c("Población heterosexual" = "#6D67E4","Población LGBT+" = "#46C2CB"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.20)) +
  scale_y_discrete(labels = scales::wrap_format(25)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "violencia_lgbt", v_formato), 
  width = 6, height = 4)
add_intlogo(graf = "/Users/adriana/Desktop/violencia_lgbt+_work/grafs/violencia_lgbt.png", escala = 10)

# Condición de violencia por tipo de orientación sexual
vio_ori <- base                   %>%
  filter(p11_3 == 1 )%>%
  srvyr::group_by(orient, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia == 1) %>% 
  rename(porcentaje = prop)

# Gráfica
v_title     <- "Proporción de personas que vivieron algún tipo de violencia o\ndiscriminación en su lugar de trabajo"
v_subtitle  <- "Por orientación sexual, en el último año (2020-2021)"

ggplot(vio_ori,
       aes(x = porcentaje, y = reorder(orient, porcentaje))) +
  geom_col(fill=c("Otra orientación" = "#FF6B6B", "Mujer lesbiana" = "#FFD93D", "Persona bisexual" = "#6BCB77", 
"Hombre gay" = "#E893CF", "Hombre heterosexual" = "#9376E0", "Mujer heterosexual" = "#EC9B3B"))+
  geom_text(aes(label=paste0(round(porcentaje,3)*100, "%")),
            position = position_stack(), size=2.5, hjust=-.1, vjust=.5, angle = 0,
            color="#b627f7", fontface = "bold") +
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  tema +
  theme(strip.background = element_rect(colour="white", fill="white"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.45)) +
  scale_y_discrete(labels = scales::wrap_format(25)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "violencia_orient", v_formato), 
  width = 6, height = 4)

# Incidentes de violencia y discriminación por orientación sexual 
v_codes     <- c("p11_4_1", "p11_4_2", "p11_4_3", "p11_4_4", "p11_4_5")
v_eventos   <- c("Recibió comentarios ofensivos o burlas", "Le excluyeron de eventos sociales",
                 "Le molestaron o acosaron", "Recibió un trato desigual respecto a beneficios, prestaciones o ascensos",
                 "Le pegaron, agredieron o amenazaron")

incidentes <- base                                                      %>%
  filter(p11_3 == 1 )%>%
  rename(respuesta = v_codes[1])                                      %>% 
  select(orient, respuesta)                                             %>%
  srvyr::group_by(orient, respuesta)                                    %>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
  rename(porcentaje = prop)                                           %>%
  mutate(evento = v_eventos[1])   

for(i in 2:length(v_codes)){
  
  print(paste("Vuelta", i, "de", length(v_codes)))
  
  df_data_loop <- base                                                    %>% 
    filter(p11_3 == 1 )%>%
    rename(respuesta = v_codes[i])                                      %>%
    select(orient, respuesta)                                             %>%
    srvyr::group_by(orient, respuesta)                                    %>%
    srvyr::summarise(
      prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
    rename(porcentaje = prop)                                           %>%
    mutate(evento = v_eventos[i]) 
  
  incidentes <- incidentes %>% 
    bind_rows(df_data_loop) %>% 
    filter(respuesta == "1") 
  
}

# Incidentes de violencia y discriminación por identidad de género 
v_codes     <- c("p11_4_1", "p11_4_2", "p11_4_3", "p11_4_4", "p11_4_5")
v_eventos   <- c("Recibió comentarios ofensivos o burlas", "Le excluyeron de eventos sociales",
                 "Le molestaron o acosaron", "Recibió un trato desigual respecto a beneficios, prestaciones o ascensos",
                 "Le pegaron, agredieron o amenazaron")

incidentes <- base                                                      %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero")) %>%
  rename(respuesta = v_codes[1])                                      %>% 
  select(domi, respuesta)                                             %>%
  srvyr::group_by(domi, respuesta)                                    %>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
  rename(porcentaje = prop)                                           %>%
  mutate(evento = v_eventos[1])   

for(i in 2:length(v_codes)){
  
  print(paste("Vuelta", i, "de", length(v_codes)))
  
  df_data_loop <- base                                                    %>% 
    filter(p11_3 == 1 )%>%
    mutate(domi = case_when(
      filtro_9_2 == 1 ~ "Población trans",
      filtro_9_2 == 2 ~ "Población cisgénero")) %>%
    rename(respuesta = v_codes[i])                                      %>%
    select(domi, respuesta)                                             %>%
    srvyr::group_by(domi, respuesta)                                    %>%
    srvyr::summarise(
      prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
    rename(porcentaje = prop)                                           %>%
    mutate(evento = v_eventos[i]) 
  
  incidentes <- incidentes %>% 
    bind_rows(df_data_loop) %>% 
    filter(respuesta == "1") 
  
}

# Gráfica
v_title     <- "Eventos de discriminación o violencia en el lugar de trabajo"
v_subtitle  <- "En el último año (2020-2021)"

ggplot(incidentes,
       aes(x = porcentaje, y = reorder(evento, porcentaje), fill = evento)) +
  geom_col(fill = "#b627f7") +
  geom_text(aes(label=paste0(round(porcentaje,3)*100, "%"), group = evento),
            position = position_dodge(1), size=2.5, hjust=-.1, vjust=.5, angle = 0,
            color="#b627f7", fontface = "bold") +
  facet_wrap(~domi)+
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  tema +
  theme(strip.background = element_rect(colour="white", fill="white"))+
  #scale_colour_manual(
  #values = c("Población heterosexual" = "#6D67E4","Población LGBT+" = "#46C2CB"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.25)) +
  scale_y_discrete(labels = scales::wrap_format(25)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "violencia_cis_trans", v_formato), 
  width = 6, height = 4)

# Condición de violencia por identidad sexual
vio_identi <- base                   %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero")) %>%
  srvyr::group_by(identidad, domi, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia == 1) %>% 
  rename(porcentaje = prop)

# Gráfica
v_title     <- "Proporción de personas que vivieron algún tipo de violencia o\ndiscriminación en su lugar de trabajo"
v_subtitle  <- "Por identidad sexual, en el último año (2020-2021)"

vio_identi$domi <- factor(vio_identi$domi, levels = c("Población trans", "Población cisgénero"))

ggplot(vio_identi,
       aes(x = porcentaje, y = reorder(identidad, porcentaje))) +
  geom_col(fill=c("Hombre" = "#FF6B6B", "Hombre" = "#FFD93D", "Mujer" = "#6BCB77", "Mujer" = "#E893CF",
                  "Otro género" = "#9376E0"))+
  geom_text(aes(label=paste0(round(porcentaje,3)*100, "%")),
            position = position_stack(), size=2.5, hjust=-.1, vjust=.5, angle = 0,
            color="#b627f7", fontface = "bold") +
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  tema +
  facet_wrap(~domi)+
  theme(strip.background = element_rect(colour="white", fill="white"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.50)) +
  scale_y_discrete(labels = scales::wrap_format(25)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "violencia_identidad", v_formato), 
  width = 6, height = 4)

# Incidentes de violencia y discriminación por identidad sexual 
v_codes     <- c("p11_4_1", "p11_4_2", "p11_4_3", "p11_4_4", "p11_4_5")
v_eventos   <- c("Recibió comentarios ofensivos o burlas", "Le excluyeron de eventos sociales",
                 "Le molestaron o acosaron", "Recibió un trato desigual respecto a beneficios, prestaciones o ascensos",
                 "Le pegaron, agredieron o amenazaron")

incidentes <- base                                                  %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero")) %>%
  rename(respuesta = v_codes[1])                                    %>% 
  select(identidad, domi, respuesta)                                %>%
  srvyr::group_by(identidad, domi, respuesta)                       %>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
  rename(porcentaje = prop)                                         %>%
  mutate(evento = v_eventos[1])   

for(i in 2:length(v_codes)){
  
  print(paste("Vuelta", i, "de", length(v_codes)))
  
  df_data_loop <- base                                                  %>% 
    filter(p11_3 == 1 )%>%
    mutate(domi = case_when(
      filtro_9_2 == 1 ~ "Población trans",
      filtro_9_2 == 2 ~ "Población cisgénero")) %>%
    rename(respuesta = v_codes[i])                                      %>%
    select(identidad, domi, respuesta)                                  %>%
    srvyr::group_by(identidad, domi, respuesta)                         %>%
    srvyr::summarise(
      prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
    rename(porcentaje = prop)                                         %>%
    mutate(evento = v_eventos[i]) 
  
  incidentes <- incidentes %>% 
    bind_rows(df_data_loop) %>% 
    filter(respuesta == "1") 
  
}

# Depresión y ansiedad por tipo de orientación sexual
depre_orient <- base                   %>%
  filter(p11_3 == 1 )%>%
  srvyr::group_by(orient, p10_1_3)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(p10_1_3 == 1) %>% 
  rename(porcentaje = prop)

ansiedad_orient <- base                   %>%
  filter(p11_3 == 1 )%>%
  srvyr::group_by(orient, p10_1_5)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(p10_1_5 == 1) %>% 
  rename(porcentaje = prop)

depre_identidad <- base           %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero"))%>%
  srvyr::group_by(identidad, domi, p10_1_3)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(p10_1_3 == 1) %>% 
  rename(porcentaje = prop)

ansiedad_identidad <- base        %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero")) %>%
  srvyr::group_by(p10_1_5, orient)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(p10_1_5 == 1) %>% 
  rename(porcentaje = prop)

depre_orient <- base                   %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    p8_1 == 1 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 2 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 3 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 4 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 5 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 6 ~ "Personas que no se identifican\ncomo heterosexuales"))%>%
  srvyr::group_by(domi, p10_1_3)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  mutate(tipo="Depresión")%>% 
  filter(p10_1_3 == 1) %>% 
  rename(porcentaje = prop)

ansiedad_orient <- base                   %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    p8_1 == 1 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 2 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 3 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 4 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 5 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 6 ~ "Personas que no se identifican\ncomo heterosexuales"))%>%
  srvyr::group_by(domi, p10_1_5)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  mutate(tipo="Ansiedad")%>% 
  filter(p10_1_5 == 1) %>% 
  rename(porcentaje = prop)

emo_orient <- bind_rows(ansiedad_orient, depre_orient)
emo_orient <- select(emo_orient, domi, porcentaje, tipo)

depre_identidad <- base                   %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero"))%>%
  srvyr::group_by(domi, p10_1_3)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  mutate(tipo="Depresión")%>% 
  filter(p10_1_3 == 1) %>% 
  rename(porcentaje = prop)

ansiedad_identidad <- base                   %>%
  filter(p11_3 == 1 )%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero"))%>%
  srvyr::group_by(domi, p10_1_5)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  mutate(tipo="Ansiedad")%>% 
  filter(p10_1_5 == 1) %>% 
  rename(porcentaje = prop)

emo_identidad <- bind_rows(ansiedad_identidad, depre_identidad)
emo_identidad <- select(emo_identidad, domi, porcentaje, tipo)

depre_lgbti <- base                   %>%
  filter(p11_3 == 1 )%>%
  srvyr::group_by(lgbti, p10_1_3)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  mutate(tipo="Depresión")%>% 
  filter(p10_1_3 == 1) %>% 
  rename(porcentaje = prop)

ansiedad_lgbti <- base                   %>%
  filter(p11_3 == 1 )%>%
  srvyr::group_by(lgbti, p10_1_5)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  mutate(tipo="Ansiedad")%>% 
  filter(p10_1_5 == 1) %>% 
  rename(porcentaje = prop)

emo_lgbti <- bind_rows(ansiedad_lgbti, depre_lgbti)
emo_lgbti <- select(emo_lgbti, lgbti, porcentaje, tipo)

# Gráfica orientación sexual
v_title     <- "Proporción de personas que han experimentado síntomas de ansiedad o depresión"
v_subtitle  <- "En el último año (2020-2021)"

ggplot(emo_lgbti,
       aes(x = porcentaje, y = reorder(lgbti, porcentaje))) +
  geom_col(fill="#b627f7")+
  geom_text(aes(label=paste0(round(porcentaje,3)*100, "%")),
            position = position_stack(), size=2.5, hjust=-.1, vjust=.5, angle = 0,
            color="#b627f7", fontface = "bold") +
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  tema +
  facet_wrap(~tipo)+
  theme(strip.background = element_rect(colour="white", fill="white"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.70)) +
  scale_y_discrete(labels = scales::wrap_format(25)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "emo_lgbti", v_formato), 
  width = 6, height = 4)

# Depresión y ansiedad por tipo de orientación sexual
ideacion_lgbti <- base                   %>%
  filter(p11_3 == 1)%>%
  filter(p10_2 == 1)%>%
  srvyr::group_by(lgbti, p10_4_5)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(p10_4_5 == 1)%>% 
  mutate(tipo="Ideación suicida")%>%
  rename(porcentaje = prop)

intento_lgbti <- base                   %>%
  filter(p11_3 == 1)%>%
  filter(p10_3 == 1)%>%
  srvyr::group_by(lgbti, p10_4_5)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(p10_4_5 == 1) %>% 
  mutate(tipo="Intento de suicidio")%>%
  rename(porcentaje = prop)

suicide_lgbti <- bind_rows(ideacion_lgbti, intento_lgbti)
suicide_lgbti <- select(suicide_lgbti, lgbti, porcentaje, tipo)

# Gráfica lgbti suicide por trabajo
v_title     <- "Proporción de personas que han tenido ideación suicida o han intentado\nsuicidarse por problemas en el trabajo"
v_subtitle  <- "A lo largo de su vida"

ggplot(suicide_lgbti,
       aes(x = porcentaje, y = reorder(lgbti, porcentaje))) +
  geom_col(fill="#b627f7")+
  geom_text(aes(label=paste0(round(porcentaje,3)*100, "%")),
            position = position_stack(), size=2.5, hjust=-.1, vjust=.5, angle = 0,
            color="#b627f7", fontface = "bold") +
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  tema +
  facet_wrap(~tipo)+
  theme(strip.background = element_rect(colour="white", fill="white"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.15)) +
  scale_y_discrete(labels = scales::wrap_format(25)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "suicide_lgbti", v_formato), 
  width = 6, height = 4)

#Color de piel
color_lgbti <- base%>%
  #filter(p11_3 == 1)%>%
  filter(lgbti == "Personas no LGBT+")%>%
  drop_na(color)%>%
  srvyr::group_by(color, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas no LGBT+")%>%
  rename(porcentaje = prop)

color_lgbti_c <- base%>%
  #filter(p11_3 == 1)%>%
  filter(lgbti == "Personas LGBT+")%>%
  drop_na(color)%>%
  srvyr::group_by(color, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas LGBT+")%>%
  rename(porcentaje = prop)

color <- bind_rows(color_lgbti, color_lgbti_c)
color <- select(color, color, porcentaje, tipo)

#gráfica tono de piel y violencia
v_title     <- "Proporción de personas que vivieron algún tipo de violencia o\ndiscriminación en su lugar de trabajo"
v_subtitle  <- "Por color de piel"

color$color <- factor(color$color, levels = c("Tono claro", "Tono medio", "Tono oscuro"))

ggplot(color,
       aes(x = porcentaje, y = color)) +
  geom_col(fill=c("Tono claro" = "#e6bc98", "Tono medio" = "#a16e4b", "Tono oscuro" = "#3b2219", 
                  "Tono claro" = "#e6bc98", "Tono medio" = "#a16e4b", "Tono oscuro" = "#3b2219"))+
  geom_text(aes(label=paste0(round(porcentaje,3)*100, "%")),
            position = position_stack(), size=2.5, hjust=-.1, vjust=.5, angle = 0,
            color="#b627f7", fontface = "bold") +
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption_alt) +
  tema +
  facet_wrap(~tipo)+
  theme(strip.background = element_rect(colour="white", fill="white"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,.50)) +
  scale_y_discrete(labels = scales::wrap_format(25)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "violencia_color", v_formato), 
  width = 6, height = 4)

color_hetero <- base%>%
  mutate(domi = case_when(
    p8_1 == 1 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 2 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 3 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 4 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 5 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 6 ~ "Personas que no se identifican\ncomo heterosexuales")) %>%
  filter(domi == "Personas que se identifican\ncomo heterosexuales")%>%
  drop_na(color)%>%
  srvyr::group_by(color, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas que se identifican\ncomo heterosexuales")%>%
  rename(porcentaje = prop)

color_gay <- base%>%
  mutate(domi = case_when(
    p8_1 == 1 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 2 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 3 ~ "Personas que no se identifican\ncomo heterosexuales",
    p8_1 == 4 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 5 ~ "Personas que se identifican\ncomo heterosexuales",
    p8_1 == 6 ~ "Personas que no se identifican\ncomo heterosexuales")) %>%
  filter(domi == "Personas que no se identifican\ncomo heterosexuales")%>%
  drop_na(color)%>%
  srvyr::group_by(color, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas que no se identifican\ncomo heterosexuales")%>%
  rename(porcentaje = prop)


color_cis <- base%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero"))%>%
  filter(domi == "Población cisgénero")%>%
  drop_na(color)%>%
  srvyr::group_by(color, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  rename(porcentaje = prop)

color_trans <- base%>%
  mutate(domi = case_when(
    filtro_9_2 == 1 ~ "Población trans",
    filtro_9_2 == 2 ~ "Población cisgénero"))%>%
  filter(domi == "Población trans")%>%
  drop_na(color)%>%
  srvyr::group_by(color, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  rename(porcentaje = prop)

color_orient <- base%>%
  drop_na(color)%>%
  srvyr::group_by(color, orient, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  rename(porcentaje = prop)

#Autoadscripción indígena
indi_hetero <- base%>%
  #filter(p11_3 == 1)%>%
  filter(lgbti == "Personas no LGBT+")%>%
  drop_na(color)%>%
  srvyr::group_by(p4_7, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas no LGBT+")%>%
  filter(p4_7== 1)%>%
  rename(porcentaje = prop)

indi_lgbti <- base%>%
  #filter(p11_3 == 1)%>%
  filter(lgbti == "Personas LGBT+")%>%
  drop_na(color)%>%
  srvyr::group_by(p4_7, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas LGBT+")%>%
  filter(p4_7== 1)%>%
  rename(porcentaje = prop)

indi_nada <- base%>%
  #filter(p11_3 == 1)%>%
  filter(lgbti == "Personas no LGBT+")%>%
  drop_na(color)%>%
  srvyr::group_by(p4_7, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas no LGBT+")%>%
  filter(p4_7== 2)%>%
  rename(porcentaje = prop)

#Autoadscripción afro
afro_hetero <- base%>%
  #filter(p11_3 == 1)%>%
  filter(lgbti == "Personas no LGBT+")%>%
  drop_na(p4_4)%>%
  srvyr::group_by(p4_4, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas no LGBT+")%>%
  rename(porcentaje = prop)

afro_hetero <- base%>%
  #filter(p11_3 == 1)%>%
  filter(lgbti == "Personas LGBT+")%>%
  drop_na(p4_4)%>%
  srvyr::group_by(p4_4, violencia)%>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
  filter(violencia==1)%>%
  mutate(tipo = "Personas LGBT+")%>%
  rename(porcentaje = prop)


