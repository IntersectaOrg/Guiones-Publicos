#------------------------------------------------------------------------------#
# Base de datos:              ENPOL 2021
# Objetivo:                   Experiencia de las mujeres trans dentro del 
#                             sistema penal
#
# Encargadas:                 Fernanda Torres y Adriana E. Ortega
# Correo:                     ftorres@intersecta.org - aortega@intersecta.org
# Fecha de creación:          30 de febrero de 2022
# Última actualización:       13 de mayo de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Fuente:            https://www.inegi.org.mx/programas/enpol/2021/

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
inp         <- "/Users/adriana/Google Drive/ENPOL/03_datos_limpios/"
out_figs    <- "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/grafs"

# Logo Intersecta
add_intlogo <- function(graf, escala){
  graf_con_logo <- add_logo(
    plot_path = graf,
    logo_path = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/logo/corto_blanco.png",
    logo_position = "bottom right",
    logo_scale = escala)
  
  magick::image_write(graf_con_logo, graf)
}

# Tema para gráficas 
tema <-  theme_linedraw() +
  theme(
    text             = element_text(family = "Avenir Next Condensed", color = "black"),
    plot.title       = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Avenir Next Condensed", color = "black"),
    plot.subtitle    = element_text(size = 13, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Avenir Next Condensed"),
    plot.caption     = element_text(hjust = .5, size = 9, family = "Avenir Next Condensed", color = "black"),
    panel.grid       = element_line(linetype = 2), 
    legend.position  = "top",
    panel.grid.minor = element_blank(),
    legend.title     = element_text(size = 10, face = "bold", family="Avenir Next Condensed"),
    legend.text      = element_text(size = 10, family="Avenir Next Condensed"),
    axis.title       = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Avenir Next Condensed"),
    axis.text.y      = element_text(size = 8, family="Avenir Next Condensed", angle=0, hjust=.5),
    axis.text.x      = element_text(size = 8, family="Avenir Next Condensed", angle=90, hjust=.5),
    strip.background = element_rect(fill="white", colour = NA),
    strip.text.x     = element_text(size=9, family = "Avenir Next Condensed", face = "bold", color = "black"),
    strip.text.y     = element_text(size=9, family = "Avenir Next Condensed", face = "bold", color = "black"))

v_colors <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2", "#2ec4b6", "#cbf3f0", "#ffbf69", 
              "#ff006e", "#8338ec", "#3a86ff", "gray")
v_colshort <- c("#ef476f", "#ffd166", "#06d6a0", "#118ab2")

# Formato para gráficas
v_formato <- ".png"

# Etiquetas
v_caption       <- "Fuente: Encuesta Nacional de Población Privada de la Libertad 2021 (ENPOL). 
Datos procesados por Intersecta.org"
v_percent       <- "\nPorcentaje"
v_empty         <- ""
v_sexo          <- "Por sexo"
v_autoridad     <- "Por autoridad"

# Cargar base y elegir variables
# ENPOL 2021
base <- load(paste0(inp, "df_ENPOL_2021.RData"))
base <- df_ENPOL_2021
base <- clean_names(base)
base <- select(base, id_per, fpc, est_dis, fac_per, sexo, p1_3, p3_2, p3_3, p3_5_a, p3_10, p3_11, p3_12_1:p3_12_5,p3_14_1, p3_14_2, p3_14_3,
                  p3_15_1:p3_15_9, p3_14_4, p3_14_5, p3_14_6, p11_1_1, p11_1_2, p11_1_3, p11_1_4, p5_31_01:p5_31_26, p5_11_01:p5_11_26, p3_13_01:p3_13_12,
                  p3_14_1:p3_14_6, p3_15_1:p3_15_9, p3_17_01:p3_17_11, p3_18_01:p3_18_15, p4_1_02, p4_1_04, p4_1_05, p4_1_07, p4_1_09, p4_1_10, p1_22, p7_25,
                  p7_36, p7_46_09, p3_19, p4_1_09, p1_29)

# Codificar autoridades
codificar_autoridad <- function(var = x){
  
  v_autoridades   <- c("Policía Municipal", "Policía Estatal", 
                       "Policía Federal", "Policía Estatal Ministerial", 
                       "Policía Federal Ministerial", "base Nacional", "Ejército", 
                       "Marina", "Operativo conjunto", "Otra autoridad", "Otros")
  
  case_when(
    var == "01" ~ v_autoridades[1], 
    var == "02" ~ v_autoridades[2], 
    var == "03" ~ v_autoridades[3], 
    var == "04" ~ v_autoridades[4], 
    var == "05" ~ v_autoridades[5], 
    var == "06" ~ v_autoridades[6], 
    var == "07" ~ v_autoridades[7], 
    var == "08" ~ v_autoridades[8], 
    var == "09" ~ v_autoridades[9], 
    var == "10" ~ v_autoridades[10], 
    TRUE ~ v_autoridades[11])
}

## 2.9. Sí/No 2 opciones -------------------------------------------------------
codificar_siono2 <- function(var = x){
  
  v_respuestas <- c("No", "Sí")
  
  case_when(
    var == "0" ~ v_respuestas[1],
    var == "1" ~ v_respuestas[2]    )
}

## Binarias: Sí/No --------------------------------------------------------

codificar_siono4 <- function(var = x){
  
  v_respuestas <- c("Sí", "No", "No sabe", "No responde")
  
  case_when(
    var == "1" ~ v_respuestas[1],
    var == "2" ~ v_respuestas[2],
    var == "8" ~ v_respuestas[3],
    var == "9" ~ v_respuestas[4]
  )
}

## Identidad de género ----------------------------------------------------
codificar_identidad <- function(var = x){
  
  v_identidad   <- c("Hombre cis", "Mujer cis", "Mujer trans", "Hombre trans", "Otra", "Otros")
  
  case_when(
    var == "1" ~ v_identidad[1], 
    var == "2" ~ v_identidad[2], 
    var == "3" ~ v_identidad[3], 
    var == "4" ~ v_identidad[4], 
    var == "5" ~ v_identidad[5],
    TRUE ~ v_identidad[6])
}

# Crear variables agrupadas binarias (uso de fuerza y tipos de violencia)
base <- base                                  %>%
  mutate(
    # Arrestos con abuso de fuerza
    uso_fuerza = case_when(
      # Sin abuso de fuerza 
      !(p3_13_02 == 1 | p3_13_03 == 0 | p3_13_04 == 1 | p3_13_05 == 1 | 
          p3_13_06 == 1 | p3_13_07 == 1 | p3_13_08 == 1 | p3_13_09 == 1 | 
          p3_13_10 == 1 | p3_13_11 == 1 | p3_13_12 == 1) ~ 0, 
      # Casos en donde se hubo algún tipo de abuso de fuerza
      (p3_13_02 == 1 | p3_13_03 == 0 | p3_13_04 == 1 | p3_13_05 == 1 | 
         p3_13_06 == 1 | p3_13_07 == 1 | p3_13_08 == 1 | p3_13_09 == 1 | 
         p3_13_10 == 1 | p3_13_11 == 1 | p3_13_12 == 1) ~ 1, 
      # En todas responde que no sabe o no responde
      (p3_13_02 %in% c(8, 9) & p3_13_03 %in% c(8, 9) & 
         p3_13_04 %in% c(8, 9) & p3_13_05 %in% c(8, 9) & 
         p3_13_06 %in% c(8, 9) & p3_13_07 %in% c(8, 9) & 
         p3_13_08 %in% c(8, 9) & p3_13_09 %in% c(8, 9) & 
         p3_13_10 %in% c(8, 9) & p3_13_11 %in% c(8, 9) & 
         p3_13_12 %in% c(8, 9) & p3_13_07 %in% c(8, 9) & 
         p3_13_08 %in% c(8, 9)) ~ NA_real_), 
    # Violencia psicológica 
    violencia_psic = case_when(
      # Casos donde no hubo violencia psicológica
      !(p3_17_01 == 1 | p3_17_02 == 1 | p3_17_03 == 1 | p3_17_04 == 1 | 
          p3_17_05 == 1 | p3_17_06 == 1 | p3_17_07 == 1 | p3_17_08 == 1 | 
          p3_17_09 == 1 | p3_17_11 == 1) ~ 0,
      # Casos donde sí hubo violencia psicológica
      (p3_17_01 == 1 | p3_17_02 == 1 | p3_17_03 == 1 | p3_17_04 == 1 | 
         p3_17_05 == 1 | p3_17_06 == 1 | p3_17_07 == 1 | p3_17_08 == 1 | 
         p3_17_09 == 1 | p3_17_11 == 1) ~ 1,
      # Casos donde no sabe o no respondió 
      (p3_17_01 %in% c(8, 9) & p3_17_02 %in% c(8, 9) & 
         p3_17_03 %in% c(8, 9) & p3_17_04 %in% c(8, 9) & 
         p3_17_05 %in% c(8, 9) & p3_17_06 %in% c(8, 9) & 
         p3_17_07 %in% c(8, 9) & p3_17_08 %in% c(8, 9) & 
         p3_17_09 %in% c(8, 9) & p3_17_11 %in% c(8, 9)) ~ NA_real_), 
    # Indicador de violencia física
    violencia_fisica = case_when(
      # Casos donde no hubo violencia física
      !(p3_18_01 == 1 | p3_18_02 == 1 | p3_18_03 == 1 | p3_18_04 == 1 | 
          p3_18_05 == 1 | p3_18_06 == 1 | p3_18_07 == 1 | p3_18_08 == 1 | 
          p3_18_09 == 1 | p3_18_10 == 1 | p3_18_11 == 1 | p3_18_15 == 1) ~ 0,
      # Casos donde sí hubo violencia física
      (p3_18_01 == 1 | p3_18_02 == 1 | p3_18_03 == 1 | p3_18_04 == 1 | 
         p3_18_05 == 1 | p3_18_06 == 1 | p3_18_07 == 1 | p3_18_08 == 1 | 
         p3_18_09 == 1 | p3_18_10 == 1 | p3_18_11 == 1 | p3_18_15 == 1) ~ 1, 
      # Casos donde no sabe o no respondió 
      (p3_18_01 %in% c(8, 9) & p3_18_02 %in% c(8, 9) &
         p3_18_03 %in% c(8, 9) & p3_18_04 %in% c(8, 9) & 
         p3_18_05 %in% c(8, 9) & p3_18_06 %in% c(8, 9) & 
         p3_18_07 %in% c(8, 9) & p3_18_08 %in% c(8, 9) & 
         p3_18_09 %in% c(8, 9) & p3_18_10 %in% c(8, 9) & 
         p3_18_11 %in% c(8, 9) & p3_18_15 %in% c(8, 9) ~ NA_real_)),
    # Indicador de violencia sexual 
    violencia_sexual = case_when(
      !(p3_17_10 == 1 |  p3_18_12 == 1 |  p3_18_13 == 1 | p3_18_14 == 1) ~ 0,  
      (p3_17_10 == 1 |  p3_18_12 == 1 |  p3_18_13 == 1 | p3_18_14 == 1) ~ 1, 
      (p3_17_10 %in% c(8, 9) & p3_18_12 %in% c(8, 9) &
         p3_18_13 %in% c(8, 9) & p3_18_14 %in% c(8, 9)) ~ NA_real_), 
    #Juntar las dos variables de tortura
    violencia = ifelse(
      violencia_psic == 1 | violencia_fisica == 1 | violencia_sexual == 1,
      1, 0)) 

# Usar la funciones para recodificar 
base <- base                     %>% 
  mutate(
    p3_2      = codificar_autoridad(p3_2), 
    autoridad = p3_2, 
    p1_22     = codificar_identidad(p1_22),
    identidad = p1_22)   

# Aplicar el diseño muestral 
base <- base                       %>%
  # Convertir a formato numérico para implementar diseño de encuesta
  # Nota: Como se trata de variables tipo factor, primero se pasa a caracter.
  mutate_at(
    .vars = c("id_per", "est_dis", "fpc", "fac_per","p1_3", "p3_5_a"), 
    ~as.numeric(as.character(.)))           %>% 
  # Diseño de encuesta
  as_survey_design(
    ids = id_per, strata = est_dis, weights = fac_per, fpc = fpc)  

class(base$variables$id_per)

# Estimar distribución de detenciones por autoridad por identidad de género
checar_aut <- base                      %>%
  filter(identidad != "Otra")%>%
  filter(identidad != "Otros")%>%
  srvyr::group_by(identidad, autoridad)        %>%
  # Estimar el porcentaje de personas con intervalos de confianza
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>%
  filter(identidad != "NA") %>%
  rename(porcentaje = prop)

# Gráfica
v_title     <- "En México, ¿qué autoridades realizan las detenciones?"
v_subtitle  <- "Por identidad de género"

ggplot(checar_aut,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(autoridad, porcentaje), fill = autoridad)) +
  geom_col() +
  geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = autoridad),
             position = position_stack(1), size=3, hjust=.5, vjust=.5, angle = 0, fill = "white",
             color="black",  family = "Fira Sans") +
  facet_wrap(~identidad) +
  # Etiquetas
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  # Tema 
  tema +
  guides(fill = "none") +
  scale_fill_manual(values = rev(v_colors)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_discrete(labels = scales::wrap_format(15)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "arresto_", "aut_identidad", "porct", v_formato), 
  width = 10, height = 9)
add_intlogo(graf = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/arresto_aut_identidadporct.png", escala = 10)

# ¿Cómo sucedieron las detenciones? por identidad de género
como_id <- base                      %>%
  filter(p3_10 != "9") %>%
  filter(identidad != "Otra")%>%
  filter(identidad != "Otros")%>%
  mutate(como1 = case_when(
    p3_10 == 1 ~ "Realizando la conducta por la que le acusan",
    p3_10 == 2 ~ "Inmediatamente después de realizar la conducta",
    p3_10 == 3 ~ "Con una orden de detención",
    p3_10 == 4 ~ "Después de una inspección",
    p3_10 == 5 ~ "Ninguna de las anteriores")) %>%
  srvyr::group_by(identidad, como1)          %>%
  # Estimar el porcentaje de personas con intervalos de confianza
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>%
  filter(identidad != "NA") %>%
  rename(porcentaje = prop)

# Gráfica
v_title     <- "En México, ¿cómo se realizaron las detenciones?"
v_subtitle  <- "Por identidad de género"

ggplot(como_id,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(como1, porcentaje), fill = como1)) +
  geom_col() +
  geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = como1),
             position = position_stack(1), size=3, hjust=.5, vjust=.5, angle = 0, fill = "white",
             color="black",  family = "Fira Sans") +
  facet_wrap(~identidad) +
  # Etiquetas
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  # Tema 
  tema +
  guides(fill = "none") +
  scale_fill_manual(values = rev(v_colors)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_discrete(labels = scales::wrap_format(15)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "arresto_", "como_id", "porct", v_formato), 
  width = 10, height = 9)
add_intlogo(graf = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/arresto_como_idporct.png", escala = 10)

# ¿Cómo sucedieron las detenciones que no fueron con orden judicial ni en flagrancia? por identidad de género
como_id_dos <- base                     %>%
  filter(p3_11 != "8") %>%
  filter(p3_11 != "9") %>%
  filter(identidad != "Otra")%>%
  filter(identidad != "Otros")%>%
  mutate(como2 = case_when(
    p3_11 == 1 ~ "Sacándole del lugar en donde estaba",
    p3_11 == 2 ~ "Mientras iba pasando por la calle",
    p3_11 == 3 ~ "De otra manera")) %>%
  srvyr::group_by(identidad, como2)      %>%
  # Estimar el porcentaje de personas con intervalos de confianza
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>%
  filter(identidad != "NA") %>%
  rename(porcentaje = prop)

# Gráfica
v_title     <- "En México, ¿cómo fueron las detenciones que no se realizaron\npor flagrancia o con alguna orden judicial?"
v_subtitle  <- "Por identidad de género"

ggplot(como_id_dos,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(como2, porcentaje), fill = como2)) +
  geom_col() +
  geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = como2),
             position = position_stack(1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
             color="black",  family = "Fira Sans") +
  facet_wrap(~identidad, ncol = 1) +
  # Etiquetas
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  # Tema 
  tema +
  guides(fill = "none") +
  scale_fill_manual(values = rev(v_colors)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_discrete(labels = scales::wrap_format(15)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "arresto_", "como_id_dos", "porct", v_formato), 
  width = 6, height = 9)
add_intlogo(graf = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/arresto_como_id_dosporct.png", escala = 10)

# Proporción de personas que vivieorn algún incidente de violencia por identidad de género
violencia_id <- base                     %>%
  filter(identidad != "Otra")%>%
  filter(identidad != "Otros")%>%
  srvyr::group_by(identidad, violencia)          %>%
  # Estimar el porcentaje de personas con intervalos de confianza
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>%
  filter(identidad != "NA") %>%
  filter(violencia == 1) %>%
  rename(porcentaje = prop)

# Gráfica
v_title     <- "En México, ¿qué proporción de personas vivieron algún incidente\nde violencia durante su detención?"
v_subtitle  <- "Por identidad de género"

ggplot(violencia_id,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(identidad, porcentaje), fill = identidad)) +
  geom_col() +
  geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = identidad),
             position = position_stack(1), size=3, hjust=.5, vjust=.5, angle = 0, fill = "white",
             color="black",  family = "Fira Sans") +
  # Etiquetas
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  # Tema 
  tema +
  guides(fill = "none") +
  scale_fill_manual(values = rev(v_colors)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_discrete(labels = scales::wrap_format(15)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "arresto_", "violencia_id", "porct", v_formato), 
  width = 8, height = 7)
add_intlogo(graf = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/arresto_violencia_idporct.png", escala = 10)

# Percepción de seguridad en el centro penitenciario por identidad de género
seg_id <- base                     %>%
  filter(identidad != "Otra")%>%
  filter(identidad != "Otros")%>%
  filter(p7_36 != "8" | p7_36 != "9")%>%
  mutate (seg_cp = case_when(
    p7_36 == 1 ~ "Seguro/a",
    p7_36 == 2 ~ "Inseguro/a"))        %>%
  srvyr::group_by(identidad, seg_cp)        %>%
  # Estimar el porcentaje de personas con intervalos de confianza
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>%
  filter(identidad != "NA") %>%
  filter(seg_cp != "NA") %>%
  filter(seg_cp == "Inseguro/a")%>%
  rename(porcentaje = prop)

# Gráfica
v_title     <- "Proporción personas privadas de la libertad que se siente insegura\ndentro de los centros penitenciarios mexicanos"
v_subtitle  <- "Por identidad de género"

ggplot(seg_id,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(identidad, porcentaje), fill = identidad)) +
  geom_col() +
  geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = identidad),
             position = position_stack(), size=3, hjust=.5, vjust=.5, angle = 0, fill = "white",
             color="black",  family = "Fira Sans") +
  # Etiquetas
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  # Tema 
  tema +
  guides(fill = "none") +
  scale_fill_manual(values = rev(v_colors)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_discrete(labels = scales::wrap_format(15)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "seguridad_cp_id", "porct", v_formato), 
  width = 8, height = 7)
add_intlogo(graf = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/seguridad_cp_idporct.png", escala = 10)

#Lesiones por la detención por identidad
lesiones_id <- base                           %>% 
  filter(identidad != "Otra")%>%
  filter(identidad != "Otros")%>%
  filter(p3_19 != "02")                       %>%
  rename(lesiones = p4_1_09)                  %>% 
  srvyr::group_by(identidad, lesiones)        %>%
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>%
  filter(identidad != "NA") %>%
  filter(lesiones == 1) %>%
  rename(porcentaje = prop)

# Gráfica  
v_title     <- "En México, ¿qué proporción de las personas privadas de la libertad\ntenían lesiones ocasionadas en la detención?" 
v_subtitle  <- "Por identidad de género"

ggplot(lesiones_id,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(identidad, porcentaje), fill = identidad)) +
  geom_col() +
  geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = identidad),
             position = position_stack(1), size=3, hjust=.5, vjust=.5, angle = 0, fill = "white",
             color="black",  family = "Fira Sans") +
  # Etiquetas
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  # Tema 
  tema +
  guides(fill = "none") +
  scale_fill_manual(values = rev(v_colors)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_discrete(labels = scales::wrap_format(15)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "lesiones_mp_id", "porct", v_formato), 
  width = 8, height = 7)
add_intlogo(graf = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/lesiones_mp_idporct.png", escala = 10)

# Tipo de violencia por identidad de género
v_codes <- c( "violencia_fisica", "violencia_psic", "violencia_sexual")
v_tipov <- c("Violencia física", "Violencia psicológica", "Violencia sexual")

violencia_tipo <- base                                                    %>% 
  rename(respuesta = v_codes[1])                                          %>% 
  select(identidad, respuesta)                                            %>% 
  mutate(respuesta = codificar_siono2(as.character(respuesta)))           %>% 
  srvyr::group_by(identidad, respuesta)                                   %>% 
  srvyr::summarise( 
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))          %>% 
  rename(porcentaje = prop)                                               %>% 
  mutate(tipo = v_tipov[1]) 

for(i in 2:length(v_codes)){ 
  
  print(paste("Vuelta", i, "de", length(v_codes))) 
  
  df_data_loop <- base                                                    %>% 
    rename(respuesta = v_codes[i])                                        %>% 
    select(identidad, respuesta)                                          %>% 
    mutate(respuesta = codificar_siono2(as.character(respuesta)))         %>% 
    # drop_na() %>% 
    srvyr::group_by(identidad, respuesta)                                 %>% 
    srvyr::summarise( 
      prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))        %>% 
    rename(porcentaje = prop)                                             %>% 
    mutate(tipo = v_tipov[i]) 
  
  violencia_tipo <- violencia_tipo %>% bind_rows(df_data_loop) 
} 

# Gráfica
v_title     <- "En México, ¿qué proporción de personas vivieron algún incidente\nde violencia durante su detención?"
v_subtitle  <- "Por tipo de violencia e identidad de género"

ggplot(violencia_tipo %>% filter(respuesta == "Sí", 
                          identidad != "Otros"),  
       aes(y = porcentaje, x = reorder(tipo, -porcentaje), fill = tipo)) + 
  facet_wrap(~identidad, ncol = 1) + 
  coord_flip() +
  geom_col() + 
  geom_label(aes( 
    label=paste0(round(porcentaje,3)*100, "%"), group = tipo), 
    position = position_stack(1), size=3, hjust=.5, vjust=.5,  
    angle = 0, fill = "white", 
    color="black",  family = "Fira Sans") + 
  # Etiquetas 
  labs( 
    title    = v_title,  
    subtitle = v_subtitle,  
    x        = v_empty,  
    y        = v_percent, 
    fill     = v_empty, 
    caption  = v_caption) + 
  # Tema  
  tema + 
  guides(fill = "none") + 
  #scale_fill_manual(values = v_colors) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_x_discrete(labels = scales::wrap_format(15)) + 
  theme(axis.text.x = element_text(angle = 0)) 
ggsave(
  file = paste0(out_figs, "lesiones_mp_id", "porct", v_formato), 
  width = 6, height = 9)
add_intlogo(graf = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/lesiones_mp_idporct.png", escala = 11)

# Proporción de personas privadas de la libertad que pensaron en quitarse la vida 
pensar_id <- base                             %>% 
  filter(identidad != "Otra")%>%
  filter(identidad != "Otros")%>%
  rename(pensar = p1_29)                      %>% 
  srvyr::group_by(identidad, pensar)          %>% 
  # Estimar el porcentaje de personas con intervalos de confianza 
  srvyr::summarise(
    prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>%
  filter(identidad != "NA") %>%
  filter(pensar == 1) %>%
  rename(porcentaje = prop)

# Gráfica  
v_title     <- "Proporción de personas en centros penitenciarios mexicanos\nque ha pensado en quitarse la vida" 
v_subtitle <- "Por identidad de género"

ggplot(pensar_id,
       # Coordenadas y geoms
       aes(x = porcentaje, y = reorder(identidad, porcentaje), fill = identidad)) +
  geom_col() +
  geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = identidad),
             position = position_stack(1), size=3, hjust=.5, vjust=.5, angle = 0, fill = "white",
             color="black",  family = "Fira Sans") +
  # Etiquetas
  labs(
    title    = v_title, 
    subtitle = v_subtitle, 
    x        = v_percent, 
    y        = v_empty,
    caption  = v_caption) +
  # Tema 
  tema +
  guides(fill = "none") +
  scale_fill_manual(values = rev(v_colors)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_discrete(labels = scales::wrap_format(15)) +
  theme(axis.text.x = element_text(angle = 0))

ggsave(
  file = paste0(out_figs, "ideacion_suicida_id", "porct", v_formato), 
  width = 8, height = 7)
add_intlogo(graf = "/Users/adriana/Google Drive/ENPOL/04_figuras/2021/post_mujeres_trans/ideacion_suicida_idporct.png", escala = 10)
