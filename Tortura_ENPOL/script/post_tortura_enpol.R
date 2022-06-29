#------------------------------------------------------------------------------#
# Proyecto:                   ENPOL 2021
# Objetivo:                   Gráficas post la vieja confiable: tortura
#
# Encargadas:                 Adriana E. Ortega (INTR)
# Correo:                     aortega@intersecta.org
# Fecha de creación:          29 de junio de 2022
# Última actualización:       --
#------------------------------------------------------------------------------#

# Fuente:            https://www.inegi.org.mx/programas/enpol/2021/

# URL Drive de los datos de la ENPOL2016 en el drive de  INTR:
# - https://drive.google.com/drive/u/0/folders/1heccHThnKXTZkbVHNvxXFBREox-sZlUx


# URL Drive de los datos de la ENPOL2021 en el drive de  INTR:
# - https://drive.google.com/drive/u/0/folders/1SUjq9_9wAudhNbHaKi1AdGum95D8prAZ

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(srvyr, tidyverse, dplyr, lubridate, scales, beepr, janitor, magick, add2ggplot)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp         <- "03_datos_limpios/"
out_data    <- "03_datos_limpios/"
out_figs    <- "04_figuras/2021/post_tortura/"

# logo intersecta
add_intlogo <- function(graf, escala){
    graf_con_logo <- add_logo(
        plot_path = graf,
        logo_path = "04_figuras/2021/post_tortura/logo/corto_blanco.png",
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
load(paste0(inp, "df_ENPOL_2021.RData"))
base <- df_ENPOL_2021
base <- clean_names(base)
base <- select(base, id_per, fpc, est_dis, fac_per, sexo, p1_3, p3_1, p3_2, p3_3, p3_5_a, p3_10, p3_11, p3_12_1:p3_12_5,p3_14_1, p3_14_2, p3_14_3,
               p3_15_1:p3_15_9, p3_14_4, p3_14_5, p3_14_6, p11_1_1, p11_1_2, p11_1_3, p11_1_4, p5_31_01:p5_31_26, p5_11_01:p5_11_26, p3_13_01:p3_13_12,
               p3_14_1:p3_14_6, p3_15_1:p3_15_9, p3_17_01:p3_17_11, p3_18_01:p3_18_15, p4_1_02, p4_1_04, p4_1_05, p4_1_07, p4_1_09, p4_1_10, p1_22, p7_25,
               p7_36, p7_46_09)

# Definir funcion para cambiar los códigos por el nombre de las autoridades
codificar_autoridad <- function(var = x){
    
    v_autoridades   <- c("Policía Municipal", "Policía Estatal", 
                         "Policía Federal", "Policía Estatal Ministerial", 
                         "Policía Federal Ministerial", "Guardia Nacional", "Ejército", 
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

## 1.1. Binarias: Sí/No --------------------------------------------------------

codificar_siono4 <- function(var = x){
    
    v_respuestas <- c("Sí", "No", "No sabe", "No responde")
    
    case_when(
        var == "1" ~ v_respuestas[1],
        var == "2" ~ v_respuestas[2],
        var == "8" ~ v_respuestas[3],
        var == "9" ~ v_respuestas[4]
    )
}

# Definir funcion para cambiar los códigos por el nombre de la entidad en la que sucedió la detención 
codificar_entidad   <- function(var = x){
    
    v_entidades     <- c(
        "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
        "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", 
        "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", 
        "Estado de México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", 
        "Oaxaca", "Puebla", "Querétaro",  "Quintana Roo", "San Luis Potosí", 
        "Sinaloa", "Sonora",  "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", 
        "Yucatán", "Zacatecas", "Fuera de México", "No especificado")
    
    case_when(
        var == "01" ~ v_entidades[1], 
        var == "02" ~ v_entidades[2], 
        var == "03" ~ v_entidades[3], 
        var == "04" ~ v_entidades[4], 
        var == "05" ~ v_entidades[5], 
        var == "06" ~ v_entidades[6], 
        var == "07" ~ v_entidades[7], 
        var == "08" ~ v_entidades[8], 
        var == "09" ~ v_entidades[9], 
        var == "10" ~ v_entidades[10],   
        var == "11" ~ v_entidades[11], 
        var == "12" ~ v_entidades[12], 
        var == "13" ~ v_entidades[13], 
        var == "14" ~ v_entidades[14], 
        var == "15" ~ v_entidades[15], 
        var == "16" ~ v_entidades[16], 
        var == "17" ~ v_entidades[17], 
        var == "18" ~ v_entidades[18], 
        var == "19" ~ v_entidades[19], 
        var == "20" ~ v_entidades[20], 
        var == "21" ~ v_entidades[21],
        var == "22" ~ v_entidades[22],
        var == "23" ~ v_entidades[23],
        var == "24" ~ v_entidades[24],
        var == "25" ~ v_entidades[25],
        var == "26" ~ v_entidades[26],
        var == "27" ~ v_entidades[27],
        var == "28" ~ v_entidades[28],
        var == "29" ~ v_entidades[29],
        var == "30" ~ v_entidades[30],
        var == "31" ~ v_entidades[31],
        var == "32" ~ v_entidades[32],
        var == "98" ~ v_entidades[98],
        var == "99" ~ v_entidades[99])
}

# Usar la función para recodificar 
base <- base                      %>% 
    mutate(
        p3_2      = codificar_autoridad(p3_2), 
        autoridad = p3_2, 
        entidad   = codificar_entidad(p3_3))    %>% 
    mutate(
        sexo = case_when(
            sexo == 1 ~ "Hombres", 
            sexo == 2 ~ "Mujeres")) 

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

# Aplicar el diseño muestral 
base <- base                        %>%
    # Convertir a formato numérico para implementar diseño de encuesta
    # Nota: Como se trata de variables tipo factor, primero se pasa a caracter.
    mutate_at(
        .vars = c("id_per", "est_dis", "fpc", "fac_per","p1_3", "p3_5_a"), 
        ~as.numeric(as.character(.)))           %>% 
    # Diseño de encuesta
    as_survey_design(
        ids = id_per, strata = est_dis, weights = fac_per, fpc = fpc)  

class(base$variables$id_per)

# incidentes de violencia por sexo y por tipo de violencia
vio_ps <- base                   %>%
    srvyr::group_by(sexo, violencia_psic)%>%
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
    filter(violencia_psic == 1) %>% 
    mutate(tipo = "Violencia psicológica") %>%
    rename(porcentaje = prop)

vio_fis <- base                   %>%
    srvyr::group_by(sexo, violencia_fisica)%>%
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
    filter(violencia_fisica == 1) %>%
    mutate(tipo = "Violencia física") %>%
    rename(porcentaje = prop)

vio_sex <- base                   %>%
    srvyr::group_by(sexo, violencia_sexual)%>%
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
    filter(violencia_sexual == 1) %>% 
    mutate(tipo = "Violencia sexual") %>%
    rename(porcentaje = prop)

violencia_tipo <- bind_rows(vio_sex, vio_fis, vio_ps)
violencia_tipo <- select(violencia_tipo, tipo, sexo, porcentaje)

# gráfica tipos de violencia general
v_title     <- "Proporción de personas privadas de su libertad\nque vivieron violencia durante la detención"
v_subtitle  <- "Por tipo y sexo"

ggplot(violencia_tipo,
       aes(x = porcentaje, y = reorder(sexo, porcentaje), fill = sexo)) +
    geom_col() +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo),
               position = position_stack(1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    facet_wrap(~tipo, ncol = 1)+
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    tema +
    guides(fill = "none") +
    scale_fill_manual(name = "sexo", 
                      values=c("Hombres" = "#ff7841" , "Mujeres" = "#6fbae1"))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    theme(axis.text.x = element_text(angle = 0))

ggsave(
    file = paste0(out_figs, "violencia_tipo_sex", v_formato), 
    width = 6, height = 8)
add_intlogo(graf = "04_figuras/2021/post_tortura/violencia_tipo_sex.png", escala = 10)

# incidentes de violencia psicológica por sexo 
v_codes     <- c("p3_17_01", "p3_17_02", "p3_17_03", "p3_17_04", "p3_17_05",
                 "p3_17_06", "p3_17_07", "p3_17_08", "p3_17_09", "p3_17_11")
v_eventos   <- c("Amenaza con cargos falsos", "Amenaza con matarle", "Amenaza con hacerle daño",
                 "Amenaza con hacerle daño\na su familia", "Otro tipo de amenaza", "Le presionaron para\ndenunciar a alguien",
                 "Le incomunicaron/aislaron", "Le pasearon en un\nauto dando vueltas", "Hirieron a su familia",
                 "Le vendaron los\nojos/cubrieron su cabeza")

violencia_ps <- base                                                  %>%
    rename(respuesta = v_codes[1])                                      %>% 
    select(sexo, respuesta)                                             %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>% 
    #drop_na() %>% 
    srvyr::group_by(sexo, respuesta)                                    %>%
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
    rename(porcentaje = prop)                                           %>%
    mutate(evento = v_eventos[1])   

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- base                                                    %>% 
        rename(respuesta = v_codes[i])                                      %>%
        select(sexo, respuesta)                                             %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>% 
        #drop_na() %>% 
        srvyr::group_by(sexo, respuesta)                                    %>%
        srvyr::summarise(
            prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
        rename(porcentaje = prop)                                           %>%
        mutate(evento = v_eventos[i])
    
    violencia_ps <- violencia_ps %>% 
        bind_rows(df_data_loop) %>% 
        filter(respuesta == "Sí") 
    
}

# gráfica
v_title     <- "Proporción de personas privadas de su libertad\nque vivieron violencia psicológica durante la detención"
v_subtitle  <- "Por tipo de agresión y sexo"

ggplot(violencia_ps,
       aes(x = porcentaje, y = reorder(sexo, porcentaje), fill = sexo)) +
    geom_col() +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo),
               position = position_dodge(1), size=2, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    facet_wrap(~evento, ncol = 2)+
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    tema +
    guides(fill = "none") +
    scale_fill_manual(name = "sexo", 
                      values=c("Hombres" = "#ff7841" , "Mujeres" = "#6fbae1"))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    theme(axis.text.x = element_text(angle = 0))

ggsave(
    file = paste0(out_figs, "violencia_psic_sex", v_formato), 
    width = 6, height = 8)
add_intlogo(graf = "04_figuras/2021/post_tortura/violencia_psic_sex.png", escala = 10)

# incidentes de violencia física
v_codes     <- c("p3_18_01", "p3_18_02", "p3_18_03", "p3_18_04", "p3_18_05", "p3_18_06",
                 "p3_18_07", "p3_18_08", "p3_18_09", "p3_18_10", "p3_18_11", "p3_18_15")
v_eventos   <- c("Ataron su cuerpo", "Le asfixiaron", "Le impidieron\nrespirar/metieron\nsu cabeza en agua",
                 "Le patearon/golpearon\ncon las manos", "Le golpearon con objetos", "Le quemaron",
                 "Le dieron descargas\neléctricas", "Aplastaron su cuerpo", "Le hirieron con algún\nobjeto afilado",
                 "Le encajaron agujas", "Le hirieron por disparo\nde arma de fuego", "Otra agresión física")

violencia_fis <- base                                                 %>%
    rename(respuesta = v_codes[1])                                      %>% 
    select(sexo, respuesta)                                             %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>% 
    #drop_na() %>% 
    srvyr::group_by(sexo, respuesta)                                    %>%
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
    rename(porcentaje = prop)                                           %>%
    mutate(evento = v_eventos[1])   

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- base                                                  %>% 
        rename(respuesta = v_codes[i])                                      %>%
        select(sexo, respuesta)                                             %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>% 
        #drop_na() %>% 
        srvyr::group_by(sexo, respuesta)                                    %>%
        srvyr::summarise(
            prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))      %>%
        rename(porcentaje = prop)                                           %>%
        mutate(evento = v_eventos[i])
    
    violencia_fis <- violencia_fis %>% 
        bind_rows(df_data_loop) %>% 
        filter(respuesta == "Sí") 
    
}

# gráfica
v_title     <- "Proporción de personas privadas de su libertad\nque vivieron violencia física durante la detención"
v_subtitle  <- "Por tipo de agresión y sexo"

ggplot(violencia_fis,
       aes(x = porcentaje, y = reorder(sexo, porcentaje), fill = sexo)) +
    geom_col() +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo),
               position = position_dodge(1), size=2, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    facet_wrap(~evento, ncol = 2)+
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    tema +
    guides(fill = "none") +
    scale_fill_manual(name = "sexo", 
                      values=c("Hombres" = "#ff7841" , "Mujeres" = "#6fbae1"))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    theme(axis.text.x = element_text(angle = 0))

ggsave(
    file = paste0(out_figs, "violencia_fis_sex", v_formato), 
    width = 6, height = 8)
add_intlogo(graf = "04_figuras/2021/post_tortura/violencia_fis_sex.png", escala = 10)

# incidentes de violencia sexual
v_codes     <- c("p3_17_10", "p3_18_12", "p3_18_13", "p3_18_14")
v_eventos   <- c("Le desvistieron", "Le agredieron mediante\nacoso sexual, manoseo, exhibicionismo\no intento de violación", 
                 "Le lastimaron\nsus órganos sexuales\n(ano, testículos, pene o vagina) mediante golpes,\ndescargas eléctricas, aplastamiento,\ncortaduras o introducción de objetos",
                 "Fue obligado(a) mediante\nviolencia física o amenaza a\ntener una actividad sexual\nno deseada (violación sexual)")

violencia_sex <- base                                                    %>%
    rename(respuesta = v_codes[1])                                      %>% 
    select(sexo, respuesta)                                             %>%
    mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>% 
    #drop_na() %>% 
    srvyr::group_by(sexo, respuesta)                                    %>%
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
    rename(porcentaje = prop)                                           %>%
    mutate(evento = v_eventos[1])   

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- base                                                    %>% 
        rename(respuesta = v_codes[i])                                      %>%
        select(sexo, respuesta)                                             %>%
        mutate(respuesta = codificar_siono4(as.character(respuesta)))       %>% 
        #drop_na() %>% 
        srvyr::group_by(sexo, respuesta)                                    %>%
        srvyr::summarise(
            prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95))    %>%
        rename(porcentaje = prop)                                           %>%
        mutate(evento = v_eventos[i])
    
    violencia_sex <- violencia_sex %>% 
        bind_rows(df_data_loop) %>% 
        filter(respuesta == "Sí") 
    
}

# gráfica
v_title     <- "Proporción de personas privadas de su libertad\nque vivieron violencia sexual durante la detención"
v_subtitle  <- "Por tipo de agresión y sexo"

ggplot(violencia_sex,
       aes(x = porcentaje, y = reorder(sexo, porcentaje), fill = sexo)) +
    geom_col() +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = sexo),
               position = position_dodge(1), size=2, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    facet_wrap(~evento, ncol = 1)+
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    tema +
    guides(fill = "none") +
    scale_fill_manual(name = "sexo", 
                      values=c("Hombres" = "#ff7841" , "Mujeres" = "#6fbae1"))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(15)) +
    theme(axis.text.x = element_text(angle = 0))

ggsave(
    file = paste0(out_figs, "violencia_sexual_sex", v_formato), 
    width = 5, height = 8)
add_intlogo(graf = "04_figuras/2021/post_tortura/violencia_sexual_sex.png", escala = 10)

# tipo de violencia por autoridad y sexo
vio_aut <- base%>%
    mutate(aut_cort=case_when(autoridad == "Policía Municipal" ~ "Policías locales",
                              autoridad == "Policía Estatal" ~ "Policías locales",
                              autoridad == "Policía Federal" ~ "Policías federales",
                              autoridad == "Policía Estatal Ministerial" ~ "Policías locales",
                              autoridad == "Policía Federal Ministerial" ~ "Policías federales",
                              autoridad == "Guardia Nacional" ~ "Policías federales",
                              autoridad == "Ejército" ~ "Ejército",
                              autoridad == "Marina" ~ "Marina",
                              autoridad == "Operativo conjunto" ~ "Operativo conjunto",
                              autoridad == "Otra autoridad" ~ "Otra autoridad",
                              autoridad == "Otros" ~ "Otros"))%>%
    srvyr::group_by(aut_cort, violencia)%>%
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
    filter(violencia == 1)%>%
    rename(porcentaje = prop)

#gráfica 
v_title     <- "Proporción de personas de su libertad que vivieron algún tipo de violencia\ndurante la detención"
v_subtitle  <- "Por tipo de autoridad"

ggplot(vio_aut,
       aes(x = porcentaje, y = reorder(aut_cort, porcentaje), fill = aut_cort)) +
    geom_col() +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = aut_cort),
               position = position_dodge(1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    theme( axis.text.y      = element_text(size = 5, family="Avenir Next Condensed", angle=0, hjust=.5),)+
    tema +
    guides(fill = "none") +
    scale_fill_manual(name = "auto_cort", 
                      values=c("Marina" = "#679f6e" , "Operativo conjunto" = "#6baed6",
                               "Ejército" = "#fec44f", "Policías federales" = "#9e9ac8",
                               "Policías locales" = "#f768a1", "Otra autoridad" = "#969696"))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    theme(axis.text.x = element_text(angle = 0))
ggsave(
    file = paste0(out_figs, "violencia_gen_aut", v_formato), 
    width = 7, height = 6)
add_intlogo(graf = "04_figuras/2021/post_tortura/violencia_gen_aut.png", escala = 12)

# violencia perpetrada a población inculpada por autoridad
inculpa <- base                         %>%
    mutate(aut_cort=case_when(autoridad == "Policía Municipal" ~ "Policías locales",
                              autoridad == "Policía Estatal" ~ "Policías locales",
                              autoridad == "Policía Federal" ~ "Policías federales",
                              autoridad == "Policía Estatal Ministerial" ~ "Policías locales",
                              autoridad == "Policía Federal Ministerial" ~ "Policías federales",
                              autoridad == "Guardia Nacional" ~ "Policías federales",
                              autoridad == "Ejército" ~ "Ejército",
                              autoridad == "Marina" ~ "Marina",
                              autoridad == "Operativo conjunto" ~ "Operativo conjunto",
                              autoridad == "Otra autoridad" ~ "Otra autoridad",
                              autoridad == "Otros" ~ "Otros"))%>%
    filter(p3_1 == 4)                   %>%
    srvyr::group_by(aut_cort, violencia)%>%
    srvyr::summarise(
        prop = survey_mean(na.rm = T, vartype = "ci", level = 0.95)) %>% 
    filter(violencia == 1)%>%
    rename(porcentaje = prop)

# gráfica 
v_title     <- "Proporción de personas privadas de su libertad\nque indican fueron inculpadas y vivieron algún incidente\nde violencia al momento de su detención"
v_subtitle  <- "Por tipo de autoridad"

ggplot(inculpa,
       aes(x = porcentaje, y = reorder(aut_cort, porcentaje), fill = aut_cort)) +
    geom_col() +
    geom_label(aes(label=paste0(round(porcentaje,3)*100, "%"), group = aut_cort),
               position = position_dodge(1), size=2.5, hjust=.5, vjust=.5, angle = 0, fill = "white",
               color="black",  family = "Fira Sans") +
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    theme( axis.text.y      = element_text(size = 5, family="Avenir Next Condensed", angle=0, hjust=.5),)+
    tema +
    guides(fill = "none") +
    scale_fill_manual(name = "auto_cort", 
                      values=c("Marina" = "#679f6e" , "Operativo conjunto" = "#6baed6",
                               "Ejército" = "#fec44f", "Policías federales" = "#9e9ac8",
                               "Policías locales" = "#f768a1", "Otra autoridad" = "#969696"))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    theme(axis.text.x = element_text(angle = 0))

ggsave(
    file = paste0(out_figs, "inculpa_vio_aut", v_formato), 
    width = 7, height = 6)
add_intlogo(graf = "04_figuras/2021/post_tortura/inculpa_vio_aut.png", escala = 12)

