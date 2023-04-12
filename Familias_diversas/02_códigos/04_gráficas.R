#------------------------------------------------------------------------------#
# Proyecto:                   Censos poblacionales del INEGI
# Objetivo:                   Construir gráficos
#
# Encargadas:                 Fernanda Torres
# Correo:                     fernanda.torres@alumnos.cide.edu
# Fecha de creación:          14 de julio de 2022
# Última actualización:       14 de julio de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(srvyr, tidyverse, dplyr, lubridate, scales, beepr,
       data.table, readr, magick, add2ggplot)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones para directorios dentro del proyecto 
paste_data  <- function(x){paste0("01_datos/"                        , x)}
paste_code  <- function(x){paste0("02_códigos/"                      , x)}
paste_figs  <- function(x){paste0("03_figuras/"                      , x)}

# 1. Cargar datos --------------------------------------------------------------

## 1.1 Censo de Población y Vivienda 2020 --------------------------------------
load(paste_data("df_familias_tipos_nacional.RData"))

## 1.2 Encuesta Intercensal 2015 -----------------------------------------------
load(paste_data("df_familias_tipos_nacional_i.RData"))


# 2. Funciones -----------------------------------------------------------------

# 2.1. Función para codificar tipo de hogar ------------------------------------

codificar_hogar <- function(var = x){
    
    v_hogar <- c("Compañeres de casa \nsin hijes", "Compañeres de casa \ncon hijes",
                 "Pareja de dos hombres \ncasados y sin hijes",
                 "Pareja de dos hombres \nen unión libre y sin hijes",
                 "Pareja de dos hombres \ncasados y con hijes",
                 "Pareja de dos hombres \nen unión libre y con hijes",
                 "Hombre solo sin hijes", "Hombre solo con hijes",
                 "Pareja de un hombre y una mujer \ncasades y sin hijes",
                 "Pareja de un hombre y una mujer  \nen unión libre y sin hijes",
                 "Pareja de un hombre y una mujer  \ncasades y con hijes",
                 "Pareja de un hombre y una mujer  \nen unión libre y con hijes",
                 "Pareja de dos mujeres \ncasadas y sin hijes",
                 "Pareja de dos mujeres \nen unión libre y sin hijes",
                 "Pareja de dos mujeres \ncasadas y con hijes",
                 "Pareja de dos mujeres \nunión libre y con hijes",
                 "Mujer sola sin hijes", "Mujer sola con hijes",
                 "No monógama sin hijes", "No monógama con hijes",
                 "Parientes sin hijes", "Parientes con hijes")
    
    case_when(
        var == "1" ~ v_hogar[1], 
        var == "2" ~ v_hogar[2], 
        var == "3" ~ v_hogar[3], 
        var == "4" ~ v_hogar[4], 
        var == "5" ~ v_hogar[5], 
        var == "6" ~ v_hogar[6], 
        var == "7" ~ v_hogar[7], 
        var == "8" ~ v_hogar[8], 
        var == "9" ~ v_hogar[9], 
        var == "10" ~ v_hogar[10], 
        var == "11" ~ v_hogar[11], 
        var == "12" ~ v_hogar[12], 
        var == "13" ~ v_hogar[13], 
        var == "14" ~ v_hogar[14], 
        var == "15" ~ v_hogar[15], 
        var == "16" ~ v_hogar[16], 
        var == "17" ~ v_hogar[17], 
        var == "18" ~ v_hogar[18], 
        var == "19" ~ v_hogar[19], 
        var == "20" ~ v_hogar[20], 
        var == "21" ~ v_hogar[21], 
        var == "22" ~ v_hogar[22])
}


# 2.3. Añadir logo -------------------------------------------------------------

add_intlogo <- function(graf, escala){
    graf_con_logo <- add_logo(
        plot_path = graf,
        logo_path = "logo/corto_blanco.png",
        logo_position = "bottom right",
        logo_scale = escala)
    
    magick::image_write(graf_con_logo, graf)
}

# 3. Creación de la base -------------------------------------------------------

## 3.1. Renombrar columnas -----------------------------------------------------

df_familias_tipos_nacional <- df_familias_tipos_nacional %>% 
    rename(`Total 2020` = total,
           `Porcejante 2020` = porcentaje)

df_familias_tipos_nacional_i <- df_familias_tipos_nacional_i %>% 
    rename(`Total 2015` = total,
           `Porcejante 2015` = porcentaje)

## 3.2. Juntar bases de datos --------------------------------------------------

df_familias_tiempo <- df_familias_tipos_nacional %>% 
    left_join(df_familias_tipos_nacional_i)      %>% 
    filter(!is.na(`Porcejante 2015`))

# 3.3. Computar cambio en el porcentaje -----------------------------------

df_familias_cambio <- df_familias_tiempo %>% 
    # Calcular cambio
    mutate(`Cambio en porcentaje` = `Porcejante 2020` - `Porcejante 2015`) %>% 
    # Ordenar
    arrange(jefatura, hijxs, `Situación conyugal`) %>% 
    # Crear id por tipo 
    mutate(tipo = row_number()) %>% 
    # Aplicar nombre al tipo 
    mutate(Hogar = codificar_hogar(tipo)) %>% 
    filter(jefatura != "No monógama")

# 4. Gráficas ------------------------------------------------------------------

## 4.1. Tema -------------------------------------------------------------------

# Tema para gráficas 
tema <-  theme_linedraw() +
    theme(
        text             = element_text(family = "Avenir Next Condensed", color = "black"),
        plot.title       = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(10,5,5,5), family="Avenir Next Condensed", color = "black"),
        plot.subtitle    = element_text(size = 13, color = "#666666", hjust = 0.5, margin = margin(5, 5, 5, 5), family="Avenir Next Condensed"),
        plot.caption     = element_text(hjust = 0.5, size = 8, family = "Avenir Next Condensed", color = "black"),
        plot.margin      = margin(0.5, 0.5, 0.8, 0.5, "cm"),
        panel.grid       = element_line(linetype = 5, color = rgb(0, 0, 0, 100, maxColorValue = 255)), 
        legend.position  = "top",
        panel.grid.minor = element_blank(),
        legend.title     = element_text(size = 10, face = "bold", family="Avenir Next Condensed"),
        legend.text      = element_text(size = 10, family="Avenir Next Condensed"),
        axis.title       = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Avenir Next Condensed"),
        axis.text.y      = element_text(size = 8, family="Avenir Next Condensed", angle=0, hjust=.5),
        axis.text.x      = element_text(size = 8, family="Avenir Next Condensed", angle=0, hjust=.5),
        strip.background = element_rect(fill="white", colour = NA),
        strip.text.x     = element_text(size=9, family = "Avenir Next Condensed", face = "bold", color = "black"),
        strip.text.y     = element_text(size=9, family = "Avenir Next Condensed", face = "bold", color = "black"))

# Escala de colores
v_colors <- c("#96D4CA", "#8F92CE")

# Formato para gráficas
v_formato <- ".png"

# Etiquetas
v_caption  <- "Fuente: Censo de Población y Vivienda 2020 y Encuesta Intercensal 2015. \nDatos procesados por Intersecta.org"
v_percent  <- "\nPuntos porcentuales"
v_empty    <- ""
v_title    <- "Cambio en el porcentaje \nde los tipos de hogares en México"
v_subtitle <- "(2015 - 2020)"

## 4.2. Cambio en el porcentaje ------------------------------------------------

df_data <- df_familias_cambio %>% 
    # Agrupar acorde al cambio 
    mutate(grupo = case_when(
        `Cambio en porcentaje` > 0 ~ "Aumentó",
        `Cambio en porcentaje` < 0 ~ "Descendió"
    ))

ggplot(df_data, aes(x = `Cambio en porcentaje`, y = reorder(Hogar,`Cambio en porcentaje`),  fill = grupo)) +
    # Columnas 
    geom_col(width = 0.8, position = position_dodge(10)) +
    # Texto 
    geom_label(aes(label=round(`Cambio en porcentaje`,3)),
               size=2.5,
               vjust=.5,
               fill = "white",
               color="black",  
               family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_percent, 
        y        = v_empty,
        caption  = v_caption) +
    # Tema
    tema +
    scale_fill_manual(values = v_colors) +
    theme(legend.position = "none") + 
    scale_x_continuous(limits = c(-5, 2))

ggsave(
    file = paste0(paste_figs("cambio_porcentaje"), v_formato), 
    width = 6, height = 8)

add_intlogo(graf = paste0(paste_figs("cambio_porcentaje"), v_formato), escala = 10)
