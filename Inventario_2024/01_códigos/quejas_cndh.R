#------------------------------------------------------------------------------#
# Proyecto:                   Fuerzas armadas
# Objetivo:                   Procesar datos de la CNDH
#
# Encargada:                  Fernanda Torres
# Correo:                     ftorres@intersecta.org
# Fecha de creación:          15 de abril de 2024
# Última actualización:       15 de abril de 2024
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, dplyr,janitor, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones con direcciones de las carpetas
paste_inp       <- function(x){paste0("01_datos/"                      , x)}
paste_fig       <- function(x){paste0("03_figuras/saque_final/04_cndh/", x)}

# 1. Cargar datos --------------------------------------------------------------

load(paste_inp("df_quejas_cndh_1990_2023.RData"))

# 2. Procesar datos ------------------------------------------------------------

# Eliminar expedientes repetidos 
df_expedientes <- df_quejas_cndh_1990_2023[!duplicated(df_quejas_cndh_1990_2023$expediente), ]

# 3. Visualizar ----------------------------------------------------------------

## 3.0. Formato de figuras -----------------------------------------------------

# ---- Tema 

### 3.0.1. Tema ----------------------------------------------------------------

tema <-  theme_light() +
    theme(
        panel.background = element_rect(fill = "#fdf2df"),
        plot.background = element_rect(fill="#fdf2df", color = "#fdf2df"),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        text                  = element_text(family = "Henriette", color = "black"),
        plot.title            = element_text(family = "Henriette", color = "black", 
                                             size = 14,  face  = "bold",  margin = margin(5,5,5,5), hjust = 0),
        plot.subtitle         = element_text(family = "Henriette", face = "italic", color = "black", 
                                             size = 13,  margin = margin(5, 5, 10, 5), hjust = 0),
        plot.caption          = element_text(family = "Henriette", color = "#2a2b29", 
                                             face = "italic", size = 11,  hjust = 0),
        # panel.grid            = element_line(linetype = 1, color = "#7A7E76"),
        panel.grid.major.y    = element_blank(),
        panel.grid.major.x    = element_blank(),
        panel.grid.minor.y    = element_blank(),
        panel.grid.minor.x    = element_blank(),
        plot.margin           = margin(0, 0, 0, 0, "cm"),
        axis.line             = element_line(linewidth = .3, colour = "#2a2b29"),
        # axis.ticks.y          = element_blank(),
        panel.border          = element_blank(),
        panel.spacing         = unit(.8, "cm"),
        legend.background     = element_rect(fill = "#fdf2df"),
        legend.position       = "top",
        legend.title          = element_text(size = 10, family = "Henriette", face  = "bold", color = "black"),
        legend.text           = element_text(size = 10, family = "Henriette", color = "black"),
        axis.title            = element_text(size = 10, family = "Henriette", hjust = .5, margin = margin(1,1,1,1), color = "black"),
        axis.text.y           = element_text(size = 10, family = "Henriette", face ="italic", angle=0,  hjust=1, color = "black"),
        axis.text.x           = element_text(size = 10, family = "Henriette", face ="italic", angle=0, hjust=.5, vjust = 1, color = "black"),
        strip.text.x          = element_text(size = 9, family = "Henriette", face = "bold", color = "black"),
        strip.text.y          = element_text(size = 9, family = "Henriette", face = "bold", color = "black"),
        strip.background      = element_rect(fill = "#fdf2df", color = "#fdf2df"),
    )

# ---- Colores

v_colores <- c("#fdf2df", "#647f2e", "#000000", "#2a2b29")
v_colores_fig <- c("#647f2e", "#2a2b29")
v_colores_figs <- c("#647f2e", "#2a2b29", "#959A90", "#2A450C")

# ---- Vectores de texto

v_formato <- ".png"
v_caption <- "
Fuente: Solicitud de Acceso a la Información (SAI) con folio 330030924000223.
Datos procesados por Intersecta (intersecta.org)"

## 3.1. Quejas totales por año -------------------------------------------------

# ---- Procesar datos 
df_data <- df_expedientes    %>% 
    group_by(year_registro)    %>% 
    summarise(total = n())

ggplot(
    # Datos 
    df_data, 
    # Coordenadas
    aes(x = year_registro, y = total)) +
    # Geoms
    geom_line() +
    geom_point() +
    # Etiquetas
    labs( 
        title = "Quejas recibidas por la Comisión Nacional de Derechos Humanos", 
        subtitle = "Por año\n", 
        x = "", 
        y = "Total de quejas recibidas\n", 
        caption = v_caption) +
    # Escalas
    scale_x_continuous(breaks = c(1990:2023)) +
    scale_y_continuous(label = scales::comma_format()) +
    # Tema
    tema +
    theme(axis.text.x = element_text(angle = 90))

# ---- Guardar visualización 
ggsave(paste_fig(paste0("01_", "quejas_total", v_formato)),
       width = 8, height = 6)

## 2.2. Quejas totales por sexo y por año --------------------------------------

# ---- Procesar datos 
df_data <- df_expedientes           %>% 
    group_by(year_registro, sexo)     %>% 
    summarise(total = n()) 

# ---- Gráfica
ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = year_registro, y = total, color = sexo)) +
    # Geoms
    geom_line() +
    geom_point(show.legend = F) +
    # Etiquetas
    labs( 
        title = "Quejas recibidas por la Comisión Nacional de Derechos Humanos", 
        subtitle = "Por año y por sexo de la persona quejosa\n", 
        x = "", 
        y = "Total de quejas recibidas\n", 
        color = "Sexo: ",
        caption = v_caption) +  # Escalas
    # Escalas
    scale_x_continuous(breaks = c(1990:2023)) +
    scale_y_continuous(label = scales::comma_format()) +
    # Tema
    tema +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_color_manual(values = v_colores_fig)

# ---- Guardar visualización 
ggsave(paste_fig(paste0("02_", "quejas_sexo_total", v_formato)),
       width = 8, height = 6)

## 2.3. Quejas totales para SEDENA, SEMAR PF/GN --------------------------------

# ---- Procesar datos 
df_data <- df_expedientes                         %>% 
    group_by(year_registro, autoridades_intr)     %>% 
    summarise(total = n())                        %>% 
    filter(autoridades_intr %in% c("SEDENA", "SEMAR", "PF/GN")) %>% 
    mutate(autoridades_intr = recode(autoridades_intr,
                                     "SEDENA" = "Sedena",
                                     "SEMAR" = "Semar",
                                     "PF/GN" = "Policía Federal / Guardia Nacional"))
# ---- Gráfica de líneas
ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = year_registro, y = total, color = autoridades_intr)) +
    # Geoms
    geom_line()  +
    geom_point() +
    # Etiquetas
    labs( 
        title = "Quejas recibidas por la Comisión Nacional de Derechos Humanos", 
        subtitle = "Por año y por autoridad\n", 
        x = "", 
        y = "Total de quejas recibidas\n", 
        color = "Autoridad: ",
        caption = v_caption) +  # Escalas
    # Escalas
    scale_x_continuous(breaks = c(1990:2023)) +
    scale_y_continuous(label = scales::comma_format()) +
    # Tema 
    tema +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_color_manual(values = v_colores_figs)

# ---- Guardar visualización 
ggsave(paste_fig(paste0("03_", "quejas_autoridad_total", v_formato)),
       width = 8, height = 6)

# ---- Gráfica stacked bars 
ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = year_registro, y = total, fill = autoridades_intr)) +
    # Geoms
    geom_col(position = position_stack())  +
    # geom_label(aes(label = scales::comma(total), group = autoridades_intr,
    #                y = total),
    #            position = position_stack(),
    #            size = 2.5,
    #            family = "Henriette",
    #            color = "black",
    #            fill  = "white",
    #            vjust = 0.5,
    #            hjust = 0.5,
    #            angle = 0) +
    # Etiquetas
labs( 
    title = "Quejas recibidas por la Comisión Nacional de Derechos Humanos", 
    subtitle = "Por año y por autoridad\n", 
    x = "", 
    y = "Total de quejas recibidas\n", 
    fill = "Autoridad: ",
    caption = v_caption) +  # Escalas
    # Escalas
    scale_x_continuous(breaks = c(1996:2023),
                       expand = c(0.01,0)) +
    scale_y_continuous(label = scales::comma_format(),
                       expand = c(0,0)) +
    # Tema 
    tema +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values = v_colores_figs)

# ---- Guardar visualización 
ggsave(paste_fig(paste0("03_", "quejas_autoridad_total_stack", v_formato)),
       width = 8, height = 6)

## 2.4. Proporción del total por año -------------------------------------------

## 2.5. Proporción del total por año y por sexo --------------------------------

# ---- Procesar datos 
df_data <- df_expedientes                             %>% 
    group_by(year_registro, autoridades_intr, sexo)     %>% 
    summarise(total = n())                              %>% 
    filter(autoridades_intr %in% c("SEDENA", "SEMAR", "PF/GN"))

# ---- Gráfica
ggplot(
    # Datos 
    df_data,
    # Coordenadas
    aes(x = year_registro, y = total, color = autoridades_intr)) +
    facet_wrap(~sexo, scales = "free_y") +
    # Geoms 
    geom_line()  +
    geom_point() +
    # Etiquetas
    labs( 
        title = "Quejas recibidas por la Comisión Nacional de Derechos Humanos", 
        subtitle = "Por año, por sexo de la persona quejosa y por autoridad\n", 
        x = "", 
        y = "Total de quejas recibidas\n", 
        color = "Autoridad: ",
        caption = v_caption) +  # Escalas
    # Escalas
    scale_x_continuous(breaks = c(1990:2023)) +
    scale_y_continuous(label = scales::comma_format()) +
    # Tema 
    tema +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_color_manual(values = v_colores_figs)

# ---- Guardar visualización 
ggsave(paste_fig(paste0("04_", "quejas_autoridad_sexo_total", v_formato)),
       width = 8, height = 6)

## 2.6. Quejas por terminación y por año ---------------------------------------

# ---- Procesar datos 
df_data <- df_expedientes                           %>% 
    group_by(   
        year_registro, motivos_de_conclusion)           %>% 
    summarise(total = n())                   

# ---- Gráfica
ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = year_registro, y = total, color = motivos_de_conclusion)) +
    # Geoms
    geom_line() +
    geom_point() +
    # Etiquetas
    labs( 
        title = "Quejas recibidas por la Comisión Nacional de Derechos Humanos", 
        subtitle = "Por año y por motivo de conclusión\n", 
        x = "", 
        y = "Total de quejas recibidas\n", 
        color = "Motivo de conclusión: ",
        caption = v_caption) +  # Escalas
    # Escalas
    scale_x_continuous(breaks = c(1990:2023)) +
    scale_y_continuous(label = scales::comma_format()) +
    # Tema 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))

# ---- Guardar visualización 
ggsave(paste_fig(paste0("06_", "quejas_terminación_total", v_formato)),
       width = 8, height = 6)

## 2.7. Quejas por terminación, por año y por sexo -----------------------------

# ---- Procesar datos 
df_data <- df_expedientes                       %>% 
    group_by(
        year_registro, motivos_de_conclusion, sexo) %>% 
    summarise(total = n())                   

# ---- Gráfica
ggplot(
    # Datos
    df_data,
    # Coordenadas
    aes(x = year_registro, y = total, color = motivos_de_conclusion)) +
    facet_wrap(~sexo) +
    # Geoms
    geom_line() +
    geom_point() +
    # Etiquetas
    labs( 
        title = "Quejas recibidas por la Comisión Nacional de Derechos Humanos", 
        subtitle = "Por año, sexo de la persona quejosa y motivo de conclusión\n", 
        x = "", 
        y = "Total de quejas recibidas\n", 
        color = "Motivo de conclusión: ",
        caption = v_caption) +  # Escalas
    # Escalas
    scale_x_continuous(breaks = c(1990:2023)) +
    scale_y_continuous(label = scales::comma_format()) +
    # Tena
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))

# ---- Guardar visualización 
ggsave(paste_fig(paste0("07_", "quejas_terminación_sexo_total", v_formato)),
       width = 8, height = 6)


## 2.7. Recomendaciones FF AA por año ------------------------------------------

df_data <- df_expedientes                                    %>% 
    filter(autoridades_intr %in% c("SEDENA", "SEMAR", "PF/GN"),
           str_detect(motivos_de_conclusion, "RECOMENDACION")) %>% 
    group_by(year_registro, autoridades_intr)                  %>% 
    summarise(total = n())

# ---- Gráfica de líneas
ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = year_registro, y = total, color = autoridades_intr)) +
    # Geoms
    geom_line() +
    geom_point() +
    # Etiquetas
    labs( 
        title = "Recomendaciones de la Comisión Nacional de los Derechos Humanos a las fuerzas federales de seguridad", 
        subtitle = "Por año\n", 
        x = "", 
        y = "Total de quejas recibidas\n", 
        color = "Motivo de conclusión: ",
        caption = v_caption) +  # Escalas
    # Escalas
    scale_x_continuous(breaks = c(1997:2023)) +
    scale_y_continuous(label = scales::comma_format()) +
    # Tema 
    tema +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_color_manual(values = v_colores_figs)

# ---- Guardar visualización 
ggsave(paste_fig(paste0("06_", "recomendaciones_ffaa", v_formato)),
       width = 8, height = 6)

# ---- Gráfica stacked bars 
ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = year_registro, y = total, fill = autoridades_intr)) +
    # Geoms
    geom_col(position = position_stack())  +
    geom_label(aes(label = scales::comma(total), group = autoridades_intr,
                   y = total),
               position = position_stack(),
               size = 2.5,
               family = "Henriette",
               color = "black",
               fill  = "white",
               vjust = 0.5,
               hjust = 0.5,
               angle = 0) +
    # Etiquetas
    labs( 
        title = "Recomendaciones de la Comisión Nacional de los Derechos Humanos a las fuerzas federales de seguridad", 
        subtitle = "Por año y autoridad\n", 
        x = "", 
        y = "Total de quejas recibidas\n", 
        fill = "Autoridad: ",
        caption = v_caption) +  # Escalas
    # Escalas
    scale_x_continuous(breaks = c(1996:2023)) +
    scale_y_continuous(label = scales::comma_format()) +
    # Tema 
    tema +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values = v_colores_figs)

# ---- Guardar visualización 
ggsave(paste_fig(paste0("06_", "recomendaciones_ffaa_stack", v_formato)),
       width = 8, height = 6)

# FIN --------------------------------------------------------------------------

