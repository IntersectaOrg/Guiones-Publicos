#------------------------------------------------------------------------------#
# Proyecto:                   Inventario Nacional de lo Militarizado
# Objetivo:                   Figuras metadatos (versión final)
#
# Encargadas:                 Fernanda Torres
# Correo:                     ftorres@intersecta.org
# 
# Fecha de creación:          09 de febrero de 2024
# Última actualización:       04 de abril   de 2024
#------------------------------------------------------------------------------#

# Fuente: https://docs.google.com/spreadsheets/d/1AxY94pAlCcuEja5rU0XyT4MLyRvbPHeo/edit?rtpof=true

# 0. Configuración inicial -----------------------------------------------------

# ---- Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# ---- Cargar librerías 
require(pacman)
p_load(
    googledrive, googlesheets4, scales,
    readxl, tidyverse, beepr, extrafont, janitor, lubridate)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Desactivar notación científica
options(scipen=999)

# ---- Establecer directorios
paste_in <- function(x){paste0("01_datos/"                              , x)}
paste_fig <- function(x){paste0("03_figuras/saque_final/01_solicitudes/", x)}

# ---- Activar las credenciales de google
# v_usuaria <- "aortega"
# v_usuaria <- "evela"
v_usuaria <- "ftorres"

googledrive::drive_auth(paste0(v_usuaria, "@intersecta.org"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@intersecta.org"))

# ---- Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# 1. Datos ---------------------------------------------------------------------

# 1.1. Cargar base de datos ----------------------------------------------------

# Path 
temp  <- tempfile(fileext = ".xlsx")

# Descargar base de datos en construcción
dl <- drive_download("https://docs.google.com/spreadsheets/d/1AxY94pAlCcuEja5rU0XyT4MLyRvbPHeo/edit?rtpof=true.xlsx",
                     path = temp, overwrite = TRUE)

# Cargar a R
df_raw <- read_excel(dl$local_path,
                     sheet = 2, 
                     # col_types = "text"
)

# 2. Procesamiento -------------------------------------------------------------

## 2.0. Funciones --------------------------------------------------------------

### 2.0.1. Wrapper -------------------------------------------------------------

wrapper <- function(x, ...) {
    paste(strwrap(x, ...), collapse = "\n")
}

## 2.1. Explorar base ----------------------------------------------------------

# El periodo de tiempo por el que se preguntó está en la misma columna
# Separar por año de inicio y año de fin 
# Versión abril de 2024

## 2.2. Recodificar ------------------------------------------------------------

### 2.2.1. Por año -------------------------------------------------------------

df_limpia_a <- df_raw  %>% 
    mutate(`Quién` = recode(`Quién`,
                            "Queretaro" = "Querétaro")) %>% 
    mutate(`Quién` = recode(`Quién`,
                            "Secretaría de Desarrollo Social/ Secretaría de bienestar" = "Secretaría de Desarrollo Social/ Secretaría de Bienestar",
                            "Secretaría de Desarrollo Social/ Bienestar" = "Secretaría de Desarrollo Social/ Secretaría de Bienestar",
                            "Caminos y puentes federales" = "Caminos y Puentes Federales",
                            "Secretaría de Medio Ambiente" = "Secretaría de Medio Ambiente y Recursos Naturales"))
    

### 2.2.2. Por vigencia --------------------------------------------------------

df_limpia <- df_raw                                               %>% 
    group_by(`Periodo de tiempo`, Folio, Año, `Quién`, 
             `Qué`, `Hubo respuesta`, 
             `Tipo de respuesta`)  %>% 
    mutate(periodo_control = `Periodo de tiempo`)                 %>% 
    # Separar en dos columnas periodo de tiempo 
    separate(col = `Periodo de tiempo`, 
             into = c("inicio", "fin"),
             sep = "-")                                    %>% 
    mutate(inicio = as.numeric(inicio), 
           fin = as.numeric(fin))                                 %>% 
    ungroup()                                                     %>% 
    # Expandir por año 
    mutate(n_times = fin - inicio + 1)                            %>% 
    slice(rep(row_number(), n_times))                             %>%  
    ungroup()                                                     %>% 
    group_by(inicio, fin, Folio, Año, `Quién`, `Qué`, `Hubo respuesta`, `Tipo de respuesta`)          %>% 
    mutate(year = seq(from = inicio[1], # Fill years column from a to b in each group
                      to = fin[1]))                               %>% 
    ungroup()                                                     %>% 
    select(-inicio, -fin)                                         %>% 
    # Limpieza de nombres
    mutate(`Quién` = recode(`Quién`,
                            "Queretaro" = "Querétaro")) %>% 
    mutate(`Quién` = recode(`Quién`,
                            "Secretaría de Desarrollo Social/ Secretaría de bienestar" = "Secretaría de Desarrollo Social/ Secretaría de Bienestar",
                            "Secretaría de Desarrollo Social/ Bienestar" = "Secretaría de Desarrollo Social/ Secretaría de Bienestar",
                            "Caminos y puentes federales" = "Caminos y Puentes Federales",
                            "Secretaría de Medio Ambiente" = "Secretaría de Medio Ambiente y Recursos Naturales")) 
    

# 3. Configuración de figuras --------------------------------------------------

### 3.1. Tema ------------------------------------------------------------------


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

### 3.2. Colores ---------------------------------------------------------------

v_colores      <- c("#fdf2df", "#647f2e", "#000000", "#2a2b29")
v_colores_fig  <- c("#647f2e", "#2a2b29")
v_colores_figs <- c("#647f2e", "#2a2b29", "#959A90", "#2A450C")

### 3.3. Vectores de texto -----------------------------------------------------

v_caption  <- "Fuente: solicitudes de información realizadas para el Inventario Nacional de lo Militarizado.\n"
# path_drive <- as_id("1NJppYu5n3QRy4EmbKPE_mNpxME1K5AzI") # En mi unidad
path_drive <- as_id("1H5pWKd-HaMyEZnhL8Xuetoha7qvkhrQd") # En unidad compartida INTR

# 4. Figuras -------------------------------------------------------------------

## 4.1. Solicitudes por tipo de respuesta --------------------------------------

# ---- Datos

df_data <- df_limpia_a                                           %>% 
    group_by(`Tipo de respuesta`) %>% 
    count()                                                      %>% 
    ungroup()                                                    %>% 
    mutate(porc = n/sum(n))

# ---- Labels 
v_title    <- "Respuestas de las autoridades a las solicitudes de acceso a la información pública\n"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(y = reorder(`Tipo de respuesta`, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = paste0(n, " (", scales::percent(round(porc, digits = 3)), ")"),
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = .5,
              hjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 85)) +
    scale_x_continuous(limits = c(0, 170),
                       expand = c(0,0),
                       breaks = seq(0, 200, 20)) +
    scale_y_discrete(labels = wrap_format(35)) +
    tema

# ---- Guardar figura 

# Local
plot_name  <- "respuesta.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.2. Solicitudes por tipo de respuesta por año ------------------------------

# ---- Datos

df_data <- df_limpia_a                                           %>%
    mutate(respuesta = ifelse(`Tipo de respuesta` == "Algún tipo de información",
                              "Algún tipo de información", 
                              "Sin información"))                %>% 
    group_by(Año, respuesta)                                     %>% 
    count()                                                      %>% 
    ungroup()                                                    %>% 
    group_by(Año)                                                %>% 
    mutate(porc = n/sum(n))

# ---- Labels 
v_title    <- "¿Qué respuestas dieron las autoridades a las solicitudes de acceso a la información pública?"
v_subtitle <- "Por año de solicitud\n"

# ---- Figura 
ggplot(df_data, aes(y = n, x = Año, fill = respuesta)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = respuesta,
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               size = 2.5,
               vjust = 0.5,
               hjust = .5,
               fill = "white",
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 178),
                       expand = c(0,0),
                       breaks = seq(0, 200, 20)) +
    scale_x_continuous(breaks = seq(2020, 2023)) +
    tema +
    scale_fill_manual(values = v_colores_fig)

# ---- Guardar figura 

# Local
plot_name  <- "respuesta_año.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.3. Solicitudes por tipo de respuesta ffaa ---------------------------------

# ---- Datos

df_data <- df_limpia_a                                           %>%
    mutate(respuesta = ifelse(`Tipo de respuesta` == "Algún tipo de información",
                              "Algún tipo de información", 
                              "Sin información"))                %>% 
    filter(`Quién` %in% c("Secretaría de la Defensa Nacional", 
                          "Secretaría de Marina",
                          "Guardia Nacional"))                   %>% 
    group_by(Año, respuesta)                                     %>% 
    count()                                                      %>% 
    ungroup()                                                    %>% 
    group_by(Año)                                                %>% 
    mutate(porc = n/sum(n))

# ---- Labels 
v_title    <- "Tipo de respuesta a las SAI de las Fuerzas Armadas"
v_subtitle <- "Por año de solicitud\n"

# ---- Figura 
ggplot(df_data, aes(y = n, x = Año, fill = respuesta)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = respuesta,
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               size = 2.5,
               vjust = 0.5,
               hjust = .5,
               fill = "white",
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 30),
                       expand = c(0,0),
                       breaks = seq(0, 40, 10)) +
    scale_x_continuous(breaks = seq(2020, 2023)) +
    tema +
    scale_fill_manual(values = v_colores_fig)

# ---- Guardar figura 

# Local
plot_name  <- "respuesta_año_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.3. Solicitudes por tipo de respuesta ffaa ---------------------------------

# ---- Datos

df_data <- df_limpia_a                                           %>%
    mutate(respuesta = ifelse(`Tipo de respuesta` == "Algún tipo de información",
                              "Algún tipo de información", 
                              "Sin información"))                %>% 
    filter(`Quién` %in% c("Secretaría de la Defensa Nacional", 
                          "Secretaría de Marina",
                          "Guardia Nacional"))                   %>% 
    group_by(Año, `Quién`, respuesta)                            %>% 
    count()                                                      %>% 
    ungroup()                                                    %>% 
    group_by(Año, `Quién`)                                       %>% 
    mutate(porc = n/sum(n))                                      %>% 
    mutate(`Quién` = recode(`Quién`,
                            "Secretaría de la Defensa Nacional" = "Sedena", 
                            "Secretaría de Marina" = "Semar",
                            "Guardia Nacional" = "GN"))          %>% 
    mutate(`Quién` = factor(`Quién`, levels = c(
        "Sedena", 
        "Semar",
        "GN"
    )))

# ---- Labels 
v_title    <- "Tipo de respuesta a las SAI de las Fuerzas Armadas"
v_subtitle <- "Por año de solicitud\n"

# ---- Figura 
ggplot(df_data, aes(y = n, x = Año, fill = respuesta)) +
    geom_col(position = position_stack()) +
    facet_wrap(~`Quién`) +
    geom_label(aes(label = n, group = respuesta,
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               size = 3,
               vjust = 0.5,
               hjust = .5,
               fill = "white",
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 10.7),
                       expand = c(0,0),
                       breaks = seq(0, 10, 1)) +
    scale_x_continuous(breaks = seq(2020, 2023)) +
    tema +
    scale_fill_manual(values = v_colores_fig)

# ---- Guardar figura 

# Local
plot_name  <- "respuesta_año_ffaa_facet.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.1. Solicitudes por tipo de respuesta por autoridad ------------------------

### 4.1.1. Entidades federativas -----------------------------------------------

# ---- Lista de entidades 
v_entidad <- c("Aguascalientes",
               "Baja California",
               "Baja California Sur",
               "Campeche",
               "Chiapas",
               "Chihuahua",
               "Ciudad de México",
               "Coahuila",
               "Colima",
               "Durango",                                                                      
               "Estado de México",
               "Guanajuato", 
               "Guerrero",
               "Hidalgo",
               "Jalisco",
               "Michoacán",
               "Morelos",
               "Nayarit",
               "Nuevo León",
               "Oaxaca",
               "Puebla",
               "Querétaro",
               "Quintana Roo",
               "San Luis Potosí",
               "Sinaloa",
               "Sonora",
               "Tabasco",
               "Tamaulipas",
               "Tlaxcala",
               "Veracruz",
               "Yucatán",
               "Zacatecas")

# ---- Datos
df_data <- df_limpia_a                                           %>% 
    filter(`Quién` %in% v_entidad)                               %>% 
    group_by(`Quién`,
             `Tipo de respuesta`) %>% 
    count()                                                      %>% 
    ungroup()                                                    %>%
    group_by(`Quién`)                                            %>% 
    mutate(porc = n/sum(n))                                      %>% 
    filter(`Tipo de respuesta` != "Sin respuesta")

# ---- Labels 
v_title    <- "Tipo de respuesta a las SAI"
v_subtitle <- "Por entidad federativa"

# ---- Figura 
ggplot(df_data, aes(y = reorder(`Quién`, desc(`Quién`)), x = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~`Tipo de respuesta`) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = .5,
              hjust = -.3,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 8),
                       expand = c(0,0)) +
    scale_y_discrete(labels = wrap_format(35)) +
    tema +
    theme(
        panel.grid.major.y    = element_blank(),
        panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76")
    )

# ---- Guardar figura 

# Local
plot_name  <- "respuesta_entidad.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 12)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 4.1.2. Dependencias federales ----------------------------------------------

# ---- Lista de entidades 
v_entidad <- c("Aguascalientes",
               "Baja California",
               "Baja California Sur",
               "Campeche",
               "Chiapas",
               "Chihuahua",
               "Ciudad de México",
               "Coahuila",
               "Colima",
               "Durango",                                                                      
               "Estado de México",
               "Guanajuato", 
               "Guerrero",
               "Hidalgo",
               "Jalisco",
               "Michoacán",
               "Morelos",
               "Nayarit",
               "Nuevo León",
               "Oaxaca",
               "Puebla",
               "Querétaro",
               "Quintana Roo",
               "San Luis Potosí",
               "Sinaloa",
               "Sonora",
               "Tabasco",
               "Tamaulipas",
               "Tlaxcala",
               "Veracruz",
               "Yucatán",
               "Zacatecas")

# ---- Datos
df_data <- df_limpia_a                                           %>% 
    filter(!`Quién` %in% v_entidad)                              %>% 
    group_by(`Quién`,
             `Tipo de respuesta`) %>% 
    count()                                                      %>% 
    ungroup()                                                    %>%
    group_by(`Quién`)                                            %>% 
    mutate(porc = n/sum(n))                                      %>% 
    filter(`Tipo de respuesta` != "Sin respuesta")


# ---- Labels 
v_title    <- "Tipo de respuesta a las SAI"
v_subtitle <- "Por dependencia federal\n"

# ---- Figura 
ggplot(df_data, aes(y = reorder(`Quién`, desc(`Quién`)), x = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~`Tipo de respuesta`,
               labeller=label_wrap_gen(width=30)) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = .5,
              hjust = -.3,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 15),
                       expand = c(0,0)) +
    scale_y_discrete(labels = wrap_format(50)) +
    tema +
    theme(
        panel.grid.major.y    = element_blank(),
        panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76")
    )

# ---- Guardar figura 

# Local
plot_name  <- "respuesta_dependencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 12, height = 15)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.2. Algún tipo de respuesta ------------------------------------------------

### 4.2.1. Por entidad federativa ----------------------------------------------

# ---- Lista de entidades 
v_entidad <- c("Aguascalientes",
               "Baja California",
               "Baja California Sur",
               "Campeche",
               "Chiapas",
               "Chihuahua",
               "Ciudad de México",
               "Coahuila",
               "Colima",
               "Durango",                                                                      
               "Estado de México",
               "Guanajuato", 
               "Guerrero",
               "Hidalgo",
               "Jalisco",
               "Michoacán",
               "Morelos",
               "Nayarit",
               "Nuevo León",
               "Oaxaca",
               "Puebla",
               "Querétaro",
               "Quintana Roo",
               "San Luis Potosí",
               "Sinaloa",
               "Sonora",
               "Tabasco",
               "Tamaulipas",
               "Tlaxcala",
               "Veracruz",
               "Yucatán",
               "Zacatecas")

# ---- Datos
df_data <- df_limpia_a                                           %>% 
    filter(`Tipo de respuesta` != "Sin respuesta",
           `Quién` %in% v_entidad)                               %>%
    mutate(info = ifelse(`Tipo de respuesta` == "Algún tipo de información", 
                         "Algún tipo de información", 
                         "Respuesta sin información"))           %>% 
    group_by(`Quién`,
             info)                                               %>% 
    count()                                                      %>% 
    ungroup()                                                    %>%
    group_by(`Quién`)                                            %>% 
    mutate(porc = n/sum(n)) %>% 
    filter(info == "Algún tipo de información")

# ---- Labels 
v_title    <- "Tipo de información en respuesta a las SAI"
v_subtitle <- "Por entidad federativa\n"

# ---- Figura 
ggplot(df_data, aes(y = reorder(`Quién`, desc(`Quién`)), x = n, fill = info)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = info,
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               size = 3,
               fill = "white",
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 11),
                       expand = c(0,0),
                       labels = label_number(accuracy = 1)) +
    scale_y_discrete(labels = wrap_format(35)) +
    scale_fill_manual(values = v_colores_fig) +
    tema +
    theme(
        panel.grid.major.y    = element_blank(),
        panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76")
    )

# ---- Guardar figura 

# Local
plot_name  <- "no_info_entidad.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 8)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 4.2.2. Por dependencia federal ---------------------------------------------

# ---- Lista de entidades 
v_entidad <- c("Aguascalientes",
               "Baja California",
               "Baja California Sur",
               "Campeche",
               "Chiapas",
               "Chihuahua",
               "Ciudad de México",
               "Coahuila",
               "Colima",
               "Durango",                                                                      
               "Estado de México",
               "Guanajuato", 
               "Guerrero",
               "Hidalgo",
               "Jalisco",
               "Michoacán",
               "Morelos",
               "Nayarit",
               "Nuevo León",
               "Oaxaca",
               "Puebla",
               "Querétaro",
               "Quintana Roo",
               "San Luis Potosí",
               "Sinaloa",
               "Sonora",
               "Tabasco",
               "Tamaulipas",
               "Tlaxcala",
               "Veracruz",
               "Yucatán",
               "Zacatecas")

# ---- Datos
df_data <- df_limpia_a                                           %>% 
    filter(`Tipo de respuesta` != "Sin respuesta",
           !`Quién` %in% v_entidad)                               %>%
    mutate(info = ifelse(`Tipo de respuesta` == "Algún tipo de información", 
                         "Algún tipo de información", 
                         "Respuesta sin información"))           %>% 
    group_by(`Quién`,
             info)                                               %>% 
    count()                                                      %>% 
    ungroup()                                                    %>%
    group_by(`Quién`)                                            %>% 
    mutate(porc = n/sum(n)) 

# ---- Labels 
v_title    <- "Tipo de información en respuesta a las SAI"
v_subtitle <- "Por dependencia federal\n"

# ---- Figura 
ggplot(df_data, aes(y = reorder(`Quién`, n), x = n, fill = info)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = info,
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               size = 3,
               fill = "white",
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 28),
                       expand = c(0,0)) +
    scale_y_discrete(labels = wrap_format(50)) +
    scale_fill_manual(values = v_colores_fig) +
    tema +
    theme(
        panel.grid.major.y    = element_blank(),
        panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76")
    )

# ---- Guardar figura 

# Local
plot_name  <- "no_info_dependencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 9)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.3. Solicitudes por ffaa ---------------------------------------------------

# ---- Datos

df_data <- df_limpia_a                                           %>%
    mutate(respuesta = ifelse(`Tipo de respuesta` == "Algún tipo de información",
                              "Algún tipo de información", 
                              "Sin información"))                %>% 
    filter(`Quién` %in% c("Secretaría de la Defensa Nacional", 
                          "Secretaría de Marina",
                          "Guardia Nacional"))                   %>% 
    group_by(`Quién`, respuesta)                                 %>% 
    count()                                                      %>% 
    ungroup()                                                    %>% 
    group_by(`Quién`)                                            %>% 
    mutate(porc = n/sum(n))

# ---- Labels 
v_title    <- "Tipo de respuesta a las SAI de las Fuerzas Armadas"
v_subtitle <- "Por año de solicitud\n"

# ---- Figura 
ggplot(df_data, aes(y = n, x = `Quién`, fill = respuesta)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = respuesta,
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               size = 3,
               vjust = 0.5,
               hjust = .5,
               fill = "white",
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 26),
                       expand = c(0,0)) +
    tema +
    scale_fill_manual(values = v_colores_fig)

# ---- Guardar figura 

# Local
plot_name  <- "respuesta_ffaa_.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.3. Año --------------------------------------------------------------------

# ---- Datos 
df_data <- df_limpia_a %>% 
    group_by(Año)      %>% 
    count()            %>% 
    mutate(Año = factor(Año, levels = c("2020", "2021", "2022", "2023")))

# ---- Labels 
v_title    <- "Solicitudes de acceso presentadas por año"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(x = as.character(Año), y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_label(aes(label = n,
                  fontface = "bold"),
               fill = "white",
              color = v_colores[4],
              size = 3,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "\nNúmero de solicitudes acceso a la información\n",
        x = "",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 189),
                       expand = c(0,0),
                       breaks = seq(0, 200, 20)) +
    scale_x_discrete(labels = wrap_format(35)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "año_solicitud.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.4. Por autoridad ----------------------------------------------------------

# ---- Lista de entidades 
v_entidad <- c("Aguascalientes",
               "Baja California",
               "Baja California Sur",
               "Campeche",
               "Chiapas",
               "Chihuahua",
               "Ciudad de México",
               "Coahuila",
               "Colima",
               "Durango",                                                                      
               "Estado de México",
               "Guanajuato", 
               "Guerrero",
               "Hidalgo",
               "Jalisco",
               "Michoacán",
               "Morelos",
               "Nayarit",
               "Nuevo León",
               "Oaxaca",
               "Puebla",
               "Querétaro",
               "Quintana Roo",
               "San Luis Potosí",
               "Sinaloa",
               "Sonora",
               "Tabasco",
               "Tamaulipas",
               "Tlaxcala",
               "Veracruz",
               "Yucatán",
               "Zacatecas")

### 4.4.1. Dependencias federeales ---------------------------------------------

# ---- Datos 

df_data <- df_limpia_a                %>% 
    filter(!`Quién` %in% v_entidad)   %>% 
    group_by(`Quién`)                 %>% 
    count() 
    
# ---- Labels 
v_title    <- "Solicitudes de acceso presentadas a dependencias federales"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(y = reorder(`Quién`, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.8,
              vjust = .5,
              hjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 28),
                       expand = c(0,0),
                       breaks = seq(0, 30, 5)) +
    scale_y_discrete(labels = wrap_format(80)) +
    tema 
    # theme(
    #     axis.text.y         = element_text(size = 8.5),
    #     panel.grid.major.y  = element_blank(),
    #     panel.grid.major.x  = element_line(linetype = 1, color = "#7A7E76")
    # )

# ---- Guardar figura 

# Local
plot_name  <- "federales_solicitud.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 8)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 4.4.2. Entidades federativas -----------------------------------------------

# ---- Datos 

df_data <- df_limpia_a                %>% 
    filter(`Quién` %in% v_entidad)    %>% 
    group_by(`Quién`)                 %>% 
    count() 

# ---- Labels 
v_title    <- "Solicitudes de acceso presentadas a entidades federativas"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(y = factor(`Quién`, 
                               levels = rev(levels(factor(`Quién`)))), x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = .5,
              hjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 11),
                       expand = c(0,0),
                       breaks = seq(0, 30, 5)) +
    scale_y_discrete(labels = wrap_format(60)) +
    tema 
    # theme(
    #     axis.text.y         = element_text(size = 8.5),
    #     panel.grid.major.y  = element_blank(),
    #     panel.grid.major.x  = element_line(linetype = 1, color = "#7A7E76")
    # )

# ---- Guardar figura 

# Local
plot_name  <- "entidades_solicitud.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 4.4.2. Respuesta de dependencias federeales --------------------------------

# ---- Datos 

df_data <- df_limpia_a                   %>% 
    filter(!`Quién` %in% v_entidad)      %>% 
    group_by(`Quién`, `Hubo respuesta`)  %>% 
    count()                              %>% 
    group_by(`Quién`)                    %>% 
    mutate(porc = n/sum(n))

# ---- Labels 
v_title    <- "Solicitudes de información presentadas a dependencias federales en las que obtuvimos respuesta"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data %>% filter(`Hubo respuesta` == "Sí"),
       aes(y = reorder(`Quién`, porc), x = porc)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = scales::percent(porc),
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = .5,
              hjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.1),
                       expand = c(0,0)) +
    scale_y_discrete(labels = wrap_format(60)) +
    tema 
    # theme(
    #     axis.text.y         = element_text(size = 8.5),
    #     panel.grid.major.y  = element_blank(),
    #     panel.grid.major.x  = element_line(linetype = 1, color = "#7A7E76")
    # )

# ---- Guardar figura 

# Local
plot_name  <- "federales_respuesta.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 4.4.4. Respuesta de entidades federativas ----------------------------------

# ---- Datos 

df_data <- df_limpia_a                   %>% 
    filter(`Quién` %in% v_entidad)       %>% 
    group_by(`Quién`, `Hubo respuesta`)  %>% 
    count()                              %>% 
    group_by(`Quién`)                    %>% 
    mutate(porc = n/sum(n))              %>% 
    mutate(porc2 = round(porc*100,
                         digits = 2))    %>% 
    mutate(porc2 = case_when(
        porc == 1 ~ "100",
        TRUE ~ as.character(porc2)))

# ---- Labels 
v_title    <- "Solicitudes de información presentadas a entidades federativas en las que obtuvimos respuesta"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data %>% filter(`Hubo respuesta` == "Sí"),
       aes(y = factor(`Quién`, 
                      levels = rev(levels(factor(`Quién`)))), x = porc)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = paste0(porc2, "%"),
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = .5,
              hjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 1.05),
                       expand = c(0,0),
                       labels = scales::percent_format()) +
    scale_y_discrete(labels = wrap_format(60)) +
    tema +
    theme(
        axis.text.y         = element_text(size = 8.5)
        # panel.grid.major.y  = element_blank(),
        # panel.grid.major.x  = element_line(linetype = 1, color = "#7A7E76")
    )

# ---- Guardar figura 

# Local
plot_name  <- "entidades_respuesta.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 5.5)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 4.5. Solicitudes por tipo de respuesta ffaa (general) -----------------------

# ---- Datos

df_data <- df_limpia_a                                            %>%
    filter(`Quién` %in% c("Secretaría de la Defensa Nacional", 
                          "Secretaría de Marina",
                          "Guardia Nacional"))                    %>% 
    group_by(`Tipo de respuesta`)  %>% 
    count()                        %>% 
    ungroup()                      %>% 
    mutate(porc = n/sum(n))
    

# ---- Labels 
v_title    <- "Respuestas de las instituciones militares a las solicitudes de acceso a la información pública"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(y = reorder(`Tipo de respuesta`, n),
                    x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = paste0(n, " (", percent(round(porc, digits = 3)), ")"),
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = .5,
              hjust = -.1,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de solicitudes de acceso a la información\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 26),
                       expand = c(0,0),
                       breaks = seq(0, 200, 20)) +
    scale_y_discrete(labels = wrap_format(35)) +
    tema
    # theme(
    #     panel.grid.major.y    = element_blank(),
    #     panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76")
    # )


# ---- Guardar figura 

# Local
plot_name  <- "tipo_respuesta_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

beepr::beep()

# FIN. -------------------------------------------------------------------------
