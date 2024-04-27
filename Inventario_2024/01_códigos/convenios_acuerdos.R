#------------------------------------------------------------------------------#
# Proyecto:                   Inventario Nacional de lo Militarizado
# Objetivo:                   Figuras saque 1
#
# Encargadas:                 Fernanda Torres
# Correo:                     ftorres@intersecta.org
# 
# Fecha de creación:          09 de febrero de 2023
# Última actualización:       18 de marzo   de 2024
#------------------------------------------------------------------------------#

# Fuente a convenios y a normas generales
# https://docs.google.com/spreadsheets/d/1hfdy7Gf9WSDkIigUrqUkRussgMzoQo9l/edit#gid=353440287

# Versión pública 
# https://docs.google.com/spreadsheets/d/1hfdy7Gf9WSDkIigUrqUkRussgMzoQo9l/edit#gid=353440287

# 0. Configuración inicial -----------------------------------------------------

# ---- Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# ---- Cargar librerías 
require(pacman)
p_load(
    googledrive, googlesheets4, writexl, scales, inegiR,
    readxl, tidyverse, beepr, extrafont, janitor, lubridate)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Desactivar notación científica
options(scipen=999)

# ---- Establecer directorios
paste_in <- function(x){paste0("01_datos/"                             , x)}
paste_fig <- function(x){paste0("03_figuras/saque_final/03_inventario/", x)}
paste_tab <- function(x){paste0("04_tablas/"                           , x)}

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

# 1.1. Cargar base de datos de convenios ---------------------------------------

# Path 
temp  <- tempfile(fileext = ".xlsx")

# Descargar base de datos final
dl <- drive_download("https://docs.google.com/spreadsheets/d/1hfdy7Gf9WSDkIigUrqUkRussgMzoQo9l/edit#gid=353440287.xlsx",
                     path = temp, overwrite = TRUE)

# Cargar a R
df_raw_convenios <- read_excel(dl$local_path,
                               sheet = 2, 
                               col_types = "text"
)

# 1.2. Cargar base de datos de normas generales --------------------------------

# Path 
temp  <- tempfile(fileext = ".xlsx")

# Descargar base de datos final
dl <- drive_download("https://docs.google.com/spreadsheets/d/1hfdy7Gf9WSDkIigUrqUkRussgMzoQo9l/edit#gid=353440287.xlsx",
                     path = temp, overwrite = TRUE)

# Cargar a R
df_raw_iniciativas <- read_excel(dl$local_path,
                                 sheet = 3, 
                                 # col_types = "text"
)

# 1.2. Notas sobre la base de datos --------------------------------------------

# En esta versión ya existe la variable "año" que facilitará el procesamiento 
# de la variable "fecha" y la variable "periodo de vigencia" que da más información sobre años
# Es la versión revisada por PPD y Nuria Valenzuela en marzo de 2024, que desagrega la variable 
# "¿Qué se transfirió?" en dos (función y presupuesto). Además se modificó la base anteriormente
# denominada como "Iniciativas" a "Normas generales" que distingue por tipo de norma. 
# Se eliminaron las variables de vehículo y clasificación de fundamento 

# 2. Procesar datos ------------------------------------------------------------

## 2.1. Funciones --------------------------------------------------------------

### 2.1.1. Wrapper -------------------------------------------------------------

wrapper <- function(x, ...) {
    paste(strwrap(x, ...), collapse = "\n")
}

## 2.2. Base de convenios y acuerdos -------------------------------------------

df_convenio <- df_raw_convenios                        %>% 
    mutate(Año = as.numeric(Año))                      %>% 
    select(-`Presupuesto transferido (pesos de 2023)`) %>% 
    # Tipo de transferencia
    mutate(`¿Presupuesto o funciones civiles?` = case_when(
        `Tipo de función` == "Civil" & `¿Presupuesto civil?` == "Sí" ~ "Función civil con presupuesto civil",
        `Tipo de función` == "Militar" & `¿Presupuesto civil?` == "Sí" ~ "Función militar con presupuesto civil",
        `Tipo de función` == "Civil" & `¿Presupuesto civil?` == "No" ~ "Función civil sin presupuesto civil",
    )) %>% 
    # Dummies por FFAA en transferencia 
    mutate(
        Sedena = ifelse(str_detect(`¿Quién recibe la transferencia?`, "Sedena"), 
                        1, 0),
        Semar = ifelse(str_detect(`¿Quién recibe la transferencia?`, "Semar"), 
                       1, 0),
        GN = case_when(
            str_detect(`¿Quién recibe la transferencia?`, "Guardia Nacional") ~ 1,
            TRUE ~ 0)
    ) %>% 
    # Dummies para nivel de gobierno
    mutate(
        federal = ifelse(str_detect(`Nivel de gobierno`, "ederal"), 
                         1, 0),
        estatal = ifelse(str_detect(`Nivel de gobierno`, "statal"), 
                         1, 0),
        municipal = ifelse(str_detect(`Nivel de gobierno`, "unicipal"), 
                           1, 0),
        privados = ifelse(str_detect(`Nivel de gobierno`, "rivados"), 
                          1, 0)
    ) %>% 
    # Dummies para qué fue transferido a las FFAA
    mutate(
        sp      = ifelse(str_detect(`¿Qué fue transferido a las instituciones militares?`, "eguridad pública"), 
                         1, 0),
        obra_p  = ifelse(str_detect(`¿Qué fue transferido a las instituciones militares?`, "bra pública"), 
                         1, 0),
        ambiente = ifelse(str_detect(`¿Qué fue transferido a las instituciones militares?`, "ambiente"), 
                          1, 0),
        misce    = ifelse(str_detect(`¿Qué fue transferido a las instituciones militares?`, "iscelánea"), 
                          1, 0),
        salud    = ifelse(str_detect(`¿Qué fue transferido a las instituciones militares?`, "alud"), 
                          1, 0),
        p_social = ifelse(str_detect(`¿Qué fue transferido a las instituciones militares?`, "olítica social"), 
                          1, 0),
        protec_c = ifelse(str_detect(`¿Qué fue transferido a las instituciones militares?`, "civil"),
                          1, 0)) %>% 
    mutate(
        sexenio = case_when(
            Año >= 2006 & Año < 2013 ~ "Calderón",
            Año >= 2013 & Año < 2019 ~ "Peña Nieto",
            Año >= 2019 & Año < 2023 ~ "López Obrador")) 

## 2.3. Base de iniciativas legislativas ---------------------------------------

df_iniciativas <- df_raw_iniciativas                         %>% 
    # Dar formato a fecha
    # Extraer año 
    mutate(year = Año)                                       %>% 
    # Convertir a lowercase variable de FFAA
    # mutate(ffaa = tolower(`Institución militar mencionada`)) %>% 
    # Dummies para ffaa
    # mutate(
    #     GN = str_detect(ffaa, "guardia nacional"),
    #     Sedena = str_detect(ffaa, "sedena"),
    #     Semar = str_detect(ffaa, "semar"),
    #     ffaa_d = str_detect(ffaa, "fuerzas armadas"))         %>%
    filter(!ID %in% c(83, 84, 85, 86))                        %>% 
    # Eliminar columnas 
    select(-c(`Descripción`, `¿Quién presentó la norma?`, 
              `Fecha`))

## 2.4. Periodo de vigencia de convenios ---------------------------------------

# El formato de la variable periodo de vigencia es "año inicial" + "-" + "año final"
# En este apartado expandí la base de datos para que cada observación con 
# información de periodo de vigencia se repitiera el número de años para el que
# el convenio o acuerdo es vigente. 

df_periodo <- df_convenio                                         %>% 
    # Seleccionar variables relevantes
    select(ID, Año, `Nivel de gobierno`, `entidad federativa`, `¿Qué institución(es) transfiere(n)?`,
           `¿Quién recibe la transferencia?`, `¿Qué fue transferido a las instituciones militares?`,
           `Tipo de función`, `¿Presupuesto civil?`, `Periodo de vigencia`, `Fundamento jurídico`,
           `Presupuesto descriptivo`, `Presupuesto transferido`, Sedena, Semar, GN, federal,
           estatal, municipal, privados, sp, obra_p, ambiente,                                   
           misce, salud, p_social, protec_c, `¿Presupuesto o funciones civiles?`)    %>% 
    # Convertir variable "Año" en character
    mutate(Año = as.character(Año))                            %>% 
    # Para convenios sin información mantener info de Año
    # Filtrar 
    filter(!`Periodo de vigencia` %in% c("Sin información",
                                         "Sin fecha de conclusión")) %>%
    mutate(periodo = `Periodo de vigencia`)                          %>% 
    # Usar información de año
    # mutate(periodo = case_when(
    #     `Periodo de vigencia` %in% c("Sin información", "Sin fecha de conclusión") ~ `Año`,
    #     TRUE ~ `Periodo de vigencia`))                             %>% 
    # Separar en dos columnas periodo de tiempo
    group_by(ID, Año, `Nivel de gobierno`, `entidad federativa`, `¿Qué institución(es) transfiere(n)?`,
             `¿Quién recibe la transferencia?`, `¿Qué fue transferido a las instituciones militares?`,
             `Tipo de función`, `¿Presupuesto civil?`, `Periodo de vigencia`, `Fundamento jurídico`,
             `Presupuesto descriptivo`, `Presupuesto transferido`, Sedena, Semar, GN, federal,
             estatal, municipal, privados, sp, obra_p, ambiente,                                   
             misce, salud, p_social, protec_c, `¿Presupuesto o funciones civiles?`)      %>% 
    mutate(periodo_control = `Periodo de vigencia`)                 %>% 
    separate(col = periodo, 
             into = c("inicio", "fin"),
             sep = "-")                                             %>% 
    # Rellenar NAs con valor de inicio en variable fin
    mutate(fin = ifelse(is.na(fin),`inicio`, `fin`))                %>% 
    # Convertir a valores numéricos
    mutate(inicio = round(as.numeric(inicio)), 
           fin = round(as.numeric(fin)))                            %>% 
    ungroup()                                                       %>% 
    # Expandir por año 
    mutate(n_times = fin - inicio + 1)                            %>% 
    slice(rep(row_number(), n_times))                             %>%  
    ungroup()                                                     %>% 
    group_by(ID, Año, `Nivel de gobierno`, `entidad federativa`, `¿Qué institución(es) transfiere(n)?`,
             `¿Quién recibe la transferencia?`, `¿Qué fue transferido a las instituciones militares?`,
             `Tipo de función`, `¿Presupuesto civil?`, `Periodo de vigencia`, `Fundamento jurídico`,
             `Presupuesto descriptivo`, `Presupuesto transferido`, inicio, fin, `¿Presupuesto o funciones civiles?`)  %>% 
    mutate(year = seq(from = inicio[1], # Fill years column from a to b in each group
                      to = fin[1]))                               %>% 
    ungroup()                                                     %>% 
    select(-inicio, -fin) 

## 2.5. Periodo de vigencia de convenios para promedio -------------------------

# El formato de la variable periodo de vigencia es "año inicial" + "-" + "año final"
# No sé que lógica tiene esto :p 

df_avigencia <- df_convenio                                         %>% 
    # Seleccionar variables relevantes
    select(ID, Año, `Nivel de gobierno`, `entidad federativa`, `¿Qué institución(es) transfiere(n)?`,
           `¿Quién recibe la transferencia?`, `¿Qué fue transferido a las instituciones militares?`,
           `Tipo de función`, `¿Presupuesto civil?`, `Periodo de vigencia`, `Fundamento jurídico`,
           `Presupuesto descriptivo`, `Presupuesto transferido`, Sedena, Semar, GN, federal,
           estatal, municipal, privados, sp, obra_p, ambiente,                                   
           misce, salud, p_social, protec_c, `¿Presupuesto o funciones civiles?`)          %>% 
    # Convertir variable "Año" en character
    mutate(Año = as.character(Año))                                 %>% 
    # Para convenios sin información mantener info de Año
    # Filtrar 
    filter(!`Periodo de vigencia` %in% c("Sin información",
                                         "Sin fecha de conclusión")) %>%
    mutate(periodo = `Periodo de vigencia`)                          %>% 
    # Usar información de año
    # mutate(periodo = case_when(
    #     `Periodo de vigencia` %in% c("Sin información", "Sin fecha de conclusión") ~ `Año`,
    #     TRUE ~ `Periodo de vigencia`))                             %>% 
    # Separar en dos columnas periodo de tiempo
    group_by(ID, Año, `Nivel de gobierno`, `entidad federativa`, `¿Qué institución(es) transfiere(n)?`,
             `¿Quién recibe la transferencia?`, `¿Qué fue transferido a las instituciones militares?`,
             `Tipo de función`, `¿Presupuesto civil?`, `Periodo de vigencia`, `Fundamento jurídico`,
             `Presupuesto descriptivo`, `Presupuesto transferido`, Sedena, Semar, GN, federal,
             estatal, municipal, privados, sp, obra_p, ambiente,                                   
             misce, salud, p_social, protec_c, `¿Presupuesto o funciones civiles?`)  %>% 
    mutate(periodo_control = `Periodo de vigencia`)                 %>% 
    separate(col = periodo, 
             into = c("inicio", "fin"),
             sep = "-")                                             %>% 
    # Rellenar NAs con valor de inicio en variable fin
    mutate(fin = ifelse(is.na(fin),`inicio`, `fin`))                %>% 
    # Convertir a valores numéricos
    mutate(inicio = round(as.numeric(inicio)), 
           fin = round(as.numeric(fin)))                            %>% 
    ungroup()                                                       %>% 
    # Expandir por año 
    mutate(n_times = fin - inicio + 1)                            %>% 
    ungroup()                                                     %>% 
    select(-inicio, -fin) 


# 3. Figuras -------------------------------------------------------------------

## 3.0. Configuración ----------------------------------------------------------

# ---- Tema 


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
v_caption  <- "Fuente: base de “Convenios y acuerdos” del Inventario Nacional de lo Militarizado.\n"
# path_drive <- as_id("1NJppYu5n3QRy4EmbKPE_mNpxME1K5AzI") # En mi unidad
path_drive <- as_id("1Gt62PYSJUNiZor3h70hHn5X0cAf-pU66") # En unidad compartida de INTR

## 3.1. Total de transferencias ------------------------------------------------

### 3.1.1. Convenios y/o acuerdos ----------------------------------------------

# ---- Datos 
df_data <- df_convenio            %>% 
    mutate(Año = as.numeric(Año)) %>% 
    group_by(Año)                 %>% 
    count()
    

df_2008 <- data.frame(
    Año = 2008, 
    n = 0
)


df_data <- df_data %>% 
    full_join(df_2008)

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto y funciones civiles a instituciones militares"
v_subtitle <- "Por año\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_label(aes(label = n,
                  fontface = "bold",
                  y = n + .87),
              color = v_colores[4],
              size = 2.5,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2007, 2022),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 47),
                       expand = c(0,0)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "total_convenio.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.1.2. Convenios y/o acuerdos + Iniciativas --------------------------------
# 
# # ---- Datos 
# df_data_c <- df_convenio %>% 
#     group_by(Año)        %>% 
#     count()              %>% 
#     rename(`Convenios y acuerdos` = n)
# 
# df_data_i <- df_iniciativas  %>% 
#     mutate(Año = as.numeric(Año)) %>% 
#     group_by(Año)            %>% 
#     count()                  %>% 
#     rename(Iniciativas  = n) %>% 
#     full_join(df_data_c)     %>% 
#     mutate(`Convenios y acuerdos` = replace_na(`Convenios y acuerdos`, 0),
#            Iniciativas = replace_na(Iniciativas, 0)) %>% 
#     pivot_longer(cols = c(2:3),
#                  values_to = "n", 
#                  names_to = "tipo") 
# 
# # ---- Labels 
# v_title    <- "Número de convenios, acuerdos e iniciativas legislaticas que transfieren presupuesto y/o funciones civiles a las instituciones militare"
# v_subtitle <- "Por año\n"
# 
# # ---- Figura 
# ggplot(df_data_i %>% filter(n != 0),
#        aes(x = Año, y = n, fill = tipo )) +
#     geom_col(position = position_stack()) +
#     geom_label(aes(label = n, group = tipo,
#                    fontface = "bold"),
#                position = position_stack(),
#                color = v_colores[4],
#                fill = "white",
#                size = 2.4,
#                vjust = 0.5,
#                hjust = .5,
#                family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "Número de convenios y acuerdos\n",
#         caption = v_caption,
#         fill = ""
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_x_continuous(breaks = seq(2007, 2023),
#                        expand = c(0,0)) +
#     scale_y_continuous(limits = c(0, NA),
#                        expand = c(0.05,0)) +
#     scale_fill_manual(values = v_colores_fig) +
#     tema +
#     theme(
#         axis.text.x = element_text(angle = 90)
#     )
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "total_transferencias.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 6)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# # ---- Con facet 
# 
# # ---- Labels 
# v_title    <- "Número de convenios, acuerdos e iniciativas legislaticas que transfieren presupuesto y/o funciones civiles a las instituciones militares"
# v_subtitle <- "Por año\n"
# 
# # ---- Figura 
# ggplot(df_data_i %>% filter(n != 0),
#        aes(x = Año, y = n)) +
#     geom_col(fill = v_colores_figs[1]) +
#     geom_label(aes(label = n, group = tipo,
#                    fontface = "bold"),
#                color = v_colores[4],
#                fill = "white",
#                size = 2.4,
#                vjust = 0.5,
#                hjust = .5,
#                family = "Fira Sans") +
#     facet_wrap(~tipo) +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "Número de convenios y acuerdos\n",
#         caption = v_caption,
#         fill = ""
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_x_continuous(breaks = seq(2007, 2022),
#                        expand = c(0,0)) +
#     scale_y_continuous(limits = c(0, NA),
#                        expand = c(0.05,0)) +
#     scale_fill_manual(values = v_colores_fig) +
#     tema +
#     theme(
#         axis.text.x = element_text(angle = 90)
#     )
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "01_total_transferencias_facet.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 9, height = 6)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.1.5. Presupuesto ---------------------------------------------------------

df_data <- df_convenio                    %>% 
    filter(`¿Presupuesto civil?` == "Sí") %>% 
    mutate(p = as.numeric(`Presupuesto transferido`)) %>% 
    summarise(total = sum(p, na.rm = T))

### 3.1.4. FFAA ----------------------------------------------------------------

# # ---- Datos 
# df_data <- df_convenio                            %>% 
#     select(Sedena, 
#            Semar,
#            GN)                                    %>% 
#     pivot_longer(cols = 1:3,
#                  names_to = "ffaa",
#                  values_to = "n")                 %>% 
#     group_by(ffaa)                                %>% 
#     summarise(n = sum(n))                         %>% 
#     mutate(ffaa = recode(ffaa, 
#                          "GN" = "Guardia Nacional")) %>% 
#     mutate(ffaa = factor(ffaa, 
#                          levels = c("Sedena", "Semar", "Guardia Nacional")))

df_data <- df_convenio %>% 
    mutate(ffaa = recode(`¿Quién recibe la transferencia?`,
                         "Sedena/Semar" = "Sedena y Semar")) %>% 
    group_by(ffaa)     %>% 
    count()

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren funciones o presupuesto civiles a instituciones militares (2007-2022)"
v_subtitle <- "Según la institución militar con la que es el convenio\n"

# ---- Figura 
ggplot(df_data, aes(x = reorder(ffaa, -n), y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                   fontface = "bold"),
               color = v_colores[4],
               size = 3.5,
               vjust = -.2,
               hjust = .5,
               family = "Henriette") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 230),
                       expand = c(0,0)) +
    scale_x_discrete(labels = wrap_format(15)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "transferencias_ffaa_total.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 3.2. Transferencia de funciones civiles -------------------------------------

### 3.2.1. General -------------------------------------------------------------

# ---- Datos 
df_data <- df_convenio          %>% 
    group_by(`Tipo de función`) %>% 
    count()

# ---- Labels 
v_title    <- "Tipo de funciones en los convenios y acuerdos de instituciones civiles a con institucines militares"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(x = `Tipo de función`, y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 248),
                       expand = c(0,0)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 0)
    )

# ---- Guardar figura 

# Local
plot_name  <- "total_funciones.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.2.2. Por año (%) ---------------------------------------------------------

# ---- Datos 
df_data <- df_convenio                                 %>% 
    group_by(Año, `Tipo de función`)                   %>% 
    count()                                            %>% 
    group_by(Año)                                      %>% 
    mutate(porc = n/sum(n))                            %>% 
    ungroup()                                          %>% 
    filter(`Tipo de función` != "Militar")             %>% 
    mutate(porc2 = round(porc, digits = 3)*100)        %>% 
    mutate(porc2 = case_when(
        porc == 1 ~ "100",
        TRUE ~ as.character(porc2)
    ))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren funciones civiles a instituciones militares"
v_subtitle <- "Por tipo de transferencia y año\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = porc)) +
    geom_col(fill = v_colores_fig[1]) +
    geom_label(aes(label = paste0(porc2, "%"), 
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               fill = "white",
               size = 2.5,
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2007, 2024),
                       expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent_format(),
                       expand = c(0,0),
                       limits = c(0, 1.1)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "funcion_año_porc.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.3. Por FFAA que recibe -------------------------------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    select(`Tipo de función`,
           Sedena, 
           Semar,
           GN)                                    %>% 
    pivot_longer(cols = 2:4,
                 names_to = "ffaa",
                 values_to = "n")                 %>% 
    group_by(ffaa, 
             `Tipo de función`)                   %>% 
    summarise(n = sum(n))                         %>% 
    filter(`Tipo de función` != "Militar")        %>% 
    mutate(ffaa = recode(ffaa, 
                         "GN" = "Guardia Nacional")) %>% 
    mutate(ffaa = factor(ffaa, 
                         levels = c("Sedena", "Semar", "Guardia Nacional")))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren funciones civiles a instituciones militares"
v_subtitle <- "Por institución que la recibe\n"

# ---- Figura 
ggplot(df_data, aes(x = ffaa, y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_label(aes(label = n,
                   fontface = "bold"),
               color = v_colores[4],
               fill = "white",
               size = 2.5,
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 210),
                       expand = c(0,0)) +
    scale_x_discrete(labels = wrap_format(15)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "funciones_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 3.3. Transferencia de presupuesto civiles -----------------------------------

### 3.3.1. General -------------------------------------------------------------

# ---- Datos 
df_data <- df_convenio              %>% 
    group_by(`¿Presupuesto civil?`) %>% 
    count()

# ---- Labels 
v_title    <- "¿En cuántos convenios y/o acuerdos es transferido presupuesto civil a instituciones militares"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(x = `¿Presupuesto civil?`, y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 270),
                       expand = c(0,0)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 0)
    )

# ---- Guardar figura 

# Local
plot_name  <- "total_presupuesto.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.2. Por año (%) ---------------------------------------------------------

# ---- Datos 
df_data <- df_convenio                                 %>% 
    group_by(Año, `¿Presupuesto civil?`)               %>% 
    count()                                            %>% 
    group_by(Año)                                      %>% 
    mutate(porc = n/sum(n))                            %>% 
    ungroup()                                          %>% 
    filter(`¿Presupuesto civil?` == "Sí")              %>% 
    mutate(porc2 = round(porc, digits = 3)*100)        %>% 
    mutate(porc2 = case_when(
        porc == 1 ~ "100",
        TRUE ~ as.character(porc2)
    ))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto civil a instituciones militares"
v_subtitle <- "Por tipo de transferencia y año\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = porc)) +
    geom_col(fill = v_colores_fig[1]) +
    geom_label(aes(label = paste0(porc2, "%"), 
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               fill = "white",
               size = 2.5,
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2007, 2024),
                       expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent_format(),
                       expand = c(0,0),
                       limits = c(0, 1.1)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "presupuesto_año_porc.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.3. Por FFAA que recibe -------------------------------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    select(`¿Presupuesto civil?`,
           Sedena, 
           Semar,
           GN)                                    %>% 
    pivot_longer(cols = 2:4,
                 names_to = "ffaa",
                 values_to = "n")                 %>% 
    group_by(ffaa, 
             `¿Presupuesto civil?`)               %>% 
    summarise(n = sum(n))                         %>% 
    filter(`¿Presupuesto civil?` == "Sí")         %>% 
    mutate(ffaa = recode(ffaa, 
                         "GN" = "Guardia Nacional")) %>% 
    mutate(ffaa = factor(ffaa, 
                         levels = c("Sedena", "Semar", "Guardia Nacional")))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto civil a instituciones militares"
v_subtitle <- "Por institución que la recibe\n"

# ---- Figura 
ggplot(df_data, aes(x = ffaa, y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 230),
                       expand = c(0,0)) +
    scale_x_discrete(labels = wrap_format(15)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "presupuesto_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.0. Prespuesto ¿identificado? -------------------------------------------

# ---- Datos

df_data <- df_convenio                    %>% 
    filter(`¿Presupuesto civil?` == "Sí") %>% 
    mutate(pre_iden = case_when(
        `Presupuesto transferido` != "-" ~ "Estipulado",
        TRUE ~ "No identificado"))        %>% 
    mutate(tipo = case_when(
        !str_detect(`Presupuesto descriptivo`, "no establece el presupuesto") & pre_iden == "No identificado" ~ "No es posible determinar",
        str_detect(`Presupuesto descriptivo`, "no establece el presupuesto") ~ "No estipulado",
        TRUE ~ "Estipulado",
    )) %>% 
    group_by(pre_iden, tipo)               %>% 
    count()                                %>% 
    ungroup()                              %>% 
    mutate(porc = n/sum(n))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto civil a instituciones militares"
v_subtitle <- "Según si es posible identificar el monto\n"

# ---- Figura 
ggplot(df_data, aes(x = pre_iden, y = n, fill = tipo)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n,group = tipo,
                   fontface = "bold"),
               position = position_stack(),
               color = v_colores[4],
               fill = "white",
               size = 2.5,
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        fill = "",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_fill_manual(values = v_colores_figs) +
    scale_y_continuous(limits = c(0, 138),
                       expand = c(0,0)) +
    scale_x_discrete(labels = wrap_format(15)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "tipo_presupuesto_iden.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.8. Presupuesto transferido no identificado -----------------------------

# ---- Datos 

df_data <- df_raw_convenios                  %>% 
    filter(
        # `¿Qué fue transferido a las Fuerzas Armadas?` %in% c(
        #     "Presupuesto civil",
        #     "Función civil con presupuesto"),
        `Presupuesto transferido` == "-")    %>% 
    mutate(presupuesto_na = case_when(
        str_detect(`Presupuesto descriptivo`, "no establece el presupuesto") ~ "No estipulado",
        TRUE ~ "No es posible determinar"
    ))                                       %>% 
    group_by(presupuesto_na)                 %>% 
    count()

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto civil cuyo monto no fue posible identificar"
v_subtitle <- "Por tipo obstáculo\n"

# ---- Figura 
ggplot(df_data, aes(x = presupuesto_na, y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 84),
                       expand = c(0,0)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "no_presupuesto.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.9. Presupuesto transferido no identificado por año ---------------------

# ---- Datos 

df_data <- df_raw_convenios                  %>% 
    filter(
        # `¿Qué fue transferido a las Fuerzas Armadas?` %in% c(
        #     "Presupuesto civil",
        #     "Función civil con presupuesto"),
        `Presupuesto transferido` == "-")    %>% 
    mutate(presupuesto_na = case_when(
        str_detect(`Presupuesto descriptivo`, "no establece el presupuesto") ~ "El monto no viene estipulado",
        TRUE ~ "No es posible determinar el monto"
    ))                                       %>% 
    group_by(presupuesto_na, Año)            %>% 
    count()

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto civil cuyo monto no fue posible identificar"
v_subtitle <- "Por año y tipo de obstáculo para identificar el monto\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n, fill = presupuesto_na)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = presupuesto_na,
                   fontface = "bold"),
               position = position_stack(),
               color = v_colores[4],
               fill = "white",
               size = 2.5,
               vjust = 0.5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 24),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = seq(2007, 2022)) +
    scale_fill_manual(values = v_colores_fig) +
    tema

# ---- Guardar figura 

# Local
plot_name  <- "no_presupuesto_año.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6.2)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.10. Presupuesto transferido no identificado por vigencia ---------------

# ---- Datos 

df_data <- df_periodo                        %>% 
    filter(
        # `¿Qué fue transferido a las Fuerzas Armadas?` %in% c(
        #     "Presupuesto civil",
        #     "Función civil con presupuesto"),
        `Presupuesto transferido` == "-")    %>% 
    mutate(presupuesto_na = case_when(
        str_detect(`Presupuesto descriptivo`, "no establece el presupuesto") ~ "No estipulado",
        TRUE ~ "No es posible determinar"
    ))                                       %>% 
    group_by(presupuesto_na, year)           %>% 
    count()

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto civiles cuyo monto no fue posible identificar"
v_subtitle <- "Por tipo obstáculo y año de vigencia\n"

# ---- Figura 
ggplot(df_data, aes(x = year, y = n, fill = presupuesto_na)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = presupuesto_na,
                   fontface = "bold"),
               position = position_stack(),
               color = v_colores[4],
               fill = "white",
               size = 2.5,
               vjust = 0.5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 36),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = seq(2007, 2027)) +
    scale_fill_manual(values = v_colores_fig) +
    tema +
    theme(
        axis.text.x = element_text(angle = 90)
    )

# ---- Guardar figura 

# Local
plot_name  <- "no_presupuesto_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.11. Presupuesto no determinado por ffaa --------------------------------

# ---- Datos 

df_data <- df_convenio                       %>% 
    filter(
        # `¿Qué fue transferido a las Fuerzas Armadas?` %in% c(
        #     "Presupuesto civil",
        #     "Función civil con presupuesto"),
        `Presupuesto transferido` == "-")    %>% 
    mutate(presupuesto_na = case_when(
        str_detect(`Presupuesto descriptivo`, "no establece el presupuesto") ~ "No estipulado",
        TRUE ~ "No es posible determinar"
    ))                                        %>%
    filter(presupuesto_na == "No estipulado") %>% 
    select(c(22:24))                          %>% 
    pivot_longer(cols = c(1:3),
                 names_to = "ffaa",
                 values_to = "n")             %>% 
    group_by(ffaa)                            %>% 
    summarise(n = sum(n))                     %>% 
    mutate(ffaa = recode(ffaa,
                         "GN" = "Guardia Nacional"))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto civiles cuyo monto no fue estipulado"
v_subtitle <- "Por institución que recibe la transferencia\n"

# ---- Figura 
ggplot(df_data, aes(x = reorder(ffaa, -n), y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 53),
                       expand = c(0,0)) +
    tema

# ---- Guardar figura 

# Local
plot_name  <- "no_presupuesto_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.12. Presupuesto no determinado por nivel --------------------------------

# ---- Datos 

df_data <- df_convenio                       %>% 
    filter(
        # `¿Qué fue transferido a las Fuerzas Armadas?` %in% c(
        #     "Presupuesto civil",
        #     "Función civil con presupuesto"),
        `Presupuesto transferido` == "-")    %>% 
    mutate(presupuesto_na = case_when(
        str_detect(`Presupuesto descriptivo`, "no establece el presupuesto") ~ "No estipulado",
        TRUE ~ "No es posible determinar"
    ))                                        %>%
    filter(presupuesto_na == "No estipulado") %>% 
    select(c(25:28))                          %>% 
    pivot_longer(cols = c(1:4),
                 names_to = "nivel",
                 values_to = "n")             %>% 
    group_by(nivel)                            %>% 
    summarise(n = sum(n))                     %>% 
    mutate(nivel = recode(nivel, 
                          "federal" = "Federal",
                          "estatal" = "Estatal",
                          "municipal" = "Municipal",
                          "privados" = "Privados"))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto civiles cuyo monto no fue estipulado"
v_subtitle <- "Por nivel de gobierno desde el que se transfiere\n"

# ---- Figura 
ggplot(df_data, aes(x = reorder(nivel, -n), y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 47),
                       expand = c(0,0)) +
    tema

# ---- Guardar figura 

# Local
plot_name  <- "no_presupuesto_nivel.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.13. Dineros ------------------------------------------------------------

df_data_dineros <- df_convenio                 %>% 
    select(`Presupuesto transferido`)          %>% 
    mutate(dineros_limpio =  gsub("\\$", '', `Presupuesto transferido`)) %>% 
    mutate(dineros_limpio =  gsub("\\,", '', dineros_limpio))            %>%
    mutate(dineros_limpio =  gsub(" ", '', dineros_limpio))            %>%
    mutate(dineros = as.numeric(dineros_limpio)) %>% 
    filter(`Presupuesto transferido` != "-")

## 3.4. Presupuesto y funciones civiles ----------------------------------------

### 3.4.1. General -------------------------------------------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    group_by(`¿Presupuesto o funciones civiles?`) %>% 
    count()

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto y funciones civiles a instituciones militares"
v_subtitle <- "Por tipo de transferencia\n"

# ---- Figura 
ggplot(df_data, aes(y = reorder(`¿Presupuesto o funciones civiles?`, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_label(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "\nNúmero de convenios y acuerdos\n",
        y = "",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 240),
                       expand = c(0,0)) +
    scale_y_discrete(labels = wrap_format(20)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "03_funcion_presupuesto.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.2. Por año de convenio -------------------------------------------------

# ---- Datos 
df_data <- df_convenio                                 %>% 
    group_by(Año, `¿Presupuesto o funciones civiles?`) %>% 
    count()

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto y funciones civiles a instituciones militares"
v_subtitle <- "Por tipo de transferencia y año\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~`¿Presupuesto o funciones civiles?`,
               ncol = 1,
               scales = "free_x") +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2007, 2022),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 45),
                       expand = c(0,0)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_año.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.2b. Por año de convenio (%) --------------------------------------------

# ---- Datos 
df_data <- df_convenio                                 %>% 
    group_by(Año, `¿Presupuesto o funciones civiles?`) %>% 
    count()                                            %>% 
    group_by(Año)                                      %>% 
    mutate(porc = n/sum(n))                            %>% 
    ungroup()                                          %>% 
    mutate(`¿Presupuesto o funciones civiles?` = factor(`¿Presupuesto o funciones civiles?`, 
                                                        levels = c(
                                                            "Función civil sin presupuesto civil",
                                                            "Función militar con presupuesto civil",
                                                            "Función civil con presupuesto civil"
                                                            )))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto y funciones civiles a instituciones militares"
v_subtitle <- "Por tipo de transferencia y año\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = porc, 
                    fill = `¿Presupuesto o funciones civiles?`,
                    group = `¿Presupuesto o funciones civiles?`)) +
    geom_col(position = position_stack()) +
    scale_fill_manual(values = v_colores_figs) +
    geom_label(aes(label = percent(round(porc, digits = 3)), group = `¿Presupuesto o funciones civiles?`,
                  fontface = "bold"),
              color = v_colores[4],
              position = position_stack(),
              fill = "white",
              size = 2.5,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2007, 2024),
                       expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent_format()) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_año_porc.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.3. Por año de vigencia -------------------------------------------------

# ---- Datos 
df_data <- df_periodo                                   %>% 
    group_by(year, `¿Presupuesto o funciones civiles?`) %>% 
    count()

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por tipo de transferencia y año de vigencia\n"

# ---- Figura 
ggplot(df_data, aes(x = year, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~`¿Presupuesto o funciones civiles?`,
               ncol = 1,
               scales = "free_x") +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2002, 2027),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 42),
                       expand = c(0,0)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 90)
    )

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.3b. Por año de vigencia (%) --------------------------------------------

# ---- Datos 
df_data <- df_periodo                                   %>% 
    group_by(year, `¿Presupuesto o funciones civiles?`) %>% 
    count()                                             %>% 
    group_by(year)                                      %>% 
    mutate(porc = n/sum(n))                             %>% 
        mutate(`¿Presupuesto o funciones civiles?` = factor(`¿Presupuesto o funciones civiles?`, 
                                                        levels = c(
                                                            "Función civil sin presupuesto civil",
                                                            "Función militar con presupuesto civil",
                                                            "Función civil con presupuesto civil"
                                                        ))) %>% 
    filter(year < 2024)

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por tipo de transferencia y año de vigencia del convenio o acuerdo\n"
v_colores_figs_ <- c( "#959A90", "#2a2b29", "#647f2e")

# ---- Figura 
ggplot(df_data, aes(x = year, y = porc, fill = `¿Presupuesto o funciones civiles?`)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = percent(round(porc, digits = 2)), group = `¿Presupuesto o funciones civiles?`,
                  fontface = "bold"),
              color = v_colores[4],
              fill = "white",
              position = position_stack(),
              size = 2.5,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2002, 2023),
                       expand = c(0,0)) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = v_colores_figs_) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_vigencia_porc.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.2.3. Años promedio de vigencia -------------------------------------------

# ---- Datos 
df_data <- df_periodo                             %>% 
    group_by(`¿Presupuesto o funciones civiles?`) %>% 
    summarise(prom = mean(n_times))

# ---- Labels 
v_title    <- "Promedio de años de vigencia de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por tipo de transferencia\n"

# ---- Figura 
ggplot(df_data, aes(x = `¿Presupuesto o funciones civiles?`, y = prom)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = round(prom, digits = 1),
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 19.1),
                       expand = c(0,0)) +
    scale_x_discrete(labels = wrap_format(20)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 0)
    )

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_prom_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.4. Por nivel de gobierno -----------------------------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    select(`¿Presupuesto o funciones civiles?`,
           25:28)                                 %>% 
    pivot_longer(cols = 2:5,
                 names_to = "nivel",
                 values_to = "n")                 %>% 
    group_by(nivel, 
             `¿Presupuesto o funciones civiles?`) %>% 
    summarise(n = sum(n))                         %>% 
    mutate(nivel = recode(nivel, 
                          "federal" = "Federal",
                          "estatal" = "Estatal",
                          "municipal" = "Municipal",
                          "privados" = "Privados"))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por tipo de transferencia y nivel de gobierno desde la que se realiza\n"

# ---- Figura 
ggplot(df_data, aes(x = reorder(nivel, -n), y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~`¿Presupuesto o funciones civiles?`,
               ncol = 3) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 120),
                       expand = c(0,0)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_nivel.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 9, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.5. Por FFAA que recibe -------------------------------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    select(`¿Presupuesto o funciones civiles?`,
           Sedena, 
           Semar,
           GN)                                    %>% 
    pivot_longer(cols = 2:4,
                 names_to = "ffaa",
                 values_to = "n")                 %>% 
    group_by(ffaa, 
             `¿Presupuesto o funciones civiles?`) %>% 
    summarise(n = sum(n))                         %>% 
    mutate(ffaa = recode(ffaa, 
                         "GN" = "Guardia Nacional")) %>% 
    mutate(ffaa = factor(ffaa, 
                         levels = c("Sedena", "Semar", "Guardia Nacional")))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto y funciones civiles a instituciones militares"
v_subtitle <- "Por tipo de transferencia e institución que la recibe\n"

# ---- Figura 
ggplot(df_data, aes(x = ffaa, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~`¿Presupuesto o funciones civiles?`,
               ncol = 3,
               labeller=label_wrap_gen(width=25)) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 210),
                       expand = c(0,0)) +
    scale_x_discrete(labels = wrap_format(15)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.5b. Por FFAA que recibe (%) --------------------------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    select(`¿Presupuesto o funciones civiles?`, 
           Sedena, Semar, GN)                     %>% 
    pivot_longer(cols = 2:4,
                 names_to = "ffaa",
                 values_to = "n")                 %>% 
    group_by(ffaa, 
             `¿Presupuesto o funciones civiles?`) %>% 
    summarise(n = sum(n))                         %>% 
    filter(n != 0)                                %>% 
    group_by(ffaa)                                %>% 
    mutate(porc = n/sum(n))                       %>% 
    mutate(ffaa = recode(ffaa, 
                         "GN" = "Guardia Nacional")) %>% 
    mutate(ffaa = factor(ffaa, 
                         levels = c("Sedena", "Semar", "Guardia Nacional")),
           `¿Presupuesto o funciones civiles?` = factor(`¿Presupuesto o funciones civiles?`, 
                                                        levels = c(
                                                            "Función civil sin presupuesto civil",
                                                            "Función militar con presupuesto civil",
                                                            "Función civil con presupuesto civil"
                                                        )))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por tipo de transferencia e institución que la recibe\n"

# ---- Figura 
ggplot(df_data, aes(x = ffaa, y = porc, fill = `¿Presupuesto o funciones civiles?`)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = percent(round(porc, digits = 3)), group = `¿Presupuesto o funciones civiles?`,
                  fontface = "bold"),
              color = v_colores[4],
              fill = "white",
              position = position_stack(),
              size = 2.5,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_fill_manual(values = v_colores_figs) +
    scale_y_continuous(labels = percent_format()) +
    scale_x_discrete(labels = wrap_format(15)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_ffaa_porc.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)


### 3.4.6. Por FFAA que recibe por año -----------------------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    select(Año, 
           `¿Presupuesto o funciones civiles?`,
           Sedena, Semar, GN)                          %>% 
    pivot_longer(cols = 3:5,
                 names_to = "ffaa",
                 values_to = "n")                 %>% 
    filter(n != 0)                                %>% 
    group_by(ffaa, Año,
             `¿Presupuesto o funciones civiles?`) %>% 
    summarise(n = sum(n))                         %>% 
    mutate(ffaa = recode(ffaa, 
                         "GN" = "Guardia Nacional")) %>% 
    mutate(ffaa = factor(ffaa, 
                         levels = c("Sedena", "Semar", "Guardia Nacional")),
           `¿Presupuesto o funciones civiles?` = factor(`¿Presupuesto o funciones civiles?`,
                                                        levels = c(
                                                            "Función civil con presupuesto militar",
                                                            "Función civil sin presupuesto civil" ,
                                                            "Función militar con presupuesto civil"
                                                        )))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por tipo de transferencia, institución que la recibe y año\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n, fill = reorder(`¿Presupuesto o funciones civiles?`, n))) +
    geom_col(position = position_stack()) +
    facet_wrap(~ ffaa,
               ncol = 3) +
    geom_label(aes(label = n,
                  fontface = "bold"),
              position = position_stack(),
              color = v_colores[4],
              fill = "white",
              size = 2,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    scale_x_continuous(breaks = seq(2007, 2022)) +
    scale_fill_manual(values = v_colores_figs) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 37),
                       expand = c(0,0)) +
    tema +
    theme(axis.text.x = element_text(angle = 90))

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_ffaa_año.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.7. Por FFAA que recibe por año -----------------------------------------

# ---- Datos 
df_data <- df_periodo                             %>% 
    select(`¿Presupuesto o funciones civiles?`,
           year, 
           Sedena, Semar, GN)                     %>% 
    pivot_longer(cols = 3:5,
                 names_to = "ffaa",
                 values_to = "n")                 %>% 
    filter(n != 0)                                %>% 
    group_by(ffaa, year,
             `¿Presupuesto o funciones civiles?`) %>% 
    summarise(n = sum(n))                         %>% 
    mutate(ffaa = recode(ffaa, 
                         "GN" = "Guardia Nacional")) %>% 
    mutate(ffaa = factor(ffaa, 
                         levels = c("Sedena", "Semar", "Guardia Nacional"))) %>% 
    filter(year %in% c(2007:2023))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por tipo de transferencia, institución que la recibe y año de vigencia\n"

# ---- Figura 
ggplot(df_data, aes(x = year, y = n, fill = reorder(`¿Presupuesto o funciones civiles?`, n))) +
    geom_col(position = position_stack()) +
    facet_wrap(~ ffaa,
               ncol = 3) +
    geom_label(aes(label = n,
                  fontface = "bold"),
              position = position_stack(),
              color = v_colores[4],
              fill = "white",
              size = 2,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    scale_x_continuous(breaks = seq(2007, 2023),
                       expand = c(0,0)
                       ) +
    scale_fill_manual(values = v_colores_figs) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 32),
                       expand = c(0,0)) +
    tema +
    theme(axis.text.x = element_text(angle = 90))

# ---- Guardar figura 

# Local
plot_name  <- "funcion_presupuesto_ffaa_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 8)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 3.3. Funciones transferidas -------------------------------------------------

### 3.3.1. Totales -------------------------------------------------------------

# ---- Datos 

df_data <- df_convenio %>%
    filter(`Tipo de función` == "Civil") %>% 
    select(c(29:35))   %>% 
    pivot_longer(cols = everything(),
                 names_to = "materia",
                 values_to = "n") %>% 
    group_by(materia)             %>% 
    summarise(n = sum(n))         %>% 
    ungroup()                     %>% 
    mutate(porc = (round(n/sum(n), digits = 2)*100))       %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        materia == "obra_p" ~ "Obra pública", 
        materia == "ambiente" ~ "Protección y restauración del ambiente",
        materia == "misce" ~ "Miscelánea", 
        materia == "salud" ~ "Salud", 
        materia == "p_social" ~ "Política social",
        materia == "protec_c" ~ "Protección civil", 
        materia == "puertos" ~ "Administración de puertos y aeropuertos",
        materia == "invest" ~ "Investigación académica y científica"
    ))                           

# ---- Labels 
v_title    <- "Acuerdos y convenios que les transfieren funciones civiles a las instituciones militares (2007-2022)"
v_subtitle <- "Según el rubro o materia de la función civil"

# ---- Figura 
ggplot(df_data, aes(y = reorder(nombre, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = paste0(porc, "% (", n, ")"),
                  fontface = "bold"),
              color = v_colores[4],
              size = 3.5,
              vjust = .5,
              hjust = -.1,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 140),
                       expand = c(0,0),
                       breaks = seq(0, 120, 20)) +
    scale_y_discrete(labels = wrap_format(30)) +
    tema

# ---- Guardar figura 

# Local
plot_name  <- "qué_funciones_total.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.2. Por año -------------------------------------------------------------

# ---- Datos 

df_data <- df_convenio      %>%
    filter(`Tipo de función` == "Civil"
           ) %>%
    select(c(29:35), Año)   %>% 
    pivot_longer(cols = c(1:7),
                 names_to = "materia",
                 values_to = "n") %>% 
    group_by(materia, Año)        %>% 
    summarise(n = sum(n))         %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        materia == "obra_p" ~ "Obra pública", 
        materia == "ambiente" ~ "Protección y restauración del ambiente",
        materia == "misce" ~ "Miscelánea", 
        materia == "salud" ~ "Salud", 
        materia == "p_social" ~ "Política social",
        materia == "protec_c" ~ "Protección civil", 
        materia == "puertos" ~ "Administración de puertos y aeropuertos",
        materia == "invest" ~ "Investigación académica y científica"
    ))

# ---- Labels 
v_title    <- "¿Qué fue transferido a las instituciones militares?"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~nombre,
               labeller=label_wrap_gen(width=30)) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = c(2007:2022)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 90)
    ) 

# ---- Guardar figura 

# Local
plot_name  <- "qué_año.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.2. Por año -------------------------------------------------------------

# ---- Datos 

df_data <- df_convenio      %>%
    filter(`Tipo de función` == "Civil"
           ) %>%
    select(c(29:35), Año)   %>% 
    pivot_longer(cols = c(1:7),
                 names_to = "materia",
                 values_to = "n") %>% 
    filter(n == 1)                %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        TRUE ~ "Otra"
    ))                            %>% 
    group_by(nombre, Año)         %>% 
    summarise(n = sum(n))         %>% 
    group_by(Año)                 %>% 
    mutate(porc = n/sum(n))

# ---- Labels 
v_title    <- "¿Qué fue transferido a las instituciones militares?"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~nombre,
               labeller=label_wrap_gen(width=30)) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 42),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = c(2007:2022)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 90)
    ) 

# ---- Guardar figura 

# Local
plot_name  <- "sp_año.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

# ---- Stacked 

# ---- Labels 
v_title    <- "¿Qué funciones fueron transferidas a las instituciones militares?"
v_subtitle <- "Por año\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = porc, fill = nombre, group = nombre)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = scales::percent(porc), group = nombre,
                  fontface = "bold"),
              color = v_colores[4],
              fill = "white",
              position = position_stack(1),
              size = 3,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(breaks = c(2007:2022)) +
    tema +
    scale_fill_manual(values = v_colores_figs) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = .5)
    ) 

# ---- Guardar figura 

# Local
plot_name  <- "sp_año_stacked_porc.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)


### 3.3.2b. Por año (stacked) --------------------------------------------------

# ---- Datos 

df_data <- df_convenio      %>%
    filter(`Tipo de función` == "Civil"
           ) %>%
    select(c(29:35), Año)   %>% 
    pivot_longer(cols = c(1:7),
                 names_to = "materia",
                 values_to = "n") %>% 
    filter(n == 1)                %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        TRUE ~ "Otra"
    ))                            %>% 
    group_by(nombre, Año)         %>% 
    summarise(n = sum(n))         %>% 
    mutate(nombre = factor(nombre, levels = c("Seguridad pública", "Otra")))


# ---- Labels 
v_title    <- "¿De qué materia fueron las funciones civiles transferidas a las instituciones militares?"
v_subtitle <- "Por año\n"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n, fill = nombre)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = nombre,
                  fontface = "bold"),
               position = position_stack(),
              color = v_colores[4],
              fill = "white",
              size = 3,
              vjust = .5,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    scale_fill_manual(values = v_colores_figs) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 52),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = c(2007:2022)) +
    tema

# ---- Guardar figura 

# Local
plot_name  <- "sp_año_stacked.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.2c. Por año (stacked %) ------------------------------------------------

# ---- Datos 

df_data <- df_convenio      %>%
    filter(`Tipo de función` == "Civil"
           )               %>%
    select(c(29:35), Año)   %>% 
    pivot_longer(cols = c(1:7),
                 names_to = "materia",
                 values_to = "n") %>% 
    filter(n == 1)                %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        TRUE ~ "Otra"
    ))                            %>% 
    group_by(nombre, Año)         %>% 
    summarise(n = sum(n))         %>% 
    group_by(Año)                 %>% 
    mutate(porc = n/sum(n))       %>% 
    mutate(nombre = factor(nombre, levels = c("Seguridad pública", "Otra")))


# ---- Labels 
v_title    <- "¿Qué fue transferido a las instituciones militares?"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n, fill = nombre)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = nombre,
                   fontface = "bold"),
               position = position_stack(),
               color = v_colores[4],
               fill = "white",
               size = 3,
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    scale_fill_manual(values = v_colores_figs) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 52),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = c(2007:2022)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 90)
    ) 

# ---- Guardar figura 

# Local
plot_name  <- "sp_año_stacked.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.3. Por vigencia --------------------------------------------------------

# ---- Datos 

df_data <- df_periodo        %>%
    filter(`Tipo de función` == "Civil"
           )                %>%
    select(c(21:27), year)   %>% 
    pivot_longer(cols = c(1:7),
                 names_to = "materia",
                 values_to = "n") %>% 
    group_by(materia, year)       %>% 
    summarise(n = sum(n))         %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        materia == "obra_p" ~ "Obra pública", 
        materia == "ambiente" ~ "Protección y restauración del ambiente",
        materia == "misce" ~ "Miscelánea", 
        materia == "salud" ~ "Salud", 
        materia == "p_social" ~ "Política social",
        materia == "protec_c" ~ "Protección civil", 
        materia == "puertos" ~ "Administración de puertos y aeropuertos",
        materia == "invest" ~ "Investigación académica y científica"
    ))

# ---- Labels 
v_title    <- "¿Qué fue transferido a las instituciones militares?"
v_subtitle <- "Por año de vigencia\n"

# ---- Figura 
ggplot(df_data, aes(x = year, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~nombre,
               labeller=label_wrap_gen(width=30),
               ncol = 3) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = .5,
              hjust = -.1,
              angle = 90,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 24),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = c(2002:2027),
                       expand = c(0,0)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 90)
    ) 

# ---- Guardar figura 

# Local
plot_name  <- "qué_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 8)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.3. Por vigencia --------------------------------------------------------

# ---- Datos 

df_data <- df_periodo        %>%
    filter(`Tipo de función` == "Civil"
           )                %>%
    select(c(21:27), year)   %>% 
    pivot_longer(cols = c(1:7),
                 names_to = "materia",
                 values_to = "n") %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        TRUE ~ "Otra"
    ))                           %>% 
    group_by(nombre, year)       %>% 
    summarise(n = sum(n))

# ---- Labels 
v_title    <- "¿Qué fue transferido a las instituciones militares?"
v_subtitle <- "Por año de vigencia\n"

# ---- Figura 
ggplot(df_data, aes(x = year, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~nombre,
               labeller=label_wrap_gen(width=30),
               ncol = 3) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 37),
                       expand = c(0,0)) +
    scale_x_continuous(breaks = c(2002:2027),
                       expand = c(0,0)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 90)
    ) 

# ---- Guardar figura 

# Local
plot_name  <- "sp_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 10, height = 8)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.4. Por nivel de gobierno -----------------------------------------------

# ---- Vectores 
v_nivel   <- c("federal", "estatal", "municipal", "privados")
v_nombres <- c("Federal", "Estatal", "Municipal", "Privados")

# ---- Datos 

# Base vacía
df_data <- data.frame(
    materia = as.character(),
    n = as.numeric(),
    nivel_g = as.character(),
    nombre = as.character()
)

# Datos
for (i in 1:length(v_nivel)) {
    
    df_temp <- df_convenio             %>%
        filter(`Tipo de función` == "Civil"
               )                      %>%
        rename(nivel = v_nivel[i])     %>%
        filter(nivel == 1)             %>% 
        select(c(29:35))               %>% 
        pivot_longer(cols = c(1:7),
                     names_to = "materia",
                     values_to = "n") %>% 
        group_by(materia)             %>% 
        summarise(n = sum(n))         %>% 
        mutate(
            nivel_g = v_nombres[i],
            nombre = case_when(
                materia == "sp" ~ "Seguridad pública", 
                materia == "obra_p" ~ "Obra pública", 
                materia == "ambiente" ~ "Protección y restauración del ambiente",
                materia == "misce" ~ "Miscelánea", 
                materia == "salud" ~ "Salud", 
                materia == "p_social" ~ "Política social",
                materia == "protec_c" ~ "Protección civil", 
                materia == "puertos" ~ "Administración de puertos y aeropuertos",
                materia == "invest" ~ "Investigación académica y científica"
            )) 
    
    # Guardar base 
    df_data <- df_data %>% full_join(df_temp)
    
}

df_data <- df_data %>% 
    group_by(nivel_g) %>% 
    mutate(porc = (round(n/sum(n), digits = 2))*100)

# ---- Labels 
v_title    <- "Acuerdos y convenios que les transfieren funciones civiles a las instituciones militares (2007-2022)"
v_subtitle <- "Según el nivel de gobierno y el rubro o materia de la función civil\n"

# ---- Figura 
ggplot(df_data %>% filter(nivel_g %in% c("Federal", "Estatal")),
       aes(y = reorder(nombre, porc), x = porc)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~nivel_g) +
    geom_text(aes(label = paste0(porc, "% (", n, ")"),
                  fontface = "bold"),
              color = v_colores[4],
              size = 3.5,
              vjust = .5,
              hjust = -.1,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(labels = percent_format(scale = 1),
                       limits = c(0, 90),
                       expand = c(0,0)) +
    scale_y_discrete(labels = wrap_format(35)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "qué_nivel.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 5)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.3.5. Por FFAA ------------------------------------------------------------

# ---- Vectores 
v_ffaa    <- c("Sedena", "Semar", "GN")
v_nombres <- c("Sedena", "Semar", "Guardia Nacional")

# ---- Datos 

# Base vacía
df_data <- data.frame(
    materia = as.character(),
    n = as.numeric(),
    nivel_g = as.character(),
    nombre = as.character()
)


# Datos
for (i in 1:length(v_ffaa)) {
    
    df_temp <- df_convenio             %>%
        filter(`Tipo de función` == "Civil"
               )                      %>%
        rename(ffaa = v_ffaa[i])       %>%
        filter(ffaa == 1)              %>% 
        select(c(29:35))               %>% 
        pivot_longer(cols = c(1:7),
                     names_to = "materia",
                     values_to = "n") %>% 
        group_by(materia)             %>% 
        summarise(n = sum(n))         %>% 
        mutate(
            ffaa_ = v_nombres[i],
            nombre = case_when(
                materia == "sp" ~ "Seguridad pública", 
                materia == "obra_p" ~ "Obra pública", 
                materia == "ambiente" ~ "Protección y restauración del ambiente",
                materia == "misce" ~ "Miscelánea", 
                materia == "salud" ~ "Salud", 
                materia == "p_social" ~ "Política social",
                materia == "protec_c" ~ "Protección civil", 
                materia == "puertos" ~ "Administración de puertos y aeropuertos",
                materia == "invest" ~ "Investigación académica y científica"
            )) 
    
    # Guardar base 
    df_data <- df_data %>% full_join(df_temp)
    
}

df_data_fig <- df_data %>% 
    mutate(ffaa_ = factor(ffaa_,
                          levels = c("Sedena", "Semar", "Guardia Nacional")))

# ---- Labels 
v_title    <- "¿Qué fue transferido a las instituciones militares?"
v_subtitle <- "Por institución que recibe la transferencia\n"

# ---- Figura 
ggplot(df_data_fig, aes(y = reorder(nombre, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~ffaa_) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = .5,
              hjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 90),
                       expand = c(0,0),
                       breaks = seq(0, 80, 20)) +
    scale_y_discrete(labels = wrap_format(35)) +
    tema +
    theme(
        panel.grid.major.y    = element_blank(),
        panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76")
    )

# ---- Guardar figura 

# Local
plot_name  <- "qué_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.2.6. Por función/presupuesto ---------------------------------------------

# ---- Datos 

df_data <- df_convenio %>%
    filter(`Tipo de función` == "Civil"
           )          %>%
    select(`¿Presupuesto o funciones civiles?`,
           c(29:35))   %>% 
    pivot_longer(cols = c(2:8),
                 names_to = "materia",
                 values_to = "n") %>% 
    filter(n == 1)                %>% 
    group_by(`¿Presupuesto o funciones civiles?`,
             materia)             %>% 
    summarise(n = sum(n))         %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        materia == "obra_p" ~ "Obra pública", 
        materia == "ambiente" ~ "Protección y restauración del ambiente",
        materia == "misce" ~ "Miscelánea", 
        materia == "salud" ~ "Salud", 
        materia == "p_social" ~ "Política social",
        materia == "protec_c" ~ "Protección civil", 
        materia == "puertos" ~ "Administración de puertos y aeropuertos",
        materia == "invest" ~ "Investigación académica y científica"
    ))

# ---- Labels 
v_title    <- "¿Qué fue transferido a las instituciones militares?"
v_subtitle <- "Por tipo de transferencia\n"

# ---- Figura 
ggplot(df_data, aes(y = reorder(nombre, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~`¿Presupuesto o funciones civiles?`,
               labeller=label_wrap_gen(width=20)) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = .5,
              hjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 120),
                       expand = c(0,0),
                       breaks = seq(0, 80, 20)) +
    scale_y_discrete(labels = wrap_format(30)) +
    tema +
    theme(
        panel.grid.major.y    = element_blank(),
        panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76")
    )

# ---- Guardar figura 

# Local
plot_name  <- "qué_total_función_presupuesto.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 3.4. Nivel de gobierno -----------------------------------------------------

### 3.4.1. Total de transferencias por nivel ----------------------------------

# ---- Datos
df_data <- df_convenio %>% 
    select(25:28)      %>% 
    pivot_longer(cols = c(1:4),
                 names_to = "nivel",
                 values_to = "n") %>% 
    group_by(nivel)               %>% 
    summarise(n = sum(n))         %>% 
    mutate(nivel = str_to_title(nivel))

# ---- Labels 
v_title    <- "Convenios y acuerdos que transfieren presupuesto y funciones civiles a instituciones militares"
v_subtitle <- "Por nivel de gobierno que transfiere\n"

# ---- Figura 
ggplot(df_data, aes(x = reorder(nivel, -n), y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 140),
                       expand = c(0,0)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "nivel.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.2. Total de transferencias por nivel por año ---------------------------

# ---- Datos
df_data <- df_convenio      %>% 
    select(25:28, Año)      %>% 
    pivot_longer(cols = c(1:4),
                 names_to = "nivel",
                 values_to = "n") %>% 
    group_by(nivel, Año)          %>% 
    summarise(n = sum(n))         %>% 
    mutate(nivel = str_to_title(nivel))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por nivel de gobierno que transfiere y año\n"

# ---- Figura 
ggplot(df_data %>% filter(nivel %in% c("Estatal", "Federal")),
       aes(x = Año, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~nivel) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2007,2022)) +
    scale_y_continuous(limits = c(0, 32),
                       expand = c(0,0)) +
    tema +
    theme(axis.text.x = element_text(angle = 90))

# ---- Guardar figura 

# Local
plot_name  <- "nivel_año.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.3. Total de transferencias por nivel por vigencia ----------------------

# ---- Datos
df_data <- df_periodo        %>% 
    select(17:20, year)      %>% 
    pivot_longer(cols = c(1:4),
                 names_to = "nivel",
                 values_to = "n") %>% 
    group_by(nivel, year)         %>% 
    summarise(n = sum(n))         %>% 
    mutate(nivel = str_to_title(nivel))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por nivel de gobierno que transfiere y año de vigencia\n"

# ---- Figura 
ggplot(df_data %>% filter(nivel %in% c("Estatal", "Federal")), 
       aes(x = year, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~nivel) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2002,2027)) +
    scale_y_continuous(limits = c(0, 32),
                       expand = c(0,0)) +
    tema +
    theme(axis.text.x = element_text(angle = 90))

# ---- Guardar figura 

# Local
plot_name  <- "nivel_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.4.3b. Promedio de vigencia por nivel -------------------------------------

# ---- Datos
df_data <- df_avigencia         %>% 
    select(17:20, n_times)      %>% 
    pivot_longer(cols = c(1:4),
                 names_to = "nivel",
                 values_to = "n")   %>% 
    filter(n != 0)                  %>% 
    group_by(nivel)                 %>% 
    summarise(prom = mean(n_times)) %>% 
    mutate(nivel = str_to_title(nivel))

df_data2 <- df_avigencia %>% 
    group_by(n_times)    %>% 
    count()
    # summarise(prom = mean(n_times))

### 3.2.4. Por FFAA que recibe -------------------------------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    select(Año, 25:28, 22:24)                     %>% 
    # Pivotear nivel de gobierno
    pivot_longer(cols = 2:5,
                 names_to = "nivel",
                 values_to = "n")                 %>% 
    # Pivotear ffaa
    pivot_longer(cols = 2:4,
                 names_to = "ffaa",
                 values_to = "n_ffaa")           %>% 
    # Filtrar porque son dummies
    filter(n == 1 & n_ffaa == 1)                 %>% 
    group_by(ffaa, 
             nivel) %>% 
    summarise(n = sum(n))                         %>% 
    mutate(ffaa = recode(ffaa, 
                         "GN" = "Guardia Nacional"),
           nivel = str_to_title(nivel)) %>% 
    mutate(ffaa = factor(ffaa, 
                         levels = c("Sedena", "Semar", "Guardia Nacional")))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por nivel de gobierno que transfiere e institución que la recibe\n"

# ---- Figura 
ggplot(df_data %>% filter(nivel%in% c("Estatal", "Federal")),
       aes(x = ffaa, y = n)) +
    geom_col(fill = v_colores[2]) +
    facet_wrap(~nivel,
               ncol = 2) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 115),
                       expand = c(0,0)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "nivel_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.2.4. Por FFAA que recibe y función/presupuesto ---------------------------

# ---- Datos 
df_data <- df_convenio                            %>% 
    select(Año, `¿Presupuesto o funciones civiles?`,
           25:28, 22:24)                          %>% 
    # Pivotear nivel de gobierno
    pivot_longer(cols = 3:6,
                 names_to = "nivel",
                 values_to = "n")                 %>% 
    # Pivotear ffaa
    pivot_longer(cols = 3:5,
                 names_to = "ffaa",
                 values_to = "n_ffaa")           %>% 
    # Filtrar porque son dummies
    filter(n == 1 & n_ffaa == 1)                 %>% 
    group_by(ffaa, `¿Presupuesto o funciones civiles?`,
             nivel) %>% 
    summarise(n = sum(n))                         %>% 
    mutate(ffaa = recode(ffaa, 
                         "GN" = "Guardia Nacional"),
           nivel = str_to_title(nivel)) %>% 
    mutate(ffaa = factor(ffaa, 
                         levels = c("Sedena", "Semar", "Guardia Nacional")),
           `¿Presupuesto o funciones civiles?` = factor(`¿Presupuesto o funciones civiles?`, 
                                                        levels = c(
                                                            "Función civil sin presupuesto civil",
                                                            "Función militar con presupuesto civil",
                                                            "Función civil con presupuesto civil"
                                                        )))

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por nivel de gobierno que transfiere, institución que la recibe y tipo de transferencia\n"

# ---- Figura 
ggplot(df_data, aes(x = ffaa, y = n, fill = `¿Presupuesto o funciones civiles?`)) +
    geom_col(position = position_dodge()) +
    facet_wrap(~nivel,
               ncol = 2,
               scales = "free_x") +
    geom_text(aes(label = n, group = `¿Presupuesto o funciones civiles?`,
                  fontface = "bold"),
              position = position_dodge(.9),
              color = v_colores[4],
              size = 2.5,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_fill_manual(values = v_colores_figs) +
    scale_y_continuous(limits = c(0, 110),
                       expand = c(0,0)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "nivel_ffaa_función_presupuesto.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.2.5. Dependencia federal -------------------------------------------------

df_inst <- df_convenio %>% 
    filter(federal == 1)     %>% 
    select(`¿Qué institución(es) transfiere(n)?`) %>% 
    # Recodificar problemáticas 
    mutate(`¿Qué institución(es) transfiere(n)?` = recode(`¿Qué institución(es) transfiere(n)?`,
                                                          "Secretaría de Comunicaciones y Transportes, AICM y Oficina de la Presidencia." = "Secretaría de Comunicaciones y Transportes, Aeropuerto Internacional de la Ciudad de México, Oficina de la Presidencia",
                                                          "Secretaría de Desarrollo Agrario, Territorial y Urbano" = "Secretaría de Desarrollo Agrario Territorial y Urbano",
                                                          "Secretaría de Desarrollo Agrario, Territorial y Urbano, Secretaría de Medio Ambiente y Recursos Naturales" = "Secretaría de Desarrollo Agrario Territorial y Urbano, Secretaría de Medio Ambiente y Recursos Naturales",
                                                          "Fondo Nacional de Fomento al Turismo, Fondo Nacional de Fomento al Turismo Tren Maya, Secretaría de Desarrollo Social/Bienestar, Comisión Nacional Forestal" = "Fondo Nacional de Fomento al Turismo, Fondo Nacional de Fomento al Turismo Tren Maya, Secretaría de Desarrollo Social/ Bienestar, Comisión Nacional Forestal")) %>% 
    # Separar por comas inst únicas 
    separate(
        col = `¿Qué institución(es) transfiere(n)?`,
        into = c("inst1", "inst2", "inst3", "inst4"),
        sep = ",") %>% 
    # Converir en lista 
    pivot_longer(cols = everything(),
                 names_to = "columna",
                 values_to = "institucion") %>% 
    filter(!is.na(institucion))             %>% 
    # Eliminar espacios al inicio       
    mutate(institucion = str_trim(institucion)) 

# Valores únicos 
v_instiitucion <- sort(unique(df_inst$institucion))
    
# df_dependencia <- df_convenio                %>% 
#     filter(federal == 1)                     %>% 
#     group_by(`¿Qué institución(es) transfiere(n)?`) %>%
#     count()
#     mutate(Semarnat = str_detect(`¿Qué institución(es) transfiere(n)?`, "Semarnat"),
#            AICM = str_detect(`¿Qué institución(es) transfiere(n)?`, "Aeropuerto Internacional de la Ciudad de México"),
#            AEM = str_detect(`¿Qué institución(es) transfiere(n)?`, "Agencia"),
#            BaBien = str_detect(`¿Qué institución(es) transfiere(n)?`, "Banco del"),
#            CFE = str_detect(`¿Qué institución(es) transfiere(n)?`, "CFE"),
#            CISEN = str_detect(`¿Qué institución(es) transfiere(n)?`, "CISEN"),
#            GACM = str_detect(`¿Qué institución(es) transfiere(n)?`, "GACM"),
#            CJF = str_detect(`¿Qué institución(es) transfiere(n)?`, "CJF"),
#            CONADIC = str_detect(`¿Qué institución(es) transfiere(n)?`, "CONADIC"),
#            CONAGUA =  str_detect(`¿Qué institución(es) transfiere(n)?`, "CONAGUA"),
#            CONALITEG = str_detect(`¿Qué institución(es) transfiere(n)?`, "CONALITEG"),
#            CONAGUA =  str_detect(`¿Qué institución(es) transfiere(n)?`, "CONAGUA"),
#            CONANP = str_detect(`¿Qué institución(es) transfiere(n)?`, "CONANP"),
#            Capufe = str_detect(`¿Qué institución(es) transfiere(n)?`, "Capufe"),
#            Conacyt = str_detect(`¿Qué institución(es) transfiere(n)?`, "Conacyt"),
#            Conafor = str_detect(`¿Qué institución(es) transfiere(n)?`, "Conafor"),
#            FONATUR = str_detect(`¿Qué institución(es) transfiere(n)?`, "FONATUR"),
#            IMSS = str_detect(`¿Qué institución(es) transfiere(n)?`, "IMSS"),
#            INM = str_detect(`¿Qué institución(es) transfiere(n)?`, "INM"),
#            INIDETAM = str_detect(`¿Qué institución(es) transfiere(n)?`, "Desarrollo Tecnologico de la Armada de México"),
#            `Oficina de la Presidencia` = str_detect(`¿Qué institución(es) transfiere(n)?`, "Oficina de la Presidencia"),
#            PROFEPA = str_detect(`¿Qué institución(es) transfiere(n)?`, "PROFEPA"),
#            Pemex = str_detect(`¿Qué institución(es) transfiere(n)?`, "Pemex"),
#            SALUD = str_detect(`¿Qué institución(es) transfiere(n)?`, "SALUD"),
#            SAT  = str_detect(`¿Qué institución(es) transfiere(n)?`, "SAT"),
#            SCT  = str_detect(`¿Qué institución(es) transfiere(n)?`, "SCT"),
#            SECTUR  = str_detect(`¿Qué institución(es) transfiere(n)?`, "SECTUR"),
#            SEDATU = str_detect(`¿Qué institución(es) transfiere(n)?`, "SEDATU"),
#            SRE = str_detect(`¿Qué institución(es) transfiere(n)?`, "SRE"),
#            Sader = str_detect(`¿Qué institución(es) transfiere(n)?`, "Sader"),
#            `Secretaría de Desarrollo Social/ Bienestar` = str_detect(`¿Qué institución(es) transfiere(n)?`, "Desarrollo Social")) %>%
#     select(37:65) %>% 
#     pivot_longer(everything(),
#                  names_to = "inst",
#                  values_to = "dummy")   %>% 
#     mutate(dummy = as.character(dummy)) %>% 
#     filter(dummy == TRUE)               %>% 
#     group_by(inst)                      %>% 
#     count()


df_data <- df_inst %>% 
    group_by(institucion) %>% 
    count() %>% 
    mutate(dependencia = case_when(
        n > 1 ~ institucion,
        TRUE ~ "Otros"
    )) %>% 
    group_by(dependencia) %>% 
    summarise(n =sum(n)) 

# ---- Labels 
v_title    <- "Convenios y acuerdos federales para transferir presupuesto o funciones civiles a instituciones militares (2007-2022)"
v_subtitle <- "Según la institución federal que firmó el convenio con las fuerzas armadas\n"

# ---- Figura 
ggplot(df_data, aes(y = reorder(dependencia, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_label(aes(label = n,
                   fontface = "bold"),
               color = v_colores[4],
               size = 2.5,
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "\nNúmero de convenios y acuerdos\n",
        y = "",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 23),
                       expand = c(0,0)) +
    scale_y_discrete(labels = wrap_format(52)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "dependencias_fed_completos.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 8)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.2.6. Entidad federativa --------------------------------------------------

df_dependencia <- df_convenio      %>% 
    filter(estatal == 1)           %>% 
    # group_by(`entidad federativa`) %>%
    # count()                        %>% 
    mutate(`Baja California` = ifelse(`entidad federativa` == "Baja California", TRUE, FALSE),
           `Baja California Sur` = ifelse(`entidad federativa` == "Baja California Sur", TRUE, FALSE),
           Chihuahua = str_detect(`entidad federativa`, "Chihuahua"),
           Coahuila = str_detect(`entidad federativa`, "Coahuila"),
           Durango = str_detect(`entidad federativa`, "Durango"),
           `Ciudad de México` = str_detect(`entidad federativa`, "Ciudad de México"),
           Colima = str_detect(`entidad federativa`, "Colima"),
           `Estado de México` = str_detect(`entidad federativa`, "Estado de México"),
           Guanajuato = str_detect(`entidad federativa`, "Guanajuato"),
           Guerrero = str_detect(`entidad federativa`, "Guerrero"),
           Hidalgo = str_detect(`entidad federativa`, "Hidalgo"),
           Jalisco = str_detect(`entidad federativa`, "Jalisco"),
           `Michoacán` = str_detect(`entidad federativa`, "Michoacán"),
           Morelos = str_detect(`entidad federativa`, "Morelos"),
           `Nuevo León` = str_detect(`entidad federativa`, "Nuevo León"),
           Oaxaca = str_detect(`entidad federativa`, "Oaxaca"),
           Puebla = str_detect(`entidad federativa`, "Puebla"),
           `Querétaro` = str_detect(`entidad federativa`, "Querétaro"),
           `Sinaloa` = str_detect(`entidad federativa`, "Sinaloa"),
           Sonora = str_detect(`entidad federativa`, "Sonora"),
           Tamaulipas = str_detect(`entidad federativa`, "Tamaulipas"),
           Veracruz = str_detect(`entidad federativa`, "Veracruz"),
           `Yucatán` = str_detect(`entidad federativa`, "Yucatán"),
           Zacatecas = str_detect(`entidad federativa`, "Zacatecas")) %>% 
    select(37:60) %>% 
    pivot_longer(everything(),
                 names_to = "entidad",
                 values_to = "dummy") %>% 
    mutate(dummy = as.character(dummy)) %>% 
    filter(dummy == TRUE) %>% 
    group_by(entidad) %>% 
    count()
    
    
df_data <- df_dependencia %>% 
    mutate(dependencia = case_when(
        n > 3 ~ entidad,
        TRUE ~ "Otros"
    )) %>% 
    group_by(dependencia) %>% 
    summarise(n =sum(n)) 

# ---- Labels 
v_title    <- "Convenios y acuerdos estatales para transferir presupuesto o funciones civiles a instituciones militares (2007-2022)"
v_subtitle <- "Según la entidad federativa de la cual proviene la institución civil que firmó el convenio"

# ---- Figura 
ggplot(df_data, aes(y = reorder(dependencia, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_label(aes(label = n,
                   fontface = "bold"),
               color = v_colores[4],
               size = 2.5,
               vjust = .5,
               hjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "\nNúmero de convenios y acuerdos\n",
        y = "",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 18),
                       expand = c(0,0)) +
    scale_y_discrete(labels = wrap_format(35)) +
    tema 

# ---- Guardar figura 

# Local
plot_name  <- "entidades_fed.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

# ### 3.4.1. Por motivación ------------------------------------------------------
# 
# # ---- Datos
# df_data <- df_convenio                              %>% 
#     select(`Clasificación del fundamento`, 27:30)   %>% 
#     pivot_longer(cols = c(2:5),
#                  names_to = "nivel",
#                  values_to = "n")                   %>% 
#     filter(n == 1)                                  %>% 
#     group_by(nivel, `Clasificación del fundamento`) %>% 
#     summarise(n = sum(n))                           %>% 
#     mutate(nivel = str_to_title(nivel))
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por nivel de gobierno que transfiere y tipo de fundamento\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(x = reorder(`Clasificación del fundamento`, n), y = n)) +
#     geom_col(fill = v_colores[2]) +
#     facet_wrap(~nivel) +
#     geom_text(aes(label = n,
#                   fontface = "bold"),
#               color = v_colores[4],
#               size = 2.5,
#               vjust = .5,
#               hjust = -.2,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "\nNúmero de convenios y acuerdos",
#         caption = v_caption
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_y_continuous(limits = c(0, 60),
#                        expand = c(0,0)) +
#     tema +
#     coord_flip() +
#     theme(panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76"),
#           panel.grid.major.y    = element_blank(),
#           panel.grid.minor.y    = element_blank(),
#           panel.grid.minor.x    = element_blank()
#     )
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "nivel_motivación.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 10)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# ### 3.4.1. Por motivación y función/presupuesto --------------------------------
# 
# # ---- Datos
# df_data <- df_convenio                              %>% 
#     select(`¿Presupuesto o funciones civiles?`,
#            `Clasificación del fundamento`, 27:30)   %>% 
#     pivot_longer(cols = c(3:6),
#                  names_to = "nivel",
#                  values_to = "n")                   %>% 
#     filter(n == 1)                                  %>% 
#     group_by(`¿Presupuesto o funciones civiles?`,
#              nivel, `Clasificación del fundamento`) %>% 
#     summarise(n = sum(n))                           %>% 
#     mutate(nivel = str_to_title(nivel))             %>% 
#     filter(nivel %in% c("Estatal", "Federal"))
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por nivel de gobierno que transfiere, tipo de fundamento y tipo de transferencia\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(x = reorder(`Clasificación del fundamento`, n), y = n, fill = `¿Presupuesto o funciones civiles?`)) +
#     geom_col(position = position_dodge2()) +
#     facet_wrap(~nivel) +
#     geom_text(aes(label = n, group = `¿Presupuesto o funciones civiles?`,
#                   fontface = "bold"),
#               position = position_dodge2(.9),
#               color = v_colores[4],
#               size = 2,
#               vjust = .5,
#               hjust = -.2,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "\nNúmero de convenios y acuerdos",
#         caption = v_caption,
#         fill = ""
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_y_continuous(limits = c(0, 55),
#                        expand = c(0,0)) +
#     tema +
#     coord_flip() +
#     scale_fill_manual(values = v_colores_figs) +
#     theme(panel.grid.major.x    = element_line(linetype = 1, color = "#7A7E76"),
#           panel.grid.major.y    = element_blank(),
#           panel.grid.minor.y    = element_blank(),
#           panel.grid.minor.x    = element_blank()
#     )
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "nivel_motivación_función_presupuesto.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 8)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

# ## 3.5. Vehículo ---------------------------------------------------------------
# 
# ### 3.5.1. Vehículo total ------------------------------------------------------
# 
# # ---- Datos 
# df_data <- df_convenio                       %>% 
#     group_by(`Vehículo de la transferencia`) %>% 
#     count()
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por vehículo de transferencia\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(x = reorder(`Vehículo de la transferencia`, -n), y = n)) +
#     geom_col(fill = v_colores[2]) +
#     geom_text(aes(label = n,
#                   fontface = "bold"),
#               color = v_colores[4],
#               size = 2.5,
#               vjust = -.2,
#               hjust = .5,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "Número de convenios y acuerdos\n",
#         caption = v_caption
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_y_continuous(limits = c(0, 255),
#                        expand = c(0,0)) +
#     tema +
#     scale_x_discrete(labels = wrap_format(25))
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "vehículo.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 6)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# ### 3.5.2. Vehículo por año ----------------------------------------------------
# 
# # ---- Datos 
# df_data <- df_convenio                            %>% 
#     group_by(`Vehículo de la transferencia`, Año) %>% 
#     count()
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por vehículo de transferencia y año\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(x = Año, y = n)) +
#     geom_col(fill = v_colores[2]) +
#     facet_wrap(~`Vehículo de la transferencia`) +
#     geom_text(aes(label = n,
#                   fontface = "bold"),
#               color = v_colores[4],
#               size = 2.5,
#               vjust = -.2,
#               hjust = .5,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "Número de convenios y acuerdos\n",
#         caption = v_caption
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_y_continuous(limits = c(0, 42),
#                        expand = c(0,0)) +
#     scale_x_continuous(breaks = c(2002:2023)) +
#     tema +
#     theme(
#         axis.text.x = element_text(angle = 90)
#     )
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "vehículo_año.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 6)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# ### 3.5.2. Vehículo por vigencia -----------------------------------------------
# 
# # ---- Datos 
# df_data <- df_periodo                              %>% 
#     group_by(`Vehículo de la transferencia`, year) %>% 
#     count()
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por vehículo de transferencia y año de vigencia\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(x = year, y = n)) +
#     geom_col(fill = v_colores[2]) +
#     facet_wrap(~`Vehículo de la transferencia`) +
#     geom_text(aes(label = n,
#                   fontface = "bold"),
#               color = v_colores[4],
#               size = 2.5,
#               vjust = -.2,
#               hjust = .5,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "Número de convenios y acuerdos\n",
#         caption = v_caption
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_y_continuous(limits = c(0, 45),
#                        expand = c(0,0)) +
#     scale_x_continuous(breaks = c(2002:2027)) +
#     tema +
#     theme(
#         axis.text.x = element_text(angle = 90)
#     )
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "vehículo_vigencia.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 10, height = 6)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# ## 3.6. Motivación -------------------------------------------------------------
# 
# ### 3.6.1. Motivación total ----------------------------------------------------
# 
# # ---- Datos 
# df_data <- df_convenio                       %>% 
#     group_by(`Clasificación del fundamento`) %>% 
#     count()
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por fundamento\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(y = reorder(`Clasificación del fundamento`, n), x = n)) +
#     geom_col(fill = v_colores[2]) +
#     geom_text(aes(label = n,
#                   fontface = "bold"),
#               color = v_colores[4],
#               size = 2.5,
#               vjust = .5,
#               hjust = -.2,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         y = "",
#         x = "Número de convenios y acuerdos\n",
#         caption = v_caption
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_x_continuous(limits = c(0, 62),
#                        expand = c(0,0)) +
#     tema +
#     scale_y_discrete(labels = wrap_format(25))
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "motivación.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 7.5)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# ### 3.5.2. Motivación por año ----------------------------------------------------
# 
# # ---- Datos 
# df_data <- df_convenio                            %>% 
#     group_by(`Clasificación del fundamento`, Año) %>% 
#     count()
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por fundamento y año\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(x = Año, y = n)) +
#     geom_col(fill = v_colores[2]) +
#     facet_wrap(~`Clasificación del fundamento`,
#                labeller=label_wrap_gen(width=25)) +
#     geom_text(aes(label = n,
#                   fontface = "bold"),
#               color = v_colores[4],
#               size = 2,
#               vjust = 0.5,
#               hjust = -.1,
#               angle = 90,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "Número de convenios y acuerdos\n",
#         caption = v_caption
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_y_continuous(limits = c(0, 12),
#                        expand = c(0,0)) +
#     scale_x_continuous(breaks = c(2002:2023)) +
#     tema +
#     theme(
#         axis.text.x = element_text(angle = 90)
#     )
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "motivación_año.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 10, height = 8)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# ### 3.5.2. Motivación por vigencia -----------------------------------------------
# 
# # ---- Datos 
# df_data <- df_periodo                              %>% 
#     group_by(`Clasificación del fundamento`, year) %>% 
#     count()
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por fundamento y año de vigencia\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(x = year, y = n)) +
#     geom_col(fill = v_colores[2]) +
#     facet_wrap(~`Clasificación del fundamento`) +
#     geom_text(aes(label = n,
#                   fontface = "bold"),
#               color = v_colores[4],
#               size = 2,
#               angle = 90,
#               vjust = .5,
#               hjust = -.2,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         x = "",
#         y = "Número de convenios y acuerdos\n",
#         caption = v_caption
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_y_continuous(limits = c(0, 15),
#                        expand = c(0,0)) +
#     scale_x_continuous(breaks = c(2002:2027)) +
#     tema +
#     theme(
#         axis.text.x = element_text(angle = 90)
#     )
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "motivación_viigencia.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 12, height = 8)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# ### 3.6.1. Motivación total ----------------------------------------------------
# 
# # ---- Datos 
# df_data <- df_convenio                            %>% 
#     group_by(`Clasificación del fundamento`,
#              `¿Presupuesto o funciones civiles?`) %>% 
#     count()
# 
# # ---- Labels 
# v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las Fuerzas Armadas"
# v_subtitle <- "Por fundamento\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(y = reorder(`Clasificación del fundamento`, n), x = n)) +
#     geom_col(fill = v_colores[2]) +
#     facet_wrap(~`¿Presupuesto o funciones civiles?`) +
#     geom_text(aes(label = n,
#                   fontface = "bold"),
#               color = v_colores[4],
#               size = 2.5,
#               vjust = .5,
#               hjust = -.2,
#               family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         y = "",
#         x = "Número de convenios y acuerdos\n",
#         caption = v_caption
#     ) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_x_continuous(limits = c(0, 62),
#                        expand = c(0,0)) +
#     tema +
#     scale_y_discrete(labels = wrap_format(25))
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "motivación_función_presupuesto.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 10)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 3.7. Vigencia ---------------------------------------------------------------

### 3.7.1. Convenios sin vigencia ----------------------------------------------

# ---- Datos
df_data <- df_convenio                                               %>% 
    filter(`Periodo de vigencia` %in% c("Sin información",
                                        "Sin fecha de conclusión")) %>% 
    group_by(`Periodo de vigencia`)                                  %>% 
    count()

# ---- Labels 
v_title <- "Número de convenios o acuerdos que transfieren funciones o presupuestos civiles a las instituciones militares que no especifican vigencia"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(x = reorder(`Periodo de vigencia`, n), y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              hjust = .5,
              vjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 53),
                       expand = c(0,0)) +
    tema +
    scale_x_discrete(labels = wrap_format(25))

# ---- Guardar figura 

# Local
plot_name  <- "no_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.7.2. Convenios sin vigencia por nivel ------------------------------------

# ---- Datos
df_data <- df_convenio                                               %>% 
    filter(`Periodo de vigencia` %in% c("Sin información",
                                        "Sin fecha de conclusión"))  %>%
    select(`Periodo de vigencia`, 24:28)                             %>% 
    pivot_longer(cols = c(2:5),
                 names_to = "nivel",
                 values_to = "n")                                    %>% 
    filter(n != 0)                                                   %>% 
    group_by(`Periodo de vigencia`, nivel)                           %>% 
    summarise(n = sum(n))                                            %>% 
    mutate(nivel = str_to_title(nivel))

# ---- Labels 
v_title <- "Número de convenios o acuerdos que transfieren funciones o presupuestos civiles a las instituciones militares que no especifican vigencia"
v_subtitle <- "Por nivel de gobierno que transfiere\n"

# ---- Figura 
ggplot(df_data, aes(x = nivel, y = n, fill = `Periodo de vigencia`)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = `Periodo de vigencia`,
                   fontface = "bold"),
               color = v_colores[4],
               fill = "white",
               position = position_stack(),
               size = 2,
               hjust = .5,
               vjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    scale_fill_manual(values = v_colores_fig) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 50),
                       expand = c(0,0)) +
    tema +
    scale_x_discrete(labels = wrap_format(25))

# ---- Guardar figura 

# Local
plot_name  <- "no_vigencia_nivel.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.7.2. Convenios sin vigencia por ffaa ------------------------------------

# ---- Datos
df_data <- df_convenio                                               %>% 
    filter(`Periodo de vigencia` %in% c("Sin información",
                                        "Sin fecha de conclusión"))  %>%
    select(`Periodo de vigencia`, 22:24)                             %>% 
    pivot_longer(cols = c(2:4),
                 names_to = "ffaa",
                 values_to = "n")                                    %>% 
    filter(n != 0)                                                   %>% 
    group_by(`Periodo de vigencia`, ffaa)                            %>% 
    summarise(n = sum(n))

# ---- Labels 
v_title <- "Número de convenios o acuerdos que transfieren funciones o presupuestos civiles a las instituciones militares que no especifican vigencia"
v_subtitle <- "Por Fuerza Armada que recibe la transferencia\n"

# ---- Figura 
ggplot(df_data, aes(x = reorder(ffaa, -n), y = n, fill = `Periodo de vigencia`)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = `Periodo de vigencia`,
                   fontface = "bold"),
               color = v_colores[4],
               fill = "white",
               position = position_stack(),
               size = 2.5,
               hjust = .5,
               vjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    scale_fill_manual(values = v_colores_fig) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 75),
                       expand = c(0,0)) +
    tema +
    scale_x_discrete(labels = wrap_format(25))

# ---- Guardar figura 

# Local
plot_name  <- "no_vigencia_ffaa.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.7.1. Convenios sin vigencia por año --------------------------------------

# ---- Datos
df_data <- df_convenio                                              %>% 
    filter(`Periodo de vigencia` %in% c("Sin información",
                                        "Sin fecha de conclusión")) %>% 
    group_by(`Periodo de vigencia`, Año)                            %>% 
    count()

# ---- Labels 
v_title <- "Número de convenios o acuerdos que transfieren funciones o presupuestos civiles a las instituciones militares que no especifican vigencia"
v_subtitle <- "Por año"

# ---- Figura 
ggplot(df_data, aes(x = Año, y = n, fill = `Periodo de vigencia`)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = `Periodo de vigencia`,
                   fontface = "bold"),
               color = v_colores[4],
               position = position_stack(),
               fill = "white",
               size = 2.5,
               hjust = .5,
               vjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = "",
    ) +
    scale_fill_manual(values = v_colores_fig) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 15),
                       expand = c(0,0)) +
    tema +
    theme(
        axis.text.x = element_text(angle = 90, vjust = .5) 
    ) +
    scale_x_continuous(breaks = c(2007:2022))


# ---- Guardar figura 

# Local
plot_name  <- "no_vigencia_año.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

### 3.7.4. Convenios sin vigencia por sexenio ----------------------------------

# % de cada sexenio 

# ---- Datos
df_data <- df_convenio                                               %>% 
    mutate(vigencia_estipulada = case_when(
        `Periodo de vigencia` %in% c("Sin información","Sin fecha de conclusión")~ "No",
        TRUE ~ "Sí"))                                                %>%
    group_by(vigencia_estipulada, sexenio)                           %>% 
    count()                                                          %>% 
    group_by(sexenio)                                                %>% 
    mutate(porc = n/sum(n))

# % de los sin vigencia que cada sexenio tien 

df_data <- df_convenio                                               %>% 
    filter(
        `Periodo de vigencia` %in% c("Sin información",
                                     "Sin fecha de conclusión"))     %>%
    group_by(`Periodo de vigencia`)                                                %>% 
    count()                                                          %>% 
    ungroup %>% 
    mutate(porc = n/sum(n))

# ---- Labels 
v_title <- "Número de convenios o acuerdos que transfieren funciones o presupuestos civiles a las instituciones militares que no especifican vigencia"
v_subtitle <- "Por nivel de gobierno que transfiere\n"

# ---- Figura 
ggplot(df_data, aes(x = nivel, y = n, fill = `Periodo de vigencia`)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = n, group = `Periodo de vigencia`,
                   fontface = "bold"),
               color = v_colores[4],
               fill = "white",
               position = position_stack(),
               size = 2,
               hjust = .5,
               vjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    scale_fill_manual(values = v_colores_fig) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_y_continuous(limits = c(0, 50),
                       expand = c(0,0)) +
    tema +
    scale_x_discrete(labels = wrap_format(25))

# ---- Guardar figura 

# Local
plot_name  <- "no_vigencia_nivel.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 7)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

# ### 3.7.1. Convenios sin vigencia por motivación -------------------------------
# 
# # ---- Datos
# df_data <- df_convenio                                              %>% 
#     filter(`Periodo de vigencia` %in% c("Sin información",
#                                         "Sin fecha de conclusión")) %>% 
#     group_by(`Periodo de vigencia`, `Clasificación del fundamento`) %>% 
#     count()
# 
# # ---- Labels 
# v_title <- "Número de convenios o acuerdos que transfieren funciones o presupuestos civiles a las Fuerzas Armadas que no especifican vigencia"
# v_subtitle <- "Por clasificación del fundamento\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(y = `Clasificación del fundamento`, x = n, fill = `Periodo de vigencia`)) +
#     geom_col(position = position_stack()) +
#     geom_label(aes(label = n, group = `Periodo de vigencia`,
#                    fontface = "bold"),
#                color = v_colores[4],
#                position = position_stack(),
#                fill = "white",
#                size = 2.5,
#                hjust = .5,
#                vjust = .5,
#                family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         y = "",
#         x = "Número de convenios y acuerdos\n",
#         caption = v_caption,
#         fill = "",
#     ) +
#     scale_fill_manual(values = v_colores_fig) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_x_continuous(limits = c(0, 19),
#                        expand = c(0,0)) +
#     tema +
#     scale_y_discrete(labels = wrap_format(25))
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "no_vigencia_fundamento.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 6)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)
# 
# ### 3.7.1. Vigencia por motivación ---------------------------------------------
# 
# # ---- Datos
# df_data <- df_convenio                                              %>% 
#     mutate(tipo_vigencia = case_when(
#         `Periodo de vigencia` %in% c("Sin información",
#                                      "Sin fecha de conclusión") ~ "Sin especificar",
#         TRUE ~ "Especificada"
#     )) %>% 
#     group_by(tipo_vigencia, `Clasificación del fundamento`) %>% 
#     count()
# 
# # ---- Labels 
# v_title <- "Número de convenios o acuerdos que transfieren funciones o presupuestos civiles a las Fuerzas Armadas"
# v_subtitle <- "Por clasificación del fundamento, según vigencia\n"
# 
# # ---- Figura 
# ggplot(df_data, aes(y = reorder(`Clasificación del fundamento`, n), x = n, fill = tipo_vigencia)) +
#     geom_col(position = position_stack()) +
#     geom_label(aes(label = n, group = tipo_vigencia,
#                    fontface = "bold"),
#                color = v_colores[4],
#                position = position_stack(),
#                fill = "white",
#                size = 2.2,
#                hjust = .5,
#                vjust = .5,
#                family = "Fira Sans") +
#     labs(
#         # title = v_title,
#         subtitle = v_subtitle,
#         y = "",
#         x = "Número de convenios y acuerdos\n",
#         caption = v_caption,
#         fill = "",
#     ) +
#     scale_fill_manual(values = v_colores_fig) +
#     ggtitle(wrapper(v_title, width = 82)) +
#     scale_x_continuous(limits = c(0, 60),
#                        expand = c(0,0)) +
#     tema +
#     scale_y_discrete(labels = wrap_format(30))
# 
# # ---- Guardar figura 
# 
# # Local
# plot_name  <- "tipo_vigencia_fundamento.png"
# path_local <- paste_fig(plot_name)
# 
# ggsave(path_local,
#        device = "png", type = "cairo", # para guardar fuentes
#        width = 8, height = 8.5)
# 
# # Drive
# drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 3.7.2. Número de convenios por vigencia -------------------------------------

# ---- Datos 
df_data <- df_periodo %>% 
    group_by(year)    %>% 
    count()           %>% 
    filter(year > 2006 & year < 2023)

# ---- Labels 
v_title    <- "Número de convenios y acuerdos que transfieren presupuesto y/o funciones civiles a las instituciones militares"
v_subtitle <- "Por año de vigencia\n"

# ---- Figura 
ggplot(df_data, aes(x = year, y = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = -.2,
              hjust = .5,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2007, 2022),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 45),
                       expand = c(0,0)) +
    tema

# ---- Guardar figura 

# Local
plot_name  <- "total_convenio_vigencia.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)

## 3.8. Sexenio ----------------------------------------------------------------

df_data <- df_convenio %>% 
    filter(federal == 1) %>% 
    group_by(sexenio)  %>% 
    count()

df_data <- df_convenio      %>%
    filter(#`Tipo de función` == "Civil",
           federal == 1
    ) %>%
    select(c(29:35), sexenio) %>% 
    pivot_longer(cols = c(1:7),
                 names_to = "materia",
                 values_to = "n") %>% 
    filter(n == 1)                %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        TRUE ~ "Otra"
    ))                            %>% 
    group_by(nombre, sexenio)     %>% 
    summarise(n = sum(n))         %>% 
    group_by(sexenio)             %>% 
    mutate(porc = n/sum(n))       %>% 
    mutate(sexenio = factor(sexenio,levels = 
                                c("Calderón",
                                  "Peña Nieto",
                                  "López Obrador")),
           nombre = factor(nombre, levels = c(
               "Seguridad pública", "Otra"
           )))

# ---- Labels 
v_title <- "Acuerdos y convenios del gobierno federal que le transfirieron funciones civiles a instituciones militares"
v_subtitle <- "Por sexenio, según si la función es de seguridad pública o no\n"

# ---- Figura 
ggplot(df_data, aes(x = sexenio, y = porc, fill = nombre)) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = scales::percent(porc), group = nombre,
                   fontface = "bold"),
               color = v_colores[4],
               fill = "white",
               position = position_stack(),
               size = 2.5,
               hjust = .5,
               vjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    scale_fill_manual(values = v_colores_fig) +
    ggtitle(wrapper(v_title, width = 90)) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.1),
                       expand = c(0,0)) +
    tema +
    scale_x_discrete(labels = wrap_format(25))

# ---- Guardar figura 

# Local
plot_name  <- "sexenio_sp_porc.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)


beepr::beep()

## 3.9. CDMX -------------------------------------------------------------------

### 3.9.1. Funciones transferidas a cdmx ---------------------------------------

# ---- Datos 

df_data <- df_convenio %>%
    filter(`Tipo de función` == "Civil",
           `entidad federativa` == "Ciudad de México") %>% 
    select(c(29:35))   %>% 
    pivot_longer(cols = everything(),
                 names_to = "materia",
                 values_to = "n") %>% 
    group_by(materia)             %>% 
    summarise(n = sum(n))         %>% 
    mutate(nombre = case_when(
        materia == "sp" ~ "Seguridad pública", 
        materia == "obra_p" ~ "Obra pública", 
        materia == "ambiente" ~ "Protección y restauración del ambiente",
        materia == "misce" ~ "Miscelánea", 
        materia == "salud" ~ "Salud", 
        materia == "p_social" ~ "Política social",
        materia == "protec_c" ~ "Protección civil", 
        materia == "puertos" ~ "Administración de puertos y aeropuertos",
        materia == "invest" ~ "Investigación académica y científica"
    ))                           

df_data <-  df_convenio %>% 
    filter(`entidad federativa` == "Ciudad de México",
           `¿Presupuesto civil?` == "Sí") %>% 
    count()

# ---- Labels 
v_title    <- "¿Qué funciones civiles fueron trasnferidas a las instituciones militares?"
v_subtitle <- ""

# ---- Figura 
ggplot(df_data, aes(y = reorder(nombre, n), x = n)) +
    geom_col(fill = v_colores[2]) +
    geom_text(aes(label = n,
                  fontface = "bold"),
              color = v_colores[4],
              size = 3,
              vjust = .5,
              hjust = -.2,
              family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        y = "",
        x = "\nNúmero de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(limits = c(0, 120),
                       expand = c(0,0),
                       breaks = seq(0, 100, 20)) +
    scale_y_discrete(labels = wrap_format(30)) +
    tema

# 4. Dineros -------------------------------------------------------------------

## 4.1. Base a deflactar -------------------------------------------------------

df_prespuesto <- df_convenio                   %>% 
    mutate(dineros_limpio =  gsub("\\$", '', `Presupuesto transferido`)) %>% 
    mutate(dineros_limpio =  gsub("\\,", '', dineros_limpio))            %>%
    mutate(dineros_limpio =  gsub(" ", '', dineros_limpio))              %>%
    mutate(dineros = as.numeric(dineros_limpio))

## 4.2. Cargar datos Inegi -----------------------------------------------------

# API INEGI: https://www.inegi.org.mx/servicios/api_indicadores.html

# Banco de Información Económica (BIE) > Cuentas nacionales > 
# Producto interno bruto trimestral, base 2013 > 
# Índice acumulado de precios implícitos, 2013=100.0 > 
# Producto Interno Bruto, a precios de mercado 


v_token <- "8ce3133c-72af-797a-dacf-442646c9de83" # Token personal
v_id    <- "495808" # 2013

# Importar los datos desde la API
df_pib_raw <- inegi_series(serie     = v_id,
                           token    = v_token,
                           database = "BIE",
                           as_tt    = TRUE) 

# Deflactor = (índice del año base con relación a la base en 2013/índice año de interés en relación a 2013)
# Valor deflactado = (valor corriente/deflactor)*100

# Limpiar el índice de precios con base en los precios de 2013
df_indices_2013 <- df_pib_raw                 %>% 
    # Crear variables de año y trimestre con formato de fecha
    mutate(
        year = lubridate::year(date), 
        trim = zoo::as.yearqtr(date))             %>% 
    # Dejar solo últimos trimestres de cada año (4° en su mayoría)
    group_by(year)                              %>% 
    filter(trim == max(trim))                   %>% 
    select(date, year, trim, index_base = values)

# Obtener índice de precios para el periodo más reciente registrado 
v_index_last <- df_indices_2013$index_base[df_indices_2013$year == max(df_indices_2013$year)]

## 4.3. Deflactar --------------------------------------------------------------

# ---- Mi base de dineros 

# # Unir índices de precios con base de cuenta pública 
# df_corriente <- df_prespuesto                     %>% 
#     mutate(
#         year       = as.numeric(Año), 
#         index_last = v_index_last, 
#         cifras = "Pesos corrientes de cada año")  %>% 
#     left_join(df_indices_2013, by = "year")       %>% 
#     select(-c(date, trim))
# 
# # Deflactar cifras 
# df_constante <- df_corriente                     %>% 
#     mutate(
#         deflactor = (index_base/index_last)*100) %>% 
#     mutate(
#         deflactado = (p_corrientes/deflactor)*100, 
#         cifras = paste0("Pesos constantes de ", max(unique(df_indices_2013$year))))
# 
# # Control de calidad 
# unique(df_constante$deflactor)

# max(df_corriente$p_corrientes, na.rm = T) # Con valores corrientes
# max(df_constante$deflactado, na.rm = T) # Con valores de 2023

# ---- Toda la base de convenios 

df_convenio_c <- df_prespuesto                   %>% 
    mutate(
        year       = as.numeric(Año), 
        index_last = v_index_last, 
        cifras = "Pesos corrientes de cada año")  %>% 
    left_join(df_indices_2013, by = "year")       %>% 
    select(-c(date, trim))   

# Deflactar 
df_convenio_d <- df_convenio_c                   %>% 
    mutate(
        deflactor = (index_base/index_last)*100) %>% 
    mutate(
        p_deflactado = round((dineros/deflactor)*100), 
        cifras = paste0("Pesos constantes de ", max(unique(df_indices_2013$year))))

# ---- Exporta a excel 

write_xlsx(list(`Presupuesto deflactado` = df_convenio_d),
           paste_tab("02_convenios_montos.xlsx"))

## 4.3. Figuras y numeritos ----------------------------------------------------

# ---- Datos 
df_data <- df_convenio_d         %>% 
    filter(!is.na(p_deflactado)) %>% 
    group_by(year)               %>% 
    summarise(pesos = sum(p_deflactado)) %>% 
    mutate(m_pesos = round(pesos/1000000))

# ---- Labels 
v_title    <- "Presupuesto civil transferido a instituciones militares a través de acuerdos y convenios con instituciones civiles"
v_subtitle <- "Por año\n"

# ---- Figura 
ggplot(df_data, aes(x = year, y = m_pesos)) +
    geom_col(fill = v_colores[2]) +
    geom_label(aes(label = comma(m_pesos),
                   y = m_pesos+200,
                   fontface = "bold"),
               color = v_colores[4],
               size = 2.5,
               vjust = .5,
               hjust = .5,
               family = "Henriette") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "En millones de pesos de 2023\n",
        caption = v_caption
    ) +
    ggtitle(wrapper(v_title, width = 82)) +
    scale_x_continuous(breaks = seq(2007, 2022),
                       expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 12300),
                       labels = comma_format(),
                       expand = c(0,0)) +
    tema

# ---- Guardar figura

# Local
plot_name  <- "presupuesto_año_deflactado.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)


### SEXENIO REVISIÓN -----------------------------------------------------------

df_data <- df_convenio %>% 
    # Nivel federal 
    filter(federal == 1) %>% 
    # Filtrar función civil 
    # filter(`Tipo de función` == "Civil") %>% 
    # SP vs otra 
    mutate(nombre = case_when(
        `¿Qué fue transferido a las instituciones militares?` == "Seguridad pública" ~ "Seguridad pública",
        TRUE ~ "Otra"
    )) %>% 
    group_by(sexenio, nombre) %>% 
    count() %>% 
    group_by(sexenio) %>% 
    mutate(porc = n/sum(n)) %>% 
    mutate(sexenio = factor(sexenio, levels = c(
        "Calderón", "Peña Nieto", "López Obrador"
    )))


# ---- Labels 
v_title <- "Acuerdos y convenios del gobierno federal que le transfirieron funciones civiles a instituciones militares"
v_subtitle <- "Por sexenio, según si la función es de seguridad pública o no\n"

# ---- Figura 
ggplot(df_data, aes(x = sexenio, y = porc, fill = reorder(nombre, porc))) +
    geom_col(position = position_stack()) +
    geom_label(aes(label = scales::percent(porc), group = reorder(nombre, porc),
                   fontface = "bold"),
               color = v_colores[4],
               fill = "white",
               position = position_stack(),
               size = 2.5,
               hjust = .5,
               vjust = .5,
               family = "Fira Sans") +
    labs(
        # title = v_title,
        subtitle = v_subtitle,
        x = "",
        y = "Número de convenios y acuerdos\n",
        caption = v_caption,
        fill = ""
    ) +
    scale_fill_manual(values = v_colores_fig) +
    ggtitle(wrapper(v_title, width = 90)) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1.1),
                       expand = c(0,0)) +
    tema +
    scale_x_discrete(labels = wrap_format(25))

# ---- Guardar figura 

# Local
plot_name  <- "sexenio_sp_porc.png"
path_local <- paste_fig(plot_name)

ggsave(path_local,
       device = "png", type = "cairo", # para guardar fuentes
       width = 8, height = 6)

# Drive
drive_upload(media = path_local, path = path_drive, name = plot_name, overwrite = TRUE)


# FIN. -------------------------------------------------------------------------