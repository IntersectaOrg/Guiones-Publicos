#------------------------------------------------------------------------------#
# Proyecto:                   Censos poblacionales del INEGI
# Objetivo:                   Clasificar las familias en México
#
# Encargadas:                 Fernanda Torres
# Correo:                     fernanda.torres@alumnos.cide.edu
# Fecha de creación:          27 de junio de 2022
# Última actualización:       27 de junio de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(tidyverse, beepr, readr, srvyr, survey)

# Limpiar espacio de trabajo 
rm(list=ls())

# Se define el método para el caso de 1 UPM en los estratos
options(survey.lonely.psu="adjust")

# Funciones para directorios dentro del proyecto 
paste_code  <- function(x){paste0("01_códigos/"                      , x)}
paste_inp   <- function(x){paste0("00_datos/"                        , x)}
paste_data  <- function(x){paste0("03_datos_limpios/"                , x)}
paste_figs  <- function(x){paste0("04_figuras/"                      , x)}


# 1. Cargar datos --------------------------------------------------------------

load(paste_data("df_viviendas_info.RData"))

## 2. Clasificación de las viviendas -----------------------------------------

df_tipo_familias <- df_viviendas_info %>% 
    # Tipo de jefatura
    mutate(jefatura = case_when(
        # Mujer sola 
        (sexo_jefx == "Mujer" & n_pareja == 0 & n_parientes == 0 & n_noparientes == 0 & n_noid == 0) ~ "Mujer sola",
        # Hombre solo
        (sexo_jefx == "Hombre" & n_pareja == 0 & n_parientes == 0 & n_noparientes == 0 & n_noid == 0) ~ "Hombre solo",
        # Hombre y mujer
        (sexo_jefx == "Mujer" & sexo_pareja == "Hombre" |
             sexo_jefx == "Hombre" & sexo_pareja == "Mujer") ~ "Mujer + hombre",
        # Mujer y mujer 
        (sexo_jefx == "Mujer" & sexo_pareja == "Mujer") ~ "Mujer + mujer",
        # Hombre y hombre
        (sexo_jefx == "Hombre" & sexo_pareja == "Hombre") ~ "Hombre + hombre",
        # No monógama 
        (n_pareja >= 2) ~ "No monógama",
        # Parientes 
        (n_pareja == 0 & n_parientes >= 1) ~ "Parientes",
        # Compañerxs de casa
        ((n_pareja == 0 & n_parientes == 0 & n_noparientes >= 1) |
             (n_pareja == 0 & n_parientes == 0 & n_noid >= 1)) ~ "Compañerxs de casa"
    ),
    # Hijxs 
    hijxs = case_when(
        n_hijx != 0 ~ "Sí",
        n_hijx == 0 ~ "No"),
    # Situación conyugal 
    `Situación conyugal` = case_when(
        (n_pareja == 1 & casadx == 1) ~ "Casadxs", 
        (n_pareja == 1 & casadx == 0) ~ "Unión libre",
        TRUE ~ "No aplica"
    ))

# 3. Diseño de encuesta ---------------------------------------------------

df_encuesta <- df_tipo_familias %>% 
    # Eliminar "-" de ESTRATO
    mutate(ESTRATO = gsub("-", "", ESTRATO, fixed = T)) %>% 
    # Convertir a numéricos
    mutate(ESTRATO = as.numeric(ESTRATO),
           UPM     = as.numeric(UPM),
           FACTOR  = as.numeric(FACTOR)) %>% 
    # Ordenar 
    arrange(ESTRATO, UPM) %>% 
    # Aplicar diseño de encuesta
    as_survey_design_(
        ids = ~UPM, 
        strata = ~ESTRATO,
        weights = ~FACTOR,
        nest = TRUE) 

# 4. Número de familias por tipo -----------------------------------------------

# 4.1. Familias por entidad ----------------------------------------------------

# Proporciones para entidad 1
df_data <- df_encuesta %>% 
    filter(entidad == 1)

df_tabla <- df_data %>% 
    srvyr::group_by(entidad, jefatura, hijxs, `Situación conyugal`)  %>%
    srvyr::survey_count()                                            %>% 
    srvyr::ungroup()                                                 %>% 
    srvyr::mutate(porcentaje = n/sum(n))

# Proporciones para el resto de las entidades
for(i in 2:32){
    print(paste("Vuelta", i, "de", 32))
    
    # Filtrar por entidad
    df_data <- df_encuesta %>% 
        filter(entidad == i)
    
    # Estimación de totales y proporciones
    df_tabla_loop <- df_data                                             %>%
        srvyr::group_by(entidad, jefatura, hijxs, `Situación conyugal`)  %>%
        srvyr::survey_count()                                            %>% 
        srvyr::ungroup()                                                 %>% 
        srvyr::mutate(porcentaje = n/sum(n))
    
    df_tabla <- df_tabla %>% bind_rows(df_tabla_loop)
}

# Renombrar
df_familias_tipos_entidades <- df_tabla 

# 4.2. Familias del país -------------------------------------------------------

df_tabla_nacional <- df_familias_tipos_entidades    %>% 
    group_by(jefatura, hijxs, `Situación conyugal`) %>% 
    summarise(total = sum(n))                       %>% 
    ungroup()                                       %>% 
    mutate(porcentaje = total/sum(total))

# Renombrar
df_familias_tipos_nacional <- df_tabla_nacional

# 5. Guardar datos -------------------------------------------------------------

# Información por entidad
save(df_familias_tipos_entidades, 
     file = paste_data("df_familias_tipos_entidades.RData"))

# Información del país
save(df_familias_tipos_nacional, 
     file = paste_data("df_familias_tipos_nacional.RData"))

# FIN. -------------------------------------------------------------------------
