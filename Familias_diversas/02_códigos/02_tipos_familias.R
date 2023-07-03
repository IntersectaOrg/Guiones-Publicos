#------------------------------------------------------------------------------#
# Proyecto:                   Censos poblacionales del INEGI
# Objetivo:                   Obtener la composición de las familias en México
#                             por entidad.
#
# Encargadas:                 Fernanda Torres
# Correo:                     fernanda.torres@alumnos.cide.edu
# Fecha de creación:          16 de junio de 2022
# Última actualización:       14 de julio de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(tidyverse, beepr, readr, srvyr, survey, data.table)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones para directorios dentro del proyecto 
paste_data  <- function(x){paste0("01_datos/"                        , x)}
paste_code  <- function(x){paste0("02_códigos/"                      , x)}
paste_figs  <- function(x){paste0("03_figuras/"                      , x)}


# 1. Cargar datos --------------------------------------------------------------

# Censo 2020
load(paste_data("df_familias.RData"))

# Intercensal 2015

load(paste_data("df_intercensal.RData"))

# 2. Funciones -----------------------------------------------------------------

## 2.1. Función para recodificar sexo ------------------------------------------

codificar_sexo <- function(var = x){
    
    v_sexo   <- c("Hombre", "Mujer", "No especificado")
    
    case_when(
        var == "1" ~ v_sexo[1], 
        var == "3" ~ v_sexo[2],
        TRUE ~ v_sexo[3])
}

## 2.2. Función para codificar parentesco --------------------------------------

# Censo
codificar_parentesco <- function(var = x){
    
    case_when(
        # Jefx
        var == "101" ~ 1,
        # Pareja
        startsWith(var, "2") ~ 2,
        # Hijx
        startsWith(var, "3") ~ 3,
        # Pariente
        startsWith(var, "4") ~ 4,
        # No pariente
        startsWith(var, "5") ~ 5,
        startsWith(var, "6") ~ 5,
        var == "701" ~ 5,
        # No especificado
        var == "999" ~ 6
    )
}

# 3. Manipulación de datos Censo 2020 ------------------------------------------

## 3.1. Recodificación de variables --------------------------------------------

df_familias_limpio <- df_familias %>% 
    rename(
        n_familia = NUMPERS,
        sexo_jefx = JEFE_SEXO,
        entidad   = ENT
    ) %>% 
    mutate(
        # Convertir en caracter el id de vivienda
        ID_VIV = as.character(ID_VIV),
        # Convertir en caracter el id de persona
        ID_PERSONA = as.character(ID_PERSONA),
        # Codificar sexo
        SEXO = codificar_sexo(SEXO),
        sexo_jefx = codificar_sexo(sexo_jefx),
        # Codificar parentesco
        PARENTESCO = codificar_parentesco(as.character(PARENTESCO))
    )

## 3.2. Creación de variables --------------------------------------------------

# Información por persona
df_personas_info <- df_familias_limpio %>% 
    mutate(
        # Identificar hijxs
        hijx = case_when(
            PARENTESCO == 3 ~ 1,
            TRUE ~ 0
        ),
        # Persona casada
        casadx = case_when(
            (SITUA_CONYUGAL == 5 | SITUA_CONYUGAL == 6 | SITUA_CONYUGAL == 7) ~ 1, 
            TRUE ~ 0),
        # Identificar pareja 
        pareja = case_when(
            PARENTESCO == 2 ~ 1,
            TRUE ~ 0),
        # Identificar parientes 
        pariente = case_when(
            PARENTESCO == 4 ~ 1,
            TRUE ~ 0
        ),
        # Identificar personas sin parentesco 
        no_pariente = case_when(
            PARENTESCO == 5 ~ 1,
            TRUE ~ 0),
        # Personas con parentesco no identificado
        no_identificado = case_when(
            (PARENTESCO == 6) ~ 1,
            TRUE ~ 0)
    )

# Información por vivienda usando data.table
df_personas_info[, `:=` (
    # Número de niñxs
    n_hijx = sum(hijx),
    # Número de parejas
    n_pareja = sum(pareja),
    # Número de parientes
    n_parientes = sum(pariente),
    # Número de personas sin parentesco 
    n_noparientes = sum(no_pariente),
    # Número de personas con parentesco no identificado 
    n_noid = sum(no_identificado)), 
    # Agrupado por vivienda
    by = "ID_VIV"]

# Información de parejas
df_parejas_info <- df_personas_info %>% 
    # Eliminar información 
    filter(pareja == 1) %>% 
    filter(n_pareja <= 1) %>% 
    select(ID_VIV, pareja, SEXO) %>%
    # Darle formato largo para añadir colunmas de sexo
    pivot_wider(names_from = "pareja", 
                values_from = "SEXO") %>% 
    select(ID_VIV,
           sexo_pareja = `1`)

## 3.3 Información de familas --------------------------------------------------

df_viviendas_info <- df_personas_info %>% 
    # Ordenar por id de vivienda y parentesco 
    arrange(ID_VIV, PARENTESCO) %>% 
    # Agrupar por vivienda
    group_by(ID_VIV) %>% 
    # Mantener primer linea por vivienda 
    slice(1) %>% 
    # Desagrupar 
    ungroup() %>% 
    # Unir información de pareja
    left_join(df_parejas_info, by = "ID_VIV") %>%
    # Seleccionar las variables a usar
    select(entidad,
           ID_VIV,
           UPM,
           FACTOR,
           ESTRATO,
           sexo_jefx,
           PARENTESCO,
           casadx,
           n_familia,
           n_hijx,
           n_pareja,
           n_parientes,
           n_noparientes,
           n_noid,
           sexo_pareja)


# 4. Manipulación de datos Intercensal 2015 ------------------------------------

## 4.1. Recodificación de variables --------------------------------------------

df_familias_intercensal <- df_intercensal %>% 
    rename(entidad = NOM_ENT) %>% 
    mutate(
        # Codificar sexo
        SEXO = codificar_sexo(SEXO)
    )

## 4.2. Creación de variables --------------------------------------------------

# Información por persona
df_personas_intercensal <- df_familias_intercensal %>% 
    mutate(
        # Identificar hijxs
        hijx = case_when(
            PARENT == 3 ~ 1,
            TRUE ~ 0
        ),
        # Persona casada
        casadx = case_when(
            SITUA_CONYUGAL == 5 ~ 1, 
            TRUE ~ 0),
        # Identificar pareja 
        pareja = case_when(
            PARENT == 2 ~ 1,
            TRUE ~ 0),
        # Identificar parientes 
        pariente = case_when(
            (PARENT == 4 | PARENT == 5 | PARENT == 6 |
                 PARENT == 7 |PARENT == 8)~ 1,
            TRUE ~ 0
        ),
        # Identificar personas sin parentesco 
        no_pariente = case_when(
            PARENT == 9 ~ 1,
            TRUE ~ 0),
        # Personas con parentesco no identificado
        no_identificado = case_when(
            (PARENT == 99) ~ 1,
            TRUE ~ 0)
    )

# Información por vivienda usando data.table
df_personas_intercensal[, `:=` (
    # Número de personas
    n_pers = .N,
    # Número de niñxs
    n_hijx = sum(hijx),
    # Número de parejas
    n_pareja = sum(pareja),
    # Número de parientes
    n_parientes = sum(pariente),
    # Número de personas sin parentesco 
    n_noparientes = sum(no_pariente),
    # Número de personas con parentesco no identificado 
    n_noid = sum(no_identificado)), 
    # Agrupado por vivienda
    by = "ID_VIV"]

# Información de parejas
df_parejas_intercensal <- df_personas_intercensal %>% 
    # Eliminar información 
    filter(pareja == 1) %>% 
    filter(n_pareja <= 1) %>% 
    select(ID_VIV, pareja, SEXO) %>%
    # Darle formato largo para añadir colunmas de sexo
    pivot_wider(names_from = "pareja", 
                values_from = "SEXO") %>% 
    select(ID_VIV,
           sexo_pareja = `1`)

## 4.3 Información de familas --------------------------------------------------

df_viviendas_intercensal <- df_personas_intercensal %>% 
    filter(PARENT == 1,
           n_pareja <= 1) %>% 
    # Unir información de pareja
    left_join(df_parejas_intercensal, by = "ID_VIV") %>%
    # Seleccionar las variables a usar
    select(ENT,
           entidad,
           ID_VIV,
           UPM,
           FACTOR,
           ESTRATO,
           PARENT,
           casadx,
           n_pers,
           n_hijx,
           n_pareja,
           n_parientes,
           n_noparientes,
           n_noid,
           sexo_pareja, 
           SEXO)


# 5. Guardar datos -------------------------------------------------------------

# Censo 2020
save(df_viviendas_info, file = paste_data("df_viviendas_info.RData"))

# Intercensal 2015
save(df_viviendas_intercensal, file = paste_data("df_viviendas_intercensal.RData"))

# FIN. -------------------------------------------------------------------------
