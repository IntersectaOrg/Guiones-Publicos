#------------------------------------------------------------------------------#
# Proyecto:                   Censos poblacionales del INEGI
# Objetivo:                   Armar base para análisis de familias en México
#
# Encargadas:                 Fernanda Torres
# Correo:                     fernanda.torres@alumnos.cide.edu
# Fecha de creación:          16 de junio de 2022
# Última actualización:       21 de junio de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# Cargar librerías 
require(pacman)
p_load(srvyr, tidyverse, dplyr, lubridate, scales, beepr, data.table)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones para directorios dentro del proyecto 
paste_code  <- function(x){paste0("01_códigos/"                      , x)}
paste_inp   <- function(x){paste0("02_datos_crudos/"                 , x)}
paste_data  <- function(x){paste0("03_datos_limpios/"                , x)}
paste_figs  <- function(x){paste0("04_figuras/2021/05_M5_uso_ppo/"   , x)}


# 1. Cargar datos --------------------------------------------------------------

# Datos de personas 
df_personas_raw <- fread(paste_inp("Personas00.CSV"),
                         # Seleccionar columnas necesarias para reducir tiempo de carga
                         select = c("ENT", "ID_VIV", "ID_PERSONA", "ESTRATO", "UPM", "FACTOR", "SEXO", "EDAD", "PARENTESCO", "SITUA_CONYUGAL"))

# Datos de viviendas
df_viviendas_raw <- fread(paste_inp("Viviendas00.CSV"),
                          # Seleccionar columnas necesarias para reducir tiempo de carga
                          select = c("ENT", "ID_VIV", "ESTRATO", "UPM", "FACTOR", "NUMPERS", "JEFE_SEXO"))

# 2. Crear base de datos a usar ------------------------------------------------

# Unir bases de datos
df_familias <- left_join(df_personas_raw, df_viviendas_raw)

# 3. Guardar base creada en archivo rda ----------------------------------------
save(df_familias, file = paste_data("df_familias.RData"))

# FIN. -------------------------------------------------------------------------
