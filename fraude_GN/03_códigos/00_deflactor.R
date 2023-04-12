#------------------------------------------------------------------------------#
# Proyecto:                   Fuerzas armadas
# Objetivo:                   Crear deflactor 
#
# Encargada:                  Regina Isabel Medina Rosales | Fernanda Torres
# Correo:                                                  | ftorres@intersecta.org
# Fecha de creación:          20 de junio de 2022
# Última actualización:       21 de febrero de 2023
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# Desactiva notación científica
options(scipen=999)

# Librerías 
require(pacman)

p_load(readr, openxlsx, inegiR, tidyverse, dplyr, beepr)

# 1. Cargar datos --------------------------------------------------------------

# API INEGI: https://www.inegi.org.mx/servicios/api_indicadores.html

# Banco de Información Económica (BIE) > Cuentas nacionales > 
# Producto interno bruto trimestral, base 2013 > 
# Índice acumulado de precios implícitos, 2013=100.0 > 
# Producto Interno Bruto, a precios de mercado 

v_token <- "" # Token personal 
v_id    <- "495808"

# Importar los datos desde la API
df_pib_raw <- inegi_series(serie    = v_id,
                           token    = v_token,
                           database = "BIE",
                           as_tt    = TRUE) 


# 2. Limpieza inicial ----------------------------------------------------------

# Limpiar el índice de precios con base en los precios de 2013
df_indices_2013 <- df_pib_raw                 %>% 
  # Crear variables de año y trimestre con formato de fecha
  mutate(
    year = lubridate::year(date), 
    trim = zoo::as.yearqtr(date))             %>% 
  # Dejar solo últimos trimestres de cada año (4° en su mayoría)
  group_by(year)                              %>% 
  filter(trim == max(trim))                   %>% 
  select(date, year, trim, index = values)


# 2. Funcionespara deflactar ---------------------------------------------------


## 2.1. Obtener deflactor ------------------------------------------------------

# Deflactor = (índice del año base con relación a la base en 2013/índice año de interés en relación a 2013)
# Valor deflactado = (valor corriente/deflactor)*100
 
# obtener_deflactor <- function(anio_corriente = 2013, anio_constante = 2013){
#       
#   # Obtener índice en año base
#   v_index_corri  <- df_indices_2013$index[df_indices_2013$year == anio_corriente]
#   
#   # Obtener índice en año a deflactar 
#   v_index_const  <- df_indices_2013$index[df_indices_2013$year == anio_constante]
#   
#   # Obtener el deflactor 
#   v_deflactor     <- (v_index_corri/v_index_const)*100
#   
#   # Reportar el valor del deflactor 
#   return(v_deflactor)
#   
# }
# 
# # Ejemplos  
# obtener_deflactor()
# obtener_deflactor(2013, 2013)
# obtener_deflactor(anio_corriente = 2022, anio_constante = 2013)
# 
# obtener_deflactor(2013, 2022) # Cuando queremos valores de 2022 en precios de 2013
# obtener_deflactor(2022, 2013) # Cuando queremos valores de 2013 en precios de 2022
# obtener_deflactor(1993, 2013) # Cuando queremos valores de 2013 en precios de 1993
# obtener_deflactor(2013, 1993) # Cuando queremos valores de 1993 en precios de 2013


## 2.2. Obtener valores deflactados --------------------------------------------

# deflactar <- function(
#     cifra_corriente = 1, anio_corriente = 2013, anio_constante = 2013){
#   
#   # Obtener índice en año base
#   v_index_corri       <- df_indices_2013$index[df_indices_2013$year == anio_corriente]
#   
#   # Obtener índice en año a deflactar 
#   v_index_const       <- df_indices_2013$index[df_indices_2013$year == anio_constante]
#   
#   # Obtener el deflactor 
#   v_deflactor         <- (v_index_corri/v_index_const)*100
#   
#   # Deflactar cifra corriente
#   v_cifra_deflactada  <- (cifra_corriente/v_deflactor)*100
#   
#   return(v_cifra_deflactada)
#   
# }
# 
# # Ejemplos  
# deflactar()
# deflactar(100, 2013, 2022) # Transformar 100 pesos de 2013 a valores de 2022 
# deflactar(100, 2022, 2013) # Transformar 100 pesos de 2022 a valores de 2013


## 3. Tabla de deflactores -----------------------------------------------------

# Crear base con combinaciones de 1993 a 2022

df_data <- as.data.frame(
  expand.grid(
    anio_corriente = c(1993:2022), 
    anio_constante = c(1993:2022)))

# ---- Una base larga con todas las combinaciones de deflactores 
df_deflactores_long <- df_data                                        %>% 
  # Pegar los índices de los años que serán precios corrientes
  left_join(df_indices_2013 %>% select(anio_corriente = year, index)) %>% 
  rename(index_corriente = index)                                     %>% 
  # Pegar los índices de los años que serán precios constantes
  left_join(df_indices_2013 %>% select(anio_constante = year, index)) %>% 
  rename(index_constante = index)                                     %>% 
  # Ordenar variables (primero valores corrientes y luego constantes )
  select(contains("corriente"), contains("constante")) %>% 
  # Estimar el deflactor   
  mutate(deflactor = round((index_corriente/index_constante)*100, 1))


# ---- Una base ancha donde las columnas sean los años base
df_deflactores_base_col <-  df_deflactores_long                       %>% 
  # Quitar índices de cada año de la base larga
  select(-contains("index"))                                          %>% 
  # Renombrar para facilitar identificación
  rename(base = anio_constante, anio = anio_corriente)                %>%  
  # Pasar a formato largo con año base como nombre de las columnas 
  pivot_wider(
    id_cols = anio, 
    names_from = base, 
    values_from = deflactor)                                          %>% 
  # Añadir a los nombres de las columnas el identificador de año base
  rename_at(vars(as.character(1993:2022)), function(x) paste0(x, "=100"))

# ---- Una base ancha donde las filas sean los años base
df_deflactores_base_row <-  df_deflactores_long                       %>% 
  # Quitar índices de cada año de la base larga
  select(-contains("index"))                                          %>% 
  # Renombrar para facilitar identificación
  rename(base = anio_constante)                                       %>%  
  # Pasar a formato largo con año base como nombre de las columnas 
  pivot_wider(                    
    id_cols = base,                     
    names_from = anio_corriente,                    
    values_from = deflactor)                                          %>% 
  # Añadir a los nombres de las filas el identificador de año base
  mutate(base = paste0(base, "=100"))


# 4. Guardar -------------------------------------------------------------------

## 4.1. En formato xlsx --------------------------------------------------------

# Crear archivo xlsx vacío 
wb <- createWorkbook()

# Poner nombre a las pestañas
addWorksheet(wb, "deflactor_base_horizontal")
addWorksheet(wb, "deflactor_base_vertical")
addWorksheet(wb, "Original INEGI Base 2013")

# Guardar pestaña de deflactores con año base en las columnas
writeData(
  wb       = wb,
  sheet    = 1,
  x        = df_deflactores_base_col,
  startRow = 1,
  colNames = T,
  rowNames = F)

# Guardar pestaña de deflactores con año base en las filas
writeData(
  wb       = wb,
  sheet    = 2,
  x        = df_deflactores_base_row,
  startRow = 1,
  colNames = T,
  rowNames = F)

# Guardar serie original del INEGI 
writeData(
  wb       = wb, 
  sheet    = 3, 
  x        = df_pib_raw %>% 
    select(Periodo = date, trimestre = date_shortcut, 
           "Producto interno bruto, a preciosde mercado" = values), 
  startRow = 1, 
  colNames = T, 
  # rownames = F
)

# Definir formatos de celda 
s_titulo  <- createStyle(halign = "center", valign = "center", wrapText = F, 
                         textDecoration = "bold")

s_color   <- createStyle(halign = "center", valign = "center", 
                         bgFill = "#E8D92E")

# Aplicar formatos de celda a nombres de columnas de primera pestaña 
addStyle(
  wb, 
  sheet = 1, 
  style = s_titulo, 
  cols = 1:ncol(df_deflactores_base_col),
  rows = 1,
  gridExpand=T
)

# Aplicar formato amarillo
conditionalFormatting(
  wb,
  sheet = 1, 
  cols = 1:ncol(df_deflactores_base_col), 
  rows = 2:nrow(df_deflactores_base_col), 
  # Condición en formato excel
  rule = "A2==100", 
  style = s_color
)

# Aplicar formatos de celda a nombres de columnas de segunda pestaña
addStyle(
  wb, 
  sheet = 2, 
  style = s_titulo, 
  cols = 1:ncol(df_deflactores_base_row),
  rows = 1,
  gridExpand=T
)


# Aplicar formato amarillo
conditionalFormatting(
  wb,
  sheet = 2, 
  cols  = 1:ncol(df_deflactores_base_row), 
  rows  = 2:nrow(df_deflactores_base_row), 
  rule  = "A2==100", 
  style = s_color
)

# Aplicar formatos de celda a nombres de columnas de tercera pestaña
addStyle(
  wb, 
  sheet      = 2, 
  style      = s_titulo, 
  cols       = 1:ncol(df_pib_raw),
  rows       = 1,
  gridExpand = T
)

# Guardar archivo xlsx
saveWorkbook(wb, file = "02_datos_limpios/00_deflactor.xlsx", overwrite = T)

## 4.2. En formato rdata -------------------------------------------------------

save(df_deflactores_long, file = "02_datos_limpios/df_deflactores_long.RData")
  
## FIN. ------------------------------------------------------------------------
