#------------------------------------------------------------------------------#
# Proyecto:                   Fuerzas armadas
# Objetivo:                   Crear serie histórica de cuenta pública
#
# Encargada:                  Regina Isabel Medina Rosales | Fernanda Torres
# Correo:                                                  | ftorres@intersecta.org
# Fecha de creación:          10 de junio de 2022
# Última actualización:       12 de abril de 2022
#------------------------------------------------------------------------------#

# Fuente:   https://www.transparenciapresupuestaria.gob.mx/es/PTP/Datos_Abiertos

# Almacenamiento: 
  # https://drive.google.com/drive/folders/1T5wZb1Z5goBs4l1pXxxlPU-wPlPzAkr3?usp=share_link

# 0. Configuración inicial -----------------------------------------------------

# Desactiva notación científica
options(scipen=999)

# Librerías 
require(pacman)
p_load(
  readr, openxlsx, googledrive, googlesheets4, inegiR, tidyverse, dplyr, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Funciones con direcciones de las carpetas
paste_code      <- function(x){paste0("03_códigos/", x)}
paste_inp       <- function(x){paste0("01_datos_crudos/"  , x)}
paste_out       <- function(x){paste0("02_datos_limpios/"  , x)}

# Activar las credenciales de google
v_correo <- "" # Correo de gmail personal

googledrive::drive_auth(v_correo)
googlesheets4::gs4_auth(v_correo)

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Cargar función para deflactar 
source(paste_code("00_deflactor.R"))

# 1. Cargar datos --------------------------------------------------------------

# Guardar nombres de archivos en carpeta drive
df_files  <- drive_ls(as_id("1T5wZb1Z5goBs4l1pXxxlPU-wPlPzAkr3")) 
v_years   <- c(2008:2021) # Años de interés
df_series <- data.frame()
temp      <- tempfile(fileext = ".csv")

for(i in 1:length(v_years)){
  print(paste0("Vuelta ", i, ": ", v_years[i]))
  
  
  dl      <- drive_download(
    as_id(df_files$id[str_detect(df_files$name, as.character(v_years[i])) &
                        str_detect(df_files$name, ".csv")]), 
    path = temp, overwrite = TRUE)
  
  # Importar base
  df_raw  <- read.csv(dl$local_path, encoding = "latin1") %>% # Encoding para acentos
    janitor::clean_names()                                %>% # Limpiar nombres
    janitor::remove_empty(which = "cols")                 %>% # Quitar columnas vacías
    mutate(year = v_years[i])                             %>% 
    mutate_all(~as.character(.))         #%>%   # Homologar tipo de dato para el pegado
  
  # Renombrar variable en años donde viene como "monto_ejercicio"
  if(v_years[i] %in% c(2013:2018)) {
    df_raw <- df_raw %>% rename(monto_ejercido = monto_ejercicio)
  }
  
  
  # Renombrar variable en años donde viene como "ejercicio"
  if(v_years[i] %in% c(2019:2021)) {
    df_raw <- df_raw %>% rename(monto_ejercido = ejercicio)
  }
  
  # Guardar copia de la base por año    
  assign(paste0("df_raw_", v_years[i]), df_raw)
  
  # Hacer una base unificada 
  df_series <- df_series %>% bind_rows(df_raw)
}

# Renombrar
df_unida <- df_series

beepr::beep()

## 1.3. Controles de calidad ---------------------------------------------------

# dim(df_raw_2008)
# dim(df_raw_2009)
# dim(df_raw_2010)
# dim(df_raw_2011)
# dim(df_raw_2012)
# dim(df_raw_2013)
# dim(df_raw_2014)
# dim(df_raw_2015)
# dim(df_raw_2016)
# dim(df_raw_2017)
# dim(df_raw_2018)
# dim(df_raw_2019)
# dim(df_raw_2020)
# dim(df_raw_2021)
# 
# sum(is.na(df_raw_2008$monto_aprobado))
# sum(is.na(df_raw_2009$monto_aprobado))
# sum(is.na(df_raw_2010$monto_aprobado))
# sum(is.na(df_raw_2011$monto_aprobado))
# sum(is.na(df_raw_2012$monto_aprobado)) # 1
# sum(is.na(df_raw_2013$monto_aprobado))
# sum(is.na(df_raw_2014$monto_aprobado))
# sum(is.na(df_raw_2015$monto_aprobado))
# sum(is.na(df_raw_2016$monto_aprobado))
# sum(is.na(df_raw_2017$monto_aprobado))
# sum(is.na(df_raw_2018$monto_aprobado))
# sum(is.na(df_raw_2019$monto_aprobado))
# sum(is.na(df_raw_2020$monto_aprobado)) # 59473 NAs
# sum(is.na(df_raw_2021$monto_aprobado))
# 
# sum(is.na(df_series$monto_aprobado)) # El monto total de NAs sí es consistente
# 
# sum(is.na(df_raw_2008$monto_ejercido))
# sum(is.na(df_raw_2009$monto_ejercido))
# sum(is.na(df_raw_2010$monto_ejercido))
# sum(is.na(df_raw_2011$monto_ejercido))
# sum(is.na(df_raw_2012$monto_ejercido)) 
# sum(is.na(df_raw_2013$monto_ejercido))
# sum(is.na(df_raw_2014$monto_ejercido))
# sum(is.na(df_raw_2015$monto_ejercido))
# sum(is.na(df_raw_2016$monto_ejercido))
# sum(is.na(df_raw_2017$monto_ejercido))
# sum(is.na(df_raw_2018$monto_ejercido))
# # sum(is.na(df_raw_2019$))
# # sum(is.na(df_raw_2020$)) 
# # sum(is.na(df_raw_2021$))

# Explorar similitudes de bases importadas
# names(df_raw_2008)
# names(df_raw_2009)
# names(df_raw_2010)
# names(df_raw_2011)
# names(df_raw_2012)
# names(df_raw_2013) # Aquí iniciaba el uso de "monto_ejercicio"
# names(df_raw_2014) 
# names(df_raw_2015)
# names(df_raw_2016)
# names(df_raw_2017)
# names(df_raw_2018) # Aquí terminaba monto_ejericicio
# names(df_raw_2019) 
# names(df_raw_2020)
# names(df_raw_2021)
# 
# # Serie unificada 
# names(df_series)
# table(df_series$year)
# 
# 
# # Revisar si las variables numéricas tienen signos de coma ","
# sum(str_detect(df_series$monto_aprobado  , ","), na.rm = T) # Sí hay comas
# sum(str_detect(df_series$monto_ejercido  , ","), na.rm = T)
# sum(str_detect(df_series$monto_ejercicio , ","), na.rm = T)
# sum(str_detect(df_series$monto_modificado, ","), na.rm = T) # Sí hay comas
# sum(str_detect(df_series$monto_devengado , ","), na.rm = T) # Sí hay comas
# sum(str_detect(df_series$monto_pagado    , ","), na.rm = T) # Sí hay comas
# sum(str_detect(df_series$monto_adefas    , ","), na.rm = T)
# 
# sum(is.na(df_series$monto_aprobado  ))
# sum(is.na(df_series$monto_ejercido  ))
# sum(is.na(df_series$monto_ejercicio ))
# sum(is.na(df_series$monto_modificado))
# sum(is.na(df_series$monto_devengado ))
# sum(is.na(df_series$monto_pagado    ))
# sum(is.na(df_series$monto_adefas    ))



# 2. Procesar datos ------------------------------------------------------------

## 2.1. Limpiar categorías -----------------------------------------------------

df_numeric <- df_unida                                                   %>%
  # Eliminar las comas de todas las variables de dinero
  mutate_at(
    .vars = c("monto_aprobado" , "monto_ejercido", "monto_modificado",
              "monto_devengado", "monto_pagado"  , "monto_adefas"),
    .funs = ~str_replace_all(., ",", ""))                               %>%
  # Convertir a formato numérico
  mutate(
    monto_aprobado   = as.numeric(monto_aprobado),
    monto_ejercido   = as.numeric(monto_ejercido),
    monto_modificado = as.numeric(monto_modificado),
    monto_devengado  = as.numeric(monto_devengado),
    monto_pagado     = as.numeric(monto_pagado),
    monto_adefas     = as.numeric(monto_adefas)
    )                   %>%
  glimpse()


# Limpiar los códigos repetidos
df_limpia <- df_numeric %>% 
  mutate(
    desc_ramo = case_when(
      id_ramo == "2"  ~ "Presidencia",
      id_ramo == "8"  ~ "Agricultura y Desarrollo Rural",
      id_ramo == "15" ~ "Desarrollo Agrario, Territorial y Urbano",
      id_ramo == "20" ~ "Bienestar",
      id_ramo == "22" ~ "INE",
      id_ramo == "32" ~ "TFJA",
      id_ramo == "36" ~ "Seguridad y Protección Ciudadana",
      id_ramo == "44" ~ "INAI",
      T ~ desc_ramo
    )
  )


# Limpiar nombres de los ramos
unique(df_limpia$ciclo)
unique(df_limpia$id_ramo)
unique(df_limpia$desc_ramo)

# Revisar combinaciones únicas de nombres y id
df_names <- df_limpia %>% 
  distinct(id_ramo, desc_ramo)

# Ver identificadores que tengan más de un nombre
View(table(df_names$id_ramo))

# Identificadores repetidos: 2, 8, 15, 20, 22, 32, 36, 44

# load(paste_out("df_cuenta_pub_pconstantes.RData"))
# df_numeric <- df_cuenta_pub_pconstantes

## 2.2. Deflactar (pesos 2022) -------------------------------------------------

# ---- Datos de transparencia presupuestaria (base 2013)
# # Ubicación del deflactor implícito usado por transparencia presupuestaria
# v_file  <- "https://www.transparenciapresupuestaria.gob.mx/work/models/PTP/Presupuesto/Programacion/Deflactores/Deflactores_PIB.xlsx"
# 
# # Crear archivo temporal donde descargar la información 
# temp    <- tempfile(fileext = ".xlsx")
# 
# # Decargar información en archivo temporal 
# download.file(v_file, destfile = temp, mode = 'wb')
# 
# # Importar archivo a R
# df_deflactor_raw <- readxl::read_excel(path = temp, skip = 3)
# 
# 
# # Procesar información 
# df_deflactor <- df_deflactor_raw          %>% 
#   janitor::clean_names()                  %>% 
#   rename(
#     deflactor = starts_with("deflactor")) %>% 
#   select(periodo, deflactor)              %>% 
#   drop_na()


# ---- Datos del INEGI (base 2013)

# API INEGI: https://www.inegi.org.mx/servicios/api_indicadores.html

# Banco de Información Económica (BIE) > Cuentas nacionales > 
# Producto interno bruto trimestral, base 2013 > 
# Índice acumulado de precios implícitos, 2013=100.0 > 
# Producto Interno Bruto, a precios de mercado 

v_token <- "" # Token personal 
v_id    <- "495808"

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

# Unir índices de precios con base de cuenta pública 
df_corriente <- df_limpia                     %>% 
  mutate(
    year       = as.numeric(year), 
    index_last = v_index_last, 
    cifras = "Pesos corrientes de cada año")  %>% 
  left_join(df_indices_2013, by = "year")     %>% 
  select(-c(date, trim))

# Deflactar cifras 
df_constante <- df_corriente                  %>% 
  mutate(
    deflactor = (index_base/index_last)*100, 
    across(starts_with("monto"), ~((./deflactor)*100)), 
    cifras = paste0("Pesos constantes de ", max(unique(df_indices_2013$year))))

# Control de calidad 
unique(df_constante$deflactor)

max(df_corriente$monto_ejercido, na.rm = T) # Con valores corrientes
max(df_constante$monto_ejercido, na.rm = T) # Con valores de 2022


## 2.3. Agregar año más reciente (monto aprobado) ------------------------------
df_last_pef <- read.xlsx(paste_inp("PEF_2022.xlsx"))

df_numeric_last <- df_last_pef                          %>%
  janitor::clean_names()                                %>% # Limpiar nombres
  janitor::remove_empty(which = "cols")                 %>% # Quitar columnas vacías
  mutate_all(~as.character(.))                          %>%
  mutate(year = 2022)                                   %>% # Agregar variable de año 
  # Eliminar las comas de todas las variables de dinero
  mutate(monto_aprobado = str_replace_all(monto_aprobado, ",", ""))     %>% 
  # Convertir a formato numérico
  mutate(monto_aprobado   = as.numeric(monto_aprobado)) %>% 
  glimpse()

# Limpiar los códigos repetidos
df_limpia_last <- df_numeric_last             %>% 
  mutate(
    desc_ramo = case_when(
      id_ramo == "2"  ~ "Presidencia",
      id_ramo == "8"  ~ "Agricultura y Desarrollo Rural",
      id_ramo == "15" ~ "Desarrollo Agrario, Territorial y Urbano",
      id_ramo == "20" ~ "Bienestar",
      id_ramo == "22" ~ "INE",
      id_ramo == "32" ~ "TFJA",
      id_ramo == "36" ~ "Seguridad y Protección Ciudadana",
      id_ramo == "44" ~ "INAI",
      T ~ desc_ramo
    )
  )

# Verificar que las variables se llamen igual 
# (no coinciden las variables sobre los capítulos)
names(df_limpia_last) %in% names(df_constante)

# Pegar al resto de la serie de tiempo 
df_corriente_all <- df_corriente %>% bind_rows(df_limpia_last)
df_constante_all <- df_constante %>% bind_rows(df_limpia_last)
  
# 2.4. Controles de calidad  ---------------------------------------------------

table(df_corriente_all$ciclo)
table(df_constante_all$ciclo)

# 3. Guardar serie histórica ---------------------------------------------------

# Renombrar
df_cuenta_pub_pconstantes <- df_constante_all

# Guardar 
save(df_cuenta_pub_pconstantes, file = paste_out("df_cuenta_pub_pconstantes.RData"))

# FIN. -------------------------------------------------------------------------
