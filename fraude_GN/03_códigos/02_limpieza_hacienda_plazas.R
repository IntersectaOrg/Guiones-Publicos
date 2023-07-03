#------------------------------------------------------------------------------#
# Proyecto:                   Fuerzas armadas
# Objetivo:                   Crear serie histórica de plazas de Hacienda
#
# Encargada:                  Regina Isabel Medina Rosales | Fernanda Torres
# Correo:                                                  | ftorres@intersecta.org
# Fecha de creación:          21 de junio   de 2021
# Última actualización:       11 de octubre de 2022
#------------------------------------------------------------------------------#

# Fuente: https://www.pef.hacienda.gob.mx/es/PEF/Analiticos_PresupuestariosPEF

# Almacenamiento:
  # https://drive.google.com/drive/folders/16Hz7x6Pb1zfFT2G-U5Tp1vLOuDFn6_as?usp=sharing

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, googledrive, googlesheets4, tidyverse, dplyr, inegiR, beepr)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
out <- "02_datos_limpios/"

# Activar las credenciales de google
v_correo <- "" # Correo de gmail personal

googledrive::drive_auth(v_correo)
googlesheets4::gs4_auth(v_correo)

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# 1. Cargar datos --------------------------------------------------------------

# Guardar nombres de archivos en carpeta drive
df_files  <- drive_ls(as_id("16Hz7x6Pb1zfFT2G-U5Tp1vLOuDFn6_as")) 
temp_2005 <- tempfile(fileext = ".xls")
temp      <- tempfile(fileext = ".xlsx")
v_years <- c("05", "06", "07", "08", "09", "10", "11", "12", "13", 
             "14", "15", "16", "17", "18", "19", "20", "21", "22")

# Para año 2005 (archivo xls)

dl  <- drive_download(
    as_id(df_files$id[str_detect(df_files$name, as.character(v_years[1]))]),
    path = temp_2005, overwrite = TRUE)

# Importar base
df_crudo  <- read_excel(dl$local_path)

# Guardar la base cruda en un objeto con su proio nombre
assign(paste0("df_crudo_", v_years[1]), df_crudo)

# Para años 2006 a 2022 (archivo xlsx)

for(i in 2:length(v_years)){
    año <- paste0("20", v_years[i])
    
    print(paste0("Vuelta ", i, ": ", año))
    
    dl  <- drive_download(
        as_id(df_files$id[str_detect(df_files$name, as.character(año))]),
        path = temp, overwrite = TRUE)

    # Importar base
    df_crudo  <- read_excel(dl$local_path)

    # Guardar la base cruda en un objeto con su proio nombre
    assign(paste0("df_crudo_", v_years[i]), df_crudo)
}

# 2. Limpieza ------------------------------------------------------------------

## 2.1. Total  de plazas por año y ramo ----------------------------------------

### 2.1.1. Limpiar todos los años ----------------------------------------------
# Nota: Tuve que ir a los archivos originales de las plazas y expandir todas 
# las filas para que se leyeran todas las observaciones.

# Bases según número de variables 
# 9  variables: 2005-2007
# 12 variables: 2008-2009
# 20 variables: 2010-2011, 2016
# 21 variables: 2012-2014, 2017
# 18 variables: 2015, 2018-2021

# Renombrar, filtrar y seleccionar para dejar bases listas para el loop 
v_names <- names(df_crudo_05)

df_2005 <- df_crudo_05                              %>% 
  rename(
    ramo   = v_names[1], 
    unidad = v_names[2], 
    puesto = v_names[4],
    plazas = v_names[6])                              %>% 
  slice(
    which(
      grepl("02 Presidencia", ramo, 
            ignore.case = T))[1]:dim(df_crudo_05)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2005)

# dim(df_2005)
# sum(is.na(df_2005$plazas))

# Para 2006
v_names <- names(df_crudo_06)

df_2006 <- df_crudo_06                                %>% 
  rename(
    ramo   = v_names[1], 
    unidad = v_names[2], 
    puesto = v_names[4],
    plazas = v_names[6])                              %>% 
  slice(
    which(
      grepl("02 Presidencia", ramo, 
            ignore.case = T))[1]:dim(df_crudo_06)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2006)

# Para 2007
v_names <- names(df_crudo_07)

df_2007 <- df_crudo_07                                %>% 
  rename(
    ramo   = v_names[1], 
    unidad = v_names[2], 
    puesto = v_names[4],
    plazas = v_names[6])                              %>% 
  slice(
    which(
      grepl("02 Presidencia", ramo, 
            ignore.case = T))[1]:dim(df_crudo_07)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2007)

# Para 2008
v_names <- names(df_crudo_08)

df_2008 <- df_crudo_08                                %>% 
  rename(
    ramo   = v_names[1], 
    unidad = v_names[3], 
    puesto = v_names[6],
    plazas = v_names[7])                              %>% 
  slice(
    which(
      grepl("02 Presidencia", ramo, 
            ignore.case = T))[1]:dim(df_crudo_08)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2008)

# Para 2009
v_names <- names(df_crudo_09)

df_2009 <- df_crudo_09                                %>% 
  rename(
    ramo   = v_names[1], 
    unidad = v_names[3], 
    puesto = v_names[6],
    plazas = v_names[7])                              %>% 
  slice(
    which(
      grepl("02 Presidencia", ramo, 
            ignore.case = T))[1]:dim(df_crudo_09)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2009)


# Para 2010
v_names <- names(df_crudo_10)

df_2010 <- df_crudo_10                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[8],
    plazas = v_names[10])                             %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_10)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2010)


# Para 2011
v_names <- names(df_crudo_11)

df_2011 <- df_crudo_11                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[8],
    plazas = v_names[10])                             %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_11)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2011)


# Para 2012
v_names <- names(df_crudo_12)

df_2012 <- df_crudo_12                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[8],
    plazas = v_names[11])                             %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_12)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2012)


# Para 2013
v_names <- names(df_crudo_13)

df_2013 <- df_crudo_13                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[8],
    plazas = v_names[11])                             %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_13)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2013)


# Para 2014
v_names <- names(df_crudo_14)

df_2014 <- df_crudo_14                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[8],
    plazas = v_names[11])                             %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_14)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2014)


# Para 2015
v_names <- names(df_crudo_15)

df_2015 <- df_crudo_15                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[7],
    plazas = v_names[9])                              %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_15)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2015)



# Para 2016
v_names <- names(df_crudo_16)

df_2016 <- df_crudo_16                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[8],
    plazas = v_names[11])                             %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_16)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2016)


# Para 2017
v_names <- names(df_crudo_17)

df_2017 <- df_crudo_17                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[8],
    plazas = v_names[11])                             %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_17)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2017)


# Para 2018
v_names <- names(df_crudo_18)

df_2018 <- df_crudo_18                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[7],
    plazas = v_names[9])                              %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_18)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2018)

# Para 2019
v_names <- names(df_crudo_19)

df_2019 <- df_crudo_19                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[7],
    plazas = v_names[9])                              %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_19)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2019)


# Para 2020
v_names <- names(df_crudo_20)

df_2020 <- df_crudo_20                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[7],
    plazas = v_names[9])                              %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_20)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2020)

# Para 2021
v_names <- names(df_crudo_21)

df_2021 <- df_crudo_21                                %>% 
  rename(
    sector = v_names[1],
    ramo   = v_names[2], 
    unidad = v_names[5], 
    puesto = v_names[7],
    plazas = v_names[9])                              %>% 
  slice(
    which(
      grepl("sectores", sector, 
            ignore.case = T))[1]:dim(df_crudo_21)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2021)

# Para 2022  (aquí es distinto el orden de las columnas)
v_names <- names(df_crudo_22)

df_2022 <- df_crudo_22                                %>% 
  rename(
    sector = v_names[3],
    ramo   = v_names[4], 
    unidad = v_names[9], 
    puesto = v_names[12],
    plazas = v_names[16])                              %>% 
  # slice(
  #   which(
  #     grepl("sectores", sector, 
  #           ignore.case = T))[1]:dim(df_crudo_22)[1]) %>%
  select(ramo, unidad, puesto, plazas)                %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2022, 
         plazas = as.character(plazas))

# Unir todos los años
df_unida <- bind_rows(
  df_2005, df_2006, df_2007, df_2008, df_2009, df_2010, 
  df_2011, df_2012, df_2013, df_2014, df_2015, df_2016,
  df_2017, df_2018, df_2019, df_2020, df_2021, df_2022
  )


### 2.1.2. Limpieza final ------------------------------------------------------

sort(unique(df_unida$ramo))

# Nombres de las secretarías
df_corregida <- df_unida %>% 
  mutate(
    # Homologar nombre de las secretarías
    ramo = case_when(
      str_detect(ramo, "02") ~ "02 Presidencia", 
      str_detect(ramo, "08") ~ "08 Agricultura y Desarrollo Rural", 
      str_detect(ramo, "09") ~ "09 Infraestructura, Comunicaciones y Transportes", 
      str_detect(ramo, "15") ~ "15 Desarrollo Agrario, Territorial y Urbano",
      str_detect(ramo, "20") ~ "20 Bienestar", 
      str_detect(ramo, "36") ~ "36 Seguridad y Protección Ciudadana", 
      T ~ ramo)) %>% 
  # Crear variables identificadoras del ramo
  mutate(
    id_ramo  = str_sub(ramo, 1, 2), 
    nom_ramo = str_remove_all(ramo, "[:digit:]"), 
    plazas   = as.numeric(plazas))  %>%
  mutate(
    # Homologar nombre de las secretarías
    nom_ramo = case_when(
      str_detect(nom_ramo, "02") ~ "Presidencia", 
      str_detect(nom_ramo, "08") ~ "Agricultura y Desarrollo Rural", 
      str_detect(nom_ramo, "09") ~ "Infraestructura, Comunicaciones y Transportes", 
      str_detect(nom_ramo, "15") ~ "Desarrollo Agrario, Territorial y Urbano",
      str_detect(nom_ramo, "20") ~ "Bienestar", 
      str_detect(nom_ramo, "36") ~ "Seguridad y Protección Ciudadana", 
      T ~ nom_ramo), 
    # Simplificar nombre de las secretarías
    ramo_corto = case_when(
      str_detect(ramo, "02") ~ "Presidencia",
      str_detect(ramo, "04") ~ "SEGOB", 
      str_detect(ramo, "05") ~ "SRE", 
      str_detect(ramo, "06") ~ "SHCP", 
      str_detect(ramo, "07") ~ "SEDENA", 
      str_detect(ramo, "08") ~ "Agricultura y Desarrollo Rural",
      str_detect(ramo, "09") ~ "SCT",
      str_detect(ramo, "10") ~ "Economía", 
      str_detect(ramo, "11") ~ "SEP", 
      str_detect(ramo, "12") ~ "Salud", 
      str_detect(ramo, "13") ~ "SEMAR", 
      str_detect(ramo, "14") ~ "STPS", 
      str_detect(ramo, "15") ~ "SEDATU",
      str_detect(ramo, "16") ~ "SEMARNAT", 
      str_detect(ramo, "17") ~ "FGR", 
      str_detect(ramo, "18") ~ "Energía", 
      str_detect(ramo, "20") ~ "Bienestar",
      str_detect(ramo, "21") ~ "Turismo", 
      str_detect(ramo, "23") ~ "Provisiones Salariales", 
      str_detect(ramo, "25") ~ "Previsiones para sistemas de educación", 
      str_detect(ramo, "27") ~ "Función Pública", 
      str_detect(ramo, "32") ~ "TFJA", 
      str_detect(ramo, "31") ~ "Tribunales Agrarios", 
      str_detect(ramo, "36") ~ "SSPC",
      str_detect(ramo, "37") ~ "Consejería Jurídica", 
      str_detect(ramo, "38") ~ "CONACYT", 
      str_detect(ramo, "45") ~ "CRE", 
      str_detect(ramo, "46") ~ "CNH", 
      str_detect(ramo, "47") ~ "No sectorizadas", 
      str_detect(ramo, "48") ~ "Cultura", 
      str_detect(ramo, "50") ~ "IMSS", 
      str_detect(ramo, "51") ~ "ISSSTE", 
      str_detect(ramo, "52") ~ "PEMEX", 
      str_detect(ramo, "53") ~ "CFE", 
      T ~ ramo) 
  )


# Estimar total de plazas de cada sector por año y hacer ranking
df_plazas_total <- df_corregida                   %>% 
  # Añadir total de plazas por ranking
  group_by(year, id_ramo, nom_ramo, ramo_corto)   %>% 
  summarise(
    tot_plazas_ramo   = sum(plazas, na.rm = T))   %>% 
  ungroup()                                       %>% 
  group_by(year)                                  %>% 
  mutate(ranking_ramo_year = rank(-tot_plazas_ramo))

# Juntar las dos bases 
df_plazas <- df_corregida %>% 
  left_join(df_plazas_total, by = c("ramo_corto", "year", "id_ramo", "nom_ramo"))

# Verificar que no haya nombres repetidos
sort(unique(df_plazas$ramo_corto))

# Revisar datos por año 
table(df_plazas$year)
table(df_plazas$year, df_plazas$ramo_corto)


# df_unidades <- df_plazas %>% 
#   select(unidad, year) %>% 
#   distinct()


### 2.1.3. Guardar base --------------------------------------------------------

df_plazas <- df_unida

save(df_plazas, file = paste0(out, "df_plazas.Rdata"))

## 2.2. Total por año, ramo e ingreso ------------------------------------------

#### 2.2.1. Limpiar todos los años ---------------------------------------------

# Bases según número de variables 
# 9  variables: 2005-2007
# 12 variables: 2008-2009
# 20 variables: 2010-2011, 2016
# 21 variables: 2012-2014, 2017
# 18 variables: 2015, 2018-2021

# ---- 2005
v_names <- names(df_crudo_05)

df_2005 <- df_crudo_05                                %>% 
  rename(
    ramo           = v_names[1], 
    unidad         = v_names[2], 
    zona_economica = v_names[3], 
    puesto         = v_names[4],
    nivel_salarial = v_names[5], 
    plazas         = v_names[6], 
    horas          = v_names[7], 
    total_de_percepciones = v_names[8])               %>% 
  slice(
    which(
      grepl("02 Presidencia", ramo, 
            ignore.case = T))[1]:dim(df_crudo_05)[1]) %>%
  select(-9) %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  drop_na(plazas) %>% 
  mutate(year = 2005)  %>% 
  mutate_all(~as.character(.))

# Para 2006
v_names <- names(df_crudo_06)

df_2006 <- df_crudo_06                                %>% 
  rename(
    ramo           = v_names[1], 
    unidad         = v_names[2],  
    zona_economica = v_names[3], 
    puesto         = v_names[4],
    nivel_salarial = v_names[5], 
    plazas         = v_names[6], 
    horas          = v_names[7], 
    total_de_percepciones = v_names[8])               %>% 
  slice(
    which(
      grepl("02 Presidencia", ramo, 
            ignore.case = T))[1]:dim(df_crudo_06)[1]) %>%
  select(-9) %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2006) %>% 
  mutate_all(~as.character(.))

# Para 2007
v_names <- names(df_crudo_07)

df_2007 <- df_crudo_07                                %>% 
  rename(
    ramo           = v_names[1], 
    unidad         = v_names[2], 
    zona_economica = v_names[3], 
    puesto         = v_names[4],
    nivel_salarial = v_names[5], 
    plazas         = v_names[6], 
    horas          = v_names[7], 
    total_de_percepciones = v_names[8])               %>% 
  slice(
    which(
      grepl("02 Presidencia", ramo, 
            ignore.case = T))[1]:dim(df_crudo_07)[1]) %>%
  select(-9)                                          %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2007)  %>% 
  mutate_all(~as.character(.))

# Para 2008
df_2008 <- df_crudo_08                                %>% 
  slice(6:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2008) %>% 
  rename(
    total_de_percepciones       = total, 
    subtotal_sueldos_y_salarios = sueldos_salarios, 
    subtotal_prestaciones       = prestaciones
  ) %>% 
  mutate_all(~as.character(.))


# Para 2009
df_2009 <- df_crudo_09                                %>% 
  slice(6:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  # Rellenar valores faltantes en las variables
  fill(ramo:puesto, .direction = "downup")            %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2009) %>% 
  rename(
    total_de_percepciones       = total, 
    subtotal_sueldos_y_salarios = sueldos_salarios, 
    subtotal_prestaciones = prestaciones
  ) %>% 
  mutate_all(~as.character(.))

# Para 2010
df_2010 <- df_crudo_10                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2010) %>% 
  # Quitar variable que no sirve
  select(-total) %>% 
  mutate_all(~as.character(.))


# Para 2011
df_2011 <- df_crudo_11                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2011) %>% 
  # Quitar variable que no sirve (puros 0)
  select(-total) %>% 
  mutate_all(~as.character(.))


# Para 2012
df_2012 <- df_crudo_12                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2012) %>% 
  # Quitar variable que no sirve
  select(-c(total, total_unico)) %>% 
  mutate_all(~as.character(.))

# identical(df_2012$total_de_percepciones, df_2012$total_unico)

# Para 2013
df_2013 <- df_crudo_13                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2013) %>% 
  # Quitar variable que no sirve
  select(-c(total, total_unico)) %>% 
  mutate_all(~as.character(.))
 
# identical(df_2013$total_de_percepciones, df_2013$total_unico)

# Para 2014
df_2014 <- df_crudo_14                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2014) %>% 
  # Quitar variable que no sirve
  select(-c(total, total_unico)) %>% 
  mutate_all(~as.character(.))

# Para 2015
df_2015 <- df_crudo_15                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2015)  %>% 
  mutate_all(~as.character(.))

# Para 2016
df_2016 <- df_crudo_16                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2016) %>% 
  # Quitar variable que no sirve
  select(-total_unico) %>% 
  mutate_all(~as.character(.))

# Para 2017
df_2017 <- df_crudo_17                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2017)  %>% 
  # Quitar variable que no sirve
  select(-c(total, total_unico)) %>% 
  mutate_all(~as.character(.))

# Para 2018
df_2018 <- df_crudo_18                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2018) %>% 
  mutate_all(~as.character(.))

# Para 2019
df_2019 <- df_crudo_19                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2019) %>% 
  mutate_all(~as.character(.))

# Para 2020
df_2020 <- df_crudo_20                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2020) %>% 
  mutate_all(~as.character(.))

# Para 2021
df_2021 <- df_crudo_21                                %>% 
  slice(5:n())                                        %>% 
  janitor::row_to_names(row = 1)                      %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  mutate(year = 2021) %>% 
  mutate_all(~as.character(.))


# Para 2022  
df_2022 <- df_crudo_22                                %>% 
  janitor::clean_names()                              %>% 
  rename(puesto = descripcion, 
         nivel_salarial = nivel)                      %>% 
  # Rellenar valores faltantes en las variables
  fill(tipo_sector:puesto, .direction = "downup")     %>% 
  # Eliminar filas sin datos de plazas 
  filter(!is.na(puesto))                              %>% 
  rename(
    year = ciclo, 
    medidas_de_proteccion_al_salario = 22, 
    fondos_y_seguros_de_ahorro_para_el_retiro = 24,
    condiciones_de_trabajo_contratos_colectivos_y_otras_remuneraciones = 25) %>% 
  # Eliminar variable repetida y variables con puros ceros 
  select(-c(total_unico, monto)) %>% 
  mutate_all(~as.character(.))

# identical(df_2022$total_unico, df_2022$total_de_percepciones)


# v_names <-  c(names(df_2005), names(df_2006), names(df_2007), names(df_2008), 
#               names(df_2009), names(df_2010), names(df_2011), names(df_2012),
#               names(df_2013), names(df_2014), names(df_2015), names(df_2016),
#               names(df_2017), names(df_2018), names(df_2019), names(df_2020),
#               names(df_2021), names(df_2022))
# 
# 
# unique(v_names)

#### 2.2.2. Unir bases ---------------------------------------------------------

# ---- Unir bases 
df_unida <- df_2005 %>% bind_rows(df_2006, df_2007, df_2008, df_2009, df_2010, 
                                  df_2011, df_2012, df_2013, df_2014, df_2015, 
                                  df_2016, df_2017, df_2018, df_2019, df_2020, 
                                  df_2021, df_2022) %>% 
  # Cambiar a formato numérico 
  mutate_at(
    .vars = c("year", "plazas", "total_de_percepciones", "subtotal_sueldos_y_salarios", 
              "subtotal_prestaciones", "percepciones_extraordinarias"), 
    .funs = ~as.numeric(.)) %>%
  mutate(
    id_ramo = str_sub(ramo, 1, 2), 
    nom_ramo = str_remove_all(ramo, "[:digit:]"), 
    # Homologar nombre de las secretarías
    nom_ramo = case_when(
      str_detect(ramo, "02") ~ "Presidencia", 
      str_detect(ramo, "08") ~ "Agricultura y Desarrollo Rural", 
      str_detect(ramo, "09") ~ "Infraestructura, Comunicaciones y Transportes", 
      str_detect(ramo, "15") ~ "Desarrollo Agrario, Territorial y Urbano",
      str_detect(ramo, "20") ~ "Bienestar", 
      str_detect(ramo, "36") ~ "Seguridad y Protección Ciudadana", 
      T ~ ramo), 
    # Simplificar nombre de las secretarías
    ramo_corto = case_when(
      str_detect(ramo, "02") ~ "Presidencia",
      str_detect(ramo, "04") ~ "SEGOB", 
      str_detect(ramo, "05") ~ "SRE", 
      str_detect(ramo, "06") ~ "SHCP", 
      str_detect(ramo, "07") ~ "SEDENA", 
      str_detect(ramo, "08") ~ "Agricultura y Desarrollo Rural",
      str_detect(ramo, "09") ~ "SCT",
      str_detect(ramo, "10") ~ "Economía", 
      str_detect(ramo, "11") ~ "SEP", 
      str_detect(ramo, "12") ~ "Salud", 
      str_detect(ramo, "13") ~ "SEMAR", 
      str_detect(ramo, "14") ~ "STPS", 
      str_detect(ramo, "15") ~ "SEDATU",
      str_detect(ramo, "16") ~ "SEMARNAT", 
      str_detect(ramo, "17") ~ "FGR", 
      str_detect(ramo, "18") ~ "Energía", 
      str_detect(ramo, "20") ~ "Bienestar",
      str_detect(ramo, "21") ~ "Turismo", 
      str_detect(ramo, "23") ~ "Provisiones Salariales", 
      str_detect(ramo, "25") ~ "Previsiones para sistemas de educación", 
      str_detect(ramo, "27") ~ "Función Pública", 
      str_detect(ramo, "32") ~ "TFJA", 
      str_detect(ramo, "31") ~ "Tribunales Agrarios", 
      str_detect(ramo, "36") ~ "SSPC",
      str_detect(ramo, "37") ~ "Consejería Jurídica", 
      str_detect(ramo, "38") ~ "CONACYT", 
      str_detect(ramo, "45") ~ "CRE", 
      str_detect(ramo, "46") ~ "CNH", 
      str_detect(ramo, "47") ~ "No sectorizadas", 
      str_detect(ramo, "48") ~ "Cultura", 
      str_detect(ramo, "50") ~ "IMSS", 
      str_detect(ramo, "51") ~ "ISSSTE", 
      str_detect(ramo, "52") ~ "PEMEX", 
      str_detect(ramo, "53") ~ "CFE", 
      T ~ ramo) 
    )
  
# names(df_unida)

#### 2.2.3. Deflactar ----------------------------------------------------------

# ---- Datos del INEGI (base 2013)

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

# Unir índices de precios con base de plazas
df_corriente <- df_unida                     %>%
  mutate(
    year       = as.numeric(year),
    index_last = v_index_last, # Indicar cuál es el índice más reciente
    cifras = "Pesos corrientes de cada año")  %>%
  left_join(df_indices_2013, by = "year")     %>%
  select(-c(date, trim))

# Deflactar cifras
df_constante <- df_corriente                  %>%
  mutate(
    deflactor = (index_base/index_last)*100,
    across(contains("total"), ~((./deflactor)*100)),
    cifras = paste0("Pesos constantes de ", max(unique(df_indices_2013$year))))

#### 2.2.4. Limpieza final -----------------------------------------------------

#### 2.2.5. Guardar base -------------------------------------------------------

# Renombrar
df_sueldos_pconstantes <- df_constante

# Guardar
save(df_sueldos_pconstantes, file = paste0(out, "df_sueldos_pconstantes.RData"))

# FIN. -------------------------------------------------------------------------
