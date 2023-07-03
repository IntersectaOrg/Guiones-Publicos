#------------------------------------------------------------------------------#
# Proyecto:                   ENDIREH 2021
# Objetivo:                   Figuras del ámbito laboral
#
# Encargadas:                 Fernanda Torres
# Correo:                     ftorres@intersecta.org
# 
# Fecha de creación:          06 de septiembre de 2022
# Última actualización:       08 de noviembre  de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# ---- Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# ---- Cargar librerías 
require(pacman)
p_load(foreign, tidyverse, srvyr, magick, add2ggplot, fauxnaif)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Desactivar notación científica
options(scipen=999)

# ---- Establecer directorios
paste_inp <- function(x){paste0("01_datos_crudos/02_endireh/", x)}
paste_fig <- function(x){paste0("03_figuras/03_endireh/"     , x)}

# 1. Cargar datos --------------------------------------------------------------

# Fuente: https://www.inegi.org.mx/programas/endireh/2021/#Microdatos

v_bases     <- c("TB_SEC_VIII.dbf",   # Ámbito laboral (8.1 a 8.13)
                 "TB_SEC_VIII_2.dbf", # Ámbito laboral (8.14 a 8.29)
                 "TB_SEC_XIX.dbf")    # Discapacidad

# 2. Funciones -----------------------------------------------------------------

## 2.01. Sí/no -----------------------------------------------------------------
codificar_siono <- function(var = x){
    case_when(
        var == 1 ~ "Sí",
        var == 0 ~ "No",
        var == 2 ~ "No"
    )
}

## 2.02. Persona agresora ------------------------------------------------------
codificar_agresor <- function(var = x){
    v_agresor <- c("Patrón(a) o jefe(a)",
                   "Supervisor(a), capataz, coordinador(a)",
                   "Gerente, directivo o ejecutivo",
                   "Compañero(a) de trabajo",
                   "Cliente",
                   "Persona desconocida del trabajo",
                   "Familiar del patrón",
                   "Otra persona del trabajo"
    )
    
    case_when(
        var == 1 ~ v_agresor[1],
        var == 2 ~ v_agresor[2],
        var == 3 ~ v_agresor[3],
        var == 4 ~ v_agresor[4],
        var == 5 ~ v_agresor[5],
        var == 6 ~ v_agresor[6],
        var == 7 ~ v_agresor[7],
        var == 8 ~ v_agresor[8],
    )
}

## 2.03. Lugar de la agresión --------------------------------------------------
codificar_lugar <- function(var = x){
    
    v_lugar <- c("En las instalaciones del trabajo",
                 "En la calle, parque o en un lugar público, cerca del trabajo",
                 "En la calle, parque o en un lugar público, lejos del trabajo",
                 "En el transporte público",
                 "En una casa particular",
                 "Otro")
    
    case_when(
        var == 1 ~ v_lugar[1],
        var == 2 ~ v_lugar[2],
        var == 3 ~ v_lugar[3],
        var == 4 ~ v_lugar[4],
        var == 5 ~ v_lugar[5],
        var == 6 ~ v_lugar[6]
    )
}

## 2.04. Tipo de agresión ------------------------------------------------------
codificar_agresion <- function(var = x){
    
    v_tipos <- c(
        "Insinuaciones sexuales o insultos en línea",
        "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
        "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
        "Represalias porque se negó a tener relaciones sexuales",
        "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
        "Piropos ofensivos",
        'Ofensas o humillaciones por "ser mujer"',
        "Patadas o golpes con el puño",
        "Ataques con cuchillo o arma de fuego",
        "Obligación de ver pornografía",
        "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
        "La han vigilado o seguido al salir del trabajo",
        "Intento de violación",
        "Violación",
        "Manoseos, tocamientos o arrimones sin su consentimiento",
        "Exhibicionismo",
        "La han ignorado por ser mujer",
        "Le han comentado que las mujeres no deberían trabajar",
        "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto"
    )
    
    case_when(
        ((startsWith(var, "p8_12_1_" , trim=TRUE))) ~ v_tipos[1],
        ((startsWith(var, "p8_12_2_" , trim=TRUE))) ~ v_tipos[2],
        ((startsWith(var, "p8_12_3_" , trim=TRUE)) | (startsWith(var, "p8_13_3_" , trim=TRUE))) ~ v_tipos[3],
        ((startsWith(var, "p8_12_4_" , trim=TRUE)) | (startsWith(var, "p8_13_4_" , trim=TRUE))) ~ v_tipos[4],
        ((startsWith(var, "p8_12_5_" , trim=TRUE)) | (startsWith(var, "p8_13_5_" , trim=TRUE))) ~ v_tipos[5],
        ((startsWith(var, "p8_12_6_" , trim=TRUE)) | (startsWith(var, "p8_13_6_" , trim=TRUE))) ~ v_tipos[6],
        ((startsWith(var, "p8_12_7_" , trim=TRUE)) | (startsWith(var, "p8_13_7_" , trim=TRUE))) ~ v_tipos[7],
        ((startsWith(var, "p8_12_8_" , trim=TRUE)) | (startsWith(var, "p8_13_8_" , trim=TRUE))) ~ v_tipos[8],
        ((startsWith(var, "p8_12_9_" , trim=TRUE)) | (startsWith(var, "p8_13_9_" , trim=TRUE))) ~ v_tipos[9],
        ((startsWith(var, "p8_12_10_" , trim=TRUE)) | (startsWith(var, "p8_13_10_" , trim=TRUE))) ~ v_tipos[10],
        ((startsWith(var, "p8_12_11_" , trim=TRUE)) | (startsWith(var, "p8_13_11_" , trim=TRUE))) ~ v_tipos[11],
        ((startsWith(var, "p8_12_12_" , trim=TRUE)) | (startsWith(var, "p8_13_12_" , trim=TRUE))) ~ v_tipos[12],
        ((startsWith(var, "p8_12_13_" , trim=TRUE)) | (startsWith(var, "p8_13_13_" , trim=TRUE))) ~ v_tipos[13],
        ((startsWith(var, "p8_12_14_" , trim=TRUE)) | (startsWith(var, "p8_13_14_" , trim=TRUE))) ~ v_tipos[14],
        ((startsWith(var, "p8_12_15_" , trim=TRUE)) | (startsWith(var, "p8_13_15_" , trim=TRUE))) ~ v_tipos[15],
        ((startsWith(var, "p8_12_16_" , trim=TRUE)) | (startsWith(var, "p8_13_16_" , trim=TRUE))) ~ v_tipos[16],
        ((startsWith(var, "p8_12_17_" , trim=TRUE)) | (startsWith(var, "p8_13_17_" , trim=TRUE))) ~ v_tipos[17],
        ((startsWith(var, "p8_12_18_" , trim=TRUE)) | (startsWith(var, "p8_13_18_" , trim=TRUE))) ~ v_tipos[18],
        ((startsWith(var, "p8_12_19_" , trim=TRUE)) | (startsWith(var, "p8_13_19_" , trim=TRUE))) ~ v_tipos[19]
    )
}

## 2.05. Tipo de violencia -----------------------------------------------------
codificar_violencia <- function(var =x){
    
    v_tipos <- c(
        "Insinuaciones sexuales o insultos en línea",
        "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
        "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
        "Represalias porque se negó a tener relaciones sexuales",
        "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
        "Piropos ofensivos",
        'Ofensas o humillaciones por "ser mujer"',
        "Patadas o golpes con el puño",
        "Ataques con cuchillo o arma de fuego",
        "Obligación de ver pornografía",
        "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
        "La han vigilado o seguido al salir del trabajo",
        "Intento de violación",
        "Violación",
        "Manoseos, tocamientos o arrimones sin su consentimiento",
        "Exhibicionismo",
        "La han ignorado por ser mujer",
        "Le han comentado que las mujeres no deberían trabajar",
        "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto"
    )
    
    case_when(
        var %in% c(v_tipos[7], v_tipos[17], v_tipos[18], v_tipos[6],
                   v_tipos[11], v_tipos[12], v_tipos[2]) ~ "Psicológica",
        var %in% c(v_tipos[8], v_tipos[9], v_tipos[19]) ~ "Física",
        TRUE ~ "Sexual"
    )
}

## 2.06. Apoyo -----------------------------------------------------------------
codificar_apoyo <- function(var = x){
    v_apoyo <- c("Orientación e información",
                 "Apoyo legal",
                 "Apoyo psicológico",
                 "Atención médica",
                 "Otros")
    
    case_when(
        var == 1 ~ v_apoyo[1],
        var == 2 ~ v_apoyo[2],
        var == 3 ~ v_apoyo[3],
        var == 4 ~ v_apoyo[4],
        var == 5 ~ v_apoyo[5]
    )
}

## 2.07. Apoyo por intitución --------------------------------------------------
codificar_institucion <- function(var = x){
    v_institucion <-   c("Instituto de las Mujeres",
                         "Línea de atención telefónica",
                         "Organismo o asociación civil",
                         "Centro de Justicia para las Mujeres",
                         "Defensoría pública",
                         "Centro de Salud público",
                         "Centro de Salud privado",
                         "DIF",
                         "Otra institución")
            
    case_when(
        ((startsWith(var, "p8_19_1_" , trim=TRUE))) ~ v_institucion[1],
        ((startsWith(var, "p8_19_2_" , trim=TRUE))) ~ v_institucion[2],
        ((startsWith(var, "p8_19_3_" , trim=TRUE))) ~ v_institucion[3],
        ((startsWith(var, "p8_19_4_" , trim=TRUE))) ~ v_institucion[4],
        ((startsWith(var, "p8_19_5_" , trim=TRUE))) ~ v_institucion[5],
        ((startsWith(var, "p8_19_6_" , trim=TRUE))) ~ v_institucion[6],
        ((startsWith(var, "p8_19_7_" , trim=TRUE))) ~ v_institucion[7],
        ((startsWith(var, "p8_19_8_" , trim=TRUE))) ~ v_institucion[8],
        ((startsWith(var, "p8_19_9_" , trim=TRUE))) ~ v_institucion[9]
    )
        
}

## 2.08. Agresión --------------------------------------------------------------
codificar_agresion3 <- function(var = x) {
    
    v_tipos <- c(
        "Insinuaciones sexuales o insultos en línea",
        "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
        "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
        "Represalias porque se negó a tener relaciones sexuales",
        "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
        "Piropos ofensivos",
        'Ofensas o humillaciones por "ser mujer"',
        "Patadas o golpes con el puño",
        "Ataques con cuchillo o arma de fuego",
        "Obligación de ver pornografía",
        "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
        "La han vigilado o seguido al salir del trabajo",
        "Intento de violación",
        "Violación",
        "Manoseos, tocamientos o arrimones sin su consentimiento",
        "Exhibicionismo",
        "La han ignorado por ser mujer",
        "Le han comentado que las mujeres no deberían trabajar",
        "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto"
    )
    
    case_when(
        var == 1 ~ v_tipos[1],
        var == 2 ~ v_tipos[2],
        var == 3 ~ v_tipos[3],
        var == 4 ~ v_tipos[4],
        var == 5 ~ v_tipos[5],
        var == 6 ~ v_tipos[6],
        var == 7 ~ v_tipos[7],
        var == 8 ~ v_tipos[8],
        var == 9 ~ v_tipos[9],
        var == 10 ~ v_tipos[10],
        var == 11 ~ v_tipos[11],
        var == 12 ~ v_tipos[12],
        var == 13 ~ v_tipos[13],
        var == 14 ~ v_tipos[14],
        var == 15 ~ v_tipos[15],
        var == 16 ~ v_tipos[16],
        var == 17 ~ v_tipos[17],
        var == 18 ~ v_tipos[18],
        var == 19 ~ v_tipos[19]
    )
    
}

## 2.09. Brindar información ---------------------------------------------------
codificar_info <- function(var = x){
    v_info <- c("Sí",
                "No",
                "Le mandaron a otra dependencia", 
                "No especificado")
    
    case_when(
        var == 1 ~ v_info[1],
        var == 2 ~ v_info[2],
        var == 3 ~ v_info[3],
        var == 4 ~ v_info[4]
    )
}

## 2.10. Atención brindada -----------------------------------------------------
codificar_atencion <- function(var = x){
    v_atencion <- c("La trataron bien y con respeto",
                    "La trataron mal, la humillaron",
                    "No hicieron nada para ayudarla",
                    "No había nadie que la atendiera",
                    "No especificado")
    
    case_when(
        var == 1 ~ v_atencion[1],
        var == 2 ~ v_atencion[2],
        var == 3 ~ v_atencion[3],
        var == 4 ~ v_atencion[4],
        var == 5 ~ v_atencion[5]
    )
}

## 2.11. Lugar de trabajo ------------------------------------------------------
codificar_trabajo <- function(var = x){
    
    v_trabajo = c("Dependencia o institución del gobierno estatal o municipal",
                  "dependencia o institución del gobierno federal",
                  "Escuela o universidad pública",
                  "Clínica u hospital público",
                  "Compañía o empresa del sector privado, comercial, bancaria o de servicios",
                  "Fábrica o taller",
                  "Escuela o universidad privada",
                  "Clínica u hospital particular",
                  "Negocio, local comercial o de servicios",
                  "En la calle o en la vía pública",
                  "Vivienda particular",
                  "Su propia casa",
                  "Campo",
                  "Otra",
                  "No especificado")
    
    case_when(
        var == "01" ~ v_trabajo[1],
        var == "02" ~ v_trabajo[2],
        var == "03" ~ v_trabajo[3],
        var == "04" ~ v_trabajo[4],
        var == "05" ~ v_trabajo[5],
        var == "06" ~ v_trabajo[6],
        var == "07" ~ v_trabajo[7],
        var == "08" ~ v_trabajo[8],
        var == "09" ~ v_trabajo[9],
        var == "10" ~ v_trabajo[10],
        var == "11" ~ v_trabajo[11],
        var == "12" ~ v_trabajo[12],
        var == "13" ~ v_trabajo[13],
        var == "14" ~ v_trabajo[14],
        var == "15" ~ v_trabajo[15]
    )
}


## 2.12. Lugar de trabajo (menos grupos) ---------------------------------------
codificar_trabajo2 <- function(var = x){
    
    v_trabajo <- c("Dependencia o institución del gobierno",
                   "Escuela o universidad",
                   "Clínica u hospital",
                   "Empresa del sector privado, comercial, bancaria o de servicios",
                   "Fábrica o taller",
                   "Negocio, local comercial o de servicios",
                   "Calle o vía pública",
                   "Vivienda particular",
                   "Campo",
                   "Otros",
                   "No especificado")
    
    case_when(
        var == "01" ~ v_trabajo[1],
        var == "02" ~ v_trabajo[1],
        var == "03" ~ v_trabajo[2],
        var == "04" ~ v_trabajo[3],
        var == "05" ~ v_trabajo[4],
        var == "06" ~ v_trabajo[5],
        var == "07" ~ v_trabajo[2],
        var == "08" ~ v_trabajo[3],
        var == "09" ~ v_trabajo[6],
        var == "10" ~ v_trabajo[7],
        var == "11" ~ v_trabajo[8],
        var == "12" ~ v_trabajo[8],
        var == "13" ~ v_trabajo[9],
        var == "14" ~ v_trabajo[10],
        var == "15" ~ v_trabajo[10], 
        var == "99" ~ v_trabajo[11]
        
    )
}

## 2.13. Resolución de denuncia ------------------------------------------------
codificar_resolucion <- function(var = x){
    v_resolucion <- c(
        "No se ratificó la denuncia",
        "No sancionaron a quien agredió",
        "Recomendación de sanción",
        "Sancionaron a quien agredió",
        "La cambiaron de área",
        "Cambiaron a quien agredió de área",
        "Se consignó ante un juez",
        "No hicieron nada",
        "No sabe qué pasó"
    )
    
    case_when(
        endsWith(var, "1") ~ v_resolucion[1],
        endsWith(var, "2") ~ v_resolucion[2],
        endsWith(var, "3") ~ v_resolucion[3],
        endsWith(var, "4") ~ v_resolucion[4],
        endsWith(var, "5") ~ v_resolucion[5],
        endsWith(var, "6") ~ v_resolucion[6],
        endsWith(var, "7") ~ v_resolucion[7],
        endsWith(var, "8") ~ v_resolucion[8],
        endsWith(var, "9") ~ v_resolucion[9]
    )
}

## 2.14. Añadir logo -----------------------------------------------------------
add_intlogo <- function(graf, escala){
    graf_con_logo <- add_logo(
        plot_path = graf,
        logo_path = "logo/corto_blanco.png",
        logo_position = "bottom right",
        logo_scale = escala)
    
    magick::image_write(graf_con_logo, graf)
}

# 3. Procesar datos ------------------------------------------------------------

## 3.1. Unir bases -------------------------------------------------------------

df_pegada <- df_raw2021_1            %>%
    full_join(df_raw2021_2)          %>%
    full_join(df_raw2021_3)

# Eliminar bases que ya no se usarán
rm(list = c("df_raw", "df_raw2021_1", "df_raw2021_2", "df_raw2021_3"))

## 3.2. Codificar --------------------------------------------------------------

### 3.2.1 Tipo de violencias --------------------------------------------------

df_codificada <- df_pegada                                                          %>% 
    janitor::clean_names()                                                          %>%
    # Personas que han trabajado
    filter(p8_1 == 1)                                                               %>% 
    # Convertir a tipo numérico las variables a usar
    mutate(across(starts_with("p8_8_"), ~as.numeric(.)),
           across(starts_with("p8_9_"), ~as.numeric(.)),
           across(starts_with("p8_11_"), ~as.numeric(.)),
           across(starts_with("p8_12_"), ~as.numeric(.)),
           across(starts_with("p8_13_"), ~as.numeric(.)),
           across(starts_with("p8_15"), ~as.numeric(.)),
           across(starts_with("p8_16_"), ~as.numeric(.)),
           across(starts_with("p8_24_"), ~as.numeric(.)),
           across(starts_with("p8_26_"), ~as.numeric(.)),
           across(starts_with("p8_29_"), ~as.numeric(.)),
           across(starts_with("p4_9_"), ~as.numeric(.)))                             %>%
    # Recodificar a opciones binarias (1/0)
    mutate(across(starts_with("p8_8_"), ~if_else(.==1, 1, 0)),
           across(starts_with("p8_9_"), ~if_else(.==1, 1, 0)),
           across(starts_with("p8_11_"), ~if_else(. %in% c("1", "2", "3"), 1, 0)))   %>% 
    mutate(
        # Número de agresiones
        num_discriminacion = rowSums(across(starts_with("p8_8_")), na.rm = TRUE),
        num_agresion_v = rowSums(across(starts_with("p8_9_")), na.rm = TRUE),
        num_agresion_a = rowSums(across(starts_with("p8_11_")), na.rm = TRUE),
        # Número de agresiones por tipo de violencia específico (a lo largo de la vida)
        num_violencia_1_v = p8_9_13 + p8_9_14,
        num_violencia_2_v = p8_9_12,
        num_violencia_3_v = p8_9_4,
        num_violencia_4_v = p8_9_6,
        num_violencia_5_v = p8_9_3,
        num_violencia_6_v = p8_9_1,
        num_violencia_7_v = p8_9_2 + p8_9_7 + p8_9_11 + p8_9_17 + p8_9_18,
        num_violencia_8_v = p8_9_8 + p8_9_9 + p8_9_19,
        num_violencia_9_v = p8_9_5 + p8_9_10 + p8_9_15 + p8_9_16,
        # Número de agresiones por tipo de violencia específico (12 meses)
        num_violencia_1 = p8_11_13 + p8_11_14,
        num_violencia_2 = p8_11_12,
        num_violencia_3 = p8_11_4,
        num_violencia_4 = p8_11_6,
        num_violencia_5 = p8_11_3,
        num_violencia_6 = p8_11_1,
        num_violencia_7 = p8_11_2 + p8_11_7 + p8_11_11 + p8_11_17 + p8_11_18,
        num_violencia_8 = p8_11_8 + p8_11_9 + p8_11_19,
        num_violencia_9 = p8_11_5 + p8_11_10 + p8_11_15 + p8_11_16,
        # Número de agresiones por tipo de violencia (12 meses)
        num_psicologica = p8_11_2 + p8_11_6 + p8_11_7  + p8_11_11 + p8_11_12 + p8_11_17 + p8_11_18,
        num_fisica      = p8_11_8 + p8_11_9 + p8_11_19,
        num_sexual      = p8_11_1 + p8_11_3 + p8_11_4 + p8_11_5 + p8_11_10 + p8_11_13 + p8_11_14 + p8_11_15 + p8_11_16,
        # Vivió agresión
        vivio_discriminacion = if_else(num_discriminacion == 0, 0, 1),
        vivio_agresion_v = if_else(num_agresion_v == 0, 0, 1), 
        vivio_agresion_a = if_else(num_agresion_a == 0, 0, 1),
        # Vivió agresión por tipo de violencia  específico (a lo largo de la vida)
        v_vivio_violencia_1 = if_else(num_violencia_1_v == 0, 0, 1),
        v_vivio_violencia_2 = if_else(num_violencia_2_v == 0, 0, 1),
        v_vivio_violencia_3 = if_else(num_violencia_3_v == 0, 0, 1),
        v_vivio_violencia_4 = if_else(num_violencia_4_v == 0, 0, 1),
        v_vivio_violencia_5 = if_else(num_violencia_5_v == 0, 0, 1),
        v_vivio_violencia_6 = if_else(num_violencia_6_v == 0, 0, 1),
        v_vivio_violencia_7 = if_else(num_violencia_7_v == 0, 0, 1),
        v_vivio_violencia_8 = if_else(num_violencia_8_v == 0, 0, 1),
        v_vivio_violencia_9 = if_else(num_violencia_9_v == 0, 0, 1),
        # Vivió agresión por tipo de violencia  específico (12 meses)
        vivio_violencia_1 = if_else(num_violencia_1 == 0, 0, 1),
        vivio_violencia_2 = if_else(num_violencia_2 == 0, 0, 1),
        vivio_violencia_3 = if_else(num_violencia_3 == 0, 0, 1),
        vivio_violencia_4 = if_else(num_violencia_4 == 0, 0, 1),
        vivio_violencia_5 = if_else(num_violencia_5 == 0, 0, 1),
        vivio_violencia_6 = if_else(num_violencia_6 == 0, 0, 1),
        vivio_violencia_7 = if_else(num_violencia_7 == 0, 0, 1),
        vivio_violencia_8 = if_else(num_violencia_8 == 0, 0, 1),
        vivio_violencia_9 = if_else(num_violencia_9 == 0, 0, 1),
        # Vivió agresión por tipo de violencia (12 meses)
        vivio_psicologica = if_else(num_psicologica == 0, 0, 1),
        vivio_fisica = if_else(num_fisica == 0, 0, 1), 
        vivio_sexual = if_else(num_sexual == 0, 0, 1))                               %>% 
    mutate(
        vivio_violencia_discr = case_when(
            (vivio_discriminacion == 1 | vivio_agresion_a == 1) ~ 1,
            TRUE ~ 0
        ))

# 4. Diseño de encuesta --------------------------------------------------------

# Aplicar el diseño muestral 
df_encuesta <- df_codificada                             %>%
    mutate(id_per = paste0(upm, viv_sel, hogar, n_ren))  %>% 
    # Diseño de encuesta
    as_survey_design(
        ids = upm_dis, strata = est_dis, weights = fac_muj)  

# Método para el caso de 1 UPM en los estratos
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# 5. Figuras -------------------------------------------------------------------

## 5.0. Configuración ----------------------------------------------------------

# ---- Tema
tema <-  theme_linedraw() +
    theme(
        plot.title.position   = "plot",
        plot.caption.position = "plot", 
        text                  = element_text(family = "Fira Sans", color = "black"),
        plot.title            = element_text(family = "Fira Sans", color = "black", size = 15,  face  = "bold",  margin = margin(10,5,5,5)),
        plot.subtitle         = element_text(family = "Fira Sans Medium", color = "#58788C", size = 14, face = "italic", margin = margin(5, 5, 5, 5)),
        plot.caption          = element_text(family = "Fira Sans", face = "italic", color = "black", size = 12,  hjust = 0),
        panel.grid            = element_line(linetype = 2),
        panel.grid.major.y    = element_blank(),
        panel.grid.minor.y    = element_blank(),
        panel.grid.minor.x    = element_blank(),
        plot.margin           = margin(0, 1.5, 0.5, 1.5, "cm"),
        legend.position       = "top",
        panel.border          = element_blank(),
        legend.title          = element_text(size = 8, family = "Fira Sans", face   = "bold"),
        legend.text           = element_text(size = 8, family = "Fira Sans"),
        axis.title            = element_text(size = 8, family = "Fira Sans", hjust = .5, margin = margin(1,1,1,1)),
        axis.text.y           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0,  hjust=1, vjust = 0.5, color = "black"),
        axis.text.x           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0, hjust=.5, vjust = 0.5, color = "#DDDDDD"),
        strip.text.x          = element_text(size = 13, family = "Fira Sans", face = "bold", color = "black"),
        strip.text.y          = element_text(size = 13, family = "Fira Sans", face = "bold", color = "black"), 
        strip.background      = element_rect(fill = "white", color = NA),
        axis.ticks = element_blank())

# ---- Colores
v_colores1 <- c("#000000", "#30475E", "#58788C", "#B3C2C9", "#F05454", "#DDDDDD", "#FCF7F7")
v_colores2 <- c("#B3C2C9", "#30475E", "#F05454")
    
# ---- Vectores de texto 
v_empty <- ""
v_caption <- "Fuente: Encuesta Nacional sobre la Dinámica de las Relaciones en los Hogares (ENDIREH) 2021.
Datos procesados por Intersecta (intersecta.org).\n"

## 5.1. Agresiones durante los últimos 12 meses --------------------------------

#### 5.1.1. Tipo de agresión ---------------------------------------------------

# Preguntas P8_11_(1-19)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_11_"))))

v_tipos <- c(
    "Insinuaciones sexuales o insultos en línea",
    "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
    "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
    "Represalias porque se negó a tener relaciones sexuales",
    "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
    "Piropos ofensivos",
    'Ofensas o humillaciones por "ser mujer"',
    "Patadas o golpes con el puño",
    "Ataques con cuchillo o arma de fuego",
    "Obligación de ver pornografía",
    "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
    "La han vigilado o seguido al salir del trabajo",
    "Intento de violación",
    "Violación",
    "Manoseos, tocamientos o arrimones sin su consentimiento",
    "Exhibicionismo",
    "La han ignorado por ser mujer",
    "Le han comentado que las mujeres no deberían trabajar",
    "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto"
)

# Obtener proporciones 

df_data <- df_encuesta                                                      %>%
    filter(p8_4 == 1)                                                       %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    mutate(respuesta = codificar_siono(as.character(respuesta)))            %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(p8_4 == 1)                                                   %>%
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta)                                                   %>%
        mutate(respuesta = codificar_siono(as.character(respuesta)))        %>%
        srvyr::group_by(respuesta)                                          %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# ---- Figura 
v_title <- "Proporción de mujeres que vivieron violencia en el ámbito laboral"
v_subtitle <- "Por tipo de agresión específica, ocurrida en los 12 meses previos\nal levantamiento de la encuesta\n"

ggplot(
    # Datos
    df_data %>% filter(respuesta == "Sí"), 
    # Coordenadas
    aes(x = porcentaje, y = reorder(tipo, porcentaje))) +
    # Geoms
    geom_col(fill = v_colores1[2], width = 0.8) +
    geom_label(aes(
        label=paste0(round(porcentaje,3)*100, "%"), group = tipo),
        position = position_stack(1), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_subtitle, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption, 
    ) +
    # Escalas
    scale_x_continuous(label = scales::percent_format(),
                       limits = c(0, .052)) +
    scale_y_discrete(labels = scales::wrap_format(40)) +
    # Tema
    tema 

# ---- Guardar en png 
ggsave(paste_fig("04_endireh_laboral_tipo_agresión_total.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 8, height = 10)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("04_endireh_laboral_tipo_agresión_total.png"), escala = 10)

#### 5.1.2. Personas agresoras -------------------------------------------------

# Preguntas P8_12_(1-19)_(1-3) (54 variables)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_12_"))))

## ---- Obtener totales 

# Para primera columna
df_data <- df_encuesta                                                      %>% 
    filter(vivio_agresion_a == 1)                                           %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    mutate(respuesta = codificar_agresor(as.character(respuesta)))          %>% 
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(   
        total = srvyr::survey_total())                                      %>%
    mutate(tipo = codificar_agresion(v_codes[1]))

# Para el resto de columnas
for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>% 
        filter(vivio_agresion_a == 1)                                       %>%
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta)                                                   %>%
        mutate(respuesta = codificar_agresor(as.character(respuesta)))      %>%
        srvyr::group_by(respuesta)                                          %>%
        srvyr::summarise(
            total = srvyr::survey_total())                                  %>%
        mutate(tipo = codificar_agresion(v_codes[i]))
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Renombrar
df_agresor_tipo_a <- df_data

## ---- Obtener proporciones (sin error estandar)
df_data1 <- df_agresor_tipo_a                                               %>%
    filter(!is.na(respuesta))                                               %>%
    group_by(tipo, respuesta)                                               %>%
    summarise(total_final = sum(total))                                     %>% 
    ungroup()                                                               %>% 
    group_by(tipo)                                                          %>% 
    mutate(total_tipo = sum(total_final))                                   %>% 
    # Porcentaje del total de personas agresoras mencionadas
    mutate(porcentaje_final = total_final/total_tipo)

# ---- Datos complementarios

df_complemento <- data.frame(
    tipo = "Ataques con cuchillo o arma de fuego",
    respuesta = c("Supervisor(a), capataz, coordinador(a)", "Familiar del patrón"),
    porcentaje_final = 0
)

# ---- Tabla 
df_tabla_agresor <- df_data1 %>% 
    mutate(porcentaje_final = round(porcentaje_final*100, digits = 1)) %>% 
    full_join(df_complemento) %>% 
    arrange(tipo, desc(porcentaje_final))

# ---- Figura 

v_title <- "Personas que agredieron a mujeres en el ámbito laboral"
v_subtitle <- "Por tipo de agresión específica, ocurrida en los 12 meses previos\nal levantamiento de la encuesta\n"

ggplot(df_tabla_agresor, aes(y = factor(tipo,
                                        levels =  c(
                                            "Ataques con cuchillo o arma de fuego",
                                            "Obligación de ver pornografía",
                                            "Violación",
                                            "Patadas o golpes con el puño",
                                            "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
                                            "Intento de violación",
                                            "Exhibicionismo",
                                            "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto",
                                            "Represalias porque se negó a tener relaciones sexuales",
                                            "La han vigilado o seguido al salir del trabajo",
                                            "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
                                            "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
                                            "Manoseos, tocamientos o arrimones sin su consentimiento",
                                            "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
                                            "La han ignorado por ser mujer",
                                            "Insinuaciones sexuales o insultos en línea",
                                            "Le han comentado que las mujeres no deberían trabajar",
                                            'Ofensas o humillaciones por "ser mujer"',
                                            "Piropos ofensivos"
                                        )), 
                             x = factor(respuesta,
                                        levels = c("Compañero(a) de trabajo",
                                                   "Patrón(a) o jefe(a)",
                                                   "Cliente",
                                                   "Supervisor(a), capataz, coordinador(a)",
                                                   "Gerente, directivo o ejecutivo",
                                                   "Otra persona del trabajo",
                                                   "Persona desconocida del trabajo",
                                                   "Familiar del patrón")), 
                             fill = porcentaje_final)) +
    geom_tile(color = "white",
              lwd = 0.5) +
    geom_label(aes(
        label=paste0(porcentaje_final, "%"), group = porcentaje_final),
        position = position_dodge(0), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_subtitle, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption
    ) +
    # Escalas
    scale_x_discrete(labels = scales::wrap_format(18)) +
    scale_y_discrete(labels = scales::wrap_format(25)) +
    # Tema
    tema  +
    guides(fill = "none") +
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 7, color = "black")) +
    scale_fill_gradient(low=v_colores1[7], high= v_colores1[5])

# ---- Guardar en png 
ggsave(paste_fig("06_endireh_laboral_persona_agresión.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 9, height = 10)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("06_endireh_laboral_persona_agresión.png"), escala = 10)

#### 5.1.3. Lugar donde ocurren las agresiones ---------------------------------

# Preguntas P8_13_(3-19)_(1-3)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_13_"))))

## ---- Obtener totales 

# Para primera columna
df_data <- df_encuesta                                                      %>% 
    filter(vivio_agresion_a == 1)                                           %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    mutate(respuesta = codificar_lugar(as.character(respuesta)))            %>% 
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(   
        total = srvyr::survey_total())                                      %>%
    mutate(tipo = codificar_agresion(v_codes[1]))

# Para el resto de columnas
for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>% 
        filter(vivio_agresion_a == 1)                                       %>%
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta)                                                   %>%
        mutate(respuesta = codificar_lugar(as.character(respuesta)))        %>%
        srvyr::group_by(respuesta)                                          %>%
        srvyr::summarise(
            total = srvyr::survey_total())                                  %>%
        mutate(tipo = codificar_agresion(v_codes[i]))
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Renombrar
df_lugar_tipo_a <- df_data

## ---- Obtener proporciones (sin error estandar)
df_data2 <- df_lugar_tipo_a                    %>%
    filter(!is.na(respuesta))                  %>%
    group_by(tipo, respuesta)                  %>%
    summarise(total_final = sum(total))        %>% 
    ungroup()                                  %>% 
    group_by(tipo)                             %>% 
    mutate(total_lugar = sum(total_final))     %>% 
    # Porcentaje del total de lugares mencionados en dónde ocurrió la agresión 
    mutate(porcentaje_final = total_final/total_lugar)

# ---- Complemento 
df_complemento <- data.frame(
    tipo = c("Violación", "Represalias porque se negó a tener relaciones sexuales", 
             "Patadas o golpes con el puño", "Obligación de ver pornografía",
             "Intento de violación", "Patadas o golpes con el puño", 
             "La han vigilado o seguido al salir del trabajo"),
    respuesta = c("En el transporte público", "En el transporte público", "En el transporte público",
                  "En el transporte público", "En el transporte público", "Otro", "Otro"),
    porcentaje_final = 0
)

# ---- Tabla 
df_tabla_lugar <- df_data2 %>% 
    mutate(porcentaje_final = round(porcentaje_final*100, digits = 1))  %>%
    full_join(df_complemento)                                           %>% 
    arrange(tipo, desc(porcentaje_final))                             

# ---- Figura 

v_title <- "Lugares en los que ocurrieron las agresiones contra mujeres en el ámbito laboral"
v_subtitle <- "Por tipo de agresión específica, ocurrida en los 12 meses previos\nal levantamiento de la encuesta\n"

ggplot(df_tabla_lugar, aes(y = factor(tipo,
                                      levels =  c(
                                          "Ataques con cuchillo o arma de fuego",
                                          "Obligación de ver pornografía",
                                          "Violación",
                                          "Patadas o golpes con el puño",
                                          "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
                                          "Intento de violación",
                                          "Exhibicionismo",
                                          "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto",
                                          "Represalias porque se negó a tener relaciones sexuales",
                                          "La han vigilado o seguido al salir del trabajo",
                                          "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
                                          "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
                                          "Manoseos, tocamientos o arrimones sin su consentimiento",
                                          "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
                                          "La han ignorado por ser mujer",
                                          "Insinuaciones sexuales o insultos en línea",
                                          "Le han comentado que las mujeres no deberían trabajar",
                                          'Ofensas o humillaciones por "ser mujer"',
                                          "Piropos ofensivos"
                                      )), 
                           x = factor(respuesta,
                                      levels = c("En las instalaciones del trabajo",
                                                 "En la calle, parque o en un lugar público, cerca del trabajo",
                                                 "En la calle, parque o en un lugar público, lejos del trabajo",
                                                 "En el transporte público",
                                                 "En una casa particular",
                                                 "Otro")),
                           fill = porcentaje_final)) +
    geom_tile(color = "white",
              lwd = 0.5) +
    geom_label(aes(
        label=paste0(porcentaje_final, "%"), group = porcentaje_final),
        position = position_dodge(0), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_subtitle, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption
    ) +
    # Escalas
    scale_x_discrete(labels = scales::wrap_format(18)) +
    scale_y_discrete(labels = scales::wrap_format(25)) +
    # Tema
    tema  +
    guides(fill = "none") +
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 7, color = "black")) +
    scale_fill_gradient(low=v_colores1[7], high= v_colores1[5])

# ---- Guardar en png 
ggsave(paste_fig("08_endireh_laboral_lugar_agresión.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 9, height = 10)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("08_endireh_laboral_lugar_agresión.png"), escala = 10)

## 5.2. Reporte o queja --------------------------------------------------------

#### 5.2.1. Persona a la que le contaron  --------------------------------------

# Preguntas p8_19(1-10)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_16_"))))

v_tipos <- c(
    "Pareja", "Algún familiar", "Amigo(a) o compañero(a)", "Supervisor(a) o gerente de su trabajo",
    "Otra autoridad del trabajo", "Persona de Sindicato", "Psicólogo(a) o trabajador(a) social",
    "Abogado(a)", "Sacerdote, religiosa o ministro", "Otra persona"
)

# Obtener proporciones 

df_data <- df_encuesta                                                      %>%
    filter(vivio_agresion_v == 1,
           p8_15 == 1)                                                      %>% 
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    mutate(respuesta = codificar_siono(as.character(respuesta)))            %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(vivio_agresion_v == 1,
               p8_15 == 1)                                                  %>% 
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta)                                                   %>%
        mutate(respuesta = codificar_siono(as.character(respuesta)))        %>%
        srvyr::group_by(respuesta)                                          %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# ---- Figura 

v_title <- "Personas a quienes las mujeres contaron lo ocurrido"

ggplot(
    # Datos
    df_data %>% filter(respuesta == "Sí"), 
    # Coordenadas
    aes(x = porcentaje, y = reorder(tipo, porcentaje))) +
    # Geoms
    geom_col(fill = v_colores1[2], width = 0.8) +
    geom_label(aes(
        label=paste0(round(porcentaje,3)*100, "%"), group = tipo),
        position = position_stack(1), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_empty, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption, 
    ) +
    # Escalas
    scale_x_continuous(label = scales::percent_format(),
                       limits = c(0, .62)) +
    scale_y_discrete(labels = scales::wrap_format(35)) +
    # Tema
    tema

# ---- Guardar en png 
ggsave(paste_fig("11_endireh_laboral_contar_persona.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 8, height = 6)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("11_endireh_laboral_contar_persona.png"), escala = 10)

#### 5.2.2. Razones por las que no buscó ayuda o denunció ----------------------

# Preguntas P8_29_(01-11)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_29_"))))

v_tipos <- c("Por vergüenza",
             "Pensó que no le iban a creer o que le iban a decir que era su culpa",
             "Por miedo de las consecuencias o a las amenazas",
             "Porque no quería que su familia se enterara",
             "Porque la convencieron de no hacerlo",
             "Porque se trató de algo sin importancia que no le afectó",
             "Porque esas eran/son las costumbres",
             "No sabía cómo y dónde denunciar",
             "Porque es una pérdida de tiempo o porque no tenía tiempo",
             "No confía en las autoridades del gobierno",
             "Otra")

# Obtener proporciones 

df_data <- df_encuesta                                                      %>%
    filter(vivio_agresion_v == 1,
           p8_17_1 == 2,
           p8_17_2 == 2)                                                    %>% 
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    mutate(respuesta = codificar_siono(as.character(respuesta)))            %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_tipos)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(vivio_agresion_v == 1,
               p8_17_1 == 2,
               p8_17_2 == 2)                                                %>% 
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta)                                                   %>%
        mutate(respuesta = codificar_siono(as.character(respuesta)))        %>%
        srvyr::group_by(respuesta)                                          %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                              %>%
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# ---- Figura 

v_title <- "¿Por qué razones las mujeres no buscaron ayuda o no denunciaron\nla situación de violencia en el ámbito laboral?"

ggplot(
    # Datos
    df_data %>% filter(respuesta == "Sí"), 
    # Coordenadas
    aes(x = porcentaje, y = reorder(tipo, porcentaje))) +
    # Geoms
    geom_col(fill = v_colores1[2], width = 0.8) +
    geom_label(aes(
        label=paste0(round(porcentaje,3)*100, "%"), group = tipo),
        position = position_stack(1), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_empty, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption 
    ) +
    # Escalas
    scale_x_continuous(label = scales::percent_format(),
                       limits = c(0, .37)) +
    scale_y_discrete(labels = scales::wrap_format(40)) +
    # Tema
    tema

# ---- Guardar en png 
ggsave(paste_fig("14_endireh_laboral_razón.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 8, height = 6)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("14_endireh_laboral_razón.png"), escala = 10)

#### 5.2.3. Tipo de información solicitada -------------------------------------

# Preguntas P8_19_(1-9)_(1-3)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_19_"))))

# Obtener proporciones 
df_data <- df_encuesta                                                      %>%
    filter(vivio_agresion_v == 1,
           p8_17_1 == 1)                                                    %>% 
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    filter(!is.na(respuesta))                                               %>% 
    mutate(respuesta = codificar_apoyo(as.character(respuesta)))            %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = srvyr::survey_total())                                      %>% 
    mutate(tipo = codificar_institucion(v_codes[1]))

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                                 %>%
        filter(vivio_agresion_v == 1,
               p8_17_1 == 1)                                                    %>% 
        rename(respuesta = v_codes[i])                                          %>%
        select(respuesta)                                                       %>%
        filter(!is.na(respuesta))                                               %>% 
        mutate(respuesta = codificar_apoyo(as.character(respuesta)))            %>%
        srvyr::group_by(respuesta)                                              %>%
        srvyr::summarise(
            total = srvyr::survey_total())                                      %>% 
        mutate(tipo = codificar_institucion(v_codes[i]))
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# ---- Figura por institución a la que acudió
v_title <- "Tipo de apoyo solicitado por la situación de violencia en el ámbito laboral"
v_subtitle <- "Por institución a la que acudieron las mujeres"

# Obtener proporciones
df_data1 <- df_data                       %>% 
    group_by(tipo, respuesta)             %>% 
    summarise(total = sum(total))         %>% 
    ungroup()                             %>% 
    group_by(tipo)                        %>% 
    mutate(total_tipo = sum(total))       %>% 
    mutate(porcentaje = total/total_tipo) %>% 
    mutate(porcentaje = round(porcentaje*100, digits = 1)) %>% 
    select(tipo, respuesta, porcentaje)

# Agregar proporciones con 0% 
tipo <- c("DIF", "Centro de Salud privado", "Centro de Justicia para las Mujeres")
respuesta <- c("Atención médica", "Otros", "Atención médica")
porcentaje <- c(0, 0, 0)
df_complemento <- data.frame(cbind(tipo, respuesta, porcentaje)) %>% 
    mutate(porcentaje = as.numeric(porcentaje))

# Unir las bases
df_data2 <- df_data1 %>% 
    full_join(df_complemento)

# Figura
ggplot(df_data2, aes(y = factor(tipo,
                                levels = c("Otra institución",
                                           "DIF",
                                           "Línea de atención telefónica",
                                           "Centro de Salud privado",
                                           "Centro de Salud público",
                                           "Organismo o asociación civil",
                                           "Centro de Justicia para las Mujeres",
                                           "Defensoría pública",
                                           "Instituto de las Mujeres")), 
                     x = factor(respuesta,
                                levels = c("Orientación e información",
                                           "Apoyo legal",
                                           "Apoyo psicológico",
                                           "Atención médica",
                                           "Otros")), 
                     fill = porcentaje)) +
    geom_tile(color = "white",
              lwd = 0.5) +
    geom_label(aes(
        label=paste0(porcentaje, "%"), group = porcentaje),
        position = position_dodge(0), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_subtitle, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption
    ) +
    # Escalas
    scale_x_discrete(labels = scales::wrap_format(15)) +
    scale_y_discrete(labels = scales::wrap_format(20)) +
    # Tema
    tema  +
    guides(fill = "none") +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10, color = "black"),
          text        = element_text(size=10, family="Gotham")) +
    scale_fill_gradient(low=v_colores1[7], high= v_colores1[5]) 

# Guardar en png 
ggsave(paste_fig("18_endireh_laboral_tipo_apoyo_institucion.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 8, height = 8)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("18_endireh_laboral_tipo_apoyo_institucion.png"), escala = 10)

#### 5.3.4.  Recibieron la denuncia --------------------------------------------

df_data <- df_encuesta %>% 
    filter(vivio_agresion_v == 1,
           p8_17_2 == 1) %>% 
    mutate(
        recibir_trabajo = case_when(
            (p8_27_1_1 == "01") ~ "Iniciaron investigación",
            (p8_27_1_1 == "02") ~ "No iniciaron investigación",
            (p8_27_1_1 == "03" | p8_27_1_1 == "04" | p8_27_1_1 == "05" | 
                 p8_27_1_1 == "06" | p8_27_1_1 == "07" | p8_27_1_1 == "08" |
                 p8_27_1_1 == "09" | p8_27_1_1 == "10") ~ "No recibieron la denuncia"),
        recibir_fiscalia = case_when(
            (p8_27_2_1 == "01") ~ "Iniciaron investigación",
            (p8_27_2_1 == "02") ~ "No iniciaron investigación",
            (p8_27_2_1 == "03" | p8_27_2_1 == "04" | p8_27_2_1 == "05" | 
                 p8_27_2_1 == "06" | p8_27_2_1 == "07" | p8_27_2_1 == "08" |
                 p8_27_2_1 == "09" | p8_27_2_1 == "10") ~ "No recibieron la denuncia"),
        recibir_policia = case_when(
            (p8_27_3_1 == "01") ~ "Iniciaron investigación",
            (p8_27_3_1 == "02") ~ "No iniciaron investigación",
            (p8_27_3_1 == "03" | p8_27_3_1 == "04" | p8_27_3_1 == "05" | 
                 p8_27_3_1 == "06" | p8_27_3_1 == "07" | p8_27_3_1 == "08" |
                 p8_27_3_1 == "09" | p8_27_3_1 == "10") ~ "No recibieron la denuncia"),
        recibir_procuraduria = case_when(
            (p8_27_4_1 == "01") ~ "Iniciaron investigación",
            (p8_27_4_1 == "02") ~ "No iniciaron investigación",
            (p8_27_4_1 == "03" | p8_27_4_1 == "04" | p8_27_4_1 == "05" | 
                 p8_27_4_1 == "06" | p8_27_4_1 == "07" | p8_27_4_1 == "08" |
                 p8_27_4_1 == "09" | p8_27_4_1 == "10") ~ "No recibieron la denuncia"),
        recibir_municipales = case_when(
            (p8_27_5_1 == "01") ~ "Iniciaron investigación",
            (p8_27_5_1 == "02") ~ "No iniciaron investigación",
            (p8_27_5_1 == "03" | p8_27_5_1 == "04" | p8_27_5_1 == "05" | 
                 p8_27_5_1 == "06" | p8_27_5_1 == "07" | p8_27_5_1 == "08" |
                 p8_27_5_1 == "09" | p8_27_5_1 == "10") ~ "No recibieron la denuncia")
    )

# Verificar codificación
df_verificar <- df_data                                  %>% 
    filter(
        p8_24_1 == 1,
        recibir_trabajo == "No recibieron la denuncia")  %>% 
    group_by(p8_27_1_1)                                  %>% 
    survey_count()

# Obtener proporciones

v_codes <- c(names(df_data$variables %>% select(starts_with("recibir_"))))
v_filtro <- c(names(df_data$variables %>% select(starts_with("p8_24"))))
v_tipos <- c("Autoridades de su trabajo o del sindicato", 
             "Fiscalía, Procuraduría o Ministerio Público",
             "Policía",
             "Procuraduría de la Defensa del Trabajo",
             "Autoridades municipales o de las alcaldías")


# Obtener proporciones 

df_data1 <- df_data                                                         %>%
    rename(filtro_i = v_filtro[1])                                          %>% 
    filter(filtro_i == 1)                                                   %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1],
           recibir = case_when(
               respuesta == "No recibieron la denuncia" ~ "No recibieron la denuncia",
               respuesta %in% c("Iniciaron investigación", "No iniciaron investigación") ~ "Recibieron la denuncia"
           ),
           investigacion = case_when(
               respuesta == "No iniciaron investigación" ~ "No iniciaron investigación\n",
               respuesta == "Iniciaron investigación"    ~ "Iniciaron investigación\n",
               respuesta == "No recibieron la denuncia" ~ ""
           ))

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data1_loop <- df_data                                                    %>%
        rename(filtro_i = v_filtro[i])                                          %>% 
        filter(filtro_i == 1)                                                   %>%
        rename(respuesta = v_codes[i])                                          %>%
        select(respuesta)                                                       %>%
        srvyr::group_by(respuesta)                                              %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                                  %>% 
        mutate(tipo = v_tipos[i],
               recibir = case_when(
                   respuesta == "No recibieron la denuncia" ~ "No recibieron la denuncia",
                   respuesta %in% c("Iniciaron investigación", "No iniciaron investigación") ~ "Recibieron la denuncia"
               ),
               investigacion = case_when(
                   respuesta == "No iniciaron investigación" ~ "No iniciaron investigación\n",
                   respuesta == "Iniciaron investigación"    ~ "Iniciaron investigación\n",
                   respuesta == "No recibieron la denuncia" ~ ""
               ))
    
    df_data1 <- df_data1 %>% bind_rows(df_data1_loop)
}

# Renombrar 
df_recibir_queja <- df_data1

# ---- Figuras por autoridad

v_tipos_title <- c("el sindicato o\nlas autoridades de su trabajo",
                   "la Fiscalía,\nla Procuraduría o el Ministerio Público",
                   "la policía",
                   "la Procuraduría\nde la Defensa del Trabajo",
                   "las autoridades\nmunicipales o de las alcadías")

for(i in 1:length(v_tipos)){
    
    print(v_tipos[i])
    v_title <- paste0("¿Qué sucede cuando las mujeres denuncian ante ", v_tipos_title[i], "?")
    
    # Figura
    ggplot(
        # Datos
        df_recibir_queja %>% filter(tipo == v_tipos[i]), 
        # Coordenadas
        aes(x = recibir, y = porcentaje, fill = respuesta)) +
        # Geoms
        geom_bar(aes(fill = respuesta), width = 0.8, stat = "identity") +
        geom_label(aes(
            label=paste0(investigacion, round(porcentaje,3)*100, "%"), group = investigacion),
            position = position_stack(vjust=0.5), size=3, 
            angle = 0, fill = "white",
            color="black",  family = "Fira Sans") +
        labs(title = v_title,
             subtitle = v_empty,
             x = v_empty,
             y = v_empty,
             caption = v_caption) +
        guides(fill = "none") +
        # Escalas
        scale_y_continuous(label = scales::percent_format(),
                           limits = c(0, 1)) +
        scale_x_discrete(labels = scales::wrap_format(40)) +
        # Tema
        tema  +
        theme(
            axis.text.y           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0,  hjust=1, vjust = 0.5, color = "#DDDDDD"),
            axis.text.x           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0, hjust=.5, vjust = 0.5, color = "black"),
            panel.grid.major.y    = element_line(linetype = 2),
            panel.grid.major.x    = element_blank(),
            panel.grid.minor.x    = element_blank(),
            panel.grid.minor.y    = element_blank()
        ) +
        scale_fill_manual(values = v_colores)
    
    # ---- Guardar en png 
    v_name <- paste0("33_endireh_laboral_denuncia_recibir_", i, ".png")
    
    ggsave(paste_fig(v_name), 
           device = "png", type = "cairo", # para guardar fuentes 
           width = 8, height = 6)
    
    # Añadir logo de INTR
    add_intlogo(graf = paste_fig(v_name), escala = 10)
    
}

#### 5.3.5.  Resolución de la denuncia -----------------------------------------

# Creación de dummies sobre resolución 

df_data <- df_encuesta %>% 
    mutate(
        # Autoridad del trabajo o Sindicato, resolucion
        resolucion_1_1 = case_when(
            (p8_28_1_1 == 1 | p8_28_1_2 == 1 | p8_28_1_3 == 1) ~ 1,
            TRUE ~ 0),
        resolucion_1_2 = case_when(
            (p8_28_1_1 == 2 | p8_28_1_2 == 2 | p8_28_1_3 == 2) ~ 1,
            TRUE ~ 0),
        resolucion_1_3 = case_when(
            (p8_28_1_1 == 3 | p8_28_1_2 == 3 | p8_28_1_3 == 3) ~ 1,
            TRUE ~ 0),
        resolucion_1_4 = case_when(
            (p8_28_1_1 == 4 | p8_28_1_2 == 4 | p8_28_1_3 == 4) ~ 1,
            TRUE ~ 0),
        resolucion_1_5 = case_when(
            (p8_28_1_1 == 5 | p8_28_1_2 == 5 | p8_28_1_3 == 5) ~ 1,
            TRUE ~ 0),
        resolucion_1_6 = case_when(
            (p8_28_1_1 == 6 | p8_28_1_2 == 6 | p8_28_1_3 == 6) ~ 1,
            TRUE ~ 0),
        resolucion_1_7 = case_when(
            (p8_28_1_1 == 7 | p8_28_1_2 == 7 | p8_28_1_3 == 7) ~ 1,
            TRUE ~ 0),
        resolucion_1_8 = case_when(
            (p8_28_1_1 == 8 | p8_28_1_2 == 8 | p8_28_1_3 == 8) ~ 1,
            TRUE ~ 0),
        resolucion_1_9 = case_when(
            (p8_28_1_1 == 9 | p8_28_1_2 == 9 | p8_28_1_3 == 9) ~ 1,
            TRUE ~ 0),
        # Fiscalía, resolución
        resolucion_2_1 = case_when(
            (p8_28_2_1 == 1 | p8_28_2_2 == 1 | p8_28_2_3 == 1) ~ 1,
            TRUE ~ 0),
        resolucion_2_2 = case_when(
            (p8_28_2_1 == 2 | p8_28_2_2 == 2 | p8_28_2_3 == 2) ~ 1,
            TRUE ~ 0),
        resolucion_2_3 = case_when(
            (p8_28_2_1 == 3 | p8_28_2_2 == 3 | p8_28_2_3 == 3) ~ 1,
            TRUE ~ 0),
        resolucion_2_4 = case_when(
            (p8_28_2_1 == 4 | p8_28_2_2 == 4 | p8_28_2_3 == 4) ~ 1,
            TRUE ~ 0),
        resolucion_2_5 = case_when(
            (p8_28_2_1 == 5 | p8_28_2_2 == 5 | p8_28_2_3 == 5) ~ 1,
            TRUE ~ 0),
        resolucion_2_6 = case_when(
            (p8_28_2_1 == 6 | p8_28_2_2 == 6 | p8_28_2_3 == 6) ~ 1,
            TRUE ~ 0),
        resolucion_2_7 = case_when(
            (p8_28_2_1 == 7 | p8_28_2_2 == 7 | p8_28_2_3 == 7) ~ 1,
            TRUE ~ 0),
        resolucion_2_8 = case_when(
            (p8_28_2_1 == 8 | p8_28_2_2 == 8 | p8_28_2_3 == 8) ~ 1,
            TRUE ~ 0),
        resolucion_2_9 = case_when(
            (p8_28_2_1 == 9 | p8_28_2_2 == 9 | p8_28_2_3 == 9) ~ 1,
            TRUE ~ 0),
        # Policía
        resolucion_3_1 = case_when(
            (p8_28_3_1 == 1 | p8_28_3_2 == 1 | p8_28_3_3 == 1) ~ 1,
            TRUE ~ 0),
        resolucion_3_2 = case_when(
            (p8_28_3_1 == 2 | p8_28_3_2 == 2 | p8_28_3_3 == 2) ~ 1,
            TRUE ~ 0),
        resolucion_3_3 = case_when(
            (p8_28_3_1 == 3 | p8_28_3_2 == 3 | p8_28_3_3 == 3) ~ 1,
            TRUE ~ 0),
        resolucion_3_4 = case_when(
            (p8_28_3_1 == 4 | p8_28_3_2 == 4 | p8_28_3_3 == 4) ~ 1,
            TRUE ~ 0),
        resolucion_3_5 = case_when(
            (p8_28_3_1 == 5 | p8_28_3_2 == 5 | p8_28_3_3 == 5) ~ 1,
            TRUE ~ 0),
        resolucion_3_6 = case_when(
            (p8_28_3_1 == 6 | p8_28_3_2 == 6 | p8_28_3_3 == 6) ~ 1,
            TRUE ~ 0),
        resolucion_3_7 = case_when(
            (p8_28_3_1 == 7 | p8_28_3_2 == 7 | p8_28_3_3 == 7) ~ 1,
            TRUE ~ 0),
        resolucion_3_8 = case_when(
            (p8_28_3_1 == 8 | p8_28_3_2 == 8 | p8_28_3_3 == 8) ~ 1,
            TRUE ~ 0),
        resolucion_3_9 = case_when(
            (p8_28_3_1 == 9 | p8_28_3_2 == 9 | p8_28_3_3 == 9) ~ 1,
            TRUE ~ 0),
        # Procuraduría de la Defensa del Trabajo
        resolucion_4_1 = case_when(
            (p8_28_4_1 == 1 | p8_28_4_2 == 1 | p8_28_4_3 == 1) ~ 1,
            TRUE ~ 0),
        resolucion_4_2 = case_when(
            (p8_28_4_1 == 2 | p8_28_4_2 == 2 | p8_28_4_3 == 2) ~ 1,
            TRUE ~ 0),
        resolucion_4_3 = case_when(
            (p8_28_4_1 == 3 | p8_28_4_2 == 3 | p8_28_4_3 == 3) ~ 1,
            TRUE ~ 0),
        resolucion_4_4 = case_when(
            (p8_28_4_1 == 4 | p8_28_4_2 == 4 | p8_28_4_3 == 4) ~ 1,
            TRUE ~ 0),
        resolucion_4_5 = case_when(
            (p8_28_4_1 == 5 | p8_28_4_2 == 5 | p8_28_4_3 == 5) ~ 1,
            TRUE ~ 0),
        resolucion_4_6 = case_when(
            (p8_28_4_1 == 6 | p8_28_4_2 == 6 | p8_28_4_3 == 6) ~ 1,
            TRUE ~ 0),
        resolucion_4_7 = case_when(
            (p8_28_4_1 == 7 | p8_28_4_2 == 7 | p8_28_4_3 == 7) ~ 1,
            TRUE ~ 0),
        resolucion_4_8 = case_when(
            (p8_28_4_1 == 8 | p8_28_4_2 == 8 | p8_28_4_3 == 8) ~ 1,
            TRUE ~ 0),
        resolucion_4_9 = case_when(
            (p8_28_4_1 == 9 | p8_28_4_2 == 9 | p8_28_4_3 == 9) ~ 1,
            TRUE ~ 0),
        # Autoridades municipales
        resolucion_5_1 = case_when(
            (p8_28_5_1 == 1 | p8_28_5_2 == 1 | p8_28_5_3 == 1) ~ 1,
            TRUE ~ 0),
        resolucion_5_2 = case_when(
            (p8_28_5_1 == 2 | p8_28_5_2 == 2 | p8_28_5_3 == 2) ~ 1,
            TRUE ~ 0),
        resolucion_5_3 = case_when(
            (p8_28_5_1 == 3 | p8_28_5_2 == 3 | p8_28_5_3 == 3) ~ 1,
            TRUE ~ 0),
        resolucion_5_4 = case_when(
            (p8_28_5_1 == 4 | p8_28_5_2 == 4 | p8_28_5_3 == 4) ~ 1,
            TRUE ~ 0),
        resolucion_5_5 = case_when(
            (p8_28_5_1 == 5 | p8_28_5_2 == 5 | p8_28_5_3 == 5) ~ 1,
            TRUE ~ 0),
        resolucion_5_6 = case_when(
            (p8_28_5_1 == 6 | p8_28_5_2 == 6 | p8_28_5_3 == 6) ~ 1,
            TRUE ~ 0),
        resolucion_5_7 = case_when(
            (p8_28_5_1 == 7 | p8_28_5_2 == 7 | p8_28_5_3 == 7) ~ 1,
            TRUE ~ 0),
        resolucion_5_8 = case_when(
            (p8_28_5_1 == 8 | p8_28_5_2 == 8 | p8_28_5_3 == 8) ~ 1,
            TRUE ~ 0),
        resolucion_5_9 = case_when(
            (p8_28_5_1 == 9 | p8_28_5_2 == 9 | p8_28_5_3 == 9) ~ 1,
            TRUE ~ 0)
    ) 


# Proporciones 

df_data2 <- data.frame(autoridad = character(),
                       resolucion = character(),
                       respuesta = character(),
                       total = numeric(),
                       porcentaje = numeric())

v_tipos <- c("Autoridades de su trabajo o del sindicato", 
             "Fiscalía, Procuraduría o Ministerio Público",
             "Policía",
             "Procuraduría de la Defensa del Trabajo",
             "Autoridades municipales o de las alcaldías")

v_filtro <- c("p8_27_1_1", "p8_27_2_1", "p8_27_3_1", "p8_27_4_1", "p8_27_5_1")

# Test loop completo
for (i in 1:length(v_tipos)) {
    s_resolucion <- paste0("resolucion_", i)
    v_codes <- c(names(df_data$variables %>% select(starts_with(s_resolucion))))
    
    for (j in 1:length(v_codes)) {
        print(paste0("vuelta ", j, " de autoridad ", i))
        df_data2_loop <- df_data %>% 
            rename(filtro_i = v_filtro[i],
                   respuesta = v_codes[j]) %>% 
            filter(filtro_i %in% c("01", "'2")) %>% 
            select(respuesta) %>% 
            group_by(respuesta) %>% 
            summarise(
                total = survey_total(),
                porcentaje = survey_prop()
            ) %>% 
            mutate(autoridad = v_tipos[i],
                   resolucion = codificar_resolucion(v_codes[j]),
                   respuesta = codificar_siono(respuesta)) %>% 
            select(autoridad, resolucion, respuesta, total, porcentaje)
        
        df_data2 <- df_data2 %>% full_join(df_data2_loop)
    }
}

# Cambiar nombre 
df_resolucion_autoridad <- df_data2

# ---- Figuras por autoridad 

v_tipos_title <- c("el sindicato o las autoridades de su trabajo",
                   "la Fiscalía, la Procuraduría o el Ministerio Público",
                   "la policía",
                   "la Procuraduría de la Defensa del Trabajo",
                   "las autoridades municipales o de las alcadías")

for(i in 1:length(v_tipos)){
    
    print(v_tipos[i])
    v_title <- paste0("¿Cuál fue el resultado de las denuncias presentadas ante\n", v_tipos_title[i], "?")
    
    # Datos 
    df_data_loop <- df_resolucion_autoridad   %>% 
        filter(autoridad == v_tipos[i],
               respuesta == "Sí")
    v_max <- max(df_data_loop$porcentaje)*1.05
    
    # Figura
    ggplot(df_data_loop,
           aes(x = porcentaje, y = reorder(resolucion, porcentaje))) +
        # Geoms
        geom_col(fill = v_colores1[2], width = 0.8) +
        geom_label(aes(
            label=paste0(round(porcentaje,3)*100, "%"), group = resolucion),
            position = position_stack(1), size=3, hjust=.5, vjust=.5, 
            angle = 0, fill = "white",
            color="black",  family = "Fira Sans") +
        labs(title = v_title,
             subtitle = v_subtitle,
             x = v_empty,
             y = v_empty,
             caption = v_caption) +
        # Escalas
        scale_x_continuous(label = scales::percent_format(),
                           limits = c(0, v_max)) +
        scale_y_discrete(labels = scales::wrap_format(40)) +
        # Tema
        tema  
    
    # ---- Guardar en png 
    v_name <- paste0("34_endireh_laboral_denuncia_resolucion_", i, ".png")
    
    ggsave(paste_fig(v_name), 
           device = "png", type = "cairo", # para guardar fuentes 
           width = 8, height = 6)
    
    # Añadir logo de INTR
    add_intlogo(graf = paste_fig(v_name), escala = 10)
    
}

### 5.3.6. Situación por la que denunció ---------------------------------------
# % respecto a la incidencia 

# ---- Total de delitos
# Preguntas P8_11_(1-19)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_9_"))))

v_tipos <- c(
    "Insinuaciones sexuales o insultos en línea",
    "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
    "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
    "Represalias porque se negó a tener relaciones sexuales",
    "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
    "Piropos ofensivos",
    'Ofensas o humillaciones por "ser mujer"',
    "Patadas o golpes con el puño",
    "Ataques con cuchillo o arma de fuego",
    "Obligación de ver pornografía",
    "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
    "La han vigilado o seguido al salir del trabajo",
    "Intento de violación",
    "Violación",
    "Manoseos, tocamientos o arrimones sin su consentimiento",
    "Exhibicionismo",
    "La han ignorado por ser mujer",
    "Le han comentado que las mujeres no deberían trabajar",
    "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto"
)

# Obtener proporciones 

df_data <- df_encuesta                                                      %>%
    filter(p8_1 == 1)                                                       %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    mutate(respuesta = codificar_siono(as.character(respuesta)))            %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = srvyr::survey_total())                                  %>% 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                             %>%
        filter(p8_1 == 1)                                                   %>%
        rename(respuesta = v_codes[i])                                      %>%
        select(respuesta)                                                   %>%
        mutate(respuesta = codificar_siono(as.character(respuesta)))        %>%
        srvyr::group_by(respuesta)                                          %>%
        srvyr::summarise(
            total = srvyr::survey_total())                                  %>%
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Renombrar 
df_tipo_agresion <- df_data

# Total de situaciones no agrupadas
df_data_agresion1 <- df_tipo_agresion   %>% 
    filter(respuesta == "Sí")           %>% 
    select(tipo, total)

# Total de situaciones agrupadas 
df_data_agresion2 <- df_tipo_agresion          %>%
    mutate(tipo = codificar_agresion2(tipo))   %>% 
    group_by(tipo, respuesta)                  %>%
    summarise(total_tipo = sum(total))         %>% 
    ungroup()                                  %>% 
    filter(respuesta == "Sí")                  %>% 
    select(tipo, total_tipo)

# ---- Denuncias por tipo

# Preguntas P8_26_(1-5)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_26_"))))

v_tipos <- c("Autoridades de su trabajo o del Sindicato", 
             "Fiscalía, Procuraduría o Ministerio Público",
             "Policía",
             "Procuraduría de la Defensa del Trabajo",
             "Autoridades municipales o de las alcaldías")


df_data <- df_encuesta                                                      %>%
    filter(vivio_agresion_v == 1,
           p8_17_2 == 1)                                                    %>% 
    rename(respuesta = v_codes[1])                                          %>%
    select(respuesta)                                                       %>%
    filter(!is.na(respuesta))                                               %>% 
    mutate(respuesta = codificar_agresion3(as.character(respuesta)))        %>%
    srvyr::group_by(respuesta)                                              %>%
    srvyr::summarise(
        total = srvyr::survey_total())                                      %>% 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                                 %>%
        filter(vivio_agresion_v == 1,
               p8_17_2 == 1)                                                    %>% 
        rename(respuesta = v_codes[i])                                          %>%
        select(respuesta)                                                       %>%
        filter(!is.na(respuesta))                                               %>% 
        mutate(respuesta = codificar_agresion3(as.character(respuesta)))        %>%
        srvyr::group_by(respuesta)                                              %>%
        srvyr::summarise(
            total = srvyr::survey_total())                                      %>% 
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

# Renombrar 
df_tipo_agresion_denuncia <- df_data

# Total de denuncias no agrupadas
df_data_denuncia1 <- df_tipo_agresion_denuncia   %>% 
    filter(!is.na(respuesta))                    %>%
    mutate(tipo = respuesta)                     %>% 
    group_by(tipo)                               %>%  
    summarise(total_denuncia = sum(total))       

# Total de denuncias agrupadas 
df_data_denuncia2 <- df_tipo_agresion_denuncia      %>%
    filter(!is.na(respuesta))                       %>%
    mutate(tipo = codificar_agresion2(respuesta))   %>% 
    group_by(tipo)                                  %>%
    summarise(total_denuncia = sum(total))          %>% 
    ungroup()                                  

# ---- Figura por agresiones no agrupadas

# Unir bases y obtener porcentaje
df_data <- df_data_agresion1                         %>% 
    full_join(df_data_denuncia1)                     %>%
    mutate(porcentaje = total_denuncia/total)        %>% 
    mutate(porcentaje = replace_na(porcentaje, 0))

# Figura 
v_title <- "Del total de agresiones mencionadas en el ámbito laboral,\n¿qué porcentaje se denunciaron?"
v_subtitle <- "Por tipo de agresión\n"

# Figura   
ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = porcentaje, y = reorder(tipo, porcentaje))) +
    # Geoms
    geom_col(fill = v_colores1[2], width = 0.8) +
    geom_label(aes(
        label=paste0(round(porcentaje,3)*100, "%"), group = tipo),
        position = position_stack(1), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_subtitle, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption, 
    ) +
    # Escalas
    scale_x_continuous(label = scales::percent_format(),
                       limits = c(-.001, .2)) +
    scale_y_discrete(labels = scales::wrap_format(40)) +
    # Tema
    tema  

# ---- Guardar en png 
ggsave(paste_fig("28_endireh_laboral_denuncia_agresión2.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 8, height = 10)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("28_endireh_laboral_denuncia_agresión2.png"), escala = 10)

## 5.3. Perfil de las mujeres --------------------------------------------------

#### 5.3.1. Agresión específica por lugar de trabajo ---------------------------

# Preguntas P8_11_(1-19)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_11_"))))

v_tipos <- c(
    "Insinuaciones sexuales o insultos en línea",
    "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
    "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
    "Represalias porque se negó a tener relaciones sexuales",
    "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
    "Piropos ofensivos",
    'Ofensas o humillaciones por "ser mujer"',
    "Patadas o golpes con el puño",
    "Ataques con cuchillo o arma de fuego",
    "Obligación de ver pornografía",
    "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
    "La han vigilado o seguido al salir del trabajo",
    "Intento de violación",
    "Violación",
    "Manoseos, tocamientos o arrimones sin su consentimiento",
    "Exhibicionismo",
    "La han ignorado por ser mujer",
    "Le han comentado que las mujeres no deberían trabajar",
    "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto"
)

# Obtener proporciones 

df_data <- df_encuesta                                                      %>%
    filter(p8_4 == 1)                                                       %>%
    filter(p8_5 %in% c("1", "2", "3"))                                      %>%
    mutate(lugar_trabajo = codificar_trabajo2(p8_7))                        %>% 
    rename(respuesta = v_codes[1])                                          %>%
    select(lugar_trabajo, respuesta)                                        %>%
    mutate(respuesta = codificar_siono(as.character(respuesta)))            %>%
    srvyr::group_by(lugar_trabajo, respuesta)                               %>%
    srvyr::summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_encuesta                                                 %>%
        filter(p8_4 == 1)                                                       %>%
        filter(p8_5 %in% c("1", "2", "3"))                                      %>%
        mutate(lugar_trabajo = codificar_trabajo2(p8_7))                        %>% 
        rename(respuesta = v_codes[i])                                          %>%
        select(lugar_trabajo, respuesta)                                        %>%
        mutate(respuesta = codificar_siono(as.character(respuesta)))            %>%
        srvyr::group_by(lugar_trabajo, respuesta)                               %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                                  %>% 
        mutate(tipo = v_tipos[i])
    
    df_data <- df_data %>% bind_rows(df_data_loop)
}

df_complemento <- data.frame(
    respuesta = "Sí",
    lugar_trabajo = c("Calle o vía pública", "Clínica u hospital", "Campo", "Campo"),
    tipo = c("Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
             "Violación", "Obligación de ver pornografía", "Ataques con cuchillo o arma de fuego"),
    porcentaje = 0
)

# Renombrar 
df_lugar_trabajo_agresion <- df_data %>% 
    full_join(df_complemento)

# Verifiquemos 
df_data1 <- df_lugar_trabajo_agresion       %>% 
    group_by(lugar_trabajo, tipo)           %>% 
    mutate(total_lugar = sum(total))        %>% 
    mutate(porcentaje = total/total_lugar)

# ---- Figura 
# Del total de mujeres que trabajaron en cada lugar de trabajo, ¿qué porcentaje reportó haber vivido 
# alguna agresión en el ámbito laboral? 
# Por tipo de agresión

v_title <- "Del total de mujeres que trabajaron, ¿qué porcentaje reportó haber vivido\nagresiones en los 12 meses previos al levantamiento de la encuesta?"
v_subtitle <- "Por tipo de agresión específica, según lugar de trabajo\n"

ggplot(df_lugar_trabajo_agresion %>% filter(!lugar_trabajo %in% c("Otros", "No especificado"),
                           respuesta == "Sí"), 
       aes(y = factor(tipo,
                      levels = c(
                          "Ataques con cuchillo o arma de fuego",
                          "Obligación de ver pornografía",
                          "Violación",
                          "Patadas o golpes con el puño",
                          "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
                          "Intento de violación",
                          "Exhibicionismo",
                          "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto",
                          "Represalias porque se negó a tener relaciones sexuales",
                          "La han vigilado o seguido al salir del trabajo",
                          "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
                          "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
                          "Manoseos, tocamientos o arrimones sin su consentimiento",
                          "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
                          "La han ignorado por ser mujer",
                          "Insinuaciones sexuales o insultos en línea",
                          "Le han comentado que las mujeres no deberían trabajar",
                          'Ofensas o humillaciones por "ser mujer"',
                          "Piropos ofensivos"
                      )),
           x = factor(lugar_trabajo,
                      levels = c("Empresa del sector privado, comercial, bancaria o de servicios",
                                 "Negocio, local comercial o de servicios",
                                 "Dependencia o institución del gobierno",
                                 "Vivienda particular",
                                 "Fábrica o taller",
                                 "Escuela o universidad", 
                                 "Campo", 
                                 "Clínica u hospital",
                                 "Calle o vía pública")),
           fill = porcentaje)) +
    geom_tile(color = "white",
              lwd = 0.5) +
    geom_label(aes(
        label=paste0(round(porcentaje*100, digits = 1), "%"), group = porcentaje),
        position = position_dodge(0), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_subtitle, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption
    ) +
    # Escalas
    scale_x_discrete(labels = scales::wrap_format(18)) +
    scale_y_discrete(labels = scales::wrap_format(25)) +
    # Tema
    tema  +
    guides(fill = "none") +
    theme(axis.text.x = element_text(size = 7, color = "black"),
          axis.text.y = element_text(size = 7, color = "black")) +
    scale_fill_gradient(low=v_colores1[7], high= v_colores1[5])

# ---- Guardar en png 
ggsave(paste_fig("31_endireh_laboral_trabajo_agresión_final.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 9, height = 10)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("31_endireh_laboral_trabajo_agresión_final.png"), escala = 10)

#### 5.3.2. Agresión específica por condición de discapacidad ------------------

# ---- Recodificar condición de discapacidad 

df_data <- df_encuesta %>% 
    mutate(condicion_discapacidad = case_when(
        (p19_1_1 %in% c("1", "2") | p19_1_2 %in% c("1", "2") | p19_1_3 %in% c("1", "2") |
             p19_1_4 %in% c("1", "2") | p19_1_5 %in% c("1", "2") | p19_1_6 %in% c("1", "2") |
             p19_1_7 %in% c("1", "2") | p19_1_8 %in% c("1", "2")) ~ "Con discapacidad",
        (p19_1_1 == "3" | p19_1_2 == "3" | p19_1_3 == "3" |
             p19_1_4 == "3" | p19_1_5 == "3" | p19_1_6 == "3"  |
             p19_1_7 == "3" | p19_1_8 == "3") ~ "Con limitación",
        TRUE ~ "Sin discapacidad o limitación"
    ))


# Preguntas P8_11_(1-19)
v_codes <- c(names(df_encuesta$variables %>% select(starts_with("p8_11_"))))

v_tipos <- c(
    "Insinuaciones sexuales o insultos en línea",
    "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
    "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
    "Represalias porque se negó a tener relaciones sexuales",
    "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
    "Piropos ofensivos",
    'Ofensas o humillaciones por "ser mujer"',
    "Patadas o golpes con el puño",
    "Ataques con cuchillo o arma de fuego",
    "Obligación de ver pornografía",
    "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
    "La han vigilado o seguido al salir del trabajo",
    "Intento de violación",
    "Violación",
    "Manoseos, tocamientos o arrimones sin su consentimiento",
    "Exhibicionismo",
    "La han ignorado por ser mujer",
    "Le han comentado que las mujeres no deberían trabajar",
    "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto"
)

# Obtener proporciones 

df_data2 <- df_data                                                         %>%
    filter(p8_4 == 1)                                                       %>%
    rename(respuesta = v_codes[1])                                          %>%
    select(condicion_discapacidad, respuesta)                               %>%
    mutate(respuesta = codificar_siono(as.character(respuesta)))            %>%
    srvyr::group_by(condicion_discapacidad, respuesta)                      %>%
    srvyr::summarise(
        total = srvyr::survey_total(),
        porcentaje = srvyr::survey_prop())                                  %>% 
    mutate(tipo = v_tipos[1])

for(i in 2:length(v_codes)){
    
    print(paste("Vuelta", i, "de", length(v_codes)))
    
    df_data_loop <- df_data                                                     %>%
        filter(p8_4 == 1)                                                       %>%
        rename(respuesta = v_codes[i])                                          %>%
        select(condicion_discapacidad, respuesta)                               %>%
        mutate(respuesta = codificar_siono(as.character(respuesta)))            %>%
        srvyr::group_by(condicion_discapacidad, respuesta)                      %>%
        srvyr::summarise(
            total = srvyr::survey_total(),
            porcentaje = srvyr::survey_prop())                                  %>% 
        mutate(tipo = v_tipos[i])
    
    
    df_data2 <- df_data2 %>% bind_rows(df_data_loop)
}

# Renombrar 
df_discapacidad_agresion <- df_data2

# Proporciones 
df_data1 <- df_discapacidad_agresion        %>% 
    group_by(condicion_discapacidad, tipo)  %>% 
    mutate(total_discapacidad = sum(total)) %>% 
    mutate(porcentaje = total/total_discapacidad)

# ---- Figura 
# Del total de mujeres que trabajaron en cada lugar de trabajo, ¿qué porcentaje reportó haber vivido 
# alguna agresión en el ámbito laboral? 
# Por tipo de agresión

v_title <- "Del total de mujeres que trabajaron, ¿qué porcentaje reportó haber vivido\nagresiones en los 12 meses previos al levantamiento de la encuesta?"
v_subtitle <- "Por tipo de agresión según condición de discapacidad\n"

ggplot(df_data1 %>% filter(respuesta == "Sí"), 
       aes(y = factor(tipo,
                      levels =  c(
                          "Ataques con cuchillo o arma de fuego",
                          "Obligación de ver pornografía",
                          "Violación",
                          "Patadas o golpes con el puño",
                          "Han publicado en medios digitales información personal, fotos o videos, de usted para dañarla",
                          "Intento de violación",
                          "Exhibicionismo",
                          "Pellizcos, jalones de cabello, empujones, bofetadas o aventado algún objeto",
                          "Represalias porque se negó a tener relaciones sexuales",
                          "La han vigilado o seguido al salir del trabajo",
                          "Han comentado que sus logros se debieron a que tuvo relaciones sexuales",
                          "Propuesta de beneficios en el trabajo a cambio de relaciones sexuales",
                          "Manoseos, tocamientos o arrimones sin su consentimiento",
                          "Le han hecho sentir miedo de ser atacada o abusada sexualmente",
                          "La han ignorado por ser mujer",
                          "Insinuaciones sexuales o insultos en línea",
                          "Le han comentado que las mujeres no deberían trabajar",
                          'Ofensas o humillaciones por "ser mujer"',
                          "Piropos ofensivos"
                      )),
           x = factor(condicion_discapacidad,
                      levels = c("Con discapacidad",
                                 "Con limitación",
                                 "Sin discapacidad o limitación")),
           fill = porcentaje)) +
    geom_tile(color = "white",
              lwd = 0.5) +
    geom_label(aes(
        label=paste0(round(porcentaje*100, digits = 1), "%"), group = porcentaje),
        position = position_dodge(0), size=3, hjust=.5, vjust=.5, 
        angle = 0, fill = "white",
        color="black",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title = v_title, 
        subtitle = v_subtitle, 
        x = v_empty, 
        y = v_empty, 
        caption = v_caption
    ) +
    # Escalas
    scale_x_discrete(labels = scales::wrap_format(18)) +
    scale_y_discrete(labels = scales::wrap_format(30)) +
    # Tema
    tema +
    guides(fill = "none") +
    theme(axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 7, color = "black")) +
    scale_fill_gradient(low=v_colores1[7], high= v_colores1[5])

# ---- Guardar en png 
ggsave(paste_fig("36_endireh_laboral_discapacidad_agresión_final.png"), 
       device = "png", type = "cairo", # para guardar fuentes 
       width = 9, height = 10)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("36_endireh_laboral_discapacidad_agresión_final.png"), escala = 10)

# FIN. -------------------------------------------------------------------------
