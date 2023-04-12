#------------------------------------------------------------------------------#
# Proyecto:                   Violencia en el ámbito laboral (2022)
# Objetivo:                   Figuras de ENVIPE (2011-2022)
#
# Encargadas:                 Regina Isabel Medina   | Fernanda Torres
# Correo:                     rmedina@intersecta.org | ftorres@intersecta.org
# 
# Fecha de creación:          23 de septiembre de 2022
# Última actualización:       08 de noviembre  de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

# ---- Silenciar mensajes de .group en dplyr
options(dplyr.summarise.inform = FALSE)

# ---- Cargar librerías 
require(pacman)
p_load(readxl, tidyverse, magick, add2ggplot)

# ---- Limpiar espacio de trabajo 
rm(list=ls())

# ---- Desactivar notación científica
options(scipen=999)

# ---- Método para el caso de 1 UPM en los estratos
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# ---- Establecer directorios
paste_fig <- function(x){paste0("03_figuras/02_envipe/", x)}

# 1. Cargar datos --------------------------------------------------------------

## 1.1. ENVIPE 2011 - 2022 -----------------------------------------------------

# Fuente: https://www.inegi.org.mx/programas/envipe/

v_bases     <- c("tmod_vic_2011.dbf", "Tmod_Vic_2012.dbf", "tmod_vic_2013.dbf",
                 "TMod_Vic_2014.dbf", "TMod_Vic_2015.dbf", "TMod_Vic_2016.dbf", 
                 "TMod_Vic_2017.dbf", "TMod_Vic_2018.dbf", "TMod_Vic_2019.dbf", 
                 "TMod_Vic_2020.dbf", "TMod_Vic_2021.dbf", "TMod_Vic_2022.dbf",
                 "TPer_Vic2_2022.dbf")

# 2. Funciones -----------------------------------------------------------------

## 2.1. Lugar donde ocurrió el delito ------------------------------------------

codificar_lugar <- function(var = x){
  v_lugar <- c("Calle", "Casa", "Trabajo", "Negocio o establecimiento",
               "Lugar público", "Trasnporte público", "Carretera",
               "Otro lugar", "Otros")

  case_when(
    var == 1 ~ v_lugar[1],
    var == 2 ~ v_lugar[2],
    var == 3 ~ v_lugar[3],
    var == 4 ~ v_lugar[4],
    var == 5 ~ v_lugar[5],
    var == 6 ~ v_lugar[6],
    var == 7 ~ v_lugar[7],
    var == 8 ~ v_lugar[8],
    var == 9 ~ v_lugar[9]
  )
}

## 2.2. Clasificación de delitos (ENVIPE 2013-2022) ----------------------------

codificar_delito <- function(var = x){
    v_delito <- c(
        "Robo de vehículo",
        "Robo de accesorios",
        "Vandalismo",
        "Robo en casa habitación",
        "Robo en la calle",
        "Otros robos",
        "Fraude bancario",
        "Fraude al consumidor",
        "Extorsión",
        "Amenazas verbales",
        "Lesiones",
        "Secuestro",
        "Otros delitos sexuales",
        "Violación",
        "Otros delitos"
    )
    
    case_when(
        var == "01" ~ v_delito[1],
        var == "02" ~ v_delito[2],
        var == "03" ~ v_delito[3],
        var == "04" ~ v_delito[4],
        var == "05" ~ v_delito[5],
        var == "06" ~ v_delito[6],
        var == "07" ~ v_delito[7],
        var == "08" ~ v_delito[8],
        var == "09" ~ v_delito[9],
        var == "10" ~ v_delito[10],
        var == "11" ~ v_delito[11],
        var == "12" ~ v_delito[12],
        var == "13" ~ v_delito[13],
        var == "14" ~ v_delito[14],
        var == "15" ~ v_delito[15]
    )
}

## 2.3. Sexo -------------------------------------------------------------------
codificar_sexo <- function(var = x){
    case_when(
        var == 1 ~ "Hombres",
        var == 2 ~ "Mujeres"
    )
}

## 2.4. Añadir logo ------------------------------------------------------------
add_intlogo <- function(graf, escala){
    graf_con_logo <- add_logo(
        plot_path = graf,
        logo_path = "logo/corto_blanco.png",
        logo_position = "bottom right",
        logo_scale = escala)
    
    magick::image_write(graf_con_logo, graf)
}

# 3. Diseño de encuesta --------------------------------------------------------

# Cambiar nombres 
v_bases <- c("df_raw_1", "df_raw_2", "df_raw_3", "df_raw_4")

for (i in v_bases) {
  df_temp <- get(i)
  df_name <- df_temp %>% 
    rename(
      UPM_DIS = UPM,
      EST_DIS = EST
    )
  assign(as.character(i), df_name)
}

# Año 2021 
df_raw_12 <- df_raw_12 %>% 
    left_join(df_raw_13)

# Formato de encuesta
for(i in 1:12){
  df_temp <- get(paste0("df_raw_",i))
  df_encuesta <- df_temp                  %>% 
    mutate(FAC_DEL = as.numeric(FAC_DEL)) %>% 
    as_survey_design(
      ids = UPM_DIS, strata = EST_DIS, weights = FAC_DEL
    )
  assign(paste0("df_encuesta_", i), df_encuesta)
}

# 4. Figuras -------------------------------------------------------------------

## 4.0. Configuración ----------------------------------------------------------

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
    axis.title            = element_text(size = 11, family = "Fira Sans", face = "bold", color = "#DDDDDD", hjust = .5, margin = margin(1,1,1,1)),
    axis.text.y           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0,  hjust=1, vjust = 0.5, color = "black"),
    axis.text.x           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0, hjust=.5, vjust = 0.5, color = "#DDDDDD"),
    strip.text.x          = element_text(size = 13, family = "Fira Sans", face = "bold", color = "black"),
    strip.text.y          = element_text(size = 13, family = "Fira Sans", face = "bold", color = "black"), 
    strip.background      = element_rect(fill = "white", color = NA),
    axis.ticks = element_blank())

# ---- Colores
v_colores1 <- c("#000000", "#30475E", "#58788C", "#B3C2C9", "#F05454", "#DDDDDD", "#FCF7F7")
v_colores2 <- c("#30475E", "#F05454")

# ---- Vectores de texto 
v_empty <- ""
v_caption <- "Fuente: Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública (ENVIPE).\nDatos procesados por Intersecta (intersecta.org).\n"

## 4.1. Lugares donde ocurrieron los delitos -----------------------------------

# ---- Obtener proporciones para cada año 

# Vector de años
v_años <- seq(2010, 2021)

# Dataframe vacío 
df_data <- tibble(
    lugar = character(),
    total = numeric(),
    total_se = numeric(),
    año = numeric()
)

# Obtener proporciones
for (i in 1:12) {
  name    <-  paste0("df_encuesta_", i)
  df_temp <- get(name) 
  
  if (i <= 2) {
      df_data_loop <- df_temp                  %>% 
          mutate(
              lugar = codificar_lugar(BP1_5))  %>%
          group_by(lugar)                      %>% 
          srvyr::summarise(
              total = survey_total()
          ) %>%
          mutate(año = v_años[i])
      
      df_data <- df_data %>% full_join(df_data_loop)
  }
  else{
      df_data_loop <- df_temp                  %>%
          filter(BPCOD != "03")                %>% 
          mutate(
              lugar = codificar_lugar(BP1_5))  %>%
          group_by(lugar)                      %>% 
          srvyr::summarise(
              total = survey_total()
          ) %>%
          mutate(año = v_años[i])
      
      df_data <- df_data %>% full_join(df_data_loop)
  }
}

# Renombrar
df_lugares <- df_data 

## 4.2. Delitos en el lugar de trabajo -----------------------------------------

# ---- Datos
df_data <- df_lugares           %>% 
    filter(lugar == "Trabajo")

# ---- Figura

v_title <- "En México, ¿cuántos delitos ocurren en el lugar de trabajo?"
v_subtitle <- "Por año de ocurrencia\n"

ggplot(df_data, aes(año, total)) +
    geom_col(fill = v_colores1[2]) + 
    geom_text(aes(
        label= scales::comma(total), group = año),
        position = position_stack(1), size=3.5, hjust=1.1, vjust=.5, 
        angle = 90, 
        color="#DDDDDD",  family = "Fira Sans") +
    # Etiquetas
    labs(
        title    = v_title, 
        subtitle = v_subtitle, 
        x        = v_empty, 
        y        = v_empty,
        fill     = v_empty,
        caption  = paste0("\n", v_caption)) +
    # Tema 
    tema +
    scale_y_continuous(labels = scales::comma) + 
    scale_x_continuous(breaks = seq(2010, 2021, 1)) +
    theme(
        panel.grid.major.y    = element_line(linetype = 2),
        panel.grid.major.x    = element_blank(),
        panel.grid.minor.y    = element_blank(),
        panel.grid.minor.x    = element_blank()
    ) +
    theme(
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "#DDDDDD")
    )

ggsave(file = paste_fig("00_delitos_lugar_2010_2021_total.png"), 
       type = "cairo", device = "png", 
       width = 8, height = 6)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("00_delitos_lugar_2011_2021_total.png"), escala = 10)

## 4.3. Delitos por sexo de la víctima en trabajo (2021) -----------------------

# ---- Datos
# df_data <- df_encuesta_12                     %>% 
#     filter(BP1_5 ==  3)                       %>% 
#     mutate(sexo = codificar_sexo(SEXO),
#            delito = codificar_delito(BPCOD))  %>% 
#     group_by(sexo, delito)                    %>% 
#     summarise(
#         porcentaje = survey_prop()
#     )

# ---- Datos

df_renombrada <- df_raw_12           %>% 
    janitor::clean_names("all_caps") %>% 
    # Seleccionar variables relevantes 
    select(
        # Lugar de ocurrencia 
        BP1_5, 
        # Identificadores y factores de expansión 
        ID_VIV, ID_HOG, ID_PER, UPM, SEXO, FAC_HOG, FAC_ELE, FAC_DEL,  
        # Incidencia (variables binarias, sí o no hubo victimización)
        AP6_4_01, # Robo vehículo 
        AP6_4_02, # Robo de accesorios 
        AP6_4_03, # Vandalismo 
        AP6_4_04, # Robo a casa habitación 
        AP7_3_05, # Robo o asalto en la calle 
        AP7_3_06, # Otro tipo de robo 
        AP7_3_07, # Fraude bancario 
        AP7_3_08, # Fraude al consumidor 
        AP7_3_09, # Extorsión
        AP7_3_10, # Amenazas verbales
        AP7_3_11, # Lesiones 
        AP7_3_12, # Secuestro 
        AP7_3_13, # Otros delitos sexuales 
        AP7_3_14, # Violación 
        AP7_3_15, # Otros delitos
        # Frecuencia (número de veces que ocurrió)
        AP6_6_01, # Robo vehículo 
        AP6_6_02, # Robo de accesorios 
        AP6_6_03, # Vandalismo 
        AP6_6_04, # Robo a casa habitación 
        AP7_4_05, # Robo o asalto en la calle 
        AP7_4_06, # Otro tipo de robo 
        AP7_4_07, # Fraude bancario 
        AP7_4_08, # Fraude al consumidor 
        AP7_4_09, # Extorsión
        AP7_4_10, # Amenazas verbales
        AP7_4_11, # Lesiones 
        AP7_4_12, # Secuestro 
        AP7_4_13, # Otros delitos sexuales 
        AP7_4_14, # Violación 
        AP7_4_15  # Otros delitos
    ) %>% 
    # Variables binarias de delitos y agresores
    mutate(
        # Delitos 
        incidencia_robovehic = if_else(AP6_4_01 == 1, 1, 0),
        incidencia_roboacces = if_else(AP6_4_02 == 1, 1, 0),
        incidencia_vandalism = if_else(AP6_4_03 == 1, 1, 0),
        incidencia_robocasah = if_else(AP6_4_04 == 1, 1, 0),
        incidencia_robocalle = if_else(AP7_3_05 == 1, 1, 0),
        incidencia_robootros = if_else(AP7_3_06 == 1, 1, 0),
        incidencia_fraudeban = if_else(AP7_3_07 == 1, 1, 0),
        incidencia_fraudecon = if_else(AP7_3_08 == 1, 1, 0),
        incidencia_extorsion = if_else(AP7_3_09 == 1, 1, 0),
        incidencia_amenazasv = if_else(AP7_3_10 == 1, 1, 0),
        incidencia_lesiones  = if_else(AP7_3_11 == 1, 1, 0),
        incidencia_secuestro = if_else(AP7_3_12 == 1, 1, 0),
        incidencia_sexualeso = if_else(AP7_3_13 == 1, 1, 0),
        incidencia_violacion = if_else(AP7_3_14 == 1, 1, 0),
        incidencia_otrosdeli = if_else(AP7_3_15 == 1, 1, 0), 
    ) %>% 
    # Variable de frecuencia de delitos
    rename(
        # Lugar 
        lugar                = BP1_5,
        # Frecuencia del delito vivido 
        frecuencia_robovehic = AP6_6_01, 
        frecuencia_roboacces = AP6_6_02, 
        frecuencia_vandalism = AP6_6_03, 
        frecuencia_robocasah = AP6_6_04, 
        frecuencia_robocalle = AP7_4_05, 
        frecuencia_robootros = AP7_4_06, 
        frecuencia_fraudeban = AP7_4_07, 
        frecuencia_fraudecon = AP7_4_08, 
        frecuencia_extorsion = AP7_4_09, 
        frecuencia_amenazasv = AP7_4_10, 
        frecuencia_lesiones  = AP7_4_11, 
        frecuencia_secuestro = AP7_4_12,
        frecuencia_sexualeso = AP7_4_13, 
        frecuencia_violacion = AP7_4_14, 
        frecuencia_otrosdeli = AP7_4_15)               %>%
    # Quitar variables de exceso
    select(-starts_with("AP"))

df_larga <- df_renombrada                               %>% 
    mutate_all(as.character)                            %>% 
    pivot_longer(
        cols      = c(frecuencia_robovehic:incidencia_otrosdeli), 
        names_to  = c("indicador", "delito"), 
        names_sep = "_", 
        values_to = "valor")                            %>% 
    # Dejar solo información de incidencia (binaria)
    filter(indicador == "frecuencia")                   %>% 
    # Calcular incidencia con factor de expansión a nivel delito 
    # Número de veces que ocurrió multiplicado por el factor de expansión 
    mutate(
        incidencia_total  = as.numeric(
            as.character(valor))*as.numeric(as.character(FAC_DEL)))

v_code      <- unique(df_larga$delito)

df_final    <- df_larga %>% 
    # Cambiar nombres de los delitos 
    mutate(
        delito = case_when(
            delito == v_code[1]  ~ "Robo de vehículo"      ,
            delito == v_code[2]  ~ "Robo de accesorios"    ,
            delito == v_code[3]  ~ "Vandalismo"            ,
            delito == v_code[4]  ~ "Robo a casa habitación",
            delito == v_code[5]  ~ "Robo en la calle"      ,
            delito == v_code[6]  ~ "Otros robos"           , 
            delito == v_code[7]  ~ "Fraude bancario"       ,
            delito == v_code[8]  ~ "Fraude al consumidor"  ,
            delito == v_code[9]  ~ "Extorsión"             ,
            delito == v_code[10] ~ "Amenazas verbales"     ,
            delito == v_code[11] ~ "Lesiones"              , 
            delito == v_code[12] ~ "Secuestro"             ,
            delito == v_code[13] ~ "Otros delitos sexuales",
            delito == v_code[14] ~ "Violación"             ,
            delito == v_code[15] ~ "Otros delitos"         , 
        ), 
        sexo = case_when(
            SEXO == 1 ~ "Hombres", 
            SEXO == 2 ~ "Mujeres"))

df_trabajo <- df_final %>% 
    filter(lugar == 3)

# Cada delito como porcentaje del total de delitos vividos por sexo 
df_data <- df_trabajo                       %>% 
    group_by(sexo, delito)                                  %>% 
    summarise(total = sum(incidencia_total, na.rm = TRUE))  %>% 
    group_by(sexo)                                          %>% 
    mutate(porcentaje = total/sum(total))                   %>% 
    # Ordenar factores de más a menos frecuente
    mutate(delito = fct_reorder(delito, porcentaje))


# ---- Figura 
v_title <- "Distribución de los delitos perpetrados en el lugar de trabajo"
v_subtitle <- "Por sexo de la víctima, en México durante 2021"

ggplot(df_data, 
       aes(x = porcentaje, 
           y = reorder(delito, porcentaje))) +
    geom_col(fill = v_colores1[2]) +
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
    scale_x_continuous(label = scales::percent_format()) +
    scale_y_discrete(labels = scales::wrap_format(35)) +
    # Tema
    tema  +
    guides(fill = "none") +
    facet_wrap(~sexo)

# Guardar en png
ggsave(file = paste_fig("00_delitos_lugar_sexo.png"), 
       type = "cairo", device = "png", 
       width = 8, height = 6)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("00_delitos_lugar_sexo.png"), escala = 10)

# FIN --------------------------------------------------------------------------
