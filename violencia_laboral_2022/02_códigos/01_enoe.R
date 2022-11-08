#------------------------------------------------------------------------------#
# Proyecto:                   Discriminación laboral
# Objetivo:                   Actualizar datos de la ENOE
#
# Encargada:                  Regina Isabel Medina Rosales
# Correo:                     rmedina@intersecta.org
# Fecha de creación:          12 de julio     de 2021
# Última actualización:       07 de noviembre de 2022
#------------------------------------------------------------------------------#

# Los datos provienen de los tabulados de la ENOE. La información de motivo
# de abandono se reporta solo para los primeros trimestres de cada año. 

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, magick, add2ggplot)

# Opciones
options(dplyr.summarise.inform = FALSE) # Silenciar error de summarise
options(scipen = 10)                    # Desactivar notación científica

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
paste_inp <- function(x){paste0("01_datos_crudos/01_enoe/", x)}
paste_fig <- function(x){paste0("03_figuras/01_enoe/"     , x)}

# 1. Cargar datos --------------------------------------------------------------

# https://www.inegi.org.mx/programas/enoe/15ymas/#Tabulados

# Ruta: ENOE > Tabulados interactivos > Eventos de abandono de empleo 

# Abandono laboral (total de personas)
df_abandono <- read_excel(paste_inp("ENOE_abandono_empleo_total.xlsx"), 
                          skip = 4)

# Abandono laboral (mujeres)
df_aban_muj <- read_excel(paste_inp("ENOE_abandono_empleo_mujeres.xlsx"), 
                          skip = 4)

# Abandono laboral (hombres)
df_aban_hom <- read_excel(paste_inp("ENOE_abandono_empleo_hombres.xlsx"), 
                          skip = 4)

# Población ocupada total 
df_pobocupa <- read_excel(paste_inp("ENOE_pob_ocupada_sexo.xlsx"), 
                          skip = 4)

# 2. Funciones -----------------------------------------------------------------

## 2.1. Añadir logo ------------------------------------------------------------

add_intlogo <- function(graf, escala){
    graf_con_logo <- add_logo(
        plot_path = graf,
        logo_path = "logo/corto_blanco.png",
        logo_position = "bottom right",
        logo_scale = escala)
    
    magick::image_write(graf_con_logo, graf)
}

# 3. Limpieza ------------------------------------------------------------------

## 3.1. Personas ocupadas (para obtener tasa) ----------------------------------

v_names <- names(df_pobocupa)

df_pob  <- df_pobocupa                              %>% 
  slice(1:65)                                       %>% 
  rename(trim_text = v_names[1])                    %>% 
  # Crear variables de fechas
  mutate(
    year = as.numeric(str_sub(trim_text, -4, -1)), 
    trim = case_when(
      str_detect(trim_text, "Primer")  ~ 1, 
      str_detect(trim_text, "Segundo") ~ 2, 
      str_detect(trim_text, "Tercer")  ~ 3, 
      str_detect(trim_text, "Cuarto")  ~ 4))        %>% 
  # Cambiar a formato largo 
  pivot_longer(
    cols      = c("Total", "Hombre", "Mujer"), 
    names_to  = "grupo", 
    values_to = "pob_ocup")                         %>% 
  # Dejar solo el primer trimestre 
  filter(trim == 1 & year >= 2010) 


## 3.2. Personas que dejaron su empleo por acoso -------------------------------

v_names         <- names(df_abandono)

# Para el total de la población 
df_acoso_total  <- df_abandono                      %>% 
  # Cambiar cuando haya una nueva actualización (se agrega una fila)
  slice(1:22)                                       %>% 
  rename(trim_text = v_names[1])                    %>% 
  # Crear variables de fechas
  mutate(
    year = as.numeric(str_sub(trim_text, -4, -1)), 
    trim = case_when(
      str_detect(trim_text, "Primer")  ~ 1, 
      str_detect(trim_text, "Segundo") ~ 2, 
      str_detect(trim_text, "Tercer")  ~ 3, 
      str_detect(trim_text, "Cuarto")  ~ 4))        %>% 
  # Cambiar a formato largo 
  pivot_longer(
    cols = -c("trim_text", "year", "trim"), 
    names_to = "motivo", 
    values_to = "pob_abandono")                     %>% 
  # Dejar solo los casos de acoso 
  filter(motivo == "Discriminación o acoso")        %>% 
  mutate(grupo = "Total")


# Para mujeres 
df_acoso_mujer  <- df_aban_muj                      %>% 
  # Cambiar cuando haya una nueva actualización (se agrega una fila)
  slice(1:22)                                       %>% 
  rename(trim_text = v_names[1])                    %>% 
  mutate(
    year = as.numeric(str_sub(trim_text, -4, -1)), 
    trim = case_when(
      str_detect(trim_text, "Primer")  ~ 1, 
      str_detect(trim_text, "Segundo") ~ 2, 
      str_detect(trim_text, "Tercer")  ~ 3, 
      str_detect(trim_text, "Cuarto")  ~ 4))        %>% 
  pivot_longer(
    cols = -c("trim_text", "year", "trim"), 
    names_to = "motivo", 
    values_to = "pob_abandono")                     %>% 
  filter(motivo == "Discriminación o acoso")        %>% 
  mutate(grupo = "Mujer")

# Para hombres 
df_acoso_hombre <- df_aban_hom                      %>% 
  # Cambiar cuando haya una nueva actualización (se agrega una fila)
  slice(1:22)                                       %>% 
  rename(trim_text = v_names[1])                    %>% 
  mutate(
    year = as.numeric(str_sub(trim_text, -4, -1)), 
    trim = case_when(
      str_detect(trim_text, "Primer")  ~ 1, 
      str_detect(trim_text, "Segundo") ~ 2, 
      str_detect(trim_text, "Tercer")  ~ 3, 
      str_detect(trim_text, "Cuarto")  ~ 4))        %>% 
  pivot_longer(
    cols = -c("trim_text", "year", "trim"), 
    names_to = "motivo", 
    values_to = "pob_abandono")                     %>% 
  filter(motivo == "Discriminación o acoso")        %>% 
  mutate(grupo = "Hombre")


## 3.3. Unir bases -------------------------------------------------------------

df_acoso <- df_acoso_total                          %>% 
  rbind(df_acoso_mujer, df_acoso_hombre)            %>% 
  arrange(year, trim)                               %>% 
  filter(trim == 1 & year >= 2010)                  %>% 
  full_join(df_pob, 
            by = c("trim_text", "year", "trim", 
                   "grupo"))                        %>% 
  # Calcular tasa por cada 100,000 trabajadores
  mutate_at(.vars = c("pob_abandono", "pob_ocup"), 
            as.numeric)                             %>% 
  mutate(tasa = round(pob_abandono*100000/pob_ocup, 1))

# 4. Figuras -------------------------------------------------------------------

## 4.1. Configuración ----------------------------------------------------------

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

v_title     <- "Tasa de abandono laboral por acoso por cada 100,000 personas ocupadas"
v_sub       <- "Al primer trimestre de cada año\n"

y_abs       <- "Número de personas\n"
y_por       <- "Porcentaje\n"
y_tasa      <- "Tasa por cada 100,000 personas ocupadas\n"

v_caption   <- "\nFuente: Encuesta Nacional de Ocupación y Empleo (ENOE). Nota: Los datos corresponden\nal primer trimestre de cada año. 
Datos procesados por Intersecta (intersecta.org).\n"


## 4.2. Graficar ---------------------------------------------------------------

### 4.2.1. Total de trabajadores -----------------------------------------------

df_data <- df_acoso                                           %>% 
  filter(grupo == "Total")                                    %>% 
  mutate(pob_cifra = format(pob_abandono, big.mark = ","))

ggplot(df_data, aes(x = year, y = tasa)) +
    geom_col(fill = v_colores1[2])  +
    geom_text(aes(
        label=paste0(tasa, "  (", pob_cifra, ")"), group = tasa),
        position = position_dodge(0), size=3, hjust=1.1, vjust=.5, 
        angle = 90,
        color="white",  family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_sub,
        y = "TASA POR CADA 100,000 PERSONAS OCUPADAS\n",
        x = v_empty,
        caption = v_caption
    ) +
    tema +
    theme(
        panel.grid.major.y    = element_line(linetype = 2),
        panel.grid.major.x    = element_blank(),
        panel.grid.minor.y    = element_blank(),
        panel.grid.minor.x    = element_blank(),
        axis.text.y           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0,  hjust=1, vjust = 0.5, color = "#DDDDDD"),
        axis.text.x           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0, hjust=.5, vjust = 0.5, color = "black")
    ) +
    scale_x_continuous(breaks = seq(2010, 2022))

# Guardar en png
ggsave(file = paste_fig("01_enoe_tasa_abandono.png"), 
       type = "cairo", device = "png", 
       width = 8, height = 6)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("01_enoe_tasa_abandono.png"), escala = 10)

### 4.2.2. Por sexo ------------------------------------------------------------

df_data <- df_acoso                                           %>% 
  filter(grupo != "Total")                                    %>% 
  mutate(pob_cifra = format(pob_abandono, big.mark = ","), 
         grupo = ifelse(grupo == "Mujer", "Mujeres", "Hombres"))

ggplot(df_data, aes(x = year, y = tasa, fill = grupo, group = grupo)) +
    geom_col(position = "dodge")  +
    geom_text(aes(
        y = tasa,
        label=paste0(tasa, "  (", pob_cifra, ")")),
        position =  position_dodge(width = 0.9), 
        size=3, 
        hjust=1.1, 
        vjust=.5, 
        angle = 90,
        color="white",  
        family = "Fira Sans") +
    labs(
        title = v_title,
        subtitle = v_sub,
        y = "TASA POR CADA 100,000 PERSONAS OCUPADAS\n",
        x = v_empty,
        caption = v_caption,
        fill = "Tasa de"
    ) +
    tema +
    scale_fill_manual(values = v_colores2) +
    theme(
        panel.grid.major.y    = element_line(linetype = 2),
        panel.grid.major.x    = element_blank(),
        panel.grid.minor.y    = element_blank(),
        panel.grid.minor.x    = element_blank(),
        axis.text.y           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0,  hjust=1, vjust = 0.5, color = "#DDDDDD"),
        axis.text.x           = element_text(size = 10, family = "Fira Sans", face = "italic", angle=0, hjust=.5, vjust = 0.5, color = "black")
    ) +
    scale_x_continuous(breaks = seq(2010, 2022))

# Guardar en png
ggsave(file = paste_fig("01_enoe_tasa_abandono_sexo.png"), 
       type = "cairo", device = "png", 
       width = 9, height = 7)

# Añadir logo de INTR
add_intlogo(graf = paste_fig("01_enoe_tasa_abandono_sexo.png"), escala = 10)

# FIN. -------------------------------------------------------------------------
