#------------------------------------------------------------------------------#
# Proyecto:                   Amicus GN
# Objetivo:                   Extraer datos de las plazas sobre las FFAA y GN

# Encargadas:                 Estefanía Vela          | Fernada Torres
# Correo:                     evela@intersecta.org    | ftorres@intersecta.org
# Fecha de creación:          07 de marzo de 2023
# Última actualización:       12 de abril de 2023
#------------------------------------------------------------------------------#

# Notación científica
options(scipen=999)

# 0. Configuración inicial -----------------------------------------------------

# Librerías 
require(pacman)
p_load(readxl, tidyverse, dplyr, beepr, scales)

# Limpiar espacio de trabajo 
rm(list=ls())

# Establecer directorios
inp <- "02_datos_limpios/"
out <- "04_figuras/"

# ---- Tema
tema <-  theme_linedraw() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot", 
    text                  = element_text(family = "Fira Sans", color = "#222222"),
    plot.title            = element_text(family = "Roboto Slab", color = "#222222", size = 10,  face  = "bold",  margin = margin(10,5,5,5)),
    plot.subtitle         = element_text(family = "Fira Sans", face = "italic", color = "#222222", size = 9,  margin = margin(5, 5, 5, 5)),
    plot.caption          = element_text(family = "Fira Sans", face = "italic", color = "#222222", size = 8,  hjust = 0),
    panel.grid            = element_line(linetype = 2),
    plot.margin           = margin(0, 0.5, 0.5, 0.5, "cm"),
    legend.position       = "top",
    panel.border          = element_rect(colour = "#222222"),
    legend.title          = element_text(size = 8, family = "Fira Sans", color = "#222222", face   = "bold"),
    legend.text           = element_text(size = 8, family = "Fira Sans", color = "#222222"),
    axis.title            = element_text(size = 8, family = "Fira Sans", color = "#7F8487", hjust = .5, margin = margin(1,1,1,1)),
    axis.text.y           = element_text(size = 7, family = "Fira Sans", color = "#7F8487", angle=0,  hjust=1),
    axis.text.x           = element_text(size = 7, family = "Fira Sans", color = "#7F8487", angle=90, hjust=.5, vjust = 0.5),
    strip.text.x          = element_text(size = 8, family = "Fira Sans", color = "#222222", face = "bold"),
    strip.text.y          = element_text(size = 8, family = "Fira Sans", face = "bold", color = "#222222"), 
    strip.background      = element_rect(fill = "white", color = "#222222"),
    axis.line.x.bottom    = element_line(color="#222222", size = .3))

# ---- Colores
v_colores_todos <- c("#222222", "#3C4048", "#DC0000", "#850000", "#7F8487", "#C8C9C7")
v_colores       <- c("#C13F3F", "#48423C", "#A6A6A6")

# 1. Cargar datos --------------------------------------------------------------

load(paste0(inp, "df_cuenta_pub_pconstantes.RData"))

base <- df_cuenta_pub_pconstantes

fuerzas = filter(base, desc_ramo == "Defensa Nacional" | desc_ramo == "Marina" | desc_ramo == "Seguridad y Protección Ciudadana" | desc_ramo == "Gobernación")

fuerzas$sedena <- as.numeric(str_detect(fuerzas$desc_ramo, "Defensa Nacional"))
fuerzas$semar <- as.numeric(str_detect(fuerzas$desc_ramo, "Marina"))
fuerzas$policia_1 <- as.numeric(str_detect(fuerzas$desc_ur, "Policía Federal"))
fuerzas$policia_2 <- as.numeric(str_detect(fuerzas$desc_ur, "Guardia Nacional"))

fuerzas$policia   <- rowSums(fuerzas[c("policia_1", "policia_2")], na.rm = T)
fuerzas$policia_1 <- fuerzas$policia_2 <- NULL

fuerzas$sedena  = gsub("1", "Sedena", fuerzas$sedena)
fuerzas$semar  = gsub("1", "Semar", fuerzas$semar)
fuerzas$policia  = gsub("1", "Policía Federal / Guardia Nacional", fuerzas$policia)

fuerzas <- fuerzas %>% 
  mutate(agentes = paste(sedena, semar, policia))%>% 
  mutate(agentes = str_remove_all(agentes, "0 "))%>% 
  mutate(agentes = str_remove_all(agentes, " 0"))

fuerzas = filter(fuerzas, agentes != "0")

######


personas = group_by(fuerzas, year, agentes)
personas = summarize(personas, total = sum(monto_ejercido, na.rm = T)/1000)
personas = filter(personas, year < 2022)


# View(personas)

ggplot(personas, aes(x = as.integer(year), y=total, fill= agentes)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = scales::comma(total),
                fontface = "bold"),
            position = position_dodge(.9),
            size=1.5, hjust=1.05, vjust=.5, 
            angle = 90,
            color="white",  family = "Fira Sans") +
  scale_fill_manual(values = v_colores) +
  labs(title="El presupuesto ejercido por las fuerzas federales en México",
       caption="\nFuente: Transparencia presupuestaria. Observatorio del gasto.
Los datos fueron procesados por Intersecta (intersecta.org).",
       x="", 
       y="EN MILES DE MILLONES DE PESOS DEL 2022\n",
       subtitle = "Por año \n", fill="Institución:") +
  tema +
  facet_wrap(~factor(agentes, 
                     levels = c("Policía Federal / Guardia Nacional", "Semar", "Sedena")),
             labeller = label_wrap_gen(width = 20)) +
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(from=2008, to=2021, by=1)) +
  scale_y_continuous(labels = scales::comma_format(),
                     limits = c(0, NA),
                     expand = c(0, 2000000))

# ---- Guardar en png
ggsave(file = paste(out, "presupuesto_ejercido_2008_2021.png", sep = "/"),
       width = 16.5, height = 12, units = "cm")

######

personas = group_by(fuerzas, year, agentes)
personas = summarize(personas, total = sum(monto_aprobado, na.rm = T)/1000)


ggplot(personas, aes(x = as.integer(year), y=total, fill= agentes)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = scales::comma(total),
                fontface = "bold"),
            position = position_dodge(.9),
            size=1.5, hjust=1.05, vjust=.5, 
            angle = 90,
            color="white",  family = "Fira Sans") +
  scale_fill_manual(values = v_colores) +
  labs(title="El presupuesto aprobado para las fuerzas federales en México",
       caption="\nFuente: Transparencia presupuestaria. Observatorio del gasto.
Los datos fueron procesados por Intersecta (intersecta.org).",
       x="", 
       y="EN MILES DE MILLONES DE PESOS DE 2022 \n",
       subtitle = "Por año \n", fill="Institución:") +
  tema +
  facet_wrap(~factor(agentes, levels = c("Policía Federal / Guardia Nacional", "Semar", "Sedena")),
             labeller = label_wrap_gen(width = 20)) +
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, vjust = 0.5)) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(from=2008, to=2022, by=1)) +
  scale_y_continuous(labels = scales::comma_format(),
                     limits = c(0, NA),
                     expand = c(0, 1700000))

# ---- Guardar en png
ggsave(file = paste(out, "presupuesto_aprobado_2008_2022.png", sep = "/"),
       width = 16.5, height = 12, units = "cm")


######

personas = group_by(fuerzas, year, agentes)
personas = summarize(personas, aprobado = sum(monto_aprobado, na.rm = T),
                     ejercido = sum(monto_ejercido, na.rm = T))


## 2. Plazas -------------------------------------------------------------------

load(paste0(inp, "df_sueldos_pconstantes.RData"))

base <- df_sueldos_pconstantes

# FFAA
df_plazas_ffaa <- base %>% 
  filter(ramo %in% c("07 Defensa Nacional", "13 Marina")) %>% 
  mutate(
    fuerza = case_when(
      ramo == "07 Defensa Nacional" ~ "Sedena",
      ramo == "13 Marina" ~ "Semar"
    )
  )

# GN
df_plazas_gn <- base %>% 
  filter(unidad == "H00 Guardia Nacional") %>% 
  mutate(fuerza = "Policía Federal / Guardia Nacional")

# PF 
df_plazas_pf <- base %>% 
  filter(grepl(c('Policía Federal'), unidad)) %>% 
  mutate(fuerza = "Policía Federal / Guardia Nacional")


# Juntar bases
df_plazas <- df_plazas_ffaa %>% 
  full_join(df_plazas_gn)   %>% 
  full_join(df_plazas_pf)   %>% 
  group_by(year, fuerza)    %>% 
  summarise(plazas_totales = sum(plazas, na.rm = T))


# Figura 1 

ggplot(df_plazas, aes(x = year, y = plazas_totales, fill = fuerza)) +
  geom_col() +
  geom_text(aes(label = paste0(scales::comma(round(plazas_totales/1000)), "k"),
                fontface = "bold"),
            position = position_dodge(.9),
            size=1.5, vjust=.5, angle = 90,
            hjust=  1.15,
            color= "white", 
            family = "Fira Sans") +
  facet_wrap(~factor(fuerza, levels = c("Policía Federal / Guardia Nacional", "Semar", "Sedena")),
             labeller = label_wrap_gen(width = 20)) +
  tema +
  scale_x_continuous(breaks=seq(from=2005, to=2022, by=1)) +
  scale_y_continuous(labels = scales::comma_format(),
                     limits = c(0, NA),
                     expand = c(0, 3000)) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  scale_fill_manual(values = v_colores) +
  guides(fill = "none") +
  labs(title="Las plazas asignadas a las instituciones federales de seguridad en México",
       caption="\nFuente: Analíticos de Plazas y Remuneraciones de la Administración Pública Federal.
Los datos fueron procesados por Intersecta (intersecta.org).", 
       x="", 
       y="",
       subtitle = "Por año \n", fill="Institución:") 

# ---- Guardar en png
ggsave(file = paste(out, "plazas_2005_2022.png", sep = "/"),
       width = 16.5, height = 12, units = "cm")

# FIN --------------------------------------------------------------------------
