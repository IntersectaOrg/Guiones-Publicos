#------------------------------------------------------------------------------#
# Proyecto:                   El abuso de la fuerza de la SEDENA
# Objetivo:                   Analizar patrones sobre uso de la fuerza en los enfrentamientos de la SEDENA
#                             para el periodo que va del 2007 al 2020, basado en la respuesta a la solicitud de 
#                             acceso a la información pública con folio 0000700007321
#
# Encargada:                  Estefanía Vela Baraba
# Correo:                     evela@intersecta.org
# Fecha de creación:          25 de marzo de 2021
# Última actualización:       4 de abril de 2021
#------------------------------------------------------------------------------#

rm(list=ls())

require(pacman)
p_load(tidyverse, stringi, dplyr, plyr, foreign, readxl, janitor, beepr, ggbump,
       extrafont, treemapify, ggmosaic, srvyr, ggrepel, lubridate, scales, add2ggplot)

########## Temas para las gráficas

tema <-  theme_linedraw() +
  theme(text = element_text(family = "Georgia", color = "grey35"),
        plot.title = element_text(size = 22, face = "bold", margin = margin(10,5,5,5), family="Georgia", color = "black"),
        plot.subtitle = element_text(size = 20, color = "#666666", margin = margin(5, 5, 5, 5), family="Georgia"),
        plot.caption = element_text(hjust = 0, size = 14, family = "Georgia", color = "black"),
        panel.grid = element_line(linetype = 2), 
        legend.position = "top",
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 18, face = "bold", family="Georgia"),
        legend.text = element_text(size = 18, family="Georgia"),
        axis.title = element_text(size = 18, hjust = .5, margin = margin(1,1,1,1), family="Georgia"),
        axis.text.y = element_text(size = 14, face = "bold", family="Georgia", angle=0, hjust=.5),
        axis.text.x = element_text(size = 16, family="Georgia", angle=90, hjust=.5),
        strip.background = element_rect(fill="#525252"),
        strip.text.x = element_text(size=14, family = "Georgia"),
        strip.text.y = element_text(size=14, family = "Georgia"))

# Agregar logo

add_intlogo <- function(graf, escala){
  graf_con_logo <- add_logo(
    plot_path = graf,
    logo_path = "https://github.com/IntersectaOrg/practicas_programacion/blob/main/logo_long.jpg",
    logo_position = "bottom right",
    logo_scale = escala)
  
  magick::image_write(graf_con_logo, graf)
}

# 0. Directorios

inp     <- "input/"
out     <- "graficas/"

# 1. Cargar datos --------------------------------------------------------------

# Datos de los enfrentamientos de la SEDENA, basados en la respuesta a la solicitud 0000700007321
enfrentamientos <- read_csv(paste0(inp, "sedena_20072020.csv"))

# 2. Limpieza de las bases de datos --------------------------------------------

# La base contiene información sobre cada enfrentamiento en el que se ha visto involucrada la SEDENA. 
# Un renglón es igual a un enfrentamiento. 
# Para cada enfrentamiento se incluye información sobre: municipio, entidad, día, mes y año de ocurrencia.
# También se incluye información sobre 1) civiles ('agresores') detenidos; 2) civiles ('agresores' y 'víctimas') fallecidos; 3) civiles ('agresores' y 'víctimas') heridos; 4) militares heridos; 5) militares fallecidos. 

enfrentamientos$civ_vic_mu <- rowSums(enfrentamientos[c("vic_mu", "civ_mu")], na.rm = T) # Esto suma "agresores" y "víctimas", que es la terminología de la SEDENA

enfrentamientos$civ_vic_her <- rowSums(enfrentamientos[c("vic_her", "civ_her")], na.rm = T) # Esto suma "agresores" y "víctimas", que es la terminología de la SEDENA

enfrentamientos$tot = 1

enfrentamientos = filter(enfrentamientos, year != 2021) # se excluyen los enfrentamientos de lo que va de 2021

enfrentamientos$year = as.numeric(enfrentamientos$year)
enfrentamientos$month = as.numeric(enfrentamientos$month)
enfrentamientos$day = as.numeric(enfrentamientos$day)

enfrentamientos = mutate(enfrentamientos, fechas = as_date(paste(year, month, day))) 

enfrentamientos$entidad  = gsub("DURANGO", "Durango", enfrentamientos$entidad)
enfrentamientos$entidad  = gsub("GUANAJUATO", "Guanajuato", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("GUERRERO", "Guerrero", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("HIDALGO", "Hidalgo", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("JALISCO", "Jalisco", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("MICHOACAN", "Michoacán", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("MORELOS", "Morelos", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("NAYARIT", "Nayarit", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("NUEVO LEON", "Nuevo León", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("OAXACA", "Oaxaca", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("PUEBLA", "Puebla", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("QUERETARO", "Querétaro", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("QUINTANA ROO", "Quintana Roo", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("SAN LUIS POTOSI", "San Luis Potosí", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("SINALOA", "Sinaloa", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("SONORA", "Sonora", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("TABASCO", "Tabasco", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("TAMAULIPAS", "Tamaulipas", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("TLAXCALA", "Tlaxcala", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("VERACRUZ", "Veracruz", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("YUCATÁN", "Yucatán", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("ZACATECAS", "Zacatecas", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("AGUASCALIENTES", "Aguascalientes", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("BAJA CALIFORNIA SUR", "Baja California Sur", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("BAJA CALIFORNIA", "Baja California", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("CAMPECHE", "Campeche", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("COAHUILA", "Coahuila", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("COLIMA", "Colima", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("CHIAPAS", "Chiapas", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("CHIHUAHUA", "Chihuahua", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("DISTRITO FEDERAL", "CDMX", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("MEXICO", "Estado de México", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("CDMX", "Ciudad de México", enfrentamientos$entidad )
enfrentamientos$entidad  = gsub("NUEVO LEÓN", "Nuevo León", enfrentamientos$entidad )

# Esto sirve para saber si en los enfrentamientos hubo civiles/militares fallecidos, heridos y detenidos. 
# En concreto: las combinaciones entre las variables. 

enfrentamientos <- enfrentamientos%>%
  mutate(total_muertos=mil_mu+civ_mu,
         civiles_mu = civ_mu+vic_mu,
         civiles_her = civ_her+vic_her,
         total_algo=mil_mu+civiles_mu+mil_her+civiles_her+civ_det,
         combinaciones=ifelse(total_algo==0, 1, 
                              ifelse(total_algo>0 & total_algo-civiles_mu==0, 2, 
                                     ifelse(total_algo>0 & total_algo-civ_det==0, 3, 
                                            ifelse(total_algo>0 & total_algo-mil_her==0, 4,
                                                   ifelse(civ_det>0 & civiles_mu>0 & mil_mu==0 & mil_her==0 & civiles_her==0, 5,
                                                          ifelse(civ_det>0 & civiles_her>0 & mil_mu==0 & mil_her==0 & civiles_mu==0, 6,
                                                                 ifelse(mil_her>0 & civiles_mu>0 & mil_mu==0 & civ_det==0 & civiles_her==0, 7,
                                                                        ifelse(civiles_mu>0 & civiles_her>0 & civ_det>0 & mil_her==0 & mil_mu==0, 8, 
                                                                               ifelse(total_algo>0 & total_algo-mil_mu==0, 9,
                                                                                      ifelse(mil_mu>0 & mil_her>0 & civ_det==0 & civiles_mu==0 & civiles_her==0, 10,
                                                                                             ifelse(mil_mu>0 & mil_her>0 & civ_det==0 & civiles_mu>0 & civiles_her==0, 11,
                                                                                                    ifelse(mil_mu>0 & mil_her==0 & civ_det==0 & civiles_mu>0 & civiles_her==0, 12, 13)))))))))))),
         combinaciones=factor(combinaciones, levels=c(1:13), labels=c("Sin nada de nada",
                                                                      "Solo civiles fallecidos",
                                                                      "Solo civiles detenidos",
                                                                      "Solo militares heridos",
                                                                      "Solo civiles fallecidos y detenidos",
                                                                      "Solo civiles detenidos y  heridos",
                                                                      "Solo civiles fallecidos y militares heridos",
                                                                      "Solo civiles fallecidos, heridos y detenidos",
                                                                      "Solo militares fallecidos",
                                                                      "Solo militares fallecidos y heridos",
                                                                      "Militares fallecidos y heridos, con civiles fallecidos",
                                                                      "Con militares y civiles fallecidos",
                                                                      "Otros")))

# Esto sirve para saber si en los enfrentamientos hubo o no civiles/militares fallecidos, heridos, detenidos, por separado. 

enfrentamientos$sin_saldo <- rowSums(enfrentamientos[c("vic_her", "civ_her", "vic_mu", "civ_mu", "mil_mu", "mil_her", "civ_det")], na.rm = T)

enfrentamientos$civ_mu_2 <- ifelse(enfrentamientos$civ_vic_mu > 0, c("1"), c("0")) # ¿Hubo civiles fallecidos?
enfrentamientos$civ_her_2 <- ifelse(enfrentamientos$civ_vic_her > 0, c("1"), c("0")) # ¿Hubo civiles heridos?
enfrentamientos$civ_det_2 <- ifelse(enfrentamientos$civ_det > 0, c("1"), c("0")) # ¿Hubo civiles detenidos?
enfrentamientos$mil_mu_2 <- ifelse(enfrentamientos$mil_mu > 0, c("1"), c("0")) # ¿Hubo militares fallecidos?
enfrentamientos$mil_her_2 <- ifelse(enfrentamientos$mil_her > 0, c("1"), c("0")) # ¿Hubo militares heridos?
enfrentamientos$sin_saldo_2 <- ifelse(enfrentamientos$sin_saldo > 0, c("0"), c("1")) # ¿No hubo nada de nada?

enfrentamientos$civ_mu_2 = as.numeric(enfrentamientos$civ_mu_2)
enfrentamientos$civ_her_2 = as.numeric(enfrentamientos$civ_her_2)
enfrentamientos$civ_det_2 = as.numeric(enfrentamientos$civ_det_2)
enfrentamientos$mil_mu_2 = as.numeric(enfrentamientos$mil_mu_2)
enfrentamientos$mil_her_2 = as.numeric(enfrentamientos$mil_her_2)
enfrentamientos$sin_saldo_2 = as.numeric(enfrentamientos$sin_saldo_2)

# División por sexenios: esto sirve para hacer análisis de los enfrentamientos que ocurrieron en cada sexenio. 

enfrentamientos = mutate(enfrentamientos, sexenios = case_when(fechas  >= "2007-01-01" & fechas < "2012-12-01" ~ "Calderón Hinojosa",
                                                               fechas  >= "2012-12-01" & fechas < "2018-12-01" ~ "Peña Nieto",
                                                               fechas  >= "2018-12-01" & fechas < "2021-01-01" ~ "López Obrador"))

# ¿Cuántos enfrentamientos hubo, en total?

total <- enfrentamientos %>% 
  group_by() %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) 

# ¿Cuántos enfrentamientos hubo cada año?

porano <- enfrentamientos %>% 
  group_by(year) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by() %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin, 1))

ggplot(porano, aes(x = year, y=total)) +
  geom_bar(stat="identity", position="stack") +
  annotate(geom = "text", x = 2016, y = 1000, label = "Enfrentamientos totales: 4,997", size = 8, family = "Georgia", color = "black")  +
  geom_label(aes(label=paste0(total)),
             position = position_stack(1), size=6, hjust=.5, vjust=.5, color="black", family = "Georgia", fill = "white")+
  labs(title="Enfrentamientos entre la SEDENA y supuestos grupos delictivos",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n", y="\n Enfrentamientos \n",
       subtitle = "Por año \n", fill = "") +
  tema +
  theme(axis.text.x = element_text(angle=0, size = 18)) +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) 

ggsave(paste(out, "01 enfrentamientos por año.png", sep = "/"), width = 20, height = 16)

add_intlogo(graf = paste(out, "01 enfrentamientos por año.png", sep = "/"), escala = 7)

# ¿En qué entidades ocurrieron los enfrentamientos?

porano <- enfrentamientos %>% 
  group_by(entidad) %>% 
  mutate(tot = 1) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by() %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 1))

ggplot(porano, aes(x = reorder(entidad, porcent), y=porcent)) +
  geom_bar(stat="identity", position="stack") +
  geom_label(aes(label=paste0(porcent, "%", " ", "(", total, ")")),
             position = position_stack(1), size=4, hjust=.5, vjust=.5, color="black", family = "Georgia", fill = "white")+
  labs(title="Enfrentamientos entre la SEDENA y supuestos grupos delictivos (2007-2020)",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321. 
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n", y="\n Distribución del total \n",
       subtitle = "Por entidad federativa \n", fill = "") +
  tema +
  theme(axis.text.x = element_text(angle=90, size = 10)) +
  coord_flip()
ggsave(paste(out, "02 enfrentamientos por entidad.png", sep = "/"), width = 20, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/02 enfrentamientos por entidad.png", escala = 7)

# ¿En qué entidades ocurrieron los enfrentamientos, cada año?

eventos = group_by(enfrentamientos, year, entidad)
eventos = summarize(eventos, total = sum(tot, na.rm=T))
eventos = ungroup(eventos)
eventos = group_by(eventos, year)
eventos = mutate(eventos,
                 total_eventos = sum(total, na.rm=T),
                 porcent = round(total / total_eventos * 100, digits=2))

ggplot(eventos, aes(x=as.character(year), y=reorder(entidad, porcent), fill = porcent)) +
  geom_tile(color="black") +
  scale_fill_continuous(low="white", high="red") + 
  labs(title="Enfrentamientos entre la SEDENA y supuestos grupos delictivos", subtitle = "Por año\n",
       caption = "\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Si sale en blanco, es porque no hubo enfrentamientos.
Datos procesados por Intersecta (intersecta.org).\n",
       x="", y="\n Entidad federativa \n", fill="Porcentaje") +
  geom_text(aes(label=paste0(porcent,"%")), size=5, hjust=.5, vjust=.5, color="black", family = "Georgia")+
  tema +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(position = "top") 
ggsave(paste(out, "03 enfrentamientos, por año, por entidad.png", sep="/"), width=16, height=16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/03 enfrentamientos, por año, por entidad.png", escala = 7)

# En total, ¿cuántas personas (civiles o militares) han sido fallecidas, heridas o detenidas en enfrentamientos?

aver <- enfrentamientos %>% 
  mutate(tot = 1) %>% 
  select(civ_vic_mu, civ_vic_her, civ_det, mil_her, mil_mu)%>%
  gather(muertes, tot, civ_vic_mu:mil_mu)%>%
  mutate(muertes = str_replace(muertes, "civ_vic_her", "Civiles heridos"),
         muertes = str_replace(muertes, "civ_vic_mu", "Civiles fallecidos"),
         muertes = str_replace(muertes, "civ_det", "Civiles detenidos"),
         muertes = str_replace(muertes, "mil_her", "Militares heridos"),
         muertes = str_replace(muertes, "mil_mu", "Militares fallecidos"))%>% 
  ungroup() %>% 
  group_by(muertes) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by() %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 2))

ggplot(aver, aes(x = factor(muertes, levels= c("Militares fallecidos", "Militares heridos", "Civiles detenidos", 
                                               "Civiles heridos", "Civiles fallecidos")), y=total)) +
  geom_bar(stat="identity", position="stack") +
  guides(fill = guide_legend(reverse=TRUE))+
  geom_label(aes(label=paste0(total)),
             position = position_stack(1), size=6, hjust=.5, vjust=.5, color="black", family = "Helvetica", fill = "white")+
  labs(title="Personas lastimadas, fallecidas o detenidas en los enfrentamientos de la SEDENA (2007-2020)",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n", y="\n Totales \n",
       subtitle = "Según su distribución, de un total de 4,997 enfrentamientos \n", fill = "") +
  tema +
  theme(axis.text.x = element_text(angle=0, size = 18)) +
  theme(legend.position = "none") +
  coord_flip()
ggsave(paste(out, "04 total de personas fallecidas, heridas, detenidas.png", sep = "/"), width = 18, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/04 total de personas fallecidas, heridas, detenidas.png", escala = 7)

# En los enfrentamientos, ¿hubo civiles/militares fallecidos, heridos, detenidos?

enfrentamientos$tot = as.numeric(enfrentamientos$tot)

porano <- enfrentamientos %>% 
  mutate(tot = 1) %>% 
  select(civ_mu_2, civ_her_2, civ_det_2, mil_mu_2, mil_her_2, sin_saldo_2)%>%
  gather(participacion, tot, civ_mu_2:sin_saldo_2)%>%
  mutate(participacion = str_replace(participacion, "civ_mu_2", "Con civiles fallecidos"),
         participacion = str_replace(participacion, "civ_her_2", "Con civiles heridos"),
         participacion = str_replace(participacion, "civ_det_2", "Con civiles detenidos"),
         participacion = str_replace(participacion, "mil_mu_2", "Con militares fallecidos"),
         participacion = str_replace(participacion, "sin_saldo_2", "Sin nada"),
         participacion = str_replace(participacion, "mil_her_2", "Con militares heridos"))%>% 
  ungroup() %>% 
  group_by(participacion)%>% 
  summarize(total = sum(as.numeric(tot),na.rm=T)) %>% 
  ungroup() %>% 
  group_by() %>% 
  mutate(porcent = round(total / 4997 * 100, 2))%>%
  filter(participacion != "Sin nada")

ggplot(porano, aes(x = reorder(participacion, porcent), y=porcent)) +
  geom_bar(stat="identity", position="stack") +
  geom_label(aes(label=paste0(porcent, "%", "\n", "(", total, ")")),
             position = position_stack(1), size=4, hjust=.5, vjust=.5, color="black", family = "Georgia", fill = "white")+
  labs(title="Los enfrentamientos de la SEDENA (2007-2020)",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321. 
Los porcentajes no suman 100 porque en un evento pudo haber militares y civiles fallecidos, heridos y detenidos.
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n Enfrentamientos \n", y="\n Porcentaje\n",
       subtitle = "Según si hubo o no militares y civiles fallecidos, heridos y detenidos \n", fill = "") +
  tema +
  theme(axis.text.x = element_text(angle=0, size = 14)) +
  coord_flip()
ggsave(paste(out, "05 enfrentamientos con.png", sep = "/"), width = 16, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/05 enfrentamientos con.png", escala = 7)

# En qué porcentaje de los enfrentamientos, para cada año: ¿hubo civiles/militares fallecidos, heridos, detenidos?

aver <- enfrentamientos %>% 
  group_by(year, civ_mu_2) %>% 
  summarize(eventos = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(total_total = sum(eventos, na.rm=T),
         porcent = round(eventos / total_total * 100, 2))
aver = filter(aver, civ_mu_2 == 1)
aver$civ_mu_2  = gsub("1", "Con civiles fallecidos", aver$civ_mu_2)
aver = rename(aver, eventos_con = civ_mu_2)

aver_1 <- enfrentamientos %>% 
  group_by(year, civ_det_2) %>% 
  summarize(eventos = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(total_total = sum(eventos, na.rm=T),
         porcent = round(eventos / total_total * 100, 2))
aver_1 = filter(aver_1, civ_det_2 == 1)
aver_1$civ_det_2  = gsub("1", "Con civiles detenidos", aver_1$civ_det_2)
aver_1 = rename(aver_1, eventos_con = civ_det_2)

aver_2 <- enfrentamientos %>% 
  group_by(year, civ_her_2) %>% 
  summarize(eventos = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(total_total = sum(eventos, na.rm=T),
         porcent = round(eventos / total_total * 100, 2))
aver_2 = filter(aver_2, civ_her_2 == 1)
aver_2$civ_her_2  = gsub("1", "Con civiles heridos", aver_2$civ_her_2)
aver_2 = rename(aver_2, eventos_con = civ_her_2)

aver_3 <- enfrentamientos %>% 
  group_by(year, mil_mu_2) %>% 
  summarize(eventos = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(total_total = sum(eventos, na.rm=T),
         porcent = round(eventos / total_total * 100, 2))
aver_3 = filter(aver_3, mil_mu_2 == 1)
aver_3$mil_mu_2  = gsub("1", "Con militares fallecidos", aver_3$mil_mu_2)
aver_3 = rename(aver_3, eventos_con = mil_mu_2)

aver_4 <- enfrentamientos %>% 
  group_by(year, mil_her_2) %>% 
  summarize(eventos = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(total_total = sum(eventos, na.rm=T),
         porcent = round(eventos / total_total * 100, 2))
aver_4 = filter(aver_4, mil_her_2 == 1)
aver_4$mil_her_2  = gsub("1", "Con militares heridos", aver_4$mil_her_2)
aver_4 = rename(aver_4, eventos_con = mil_her_2)

eventos <- bind_rows(aver, aver_1)
eventos <- bind_rows(eventos, aver_2)
eventos <- bind_rows(eventos, aver_3)
eventos <- bind_rows(eventos, aver_4)

ggplot(eventos, aes(x = year, y=porcent)) +
  geom_bar(stat="identity", position="stack") +
  geom_label(aes(label=paste0(porcent, "%")),
             position = position_stack(1), size=3, hjust=.5, vjust=.5, color="black", family = "Georgia", fill = "white")+
  labs(title="Enfrentamientos de la SEDENA",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n", y="\n Proporción de enfrentamientos\n",
       subtitle = "Por año \n", fill = "") +
  tema +
  facet_wrap(~eventos_con) +
  theme(axis.text.x = element_text(angle=90, size = 12)) +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) 
ggsave(paste(out, "06 enfrentamientos con, por año.png", sep = "/"), width = 20, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/06 enfrentamientos con, por año.png", escala = 7)

# ¿Qué proporción de los enfrentamientos fueron 1) solo con civiles fallecidos; 2) solo con militares fallecidos; 3) solo con civiles detenidos?

eventos = group_by(enfrentamientos, year, combinaciones)
eventos = summarize(eventos, eventos = sum(tot, na.rm=T))
eventos = ungroup(eventos)
eventos = group_by(eventos, year)
eventos = mutate(eventos,
                 total_eventos = sum(eventos, na.rm=T),
                 porcent_eventos = round(eventos / total_eventos * 100, digits=2))
eventos = filter(eventos, combinaciones == "Solo civiles fallecidos" | combinaciones == "Solo militares fallecidos" |
                   combinaciones == "Solo civiles detenidos")

ggplot(eventos, aes(x = year, y=porcent_eventos, 
                    fill = combinaciones)) +
  geom_bar(stat="identity", position="stack") +
  geom_label(aes(label=paste0(porcent_eventos, "%"), 
                 group = combinaciones),
             position = position_stack(1), size=5, hjust=.5, vjust=.5, color="black", family = "Georgia", fill = "white")+
  labs(title="Los enfrentamientos de la SEDENA",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n", y="\n Porcentaje \n",
       subtitle = "En los que o solo hubo civiles fallecidos, o solo hubo civiles detenidos o solo hubo militares fallecidos, por año\n", fill = "En los enfrentamientos hubo:") +
  tema +
  theme(legend.position = "top") +
  theme(axis.text.x = element_text(angle=0, size = 16)) +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) +
  scale_fill_manual(values = c("red","black", "gray"))
ggsave(paste(out, "07 enfrentamientos solo con, por año.png", sep = "/"), width = 16, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/07 enfrentamientos solo con, por año.png", escala = 7)

# Civiles muertos y civiles heridos, por año, con su respectivo índice (de letalidad)

aver <- enfrentamientos %>% 
  mutate(tot = 1) %>% 
  select(year, civ_vic_mu, civ_vic_her)%>%
  gather(muertes, tot, civ_vic_mu:civ_vic_her)%>%
  mutate(muertes = str_replace(muertes, "civ_vic_her", "Civiles heridos"),
         muertes = str_replace(muertes, "civ_vic_mu", "Civiles fallecidos"))%>% 
  ungroup() %>% 
  group_by(year, muertes) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 2))

eventos = group_by(enfrentamientos, year)
eventos = summarize(eventos, fallecidos = sum(civ_vic_mu, na.rm=T),
                    heridos = sum(civ_vic_her, na.rm=T))
eventos = ungroup(eventos)
eventos = group_by(eventos, year)
eventos = mutate(eventos, 
                 indice_year = round(fallecidos / heridos, 1))

df_eventos <- eventos %>% 
  select(year, indice_year)

df_eventos_aver <- aver %>% 
  select(year, muertes, total)

personas = left_join(df_eventos_aver, df_eventos, by=c("year"))
personas =  mutate(personas, indice_year_adj = indice_year*100)

ggplot(data = personas, aes(x = as.integer(year), y = total, fill = muertes),
       position = position_stack(1), size=4, hjust=.5, vjust=.5, color="black", 
       family = "Georgia", fill = "white") +
  # Valores absolutos de personas heridas y fallecidas
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(label=paste0(total), group = muertes),
             position = position_dodge(1), size=4, hjust=.5, vjust=.5, color="black", 
             family = "Georgia", fill = "white") +
  # Índice de letalidad
  geom_point(aes(x = as.integer(year), y = indice_year_adj), 
             size=3, color = "gray", show.legend = FALSE) +
  geom_line(aes(x = as.integer(year), y = indice_year_adj, group = 1), 
            size=2, color = "gray") +
  geom_label(aes(x = as.integer(year), y = indice_year_adj, label = (indice_year), fill = c("white")), 
             hjust = -0.2, vjust = 0.7, show.legend = FALSE) +
  # Establecer segunda escala
  scale_y_continuous(sec.axis = sec_axis(~ . / 100, name = "\n Índice \n Civiles fallecidos por cada civil herido \n"))  +
  # Etiquetado de la gráfica 
  labs(title = "Civiles heridos y fallecidos en los enfrentamientos de la SEDENA", 
       subtitle = "Por año, con su respectivo índice \n", 
       y = "\n Civiles fallecidos y civiles heridos en enfrentamientos \n", x="",
       caption = "\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública con folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       fill ="") +
  tema+
  theme(legend.position = "top") +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) +
  theme(axis.text.x = element_text(angle=0, size = 14)) +
  # Agregar el argumento breaks 
  scale_fill_manual(values = c("red", "black", "white"), 
                    breaks = c("Civiles fallecidos", "Civiles heridos"))
ggsave(paste(out, "08 civiles muertos y heridos.png", sep = "/"), width = 16, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/08 civiles muertos y heridos.png", escala = 7)

# Civiles muertos y detenidos por año, con su respectivo índice

aver <- enfrentamientos %>% 
  mutate(tot = 1) %>% 
  select(year, civ_vic_mu, civ_det)%>%
  gather(muertes, tot, civ_vic_mu:civ_det)%>%
  mutate(muertes = str_replace(muertes, "civ_det", "Civiles detenidos"),
         muertes = str_replace(muertes, "civ_vic_mu", "Civiles fallecidos"))%>% 
  ungroup() %>% 
  group_by(year, muertes) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 2))

eventos = group_by(enfrentamientos, year)
eventos = summarize(eventos, fallecidos = sum(civ_vic_mu, na.rm=T),
                    detenidos = sum(civ_det, na.rm=T))
eventos = ungroup(eventos)
eventos = group_by(eventos, year)
eventos = mutate(eventos, 
                 indice_year = round(fallecidos / detenidos, 1))

df_eventos <- eventos %>% 
  select(year, indice_year)

df_eventos_aver <- aver %>% 
  select(year, muertes, total)

personas = left_join(df_eventos_aver, df_eventos, by=c("year"))
personas =  mutate(personas, indice_year_adj = indice_year*100)

ggplot(data = personas, aes(x = as.integer(year), y = total, fill = muertes),
       position = position_stack(1), size=4, hjust=.5, vjust=.5, color="black", 
       family = "Georgia", fill = "white") +
  # Valores absolutos de personas heridas y fallecidas
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(label=paste0(total), group = muertes),
             position = position_dodge(1), size=4, hjust=.5, vjust=.5, color="black", 
             family = "Georgia", fill = "white") +
  # Índice de letalidad
  geom_point(aes(x = as.integer(year), y = indice_year_adj), 
             size=3, color = "gray", show.legend = FALSE) +
  geom_line(aes(x = as.integer(year), y = indice_year_adj, group = 1), 
            size=2, color = "gray") +
  geom_label(aes(x = as.integer(year), y = indice_year_adj, label = (indice_year), fill = c("white")), 
             hjust = -0.2, vjust = 0.7, show.legend = FALSE) +
  # Establecer segunda escala
  scale_y_continuous(sec.axis = sec_axis(~ . / 100, name = "\n Índice \n Civiles fallecidos por cada civil detenido \n"))  +
  # Etiquetado de la gráfica 
  labs(title = "Civiles fallecidos y detenidos en los enfrentamientos de la SEDENA", 
       subtitle = "Por año, con su respectivo índice \n", 
       y = "\n Civiles fallecidos y civiles detenidos en enfrentamientos \n", x="",
       caption = "\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública con folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       fill ="") +
  tema+
  theme(legend.position = "top") +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) +
  theme(axis.text.x = element_text(angle=0, size = 14)) +
  # Agregar el argumento breaks 
  scale_fill_manual(values = c("black", "red", "white"), 
                    breaks = c("Civiles fallecidos", "Civiles detenidos"))
ggsave(paste(out, "09 civiles muertos y detenidos.png", sep = "/"), width = 16, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/09 civiles muertos y detenidos.png", escala = 7)

# Civiles muertos y militares muertos por año, con su respectivo índice

aver <- enfrentamientos %>% 
  mutate(tot = 1) %>% 
  select(year, civ_vic_mu, mil_mu)%>%
  gather(muertes, tot, civ_vic_mu:mil_mu)%>%
  mutate(muertes = str_replace(muertes, "mil_mu", "Militares fallecidos"),
         muertes = str_replace(muertes, "civ_vic_mu", "Civiles fallecidos"))%>% 
  ungroup() %>% 
  group_by(year, muertes) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 2))

eventos = group_by(enfrentamientos, year)
eventos = summarize(eventos, civ_fallecidos = sum(civ_vic_mu, na.rm=T),
                    mil_fallecidos = sum(mil_mu, na.rm=T))
eventos = ungroup(eventos)
eventos = group_by(eventos, year)
eventos = mutate(eventos, 
                 indice_year = round(civ_fallecidos / mil_fallecidos, 1))

df_eventos <- eventos %>% 
  select(year, indice_year)

df_eventos_aver <- aver %>% 
  select(year, muertes, total)

personas = left_join(df_eventos_aver, df_eventos, by=c("year"))
personas =  mutate(personas, indice_year_adj = indice_year*100)

ggplot(data = personas, aes(x = as.integer(year), y = total, fill = muertes),
       position = position_stack(1), size=4, hjust=.5, vjust=.5, color="black", 
       family = "Georgia", fill = "white") +
  # Valores absolutos de personas heridas y fallecidas
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(label=paste0(total), group = muertes),
             position = position_dodge(1), size=4, hjust=.5, vjust=.5, color="black", 
             family = "Georgia", fill = "white") +
  # Índice de letalidad
  geom_point(aes(x = as.integer(year), y = indice_year_adj), 
             size=3, color = "gray", show.legend = FALSE) +
  geom_line(aes(x = as.integer(year), y = indice_year_adj, group = 1), 
            size=2, color = "gray") +
  geom_label(aes(x = as.integer(year), y = indice_year_adj, label = (indice_year), fill = c("white")), 
             hjust = -0.2, vjust = 0.7, show.legend = FALSE) +
  # Establecer segunda escala
  scale_y_continuous(sec.axis = sec_axis(~ . / 100, name = "\n Índice \n Civiles fallecidos por cada militar fallecido \n"))  +
  # Etiquetado de la gráfica 
  labs(title = "Civiles fallecidos y militares fallecidos en los enfrentamientos de la SEDENA", 
       subtitle = "Por año, con su respectivo índice \n", 
       y = "\n Civiles fallecidos y militares fallecidos en enfrentamientos \n", x="",
       caption = "\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública con folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       fill ="") +
  tema+
  theme(legend.position = "top") +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) +
  theme(axis.text.x = element_text(angle=0, size = 14)) +
  # Agregar el argumento breaks 
  scale_fill_manual(values = c("red", "black", "white"), 
                    breaks = c("Civiles fallecidos", "Militares fallecidos"))
ggsave(paste(out, "10 civiles muertos y militares muertos.png", sep = "/"), width = 16, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/10 civiles muertos y militares muertos.png", escala = 7)

# ¿Cuál es el resultado más común de los enfrentamientos, considerando las combinaciones de civiles/militares detenidos/heridos/fallecidos?

eventos = group_by(enfrentamientos, combinaciones)
eventos = summarize(eventos, eventos = sum(tot, na.rm=T))
eventos = ungroup(eventos)
eventos = group_by(eventos)
eventos = mutate(eventos,
                 total_eventos = sum(eventos, na.rm=T),
                 porcent_eventos = round(eventos / total_eventos * 100, digits=2))

ggplot(eventos, aes(x = reorder(combinaciones, porcent_eventos), y=porcent_eventos)) +
  geom_bar(stat="identity", position="stack", color = "black") +
  guides(fill = guide_legend(reverse=TRUE))+
  geom_label(aes(label=paste0(porcent_eventos, "%"), group = combinaciones),
             position = position_stack(1), size=6, hjust=.5, vjust=.5, color="black", family = "Helvetica", fill = "white")+
  labs(title="Los enfrentamientos de la SEDENA (2007-2020)",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n",
       x="\n En el enfrentamiento hubo: \n", y=" \n Porcentaje \n", fill = "",
       subtitle = "Según si hubo o no militares o civiles detenidos, heridos o fallecidos\n") +
  tema +
  theme(axis.text.x = element_text(angle=0, size = 14)) +
  coord_flip()
ggsave(paste(out, "11 tipos de eventos.png", sep = "/"), width = 20, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/11 tipos de eventos.png", escala = 7)

# De las personas 'lastimadas', ¿cuántos son civiles/militares muertos y heridos?

aver <- enfrentamientos %>% 
  mutate(tot = 1) %>% 
  select(year, civ_vic_mu, civ_vic_her, mil_mu, mil_her)%>%
  gather(muertes, tot, civ_vic_mu:mil_her)%>%
  mutate(muertes = str_replace(muertes, "civ_vic_her", "Civiles heridos"),
         muertes = str_replace(muertes, "civ_vic_mu", "Civiles fallecidos"),
         muertes = str_replace(muertes, "mil_mu", "Militares fallecidos"),
         muertes = str_replace(muertes, "mil_her", "Militares heridos"))%>% 
  ungroup() %>% 
  group_by(year, muertes) %>% 
  summarize(total = sum(tot, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(denomin = sum(total, na.rm = T),
         porcent = round(total / denomin * 100, 2))

ggplot(aver, aes(x = year, y=porcent, 
                 fill = factor(muertes, levels= c("Militares heridos", "Militares fallecidos", 
                                                  "Civiles heridos", "Civiles fallecidos")))) +
  geom_bar(stat="identity", position="stack") +
  geom_label(aes(label=paste0(porcent, "%"), 
                 group = factor(muertes, levels= c("Militares heridos", "Militares fallecidos", "Civiles heridos", "Civiles fallecidos"))),
             position = position_stack(1), size=3, hjust=.5, vjust=.5, color="black", family = "Helvetica", fill = "white")+
  labs(title="Personas heridas y fallecidas en los enfrentamientos de la SEDENA",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n", y="\n Porcentaje \n",
       subtitle = "Por año\n", fill = "Las personas eran:") +
  tema +
  theme(legend.position = "right") +
  theme(axis.text.x = element_text(angle=0, size = 14)) +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) +
  scale_fill_manual(values = c("gray", "#fb8500", "black","red"))
ggsave(paste(out, "12 personas lastimadas.png", sep = "/"), width = 16, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/12 personas lastimadas.png", escala = 7)

# ¿Qué proporción de civiles  fallecieron en eventos con solo civiles fallecidos, por año?

eventos = group_by(enfrentamientos, year, combinaciones)
eventos = summarize(eventos, fallecidos = sum(civ_vic_mu, na.rm=T))
eventos = ungroup(eventos)
eventos = group_by(eventos, year)
eventos = mutate(eventos,
                 total_fallecidos = sum(fallecidos, na.rm=T),
                 porcent_eventos = round(fallecidos / total_fallecidos * 100, digits=2))
eventos = filter(eventos, combinaciones == "Solo civiles fallecidos")

ggplot(eventos, aes(x = year, y=porcent_eventos, 
                    fill = combinaciones)) +
  geom_bar(stat="identity", position="stack") +
  geom_label(aes(label=paste0(porcent_eventos, "%"), 
                 group = combinaciones),
             position = position_stack(1), size=6, hjust=.5, vjust=.5, color="black", family = "Georgia", fill = "white")+
  labs(title="Del total de civiles que fallecieron en enfrentamientos de la SEDENA,
       ¿qué proporción falleció en enfrentamientos en los que solo hubo civiles fallecidos?",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n", y="\n Porcentaje \n",
       subtitle = "Esto es: en eventos sin civiles heridos, ni detenidos y sin militares fallecidos, ni detenidos\n", fill = "") +
  tema +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle=0, size = 14)) +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) +
  scale_fill_manual(values = c("red"))
ggsave(paste(out, "13 civiles fallecidos en eventos solo con civiles fallecidos.png", sep = "/"), width = 16, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/13 civiles fallecidos en eventos solo con civiles fallecidos.png", escala = 7)

# En promedio, ¿cuántos civiles han fallecido considerando el total de los enfrentamientos, para cada año?

aver <- enfrentamientos %>% 
  group_by(year) %>% 
  summarize(eventos = sum(tot, na.rm = T),
            personas = sum(civ_vic_mu, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(promedio = round(personas / eventos, 2))

ggplot(aver, aes(x = year, y=promedio)) +
  geom_bar(stat="identity", position="stack") +
  geom_label(aes(label=paste0(promedio)),
             position = position_stack(1), size=6, hjust=.5, vjust=.5, color="black", family = "Georgia", fill = "white")+
  labs(title="Promedio de civiles fallecidos en enfrentamientos de la SEDENA",
       caption="\nFuente: Respuesta de la SEDENA a una solicitud de acceso a la información pública. Folio número 0000700007321.
Datos procesados por Intersecta (intersecta.org).\n", 
       x="\n", y="\n Promedio de civiles fallecidos \n en enfrentamientos de la SEDENA \n",
       subtitle = "Por año \n", fill = "") +
  tema +
  theme(axis.text.x = element_text(angle=0, size = 18)) +
  scale_x_continuous(breaks=seq(from=2007, to=2020, by=1)) 
ggsave(paste(out, "14 promedio de civiles fallecidos.png", sep = "/"), width = 20, height = 16)

add_intlogo(graf = "/Users/samnbk/Documents/GitHub/FFAA/out/sedena/enfrentamientos/parapublicar/14 promedio de civiles fallecidos.png", escala = 7)

