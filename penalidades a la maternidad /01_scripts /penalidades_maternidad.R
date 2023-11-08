##
## Descripción:    Graficas y estimaciones de la penalidades a la maternidad 
##
## Autor:          Daniela Martinez Rodriguez
##
## Fecha creac.:   7 de noviembre del 2023
##
## Email:          dmartinez@intersecta.org
##
## Notas:          

# Setup ----
## Paquetes a utilizar ----
pacman::p_load(tidyverse, janitor, writexl, readxl, scales, mytidyfunctions, presupuestoR)

## Especificar locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Desabilitar notación científica.----
options(scipen = 999)

## Cargar bases de datos de la ENOE de 2019 2022 ----
datos_combinados <- read.csv("02_proc-data/sdem_coe2.csv")


## Tendencias de uso del tiempo ----
      
# 1. Grafica los promedios de horas trabajadas por las mujeres (con y sin hijos)
      
      ggplot(datos_combinados, aes(x = fecha, y = promedio, group = Grupo, color = Grupo)) +
        geom_line(size=1.5) +
        labs(title= "Promedio de horas trabajadas por mujeres entre 15-65 años \n", x = "Fecha", y = "Horas de trabajo remunerado a la semana", fill = element_blank()) +
        geom_vline(xintercept = "2020-03-01", color = "#C52727", linetype = "dashed", size = 1) + 
        scale_color_manual(values = c("Mujeres con hijos" = "#592686", "Mujeres sin hijos" = "#9F33FF")) + 
        annotate("text", x = "2020-03-01", y = max(hrsocup_promedios$promedio) +0.5 , 
                 label = "Pandemia COVID-19", color = "#C52727", angle = 90, hjust = 1, vjust = -1) +
        theme_linedraw() +
        theme(
          text             = element_text(family = "Lora", color = "#16171d"),
          plot.title       = element_text(size = 15, face = "bold", hjust = 0, margin = margin(10,10,5,5), family="Lora", color = "#16171d"),
          plot.subtitle    = element_text(size = 15, face = "italic", color = "#16171d", hjust = 0, margin = margin(5, 5, 5, 5), family="Lora"),
          plot.caption     = element_text(hjust = 0, size = 9, family = "Lora", color = "#16171d"),
          panel.grid       = element_line(linetype = 2), 
          legend.position  = "top",
          panel.grid.minor = element_blank(),
          legend.title     = element_text(size = 12, face = "bold", family="Lora"),
          legend.text      = element_text(size = 12, family="Lora"),
          axis.title       = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Lora"),
          axis.text.y      = element_text(size = 12, family="Lora", angle=0, hjust=.5),
          axis.text.x      = element_text(size = 12, family="Lora", angle=90, hjust=.5),
          strip.background = element_rect(fill="white", colour = NA),
          strip.text.x     = element_text(size=10, family = "Lora", face = "bold", color = "#16171d"),
          strip.text.y     = element_text(size=10, family = "Lora", face = "bold", color = "#16171d"))
      
      ggsave_mult(c(".png", ".jpg"),
                  path_name = paste0("02_fig-output/",  "trabajo-remunerado-mujeres-combinado"),
                  units = "mm",
                  width = 180,                 # Ancho de la gráfica
                  height = 120,
                  dpi = 600)

# 2 Grafica de promedio de horas trabajadas por las mujeres con hijos, por sectores
            
      hrsocup_promedios_6$Grupo <- 'Agropecuario'
      hrsocup_promedios_1$Grupo <- 'Construcción'
      hrsocup_promedios_2$Grupo <- 'Industria Manufacturera'
      hrsocup_promedios_3$Grupo <- 'Comercio'
      hrsocup_promedios_4$Grupo <- 'Servicios'
      
      datos_combinados <- rbind(hrsocup_promedios_6, hrsocup_promedios_1, hrsocup_promedios_2, hrsocup_promedios_3, hrsocup_promedios_4)
      
      # Grafica los datos combinados
      ggplot(datos_combinados, aes(x = fecha, y = promedio, group = Grupo, color = Grupo)) +
        geom_line(size =1) +
        labs(x = "Fecha", y = "Horas de trabajo remunerado a la semana") +
        ggtitle("Promedio de horas trabajadas por mujeres con hijos por sector económico \n")  +
        scale_color_manual(values = c("Agropecuario" = "#C79FE7", "Construcción" = "#4A235A", "Industria Manufacturera" = "#8E44AD", "Comercio" = "#A373E5", "Servicios" = "#D073E5")) + 
        geom_vline(xintercept = "2020-03-01", color = "#C52727", linetype = "dashed") + 
        annotate("text", x = "2020-03-01", y = max(hrsocup_promedios$promedio) +0.5 , 
                 label = "Pandemia COVID-19", color = "#C52727", angle = 90, hjust = 1, vjust = -1) +
        theme_linedraw() +
        theme(
          text             = element_text(family = "Lora", color = "#16171d"),
          plot.title       = element_text(size = 15, face = "bold", hjust = 0, margin = margin(10,10,5,5), family="Lora", color = "#16171d"),
          plot.subtitle    = element_text(size = 15, face = "italic", color = "#16171d", hjust = 0, margin = margin(5, 5, 5, 5), family="Lora"),
          plot.caption     = element_text(hjust = 0, size = 9, family = "Lora", color = "#16171d"),
          panel.grid       = element_line(linetype = 2), 
          legend.position  = "top",
          panel.grid.minor = element_blank(),
          legend.title     = element_text(size = 12, face = "bold", family="Lora"),
          legend.text      = element_text(size = 12, family="Lora"),
          axis.title       = element_text(size = 10, hjust = .5, margin = margin(1,1,1,1), family="Lora"),
          axis.text.y      = element_text(size = 12, family="Lora", angle=0, hjust=.5),
          axis.text.x      = element_text(size = 12, family="Lora", angle=90, hjust=.5),
          strip.background = element_rect(fill="white", colour = NA),
          strip.text.x     = element_text(size=10, family = "Lora", face = "bold", color = "#16171d"),
          strip.text.y     = element_text(size=10, family = "Lora", face = "bold", color = "#16171d"))
      

## Estimaciones caida entre grupos ----

## Cargar bases de datos 


base_completa <- read.csv("02_proc-data/sdem_coe2.csv")
base_completa <-  base_completa %>% mutate(pan = as.Date("2020-03-01")) # Inicio de la pandemia 
base_completa<- base_completa %>% mutate(time = as.integer(difftime(fecha, pan, units = "days")))#Dias hasta la pandema 

################## Margen Intensivo ##########################

#### MODELO 1 ----
####### Modelo conservando solamente a las madres y mujeres no madres (Sector formal e Informal)
        
        mujeres <- base_completa %>% filter(sex != 1, eda >= 15, eda <= 65)
        mujeres <- mujeres %>%
          group_by(id_ind_complete) %>%
          mutate(treat = ifelse(n_hij == 0, 0, 1))
        
        muj_est <- feols(hrsocup ~ i(time, treat, ref = -90) +    ## Interacción clave: time * treatment status
                           anios_esc + eda + ing_x_hrs |
                           id_ind_complete + trimestre.x ,  ## Efectos fijos de tiempo
                         data = mujeres)
        
        summary(muj_est)
        etable(muj_est, tex= F)
        
        iplot(muj_est, xlab = 'Dias hasta la pandemia', main = 'Mujeres: Diferencia entre madres y no madres')
        

#### MODELO 2----
# Haciendo el analisis de las mujeres que son madres y las que no separado por sector economico

# AGROPECUARIO 
        
        mujeres_agro <- mujeres %>%
          filter(rama == 6 )
        
        
        muj_agro <- feols(hrsocup ~ i(time, treat, ref = -90) +    ## Interacción clave: time * treatment status
                            anios_esc + eda + ing_x_hrs|
                            id_ind_complete + trimestre.x ,  ## Efectos fijos de tiempo
                          data = mujeres_agro)
        
        summary(muj_agro)
        etable(muj_agro, tex= F)
        
        iplot(muj_agro,
              xlab = 'Dias hasta la pandemia',
              main = 'Todas las mujeres con y sin hijos: agropecuario')


# CONSTRUCCION  
        
        mujeres_con <- mujeres %>%
          filter(rama == 1 )
        
        
        muj_con <- feols(hrsocup ~ i(time, treat, ref = -90) +    ## Interacción clave: time * treatment status
                           anios_esc + eda + ing_x_hrs |
                           id_ind_complete + trimestre.x ,  ## Efectos fijos de tiempo
                         data = mujeres_con)
        
        summary(muj_con)
        etable(muj_con, tex= F)
        
        iplot(muj_con,
              xlab = 'Dias hasta la pandemia',
              main = 'Todas las mujeres con y sin hijos: Construcción')

# INDUSTRIA MANUFACTURERA 
                
        mujeres_ind <- mujeres %>%
          filter(rama == 2 )
        
        
        muj_ind <- feols(hrsocup ~ i(time, treat, ref = -90) +    ## Interacción clave: time * treatment status
                           anios_esc + eda + ing_x_hrs|
                           id_ind_complete + trimestre.x ,  ## Efectos fijos de tiempo
                         data = mujeres_ind)
        
        summary(muj_ind)
        etable(muj_ind, tex= F)
        
        iplot(muj_ind,
              xlab = 'Dias hasta la pandemia',
              main = 'Todas las mujeres con y sin hijos: Industria Manufacturera')
        
        

# COMERCIO  
        
        mujeres_com <- mujeres %>%
          filter(rama == 3 )
        
        
        muj_com <- feols(hrsocup ~ i(time, treat, ref = -90) +    ## Interacción clave: time * treatment status
                           anios_esc + eda + ing_x_hrs |
                           id_ind_complete + trimestre.x ,  ## Efectos fijos de tiempo
                         data = mujeres_com)
        
        summary(muj_com)
        etable(muj_com, tex= F)
        
        iplot(muj_com,
              xlab = 'Dias hasta la pandemia',
              main = 'Todas las mujeres con y sin hijos: Comercio')

# SERVICIOS 
        
        mujeres_ser <- mujeres %>%
          filter(rama == 4 )
        
        
        muj_ser <- feols(hrsocup ~ i(time, treat, ref = -90) +    ## Interacción clave: time * treatment status
                           anios_esc + eda + ing_x_hrs |
                           id_ind_complete + trimestre.x ,  ## Efectos fijos de tiempo
                         data = mujeres_ser)
        
        summary(muj_ser)
        etable(muj_ser, tex= F)
        
        iplot(muj_ser,
              xlab = 'Dias hasta la pandemia',
              main = 'Todas las mujeres con y sin hijos: Servicios')
        

#Todos los plt juntos 

        iplot(list(muj_agro,muj_com,muj_con,muj_ind,muj_ser),  sep = 0.5, ref.line = -90,
              xlab = 'Dias hasta la pandemia',
              main = 'Trabajo madres y no madres: Diferentes sectores') +
          legend("bottomright", col = c(1,2,3,4,5), pch = c(15, 15), 
                 legend = c("Agropecuario", "Comercio","Construccion","Industrial","Servicios"))

