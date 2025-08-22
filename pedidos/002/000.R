rm(list = ls())

library(tidyverse)

marco_per <- readRDS("productos/01_tratamiento/marco_per.rds")

lol <- marco_per |> 
  filter(p03 < 5)

apoyo <- lol %>% 
  group_by(id_upm) %>% 
  summarise(viv_0a5 = n_distinct(id_vivienda)) %>% 
  group_by(viv_0a5) %>% 
  summarise(n = n())

marco_upm_06 <- readRDS("D:/MAG/marco_administracion/productos/01_general/marco_upm_06.rds")

lol1 <- lol %>% filter(id_upm %in% substr(marco_upm_06$id_upm[marco_upm_06$endi3_sel == 1], 1, 10))



apoyo1 <- lol1 %>% 
  group_by(id_upm) %>% 
  summarise(viv_0a5 = n_distinct(id_vivienda)) |> 
  left_join(marco_per |>
              group_by(id_upm) |> 
              summarise(Mi = n_distinct(id_vivienda)),
            by = "id_upm")



marco_viviendas <- readRDS("D:/MAG/ENDI2/productos/02_enlistamiento/marco_viviendas.rds")

apoyo <- marco_viviendas |>
  #filter(n0a5 > 0) |>
  group_by(id_upm) |>
  summarise(viv_0a5 = sum(n0a5 > 0),
            viv = n())


plot(apoyo$viv[apoyo$viv < 300], apoyo$viv_0a5[apoyo$viv < 300])
abline(a=8, b = 0)

plot(apoyo1$Mi, apoyo1$viv_0a5)
abline(a=8, b = 0)

g1 <- apoyo1 |>  
  filter(Mi < 300) |> 
  mutate(categoria = ifelse(viv_0a5 < 8, "Inferior a 8 viv", "Mayor o igual a 8 viv")) |> 
  ggplot(aes(x=Mi, y=viv_0a5, color = categoria)) + 
  geom_point(size = 0.5) +
  labs(x = "Número de viviendas ocupadas",
       y = "Número de viviendas niños menores a 5") +
  xlim(0, 300) + 
  ylim(0, 150) + 
  theme(panel.background = element_rect(fill = "#E4E3DB",
                                        colour = "#E4E3DB",
                                        size = 0.5, linetype = "solid"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="white"),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        axis.ticks = element_blank(),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.spacing = unit(0, "cm"),
        legend.title.align = 0.5) 

plot(g1)
sum(apoyo1$viv_0a5 < 8)
a = 250
h = 100

ggsave(file ="viv_viv_0a5_anio3.png",
       plot = g1,
       #device = cairo_ps,
       device = "png",
       path = paste0("pedidos/002"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)



g2 <- apoyo |>  
  filter(viv < 300) |> 
  mutate(categoria = ifelse(viv_0a5 < 8, "Inferior a 8 viv", "Mayor o igual a 8 viv")) |>  
  ggplot(aes(x=viv, y=viv_0a5, color = categoria)) + 
  geom_point(size = 0.5) +
  labs(x = "Número de viviendas ocupadas",
       y = "Número de viviendas niños menores a 5") +
  xlim(0, 300) + 
  ylim(0, 150) + 
  theme(panel.background = element_rect(fill = "#E4E3DB",
                                        colour = "#E4E3DB",
                                        size = 0.5, linetype = "solid"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="white"),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        axis.ticks = element_blank(),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.spacing = unit(0, "cm"),
        legend.title.align = 0.5) 

plot(g2)
sum(apoyo$viv_0a5 < 8)
a = 250
h = 100

ggsave(file ="viv_viv_0a5_anio2.png",
       plot = g2,
       #device = cairo_ps,
       device = "png",
       path = paste0("pedidos/002"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)
