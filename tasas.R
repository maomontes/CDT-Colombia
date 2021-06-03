pacman::p_load(tidyverse, scales, ggforce, ggtext, glue, extrafont, ggrepel, lubridate,
               tidytext, RColorBrewer, forecast, timetk, tidytext, gghighlight, ggthemes,
               gifski, gganimate, hrbrthemes)

#font_import()
loadfonts(device = "win")
theme_set(theme_ipsum_ps())

# df <- read_csv("data/Tasas_de_interes_de_captacion_y_operaciones_del_mercado_monetario.csv") %>%
#   janitor::clean_names() %>%
#   mutate(nombreentidad = str_to_lower(nombreentidad),
#          nombreentidad = str_to_title(nombreentidad),
#          nombre_unidad_de_captura = str_to_lower(nombre_unidad_de_captura),
#          descripcion = str_to_title(descripcion),
#          fechacorte = as.Date(fechacorte, format = "%d/%m/%Y"),
#          Year = year(fechacorte),
#          Month = month(fechacorte, label = TRUE, abbr = TRUE),
#          Month = str_to_title(Month),
#          Year = as.factor(Year),
#          Month = as.factor(Month),
#          Week = week(fechacorte),
#          Week = as.factor(Week),
#          Wday = wday(fechacorte, label = TRUE, abbr = FALSE),
#          Wday = str_to_title(Wday))
# 
# df_dias <- df %>%
#   filter(str_detect(descripcion, "Dias$")) %>%
#   select(-codificacion, -codigoentidad, -tipoentidad, -uca , -subcuenta)
# 
# df_dias_cdt <- df_dias %>%
#   filter(str_detect(nombre_unidad_de_captura, "cdt") |
#          str_detect(descripcion, "^A"),
#          !str_detect(descripcion, "45"),
#          #Year %in% c("2020", "2021"),
#          tasa>0) %>%
#   select(-nombre_unidad_de_captura)
# 
# # df para entre dias
# df_entre_dias <- df %>% 
#   filter(str_detect(descripcion, "^Entre"),
#          str_detect(descripcion, "Dias$")) %>% 
#   select(-codificacion, -codigoentidad, -tipoentidad, -uca , -subcuenta, -nombre_unidad_de_captura)
# 
# glimpse(df)
# 
# saveRDS(df_dias_cdt, file = "data/df_dias_cdt_RDS")
# saveRDS(df_entre_dias, file = "data/df_dias_entre")

df_dias_cdt <- readRDS(file = "data/df_dias_cdt_RDS")
df_entre_dias <- readRDS(file = "data/df_dias_entre")

# Histogram cdt´s per year
df_dias_cdt %>%
  ggplot(aes(tasa, fill = Year)) +
  geom_histogram(bins = 15) +
  facet_wrap(~Year, scales = "free_x") +
  theme(legend.position = "none")


# Lineplot mean interes por año
df_dias_cdt %>% 
  group_by(Year, descripcion) %>% 
  summarise(avg_tasa = mean(tasa),
            .groups = "drop") %>% 
  ggplot(aes(Year, avg_tasa, group = descripcion)) +
  geom_point(aes(color = descripcion), size = 1.5) +
  geom_line(aes(color = descripcion)) +
  scale_color_brewer(type = "qual", palette = 2,
                     breaks=c("A 30 Dias","A 60 Dias","A 90 Dias",
                              "A 120 Dias", "A 180 Dias", "A 180 Dias",
                              "A 360 Dias")) +
  expand_limits(y = 0) +
  theme(legend.position = "top")


# Scatterplot tasa de interes and monto
df_dias_cdt %>% 
  ggplot(aes(tasa, monto)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_y_log10()


# Facet of Avg Median Monto and Avg Median Tasa between Year and descripcion.
# Years 2018 to 2020
df_dias_cdt %>% 
  filter(fechacorte >= "2018-01-01", fechacorte <= "2020-12-31") %>% 
  group_by(fechacorte, descripcion, Year) %>% 
  summarise(avg_median_tasa = median(tasa),
            avg_median_monto = median(monto),
            .groups = "drop") %>% 
  ggplot(aes(avg_median_tasa, avg_median_monto)) +
  geom_point(alpha = 0.3) +
  scale_y_log10() +
  facet_grid(descripcion~Year)

# 45 dias anomaly   
df_dias_cdt %>% 
  filter(str_detect(descripcion, "45")) %>%
  group_by(fechacorte, descripcion, nombreentidad, Year) %>% 
  summarise(avg_median_tasa = median(tasa),
            avg_median_monto = median(monto),
            .groups = "drop") %>% 
  ggplot(aes(avg_median_tasa, avg_median_monto)) +
  geom_jitter(alpha = .3) +
  scale_y_log10() +
  facet_wrap(~Year)
#45 removed 

#lbrary for palettes
library(RColorBrewer)

# plot Volatility (SD) of each term of days 
df_dias_cdt %>% 
  filter(Year %in% c(2018, 2019, 2020, 2021)) %>% 
  group_by(fechacorte, descripcion) %>%
  summarise(avg_med_tasa = mean(tasa),
            sd_tasa = sd(tasa),
            .groups = "drop") %>% 
  mutate(rate_tasa = avg_med_tasa - lag(avg_med_tasa, 1, order_by = descripcion),
         pct_change = (avg_med_tasa/lag(avg_med_tasa, order_by = descripcion) - 1)*100,
         rate_sd = sd_tasa - lag(sd_tasa)) %>% 
  filter(fechacorte != min(fechacorte)) %>% 
  ggplot(aes(fechacorte, rate_sd, color = descripcion)) +
  geom_line() +
  #scale_color_brewer(palette = "Set1") %>%
  scale_color_manual(values = c("dodgerblue4",
                                "darkolivegreen4",
                                "darkorchid3",
                                "goldenrod1",
                                "red",
                                "black")) +
  facet_wrap(~descripcion, scales = "free_y") +
  labs(x = "Año",
       y = "Standard Deviation") +
  theme(legend.position = "none")



# La pandemia origino que los CDTs aumentaran en su volatilidad. Los de 120 y 180 dias
# no presentan una variacion tan visible. Aun asi hay que esperar los cambios originados
# por el Paro Nacional. Tome el rango desde 2018 hasta 20/05/2021. 
#-------------------------------------------------------------------------------


# Bancos------------------------------------------------------------------------
df_dias_cdt %>% 
  filter(Year %in% c("2018", "2019", "2020"),
         fct_lump(nombreentidad, 10) != "Other") %>%
  group_by(nombreentidad, Year) %>% 
  summarise(count = n(),
            .groups = "drop") %>%
  mutate(nombreentidad = reorder_within(nombreentidad, count, Year)) %>%
  ggplot(aes(count, nombreentidad)) +
  geom_col(fill = "lightcyan4", color = "grey90") +
  scale_y_reordered() +
  facet_wrap(~Year, ncol = 1, scales = "free_y")


df_dias_cdt %>% 
  count(Year) %>% 
  ggplot(aes(x = Year, y = n), group = Year) +
  geom_path() +
  geom_point()
#-------------------------------------------------------------------------------



#Plots CDT----------------------
df_dias_cdt %>%
  filter(Year %in% c("2018", "2019", "2020")) %>% 
  group_by(fechacorte, Wday) %>% 
  summarize(median_tasa = median(tasa),
            sd_tasa = sd(tasa),
            .groups = "drop") %>% 
  mutate(tasa_rel = sd_tasa - lag(sd_tasa, 1)) %>% 
  ggplot(aes(fechacorte, tasa_rel, color = Wday)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0, linetype = 5, alpha = 0.3) +
  #gghighlight(Wday %in% c("lunes", "viernes", "miércoles"),
  #            use_direct_label = FALSE) +
  #expand_limits(y=0) +
  facet_wrap(~Wday) +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  labs(x = "Fecha",
       y = "SD de la tasa") 


#-------Heatmap-------
p <- df_dias_cdt %>%
  filter(Year != "2021") %>% 
  group_by(Wday, Month, Year) %>% 
  summarise(median_tasa = median(tasa),
            .groups = "drop") %>% 
  mutate(Year = as.Date(Year, format = "%Y")) %>%
  ggplot(aes(Month, Wday, fill = median_tasa)) +
  geom_tile(color = "white") +
  labs(x = "",
       y = "",
       fill = "Median Tasa de Interes",
       title = "Media de Tasas de intereses. Periodo 2008-2020",
       caption = "Fuente: Tasas de interes de captacion y operaciones del mercado monetario") +
  scale_y_discrete(limits = c("Viernes", "Jueves", "Miércoles", "Martes", "Lunes")) +
  scale_x_discrete(limits = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                              "Jul", "Ago", "Sept", "Nov", "Dic")) +
  scale_fill_gradient2(low = "#2b83ba",
                       mid = "#ffffbf",
                       high = "#d7191c",
                       midpoint = median(df_dias_cdt$tasa),
                       guide = guide_colorbar(title.position = 'top',
                                              barwidth = unit(15, 'lines'),
                                              barheight = unit(2, 'lines'))) +
  coord_cartesian(expand = FALSE) +
  theme_minimal() +
  theme(text = element_text(family = "Roboto", face = "bold", size = 13),
        legend.position = "top",
        plot.title.position = 'panel',
        plot.margin = margin(0, 25, 25, 0))
#facet_grid(.~Year, scales = "free_x")

q <- p + 
  transition_time(Year) +
  labs(subtitle = "Year {format(frame_time, '%Y-%m')}") +
  ease_aes('linear')

animate(q, height = 500, width = 800, fps = 30, duration = 10, end_pause = 60, res = 100, 
        renderer = gifski_renderer())


# CDT dias por monto
df_dias_cdt %>% 
  group_by(Year, descripcion) %>% 
  summarise(avg_monto = mean(monto),
            .groups = "drop") %>% 
  ggplot(aes(Year, avg_monto, group = descripcion)) +
  geom_line(aes(color = descripcion), size = 1.5, alpha = 0.8) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_y_continuous(labels = comma)
  


# ------------------CDT entre dias----------------------------------------------
df_entre_dias %>%
  group_by(descripcion, Year) %>% 
  summarise(avg_tasa = mean(tasa),
            .groups = "drop") %>% 
  ggplot(aes(Year, avg_tasa, group = descripcion, color = descripcion)) +
  geom_line() +
  expand_limits(y = 0) +
  facet_wrap(~descripcion) + 
  theme(legend.position = "top")


df_entre_dias %>% 
  group_by(Year, descripcion) %>% 
  summarise(avg_monto = mean(monto),
            avg_tasa = mean(tasa),
            .groups = "drop") %>% 
  ggplot(aes(Year, avg_monto, group = descripcion)) +
  geom_line(aes(color = descripcion), size = 1.5, alpha = 0.8) +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_y_log10(labels = comma)

df_entre_dias %>% 
  group_by(Year, descripcion) %>%
  summarise(avg_monto = mean(monto),
            avg_tasa = mean(tasa),
            .groups = "drop") %>%
  ggplot(aes(avg_monto, avg_tasa, color = Year)) +
  geom_point(size = 2) +
  scale_y_log10() +
  scale_x_log10(labels = comma) +
  facet_wrap(~descripcion) +
  theme(legend.position = "top")
