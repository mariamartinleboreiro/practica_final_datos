library(readxl)
library(dplyr)
library(stringr)
library(readxl)
library(sf)
library(tibble)

# DATASET 1. (DATOS_LIMPIOS)

mayores <- read_excel("Desktop/Datos espaciales y espaciotemporales/Practica_final/33944.xlsx")

# Crear vector con índices de filas que contienen nombres de municipio
municipio_filas <- which(str_detect(mayores[[1]], "^\\d{5} "))

# Extraer las filas de datos que están justo debajo de cada municipio
datos_limpios <- mayores[municipio_filas + 1, ]
nombres_municipios <- mayores[municipio_filas, 1] |> pull()

# Limpiar nombre de municipio (quitar código INE)
nombres_municipios <- str_remove(nombres_municipios, "^\\d{5} ")

# Añadir columna 'municipio'
datos_limpios$municipio <- nombres_municipios

#reordenar columnas
datos_limpios <- datos_limpios %>%
  relocate(municipio) %>%
  mutate(across(-municipio, as.numeric))  # asegurar que las edades son numéricas

View(datos_limpios)

#eliminar primera columna
datos_limpios <- datos_limpios %>%
  select(-`...1`)



# DATASET 2. RENTA MEDIA POR MUNICIPIO (RENTA_LIMPIA)

renta <- read_excel("Desktop/Datos espaciales y espaciotemporales/Practica_final/31250.xlsx")
View(renta)

# 2. Limpiar nombres de municipios
renta_limpia <- renta %>%
  rename(nombre_municipio = 1, renta_per_capita = 2) %>%
  mutate(nombre_municipio = str_remove(nombre_municipio, "^\\d{5} "))

# 3. Verificar
View(renta_limpia)


# DATASET 3. ZONAS DE SALUD (ZONAS_VALENCIA)

zonas_salud <- st_read("/Users/mmart/Desktop/Datos espaciales y espaciotemporales/Practica_final/15_SistemaValencianoSalud.gpkg")

departamentos_valencia <- c(
  "VALENCIA ARNAU LLIRIA",
  "XATIVA - ONTINYENT",
  "VALENCIA - DR. PESET",
  "GANDIA",
  "SAGUNTO",
  "LA RIBERA",
  "REQUENA",
  "VALENCIA - CLINICO",
  "DENIA",
  "VALENCIA HOSPITAL GENERAL",
  "MANISES"
)

zonas_valencia <- zonas_salud %>%
  filter(nom_departamento %in% departamentos_valencia)



# DATASET 4. MUNICIPIOS GEOMETRIAS (MUNICIPIOS_VALENCIA_CLEAN)

library(sf)
library(osmdata)
library(dplyr)

# Obtener bounding box para la provincia de València (más amplia que la ciudad)
bbox_valencia_prov <- getbb("Valencia Province, Spain")

# Nueva consulta para municipios dentro de esa bounding box
valencia_prov_query <- opq(bbox = bbox_valencia_prov) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "8")  # Nivel municipal

# Descargar y convertir a sf
valencia_prov_municipios <- osmdata_sf(valencia_prov_query)$osm_multipolygons

# Limpiar
municipios_validos <- valencia_prov_municipios %>%
  filter(!is.na(name)) %>%
  select(name, geometry)

# Suponiendo que 'datos_limpios$municipio' contiene los nombres válidos
municipios_valencia <- municipios_validos %>%
  filter(name %in% datos_limpios$municipio)


# Eliminar posibles rownames previos
municipios_valencia_clean <- municipios_valencia %>%
  select(name, geometry) 
  





# 1: Preparación de los datos y estructura espacial

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(ggplot2)
library(leaflet)
library(sf)
library(spdep)
library(osmdata)
library(classInt)
library(tibble)
library(janitor)
library(knitr)
#install.packages("kableExtra")
library(kableExtra)



# Verificar CRS actuales
st_crs(municipios_valencia_clean)
st_crs(zonas_valencia)

# Unificar CRS (opcionalmente a EPSG:25830, UTM zona 30N)
municipios_valencia_clean <- st_transform(municipios_valencia_clean, crs = 25830)
zonas_valencia <- st_transform(zonas_valencia, crs = 25830)

# Crear columna con total de personas mayores
datos_limpios <- datos_limpios %>%
  mutate(mayores_65 = rowSums(across(`De 65 a 69 años`:`100 y más años`), na.rm = TRUE))

# Unir datos de envejecimiento
municipios_valencia_clean <- municipios_valencia_clean %>%
  left_join(datos_limpios, by = c("name" = "municipio"))

# Unir datos de renta
municipios_valencia_clean <- municipios_valencia_clean %>%
  left_join(renta_limpia, by = c("name" = "nombre_municipio"))





# 2: Análisis descriptivo y exploratorio de variables

# Convertir renta_per_capita a numérico (si tiene puntos o símbolos)
municipios_valencia_clean <- municipios_valencia_clean %>%
  mutate(renta_per_capita = str_replace_all(renta_per_capita, "\\.", ""),
         renta_per_capita = as.numeric(renta_per_capita))

# Estadísticas descriptivas básicas
summary(municipios_valencia_clean$mayores_65)
summary(municipios_valencia_clean$renta_per_capita)

municipios_valencia_clean <- municipios_valencia_clean %>%
  mutate(pct_mayores_65 = (mayores_65 / sum(mayores_65, na.rm = TRUE)) * 100)

# Crear columna logaritmo base 10 para mayores de 65
municipios_valencia_clean <- municipios_valencia_clean %>%
  mutate(log_mayores_65 = log10(mayores_65 + 1))




# Histograma de densidad - proporción de mayores
library(ggrepel)
library(scales)

ggplot(municipios_valencia_clean, aes(x = mayores_65)) +
  geom_histogram(fill = "steelblue", bins = 30, alpha = 0.7) +
  geom_text_repel(data = top_muni,
                  aes(x = mayores_65, y = 2, label = name),
                  nudge_y = 1,
                  direction = "x",
                  size = 3) +
  scale_x_log10(labels = label_comma()) +
  labs(title = "Distribución (log) de personas mayores de 65 años",
       x = "Nº personas mayores (escala log)", y = "Frecuencia") +
  theme_minimal()


# Histograma de densidad - renta per cápita

top_renta <- municipios_valencia_clean %>%
  slice_max(order_by = renta_per_capita, n = 3)

ggplot(municipios_valencia_clean, aes(x = renta_per_capita)) +
  geom_histogram(fill = "darkgreen", bins = 30, alpha = 0.7) +
  geom_density(aes(y = ..density..), color = "black", size = 1) +
  geom_text_repel(data = top_renta,
                  aes(x = renta_per_capita, y = 0, label = name),
                  nudge_y = 0.002,
                  direction = "y",
                  size = 3) +
  scale_x_continuous(labels = label_number(suffix = " €", big.mark = ".")) +
  labs(title = "Distribución de renta neta per cápita",
       x = "Renta per cápita (€)", y = "Frecuencia / Densidad") +
  theme_minimal()


# Boxplots para detectar outliers

ggplot(municipios_valencia_clean, aes(x = log_mayores_65)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot: personas mayores de 65 años (log10)",
       x = "log10(Nº personas mayores)") +
  theme_minimal()

boxplot(municipios_valencia_clean$renta_per_capita,
        main = "Boxplot: renta per cápita",
        col = "lightgreen", horizontal = TRUE)



# 3. Mapas coropléticos comparativos de envejecimiento y renta
library(ggplot2)
library(sf)
library(viridis)

municipios_valencia_clean <- municipios_valencia_clean %>%
  mutate(log_mayores_65 = log10(mayores_65 + 1))

top_mayores <- municipios_valencia_clean %>%
  slice_max(order_by = mayores_65, n = 3)

ggplot(municipios_valencia_clean) +
  geom_sf(aes(fill = log_mayores_65), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "inferno", name = "log10 mayores 65") +
  geom_text_repel(data = top_mayores,
                  aes(geometry = geometry, label = name),
                  stat = "sf_coordinates",
                  min.segment.length = 0,
                  size = 3, color = "black") +
  labs(title = "Distribución (log) de personas mayores de 65 años") +
  theme_minimal()


top_renta <- municipios_valencia_clean %>%
  slice_max(order_by = renta_per_capita, n = 3)

ggplot(municipios_valencia_clean) +
  geom_sf(aes(fill = renta_per_capita), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "€ per cápita") +
  geom_text_repel(data = top_renta,
                  aes(geometry = geometry, label = name),
                  stat = "sf_coordinates",
                  min.segment.length = 0,
                  size = 3,
                  color = "black") +
  labs(title = "Renta neta per cápita por municipio") +
  theme_minimal()



# 4: Autocorrelación espacial global (Índice de Moran)
library(sf)
library(spdep)
library(tidyverse)

# Crear un dataset sin NAs en renta_per_capita y mayores_65
datos_moran <- municipios_valencia_clean %>%
  filter(!is.na(renta_per_capita), !is.na(mayores_65))

# Crear vecinos espaciales
vecinos <- poly2nb(datos_moran)

# Crear matriz de pesos
pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

# Calcular Moran para mayores_65
moran_mayores <- moran.test(datos_moran$mayores_65, pesos, zero.policy = TRUE)

# Calcular Moran para renta_per_capita
moran_renta <- moran.test(datos_moran$renta_per_capita, pesos, zero.policy = TRUE)

# Resultados
moran_mayores
moran_renta

# crear tabla de resultados
library(gt)
library(dplyr)

tabla_moran <- tibble(
  Variable = c("Proporción mayores de 65", "Renta per cápita"),
  `Moran's I` = c(0.48, 0.34),
  `p-valor` = c(3.661e-06, 6.178e-15),
  Interpretación = c(
    "Autocorrelación espacial positiva alta y significativa",
    "Autocorrelación espacial moderada y significativa"
  )
)

# Crear tabla visual con gt
tabla_moran %>%
  gt() %>%
  tab_header(
    title = md("**Resultados del Índice de Moran**"),
    subtitle = md("Autocorrelación espacial global por variable (Provincia de València)")
  ) %>%
  fmt_number(columns = c(`Moran's I`, `p-valor`), decimals = 3) %>%
  data_color(
    columns = `Moran's I`,
    colors = scales::col_numeric(
      palette = c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000"),
      domain = c(0.2, 0.5)
    )
  ) %>%
  data_color(
    columns = `p-valor`,
    colors = scales::col_bin(
      palette = c("#e5f5e0", "#a1d99b", "#31a354"),
      bins = c(0, 0.001, 0.01, 0.05)
    )
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.font.size = "small",
    data_row.padding = px(6),
    heading.align = "center"
  )



# 5: Análisis espacial local (LISA: clusters HH, LL, HL, LH)
library(sf)
library(spdep)
library(dplyr)
library(ggplot2)
library(viridis)

# Usamos datos sin NA
datos_lisa <- municipios_valencia_clean %>%
  filter(!is.na(renta_per_capita), !is.na(mayores_65))

# Crear vecinos y matriz de pesos
vecinos_lisa <- poly2nb(datos_lisa)
pesos_lisa <- nb2listw(vecinos_lisa, style = "W", zero.policy = TRUE)

# Calcular local Moran para mayores_65
local_moran_mayores <- localmoran(datos_lisa$mayores_65, pesos_lisa, zero.policy = TRUE)

# Calcular local Moran para renta_per_capita
local_moran_renta <- localmoran(datos_lisa$renta_per_capita, pesos_lisa, zero.policy = TRUE)

# Añadir resultados al dataframe
datos_lisa <- datos_lisa %>%
  mutate(
    moran_mayores_I = local_moran_mayores[, 1],
    p_mayores = local_moran_mayores[, 5],
    lag_mayores = lag.listw(pesos_lisa, datos_lisa$mayores_65),
    
    moran_renta_I = local_moran_renta[, 1],
    p_renta = local_moran_renta[, 5],
    lag_renta = lag.listw(pesos_lisa, datos_lisa$renta_per_capita)
  )

# Clasificación de clústers significativos
datos_lisa <- datos_lisa %>%
  mutate(
    cluster_mayores = case_when(
      p_mayores > 0.05 ~ "No significativo",
      mayores_65 >= mean(mayores_65) & lag_mayores >= mean(lag_mayores) ~ "Alto-Alto",
      mayores_65 < mean(mayores_65) & lag_mayores < mean(lag_mayores) ~ "Bajo-Bajo",
      mayores_65 >= mean(mayores_65) & lag_mayores < mean(lag_mayores) ~ "Alto-Bajo",
      mayores_65 < mean(mayores_65) & lag_mayores >= mean(lag_mayores) ~ "Bajo-Alto"
    ),
    cluster_renta = case_when(
      p_renta > 0.05 ~ "No significativo",
      renta_per_capita >= mean(renta_per_capita) & lag_renta >= mean(lag_renta) ~ "Alto-Alto",
      renta_per_capita < mean(renta_per_capita) & lag_renta < mean(lag_renta) ~ "Bajo-Bajo",
      renta_per_capita >= mean(renta_per_capita) & lag_renta < mean(lag_renta) ~ "Alto-Bajo",
      renta_per_capita < mean(renta_per_capita) & lag_renta >= mean(lag_renta) ~ "Bajo-Alto"
    )
  )

# Factores ordenados para mejor leyenda
datos_lisa$cluster_mayores <- factor(datos_lisa$cluster_mayores,
                                     levels = c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo"))

datos_lisa$cluster_renta <- factor(datos_lisa$cluster_renta,
                                   levels = c("Alto-Alto", "Bajo-Bajo", "Alto-Bajo", "Bajo-Alto", "No significativo"))

#visualizacion
library(ggplot2)
library(ggrepel)
library(dplyr)

# mayores de 65

# Seleccionar un municipio representativo por cada tipo de cluster (excepto NS)
representativos_mayores <- datos_lisa %>%
  filter(cluster_mayores != "No significativo") %>%
  group_by(cluster_mayores) %>%
  slice_max(order_by = mayores_65, n = 1)

ggplot(datos_lisa) +
  geom_sf(aes(fill = cluster_mayores), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c("Alto-Alto" = "#b30000", "Bajo-Bajo" = "#08519c",
               "Alto-Bajo" = "#fc9272", "Bajo-Alto" = "#6baed6",
               "No significativo" = "lightgrey"),
    name = "Clúster Local (mayores 65+)"
  ) +
  geom_text_repel(
    data = representativos_mayores,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    min.segment.length = 0,
    max.overlaps = 10
  ) +
  labs(title = "Mapa LISA: Proporción mayores de 65 años") +
  theme_minimal()


# renta per cápita

representativos_renta <- datos_lisa %>%
  filter(cluster_renta != "No significativo") %>%
  group_by(cluster_renta) %>%
  slice_max(order_by = renta_per_capita, n = 1)

ggplot(datos_lisa) +
  geom_sf(aes(fill = cluster_renta), color = "white", size = 0.2) +
  scale_fill_manual(
    values = c("Alto-Alto" = "#b30000", "Bajo-Bajo" = "#08519c",
               "Alto-Bajo" = "#fc9272", "Bajo-Alto" = "#6baed6",
               "No significativo" = "lightgrey"),
    name = "Clúster Local (renta per cápita)"
  ) +
  geom_text_repel(
    data = representativos_renta,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    min.segment.length = 0,
    max.overlaps = 10
  ) +
  labs(title = "Mapa LISA: Renta neta per cápita") +
  theme_minimal()


# 6:

# Asegurar mismo CRS, transformando si es necesario
if (st_crs(municipios_valencia_clean) != st_crs(zonas_valencia)) {
  zonas_valencia <- st_transform(zonas_valencia, st_crs(municipios_valencia_clean))
}
# Corregir geometrías inválidas si es necesario
municipios_valencia_clean <- st_make_valid(municipios_valencia_clean)
zonas_valencia       <- st_make_valid(zonas_valencia)

library(sf)
library(dplyr)
library(ggplot2)

# Obtener bounding box de los municipios de València
bbox_valencia <- st_bbox(municipios_valencia_clean)

# Filtrar los centros de salud que estén dentro del bbox de la provincia
zonas_filtradas <- zonas_valencia %>%
  st_crop(bbox_valencia) %>%
  st_filter(municipios_valencia_clean, .predicate = st_intersects)

# Obtener centroides
health_centroids <- st_centroid(zonas_filtradas)

# Calcular distancias y centro de salud más cercano
distancias <- st_distance(municipios_valencia_clean, health_centroids)
distancia_min <- apply(distancias, 1, min)
indice_min <- apply(distancias, 1, which.min)

# Coordenadas de municipios y de sus centros de salud más cercanos
coords_muni <- st_coordinates(st_centroid(municipios_valencia_clean))
coords_salud <- st_coordinates(health_centroids[indice_min, ])

# Crear df de líneas de conexión
lines_df <- data.frame(
  x = coords_muni[,1],
  y = coords_muni[,2],
  xend = coords_salud[,1],
  yend = coords_salud[,2]
)

# Preparar los puntos de los centros para el mapa
coords_salud_all <- st_coordinates(health_centroids)
health_centroids_df <- health_centroids %>%
  st_drop_geometry() %>%
  mutate(X = coords_salud_all[,1], Y = coords_salud_all[,2])

# Gráfico final
ggplot() +
  geom_sf(data = municipios_valencia_clean, fill = NA, color = "gray60") +
  geom_point(data = health_centroids_df,
             aes(x = X, y = Y),
             shape = 21, fill = "red", color = "black", size = 2) +
  geom_segment(data = lines_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "steelblue", linewidth = 0.6,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  theme_void() +
  theme(panel.background = element_rect(fill = "gray95"),
        legend.position = "none")


# CORRELACIÓN
# Creamos un objeto filtrado: solo los centros de salud dentro del área municipal
zonas_centros_validos <- zonas_valencia[st_intersects(zonas_valencia, st_union(municipios_valencia_clean), sparse = FALSE), ]

# Convertimos a centroides si son polígonos
zonas_centros_validos <- zonas_centros_validos %>% st_centroid()

# Calcular distancia mínima entre cada municipio y el centro de salud más cercano (en metros)
distancias <- st_distance(municipios_valencia_clean, zonas_centros_validos)
distancia_min <- apply(distancias, 1, min)  # Mínimo por municipio

# Añadir al dataframe (conversión a km)
municipios_valencia_clean <- municipios_valencia_clean %>%
  mutate(dist_centro_salud_km = as.numeric(distancia_min) / 1000)

# Análisis de correlación entre distancia y número de mayores
cor_test <- cor.test(
  municipios_valencia_clean$dist_centro_salud_km,
  municipios_valencia_clean$mayores_65,
  method = "pearson",
  use = "complete.obs"
)

# Mostrar resultados del test de correlación
print(cor_test)

# Gráfico con escala logarítmica + top 3 municipios con más mayores señalados
ggplot(municipios_valencia_clean, aes(x = dist_centro_salud_km, y = mayores_65)) +
  geom_point(color = "darkred", alpha = 0.6) +
  scale_y_log10(labels = scales::label_comma(), breaks = c(10, 100, 1000, 10000, 100000)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_text_repel(
    data = municipios_valencia_clean %>% slice_max(order_by = mayores_65, n = 3),
    aes(label = name),
    stat = "identity", size = 3, max.overlaps = 10
  ) +
  labs(
    title = "Relación entre distancia al centro de salud y personas mayores de 65 años",
    x = "Distancia al centro de salud más cercano (km)",
    y = "Nº personas mayores (escala log)"
  ) +
  theme_minimal()



# 7: Correlación entre envejecimiento y renta

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(ggrepel)

# Calcular la correlación de Pearson entre proporción de mayores y renta per cápita
cor_test_envejecimiento_renta <- cor.test(
  municipios_valencia_clean$mayores_65,
  municipios_valencia_clean$renta_per_capita,
  method = "pearson",
  use = "complete.obs"
)

# Mostrar resultados de la correlación
print(cor_test_envejecimiento_renta)

# Visualizar la relación con gráfico de dispersión
ggplot(municipios_valencia_clean, aes(x = mayores_65, y = renta_per_capita)) +
  geom_point(color = "midnightblue", alpha = 0.6) +
  scale_x_log10(labels = scales::label_comma()) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_text_repel(
    data = municipios_valencia_clean %>%
      slice_max(order_by = mayores_65, n = 3),
    aes(label = name),
    stat = "identity", size = 3, max.overlaps = 10
  ) +
  labs(
    title = "Relación entre número de personas mayores de 65 años y renta per cápita",
    x = "Nº personas mayores (escala logarítmica)",
    y = "Renta per cápita (€)"
  ) +
  theme_minimal()


# 8. Regresión lineal simple (Envejecimiento ~ Renta)

library(dplyr)
library(ggplot2)
library(ggrepel)
library(broom)

# Paso 1: Normalizar la variable de mayores de 65 años
library(dplyr)
municipios_valencia_clean <- municipios_valencia_clean %>%
  mutate(mayores_65_norm = mayores_65 / sum(mayores_65))

# Paso 2: Ajustar el modelo de regresión lineal
modelo1 <- lm(mayores_65_norm ~ renta_per_capita, data = municipios_valencia_clean)
summary(modelo1)

# Paso 3: Visualizar scatter plot con línea de regresión
library(ggplot2)
ggplot(municipios_valencia_clean, aes(x = renta_per_capita, y = mayores_65_norm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Renta per cápita", 
       y = "Proporción de mayores de 65 años",
       title = "Relación entre renta per cápita y proporción de mayores (Valencia)")



#9: Diagnóstico espacial de los residuos
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(spdep)

# 1. Calcular residuos y añadir al dataframe
modelo_data <- municipios_valencia_clean %>%
  filter(!is.na(mayores_65) & !is.na(renta_per_capita)) %>%
  mutate(residuos = residuals(modelo_rls))

# 2. Unir residuos al dataframe original
municipios_valencia_residuos <- municipios_valencia_clean %>%
  left_join(st_drop_geometry(modelo_data)[, c("name", "residuos")], by = "name")

# 3. Clasificar municipios por signo de residuo
municipios_valencia_residuos <- municipios_valencia_residuos %>%
  mutate(grupo_residuo = ifelse(residuos > 0, "Positivo", "Negativo"))

# 4. Etiquetar los municipios con residuos más extremos
municipios_extremos <- municipios_valencia_residuos %>%
  filter(!is.na(residuos)) %>%
  slice_max(abs(residuos), n = 5)


# 1. Identificar los municipios con residuos más extremos
municipios_extremos <- municipios_valencia_residuos %>%
  filter(!is.na(residuos)) %>%
  slice_max(order_by = abs(residuos), n = 5)

# 2. Gráfico mejorado
ggplot(data = municipios_valencia_residuos) +
  geom_sf(aes(fill = residuos), color = "gray90", size = 0.2) +
  scale_fill_gradient2(
    low = "firebrick", mid = "white", high = "blue", midpoint = 0,
    name = "Residuos",
    labels = scales::label_comma(),  # Formato legible: 10.000 → 10,000
    guide = guide_colorbar(
      barwidth = 0.5,
      barheight = 10,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  geom_text_repel(
    data = municipios_extremos,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3, max.overlaps = 10
  ) +
  labs(
    title = "Diagnóstico espacial de los residuos",
    subtitle = "Modelo de regresión lineal simple (mayores_65 ~ renta_per_capita)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray95"),
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Indice de moran de los residuos

library(spdep)

# Filtrar municipios sin NA en residuos
municipios_validos <- municipios_valencia_residuos %>%
  filter(!is.na(residuos))

# Crear matriz de vecinos solo para los municipios válidos
vecinos <- poly2nb(municipios_validos)
pesos <- nb2listw(vecinos, style = "W", zero.policy = TRUE)

# Calcular Moran
moran_test <- moran.test(municipios_validos$residuos, pesos, zero.policy = TRUE)

# Mostrar resultados
print(moran_test)

library(gt)

# Crear tabla base con nombres sin espacios
tabla_moran <- data.frame(
  moran_I = round(moran_test$estimate[[1]], 5),
  expectation = round(moran_test$estimate[[2]], 5),
  variance = signif(moran_test$estimate[[3]], 3),
  p_value = signif(moran_test$p.value, 3),
  conclusion = ifelse(moran_test$p.value > 0.05,
                      "No hay autocorrelación espacial significativa",
                      "Autocorrelación espacial significativa")
)

# Crear tabla visual elegante
tabla_moran %>%
  gt() %>%
  tab_header(
    title = md("**Resumen del test de Moran**"),
    subtitle = "Diagnóstico espacial de los residuos del modelo lineal simple"
  ) %>%
  cols_label(
    moran_I = "Moran I",
    expectation = "Valor esperado",
    variance = "Varianza",
    p_value = "p-value",
    conclusion = "Conclusión"
  ) %>%
  fmt_number(columns = 1:4, decimals = 5) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f4f8"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7f7f7"),
    locations = cells_body(rows = seq(1, nrow(tabla_moran), 2))
  ) %>%
  tab_options(
    table.border.top.color = "#cccccc",
    table.border.bottom.color = "#cccccc",
    heading.align = "center",
    table.font.names = "Helvetica",
    column_labels.font.weight = "bold",
    table.background.color = "white"
  )


#10: Geographically Weighted Regression (GWR)

install.packages("spgwr")
library(spgwr)

# Cargar librerías necesarias
library(spgwr)
library(dplyr)

# Asegurar que no hay NAs
modelo_gwr_data <- modelo_data %>%
  filter(!is.na(mayores_65.y), !is.na(renta_per_capita.y), !is.na(X), !is.na(Y))

# Crear matriz de coordenadas
coords <- cbind(modelo_gwr_data$X, modelo_gwr_data$Y)

# Calcular banda óptima con cross-validation (puede tardar unos segundos)
banda_optima <- gwr.sel(mayores_65.y ~ renta_per_capita.y,
                        data = modelo_gwr_data,
                        coords = coords,
                        adapt = TRUE)

# Ajustar modelo GWR
gwr_model <- gwr(mayores_65.y ~ renta_per_capita.y,
                 data = modelo_gwr_data,
                 coords = coords,
                 adapt = banda_optima,
                 hatmatrix = TRUE,
                 se.fit = TRUE)

# Ver resumen de resultados
summary(gwr_model)

library(sf)
library(ggplot2)
library(dplyr)
library(scales)

# 1. Extraer resultados del modelo y unirlos al sf original
resultados_gwr <- as.data.frame(gwr_model$SDF)
modelo_gwr_data$local_r2 <- resultados_gwr$localR2

# 2. Unir los valores al dataset con geometría
modelo_gwr_sf <- st_as_sf(modelo_gwr_data)

# 3. Seleccionar municipios con R2 local más alto para etiquetar
etiquetas_gwr <- modelo_gwr_sf %>%
  arrange(desc(local_r2)) %>%
  slice(1:5)

# 4. Crear el mapa
ggplot(modelo_gwr_sf) +
  geom_sf(aes(fill = local_r2), color = "white", size = 0.1, alpha = 0.9) +
  scale_fill_viridis_c(
    name = expression(R^2 ~ local),
    option = "magma",  # Paleta más suave
    direction = -1,
    begin = 0.2, end = 0.8  # Evita los extremos oscuros o muy brillantes
  ) +
  geom_text_repel(
    data = etiquetas_gwr,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3.5,
    color = "black",
    max.overlaps = 10
  ) +
  labs(
    title = "Bondad de ajuste local del modelo GWR",
    subtitle = "Variable dependiente: población mayor de 65 años",
    caption = "Los valores altos de R² indican mejor ajuste local"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.background = element_rect(fill = "gray98")
  )

# Extraer coeficientes locales del modelo GWR
modelo_gwr_resultados <- as.data.frame(gwr_model$SDF)

# Histograma del coeficiente local de renta
library(ggplot2)

ggplot(modelo_gwr_resultados, aes(x = renta_per_capita.y)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "royalblue", color = "black", alpha = 0.6) +
  geom_density(color = "blue", size = 1) +
  labs(
    title = "Distribución del coeficiente local de renta per cápita (GWR)",
    x = "Coeficiente estimado",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 13)

install.packages("tinytex")
tinytex::install_tinytex()


saveRDS(municipios_valencia_clean, "municipios_valencia_clean.rds")
