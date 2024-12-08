Remuneraciones por tipo de Empleo
================
William Guiñansaca
2024-07-17

# Distribución de las remuneraciones por empleo en Ecuador.

El proyecto tiene como objetivo comparar los salarios de empleos en
diferentes industrías en el país.

## Preguntar

La primera etapa contempla la definición del objetivo de este análisis.
Realizamos las preguntas de investigación necesarias que respondan al
objetivo.

- ¿Cómo se distribuye las remuneraciones según las diferentes
  industrias?

- ¿Cuáles son los sectores industriales con mayor remuneración?

- ¿Cómo se distribuye las remuneraciones según el género?

## Preparación

Se recolecta los datos necesarios para el análisis. Se plantean varias
formas de recolección de datos, sin embargo, existe una fuente de base
de datos oficial del estado de Ecuador sobre las empresas ecuatorianas.

El Registro Estadístico de Empresas (REEM) se estructura tomando la
información de diferentes fuentes, entre las principales se tiene: el
SRI, IESS, Superintendencia de Compañías, Ministerio de Educación, bases
de datos con verificaciones de call center, e información obtenida de
operaciones estadísticas del INEC como son: Encuesta Estructural
Empresarial, Encuesta para actualización cartográfica del Registro
Estadístico de Empresas (REEM)

### Detalles de la fuente

- **Nombre de la Fuente:** Registro Estadístico de Empresas

- **Proveedor de Datos:** Instituto Nacional de Estadística y Censos
  (INEC) de Ecuador

- **URL de la Fuente:**
  <http://www.ecuadorencifras.gob.ec/directoriodeempresas/>

- **Fecha de Acceso:** 10 de julio de 2024

- **Tipo de Licencia:** Open Data Base License (ODbL-1.0)

### Identificación de la organización de los datos

``` r
#  Carga de librerias necesarias
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.3

``` r
library(skimr)
```

    ## Warning: package 'skimr' was built under R version 4.3.3

``` r
# Importacion de datos
df <- read.csv2("rawdata/EMPRESAS2023_periodo_2023.csv") # tiene la , como delim.
df1 <- read_csv2("rawdata/EMPRESAS2023_periodo_2022.csv")

# Observamosla estructura de los datos
str(df)
str(df1)

# Observaoms el data frame
head(df)
tail(df)
```

El primer conjunto de datos tiene 1246162 obs. de 96 variables y el
segundo cuenta con 1,239,822obs. de 98 variables. En resumen, cada
empresa tiene un identificador único y variables para: ubicación, tipo
de contribuyente, sectores económicos, ventas, plazas de empleo,
empleos, tamaños de empresas, remuneraciones, fechas de actividad como
contribuyentes. El conjunto de datos cuenta con información que sirve
para responder las preguntas de investigación, sin embargo, muchas de
las variables no son relevantes para el objetivo del análisis.
Finalmente, se cuenta con 2 conjuntos de datos que corresponde al año
2023 y 2022.

### Determinar la credibilidad de los datos

Para determinar la credibilidad de los datos usamos la metodología ROCCC
y encontramos que los datos son:

- Reliability (Fiabilidad) - Alta: El Registro Estadístico de Empresas
  (REEM) se estructura tomando la información de diferentes fuentes:
  SRI, IESS, Superintendencia de Compañías, Ministerio de Educación,
  INEC (a través de encuestas).

- Originality (Originalidad) - Media: Datos obtenidos de fuente primaria
  y secundaria.

- Comprehensive (Entendibles) - Alta: Cuenta con metadatos y
  diccionario.

- Current (Actuales) - Alta: Los últimos datos obtenidos son del año
  2023

- Cited (Citados) - Alta: La recolección de datos y las fuentes están
  bien citadas.

## Proceso

### Limpieza de datos

#### Conociendo los datos

- Se inicia con la eliminación de variables que no responden a las
  preguntas de investigación.

``` r
# revision de variables
unique(df$seccion)
unique(df1$seccion)
unique(df$division)
unique(df1$division)
unique(df$clase)
unique(df$plazas_rango_edad_1)
unique(df$empleo_rango_edad_1)
```

``` r
# Eliminación de variables
df_clean1 <- select(df, id_empresa, anio,gsectores, seccion, division, clase, 
                    provincia, canton, empleo, empleo_hombres, empleo_mujeres, 
                    empleo_rango_edad_1, empleo_rango_edad_2, empleo_rango_edad_3, 
                    empleo_rango_edad_4, empleo_rango_edad_5, empleo_iess_trim_1,
                    empleo_iess_trim_2, empleo_iess_trim_3, empleo_iess_trim_4,
                    remuneraciones, remuneraciones_hombres, 
                    remuneraciones_mujeres)
str(df_clean1)

df1_clean <- select(df1, id_empresa, anio,gsectores, seccion, division, clase, 
                    provincia, canton, empleo, empleo_hombres, empleo_mujeres, 
                    empleo_rango_edad_1, empleo_rango_edad_2, empleo_rango_edad_3, 
                    empleo_rango_edad_4, empleo_rango_edad_5, empleo_iess_trim_1,
                    empleo_iess_trim_2, empleo_iess_trim_3, empleo_iess_trim_4,
                    remuneraciones, remuneraciones_hombres, 
                    remuneraciones_mujeres)
str(df1_clean)
```

``` r
# revision variables division, clase y sectores con filtro "servicios"
df_clean2 <- filter(df_clean1, gsectores == "Servicios")
table(df_clean2$gsectores)
unique(df_clean2$division)
unique(df_clean2$clase)
```

``` r
# comparacion nombre de columnas
colnames(df_clean1)
colnames(df1_clean)
```

- Las variables seccion, division y clase son subcategorias de los
  sectores productivos de las empresas. Estas variables se van a incluir
  en el análisis.

``` r
# Vemos un resumen estadistico de cada variable
summary(df_clean1)
summary(df1_clean)

# revision provincia y canton
unique(df_clean1$provincia)
unique(df_clean1$canton)
unique(df1_clean$provincia)
unique(df1_clean$canton)
```

- Los tipos de datos son los correctos para cada variable.

- El resumen de los datos nos muestra que las variables catégoricas:
  sectores, provincia y cantón no contienen valores nulos.

- Las provincias y cantones son los correctos y existe una observación
  más llamada: “zona no delimitada”.

#### Datos duplicados

``` r
# observamos si existen registros duplicados
sum(duplicated(df_clean1$id_empresa))
sum(duplicated(df1_clean$id_empresa))
```

- Para observar datos duplicados nos fijamos en el identificador único
  de la empresa (no hay valores duplicados).

#### Datos en blanco (cadenas vacias con espacios)

``` r
# vemos si hay espacios en blanco en las varibles 
contar_columnas_blancos <- sapply(df_clean1,
                                  function(x) sum(trimws(x) == "" | grepl("^\\s*$", x)))
contar_columnas_blancos

contar_columnas_blancos2 <- sapply(df1_clean,
                                   function(x) sum(trimws(x) == "" | grepl("^\\s*$", x)))
contar_columnas_blancos2
```

- No existen datos en blanco (cadenas vacias con espacio) en el data
  frame.

### Transformación de los datos.

- La variable año tiene es un tipo de dato *num* e *int* en cada uno de
  los conjuntos de datos respectivamente. Se realiza el cambio de tipo
  de dato a entero.

``` r
# Cambio de tipo de datos
df1_clean$anio <- as.integer(df1_clean$anio)
str(df1_clean)
```

- Se realiza la unión de ambos conjuntos de datos para el análisis.
  Cuentan con el mismo número y nombre de columnas.

``` r
# Union de 2 conjuntos de datos
df_clean <- rbind(df_clean1, df1_clean)
str(df_clean)
```

- Después de haber unido los conjuntos de datos se cambia las variables:
  anio, gsectores, sección, división, clase, provincia, cantón por tipos
  de datos categoricos (factores).

``` r
summary(df_clean)
```

    ##    id_empresa             anio       gsectores           seccion         
    ##  Min.   :1.358e+10   Min.   :2022   Length:2485984     Length:2485984    
    ##  1st Qu.:4.274e+10   1st Qu.:2022   Class :character   Class :character  
    ##  Median :4.653e+10   Median :2023   Mode  :character   Mode  :character  
    ##  Mean   :4.187e+10   Mean   :2023                                        
    ##  3rd Qu.:4.899e+10   3rd Qu.:2023                                        
    ##  Max.   :5.045e+10   Max.   :2023                                        
    ##                                                                          
    ##    division            clase            provincia            canton         
    ##  Length:2485984     Length:2485984     Length:2485984     Length:2485984    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##      empleo        empleo_hombres    empleo_mujeres    empleo_rango_edad_1
    ##  Min.   :    0.8   Min.   :    0.0   Min.   :   0.0    Min.   : 0.0       
    ##  1st Qu.:    1.0   1st Qu.:    1.0   1st Qu.:   1.0    1st Qu.: 0.1       
    ##  Median :    1.0   Median :    1.0   Median :   1.0    Median : 0.2       
    ##  Mean   :    5.7   Mean   :    4.9   Mean   :   3.9    Mean   : 0.4       
    ##  3rd Qu.:    2.0   3rd Qu.:    2.0   3rd Qu.:   2.0    3rd Qu.: 0.5       
    ##  Max.   :13218.0   Max.   :11179.0   Max.   :4960.0    Max.   :17.4       
    ##  NA's   :1464955   NA's   :1775702   NA's   :1874814   NA's   :2484486    
    ##  empleo_rango_edad_2 empleo_rango_edad_3 empleo_rango_edad_4
    ##  Min.   :   0.1      Min.   :   0.1      Min.   :   0.1     
    ##  1st Qu.:   0.5      1st Qu.:   0.7      1st Qu.:   1.0     
    ##  Median :   1.0      Median :   1.0      Median :   1.0     
    ##  Mean   :   4.6      Mean   :   4.5      Mean   :   2.5     
    ##  3rd Qu.:   2.0      3rd Qu.:   2.0      3rd Qu.:   1.3     
    ##  Max.   :6800.2      Max.   :6057.9      Max.   :3033.3     
    ##  NA's   :2193183     NA's   :1973446     NA's   :1883428    
    ##  empleo_rango_edad_5 empleo_iess_trim_1 empleo_iess_trim_2 empleo_iess_trim_3
    ##  Min.   :  0.1       Min.   :    0.8    Min.   :    0.8    Min.   :    0.8   
    ##  1st Qu.:  0.5       1st Qu.:    1.0    1st Qu.:    1.0    1st Qu.:    1.0   
    ##  Median :  1.0       Median :    1.0    Median :    1.0    Median :    1.0   
    ##  Mean   :  1.0       Mean   :    6.2    Mean   :    6.2    Mean   :    6.2   
    ##  3rd Qu.:  1.0       3rd Qu.:    2.0    3rd Qu.:    2.0    3rd Qu.:    2.0   
    ##  Max.   :214.8       Max.   :12658.0    Max.   :12762.0    Max.   :13456.3   
    ##  NA's   :2366579     NA's   :1584568    NA's   :1578219    NA's   :1572413   
    ##  empleo_iess_trim_4 remuneraciones      remuneraciones_hombres
    ##  Min.   :    0.8    Min.   :        0   Min.   :        0     
    ##  1st Qu.:    1.0    1st Qu.:     5100   1st Qu.:     4545     
    ##  Median :    1.0    Median :     5400   Median :     5400     
    ##  Mean   :    6.3    Mean   :    53028   Mean   :    44787     
    ##  3rd Qu.:    2.0    3rd Qu.:    11721   3rd Qu.:    10800     
    ##  Max.   :13995.0    Max.   :140368564   Max.   :111516942     
    ##  NA's   :1575174    NA's   :1448278     NA's   :1761525       
    ##  remuneraciones_mujeres
    ##  Min.   :       0      
    ##  1st Qu.:    4675      
    ##  Median :    5400      
    ##  Mean   :   36368      
    ##  3rd Qu.:   10620      
    ##  Max.   :69258549      
    ##  NA's   :1865080

``` r
df_clean <- df_clean %>% 
  mutate(across(c(anio, gsectores, seccion, division, clase, provincia, canton), as.factor))
```

#### Datos faltantes

``` r
library(naniar)
```

    ## Warning: package 'naniar' was built under R version 4.3.3

    ## 
    ## Attaching package: 'naniar'

    ## The following object is masked from 'package:skimr':
    ## 
    ##     n_complete

``` r
# Gráfico de calor para valores faltantes
gg_miss_var(df_clean) +
  labs(title = "Distribución de Valores Faltantes por Variable") +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Porcentaje de valores faltantes por variable
missing_percent <- colSums(is.na(df_clean)) / nrow(df_clean) * 100
missing_percent_df <- data.frame(Variable = names(missing_percent),
                                 Percent = missing_percent) %>%
  arrange(desc(Percent))

print(missing_percent_df)
```

    ##                                      Variable  Percent
    ## empleo_rango_edad_1       empleo_rango_edad_1 99.93974
    ## empleo_rango_edad_5       empleo_rango_edad_5 95.19687
    ## empleo_rango_edad_2       empleo_rango_edad_2 88.22193
    ## empleo_rango_edad_3       empleo_rango_edad_3 79.38289
    ## empleo_rango_edad_4       empleo_rango_edad_4 75.76187
    ## empleo_mujeres                 empleo_mujeres 75.41537
    ## remuneraciones_mujeres remuneraciones_mujeres 75.02381
    ## empleo_hombres                 empleo_hombres 71.42854
    ## remuneraciones_hombres remuneraciones_hombres 70.85826
    ## empleo_iess_trim_1         empleo_iess_trim_1 63.74007
    ## empleo_iess_trim_2         empleo_iess_trim_2 63.48468
    ## empleo_iess_trim_4         empleo_iess_trim_4 63.36219
    ## empleo_iess_trim_3         empleo_iess_trim_3 63.25113
    ## empleo                                 empleo 58.92858
    ## remuneraciones                 remuneraciones 58.25774
    ## id_empresa                         id_empresa  0.00000
    ## anio                                     anio  0.00000
    ## gsectores                           gsectores  0.00000
    ## seccion                               seccion  0.00000
    ## division                             division  0.00000
    ## clase                                   clase  0.00000
    ## provincia                           provincia  0.00000
    ## canton                                 canton  0.00000

``` r
# Resumen de empleo y remuneraciones por sector
sector_summary_wo <- df_clean %>%
  group_by(gsectores) %>%
  summarise(
    empleo_promedio = mean(empleo, na.rm = TRUE),
    remuneraciones_promedio = mean(remuneraciones, na.rm = TRUE)
  ) %>%
  arrange(desc(empleo_promedio))

# Gráfico de barras para el empleo promedio por sector
ggplot(sector_summary_wo, aes(x = reorder(gsectores, empleo_promedio), y = empleo_promedio)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + 
  labs(
    title = "Empleo Promedio por Sector con Valores Nulos",
    x = "Sector",
    y = "Empleo Promedio"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# Gráfico de barras para las remuneraciones promedio por sector
ggplot(sector_summary_wo, aes(x = reorder(gsectores, remuneraciones_promedio), y = remuneraciones_promedio)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() + 
  labs(
    title = "Remuneraciones Promedio por Sector con Valores Nulos",
    x = "Sector",
    y = "Remuneraciones Promedio"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# Crear un indicador de valores faltantes en "empleo" y "remuneraciones"
df_clean <- df_clean %>%
  mutate(missing_empleo = is.na(empleo),
         missing_remuneraciones = is.na(remuneraciones))

# Tabla de proporciones para sectores
missing_by_sector <- df_clean %>%
  group_by(gsectores) %>%
  summarise(missing_empleo_percent = mean(missing_empleo) * 100,
            missing_remuneraciones_percent = mean(missing_remuneraciones) * 100)

print(missing_by_sector)
```

    ## # A tibble: 6 × 3
    ##   gsectores                        missing_empleo_percent missing_remuneracion…¹
    ##   <fct>                                             <dbl>                  <dbl>
    ## 1 Agricultura, ganadería, silvicu…                   51.3                   50.7
    ## 2 Comercio                                           67.5                   67.1
    ## 3 Construcción                                       46.9                   45.5
    ## 4 Explotación de Minas y Canteras                    31.4                   29.6
    ## 5 Industrias Manufactureras                          61.4                   61.0
    ## 6 Servicios                                          53.4                   52.5
    ## # ℹ abbreviated name: ¹​missing_remuneraciones_percent

``` r
# Gráfico de barras por sectores
missing_by_sector %>%
  pivot_longer(cols = c(missing_empleo_percent, missing_remuneraciones_percent),
               names_to = "Variable", values_to = "Percent") %>%
  ggplot(aes(x = gsectores, y = Percent, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Porcentaje de Valores Faltantes por Sector",
       x = "Sectores",
       y = "Porcentaje de Valores Faltantes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](proyecto2_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
ggplot(df_clean, aes(x = empleo)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Distribución del empleo promedio", x = "Empleo promedio", y = "Frecuencia") +
  xlim(0, 100) # Ajusta según el rango de interés
```

    ## Warning: Removed 1471346 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_bar()`).

![](proyecto2_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
df_clean <- df_clean %>%
  mutate(employment_size = case_when(
    is.na(empleo) ~ "Sin Información",
    empleo < 10 ~ "Pequeña",
    empleo >= 10 & empleo < 50 ~ "Mediana",
    empleo >= 50 ~ "Grande"
  ))
```

``` r
# Distribución de empresas por tamaño
df_clean %>%
  count(employment_size) %>%
  ggplot(aes(x = employment_size, y = n, fill = employment_size)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de empresas por tamaño", x = "Tamaño de la empresa", y = "Número de empresas") +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
#Valores faltantes por tamaño

missing_by_size <- df_clean %>%
  group_by(employment_size) %>%
  summarise(
    missing_empleo_percent = mean(is.na(empleo)) * 100,
    missing_remuneraciones_percent = mean(is.na(remuneraciones)) * 100
  )
print(missing_by_size)
```

    ## # A tibble: 4 × 3
    ##   employment_size missing_empleo_percent missing_remuneraciones_percent
    ##   <chr>                            <dbl>                          <dbl>
    ## 1 Grande                               0                            0  
    ## 2 Mediana                              0                            0  
    ## 3 Pequeña                              0                            0  
    ## 4 Sin Información                    100                           98.9

``` r
# Porcentaje de valores no faltantes en "empleo"
non_missing_empleo <- sum(!is.na(df_clean$empleo)) / nrow(df_clean) * 100
print(paste("Porcentaje de observaciones con valores no faltantes en 'empleo':", round(non_missing_empleo, 2), "%"))
```

    ## [1] "Porcentaje de observaciones con valores no faltantes en 'empleo': 41.07 %"

``` r
# Verificar el tamaño del dataframe
dim(df_clean)
```

    ## [1] 2485984      26

- Para analizar los datos faltantes nos vamos a centrar en las variables
  que responden al objetivo de la investigación: remuneraciones y
  empleos.

- Existen más observaciones faltantes en empleos que en remuneraciones.

- La variable **empleo** es de suma importancia porque de esta se
  segmentan el resto de las variables. Al faltar esta variable, el resto
  de las 14 variables cuantitativas faltan también.

- Las variables faltantes representan el 58.93% de observaciones del
  conjunto de datos. Lo cuál indica perdida de información que puede
  repercutir en sesgo.

- Las variables en las que tendra repercusión directa los valores
  faltantes en este análisis son las variables cualitativas. Hay que
  realizar un análisis de datos faltantes y variables cuantitativas.

- La imputación de datos se descarta debido a la dificultad al imponer
  un valor de empleados a cierta empresa. Adicionalmente, se tendría que
  hacer una imputación a las remuneraciones de esos empleados imputados.

- El tipo de dato faltante se define como datos faltantes aleatorios
  debido al no completar la encuesta sobre información de la empresa.
  Por tanto, las valores faltantes serán eliminados.

- Ademas, la variable empleo es el “empleo registrado promedio” que
  implica que este dato puede no ser un entero y podría incluir valores
  decimales.

``` r
# eliminación de valores faltantes
df_wonull <- df_clean %>% 
  filter(!is.na(empleo))
df_wonull

# revisión luego de eliminar valores faltantes
colSums(is.na(df_wonull))
```

``` r
# análisis de valores faltantes en empleo_hombres
df_wonull %>% 
  filter(is.na(empleo_hombres))
str(df_wonull)

# comparación del resumen de empleo luego de eliminación
summary(df_wonull)
```

``` r
df_wonull %>%
  group_by(gsectores) %>%
  summarise(
    empleo_promedio = mean(empleo, na.rm = TRUE),
    remuneraciones_promedio = mean(remuneraciones, na.rm = TRUE)
  ) %>%
  arrange(desc(empleo_promedio))
```

    ## # A tibble: 6 × 3
    ##   gsectores                               empleo_promedio remuneraciones_prome…¹
    ##   <fct>                                             <dbl>                  <dbl>
    ## 1 Explotación de Minas y Canteras                   27.7                 472098.
    ## 2 Agricultura, ganadería, silvicultura y…           12.3                  83646.
    ## 3 Industrias Manufactureras                          8.46                 80336.
    ## 4 Servicios                                          5.77                 58629.
    ## 5 Construcción                                       5.55                 38524.
    ## 6 Comercio                                           3.78                 30858.
    ## # ℹ abbreviated name: ¹​remuneraciones_promedio

``` r
# Resumen de empleo y remuneraciones por sector
sector_summary <- df_wonull %>%
  group_by(gsectores) %>%
  summarise(
    empleo_promedio = mean(empleo, na.rm = TRUE),
    remuneraciones_promedio = mean(remuneraciones, na.rm = TRUE)
  ) %>%
  arrange(desc(empleo_promedio))

# Gráfico de barras para el empleo promedio por sector
ggplot(sector_summary, aes(x = reorder(gsectores, empleo_promedio), y = empleo_promedio)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + 
  labs(
    title = "Empleo Promedio por Sector",
    x = "Sector",
    y = "Empleo Promedio"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
# Gráfico de barras para las remuneraciones promedio por sector
ggplot(sector_summary, aes(x = reorder(gsectores, remuneraciones_promedio), y = remuneraciones_promedio)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() + 
  labs(
    title = "Remuneraciones Promedio por Sector",
    x = "Sector",
    y = "Remuneraciones Promedio"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

- Luego de la eliminación se compara los valores de tendencia central y
  posición antes y después de la eliminación y son los mismos. Como
  ejemplo la media previa a la eliminación es de 5.7 y pos-eliminación
  es de 5.719.

- Luego de la eliminacion de valores faltante, se determina que no se
  afecta la clasificación general de los sectores al relacionarlas con
  las remuneraciones o empleos promedios.

``` r
# variable empleo_hombres
# analisis valores 0 y NA
df_wonull %>% 
  filter(empleo_hombres == 0) # 21,126 

df_wonull %>% 
  filter(is.na(empleo_hombres)) # 310,747

# variable empleo mujeres
df_wonull %>% 
  filter(empleo_mujeres == 0) # 16,873

df_wonull %>% 
  filter(is.na(empleo_mujeres)) #409,859

# varibale rango edad
df_wonull %>% 
  filter(is.na(empleo_rango_edad_1))

# variable remuneraciones
df_wonull %>% 
  filter(is.na(remuneraciones_hombres))

df_wonull %>% 
  filter(remuneraciones_hombres ==0)

# convertir valores 0 a NA
df_wonull <- df_wonull %>%
  mutate(empleo_hombres = na_if(empleo_hombres, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_mujeres = na_if(empleo_mujeres, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_rango_edad_1 = na_if(empleo_rango_edad_1, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_rango_edad_2 = na_if(empleo_rango_edad_2, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_rango_edad_3 = na_if(empleo_rango_edad_3, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_rango_edad_4 = na_if(empleo_rango_edad_4, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_rango_edad_5 = na_if(empleo_rango_edad_5, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_iess_trim_1 = na_if(empleo_iess_trim_1, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_iess_trim_2 = na_if(empleo_iess_trim_2, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_iess_trim_3 = na_if(empleo_iess_trim_3, 0))

df_wonull <- df_wonull %>%
  mutate(empleo_iess_trim_4 = na_if(empleo_iess_trim_4, 0))

df_wonull <- df_wonull %>%
  mutate(remuneraciones_hombres = na_if(remuneraciones_hombres, 0))

df_wonull <- df_wonull %>%
  mutate(remuneraciones_mujeres = na_if(remuneraciones_mujeres, 0))
```

- Los datos con valor cero de las variables *empleo_hombres,
  empleo_mujeres, empleo_rango_edad, empleo_iess, remuneraciones_hombre
  y remuneraciones_mujeres* se van a transformar a NA. Todo esto con el
  objetivo que los valores 0 no influyan en la estadistica descriptiva.

- Cabe aclarar que la transformación de nulos por ceros, causaban que
  los ceros dominen los datos, lo que fue problemático para obtener una
  visión clara de la distribución real de los datos.

- El nuevo tamaño del data frame es de 1021029 obs. de 23 variables.

``` r
summary(df_wonull)
```

## Análisis

### Análisis Univariado

#### Variables Cualitativas

Para el análisis de estas variables realizamos tablas de frecuencia,
graficos de barras y segmentación por año para encontrar patrones.

- En la variable del identificador único de empresas solo se revisa por
  dúplicados.

``` r
# variable año 
unique(df_wonull$anio)
```

- El año del conjunto de datos es el 2022 y 2023

![](proyecto2_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->![](proyecto2_files/figure-gfm/unnamed-chunk-37-2.png)<!-- -->

- Se realiza la comparación entre el set de datos con y sin valores
  nulos, en lo que respecta a los sectores y su frecuencia durante los
  años 2022 y 2023. Como el gráfico muestra, los sectores no tienen
  cambios en su orden de frecuencia de mayor a menor. Sin embargo, el
  sector comercio tiene un descenso en su porcentaje de frecuencia (del
  16% al 14%). En cambio, el sector servicios aumenta su frecuencia (del
  24% al 28%) y los demás sectores se encuentran relativamente estables.

- El sector comercio se ve afectado ya que en el data set con valores
  nulos existen más comercios en el año 2022 que en el 2023 y con la
  eliminación de valores nulos el sector comercio dismuye siendo el año
  2022 inferior que 2023. Por tanto, no se puede aseverar que durante el
  año 2023 hay mas emprendimientos en el sector comercio que en el año
  2022.

- El resto de sectores mantienen las tendencias en sectores y en los
  años 2022 y 2023.

- Los sectores con el mayor número de empresas de mayor a menor son:

  - Servicios

  - Comercio

  - Industrias Manufactureras

  - Agricultura, ganadería, silvicultura y pesca

  - Construcción

  - Explotación de Minas y Canteras

``` r
# variable seccion
unique(df_wonull$seccion)

# variable division
unique(df_wonull$division)

# variable clase 
unique(df_wonull$clase)
```

- Las variables: *division, grupo y clase* forman parte del sistema de
  Clasificación Industrial Internacional Uniforme (CIIU). Estas
  variables son una subdivisión de los tipos de empleos y permiten
  ejecutar un análisis más detallado de los trabajos.

- Las variables *division, grupo y clase* contienen 19, 85 y 413
  categorías respectivamente.

``` r
# variable provincia
sort(table(df_clean$provincia), decreasing = TRUE)
```

    ## 
    ##                      Pichincha                         Guayas 
    ##                         672185                         529372 
    ##                         Manabí                          Azuay 
    ##                         177734                         152869 
    ##                     Tungurahua                         El Oro 
    ##                         110514                         109936 
    ##                           Loja                       Imbabura 
    ##                          77293                          76990 
    ## Santo Domingo de los Tsáchilas                       Los Ríos 
    ##                          72992                          70939 
    ##                     Chimborazo                       Cotopaxi 
    ##                          69550                          59778 
    ##                     Esmeraldas                          Cañar 
    ##                          45912                          32375 
    ##                      Sucumbíos                    Santa Elena 
    ##                          31914                          30968 
    ##                Morona Santiago                       Orellana 
    ##                          25348                          24164 
    ##                        Bolívar                         Carchi 
    ##                          23482                          22735 
    ##               Zamora Chinchipe                           Napo 
    ##                          21148                          18379 
    ##                        Pastaza                      Galápagos 
    ##                          17927                          11474 
    ##             Zona No Delimitada 
    ##                              6

``` r
sort(table(df_wonull$provincia), decreasing = TRUE)
```

    ## 
    ##                      Pichincha                         Guayas 
    ##                         306977                         225495 
    ##                          Azuay                         Manabí 
    ##                          69452                          68381 
    ##                     Tungurahua                         El Oro 
    ##                          44973                          43167 
    ##                       Imbabura                           Loja 
    ##                          28900                          28690 
    ##                     Chimborazo                       Los Ríos 
    ##                          26247                          24160 
    ##                       Cotopaxi Santo Domingo de los Tsáchilas 
    ##                          24035                          22066 
    ##                     Esmeraldas                          Cañar 
    ##                          16315                          15393 
    ##                    Santa Elena                      Sucumbíos 
    ##                          10284                           9517 
    ##                Morona Santiago                         Carchi 
    ##                           9125                           8881 
    ##                        Bolívar               Zamora Chinchipe 
    ##                           8527                           7597 
    ##                       Orellana                           Napo 
    ##                           6809                           5797 
    ##                        Pastaza                      Galápagos 
    ##                           5587                           4648 
    ##             Zona No Delimitada 
    ##                              6

``` r
# Graficos de comparacion por años y por datos con y sin tratamiento de nulos
ggplot(df_clean, aes(provincia, fill = anio)) + 
  geom_bar(aes(y=after_stat(count / sum(count) * 100)), position = "dodge") +
  coord_flip() + 
  labs(x="Provincias", y="Frecuencia (%)", fill="Año", title = "Empresas por provincia (con datos faltantes)") + 
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
ggplot(df_wonull, aes(provincia, fill = anio)) + 
  geom_bar(aes(y=after_stat(count / sum(count) * 100)), position = "dodge") +
  coord_flip() + 
  labs(x="Provincias", y="Frecuencia (%)", fill="Año", title = "Empresas por provincia (sin datos faltantes)") + 
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-39-2.png)<!-- -->

``` r
# Filtrar las 5 primeras provincias
#categorias_filtro <- c("Pichincha", "Guayas", "Manabí", "Azuay", "Tungurahua") 

# Generar data frame con los filtros de las provincias
#df_provincias <- df_wonull %>%
  #filter(provincia %in% categorias_filtro)
```

- Igualmente, al analizar la variable provincias con y sin eliminación
  de valores nulos, se observa que la tercera provincia con mas empresas
  es Manabí antes de la eliminación. Con los datos nulos eliminados, ese
  tercer lugar lo ocupa Azuay.

``` r
df_clean %>% 
  count(provincia == "Manabí") %>% 
  mutate(porcentaje = n / sum(n)*100)  # 7.15%

df_wonull %>% 
  count(provincia == "Manabí") %>% 
  mutate(porcentaje = n / sum(n)*100) # 6.7%

df_clean %>% 
  count(provincia == "Azuay") %>% 
  mutate(porcentaje = n / sum(n)*100) # 6.14%

df_wonull %>% 
  count(provincia == "Azuay") %>% 
  mutate(porcentaje = n / sum(n)*100) # 6.8%
```

- La diferencia de empresas previo a la eliminación de datos nulos entre
  Azuay y Manabí es del 0.45% a favor de Manabí. Luego de la eliminación
  de valores faltantes es del 0.1% a favor de Azuay.

- Observamos que las provincias con mayor número de empresas son:
  **Pichincha, Guayas, Azuay, Manabí y Tungurahua.**

- Las tendencias entre los años 2022 y 2023 no cambían.

![](proyecto2_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->![](proyecto2_files/figure-gfm/unnamed-chunk-41-2.png)<!-- -->

- Los cantones con mayor número de empresas son: **Quito, Guayaquil,
  Cuenca, Ambato y Santo Domingo.**

- La tendencia de los cantones con mayor número de empreas no cambia
  entre los años 2022 y 2023.

##### Variables Cuantitativas

En este tipo de variables encontramos medidas descriptivas (tendencia
central, posición, disperción, forma), histogramas, poligonos de
frecuencia.

``` r
ggplot(df_wonull, aes(x = empleo)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Distribución del empleo promedio", x = "Empleo promedio", y = "Frecuencia") +
  xlim(0, 25) # Ajusta según el rango de interés
```

    ## Warning: Removed 25724 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_bar()`).

![](proyecto2_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

- **Variable empleo:** Calcula el promedio de empleos que existe durante
  todo el año en cada empresa. Al obtener medidas de tendencia central y
  posición encontramos que la dispersión es alta. Sin embargo, por la
  naturaleza de la variable, la cuál puede pertenecer a una empresa con
  muchos empleados es asume como normal la dispersión elevada.

  - Al analizar el valor máximo encontramos que existe una empresa
    guayaquileña (Pesca y acuicultura) que tiene una plantilla de 13218
    empleos.

  - El promedio de empleo promedio en las empresas del país es de 5.719,
    sin embargo esta medida es influenciada fuertemente por outliers asi
    que para mejor medida de referencia tomamos la mediana de1 empleo
    promedio registrado en las empresas del país.

  - Los outliers representan las empresas con mayor número de empleados.

``` r
# variable 
summary(df_wonull$empleo_hombres)


ggplot(df_wonull, aes(x = empleo_hombres)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(
    title = "Distribución del Empleo para Hombres",
    x = "Empleo (Hombres)",
    y = "Frecuencia"
  ) +
  xlim(0, 25) +
  theme_minimal()
```

    ## Warning: Removed 346966 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_bar()`).

![](proyecto2_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

- **Variable empleo_hombres:** Hay una mediana de 1 con un promedio de 5
  lo que indica una distribución asimetrica. Existe mayor concetración
  alrededor del rango intercuartilico.

``` r
# variable 
summary(df_wonull$empleo_mujeres)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.8     1.0     1.0     4.0     2.0  4960.0  426732

``` r
# grafico
ggplot(df_wonull, aes(x = empleo_mujeres)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(
    title = "Distribución del Empleo para Mujeres",
    x = "Empleo (Hombres)",
    y = "Frecuencia"
  ) +
  xlim(0, 25) +
  theme_minimal()
```

    ## Warning: Removed 438196 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_bar()`).

![](proyecto2_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

- **Variable empleo_mujeres:** Hay una mediana de 1 con un promedio de 4

``` r
summary(df_wonull$empleo_rango_edad_1)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.1     0.1     0.2     0.4     0.5    17.4 1019533

``` r
# hist(df_wonull$empleo_rango_edad_1)
# boxplot(df_wonull$empleo_rango_edad_1)

summary(df_wonull$empleo_rango_edad_2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.1     0.5     1.0     4.6     2.0  6800.2  728228

``` r
# hist(df_wonull$empleo_rango_edad_2)
# boxplot(df_wonull$empleo_rango_edad_2)

summary(df_wonull$empleo_rango_edad_3)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.1     0.7     1.0     4.5     2.0  6057.9  508491

``` r
# hist(df_wonull$empleo_rango_edad_3)
# boxplot(df_wonull$empleo_rango_edad_3)

summary(df_wonull$empleo_rango_edad_4)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.1     1.0     1.0     2.5     1.3  3033.3  418473

``` r
# hist(df_wonull$empleo_rango_edad_4)
# boxplot(df_wonull$empleo_rango_edad_4)

summary(df_wonull$empleo_rango_edad_5)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##     0.1     0.5     1.0     1.0     1.0   214.8  901624

``` r
# hist(df_wonull$empleo_rango_edad_5)
# boxplot(df_wonull$empleo_rango_edad_5)
```

- **Variable empleo_rango_edad:** Hay una mediana de 0.2 con un promedio
  de 0.4

En todas las variables notamos que la media se encuentra fuera del rango
intercuartílico, acompañada de una varianza muy alta. En este caso, los
outliers son reales y reflejan la presencia de grandes empresas. Se
aplicará transformaciones a los datos (como la transformación
logarítmica) para reducir la influencia de los outliers sin eliminarlos.

### Análisis Bivariado

#### 1. Relación entre remuneraciones y sector

#### Outliers

- Se detecto valores atípicos en las columnas de análisis: empleo y
  remuneraciones, con 168430 y 152726 valores considerados atípicos
  respectivamente. Sin embargo, la relación entre la remuneración y el
  número de empleados promedio es clave para determinar si un valor alto
  de remuneración realmente es un outlier “real” o simplemente refleja
  una empresa con una gran cantidad de empleados. Por tanto, se crea una
  nueva variable “remuneración promedio por empleado” para determinar
  los outliers. Este nuevo cálculo permitirá identificar empresas que
  pagan valores desproporcionados por empleado, en lugar de basarse
  únicamente en la remuneración total.

``` r
# Calcular IQR y límites para detectar outliers en 'empleo' y 'remuneraciones'
outlier_info <- function(data, column) {
  stats <- summary(data[[column]])
  Q1 <- stats["1st Qu."]
  Q3 <- stats["3rd Qu."]
  IQR <- Q3 - Q1
  lower_limit <- Q1 - 1.5 * IQR
  upper_limit <- Q3 + 1.5 * IQR
  
  outliers <- data %>%
    filter(data[[column]] < lower_limit | data[[column]] > upper_limit)
  
  list(
    lower_limit = lower_limit,
    upper_limit = upper_limit,
    outliers_count = nrow(outliers),
    outliers = outliers
  )
}

# Identificar outliers en empleo
empleo_outliers <- outlier_info(df_wonull, "empleo")
cat("Outliers en 'empleo':\n")
```

    ## Outliers en 'empleo':

``` r
#print(empleo_outliers)

# Identificar outliers en remuneraciones
remuneraciones_outliers <- outlier_info(df_wonull, "remuneraciones")
cat("\nOutliers en 'remuneraciones':\n")
```

    ## 
    ## Outliers en 'remuneraciones':

``` r
#print(remuneraciones_outliers)

# Visualizar con boxplots
df_long <- df_wonull %>%
  select(empleo, remuneraciones) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

ggplot(df_long, aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  labs(
    title = "Boxplots para Empleo y Remuneraciones",
    x = "Variable",
    y = "Valores"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

``` r
# Calcular límites de outliers para la columna remuneraciones
remuneraciones_stats <- summary(df_wonull$remuneraciones)
Q1 <- remuneraciones_stats["1st Qu."]
Q3 <- remuneraciones_stats["3rd Qu."]
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Filtrar los valores outliers
outliers_remuneraciones <- df_wonull %>%
  filter(remuneraciones < lower_limit | remuneraciones > upper_limit)

# Contar y mostrar los valores outliers
outliers_count <- nrow(outliers_remuneraciones)
cat("Número de outliers en 'remuneraciones':", outliers_count, "\n")

cat("Valores de outliers en 'remuneraciones':\n")
print(outliers_remuneraciones$remuneraciones)
```

``` r
# Calcular límites de outliers para la columna empleo
empleo_stats <- summary(df_wonull$empleo)
Q1 <- empleo_stats["1st Qu."]
Q3 <- empleo_stats["3rd Qu."]
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Filtrar los valores outliers
outliers_empleo <- df_wonull %>%
  filter(empleo < lower_limit | empleo > upper_limit)

# Contar y mostrar los valores outliers
outliers_count <- nrow(outliers_empleo)
cat("Número de outliers en 'empleo':", outliers_count, "\n")

cat("Valores de outliers en 'empleo':\n")
print(outliers_empleo$empleo)
```

``` r
# Crear una nueva variable: remuneración promedio por empleado
df_wonull <- df_wonull %>%
  mutate(remuneracion_por_empleado = remuneraciones / empleo)

# Calcular los límites de outliers para la nueva variable
remuneracion_stats <- summary(df_wonull$remuneracion_por_empleado, na.rm = TRUE)
Q1 <- remuneracion_stats["1st Qu."]
Q3 <- remuneracion_stats["3rd Qu."]
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Filtrar los nuevos outliers
outliers_nuevos <- df_wonull %>%
  filter(remuneracion_por_empleado > upper_limit)

# Visualizar la relación entre empleo y remuneraciones
ggplot(df_wonull, aes(x = empleo, y = remuneraciones)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relación entre empleo y remuneraciones",
    x = "Empleo promedio",
    y = "Remuneraciones"
  ) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](proyecto2_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

``` r
# Analizar los nuevos límites
cat("Nuevo límite superior para outliers:", upper_limit, "\n")
```

    ## Nuevo límite superior para outliers: 8034.6

``` r
cat("Número de nuevos outliers en remuneración por empleado:", nrow(outliers_nuevos), "\n")
```

    ## Número de nuevos outliers en remuneración por empleado: 109531

- Hay una relación positiva y lineal entre el empleo promedio y las
  remuneraciones totales. Esto significa que a medida que aumenta el
  empleo promedio, también aumentan las remuneraciones totales. La línea
  roja representa una tendencia central que muestra esta relación
  directa.

``` r
# Calcular límites de outliers para la columna remuneracion_por_empleado
remuneracion_por_empleado_stats <- summary(df_wonull$remuneracion_por_empleado)
Q1 <- remuneracion_por_empleado_stats["1st Qu."]
Q3 <- remuneracion_por_empleado_stats["3rd Qu."]
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Filtrar los valores outliers
outliers_remuneracion_por_empleado <- df_wonull %>%
  filter(remuneracion_por_empleado < lower_limit | remuneracion_por_empleado > upper_limit)

# Contar y mostrar los valores outliers
outliers_count <- nrow(outliers_remuneracion_por_empleado)
cat("Número de outliers en 'remuneracion_por_empleado':", outliers_count, "\n")

cat("Valores de outliers en 'remuneracion_por_empleado':\n")
print(outliers_remuneracion_por_empleado$remuneracion_por_empleado)
```

``` r
summary(df_wonull$remuneracion_por_empleado)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    4050    5220    5396    5644  694448

``` r
# Contar outliers por sector
outliers_nuevos %>%
  group_by(gsectores) %>%
  summarise(n_outliers = n()) %>%
  arrange(desc(n_outliers)) %>%
  print()
```

    ## # A tibble: 6 × 2
    ##   gsectores                                    n_outliers
    ##   <fct>                                             <int>
    ## 1 Servicios                                         73506
    ## 2 Comercio                                          22317
    ## 3 Industrias Manufactureras                          6551
    ## 4 Construcción                                       3612
    ## 5 Agricultura, ganadería, silvicultura y pesca       2922
    ## 6 Explotación de Minas y Canteras                     623

``` r
ggplot(df_wonull, aes(x = gsectores, y = remuneracion_por_empleado)) +
  geom_boxplot(outlier.color = "red", outlier.size = 1.5) +
  coord_flip() +
  labs(
    title = "Distribución de remuneración promedio por empleado por sector",
    x = "Sectores",
    y = "Remuneración promedio por empleado"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

\*Desigualdad en las Remuneraciones: La gráfica muestra una clara
desigualdad en las remuneraciones promedio entre los diferentes
sectores. Algunos sectores, como los servicios, tienden a tener
remuneraciones promedio más altas en comparación con otros como la
agricultura.

- Concentración de Remuneraciones: En algunos sectores, las
  remuneraciones promedio se concentran en un rango más estrecho,
  mientras que en otros hay una mayor dispersión. Esto sugiere que puede
  haber una mayor variabilidad en los salarios dentro de ciertos
  sectores.

- Sectores con Mayores Remuneraciones: Los sectores de servicios y, en
  menor medida, industrias manufactureras, parecen tener las
  remuneraciones promedio más altas.

- Sectores con Menores Remuneraciones: Los sectores de agricultura,
  ganadería, silvicultura y pesca, así como la construcción, presentan
  las remuneraciones promedio más bajas.

``` r
# Obtener los 10 mayores outliers
top_outliers <- df_wonull %>%
  arrange(desc(remuneracion_por_empleado)) %>%
  slice_head(n = 10) %>%
  select(clase, canton, empleo, remuneracion_por_empleado)

# Mostrar los resultados
print(top_outliers)
```

    ##                                                                                                                    clase
    ## 1                                                         Otras actividades profesionales, científicas y técnicas n.c.p.
    ## 2                                                                                   Actividades de oficinas principales.
    ## 3                                                         Otras actividades profesionales, científicas y técnicas n.c.p.
    ## 4                                                                       Otras actividades de servicios personales n.c.p.
    ## 5                                                      Alquiler de otros tipos de maquinaria, equipo y bienes tangibles.
    ## 6  Venta al por menor de productos farmacéuticos y medicinales, cosméticos y artículos de tocador en comercios especiali
    ## 7  Venta al por menor de productos farmacéuticos y medicinales, cosméticos y artículos de tocador en comercios especiali
    ## 8                                                  Actividades inmobiliarias realizadas con bienes propios o arrendados.
    ## 9                                                      Alquiler de otros tipos de maquinaria, equipo y bienes tangibles.
    ## 10                                      Actividades inmobiliarias realizadas a cambio de una retribución o por contrato.
    ##                             canton empleo remuneracion_por_empleado
    ## 1                        Guayaquil      1                  694448.1
    ## 2                           Cuenca      1                  688223.4
    ## 3                        Guayaquil      1                  528771.4
    ## 4                           Cuenca      1                  518047.4
    ## 5                        Naranjito      1                  510000.0
    ## 6                        Guayaquil      1                  482076.0
    ## 7                        Guayaquil      1                  482076.0
    ## 8  Distrito Metropolitano de Quito      1                  462973.5
    ## 9                        Naranjito      1                  425000.0
    ## 10 Distrito Metropolitano de Quito      1                  385967.1

``` r
# Crear el gráfico de burbujas
ggplot(top_outliers, aes(x = empleo, y = remuneracion_por_empleado, size = empleo, color = clase, label = canton)) +
  geom_point(alpha = 0.7) +  # Burbujas transparentes
  geom_text(vjust = 1.5, size = 3) +  # Etiquetas con el nombre del cantón
  scale_size_continuous(range = c(5, 15)) +  # Tamaño de burbujas ajustado
  labs(
    title = "Outliers: Relación entre Empleo y Remuneraciones por Empleado",
    x = "Empleo Promedio",
    y = "Remuneraciones por Empleado",
    size = "Empleo",
    color = "Clase"
  ) +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))
```

![](proyecto2_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

- Las actividades profesionales, científicas y técnicas junto con las
  actividades inmobiliarias presentan las remuneraciones por empleado
  más altas entre los outliers. Estas actividades suelen requerir un
  alto nivel de especialización o están asociadas a sectores con altos
  ingresos.

- Las ciudades principales como Guayaquil, Cuenca y el Distrito
  Metropolitano de Quito concentran la mayoría de estos casos. Esto
  sugiere que los outliers en remuneraciones están vinculados a zonas
  urbanas con una alta actividad económica.

- Es notable que todas las empresas en esta lista tienen un empleo
  promedio de 1 persona. Esto indica que estos outliers están ligados a
  empresas pequeñas o unidades económicas individuales que generan
  ingresos muy elevados por empleado. O hay falta de ingreso de
  informacion por parte de estas empresas.

- Se observa que algunas actividades, como la venta al por menor de
  productos farmacéuticos y el alquiler de maquinaria, aparecen más de
  una vez. Esto evidencia que ciertos sectores económicos tienen una
  tendencia consistente hacia remuneraciones elevadas por empleado.

Dado que los datos disponibles no incluyen detalles sobre los nombres de
las empresas ni información adicional sobre sus actividades específicas,
sería ideal realizar un análisis más profundo para entender el contexto
detrás de estos valores atípicos (outliers). Sin información adicional
sobre las empresas, como su tamaño, sector exacto o estructura, no es
posible determinar si estos valores representan verdaderos ingresos o si
son distorsiones debido a factores específicos.

El análisis adicional podría incluir la obtención de más datos, como los
nombres de las empresas, tipos de contratos, fuentes de ingresos, o
incluso el tamaño real de las empresas (más allá del número de
empleados), lo que permitiría esclarecer las razones detrás de estas
remuneraciones elevadas y validar si son una excepción o un patrón en el
sector.

``` r
# Calcular los cuartiles y el rango intercuartílico
Q1 <- quantile(df_wonull$remuneracion_por_empleado, 0.25, na.rm = TRUE)
Q3 <- quantile(df_wonull$remuneracion_por_empleado, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Definir los límites inferior y superior para detectar los outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Filtrar el data frame para eliminar los outliers
df_wonull_sin_outliers <- df_wonull %>%
  filter(remuneracion_por_empleado >= limite_inferior & remuneracion_por_empleado <= limite_superior)

# Ver el tamaño del nuevo data frame sin outliers
nrow(df_wonull_sin_outliers)
```

    ## [1] 819796

- Estos son las clases de empleos mejor pagados con outliers:

``` r
# Filtrar los 10 empleos mejor pagados (con outliers)
empleos_mejor_pagados_con_outliers <- df_wonull %>%
  arrange(desc(remuneracion_por_empleado)) %>%
  head(10)

# Ver los empleos mejor pagados con outliers
empleos_mejor_pagados_con_outliers
```

    ##     id_empresa anio gsectores
    ## 1  43937581093 2023 Servicios
    ## 2  49912943011 2023 Servicios
    ## 3  43937581093 2022 Servicios
    ## 4  50445006014 2023 Servicios
    ## 5  49871190126 2023 Servicios
    ## 6  47037015098 2023  Comercio
    ## 7  47037015098 2022  Comercio
    ## 8  13824854178 2023 Servicios
    ## 9  49871190126 2022 Servicios
    ## 10 14615742172 2022 Servicios
    ##                                                seccion
    ## 1    Actividades profesionales, científicas y técnicas
    ## 2    Actividades profesionales, científicas y técnicas
    ## 3    Actividades profesionales, científicas y técnicas
    ## 4                       Otras actividades de servicios
    ## 5  Actividades de servicios administrativos y de apoyo
    ## 6      Comercio, reparación automotores y motocicletas
    ## 7      Comercio, reparación automotores y motocicletas
    ## 8                            Actividades inmobiliarias
    ## 9  Actividades de servicios administrativos y de apoyo
    ## 10                           Actividades inmobiliarias
    ##                                                                       division
    ## 1                     Otras actividades profesionales, científicas y técnicas.
    ## 2  Actividades de oficinas principales; actividades de consultoría de gestión.
    ## 3                     Otras actividades profesionales, científicas y técnicas.
    ## 4                                   Otras actividades de servicios personales.
    ## 5                                     Actividades de alquiler y arrendamiento.
    ## 6   Comercio al por menor, excepto el de vehículos automotores y motocicletas.
    ## 7   Comercio al por menor, excepto el de vehículos automotores y motocicletas.
    ## 8                                                   Actividades inmobiliarias.
    ## 9                                     Actividades de alquiler y arrendamiento.
    ## 10                                                  Actividades inmobiliarias.
    ##                                                                                                                    clase
    ## 1                                                         Otras actividades profesionales, científicas y técnicas n.c.p.
    ## 2                                                                                   Actividades de oficinas principales.
    ## 3                                                         Otras actividades profesionales, científicas y técnicas n.c.p.
    ## 4                                                                       Otras actividades de servicios personales n.c.p.
    ## 5                                                      Alquiler de otros tipos de maquinaria, equipo y bienes tangibles.
    ## 6  Venta al por menor de productos farmacéuticos y medicinales, cosméticos y artículos de tocador en comercios especiali
    ## 7  Venta al por menor de productos farmacéuticos y medicinales, cosméticos y artículos de tocador en comercios especiali
    ## 8                                                  Actividades inmobiliarias realizadas con bienes propios o arrendados.
    ## 9                                                      Alquiler de otros tipos de maquinaria, equipo y bienes tangibles.
    ## 10                                      Actividades inmobiliarias realizadas a cambio de una retribución o por contrato.
    ##    provincia                          canton empleo empleo_hombres
    ## 1     Guayas                       Guayaquil      1              1
    ## 2      Azuay                          Cuenca      1              1
    ## 3     Guayas                       Guayaquil      1              1
    ## 4      Azuay                          Cuenca      1              1
    ## 5     Guayas                       Naranjito      1              1
    ## 6     Guayas                       Guayaquil      1             NA
    ## 7     Guayas                       Guayaquil      1             NA
    ## 8  Pichincha Distrito Metropolitano de Quito      1              1
    ## 9     Guayas                       Naranjito      1              1
    ## 10 Pichincha Distrito Metropolitano de Quito      1              1
    ##    empleo_mujeres empleo_rango_edad_1 empleo_rango_edad_2 empleo_rango_edad_3
    ## 1              NA                  NA                  NA                  NA
    ## 2              NA                  NA                  NA                  NA
    ## 3              NA                  NA                  NA                  NA
    ## 4              NA                  NA                  NA                  NA
    ## 5              NA                  NA                  NA                  NA
    ## 6               1                  NA                  NA                   1
    ## 7               1                  NA                  NA                   1
    ## 8              NA                  NA                  NA                  NA
    ## 9              NA                  NA                  NA                  NA
    ## 10             NA                  NA                  NA                  NA
    ##    empleo_rango_edad_4 empleo_rango_edad_5 empleo_iess_trim_1
    ## 1           1.00000000                  NA                  1
    ## 2           1.00000000                  NA                  1
    ## 3           1.00000000                  NA                  1
    ## 4                   NA                 0.5                 NA
    ## 5           1.00000000                  NA                  1
    ## 6                   NA                  NA                  1
    ## 7                   NA                  NA                  1
    ## 8           0.08333333                  NA                 NA
    ## 9           0.83333333                  NA                  1
    ## 10          1.00000000                  NA                  1
    ##    empleo_iess_trim_2 empleo_iess_trim_3 empleo_iess_trim_4 remuneraciones
    ## 1                   1                  1                  1       694448.1
    ## 2                   1                  1                  1       688223.4
    ## 3                   1                  1                  1       528771.4
    ## 4                  NA                  1                  1       518047.4
    ## 5                   1                  1                  1       510000.0
    ## 6                   1                  1                  1       482076.0
    ## 7                   1                  1                  1       482076.0
    ## 8                   1                 NA                 NA       462973.5
    ## 9                   1                  1                  1       425000.0
    ## 10                  1                  1                  1       385967.1
    ##    remuneraciones_hombres remuneraciones_mujeres missing_empleo
    ## 1                694448.1                     NA          FALSE
    ## 2                688223.4                     NA          FALSE
    ## 3                528771.4                     NA          FALSE
    ## 4                518047.4                     NA          FALSE
    ## 5                510000.0                     NA          FALSE
    ## 6                      NA                 482076          FALSE
    ## 7                      NA                 482076          FALSE
    ## 8                462973.5                     NA          FALSE
    ## 9                425000.0                     NA          FALSE
    ## 10               385967.1                     NA          FALSE
    ##    missing_remuneraciones employment_size remuneracion_por_empleado
    ## 1                   FALSE         Pequeña                  694448.1
    ## 2                   FALSE         Pequeña                  688223.4
    ## 3                   FALSE         Pequeña                  528771.4
    ## 4                   FALSE         Pequeña                  518047.4
    ## 5                   FALSE         Pequeña                  510000.0
    ## 6                   FALSE         Pequeña                  482076.0
    ## 7                   FALSE         Pequeña                  482076.0
    ## 8                   FALSE         Pequeña                  462973.5
    ## 9                   FALSE         Pequeña                  425000.0
    ## 10                  FALSE         Pequeña                  385967.1

- Empleos mejor pagados sin outliers

``` r
# Filtrar los 10 empleos mejor pagados sin outliers
empleos_mejor_pagados_sin_outliers <- df_wonull_sin_outliers %>%
  arrange(desc(remuneracion_por_empleado)) %>%
  head(10)

# Ver los empleos mejor pagados sin outliers
empleos_mejor_pagados_sin_outliers
```

- Comparacion de empleos mejor pagados:

``` r
# Agregar columna para diferenciar los datos con y sin outliers
empleos_con_outliers <- df_wonull %>%
  arrange(desc(remuneracion_por_empleado)) %>%
  head(10) %>%
  mutate(outlier_status = "Con Outliers")

empleos_sin_outliers <- df_wonull_sin_outliers %>%
  arrange(desc(remuneracion_por_empleado)) %>%
  head(10) %>%
  mutate(outlier_status = "Sin Outliers")

# Combinar ambos conjuntos de datos
empleos_comparados <- bind_rows(empleos_con_outliers, empleos_sin_outliers)

# Crear gráfico de barras
ggplot(empleos_comparados, aes(x = reorder(clase, remuneracion_por_empleado), y = remuneracion_por_empleado, fill = outlier_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparación de los 10 empleos mejor pagados",
    x = "Clase de empleo",
    y = "Remuneración por empleado",
    fill = "Estado de outliers"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

- Distribución de remuneración por empleado con y sin outliers:

``` r
# Agregar columna de estado de outliers a cada data frame
df_wonull$outlier_status <- "Con Outliers"
df_wonull_sin_outliers$outlier_status <- "Sin Outliers"

# Combinar los dos data frames
df_combined <- bind_rows(df_wonull, df_wonull_sin_outliers)

# Crear un boxplot con los datos combinados
ggplot(df_combined, aes(x = outlier_status, y = remuneracion_por_empleado, fill = outlier_status)) +
  geom_boxplot() +
  labs(
    title = "Distribución de remuneración por empleado (con vs sin outliers)",
    x = "Estado de outliers",
    y = "Remuneración por empleado"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

![](proyecto2_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

``` r
# Calcular promedios por sector para el data frame con outliers
top5_with_outliers <- df_wonull %>%
  group_by(gsectores) %>%
  summarise(promedio_remuneracion = mean(remuneracion_por_empleado, na.rm = TRUE)) %>%
  arrange(desc(promedio_remuneracion)) %>%
  slice(1:5)

# Calcular promedios por sector para el data frame sin outliers
top5_without_outliers <- df_wonull_sin_outliers %>%
  group_by(gsectores) %>%
  summarise(promedio_remuneracion = mean(remuneracion_por_empleado, na.rm = TRUE)) %>%
  arrange(desc(promedio_remuneracion)) %>%
  slice(1:5)

# Mostrar los resultados
top5_with_outliers
```

    ## # A tibble: 5 × 2
    ##   gsectores                                    promedio_remuneracion
    ##   <fct>                                                        <dbl>
    ## 1 Explotación de Minas y Canteras                              7985.
    ## 2 Servicios                                                    5493.
    ## 3 Comercio                                                     5283.
    ## 4 Industrias Manufactureras                                    5265.
    ## 5 Agricultura, ganadería, silvicultura y pesca                 5177.

``` r
top5_without_outliers
```

    ## # A tibble: 5 × 2
    ##   gsectores                                    promedio_remuneracion
    ##   <fct>                                                        <dbl>
    ## 1 Explotación de Minas y Canteras                              5221.
    ## 2 Industrias Manufactureras                                    5039.
    ## 3 Comercio                                                     4997.
    ## 4 Agricultura, ganadería, silvicultura y pesca                 4991.
    ## 5 Servicios                                                    4910.

``` r
# Calcular promedios por clase para el data frame con outliers
top5_clases_with_outliers <- df_wonull %>%
  group_by(clase) %>%
  summarise(promedio_remuneracion = mean(remuneracion_por_empleado, na.rm = TRUE)) %>%
  arrange(desc(promedio_remuneracion)) %>%
  slice(1:5)

# Calcular promedios por clase para el data frame sin outliers
top5_clases_without_outliers <- df_wonull_sin_outliers %>%
  group_by(clase) %>%
  summarise(promedio_remuneracion = mean(remuneracion_por_empleado, na.rm = TRUE)) %>%
  arrange(desc(promedio_remuneracion)) %>%
  slice(1:5)

# Mostrar los resultados
top5_clases_with_outliers
```

    ## # A tibble: 5 × 2
    ##   clase                                                    promedio_remuneracion
    ##   <fct>                                                                    <dbl>
    ## 1 Transporte de pasajeros por vía aérea.                                  21091.
    ## 2 Extracción de minerales de hierro.                                      18805.
    ## 3 Servicios de asesoramiento, gestión y operación de merc…                18000 
    ## 4 Transporte de carga por vía aérea.                                      17626.
    ## 5 Relaciones exteriores.                                                  17235.

``` r
top5_clases_without_outliers
```

    ## # A tibble: 5 × 2
    ##   clase                                                    promedio_remuneracion
    ##   <fct>                                                                    <dbl>
    ## 1 Actividades de defensa.                                                  7808.
    ## 2 Fabricación de otros hilos y cables eléctricos.                          7520.
    ## 3 Actividades de mantenimiento del orden público y de seg…                 7127.
    ## 4 Actividades de la administración pública en general.                     7003.
    ## 5 Fabricación de motocicletas.                                             6614.

``` r
# Unir los resultados en un solo data frame para facilitar el gráfico
resultados_clase <- bind_rows(
  top5_clases_with_outliers %>% mutate(outlier_status = "Con Outliers"),
  top5_clases_without_outliers %>% mutate(outlier_status = "Sin Outliers")
)

# Crear el gráfico de barras agrupadas
ggplot(resultados_clase, aes(x = reorder(clase, -promedio_remuneracion), y = promedio_remuneracion, fill = outlier_status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Comparación de promedio de remuneración por clase",
    x = "Clase",
    y = "Promedio de remuneración por empleado",
    fill = "Estado de outliers"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)
```

![](proyecto2_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

- Sobre las clases mejor remuneradas:
- Con outliers, el análisis se concentra en sectores muy específicos y
  de alta especialización.
- Sin outliers, los resultados son más generalizables y se enfocan en
  sectores más tradicionales o ampliamente distribuidos.
- Si se refleja la tendencia general del mercado, se deberían usar los
  resultados sin outliers.
- Por otro lado, si se busca identificar sectores con oportunidades
  únicas o excepcionales, los resultados con outliers pueden aportar
  información valiosa.

``` r
# Calcular promedio de remuneraciones por estado de outlier
promedios_remuneracion <- empleos_comparados %>%
  group_by(outlier_status) %>%
  summarise(promedio_remuneracion = mean(remuneracion_por_empleado, na.rm = TRUE))

# Mostrar los resultados
print(promedios_remuneracion)
```

    ## # A tibble: 2 × 2
    ##   outlier_status promedio_remuneracion
    ##   <chr>                          <dbl>
    ## 1 Con Outliers                 517758.
    ## 2 Sin Outliers                   8034.

- Con Outliers:

- El promedio de 517,758.29 es extremadamente alto y está influenciado
  por los valores atípicos, que representan casos extraordinarios.

- Esto refleja que las empresas con valores muy altos de remuneración
  por empleado tienen un impacto desproporcionado en el promedio, lo que
  puede distorsionar las conclusiones sobre el comportamiento general de
  los datos.

- Sin Outliers:

- El promedio de 8,034.20 es mucho más bajo y representa de manera más
  fiel el comportamiento típico de los datos.

- Excluir los valores atípicos permite obtener un promedio más
  representativo del conjunto mayoritario de empresas.

``` r
# Crear data frames resumidos por clase para los dos escenarios
df_con_outliers <- df_wonull %>%
  group_by(gsectores) %>%
  summarise(
    promedio_remuneracion = mean(remuneracion_por_empleado, na.rm = TRUE),
    mediana_remuneracion = median(remuneracion_por_empleado, na.rm = TRUE),
    sd_remuneracion = sd(remuneracion_por_empleado, na.rm = TRUE),
    n = n()
  )

df_sin_outliers <- df_wonull_sin_outliers %>%
  group_by(gsectores) %>%
  summarise(
    promedio_remuneracion = mean(remuneracion_por_empleado, na.rm = TRUE),
    mediana_remuneracion = median(remuneracion_por_empleado, na.rm = TRUE),
    sd_remuneracion = sd(remuneracion_por_empleado, na.rm = TRUE),
    n = n()
  )

# Añadir una columna para indicar el estado de outliers
df_con_outliers$status <- "Con Outliers"
df_sin_outliers$status <- "Sin Outliers"

# Combinar ambos en un único data frame
df_combinado <- bind_rows(df_con_outliers, df_sin_outliers)

# Boxplot para comparar distribución de remuneración por clase
ggplot(df_combinado, aes(x = reorder(gsectores, promedio_remuneracion), y = promedio_remuneracion, fill = status)) +
  geom_boxplot() +
  labs(
    title = "Distribución de remuneración promedio por sector (con vs sin outliers)",
    x = "Sector",
    y = "Promedio de remuneración"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Con Outliers" = "tomato", "Sin Outliers" = "skyblue"))
```

![](proyecto2_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

- El análisis de remuneraciones por sector revela que los valores
  atípicos tienen un impacto significativo en los rankings. El sector de
  Explotación de minas y canteras se mantiene como el más remunerado,
  tanto con como sin outliers, aunque la gran brecha entre estos valores
  sugiere que los datos extremos inflan notablemente los promedios. Por
  otro lado, el sector de Servicios experimenta un cambio drástico,
  cayendo de la segunda posición con outliers a la penúltima sin ellos,
  lo que indica una fuerte dependencia de pocos casos extremos que no
  reflejan la realidad general del sector.

- Otros sectores como Industrias manufactureras y Comercio muestran
  mayor estabilidad, mientras que Construcción permanece como el menos
  remunerado en ambos escenarios, lo que sugiere una estructura salarial
  más homogénea. Finalmente, el sector Agricultura también muestra
  cierta influencia de valores extremos, destacando la necesidad de
  analizar subsectores para obtener una visión más detallada de los
  factores que afectan la distribución de las remuneraciones.

``` r
# Filtrar los sectores Minas y Servicios para ambos dataframes
minas_servicios_outliers <- df_wonull %>%
  filter(gsectores %in% c("Explotación de minas y canteras", "Servicios"))

minas_servicios_sin_outliers <- df_wonull_sin_outliers %>%
  filter(gsectores %in% c("Explotación de minas y canteras", "Servicios"))

# Gráfico con outliers
plot_with_outliers <- ggplot(minas_servicios_outliers, aes(x = empleo, y = remuneracion_por_empleado, color = gsectores)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Remuneración por empleado (con outliers)",
    x = "Empleo promedio",
    y = "Remuneración por empleado",
    color = "Sector"
  ) +
  theme_minimal()

# Gráfico sin outliers
plot_without_outliers <- ggplot(minas_servicios_sin_outliers, aes(x = empleo, y = remuneracion_por_empleado, color = gsectores)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Remuneración por empleado (sin outliers)",
    x = "Empleo promedio",
    y = "Remuneración por empleado",
    color = "Sector"
  ) +
  theme_minimal()

# Mostrar gráficos
plot_with_outliers
```

![](proyecto2_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

``` r
plot_without_outliers
```

![](proyecto2_files/figure-gfm/unnamed-chunk-68-2.png)<!-- -->

- En el sector de Minas, los outliers exageran la escala de
  remuneraciones, ocultando relaciones regulares y dando la impresión de
  una industria altamente remunerada pero desigual. Sin ellos, se
  observa una distribución más uniforme que refleja mejor la realidad
  del sector. En Servicios, los outliers elevan artificialmente las
  remuneraciones promedio, posicionándolo como uno de los sectores más
  lucrativos; sin embargo, al eliminarlos, el sector se ubica entre los
  menos remunerados, evidenciando que la mayoría de las empresas tienen
  empleos de baja remuneración.

``` r
# Agrupar y calcular los totales y promedios por género (con outliers)
genero_con_outliers <- df_wonull %>%
  summarise(
    total_empleo_hombres = sum(empleo_hombres, na.rm = TRUE),
    total_empleo_mujeres = sum(empleo_mujeres, na.rm = TRUE),
    total_remuneraciones_hombres = sum(remuneraciones_hombres, na.rm = TRUE),
    total_remuneraciones_mujeres = sum(remuneraciones_mujeres, na.rm = TRUE),
    promedio_remuneracion_hombres = mean(remuneraciones_hombres / empleo_hombres, na.rm = TRUE),
    promedio_remuneracion_mujeres = mean(remuneraciones_mujeres / empleo_mujeres, na.rm = TRUE)
  )

# Agrupar y calcular los totales y promedios por género (sin outliers)
genero_sin_outliers <- df_wonull_sin_outliers %>%
  summarise(
    total_empleo_hombres = sum(empleo_hombres, na.rm = TRUE),
    total_empleo_mujeres = sum(empleo_mujeres, na.rm = TRUE),
    total_remuneraciones_hombres = sum(remuneraciones_hombres, na.rm = TRUE),
    total_remuneraciones_mujeres = sum(remuneraciones_mujeres, na.rm = TRUE),
    promedio_remuneracion_hombres = mean(remuneraciones_hombres / empleo_hombres, na.rm = TRUE),
    promedio_remuneracion_mujeres = mean(remuneraciones_mujeres / empleo_mujeres, na.rm = TRUE)
  )

# Unir resultados en un solo data frame para facilitar el análisis
resultado_genero <- bind_rows(
  genero_con_outliers %>% mutate(outlier_status = "Con Outliers"),
  genero_sin_outliers %>% mutate(outlier_status = "Sin Outliers")
)

# Mostrar los resultados en tabla
print(resultado_genero)
```

    ##   total_empleo_hombres total_empleo_mujeres total_remuneraciones_hombres
    ## 1              3478275              2361040                  32406579527
    ## 2              1671424              1034258                   9469086455
    ##   total_remuneraciones_mujeres promedio_remuneracion_hombres
    ## 1                  22558514054                      5626.742
    ## 2                   5669594118                      5033.218
    ##   promedio_remuneracion_mujeres outlier_status
    ## 1                      5684.444   Con Outliers
    ## 2                      4967.058   Sin Outliers

``` r
# 1. Comparación de empleo total por género
ggplot(resultado_genero, aes(x = outlier_status)) +
  geom_bar(aes(y = total_empleo_hombres, fill = "Hombres"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = total_empleo_mujeres, fill = "Mujeres"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "pink")) +
  labs(
    title = "Comparación de empleo total por género (con vs sin outliers)",
    x = "Estado de outliers",
    y = "Empleo total",
    fill = "Género"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

``` r
# 2. Comparación de remuneraciones totales por género
ggplot(resultado_genero, aes(x = outlier_status)) +
  geom_bar(aes(y = total_remuneraciones_hombres, fill = "Hombres"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = total_remuneraciones_mujeres, fill = "Mujeres"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "pink")) +
  labs(
    title = "Comparación de remuneraciones totales por género (con vs sin outliers)",
    x = "Estado de outliers",
    y = "Remuneraciones totales",
    fill = "Género"
  ) +
  theme_minimal()
```

![](proyecto2_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

``` r
# Calcular el promedio de remuneraciones para hombres y mujeres por sector con y sin outliers

# Promedio de remuneraciones por sector para df_wonull (con outliers)
df_wonull_promedios <- df_wonull %>%
  group_by(gsectores) %>%
  summarise(
    promedio_remuneracion_hombres = mean(remuneraciones_hombres, na.rm = TRUE),
    promedio_remuneracion_mujeres = mean(remuneraciones_mujeres, na.rm = TRUE)
  )

# Promedio de remuneraciones por sector para df_wonull_sin_outliers (sin outliers)
df_wonull_sin_outliers_promedios <- df_wonull_sin_outliers %>%
  group_by(gsectores) %>%
  summarise(
    promedio_remuneracion_hombres = mean(remuneraciones_hombres, na.rm = TRUE),
    promedio_remuneracion_mujeres = mean(remuneraciones_mujeres, na.rm = TRUE)
  )

# MOstrar valores 
df_wonull_promedios
```

    ## # A tibble: 6 × 3
    ##   gsectores                        promedio_remuneracio…¹ promedio_remuneracio…²
    ##   <fct>                                             <dbl>                  <dbl>
    ## 1 Agricultura, ganadería, silvicu…                 74950.                 39820.
    ## 2 Comercio                                         28657.                 19001.
    ## 3 Construcción                                     34703.                 16142.
    ## 4 Explotación de Minas y Canteras                 435735.                131432.
    ## 5 Industrias Manufactureras                        77509.                 37851.
    ## 6 Servicios                                        45183.                 47638.
    ## # ℹ abbreviated names: ¹​promedio_remuneracion_hombres,
    ## #   ²​promedio_remuneracion_mujeres

``` r
df_wonull_sin_outliers_promedios
```

    ## # A tibble: 6 × 3
    ##   gsectores                        promedio_remuneracio…¹ promedio_remuneracio…²
    ##   <fct>                                             <dbl>                  <dbl>
    ## 1 Agricultura, ganadería, silvicu…                 55849.                 33247.
    ## 2 Comercio                                         12619.                  9747.
    ## 3 Construcción                                     20513.                  8910.
    ## 4 Explotación de Minas y Canteras                  48350.                 13147.
    ## 5 Industrias Manufactureras                        20398.                 15370.
    ## 6 Servicios                                        14609.                 11218.
    ## # ℹ abbreviated names: ¹​promedio_remuneracion_hombres,
    ## #   ²​promedio_remuneracion_mujeres

``` r
# Crear el gráfico de burbujas comparando las remuneraciones promedio para hombres y mujeres por sector
ggplot() +
  geom_point(data = df_wonull_promedios, aes(x = promedio_remuneracion_hombres, 
                                             y = promedio_remuneracion_mujeres, 
                                             size = promedio_remuneracion_hombres + promedio_remuneracion_mujeres, 
                                             color = gsectores),
             alpha = 0.6) +
  geom_point(data = df_wonull_sin_outliers_promedios, aes(x = promedio_remuneracion_hombres, 
                                                         y = promedio_remuneracion_mujeres, 
                                                         size = promedio_remuneracion_hombres + promedio_remuneracion_mujeres, 
                                                         color = gsectores),
             shape = 17, alpha = 0.6) +
  scale_size_continuous(range = c(2, 15)) + # Ajuste del tamaño de las burbujas
  labs(
    title = "Comparación de remuneración promedio por género y sector (con vs sin outliers)",
    x = "Promedio remuneración hombres",
    y = "Promedio remuneración mujeres",
    size = "Tamaño de la burbuja",
    color = "Sector"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set3")
```

![](proyecto2_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

- El análisis revela que la brecha salarial de género es evidente en
  todos los sectores, pero con una variabilidad significativa según el
  sector. Algunos sectores presentan brechas salariales más amplias
  (como Explotación de Minas y Canteras), mientras que otros tienen una
  diferencia menos pronunciada (como Servicios). La eliminación de los
  outliers ajusta las percepciones de la magnitud de la brecha, y
  permite una observación más precisa del impacto de los valores
  extremos en los análisis salariales. Sin embargo, incluso sin
  outliers, la desigualdad salarial por género sigue siendo una cuestión
  importante en la mayoría de los sectores.

``` r
# Calcular el promedio de remuneraciones por clase de trabajo para hombres y mujeres
# Primero para df_wonull (con outliers)
top_5_hombres_outliers <- df_wonull %>%
  group_by(clase) %>%
  summarise(promedio_remuneracion_hombres = mean(remuneraciones_hombres, na.rm = TRUE)) %>%
  arrange(desc(promedio_remuneracion_hombres)) %>%
  top_n(5, promedio_remuneracion_hombres)

top_5_mujeres_outliers <- df_wonull %>%
  group_by(clase) %>%
  summarise(promedio_remuneracion_mujeres = mean(remuneraciones_mujeres, na.rm = TRUE)) %>%
  arrange(desc(promedio_remuneracion_mujeres)) %>%
  top_n(5, promedio_remuneracion_mujeres)

# Ahora para df_wonull_sin_outliers (sin outliers)
top_5_hombres_sin_outliers <- df_wonull_sin_outliers %>%
  group_by(clase) %>%
  summarise(promedio_remuneracion_hombres = mean(remuneraciones_hombres, na.rm = TRUE)) %>%
  arrange(desc(promedio_remuneracion_hombres)) %>%
  top_n(5, promedio_remuneracion_hombres)

top_5_mujeres_sin_outliers <- df_wonull_sin_outliers %>%
  group_by(clase) %>%
  summarise(promedio_remuneracion_mujeres = mean(remuneraciones_mujeres, na.rm = TRUE)) %>%
  arrange(desc(promedio_remuneracion_mujeres)) %>%
  top_n(5, promedio_remuneracion_mujeres)

# Mostrar resultados
top_5_hombres_outliers
```

    ## # A tibble: 5 × 2
    ##   clase                                                   promedio_remuneracio…¹
    ##   <fct>                                                                    <dbl>
    ## 1 Extracción de petróleo crudo.                                        13544610.
    ## 2 Relaciones exteriores.                                               11789714.
    ## 3 Actividades de planes de seguridad social de afiliació…               6227783.
    ## 4 Fabricación de productos de la refinación del petróleo.               4998796.
    ## 5 Actividades de defensa.                                               2043791.
    ## # ℹ abbreviated name: ¹​promedio_remuneracion_hombres

``` r
top_5_mujeres_outliers
```

    ## # A tibble: 5 × 2
    ##   clase                                                   promedio_remuneracio…¹
    ##   <fct>                                                                    <dbl>
    ## 1 Relaciones exteriores.                                                9553458.
    ## 2 Actividades de planes de seguridad social de afiliació…               7754327.
    ## 3 Extracción de petróleo crudo.                                         3351367.
    ## 4 Otras actividades de concesión de crédito.                            2047253.
    ## 5 Regulación de las actividades de organismos que presta…               1529183.
    ## # ℹ abbreviated name: ¹​promedio_remuneracion_mujeres

``` r
top_5_hombres_sin_outliers
```

    ## # A tibble: 5 × 2
    ##   clase                                                   promedio_remuneracio…¹
    ##   <fct>                                                                    <dbl>
    ## 1 Actividades de defensa.                                              13570574.
    ## 2 Elaboración y conservación de pescados, crustáceos y m…               1009320.
    ## 3 Actividades de exhibición de películas cinematográfica…                557087.
    ## 4 Elaboración de aceites y grasas de origen vegetal y an…                399665.
    ## 5 Recolección de desechos no peligrosos.                                 380338.
    ## # ℹ abbreviated name: ¹​promedio_remuneracion_hombres

``` r
top_5_mujeres_sin_outliers
```

    ## # A tibble: 5 × 2
    ##   clase                                                   promedio_remuneracio…¹
    ##   <fct>                                                                    <dbl>
    ## 1 Actividades de defensa.                                               1605356.
    ## 2 Regulación de las actividades de organismos que presta…                819190.
    ## 3 Elaboración y conservación de pescados, crustáceos y m…                801358.
    ## 4 Actividades de exhibición de películas cinematográfica…                359298.
    ## 5 Cultivo de tabaco.                                                     350142.
    ## # ℹ abbreviated name: ¹​promedio_remuneracion_mujeres

``` r
# Unir los resultados en un solo data frame para hombres y mujeres con outliers
top_hombres_outliers <- top_5_hombres_outliers %>%
  mutate(genero = "Hombres", promedio_remuneracion = promedio_remuneracion_hombres) %>%
  select(clase, genero, promedio_remuneracion)

top_mujeres_outliers <- top_5_mujeres_outliers %>%
  mutate(genero = "Mujeres", promedio_remuneracion = promedio_remuneracion_mujeres) %>%
  select(clase, genero, promedio_remuneracion)

top_outliers <- bind_rows(top_hombres_outliers, top_mujeres_outliers)

# Unir los resultados en un solo data frame para hombres y mujeres sin outliers
top_hombres_sin_outliers <- top_5_hombres_sin_outliers %>%
  mutate(genero = "Hombres", promedio_remuneracion = promedio_remuneracion_hombres) %>%
  select(clase, genero, promedio_remuneracion)

top_mujeres_sin_outliers <- top_5_mujeres_sin_outliers %>%
  mutate(genero = "Mujeres", promedio_remuneracion = promedio_remuneracion_mujeres) %>%
  select(clase, genero, promedio_remuneracion)

top_sin_outliers <- bind_rows(top_hombres_sin_outliers, top_mujeres_sin_outliers)


# Gráfico para resultados con outliers
ggplot(top_outliers, aes(x = reorder(clase, promedio_remuneracion), y = promedio_remuneracion, fill = genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 5 Clases de Trabajo Mejor Remuneradas (Con Outliers)",
       x = "Clase de Trabajo", y = "Promedio de Remuneración") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("Hombres" = "blue", "Mujeres" = "pink"))
```

![](proyecto2_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

``` r
# Gráfico para resultados sin outliers
ggplot(top_sin_outliers, aes(x = reorder(clase, promedio_remuneracion), y = promedio_remuneracion, fill = genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 5 Clases de Trabajo Mejor Remuneradas (Sin Outliers)",
       x = "Clase de Trabajo", y = "Promedio de Remuneración") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("Hombres" = "blue", "Mujeres" = "pink"))
```

![](proyecto2_files/figure-gfm/unnamed-chunk-74-2.png)<!-- -->

- En Ecuador, las clases de trabajo con las mejores remuneraciones para
  hombres están dominadas por sectores como la extracción de petróleo
  crudo, relaciones exteriores, y actividades de planes de seguridad
  social, con salarios significativamente más altos en comparación con
  las mujeres. A pesar de que las mujeres también tienen acceso a
  sectores bien remunerados como relaciones exteriores y actividades de
  planes de seguridad social, la brecha salarial sigue siendo evidente,
  especialmente en sectores claves como la industria petrolera.

``` r
# Análisis de la cantidad de empleados hombres y mujeres por sector

# Resumen para df_wonull (con outliers)
empleo_sectores_con_outliers <- df_wonull %>%
  group_by(gsectores) %>%
  summarise(
    total_empleo_hombres = sum(empleo_hombres, na.rm = TRUE),
    total_empleo_mujeres = sum(empleo_mujeres, na.rm = TRUE)
  )

# Resumen para df_wonull_sin_outliers (sin outliers)
empleo_sectores_sin_outliers <- df_wonull_sin_outliers %>%
  group_by(gsectores) %>%
  summarise(
    total_empleo_hombres = sum(empleo_hombres, na.rm = TRUE),
    total_empleo_mujeres = sum(empleo_mujeres, na.rm = TRUE)
  )

# Imprimir los resultados
print("Empleo por sector con outliers:")
```

    ## [1] "Empleo por sector con outliers:"

``` r
print(empleo_sectores_con_outliers)
```

    ## # A tibble: 6 × 3
    ##   gsectores                            total_empleo_hombres total_empleo_mujeres
    ##   <fct>                                               <dbl>                <dbl>
    ## 1 Agricultura, ganadería, silvicultur…              349339               118173 
    ## 2 Comercio                                          630054               469254 
    ## 3 Construcción                                      174343                32341 
    ## 4 Explotación de Minas y Canteras                    66204.                9491.
    ## 5 Industrias Manufactureras                         504607.              218011.
    ## 6 Servicios                                        1753728.             1513770.

``` r
print("Empleo por sector sin outliers:")
```

    ## [1] "Empleo por sector sin outliers:"

``` r
print(empleo_sectores_sin_outliers)
```

    ## # A tibble: 6 × 3
    ##   gsectores                            total_empleo_hombres total_empleo_mujeres
    ##   <fct>                                               <dbl>                <dbl>
    ## 1 Agricultura, ganadería, silvicultur…               251750                91895
    ## 2 Comercio                                           348764               288496
    ## 3 Construcción                                       104956                19437
    ## 4 Explotación de Minas y Canteras                     14952                 1762
    ## 5 Industrias Manufactureras                          183455               112537
    ## 6 Servicios                                          767547               520131

``` r
# Opcional: Gráfico comparativo entre hombres y mujeres por sector con y sin outliers

# Usando ggplot2 para visualizar la comparación
library(ggplot2)

# Crear un gráfico para comparar la cantidad de empleados hombres y mujeres por sector
empleo_sectores_con_outliers_long <- empleo_sectores_con_outliers %>%
  pivot_longer(cols = c(total_empleo_hombres, total_empleo_mujeres),
               names_to = "genero", values_to = "empleo")

empleo_sectores_sin_outliers_long <- empleo_sectores_sin_outliers %>%
  pivot_longer(cols = c(total_empleo_hombres, total_empleo_mujeres),
               names_to = "genero", values_to = "empleo")

# Combinar ambos dataframes para comparación
empleo_comparacion <- bind_rows(
  mutate(empleo_sectores_con_outliers_long, outliers = "Con Outliers"),
  mutate(empleo_sectores_sin_outliers_long, outliers = "Sin Outliers")
)

# Gráfico de barras comparativo
ggplot(empleo_comparacion, aes(x = reorder(gsectores, -empleo), y = empleo, fill = genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ outliers) +
  labs(title = "Comparación de Empleo por Sector (Hombres vs Mujeres)",
       x = "Sector", y = "Número de Empleados",
       fill = "Género") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](proyecto2_files/figure-gfm/unnamed-chunk-75-1.png)<!-- -->

- Desbalance de Género: En la mayoría de los sectores, los hombres
  superan ampliamente a las mujeres en términos de empleo total. Sin
  embargo, al eliminar los outliers, las proporciones de género en los
  sectores se vuelven más equilibradas, lo que sugiere que los valores
  extremos (outliers) en los datos influyen significativamente en la
  visualización de estas diferencias.

- Los sectores: Construcción y Explotación de Minas y Canteras muestran
  un gran desbalance de género.

``` r
# Usando df_wonull_sin_outliers
top_cantones_sin_outliers <- df_wonull_sin_outliers %>%
  group_by(canton) %>%
  summarise(numero_industrias = n()) %>%
  arrange(desc(numero_industrias)) %>%
  head(5)

# Usando df_wonull
top_cantones_con_outliers <- df_wonull %>%
  group_by(canton) %>%
  summarise(numero_industrias = n()) %>%
  arrange(desc(numero_industrias)) %>%
  head(5)

# Mostrar resultados
top_cantones_sin_outliers
```

    ## # A tibble: 5 × 2
    ##   canton                          numero_industrias
    ##   <fct>                                       <int>
    ## 1 Distrito Metropolitano de Quito            222713
    ## 2 Guayaquil                                  136631
    ## 3 Cuenca                                      50559
    ## 4 Ambato                                      29477
    ## 5 Machala                                     19169

``` r
top_cantones_con_outliers
```

    ## # A tibble: 5 × 2
    ##   canton                          numero_industrias
    ##   <fct>                                       <int>
    ## 1 Distrito Metropolitano de Quito            276274
    ## 2 Guayaquil                                  171697
    ## 3 Cuenca                                      60829
    ## 4 Ambato                                      34615
    ## 5 Machala                                     23088

``` r
# Top 5 Clases Más Populares
# Usando df_wonull_sin_outliers
top_clases_sin_outliers <- df_wonull_sin_outliers %>%
  group_by(clase) %>%
  summarise(numero_ocurrencias = n()) %>%
  arrange(desc(numero_ocurrencias)) %>%
  head(5)

# Usando df_wonull
top_clases_con_outliers <- df_wonull %>%
  group_by(clase) %>%
  summarise(numero_ocurrencias = n()) %>%
  arrange(desc(numero_ocurrencias)) %>%
  head(5)

# Mostrar resultados
top_clases_sin_outliers
```

    ## # A tibble: 5 × 2
    ##   clase                                                       numero_ocurrencias
    ##   <fct>                                                                    <int>
    ## 1 Transporte de carga por carretera.                                       40540
    ## 2 Venta al por menor en comercios no especializados con pred…              38869
    ## 3 Actividades de restaurantes y de servicio móvil de comidas.              36057
    ## 4 Otras actividades profesionales, científicas y técnicas n.…              35602
    ## 5 Otras actividades de servicios personales n.c.p.                         33953

``` r
top_clases_con_outliers
```

    ## # A tibble: 5 × 2
    ##   clase                                                       numero_ocurrencias
    ##   <fct>                                                                    <int>
    ## 1 Transporte de carga por carretera.                                       49764
    ## 2 Otras actividades profesionales, científicas y técnicas n.…              44838
    ## 3 Venta al por menor en comercios no especializados con pred…              43645
    ## 4 Otras actividades de servicios personales n.c.p.                         42888
    ## 5 Actividades de restaurantes y de servicio móvil de comidas.              40643

## Conclusiones

### Impacto de los Outliers

Los outliers tienen un efecto significativo en las métricas clave, como
remuneraciones promedio y distribución de empleados por género y sector.
Al incluir outliers, las cifras presentan una distorsión que dificulta
interpretar las tendencias reales. Al eliminarlos, los datos muestran
una visión más precisa y manejable, permitiendo identificar patrones y
desbalances de género o sector de manera más confiable.

### Brecha Salarial por Género

Existe una brecha salarial notable entre hombres y mujeres en la mayoría
de los sectores. En sectores como **“Servicios”** y **“Agricultura”**,
las mujeres reciben salarios promedio considerablemente más bajos que
los hombres. Sin embargo, al ajustar los datos eliminando outliers, la
magnitud de esta brecha se reduce, pero persiste.

### Sectores y Clases de Trabajo con Mejores Remuneraciones

Los sectores de **“Explotación de Minas y Canteras”** y **“Relaciones
Exteriores”** ofrecen las remuneraciones más altas para hombres y
mujeres respectivamente. Al analizar las clases de trabajo, actividades
relacionadas con defensa, petróleo, y regulación de organismos también
destacaron como las mejores remuneradas.

### Distribución de Empleo por Género

Los sectores como **“Agricultura”** y **“Comercio”** presentan una mayor
cantidad de empleados tanto hombres como mujeres, aunque en términos
proporcionales los hombres tienden a estar sobrerrepresentados en
sectores como **“Construcción”** y **“Minas y Canteras”**. Sectores como
**“Servicios”** presentan una distribución más equilibrada entre ambos
géneros.

### Visualización de Datos

Las visualizaciones, como gráficos de burbujas y barras, resultaron
esenciales para identificar patrones y diferencias entre géneros y
sectores. Ayudaron a comunicar de manera efectiva las conclusiones
derivadas de los análisis numéricos.
