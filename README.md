# raefy

Un paquete de R para interactuar con el Diccionario de la Lengua Española (RAE). Permite buscar definiciones, obtener conjugaciones verbales, buscar anagramas y más.

## Instalación

Puedes instalar este paquete localmente

``` r
# Desde directorio local
devtools::install(".")
```

O con devtools directo desde github

``` r
# Desde github
devtools::install_github("exetrujillo/raefy")
```

## Uso Básico

Carga el paquete:

``` r
library(raefy)
```

### Buscar una palabra

Obtén definiciones, etimología y más:

``` r
info <- obtener_palabra("programar")
print(info$definiciones)
```

### Obtener Conjugaciones

Para verbos, puedes solicitar la tabla de conjugación completa:

``` r
verbo <- obtener_palabra("correr", conjugaciones = TRUE)
print(head(verbo$conjugaciones))
```

### Otras Funciones

#### Palabra del Día

``` r
palabra_del_dia()
```

#### Palabra Aleatoria

``` r
palabra_aleatoria()
```

#### Buscar Anagramas

``` r
buscar_anagrama("amor")
# [1] "armo" "armó" "maro" "mora" "morá" "ramo" "roma" "Roma"
```

#### Autocompletar

``` r
autocompletar("const")
```

#### Procesamiento en Lote

Busca múltiples palabras y obtén un dataframe consolidado:

``` r
df <- recorrer_palabras(c("ciencia", "arte", "tecnología"))
```