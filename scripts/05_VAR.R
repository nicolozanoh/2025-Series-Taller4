## ---------------------------
##
## Script name: 05_VAR.R
##
## Purpose of script: Desarrollar el punto 5 del cuarto taller de Series de Tiempo
##
## Author: Nicolas Lozano, Valentina Rondon, Sofia Prada, Juan Jose Gutierrez
##
## Date Created: 2025-05-07
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## ---- 01 Limpiamos entorno ----
rm(list = ls())

## ---- 02 Cargamos paquetes ----

require(pacman)

p_load(
        "ggplot2",
        "tidyverse",
        "dplyr",
        "vars",
        "tseries",
        "forecast",
        "xts",
        "readxl",
        "zoo"
      )

## ---- 03 Cargamos data ----

df <-  read_excel("stores\\VAR.xlsx")

## ---- 04 Organizamos los datos ----

df <- df %>% rename(
  fecha = Fecha,
  ingresos = `Ingresos Tributarios`,
  brent = Brent
)

df <- df %>% mutate(
  ln_ingresos = log(ingresos),
  ln_brent = log(brent)
)

df$fecha <- as.Date(df$fecha)

df <- df[order(df$fecha), ]

## ---- 05 Creamos objetos de tiempo ----

ts_ing <- xts(df$ln_ingresos, order.by = df$fecha)
ts_ing <- na.omit(ts_ing)

ts_brent <- xts(df$ln_brent, order.by = df$fecha)
ts_brent <- na.omit(ts_brent)

## ---- 06 Diferenciamos las series ----

d_ing <- diff(ts_ing)
d_ing <- na.omit(d_ing)

d_brent <- diff(ts_brent)
d_brent <- na.omit(d_brent)

## ---- 07 Tests de raíz unitaria ----

adf_ing_level   <- ur.df(ts_ing,   type = "drift", selectlags = "AIC")
adf_ing_diff1   <- ur.df(d_ing,   type = "drift", selectlags = "AIC")
adf_brent_level <- ur.df(ts_brent, type = "drift", selectlags = "AIC")
adf_brent_diff1 <- ur.df(d_brent, type = "drift", selectlags = "AIC")

