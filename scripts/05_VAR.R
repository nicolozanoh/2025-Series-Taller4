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
        "zoo",
        "expm"
      )

## ---- 03 Cargamos data ----

df <-  read_excel("stores\\VAR.xlsx")

## ---- 04 Organizamos los datos ----

df <- df %>% rename(
  date = Fecha,
  ingresos = `Ingresos Tributarios`,
  brent = Brent
)

df <- df %>% mutate(
  ln_ingresos = log(ingresos),
  ln_brent = log(brent)
)

df$date <- as.Date(df$date)

df <- df[order(df$date), ]

df$d_ln_ing <- c(NA, diff(df$ln_ingresos))
df$d_ln_brent <- c(NA, diff(df$ln_brent))

df_dif <- df %>% select(date, d_ln_ing, d_ln_brent)
df_dif <- na.omit(df_dif)

## ---- 05 Creamos objetos de tiempo ----
ts_ing <- xts(df$ln_ingresos, order.by = df$date)
ts_ing <- na.omit(ts_ing)

ts_brent <- xts(df$ln_brent, order.by = df$date)
ts_brent <- na.omit(ts_brent)

## ---- 06 Diferenciamos las series ----
d_ing <- diff(ts_ing)
d_ing <- na.omit(d_ing)

d_brent <- diff(ts_brent)
d_brent <- na.omit(d_brent)

## ---- 07 Tests de raíz unitaria ----

adf_ing   <- ur.df(ts_ing,   type = "none", selectlags = "BIC")
adf_ing_diff   <- ur.df(d_ing,   type = "none", selectlags = "BIC")
adf_brent <- ur.df(ts_brent, type = "none", selectlags = "BIC")
adf_brent_diff <- ur.df(d_brent, type = "none", selectlags = "BIC")


summary(adf_ing)
summary(adf_ing_diff)
summary(adf_brent)
summary(adf_brent_diff)


## ---- 08 Estimacion VAR ----
y <-  cbind(d_brent, d_ing)

ci <- data.frame(
  p = c(2,4,8,12),
  AIC = NA,
  BIC = NA
)

for (r in seq_along(ci$p)) {
  p = ci$p[r]
  print(p)
  fit <- vars::VAR(y, p = p, type = "const")
  ci[r, 2:3] = c(stats::AIC(fit), stats::BIC(fit))
}

cat("Min AIC")
print(ci$p[which.min(ci$AIC)])

cat("Min BIC")
print(ci$p[which.min(ci$BIC)])

p <- ci$p[which.min(ci$BIC)]
p_AIC <- ci$p[which.min(ci$AIC)]

## ---- 09 estimamos VAR optimo ----

var_fin <- vars::VAR(y, p = p, type = "const")

## ---- 10 causlaidad de granger ----

caus_ing <- causality(var_fin, cause = "d_brent")
caus_brent <- causality(var_fin, cause = "d_ing")

print(caus_ing$Granger)
print(caus_brent$Granger)

## ---- 11 Impulso respuesta ----

ir_brent_ing <- irf(var_fin, 
                    impulse = "d_brent", 
                    response = "d_ing", 
                    n.ahead = 20, 
                    runs = 1000, 
                    boot = TRUE)

ir_ing_brent <- irf(var_fin, 
                    impulse = "d_ing", 
                    response = "d_brent", 
                    n.ahead = 20, 
                    runs = 1000, 
                    boot = TRUE)

plot(ir_ing_brent)

plot(ir_brent_ing)

## ---- 12 descomposicion varianza ingreso ----
 
dv_ing <- fevd(var_fin, n.ahead = 20)
 
df_dv <- as.data.frame(dv_ing[["d_ing"]])
df_dv$horizon <- 1:20
df_dv <- pivot_longer(df_dv, cols = -horizon,
                      names_to  = "choque",
                       values_to = "fracvar") 
 
 
 
ggplot(df_dv, aes(x = horizon, y = fracvar * 100, color = choque)) +
   geom_line(size = 1.2) +
   scale_x_continuous(breaks = seq(0,20,2)) +
   labs(
     title = "descomposición de varianza del ingreso",
     x     = "periodo",
     y     = "% explicado"
   ) +
   theme_minimal(base_size = 14) +
   theme(legend.position = "top")

## ---- 13 descomposicion historica varianza ingreso ----
