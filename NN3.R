library(keras)
library(tidyverse)
library(car)
rm(list = ls())
df_ent<-read_csv("data_in4.csv")%>%
  rename("PIT704B3" = "99_99619_LCI07_SE704B_PIT-704B3_PV",
         "PIT704B1" = "99_99619_LCI07_SE704B_PIT-704B1_PV", 
         "LIT704B1" = "99_99619_LCI07_SE704B_LIT-704B1_PV", 
         "LCV704B1" = "99_99619_LCI07_P701G_LCV-704B1_PV", 
         "FIT704B1" = "99_99619_LCI07_SE704B_FIT-704B1_PV",
         "LIT704B2" = "99_99619_LCI07_SE704B_LIT-704B2_PV",
         "FCV704B3" = "99_99619_LCI07_SE704B_FCV-704B3_ZT",
         "PIT704B2" = "99_99619_LCI07_SE704B_PIT-704B2_PV",
         "LIT701A" = "99_99619_LCI07_T701_LIT-701A_PV")%>%
  mutate(date = as.POSIXct(strptime(date, "%d-%m-%Y %H:%M:%S")))

# Se guarda la media y la desviación estándar del flujo para devolver la normalización
m<-mean(df_ent$FIT704B1)
s<-sd(df_ent$FIT704B1)
m1<-mean(df_ent$LCV704B1)
s1<-sd(df_ent$LCV704B1)

# Función para la normalización de las variables
col_n<-function(colu){
  m<-mean(colu)
  s<-sd(colu)
  out<-(colu-m)/s
  return(out)
}
nor<-function(df){
  for(i in seq_along(df)){
    df[[i]]<-col_n(df[[i]])
  }
  return(df)
}

#Se elimina la columna de tiempo y la de incluido, en este ejercicio no vamos a tener en cuenta el desarrollo temporal
df_norm<-nor(df_ent%>%select(-date, -included))

# Creación del conjunto de entrenamiento y prueba
r<-c(1:nrow(df_norm))
n<-round(0.2*nrow(df_norm))
vec_te<-sample(r, n)
df_tr<-df_norm[-vec_te,]
df_te<-df_norm[vec_te,]


# Entrenamiento de la regresión lineal
mod_l<-lm(FIT704B1 ~ ., data = df_tr)
summary(mod_l)
vif(mod_l)
# elemento con menos significancia es PIT704B1 y el que mas aporta a la inflación de la varianza, se elimina del modelo
mod_l<-lm(FIT704B1 ~ .-PIT704B1, data = df_tr)
summary(mod_l)
vif(mod_l)
# el siguiente es LIT704B1, se elimina del modelo
mod_l<-lm(FIT704B1 ~ .-PIT704B1-LIT704B1, data = df_tr)
summary(mod_l)
vif(mod_l)
# sigue el PIT704B3 en inflación de varianza, así que lo eliminamos también
mod_l<-lm(FIT704B1 ~ .-PIT704B1-LIT704B1-PIT704B3, data = df_tr)
summary(mod_l)
vif(mod_l)
# el menos significativo de los que quedan son LIT704B1, FCV704B3 y LIT701A, se eliminan
mod_l<-lm(FIT704B1 ~ .-PIT704B1-LIT704B1-PIT704B3-LIT704B2-FCV704B3-LIT701A, data = df_tr)
summary(mod_l)
plot(mod_l, which = 1)

# Existe una pequeña proporción de función no lineal, se usa el modelo previo con factores no lineales
library(splines)
mod_nl<-lm(FIT704B1 ~ ns(PIT704B2, 4)+ns(LCV704B1, 4), data = df_tr)
summary(mod_nl)
plot(mod_nl, which = 1)
#Se observa una mejora en los residuales y también en el R-squared

# Observemos el comportamiento en el conjunto de entrenamiento:
plot(1:nrow(df_tr), df_tr$FIT704B1*s+m, type = "l", main = "Predicción en el conjunto de entrenamiento",
     xlab = "Número de muestra", ylab = "Flujo [BPD]")
lines(1:nrow(df_tr), mod_nl$fitted.values*s+m, col = "red")

# Ahora se observa el comportamiento en el conjunto de prueba:
pred_m_nl<-predict(mod_nl, newdata = df_te)
plot(1:nrow(df_te), df_te$FIT704B1*s+m, type = "l", main = "Predicción en el conjunto de prueba",
     xlab = "Número de muestra", ylab = "Flujo [BPD]")
lines(1:nrow(df_te), pred_m_nl*s+m, col = "red")

# Transformación de variable, que pasa si introducimos una nueva variable que sea la diferencia de presión entre el separador y el tanque?

rho<-61.856                   # densidad del fluido [lb/ft³]
Gf<-rho/62.4                  # Gravedad especifica del fluido [-]
R<-3.25                       # Radio del Separador SE704B [ft]
w<-22                         # Longitud del Separador SE704B [ft]
g<-32.17                      # Constante gravitacional [ft/s²]
K1 <- 0.000215839931747008    # Constante de conversión de lb/(ft*s²) a PSIG
K2 <- 0.133681                # Constante de conversión de GAL a ft³

df_ent1<-df_ent%>%
  mutate(DP = PIT704B2-K1*rho*g*LIT701A)

m2<-mean(df_ent1$DP)
s2<-sd(df_ent1$DP)

df_norm1<-nor(df_ent1%>%select(-date, -included, -PIT704B2, -LIT701A, -LIT704B1, -PIT704B1, -LIT704B2, -FCV704B3, -PIT704B3))

# Creación del conjunto de entrenamiento y prueba
df_tr1<-df_norm1[-vec_te,]
df_te1<-df_norm1[vec_te,]

# Entrenamiento del modelo
mod_l1<-lm(FIT704B1 ~ . , data = df_tr1)
summary(mod_l1)
plot(mod_l1, which = 1)

# Entrenamiento de modelo no lineal con el DP
mod_nl1<-lm(FIT704B1 ~ ns(DP, 4)+ns(LCV704B1, 4), data = df_tr1)
summary(mod_nl1)
plot(mod_nl1, which = 1)

# Observemos el comportamiento en el conjunto de entrenamiento:
plot(1:nrow(df_tr1), df_tr1$FIT704B1*s+m, type = "l", main = "Predicción en el conjunto de entrenamiento reg lineal",
     xlab = "Número de muestra", ylab = "Flujo [BPD]")
lines(1:nrow(df_tr1), mod_nl1$fitted.values*s+m, col = "red")

# Ahora se observa el comportamiento en el conjunto de prueba:
pred_m_nl1<-predict(mod_nl1, newdata = df_te1)
plot(1:nrow(df_te1), df_te1$FIT704B1*s+m, type = "l", main = "Predicción en el conjunto de prueba reg lineal",
     xlab = "Número de muestra", ylab = "Flujo [BPD]")
lines(1:nrow(df_te1), pred_m_nl1*s+m, col = "red")

# como es la función flujo de salida vs porcentaje de entrada?
# primero se genera el dataframe de datos de entrada, recordemos que la curva de cv es cuando hay una diferencia de 1 PSI entre la entrada y la
# salida

nd<-tibble(LCV704B1 = (seq(0, 100, 0.1)-m1)/s1, DP =(1-m2)/s2)
pred_m_nl2<-predict(mod_nl1, newdata = nd, interval = "prediction")
#convertimos el flujo a galones por minuto
pred_m_nl2<-42*(pred_m_nl2*s+m)/(24*60)
plot(seq(0,100, 0.1), pred_m_nl2[, 1], type = "l", main = "Coef de flujo Vs % Apertura", xlab = "% Apertura", ylab = "CV")
lines(seq(0,100, 0.1), pred_m_nl2[, 2], col = "red")
lines(seq(0,100, 0.1), pred_m_nl2[, 3], col = "red")
range(df_ent$LCV704B1)

# Creación de las matrices para la red neuronal
df_tr_d<-as.matrix(select(df_tr, -FIT704B1))
df_tr_l<-as.matrix(select(df_tr, FIT704B1))
df_te_d<-as.matrix(select(df_te, -FIT704B1))
df_te_l<-as.matrix(select(df_te, FIT704B1))

mod_n<-keras_model_sequential()
mod_n%>%
  layer_dense(units = 10, activation = 'relu', input_shape = c(1*8)) %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 1)

mod_n %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )

mod_n %>% fit(x = df_tr_d, y = df_tr_l, epochs = 100, verbose = 2)
score <- mod_n %>% evaluate(df_te_d, df_te_l, verbose = 0)
# Comportamiento con el conjunto de entrenamiento
pred_n <- mod_n%>%
  predict(df_tr_d)
pred_n1<-pred_n*s+m
plot(1:nrow(df_tr), df_tr$FIT704B1*s+m, type = "l", main = "Predicción en el conjunto de entrenamiento NN y LR",
     xlab = "Número de muestra", ylab = "Flujo [BPD]")
lines(1:nrow(df_tr), pred_n1, col = "blue")
lines(1:nrow(df_tr), mod_nl1$fitted.values*s+m, col = "red")

# Comportamiento con el conjunto de prueba
pred_n2 <- mod_n%>%
  predict(df_te_d)
pred_n2<-pred_n2*s+m
plot(1:nrow(df_te), df_te$FIT704B1*s+m, type = "l", main = "Predicción en el conjunto de prueba NN y LR",
     xlab = "Número de muestra", ylab = "Flujo [BPD]")
lines(1:nrow(df_te), pred_n2, col = "blue")
lines(1:nrow(df_te), pred_m_nl1*s+m, col = "red")
