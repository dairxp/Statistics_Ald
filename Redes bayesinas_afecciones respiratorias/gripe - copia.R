####################   CASO AFECCIONES RESPIRATORIAS     ########

library(dplyr)
library(readxl)

gripe <- read_excel("D:/A ARCHIVOS UNA/7 semestre/Estadistica Bayesiana/gripe.xlsx")

# Seleccionar y renombrar las columnas
data <- gripe %>%
  select(temperatura, frecuencia_cardíaca, oxigeno, glicemia, tensión, etiqueta) %>%
  rename(T = temperatura, F = frecuencia_cardíaca, O = oxigeno, G = glicemia, E = tensión, S = etiqueta)

str(data)

head(data)
# Verificar si hay valores faltantes
summary(data)

#######

#extreme_values <- data %>% filter(T > 500)
#extreme_values

# Mostrar las filas que cumplen con las condiciones especificadas
#extreme_values <- data %>%
  #filter(T > 45 | F > 100 | O > 120 | G > 120)

#print(extreme_values)

# Eliminar las filas que cumplen con las condiciones especificadas
#data <- data %>%
  #filter(!(T > 45 | F > 100 | O > 120 | G > 120))

#summary(data)

########
# Definir rangos razonables para cada variable
range_T <- c(35, 45)      # Rango para temperatura
range_F <- c(50, 100)     # Rango para frecuencia cardíaca
range_O <- c(90, 120)     # Rango para oxígeno
range_G <- c(70, 120)     # Rango para glicemia

# Función para reemplazar valores extremos con valores aleatorios dentro de un rango dado
replace_extreme <- function(value, range) {
  if (is.na(value) || value < range[1] || value > range[2]) {
    return(runif(1, min = range[1], max = range[2])) # Genera un valor aleatorio en el rango
  }
  return(value)
}

# Aplicar la función para reemplazar valores extremos
data <- data %>%
  mutate(
    T = sapply(T, replace_extreme, range = range_T),
    F = sapply(F, replace_extreme, range = range_F),
    O = sapply(O, replace_extreme, range = range_O),
    G = sapply(G, replace_extreme, range = range_G)
  )

# Verificar el resultado después del reemplazo
summary(data)


####################
#######################

# Eliminar filas con valores faltantes si es necesario
data <- na.omit(data)
summary(data)
head(data)
############   TEMEPRATURA    ###############     #como "Normal" o "Alta"
data$T <- ifelse(data$T < 37.2, "normal","alta")
head(data)

#factor numérico 
data$T <- as.numeric(factor(data$T, levels = c("normal", "alta")))  
head(data)

##########   Frecuencia CArdica   #############       como "Normal", "Baja" o "Alta"
data$F <- ifelse(data$F < 60, "Baja", ifelse(data$F < 100, "Normal", "Alta"))
head(data)

# factor numérico 
data$F <- as.numeric(factor(data$F, levels = c("Baja", "Normal", "Alta")))
head(data)

###########   OXIGENO   ########            "Normal", "Baja" o "Críticamente baja"
data$O <- ifelse(data$O < 90, "Críticamente baja", ifelse(data$O < 95, "Baja", "Normal"))
head(data)

# factor numérico 
data$O <- as.numeric(factor(data$O, levels = c("Críticamente baja", "Baja", "Normal")))
head(data)

###########   Glicerina     #############  como "Normal", "Baja" o "Alta"
data$G <- ifelse(data$G < 70, "Baja", ifelse(data$G < 99, "Normal", "Alta"))
head(data)

#factor numérico 
data$G <- as.numeric(factor(data$G, levels = c("Baja", "Normal", "Alta")))
head(data)

 
############## tensión  ########### (tensión) como "Baja", "Normal", "Elevada" o "Alta"
data$E <- ifelse(data$E < 90, "Baja",
                 ifelse(data$E < 120, "Normal",
                        ifelse(data$E <= 129, "Elevada", "Alta")))
head(data)

# factor numérico 
data$E <- as.numeric(factor(data$E, levels = c("Baja", "Normal", "Elevada", "Alta")))
head(data)


############################################################
# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Eliminar filas con valores faltantes si es necesario
data <- na.omit(data)

# Convertir las variables a factores con etiquetas respectivas
data$T <- factor(data$T, levels = c(1, 2), labels = c("normal", "alta"))
data$F <- factor(data$F, levels = c(1, 2, 3), labels = c("Baja", "Normal", "Alta"))
data$O <- factor(data$O, levels = c(1, 2, 3), labels = c("Críticamente baja", "Baja", "Normal"))
data$G <- factor(data$G, levels = c(1, 2, 3), labels = c("Baja", "Normal", "Alta"))
data$E <- factor(data$E, levels = c(1, 2, 3, 4), labels = c("Baja", "Normal", "Elevada", "Alta"))
data$S <- factor(data$S, levels = c(0, 1), labels = c("Enfermo", "Sano"))

# Histograma de Temperatura
ggplot(data, aes(x = T)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Histograma de Temperatura", x = "Temperatura", y = "Frecuencia") +
  theme_minimal()

# Histograma de Frecuencia Cardíaca
ggplot(data, aes(x = F)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Histograma de Frecuencia Cardíaca", x = "Frecuencia Cardíaca", y = "Frecuencia") +
  theme_minimal()

# Histograma de Oxígeno
ggplot(data, aes(x = O)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Histograma de Oxígeno", x = "Oxígeno", y = "Frecuencia") +
  theme_minimal()

# Histograma de Glicemia
ggplot(data, aes(x = G)) +
  geom_bar(fill = "lightgoldenrodyellow", color = "black") +
  labs(title = "Histograma de Glicemia", x = "Glicemia", y = "Frecuencia") +
  theme_minimal()

# Histograma de Tensión
ggplot(data, aes(x = E)) +
  geom_bar(fill = "lightpink", color = "black") +
  labs(title = "Histograma de Tensión", x = "Tensión", y = "Frecuencia") +
  theme_minimal()

# Histograma de Etiqueta (S)
ggplot(data, aes(x = S)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Histograma de Etiqueta", x = "Etiqueta", y = "Frecuencia") +
  theme_minimal()

##########################


##############################################

data <- na.omit(data)
summary(data)
head(data)

#Guaradar la data en cvs
write.csv(data, file = "t.csv",row.names = FALSE)

#conocer tu ubicacion de trabajo
getwd()
