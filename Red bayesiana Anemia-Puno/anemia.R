
library(dplyr)
library(readr)
library(writexl)
library(bnlearn)
library(Rgraphviz)

df <- read_csv("infantes.csv")

# Seleccionar columnas útiles

df <- df %>%
  select(Sexo, Edad, Peso, Talla, Altitud_Loc, SIS, JUNTOS, PIN,
         N_Sachets, N_Consejerias, Hbc, Dx_anemia)

# Categorizar Edad

df <- df %>%
  mutate(
    Edad_cat = case_when(
      Edad <= 6  ~ "0-6m",
      Edad <= 12 ~ "7-12m",
      Edad <= 24 ~ "1-2a",
      Edad <= 60 ~ "2-5a",
      TRUE       ~ ">5a"
    )
  )


# Categorizar si recibió suplemento

df <- df %>%
  mutate(
    N_Sachets = as.numeric(N_Sachets),
    N_Sachets = ifelse(is.na(N_Sachets), 0, N_Sachets),  # NA = 0 sachets
    Recibio_suplemento = ifelse(N_Sachets == 0, "No", "Si")
  )

#  Categorizar Hemoglobina (Hbc)

df <- df %>%
  filter(!is.na(Hbc), !is.na(Edad)) %>%
  mutate(
    Hbc_cat = case_when(
      Hbc < 7 ~ "Severa",
      Hbc < 10 ~ "Moderada",
      Hbc < 11 ~ "Leve",
      TRUE ~ "Normal"
    )
  )

# Categorizar sachets en niveles

df <- df %>%
  mutate(
    Sachets_cat = case_when(
      N_Sachets == 0 ~ "0 (Ninguno)",
      N_Sachets <= 10 ~ "Bajo",
      N_Sachets <= 20 ~ "Medio",
      TRUE ~ "Alto"
    )
  )

#Variable binaria para anemia

df <- df %>%
  mutate(
    Tiene_anemia = ifelse(grepl("Anemia", Dx_anemia) & !grepl("Normal", Dx_anemia), "Si", "No")
  )


# Calcular IMC y categorizar

df <- df %>%
  mutate(
    Talla_m = Talla / 100,
    IMC = Peso / (Talla_m^2),
    IMC_cat = case_when(
      IMC < 14 ~ "Bajo peso",
      IMC <= 17 ~ "Normal",
      TRUE ~ "Sobrepeso"
    )
  )


# Exportar dataset limpio

write.csv(df, "anemia_limpio.csv", row.names = FALSE, fileEncoding = "UTF-8")

#  Preparar dataset para Red Bayesiana

df_bayes <- df %>%
  select(Sexo, Edad_cat, Hbc_cat, Sachets_cat, Tiene_anemia,
         SIS, JUNTOS, PIN, IMC_cat)

# Asegurar factores con niveles explícitos
df_bayes <- df_bayes %>%
  mutate(
    Sexo = factor(Sexo, levels = c("F", "M")),
    Edad_cat = factor(Edad_cat, levels = c("0-6m", "7-12m", "1-2a", "2-5a", ">5a")),
    Hbc_cat = factor(Hbc_cat, levels = c("Severa", "Moderada", "Leve", "Normal")),
    Sachets_cat = factor(Sachets_cat, levels = c("0 (Ninguno)", "Bajo", "Medio", "Alto")),
    Tiene_anemia = factor(Tiene_anemia, levels = c("No", "Si")),
    SIS = factor(SIS, levels = c("No", "Si")),
    JUNTOS = factor(JUNTOS, levels = c("No", "Si")),
    PIN = factor(PIN, levels = c("No", "Si")),
    IMC_cat = factor(IMC_cat, levels = c("Bajo peso", "Normal", "Sobrepeso"))
  )

# Eliminar filas con valores faltantes
df_bayes <- na.omit(df_bayes)

# Verificar estructura final
str(df_bayes)

############################
# Convertir a data.frame clásico y eliminar niveles no usados
df_bayes <- as.data.frame(df_bayes)
df_bayes <- droplevels(df_bayes)

sapply(df_bayes, function(x) length(unique(x)))

# ====================================
# PASO  Red Bayesiana

set.seed(123)
red <- hc(df_bayes)              # Estructura
ajuste <- bn.fit(red, data = df_bayes)  # Parámetros

# Visualizar red
graphviz.plot(red, main = "Red Bayesiana: Anemia en niños", layout = "dot")

# Ver tabla CPT del nodo objetivo
print(ajuste$Tiene_anemia)



#############   PARA NETICA  ############

df <- read_csv("anemia_limpio.csv")

# Crear dataset sólo con variables categóricas importantes

df_netica <- df %>%
  select(Sexo, Edad_cat, Hbc_cat, Sachets_cat, Tiene_anemia,
         SIS, JUNTOS, PIN, IMC_cat)

# Codificar categorías como números

df_netica_cod <- df_netica %>%
  mutate(
    Sexo = ifelse(Sexo == "F", 0, 1),
    Edad_cat = case_when(
      Edad_cat == "0-6m"  ~ 1,
      Edad_cat == "7-12m" ~ 2,
      Edad_cat == "1-2a"  ~ 3,
      Edad_cat == "2-5a"  ~ 4,
      Edad_cat == ">5a"   ~ 5
    ),
    Hbc_cat = case_when(
      Hbc_cat == "Severa"   ~ 1,
      Hbc_cat == "Moderada" ~ 2,
      Hbc_cat == "Leve"     ~ 3,
      Hbc_cat == "Normal"   ~ 4
    ),
    Sachets_cat = case_when(
      Sachets_cat == "0 (Ninguno)" ~ 1,
      Sachets_cat == "Bajo"        ~ 2,
      Sachets_cat == "Medio"       ~ 3,
      Sachets_cat == "Alto"        ~ 4
    ),
    Tiene_anemia = ifelse(Tiene_anemia == "No", 0, 1),
    SIS = ifelse(SIS == "No", 0, 1),
    JUNTOS = ifelse(JUNTOS == "No", 0, 1),
    PIN = ifelse(PIN == "No", 0, 1),
    IMC_cat = case_when(
      IMC_cat == "Bajo peso"  ~ 1,
      IMC_cat == "Normal"     ~ 2,
      IMC_cat == "Sobrepeso"  ~ 3
    )
  )

# Exportar 
write.csv(df_netica_cod, "anemia_codificada.csv", row.names = FALSE)

