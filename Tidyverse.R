library(tidyverse)

# Bases de datos --> Data frame

# Readr -------------------------------------------------------------------


df_total <- read_csv("https://raw.githubusercontent.com/azulchinca98/Clase-econometria-II/main/df_total.csv")

view(df_total)

colnames(df_total)

# Pregunta general
# ¿Cuales son los lenguajes de programacion asociados a los mejores salarios?


# DYPLR -------------------------------------------------------------------

# Manipulacion y transformacion de datos


# Seleccionamos columnas relevantes (select)

df <- df_total %>% 
  select(id, work_province, work_dedication, salary_monthly_BRUTO, salary_monthly_NETO,
           profile_studies_level, profile_age, profile_gender, tools_programming_languages, 
         lenguaje_programacion, profile_years_experience)


# Renombramos variables (rename)

df1 <- df %>% 
  rename(programming_language = lenguaje_programacion)

colnames(df)

# Filtramos (filter). ¿Todos trabajan la misma cantidad de horas?

df2 <- df %>% 
  filter(work_dedication == "Full-Time")

# Creacion de nueva columna (mutate)

df3 <- df2 %>% 
  mutate(neto_mayor_bruto = if_else(salary_monthly_BRUTO<salary_monthly_NETO, 1, 0)) 

df4 <- df3 %>% 
  summarise(suma_inconsistencias = sum(neto_mayor_bruto))
df4


df5 <- df3 %>% 
  filter(neto_mayor_bruto == 0)

# Hay formas más eficientes de observar estos casos raros sin usar tidyverse!


sum(df_total$salary_monthly_NETO>df_total$salary_monthly_BRUTO)


sum(df[df$work_dedication == 'Full-Time',]$salary_monthly_NETO > 
      
      df[df$work_dedication == 'Full-Time',]$salary_monthly_BRUTO)


# Podemos combinar para armar un código más eficiente --> ANIDAR FUNCIONES!

df <- df_total %>% 
  select(id, work_province, work_dedication, salary_monthly_BRUTO, salary_monthly_NETO,
         profile_studies_level, profile_age, profile_gender, tools_programming_languages, 
         lenguaje_programacion, profile_years_experience) %>% 
  
  rename(programming_language = lenguaje_programacion) %>% 
  
  filter(work_dedication == "Full-Time" , 
         salary_monthly_BRUTO >= salary_monthly_NETO)  # Permitimos casos iguales       
         

rm(df1, df2, df3, df4, df5)



# Combinacion GROUP BY - COUNT/SUMMARISE ----------------------------------------


# Cuantos lenguajes saben en promedio las personas?

# Cada persona aparece tantas veces como lenguajes sabe. 

cant_len <- df %>% 
  group_by(id) %>% 
  count()

df_len <- df %>% 
  left_join(cant_len, by = "id") # tambien existe inner_join, right join


df_len2 <- df %>% 
  group_by(id) %>% 
  mutate(n = n() )


identical(df_len, df_len2)

df_len <- as.data.frame(df_len)

df_len2 <- as.data.frame(df_len2)

identical(df_len, df_len2)

rm(df_len2)


len_populares  <- df_len %>% 
  rename(cantidad_lenguajes = n) %>% 
  filter(cantidad_lenguajes == 1) %>% 
  group_by(programming_language) %>% 
  count() %>% 
  arrange(-n)

len_populares <- len_populares[2:11,]  # filtro los primeros 10 lenguajes mas populares

# Identificar cuándo es más eficiente no usar tidyverse


lenguajes_populares <- len_populares$programming_language

lenguajes_populares


df2 <- df_len %>% 
  rename(cantidad_lenguajes = n) %>% 
  filter(cantidad_lenguajes == 1 & programming_language %in% lenguajes_populares) 


salario_len_populares <- df2 %>% 
  group_by(programming_language) %>% 
  summarise(media_salario = mean(salary_monthly_BRUTO))%>% 
  mutate(across(-1, ~ as.integer(round(. / 1000, digits = 0)))) %>% 
  arrange(-media_salario)
salario_len_populares

salario_len_populares <- as.data.frame(salario_len_populares)


# Case_when

table(df2$profile_gender)

Mujer <- c("Mujer", "Mujer cis", "Mujer Cis")

Varon <- c("Varón", "Varón Cis")


df2 <- df2 %>% 
  mutate(gender = case_when(profile_gender %in% Mujer ~ "Mujeres",
                            profile_gender %in% Varon ~ "Varones",
                            !profile_gender %in% c(Varon, Mujer)~ "Diversidades"))


salario_len_populares_genero <- df2 %>% 
  group_by(programming_language, gender) %>% 
  summarise(media_salario = mean(salary_monthly_BRUTO, rm.na =T)) 


# TIDYR -------------------------------------------------------------------

# pivot_wider and pivot_longer


salario_len_populares_genero <- salario_len_populares_genero %>% 
  pivot_wider(names_from = "gender", values_from = "media_salario")



# PURR --------------------------------------------------------------------

# map function

df_mujeres <- df2 %>% 
  filter(gender == "Mujeres")

df_varones <- df2 %>% 
  filter(gender == "Varones")

lista <- list(df_mujeres, df_varones)


salarios_provincia <- map(lista, ~group_by(., programming_language, work_province) %>% 
                            summarise(media_salario = mean(salary_monthly_BRUTO, rm.na =T)) %>% 
                            pivot_wider(names_from = "work_province", 
                                        values_from = "media_salario"))

mujeres_salario <- salarios_provincia[[1]]



# GGPLOT ------------------------------------------------------------------



library(esquisse)

# Interfaz gráfica que se integra con GGPLOT2
esquisser(
  data = NULL,
  controls = c("labs", "parameters", "appearance", "filters", "code"),
  viewer = getOption(x = "esquisse.viewer", default = "dialog"))


esquisser(
  data = NULL,
  controls = c("labs", "parameters", "appearance", "filters", "code"),
  viewer = "browser")


salario_len_populares <- salario_len_populares %>%
  mutate(programming_language = str_to_title(programming_language),
         programming_language = factor(programming_language, levels = unique(programming_language)))




ggplot(salario_len_populares) +
  aes(
    x = programming_language,
    y = media_salario,
    fill = media_salario
  ) +
  geom_col() +

  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    y = "Salario bruto promedio (en miles)",
    x = "Lenguaje de programación",
    title = "Salario promedio de los lenguajes de programación más populares",
    fill = "Salario medio"
  ) +
  scale_x_discrete(limits = rev(levels(salario_len_populares$programming_language)))


