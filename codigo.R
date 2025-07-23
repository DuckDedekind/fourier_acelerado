setwd(r"(C:\Users\Brian\Desktop\project_banana)")
rm(list = ls(all.names = TRUE))
invisible(gc())

require(tidyverse)
require(purrr)
require(ggplot2)

datos <- list.files(path = "datos/", pattern = "\\.csv$", full.names = TRUE) |> 
  set_names(~ str_remove(basename(.), "\\.csv$")) |> 
  map(read_csv)

analizar_actividad <- function(datos, 
                               Fs, 
                               ventana_seg = 60, 
                               rango_frec = c(0.5, 3)) {

  resultados <- datos |> 
    mutate(
      across(c(AccX, AccY, AccZ), 
             ~ approx(relative_time, .x, xout = relative_time, method = "linear")$y
      )
    ) |> 
    na.omit() |> 
    mutate(
      relative_time_s = relative_time / 1000,
      AccT = sqrt(AccX^2 + AccY^2 + AccZ^2)
    ) %>%
    select(relative_time_s, AccT) |> 
    with({
      tiempo_uniforme <- seq(min(relative_time_s), max(relative_time_s), by = 1/Fs)
      tibble(
        time = tiempo_uniforme,
        AccT = approx(relative_time_s, AccT, xout = tiempo_uniforme, method = "linear")$y
      )
    }) |> 
    mutate(
      ventana = floor(time / ventana_seg)
    ) |> 
    group_by(ventana) |> 
    group_modify(~ {
      n <- nrow(.x)
      fft_result <- fft(.x$AccT)
      amplitud <- abs(fft_result[1:(n/2 + 1)])
      frecuencias <- seq(0, Fs/2, length.out = n/2 + 1)
            mascara_rango <- frecuencias > rango_frec[1] & frecuencias < rango_frec[2]
      if (sum(mascara_rango) == 0) {
        return(tibble(freq_dominante = NA_real_, amplitud_pico = NA_real_))
      }
      
      indice_pico <- which.max(amplitud[mascara_rango])
      tibble(
        freq_dominante = frecuencias[mascara_rango][indice_pico],
        amplitud_pico = amplitud[mascara_rango][indice_pico]
      )
    }) |> 
    ungroup() |> 
    summarise(
      freq_Q1 = quantile(freq_dominante, 0.25, na.rm = TRUE),
      mediana = quantile(freq_dominante, 0.50, na.rm = TRUE),
      freq_Q3 = quantile(freq_dominante, 0.75, na.rm = TRUE),
      n_ventanas = sum(!is.na(freq_dominante))
    )
  
  return(resultados)
}

analizar_actividad(datos$caminando, Fs = 32, ventana_seg = 60)


datos[!names(datos) %in% 
        c("trayecto_1", "trayecto_2")] |> 
  imap_dfr(~ {
    analizar_actividad(.x, Fs = 32, ventana_seg = 60) |> 
      mutate(actividad = .y, .before = 1) 
  })
























# Análisis de todas las actividades (excluyendo trayectos)
resultados_actividades <- datos[!names(datos) %in% c("trayecto_1", "trayecto_2",
                                                     "escalas_bajando", "escalas_subiendo")] |> 
  imap_dfr(~ {
    # Analizar cada ventana de 60 segundos
    .x |> 
      mutate(
        across(c(AccX, AccY, AccZ), 
               ~ approx(relative_time, .x, xout = relative_time, method = "linear")$y
        )
      ) |> 
      na.omit() |> 
      mutate(
        relative_time_s = relative_time / 1000,
        AccT = sqrt(AccX^2 + AccY^2 + AccZ^2)
      ) %>%
      select(relative_time_s, AccT) |> 
      with({
        tiempo_uniforme <- seq(min(relative_time_s), max(relative_time_s), by = 1/32)
        tibble(
          time = tiempo_uniforme,
          AccT = approx(relative_time_s, AccT, xout = tiempo_uniforme, method = "linear")$y
        )
      }) |> 
      mutate(
        ventana = floor(time / 60)  # Ventanas de 60 segundos
      ) |> 
      group_by(ventana) |> 
      group_modify(~ {
        n <- nrow(.x)
        fft_result <- fft(.x$AccT)
        amplitud <- abs(fft_result[1:(n/2 + 1)])
        frecuencias <- seq(0, 32/2, length.out = n/2 + 1)
        mascara_rango <- frecuencias > 0.5 & frecuencias < 3
        if (sum(mascara_rango) == 0) {
          return(tibble(freq_dominante = NA_real_))
        }
        indice_pico <- which.max(amplitud[mascara_rango])
        tibble(freq_dominante = frecuencias[mascara_rango][indice_pico])
      }) |> 
      ungroup() |> 
      mutate(actividad = .y, .before = 1) |> 
      filter(!is.na(freq_dominante))
  })

# Crear boxplot profesional
ggplot(resultados_actividades, aes(x = actividad, y = freq_dominante, fill = actividad)) +
  geom_boxplot(
    alpha = 0.8,
    outlier.shape = 21,
    outlier.fill = "red",
    outlier.alpha = 0.6,
    notch = FALSE
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Distribución de Frecuencias Dominantes por Actividad",
    subtitle = "Análisis por ventanas de 60 segundos (excluyendo trayectos)",
    x = "Actividad",
    y = "Frecuencia dominante (Hz)",
    caption = "Datos procesados con FFT en ventanas de 1 minuto | Rango 0.5-3 Hz"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Actividad") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_flip()  # Para mejor visualización de nombres de actividades

# Opcional: Guardar el gráfico
ggsave("boxplot_actividades.png", width = 8, height = 5, dpi = 300)

