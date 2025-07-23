ventanas_trayecto <- function(datos_trayecto, Fs = 32, ventana_seg = 60) {
  ventanas <- datos_trayecto |> 
    mutate(
      across(c(AccX, AccY, AccZ), 
             ~ approx(relative_time, .x, xout = relative_time, method = "linear")$y)
    ) |>
    na.omit() |> 
    mutate(
      relative_time_s = relative_time / 1000,
      AccT = sqrt(AccX^2 + AccY^2 + AccZ^2)
    ) |> 
    select(relative_time_s, AccT) |>  
    with({
      time <- seq(min(relative_time_s), max(relative_time_s), by = 1/Fs)
      tibble(time = time, AccT = approx(relative_time_s, AccT, xout = time, method = "linear")$y)
    }) |> 
    mutate(ventana = floor(time / ventana_seg)) |> 
    group_by(ventana) |> 
    group_modify(~ {
      n <- nrow(.x)
      fft_result <- fft(.x$AccT)
      amplitud <- abs(fft_result[1:(n/2 + 1)])
      frecuencias <- seq(0, Fs/2, length.out = n/2 + 1)
      
      mascara_rango <- frecuencias > 0.5 & frecuencias < 3
      indice_pico <- which.max(amplitud[mascara_rango])
      
      tibble(
        freq_dominante = frecuencias[mascara_rango][indice_pico],
        amplitud_pico = amplitud[mascara_rango][indice_pico]
      )
    })
  
  return(ventanas)
}


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
df <- ventanas_trayecto(datos$trayecto_2, ventana_seg = 60)

actividades <- tibble::tibble(
  actividad = c("quieto", "metro", "metroplus", "caminando", "bus"),
  ymin = c(0.000, 0.583, 0.992, 0.883, 1.16),
  ymax = c(0.767, 1.15, 1.25, 2.00, 2.20),
  fill_color = c("gray90", "lightblue", "lightgreen", "khaki", "lightcoral"),
  line_color = c("gray50", "blue4", "green4", "goldenrod4", "firebrick"),
  label = paste0(actividad, " (", ymin, "-", ymax, " Hz)")
)

ggplot(df) +
  # 1. Franjas de fondo (más claras que los recuadros)
  geom_rect(data = actividades,
            aes(ymin = ymin, ymax = ymax, xmin = -Inf, xmax = Inf, fill = actividad),
            alpha = 0.3) +  # Reducida transparencia para mayor contraste
  
  # 2. Líneas punteadas
  geom_hline(data = actividades,
             aes(yintercept = ymin, color = actividad), 
             linetype = "dashed", linewidth = 0.6) +
  geom_hline(data = actividades,
             aes(yintercept = ymax, color = actividad), 
             linetype = "dashed", linewidth = 0.6) +
  
  # 3. Línea principal
  geom_line(aes(x = ventana, y = freq_dominante), 
            linewidth = 0.8, color = "black") +
  
  # 4. Etiquetas con fondo más oscuro (nivel "white smoke")
  geom_label(data = actividades,
             aes(x = max(df$ventana) * 1.05,
                 y = (ymin + ymax)/2, 
                 label = label),
             hjust = 0, 
             vjust = 0.5, 
             size = 3.2,
             fontface = "bold",
             color = "black",                   # Texto negro
             fill = "white",                   # Fondo blanco
             label.size = 0.3,                  # Borde más fino
             label.padding = unit(0.35, "lines"), # Espacio interno ajustado
             alpha = 0.9) +                     # Menos transparencia
  
  # 5. Fondo del panel más oscuro (gris muy claro)
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#f5f5f5", color = NA),  # Fondo general gris claro
    panel.background = element_rect(fill = "#fafafa", color = NA),  # Área del gráfico más clara
    
    # [Mantener el resto de configuraciones de tema anteriores...]
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, color = "gray50", size = 9),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(15, 80, 15, 15)
  ) +
  
  # [Mantener escalas y demás configuraciones anteriores...]
  scale_fill_manual(values = setNames(actividades$fill_color, actividades$actividad)) +
  scale_color_manual(values = setNames(actividades$line_color, actividades$actividad)) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.2))) +
  
  labs(
    title = "Análisis de Frecuencia Dominante en Trayecto 2",
    subtitle = "Segmentado en ventanas temporales de 60 segundos",
    y = "Frecuencia Dominante (Hz)", 
    x = "Número de Ventana Temporal",

    )
  )

ggsave(
  filename = "analisis_frecuencia_trayecto2.png",
  plot = last_plot(),  # Guarda el último gráfico creado
  device = "png",
  width = 9,          # Ancho en pulgadas (ajustar según necesidad)
  height = 7,          # Alto en pulgadas
  units = "in",        # Unidades en pulgadas
  dpi = 300,           # Resolución para publicaciones
  bg = "#f5f5f5"       # Mismo color del fondo del gráfico
)

