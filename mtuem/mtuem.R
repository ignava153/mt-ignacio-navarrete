# ===============================================================
# V3 - MTUEM CONTINUO Y ANALISIS DEL VALOR DEL OCIO POR GRUPOS
#
# Archivo obligatorio de entrada:
#   enut_ii.xlsx
#   hoja: enut_ii
#
# Archivo opcional de entrada para clusters modales, si enut_ii.xlsx no
# trae la columna cluster_modo:
#   resultados_clusters_modo_k4_sociodemografico.xlsx
#   hoja: 10_asignacion_persona
#
# Objetivo:
#   1) Reproducir de forma autosuficiente el MTUEM V2 corregido.
#   2) Estimar nuevamente el MTUEM para submuestras por:
#        edad, sexo, ingreso, territorio, cluster de uso del tiempo,
#        cluster de modo de transporte.
#   3) Calcular VoL, VTAW y salario horario con intervalos de confianza.
#   4) Guardar resultados ordenados en carpetas, con tablas y graficos
#      inspirados en la forma de presentacion de Pablo Reyes.
#
# Decisiones metodologicas confirmadas:
#   - Se mantiene el MNL V9 como modelo discreto final.
#   - Se recodifican modos como primer paso:
#        ENUT original: 1 = transporte publico, 2 = auto/moto particular
#        Interno:       1 = auto/moto particular, 2 = transporte publico
#   - Se usa la muestra compatible con MNL V9 como base.
#   - Se mantiene la construccion corregida de Ec:
#        Ec = Ec_cuentas + Ec_hogar + Ec_salud + Ec_transporte + Ec_educacion
#             - ing_jub_aps - ing_gpp
#             + w_dc * (t_domestic_work + t_care_work)
#   - Se estima el mismo bloque continuo Tw y Tf1.
#   - Se reporta VoL predicho como indicador principal y VoL observado
#     como diagnostico.
#   - Para grupos se estima un modelo propio por submuestra.
#
# Salida:
#   prueba/v3 - mtuem/
# ===============================================================

rm(list = ls())

# ---------------------------------------------------------------
# 0. Directorio de trabajo y controles
# ---------------------------------------------------------------

directorio_trabajo <- "C:/Users/ignav/OneDrive/Desktop/UDCT/FINAL"
if (dir.exists(directorio_trabajo)) {
  setwd(directorio_trabajo)
}

archivo_base <- "enut_ii.xlsx"
hoja_base <- "enut_ii"

carpeta_salida <- file.path("prueba", "v3 - mtuem")

USAR_RDS_EXISTENTE <- FALSE
ITER_CONT_BFGS <- 30000
ITER_CONT_NM <- 60000
ITER_CONT_SANN <- 20000
MIN_N_GRUPO <- 100

# Si TRUE, intenta calcular IC delta si el objeto nmm trae matriz de covarianza o Hessiano.
# Si no esta disponible, el codigo usa IC de la media muestral de los valores individuales.
USAR_DELTA_SI_DISPONIBLE <- TRUE

set.seed(42)

# ---------------------------------------------------------------
# 1. Paquetes
# ---------------------------------------------------------------

paquetes <- c(
  "readxl", "dplyr", "tibble", "tidyr", "stringr", "readr", "writexl",
  "ggplot2", "purrr"
)

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

if (!requireNamespace("nmm", quietly = TRUE)) {
  if (file.exists("nmm_0.9.tar.gz")) {
    install.packages("nmm_0.9.tar.gz", repos = NULL, type = "source")
  } else {
    install.packages("nmm")
  }
}
library(nmm)

# ---------------------------------------------------------------
# 2. Funciones auxiliares generales
# ---------------------------------------------------------------

crear_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

num0 <- function(x) {
  dplyr::coalesce(num(x), 0)
}

limpiar_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  x[is.na(x)] <- ""
  x
}

nombre_archivo_seguro <- function(x) {
  x <- limpiar_txt(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(x == "", "sin_nombre", x)
}

recodificar_modo_auto_ref <- function(x) {
  x_num <- num(x)
  dplyr::case_when(
    x_num == 1 ~ 2,
    x_num == 2 ~ 1,
    x_num %in% 3:7 ~ x_num,
    TRUE ~ x_num
  )
}

extraer_loglik <- function(obj) {
  tryCatch(as.numeric(logLik(obj)), error = function(e) NA_real_)
}

extraer_code <- function(obj) {
  out <- tryCatch(obj$code, error = function(e) NA_real_)
  if (length(out) == 0 || is.null(out)) return(NA_real_)
  as.numeric(out[1])
}

modelo_loglik_ok <- function(obj) {
  is.finite(extraer_loglik(obj))
}

modelo_convergio <- function(obj) {
  if (is.null(obj)) return(FALSE)
  ll <- extraer_loglik(obj)
  if (!is.finite(ll)) return(FALSE)
  cd <- extraer_code(obj)
  if (is.na(cd)) return(TRUE)
  cd == 0
}

safe_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- NA_real_
  out
}

ci_media_t <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2) {
    return(tibble::tibble(media = mean(x, na.rm = TRUE), se = NA_real_, ic_inf = NA_real_, ic_sup = NA_real_, n_valido = n))
  }
  alpha <- 1 - conf
  media <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  crit <- stats::qt(1 - alpha / 2, df = n - 1)
  tibble::tibble(
    media = media,
    se = se,
    ic_inf = media - crit * se,
    ic_sup = media + crit * se,
    n_valido = n
  )
}

extraer_parametros <- function(modelo) {
  est_vec <- tryCatch(as.numeric(modelo$estimate), error = function(e) numeric(0))
  names_est <- tryCatch(names(modelo$estimate), error = function(e) NULL)

  if (length(est_vec) == 0) {
    return(tibble(parametro = character(), estimacion = numeric(), error_estandar = numeric(), t_value = numeric(), sig = character()))
  }

  if (is.null(names_est) || length(names_est) != length(est_vec)) {
    names_est <- paste0("par_", seq_along(est_vec))
  }

  se_vec <- tryCatch(as.numeric(modelo$se), error = function(e) numeric(0))
  if (length(se_vec) != length(est_vec)) {
    se_vec <- rep(NA_real_, length(est_vec))
  }

  t_vec <- est_vec / se_vec
  t_vec[!is.finite(t_vec)] <- NA_real_

  tibble::tibble(
    parametro = names_est,
    estimacion = est_vec,
    error_estandar = se_vec,
    t_value = t_vec,
    sig = dplyr::case_when(
      is.na(t_value) ~ "",
      abs(t_value) >= 3.291 ~ "***",
      abs(t_value) >= 2.576 ~ "**",
      abs(t_value) >= 1.960 ~ "*",
      abs(t_value) >= 1.645 ~ ".",
      TRUE ~ ""
    )
  )
}

obtener_vcov <- function(modelo) {
  # Intenta distintas rutas porque distintos objetos nmm pueden guardar
  # la matriz de covarianza con nombres distintos o no guardarla.
  vc <- tryCatch(stats::vcov(modelo), error = function(e) NULL)
  if (is.matrix(vc) && all(dim(vc) > 0)) return(vc)

  candidatos <- list(
    tryCatch(modelo$vcov, error = function(e) NULL),
    tryCatch(modelo$cov, error = function(e) NULL),
    tryCatch(modelo$covariance, error = function(e) NULL),
    tryCatch(modelo$varcov, error = function(e) NULL)
  )

  for (cand in candidatos) {
    if (is.matrix(cand) && all(dim(cand) > 0)) return(cand)
  }

  hess <- tryCatch(modelo$hessian, error = function(e) NULL)
  if (is.matrix(hess) && nrow(hess) == ncol(hess)) {
    vc_h <- tryCatch(solve(-hess), error = function(e) NULL)
    if (is.matrix(vc_h) && all(dim(vc_h) > 0)) return(vc_h)
  }

  NULL
}

calcular_predicciones_con_parametros <- function(data_est, eq_Tw_rhs, par_vals) {
  df <- data_est
  for (p in names(par_vals)) {
    df[[p]] <- as.numeric(par_vals[p])
  }
  df$Tw_hat <- eval(parse(text = eq_Tw_rhs), envir = df)
  df$Tf1_hat <- df$th1 * (df$ta - df$Tw_hat - df$Tc)
  df$Tf2_hat <- (1 - df$th1) * (df$ta - df$Tw_hat - df$Tc)

  df$error_Tw <- df$Tw - df$Tw_hat
  df$error_Tf1 <- df$Tf1 - df$Tf1_hat
  df$error_Tf2 <- df$Tf2 - df$Tf2_hat

  df$den_VoL_predicho <- df$PH * (df$ta - df$Tw_hat - df$Tc)
  df$den_VoL_observado <- df$PH * (df$ta - df$Tw - df$Tc)

  df$VoL_predicho <- safe_div(df$w * df$Tw_hat - df$Ec, df$den_VoL_predicho)
  df$VoL_observado <- safe_div(df$w * df$Tw - df$Ec, df$den_VoL_observado)
  df$VTAW_predicho <- df$VoL_predicho - df$w
  df$VTAW_observado <- df$VoL_observado - df$w

  df$VoL_predicho_sobre_w <- safe_div(df$VoL_predicho, df$w)
  df$VTAW_predicho_sobre_w <- safe_div(df$VTAW_predicho, df$w)

  df$ingreso_libre_predicho <- df$w * df$Tw_hat - df$Ec
  df$cierre_temporal_predicho <- df$Tw_hat + df$Tf1_hat + df$Tf2_hat + df$Tc
  df$diferencia_cierre_predicho <- df$cierre_temporal_predicho - df$ta
  df
}

gradiente_numerico <- function(fun, par_vals) {
  p <- as.numeric(par_vals)
  names(p) <- names(par_vals)
  g <- rep(NA_real_, length(p))
  names(g) <- names(p)

  for (i in seq_along(p)) {
    h <- sqrt(.Machine$double.eps) * (abs(p[i]) + 1)
    p_up <- p
    p_dn <- p
    p_up[i] <- p_up[i] + h
    p_dn[i] <- p_dn[i] - h
    f_up <- tryCatch(fun(p_up), error = function(e) NA_real_)
    f_dn <- tryCatch(fun(p_dn), error = function(e) NA_real_)
    g[i] <- (f_up - f_dn) / (2 * h)
  }
  g
}

ic_delta_media_valor <- function(modelo, data_est, eq_Tw_rhs, indicador = c("VoL_predicho", "VTAW_predicho")) {
  indicador <- match.arg(indicador)
  if (!isTRUE(USAR_DELTA_SI_DISPONIBLE)) {
    return(tibble::tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }

  par_names <- c("PH", "tw", "th1")
  par_vals <- tryCatch(modelo$estimate[par_names], error = function(e) NULL)
  if (is.null(par_vals) || any(!is.finite(par_vals))) {
    return(tibble::tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }

  vc <- obtener_vcov(modelo)
  if (is.null(vc)) {
    return(tibble::tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }

  if (is.null(rownames(vc)) || is.null(colnames(vc))) {
    rownames(vc) <- names(modelo$estimate)
    colnames(vc) <- names(modelo$estimate)
  }

  if (!all(par_names %in% rownames(vc)) || !all(par_names %in% colnames(vc))) {
    return(tibble::tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }

  vc_sub <- vc[par_names, par_names, drop = FALSE]

  f_media <- function(par_vec) {
    names(par_vec) <- par_names
    pred <- calcular_predicciones_con_parametros(data_est, eq_Tw_rhs, par_vec)
    mean(pred[[indicador]], na.rm = TRUE)
  }

  est <- f_media(as.numeric(par_vals))
  grad <- gradiente_numerico(f_media, par_vals)
  var_delta <- as.numeric(t(grad) %*% vc_sub %*% grad)

  if (!is.finite(var_delta) || var_delta < 0) {
    return(tibble::tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }

  se <- sqrt(var_delta)
  tibble::tibble(
    se_delta = se,
    ic_delta_inf = est - 1.96 * se,
    ic_delta_sup = est + 1.96 * se,
    delta_ok = TRUE
  )
}

resumir_valores_tiempo <- function(pred, modelo, data_est, eq_Tw_rhs) {
  filas <- list()

  # VoL y VTAW predichos: se intenta IC delta. Si no se puede, se usa IC t de media muestral.
  for (ind in c("VoL_predicho", "VTAW_predicho", "VoL_observado", "VTAW_observado", "w")) {
    st <- ci_media_t(pred[[ind]])
    delta <- tibble::tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE)

    if (ind %in% c("VoL_predicho", "VTAW_predicho")) {
      delta <- ic_delta_media_valor(modelo, data_est, eq_Tw_rhs, ind)
    }

    usar_delta <- isTRUE(delta$delta_ok[1])

    filas[[ind]] <- tibble::tibble(
      indicador = ind,
      media = st$media,
      mediana = median(pred[[ind]], na.rm = TRUE),
      p25 = as.numeric(quantile(pred[[ind]], 0.25, na.rm = TRUE)),
      p75 = as.numeric(quantile(pred[[ind]], 0.75, na.rm = TRUE)),
      p95 = as.numeric(quantile(pred[[ind]], 0.95, na.rm = TRUE)),
      p99 = as.numeric(quantile(pred[[ind]], 0.99, na.rm = TRUE)),
      se_muestral = st$se,
      ic_muestral_inf = st$ic_inf,
      ic_muestral_sup = st$ic_sup,
      se_delta = delta$se_delta,
      ic_delta_inf = delta$ic_delta_inf,
      ic_delta_sup = delta$ic_delta_sup,
      metodo_ic_principal = ifelse(usar_delta, "delta", "t_muestra"),
      ic95_inf = ifelse(usar_delta, delta$ic_delta_inf, st$ic_inf),
      ic95_sup = ifelse(usar_delta, delta$ic_delta_sup, st$ic_sup),
      n_valido = st$n_valido
    )
  }

  bind_rows(filas)
}

crear_tabla_estilo_pablo <- function(resumen_modelo, parametros, valores_ic, dimension, categoria) {
  met <- resumen_modelo$valor[resumen_modelo$indicador == "Metodo elegido"]
  if (length(met) == 0) met <- NA_character_

  modelo_tbl <- tibble::tibble(
    dimension = dimension,
    categoria = categoria,
    bloque = "Model specification",
    indicador = c("# individuals", "# parameters", "LL", "Method"),
    valor = c(
      resumen_modelo$valor[resumen_modelo$indicador == "N personas estimadas"],
      resumen_modelo$valor[resumen_modelo$indicador == "N parametros"],
      resumen_modelo$valor[resumen_modelo$indicador == "Log likelihood"],
      met
    ),
    estimacion = NA_real_,
    error_estandar = NA_real_,
    t_value = NA_real_,
    sig = "",
    ic95_inf = NA_real_,
    ic95_sup = NA_real_
  )

  parametros_tbl <- parametros %>%
    transmute(
      dimension = dimension,
      categoria = categoria,
      bloque = "Time-use parameters",
      indicador = parametro,
      valor = NA_character_,
      estimacion = estimacion,
      error_estandar = error_estandar,
      t_value = t_value,
      sig = sig,
      ic95_inf = NA_real_,
      ic95_sup = NA_real_
    )

  valores_tbl <- valores_ic %>%
    filter(indicador %in% c("VoL_predicho", "VTAW_predicho", "w")) %>%
    mutate(
      indicador = case_when(
        indicador == "VoL_predicho" ~ "VoL predicho",
        indicador == "VTAW_predicho" ~ "VTAW predicho",
        indicador == "w" ~ "Weighted wage rate",
        TRUE ~ indicador
      )
    ) %>%
    transmute(
      dimension = dimension,
      categoria = categoria,
      bloque = "Values of time",
      indicador,
      valor = paste0(round(media, 4), " [", round(ic95_inf, 4), " ; ", round(ic95_sup, 4), "]"),
      estimacion = media,
      error_estandar = ifelse(metodo_ic_principal == "delta", se_delta, se_muestral),
      t_value = NA_real_,
      sig = "",
      ic95_inf = ic95_inf,
      ic95_sup = ic95_sup
    )

  bind_rows(modelo_tbl, parametros_tbl, valores_tbl)
}

# ---------------------------------------------------------------
# 3. Carpetas principales
# ---------------------------------------------------------------

crear_dir("prueba")
crear_dir(carpeta_salida)
crear_dir(file.path(carpeta_salida, "00_base_y_diagnosticos"))
crear_dir(file.path(carpeta_salida, "01_muestra_general"))
crear_dir(file.path(carpeta_salida, "02_grupos"))
crear_dir(file.path(carpeta_salida, "03_consolidados"))
crear_dir(file.path(carpeta_salida, "04_graficos"))

cat("\nDirectorio de trabajo actual:\n")
print(getwd())
cat("\nCarpeta de salida:\n")
print(carpeta_salida)

# ---------------------------------------------------------------
# 4. Leer enut_ii y preparar clusters modales si corresponde
# ---------------------------------------------------------------

if (!file.exists(archivo_base)) {
  stop(paste0("No se encontro ", archivo_base, ". Debe estar en el directorio de trabajo."))
}

base_original <- readxl::read_excel(archivo_base, sheet = hoja_base) %>%
  as.data.frame()

names(base_original) <- stringr::str_trim(names(base_original))

if (!("id_persona" %in% names(base_original))) {
  stop("La base debe contener id_persona.")
}

base_original <- base_original %>%
  mutate(id_persona = as.character(id_persona))

# Si cluster_modo no esta en enut_ii, se intenta recuperar desde el archivo de clusters modales.
if (!("cluster_modo" %in% names(base_original))) {
  candidatos_cluster_modo <- c(
    "resultados_clusters_modo_k4_sociodemografico.xlsx",
    "resultados_clusters_modo.xlsx",
    "clusters_modo.xlsx"
  )
  archivo_cluster_modo <- candidatos_cluster_modo[file.exists(candidatos_cluster_modo)][1]

  if (!is.na(archivo_cluster_modo)) {
    cat("\ncluster_modo no estaba en enut_ii. Se intentara unir desde:\n")
    print(archivo_cluster_modo)

    asignacion_modo <- readxl::read_excel(archivo_cluster_modo, sheet = "10_asignacion_persona") %>%
      as.data.frame() %>%
      transmute(
        id_persona = as.character(id_persona),
        cluster_modo = as.character(cluster_modo)
      )

    base_original <- base_original %>%
      left_join(asignacion_modo, by = "id_persona")
  } else {
    warning("No se encontro cluster_modo en enut_ii ni archivo opcional de asignacion modal. Se omitira el analisis por cluster_modo.")
    base_original$cluster_modo <- NA_character_
  }
}

# ---------------------------------------------------------------
# 5. Recodificacion modal obligatoria
# ---------------------------------------------------------------

vars_modo <- c("modo_to", "modo_ed", "modo_tdnr", "modo_tcnr", "modo_cp")

faltan_modos <- setdiff(vars_modo, names(base_original))
if (length(faltan_modos) > 0) {
  stop(paste0("Faltan variables de modo: ", paste(faltan_modos, collapse = ", ")))
}

for (v in vars_modo) {
  base_original[[paste0(v, "_original_enut")]] <- num(base_original[[v]])
  base_original[[v]] <- recodificar_modo_auto_ref(base_original[[v]])
}

control_recodificacion <- dplyr::bind_rows(lapply(vars_modo, function(v) {
  tibble::tibble(
    variable = v,
    original_enut = base_original[[paste0(v, "_original_enut")]],
    interno_auto_ref = base_original[[v]]
  ) %>%
    filter(original_enut %in% 1:7 | interno_auto_ref %in% 1:7) %>%
    count(variable, original_enut, interno_auto_ref, name = "n")
})) %>%
  arrange(variable, original_enut, interno_auto_ref)

readr::write_csv(control_recodificacion, file.path(carpeta_salida, "00_base_y_diagnosticos", "00_control_recodificacion_modos.csv"))

# ---------------------------------------------------------------
# 6. Verificar variables necesarias
# ---------------------------------------------------------------

vars_mnl_muestra <- c(
  "id_persona",
  "modo_to", "modo_ed", "modo_tdnr", "modo_tcnr", "modo_cp",
  "t_paid_work", "t_education", "t_domestic_work", "t_care_work", "t_personal_care",
  "sexo", "macrozona", "edad_anios",
  "n_menores_18", "n_trabajadores", "n_profesionales", "vive_pareja",
  "nivel_escolaridad", "quintil", "ing_personal"
)

vars_mtuem <- c(
  "Tw_new", "w_new", "t_leisure", "t_meals", "w_dc",
  "Ec_cuentas", "Ec_hogar", "Ec_salud", "Ec_transporte", "Ec_educacion",
  "ing_jub_aps", "ing_gpp",
  "t_domestic_work", "t_care_work",
  "cluster_tiempo", "cluster_modo"
)

vars_necesarias <- unique(c(vars_mnl_muestra, vars_mtuem))
faltantes <- setdiff(vars_necesarias, names(base_original))

if (length(faltantes) > 0) {
  stop(paste0("Faltan variables necesarias en enut_ii o archivos auxiliares:\n", paste(faltantes, collapse = "\n")))
}

# ---------------------------------------------------------------
# 7. Construir variables de grupo y muestra compatible con MNL V9
# ---------------------------------------------------------------

base_original <- base_original %>%
  mutate(
    sexo_num = num(sexo),
    edad_num = num(edad_anios),
    quintil_num = num(quintil),
    macrozona_limpia = limpiar_txt(macrozona),
    cluster_tiempo_limpio = limpiar_txt(cluster_tiempo),
    cluster_modo_limpio = limpiar_txt(cluster_modo),

    grupo_edad = case_when(
      !is.na(edad_num) & edad_num >= 18 & edad_num <= 29 ~ "18 a 29",
      !is.na(edad_num) & edad_num >= 30 & edad_num <= 44 ~ "30 a 44",
      !is.na(edad_num) & edad_num >= 45 & edad_num <= 59 ~ "45 a 59",
      !is.na(edad_num) & edad_num >= 60 ~ "60 o mas",
      TRUE ~ NA_character_
    ),
    grupo_sexo = case_when(
      sexo_num == 0 ~ "Hombres",
      sexo_num == 1 ~ "Mujeres",
      TRUE ~ NA_character_
    ),
    grupo_ingreso = case_when(
      quintil_num %in% c(1, 2, 3) ~ "Quintil bajo",
      quintil_num == 4 ~ "Quintil medio",
      quintil_num == 5 ~ "Quintil alto",
      TRUE ~ NA_character_
    ),
    grupo_territorio = case_when(
      macrozona_limpia %in% c("norte", "zona norte") ~ "Norte",
      macrozona_limpia %in% c("centro", "metropolitana", "rm", "region metropolitana", "metropolitan") ~ "Centro",
      macrozona_limpia %in% c("sur", "zona sur") ~ "Sur",
      TRUE ~ NA_character_
    ),
    grupo_cluster_tiempo = case_when(
      cluster_tiempo_limpio %in% c("cluster 1", "1") ~ "Cuidadores domesticos",
      cluster_tiempo_limpio %in% c("cluster 2", "2") ~ "Rutina flexible",
      cluster_tiempo_limpio %in% c("cluster 3", "3") ~ "Jovenes trabajadores",
      cluster_tiempo_limpio %in% c("cluster 4", "4") ~ "Trabajadores rutinarios",
      cluster_tiempo_limpio %in% c("cluster 5", "5") ~ "Trabajadores moviles",
      TRUE ~ NA_character_
    ),
    grupo_cluster_modo = case_when(
      cluster_modo_limpio %in% c("cluster 1", "1") ~ "Usuarios publicos",
      cluster_modo_limpio %in% c("cluster 2", "2") ~ "Movilidad activa",
      cluster_modo_limpio %in% c("cluster 3", "3") ~ "Movilidad mixta",
      cluster_modo_limpio %in% c("cluster 4", "4") ~ "Motorizados privados",
      TRUE ~ NA_character_
    )
  )

# Ordenes de categorias para tablas y graficos.
ordenes_categorias <- list(
  edad = c("18 a 29", "30 a 44", "45 a 59", "60 o mas"),
  sexo = c("Hombres", "Mujeres"),
  ingresos = c("Quintil bajo", "Quintil medio", "Quintil alto"),
  territorio = c("Norte", "Centro", "Sur"),
  cluster_tiempo = c("Cuidadores domesticos", "Rutina flexible", "Jovenes trabajadores", "Trabajadores rutinarios", "Trabajadores moviles"),
  cluster_modo = c("Usuarios publicos", "Movilidad activa", "Movilidad mixta", "Motorizados privados")
)

base_persona_mnl <- base_original %>%
  mutate(
    female = as.integer(num(sexo) == 1),

    zona_centro = as.integer(macrozona_limpia %in% c("centro", "metropolitana")),
    zona_sur = as.integer(macrozona_limpia == "sur"),

    edad_18_24 = as.integer(!is.na(edad_num) & edad_num >= 18 & edad_num <= 24),
    edad_45_64 = as.integer(!is.na(edad_num) & edad_num >= 45 & edad_num <= 64),
    edad_65mas = as.integer(!is.na(edad_num) & edad_num >= 65),

    n_menores_18 = num(n_menores_18),
    n_trabajadores = num(n_trabajadores),
    n_profesionales = num(n_profesionales),
    vive_pareja = as.integer(num(vive_pareja) == 1),

    nivel_escolaridad_limpia = limpiar_txt(nivel_escolaridad),
    educ_secundaria = as.integer(nivel_escolaridad_limpia == "secundaria"),
    educ_tecnica = as.integer(nivel_escolaridad_limpia == "tecnica"),
    educ_universitaria = as.integer(nivel_escolaridad_limpia == "universitaria"),

    quintil_2 = as.integer(quintil_num == 2),
    quintil_3 = as.integer(quintil_num == 3),
    quintil_4 = as.integer(quintil_num == 4),
    quintil_5 = as.integer(quintil_num == 5),

    ing_personal = num(ing_personal)
  ) %>%
  select(
    id_persona,
    female, zona_centro, zona_sur,
    edad_18_24, edad_45_64, edad_65mas,
    n_menores_18, n_trabajadores, n_profesionales, vive_pareja,
    educ_secundaria, educ_tecnica, educ_universitaria,
    quintil_2, quintil_3, quintil_4, quintil_5,
    ing_personal
  )

mapa_modulos <- tibble::tribble(
  ~modulo, ~modo_var, ~tiempo_modulo_var,
  "TO", "modo_to", "t_paid_work",
  "ED", "modo_ed", "t_education",
  "TD", "modo_tdnr", "t_domestic_work",
  "TC", "modo_tcnr", "t_care_work",
  "CP", "modo_cp", "t_personal_care"
)

base_long_mnl <- dplyr::bind_rows(lapply(seq_len(nrow(mapa_modulos)), function(i) {
  tibble::tibble(
    id_persona = base_original$id_persona,
    modulo = mapa_modulos$modulo[i],
    choice = num(base_original[[mapa_modulos$modo_var[i]]]),
    T_modulo_sem = num(base_original[[mapa_modulos$tiempo_modulo_var[i]]])
  )
})) %>%
  mutate(
    modulo = factor(modulo, levels = c("TO", "TD", "TC", "ED", "CP")),
    T_mod_10h = T_modulo_sem / 10
  ) %>%
  left_join(base_persona_mnl, by = "id_persona")

base_joint_mnl <- base_long_mnl %>%
  filter(
    choice %in% 1:7,
    is.finite(T_mod_10h), T_mod_10h >= 0,
    female %in% c(0, 1),
    zona_centro %in% c(0, 1), zona_sur %in% c(0, 1),
    edad_18_24 %in% c(0, 1), edad_45_64 %in% c(0, 1), edad_65mas %in% c(0, 1),
    is.finite(n_menores_18), n_menores_18 >= 0,
    is.finite(n_trabajadores), n_trabajadores >= 0,
    is.finite(n_profesionales), n_profesionales >= 0,
    vive_pareja %in% c(0, 1),
    educ_secundaria %in% c(0, 1), educ_tecnica %in% c(0, 1), educ_universitaria %in% c(0, 1),
    quintil_2 %in% c(0, 1), quintil_3 %in% c(0, 1), quintil_4 %in% c(0, 1), quintil_5 %in% c(0, 1),
    is.finite(ing_personal), ing_personal >= 0
  ) %>%
  arrange(id_persona, modulo)

ids_mnl <- base_joint_mnl %>%
  distinct(id_persona) %>%
  arrange(id_persona) %>%
  mutate(PeID = row_number())

readr::write_csv(ids_mnl, file.path(carpeta_salida, "00_base_y_diagnosticos", "00_equivalencia_PeID_id_persona.csv"))

freq_modos_muestra <- base_joint_mnl %>%
  left_join(ids_mnl, by = "id_persona") %>%
  count(modulo, choice, name = "n") %>%
  group_by(modulo) %>%
  mutate(pct_modulo = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(
    alternativa = case_when(
      choice == 1 ~ "1 Auto/moto particular",
      choice == 2 ~ "2 Transporte publico",
      choice == 3 ~ "3 Taxi/app",
      choice == 4 ~ "4 Bicicleta/ciclos",
      choice == 5 ~ "5 A pie",
      choice == 6 ~ "6 Otros",
      choice == 7 ~ "7 Multimodal",
      TRUE ~ "Otra"
    )
  ) %>%
  arrange(modulo, choice)

readr::write_csv(freq_modos_muestra, file.path(carpeta_salida, "00_base_y_diagnosticos", "00_frecuencia_modos_muestra_compatible.csv"))

# ---------------------------------------------------------------
# 8. Construir base continua MTUEM corregida
# ---------------------------------------------------------------

base_cont_persona <- base_original %>%
  semi_join(ids_mnl, by = "id_persona") %>%
  left_join(ids_mnl, by = "id_persona") %>%
  transmute(
    PeID = as.numeric(PeID),
    id_persona = as.character(id_persona),

    grupo_edad, grupo_sexo, grupo_ingreso, grupo_territorio,
    grupo_cluster_tiempo, grupo_cluster_modo,

    Tw = num(Tw_new),
    Tf1 = num(t_leisure),
    Tf2 = num(t_meals),
    w = num(w_new),
    ta = 168,

    Ec_cuentas_num = num0(Ec_cuentas),
    Ec_hogar_num = num0(Ec_hogar),
    Ec_salud_num = num0(Ec_salud),
    Ec_transporte_num = num0(Ec_transporte),
    Ec_educacion_num = num0(Ec_educacion),
    ing_jub_aps_num = num0(ing_jub_aps),
    ing_gpp_num = num0(ing_gpp),
    w_dc_num = num0(w_dc),
    t_domestic_work_num = num0(t_domestic_work),
    t_care_work_num = num0(t_care_work),

    Ec_monetario = Ec_cuentas_num + Ec_hogar_num + Ec_salud_num + Ec_transporte_num + Ec_educacion_num,
    ing_fijo = ing_jub_aps_num + ing_gpp_num,
    EcI = Ec_monetario - ing_fijo,
    Ec_trabajo_no_remunerado = w_dc_num * (t_domestic_work_num + t_care_work_num),
    Ec = EcI + Ec_trabajo_no_remunerado
  ) %>%
  mutate(
    Tc = ta - Tw - Tf1 - Tf2,
    cierre_temporal = Tw + Tf1 + Tf2 + Tc,
    tiempo_libre_observado = Tf1 + Tf2,
    ingreso_libre_observado = w * Tw - Ec,
    ingreso_potencial_neto_Ec = w * (ta - Tc) - Ec,

    filtro_finite = is.finite(PeID) & is.finite(Tw) & is.finite(Tf1) & is.finite(Tf2) &
      is.finite(Tc) & is.finite(w) & is.finite(Ec) & is.finite(ta),
    filtro_trabajo_salario = is.finite(Tw) & Tw > 0 & is.finite(w) & w > 0,
    filtro_tiempo = is.finite(Tc) & Tc >= 0 & Tc < ta & is.finite(tiempo_libre_observado) & tiempo_libre_observado > 0,
    filtro_Ec = is.finite(Ec) & Ec > 0,
    filtro_ingreso_potencial = is.finite(ingreso_potencial_neto_Ec) & ingreso_potencial_neto_Ec > 0,
    filtro_mtuem_final = filtro_finite & filtro_trabajo_salario & filtro_tiempo & filtro_Ec & filtro_ingreso_potencial
  )

readr::write_csv(base_cont_persona, file.path(carpeta_salida, "00_base_y_diagnosticos", "01_base_mtuem_pre_filtro_economico.csv"))

# Trazabilidad general antes de estimar.
trazabilidad_mtuem_global <- tibble::tibble(
  etapa = c(
    "Personas enut_ii",
    "Personas compatibles con MNL V9",
    "Variables finitas para MTUEM",
    "Tw > 0 y w > 0",
    "Tc valido y tiempo libre observado > 0",
    "Ec corregido > 0",
    "Ingreso potencial neto de Ec > 0",
    "Muestra final MTUEM general"
  ),
  n_personas = c(
    nrow(base_original),
    nrow(base_cont_persona),
    sum(base_cont_persona$filtro_finite, na.rm = TRUE),
    sum(base_cont_persona$filtro_finite & base_cont_persona$filtro_trabajo_salario, na.rm = TRUE),
    sum(base_cont_persona$filtro_finite & base_cont_persona$filtro_trabajo_salario & base_cont_persona$filtro_tiempo, na.rm = TRUE),
    sum(base_cont_persona$filtro_finite & base_cont_persona$filtro_trabajo_salario & base_cont_persona$filtro_tiempo & base_cont_persona$filtro_Ec, na.rm = TRUE),
    sum(base_cont_persona$filtro_finite & base_cont_persona$filtro_trabajo_salario & base_cont_persona$filtro_tiempo & base_cont_persona$filtro_Ec & base_cont_persona$filtro_ingreso_potencial, na.rm = TRUE),
    sum(base_cont_persona$filtro_mtuem_final, na.rm = TRUE)
  )
) %>%
  mutate(pct_sobre_mnl_v9 = 100 * n_personas / nrow(base_cont_persona))

readr::write_csv(trazabilidad_mtuem_global, file.path(carpeta_salida, "00_base_y_diagnosticos", "01_trazabilidad_muestra_mtuem_global.csv"))

# ---------------------------------------------------------------
# 9. Especificacion MTUEM continuo: Tw + Tf1
# ---------------------------------------------------------------

eq_c <- c(
  "Tw ~ ((((PH) + (tw)) * (ta - Tc + 2) + (1 + (tw)) * (Ec/w - 2/w) - (1 + (PH))) + sqrt((((PH) + (tw)) * (ta - Tc + 2) + (1 + (tw)) * (Ec/w - 2/w) - (1 + (PH)))^2 - 4 * (1 + (PH) + (tw)) * (-(PH) * (ta - Tc + 2) + (1 - (tw) * (ta - Tc + 2)) * (2/w - Ec/w))))/(2 * (1 + (PH) + (tw)))",
  "Tf1 ~ (th1) * (ta - (Tw) - Tc)"
)

eq_c[-1] <- gsub("Tw", gsub(".*~", "", eq_c[1]), eq_c[-1])
par_c <- c("PH", "tw", "th1")
eq_Tw_rhs <- gsub(".*~", "", eq_c[1])

writeLines(eq_c, con = file.path(carpeta_salida, "00_base_y_diagnosticos", "02_eq_c_mtuem_Tw_Tf1.txt"))

# ---------------------------------------------------------------
# 10. Funcion principal de estimacion por segmento
# ---------------------------------------------------------------

estimar_mtuem_segmento <- function(base_segmento_pre, carpeta_segmento, dimension, categoria, orden_categoria = NA_integer_, start_base = NULL) {
  crear_dir(carpeta_segmento)

  etiqueta_modelo <- paste(dimension, categoria, sep = " - ")
  cat("\n====================================================\n")
  cat("Estimando segmento:", etiqueta_modelo, "\n")
  cat("Carpeta:", carpeta_segmento, "\n")
  cat("====================================================\n")

  trazabilidad <- tibble::tibble(
    dimension = dimension,
    categoria = categoria,
    etapa = c(
      "Personas compatibles con MNL V9 en grupo",
      "Variables finitas para MTUEM",
      "Tw > 0 y w > 0",
      "Tc valido y tiempo libre observado > 0",
      "Ec corregido > 0",
      "Ingreso potencial neto de Ec > 0",
      "Muestra final MTUEM"
    ),
    n_personas = c(
      nrow(base_segmento_pre),
      sum(base_segmento_pre$filtro_finite, na.rm = TRUE),
      sum(base_segmento_pre$filtro_finite & base_segmento_pre$filtro_trabajo_salario, na.rm = TRUE),
      sum(base_segmento_pre$filtro_finite & base_segmento_pre$filtro_trabajo_salario & base_segmento_pre$filtro_tiempo, na.rm = TRUE),
      sum(base_segmento_pre$filtro_finite & base_segmento_pre$filtro_trabajo_salario & base_segmento_pre$filtro_tiempo & base_segmento_pre$filtro_Ec, na.rm = TRUE),
      sum(base_segmento_pre$filtro_finite & base_segmento_pre$filtro_trabajo_salario & base_segmento_pre$filtro_tiempo & base_segmento_pre$filtro_Ec & base_segmento_pre$filtro_ingreso_potencial, na.rm = TRUE),
      sum(base_segmento_pre$filtro_mtuem_final, na.rm = TRUE)
    )
  ) %>%
    mutate(pct_sobre_grupo_mnl_v9 = 100 * n_personas / first(n_personas))

  readr::write_csv(trazabilidad, file.path(carpeta_segmento, "01_trazabilidad_muestra_mtuem.csv"))

  data_est <- base_segmento_pre %>%
    filter(filtro_mtuem_final) %>%
    arrange(PeID) %>%
    select(
      PeID, Tw, Tf1, Tf2, Tc, w, Ec, ta,
      id_persona, grupo_edad, grupo_sexo, grupo_ingreso, grupo_territorio,
      grupo_cluster_tiempo, grupo_cluster_modo,
      Ec_monetario, ing_fijo, EcI, Ec_trabajo_no_remunerado,
      cierre_temporal, tiempo_libre_observado,
      ingreso_libre_observado, ingreso_potencial_neto_Ec
    )

  readr::write_csv(data_est, file.path(carpeta_segmento, "01_base_mtuem_continua_preparada.csv"))

  resumen_base <- tibble::tibble(
    dimension = dimension,
    categoria = categoria,
    indicador = c(
      "N personas compatibles con MNL V9 en grupo",
      "N personas usadas en MTUEM continuo",
      "N personas removidas por filtros MTUEM",
      "Media Tw",
      "Media Tf1 ocio",
      "Media Tf2 comidas",
      "Media tiempo libre observado",
      "Media Tc residual",
      "Media w",
      "Media Ec monetario bruto",
      "Media ingreso fijo descontado",
      "Media EcI neto de ingreso fijo",
      "Media Ec trabajo no remunerado",
      "Media Ec total corregido",
      "Media cierre temporal",
      "N cierre temporal distinto de 168",
      "Porcentaje cierre temporal distinto de 168",
      "N Tc <= 0",
      "Porcentaje Tc <= 0",
      "N ingreso libre observado <= 0",
      "Porcentaje ingreso libre observado <= 0",
      "N ingreso potencial neto Ec <= 0",
      "Porcentaje ingreso potencial neto Ec <= 0"
    ),
    valor = c(
      nrow(base_segmento_pre),
      nrow(data_est),
      nrow(base_segmento_pre) - nrow(data_est),
      mean(data_est$Tw, na.rm = TRUE),
      mean(data_est$Tf1, na.rm = TRUE),
      mean(data_est$Tf2, na.rm = TRUE),
      mean(data_est$tiempo_libre_observado, na.rm = TRUE),
      mean(data_est$Tc, na.rm = TRUE),
      mean(data_est$w, na.rm = TRUE),
      mean(data_est$Ec_monetario, na.rm = TRUE),
      mean(data_est$ing_fijo, na.rm = TRUE),
      mean(data_est$EcI, na.rm = TRUE),
      mean(data_est$Ec_trabajo_no_remunerado, na.rm = TRUE),
      mean(data_est$Ec, na.rm = TRUE),
      mean(data_est$cierre_temporal, na.rm = TRUE),
      sum(abs(data_est$cierre_temporal - data_est$ta) > 1e-6, na.rm = TRUE),
      mean(abs(data_est$cierre_temporal - data_est$ta) > 1e-6, na.rm = TRUE),
      sum(data_est$Tc <= 0, na.rm = TRUE),
      mean(data_est$Tc <= 0, na.rm = TRUE),
      sum(data_est$ingreso_libre_observado <= 0, na.rm = TRUE),
      mean(data_est$ingreso_libre_observado <= 0, na.rm = TRUE),
      sum(data_est$ingreso_potencial_neto_Ec <= 0, na.rm = TRUE),
      mean(data_est$ingreso_potencial_neto_Ec <= 0, na.rm = TRUE)
    )
  )

  readr::write_csv(resumen_base, file.path(carpeta_segmento, "01_resumen_base_mtuem.csv"))
  writeLines(eq_c, con = file.path(carpeta_segmento, "02_eq_c_mtuem_Tw_Tf1.txt"))

  if (nrow(data_est) < MIN_N_GRUPO) {
    nota <- tibble::tibble(
      dimension = dimension,
      categoria = categoria,
      estado = "no_estimado",
      motivo = paste0("Muestra final menor a MIN_N_GRUPO = ", MIN_N_GRUPO),
      n_final = nrow(data_est)
    )
    readr::write_csv(nota, file.path(carpeta_segmento, "modelo_no_estimado.csv"))
    return(list(
      status = nota,
      resumen_base = resumen_base,
      trazabilidad = trazabilidad,
      parametros = tibble(),
      resumen_modelo = tibble(),
      valores_ic = tibble(),
      percentiles = tibble(),
      tabla_pablo = tibble()
    ))
  }

  den_th1 <- data_est$ta - data_est$Tw - data_est$Tc
  ratio_th1 <- data_est$Tf1 / den_th1
  ratio_th1 <- ratio_th1[is.finite(ratio_th1)]
  th1_start <- suppressWarnings(median(ratio_th1, na.rm = TRUE))
  if (!is.finite(th1_start)) th1_start <- 0.50
  th1_start <- min(max(th1_start, 0.05), 0.95)

  if (is.null(start_base)) {
    start_cont <- c(PH = 0.2633, tw = 0.2970, th1 = th1_start)
  } else {
    start_cont <- start_base
    if (!is.finite(start_cont["th1"])) start_cont["th1"] <- th1_start
  }

  readr::write_csv(tibble::enframe(start_cont, name = "parametro", value = "start"), file.path(carpeta_segmento, "02_valores_iniciales_mtuem.csv"))

  run_nmm <- function(nombre, metodo, miterlim) {
    archivo_rds <- file.path(carpeta_segmento, paste0(nombre, ".rds"))
    archivo_log <- file.path(carpeta_segmento, paste0(nombre, "_log.txt"))

    if (isTRUE(USAR_RDS_EXISTENTE) && file.exists(archivo_rds)) {
      return(readRDS(archivo_rds))
    }

    inicio <- Sys.time()
    sink(archivo_log, split = TRUE)
    cat("\n====================================================\n")
    cat("Segmento:", etiqueta_modelo, "\n")
    cat("Ejecutando:", nombre, "\n")
    cat("Metodo:", metodo, "\n")
    cat("Inicio:", format(inicio, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("====================================================\n")

    obj <- tryCatch(
      nmm(
        data = data_est %>% select(PeID, Tw, Tf1, Tf2, Tc, w, Ec, ta),
        eq_type = "cont",
        eq_c = eq_c,
        par_c = par_c,
        start_v = start_cont,
        fixed_term = FALSE,
        best_method = FALSE,
        DEoptim_run = FALSE,
        try_last_DEoptim = FALSE,
        opt_method = metodo,
        numerical_deriv = TRUE,
        miterlim = miterlim
      ),
      error = function(e) {
        cat("\nERROR:\n")
        cat(conditionMessage(e), "\n")
        return(NULL)
      }
    )

    fin <- Sys.time()
    if (!is.null(obj)) {
      cat("\nResumen del objeto:\n")
      print(summary(obj))
      saveRDS(obj, archivo_rds)
    }
    cat("\nFin:", format(fin, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Duracion minutos:", round(as.numeric(difftime(fin, inicio, units = "mins")), 2), "\n")
    sink()

    obj
  }

  res_bfgs <- run_nmm("03_cont_Tw_Tf1_BFGS", "BFGS", ITER_CONT_BFGS)
  res_nm <- NULL
  if (!modelo_loglik_ok(res_bfgs)) {
    res_nm <- run_nmm("03_cont_Tw_Tf1_NM", "NM", ITER_CONT_NM)
  }
  res_sann <- NULL
  if (!modelo_loglik_ok(res_bfgs) && !modelo_loglik_ok(res_nm)) {
    res_sann <- run_nmm("03_cont_Tw_Tf1_SANN_fallback", "SANN", ITER_CONT_SANN)
  }

  candidatos <- list(BFGS = res_bfgs, NM = res_nm, SANN_fallback = res_sann)

  comparacion_candidatos <- tibble::tibble(
    dimension = dimension,
    categoria = categoria,
    metodo = names(candidatos),
    logLik = sapply(candidatos, extraer_loglik),
    valido = is.finite(logLik),
    code = sapply(candidatos, extraer_code)
  )

  readr::write_csv(comparacion_candidatos, file.path(carpeta_segmento, "03_comparacion_candidatos_mtuem.csv"))

  if (!any(comparacion_candidatos$valido)) {
    nota <- tibble::tibble(
      dimension = dimension,
      categoria = categoria,
      estado = "no_estimado",
      motivo = "Ningun metodo produjo logLik valido",
      n_final = nrow(data_est)
    )
    readr::write_csv(nota, file.path(carpeta_segmento, "modelo_no_estimado.csv"))
    return(list(
      status = nota,
      resumen_base = resumen_base,
      trazabilidad = trazabilidad,
      parametros = tibble(),
      resumen_modelo = tibble(),
      valores_ic = tibble(),
      percentiles = tibble(),
      tabla_pablo = tibble()
    ))
  }

  metodo_elegido <- comparacion_candidatos %>%
    filter(valido) %>%
    arrange(desc(logLik)) %>%
    slice(1) %>%
    pull(metodo)

  res_cont <- candidatos[[metodo_elegido]]
  saveRDS(res_cont, file.path(carpeta_segmento, "03_cont_Tw_Tf1_MEJOR.rds"))

  tabla_parametros <- extraer_parametros(res_cont) %>%
    mutate(dimension = dimension, categoria = categoria, .before = 1)
  readr::write_csv(tabla_parametros, file.path(carpeta_segmento, "04_parametros_mtuem.csv"))

  data_eval <- calcular_predicciones_con_parametros(data_est, eq_Tw_rhs, res_cont$estimate[c("PH", "tw", "th1")])

  resumen_modelo <- tibble::tibble(
    dimension = dimension,
    categoria = categoria,
    indicador = c(
      "N personas estimadas",
      "N parametros",
      "Log likelihood",
      "Metodo elegido",
      "Return code",
      "PH",
      "tw",
      "th1",
      "Tw observado promedio",
      "Tw predicho promedio",
      "Tf1 ocio observado promedio",
      "Tf1 ocio predicho promedio",
      "Tf2 comidas observado promedio",
      "Tf2 comidas predicho promedio",
      "RMSE Tw",
      "RMSE Tf1 ocio",
      "RMSE Tf2 comidas",
      "VoL predicho promedio",
      "VoL predicho mediana",
      "VoL observado promedio",
      "VoL observado mediana",
      "VTAW predicho promedio",
      "VTAW observado promedio",
      "Weighted wage rate promedio",
      "N ingreso libre observado <= 0",
      "Porcentaje ingreso libre observado <= 0",
      "N ingreso libre predicho <= 0",
      "Porcentaje ingreso libre predicho <= 0",
      "N ingreso potencial neto Ec <= 0",
      "Porcentaje ingreso potencial neto Ec <= 0",
      "Media cierre temporal observado",
      "Media cierre temporal predicho",
      "Media diferencia cierre temporal predicho"
    ),
    valor = as.character(c(
      nrow(data_eval),
      length(res_cont$estimate),
      as.numeric(logLik(res_cont)),
      metodo_elegido,
      extraer_code(res_cont),
      as.numeric(res_cont$estimate["PH"]),
      as.numeric(res_cont$estimate["tw"]),
      as.numeric(res_cont$estimate["th1"]),
      mean(data_eval$Tw, na.rm = TRUE),
      mean(data_eval$Tw_hat, na.rm = TRUE),
      mean(data_eval$Tf1, na.rm = TRUE),
      mean(data_eval$Tf1_hat, na.rm = TRUE),
      mean(data_eval$Tf2, na.rm = TRUE),
      mean(data_eval$Tf2_hat, na.rm = TRUE),
      sqrt(mean(data_eval$error_Tw^2, na.rm = TRUE)),
      sqrt(mean(data_eval$error_Tf1^2, na.rm = TRUE)),
      sqrt(mean(data_eval$error_Tf2^2, na.rm = TRUE)),
      mean(data_eval$VoL_predicho, na.rm = TRUE),
      median(data_eval$VoL_predicho, na.rm = TRUE),
      mean(data_eval$VoL_observado, na.rm = TRUE),
      median(data_eval$VoL_observado, na.rm = TRUE),
      mean(data_eval$VTAW_predicho, na.rm = TRUE),
      mean(data_eval$VTAW_observado, na.rm = TRUE),
      mean(data_eval$w, na.rm = TRUE),
      sum(data_eval$ingreso_libre_observado <= 0, na.rm = TRUE),
      mean(data_eval$ingreso_libre_observado <= 0, na.rm = TRUE),
      sum(data_eval$ingreso_libre_predicho <= 0, na.rm = TRUE),
      mean(data_eval$ingreso_libre_predicho <= 0, na.rm = TRUE),
      sum(data_eval$ingreso_potencial_neto_Ec <= 0, na.rm = TRUE),
      mean(data_eval$ingreso_potencial_neto_Ec <= 0, na.rm = TRUE),
      mean(data_eval$cierre_temporal, na.rm = TRUE),
      mean(data_eval$cierre_temporal_predicho, na.rm = TRUE),
      mean(data_eval$diferencia_cierre_predicho, na.rm = TRUE)
    ))
  )

  readr::write_csv(resumen_modelo, file.path(carpeta_segmento, "04_resumen_mtuem.csv"))

  valores_ic <- resumir_valores_tiempo(data_eval, res_cont, data_est, eq_Tw_rhs) %>%
    mutate(
      dimension = dimension,
      categoria = categoria,
      orden_categoria = orden_categoria,
      .before = 1
    )
  readr::write_csv(valores_ic, file.path(carpeta_segmento, "04_valores_tiempo_IC.csv"))

  predicciones <- data_eval %>%
    select(
      PeID, id_persona,
      grupo_edad, grupo_sexo, grupo_ingreso, grupo_territorio, grupo_cluster_tiempo, grupo_cluster_modo,
      Tw, Tw_hat,
      Tf1, Tf1_hat,
      Tf2, Tf2_hat,
      Tc, w, Ec, Ec_monetario, ing_fijo, EcI, Ec_trabajo_no_remunerado,
      VoL_predicho, VoL_observado,
      VTAW_predicho, VTAW_observado,
      VoL_predicho_sobre_w, VTAW_predicho_sobre_w,
      ingreso_libre_observado, ingreso_libre_predicho, ingreso_potencial_neto_Ec,
      error_Tw, error_Tf1, error_Tf2,
      cierre_temporal, cierre_temporal_predicho, diferencia_cierre_predicho
    )

  readr::write_csv(predicciones, file.path(carpeta_segmento, "04_predicciones_y_valor_ocio_mtuem.csv"))

  percentiles <- data_eval %>%
    summarise(
      across(
        c(
          VoL_predicho, VoL_observado, VTAW_predicho, VTAW_observado,
          VoL_predicho_sobre_w, VTAW_predicho_sobre_w,
          Tw_hat, Tf1_hat, Tf2_hat,
          ingreso_libre_observado, ingreso_libre_predicho, ingreso_potencial_neto_Ec
        ),
        list(
          p01 = ~quantile(.x, 0.01, na.rm = TRUE),
          p05 = ~quantile(.x, 0.05, na.rm = TRUE),
          p25 = ~quantile(.x, 0.25, na.rm = TRUE),
          p50 = ~quantile(.x, 0.50, na.rm = TRUE),
          p75 = ~quantile(.x, 0.75, na.rm = TRUE),
          p95 = ~quantile(.x, 0.95, na.rm = TRUE),
          p99 = ~quantile(.x, 0.99, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    tidyr::pivot_longer(everything(), names_to = "indicador", values_to = "valor") %>%
    mutate(dimension = dimension, categoria = categoria, .before = 1)

  readr::write_csv(percentiles, file.path(carpeta_segmento, "04_percentiles_mtuem.csv"))

  tabla_pablo <- crear_tabla_estilo_pablo(resumen_modelo, tabla_parametros, valores_ic, dimension, categoria)
  readr::write_csv(tabla_pablo, file.path(carpeta_segmento, "04_tabla_estilo_pablo.csv"))

  # Grafico local: distribucion de VoL y VTAW en escala sobre salario.
  valores_largos <- data_eval %>%
    select(VoL_predicho_sobre_w, VTAW_predicho_sobre_w) %>%
    pivot_longer(everything(), names_to = "indicador", values_to = "valor") %>%
    mutate(
      indicador = case_when(
        indicador == "VoL_predicho_sobre_w" ~ "VoL / w",
        indicador == "VTAW_predicho_sobre_w" ~ "VTAW / w",
        TRUE ~ indicador
      )
    )

  graf_dist <- ggplot(valores_largos, aes(x = indicador, y = valor)) +
    geom_boxplot(outlier.alpha = 0.15) +
    labs(
      title = paste0("Distribucion de valores de tiempo - ", categoria),
      x = NULL,
      y = "Valor relativo al salario horario"
    ) +
    theme_minimal()

  ggsave(file.path(carpeta_segmento, "04_grafico_distribucion_valores_tiempo.png"), graf_dist, width = 8, height = 5, dpi = 300)

  writexl::write_xlsx(
    list(
      trazabilidad_muestra = trazabilidad,
      resumen_base = resumen_base,
      comparacion_metodos = comparacion_candidatos,
      parametros = tabla_parametros,
      resumen_modelo = resumen_modelo,
      valores_tiempo_IC = valores_ic,
      tabla_estilo_pablo = tabla_pablo,
      percentiles = percentiles,
      predicciones = predicciones
    ),
    path = file.path(carpeta_segmento, "resultados_mtuem_segmento.xlsx")
  )

  status <- tibble::tibble(
    dimension = dimension,
    categoria = categoria,
    orden_categoria = orden_categoria,
    estado = "estimado",
    n_mnl_v9_grupo = nrow(base_segmento_pre),
    n_mtuem_final = nrow(data_est),
    metodo_elegido = metodo_elegido,
    logLik = as.numeric(logLik(res_cont)),
    return_code = extraer_code(res_cont)
  )

  list(
    status = status,
    resumen_base = resumen_base,
    trazabilidad = trazabilidad,
    parametros = tabla_parametros,
    resumen_modelo = resumen_modelo,
    valores_ic = valores_ic,
    percentiles = percentiles,
    tabla_pablo = tabla_pablo,
    start_estimado = res_cont$estimate[c("PH", "tw", "th1")]
  )
}

# ---------------------------------------------------------------
# 11. Estimar muestra general
# ---------------------------------------------------------------

resultados <- list()

resultado_general <- estimar_mtuem_segmento(
  base_segmento_pre = base_cont_persona,
  carpeta_segmento = file.path(carpeta_salida, "01_muestra_general"),
  dimension = "muestra_general",
  categoria = "Muestra general",
  orden_categoria = 1,
  start_base = NULL
)

resultados[["muestra_general__general"]] <- resultado_general

start_general <- resultado_general$start_estimado
if (is.null(start_general) || any(!is.finite(start_general))) {
  start_general <- c(PH = 0.2633, tw = 0.2970, th1 = 0.80)
}

# ---------------------------------------------------------------
# 12. Estimar modelos por grupos
# ---------------------------------------------------------------

definicion_grupos <- list(
  edad = list(var = "grupo_edad", niveles = ordenes_categorias$edad),
  sexo = list(var = "grupo_sexo", niveles = ordenes_categorias$sexo),
  ingresos = list(var = "grupo_ingreso", niveles = ordenes_categorias$ingresos),
  territorio = list(var = "grupo_territorio", niveles = ordenes_categorias$territorio),
  cluster_tiempo = list(var = "grupo_cluster_tiempo", niveles = ordenes_categorias$cluster_tiempo),
  cluster_modo = list(var = "grupo_cluster_modo", niveles = ordenes_categorias$cluster_modo)
)

for (dim_nombre in names(definicion_grupos)) {
  var_grupo <- definicion_grupos[[dim_nombre]]$var
  niveles <- definicion_grupos[[dim_nombre]]$niveles

  carpeta_dim <- file.path(carpeta_salida, "02_grupos", nombre_archivo_seguro(dim_nombre))
  crear_dir(carpeta_dim)

  for (i in seq_along(niveles)) {
    cat_val <- niveles[i]
    base_seg <- base_cont_persona %>%
      filter(.data[[var_grupo]] == cat_val)

    # Si no hay datos para cluster_modo por falta de asignacion, se guarda nota y continua.
    carpeta_seg <- file.path(carpeta_dim, nombre_archivo_seguro(cat_val))

    if (nrow(base_seg) == 0) {
      crear_dir(carpeta_seg)
      nota <- tibble::tibble(
        dimension = dim_nombre,
        categoria = cat_val,
        estado = "no_estimado",
        motivo = "Sin observaciones para esta categoria",
        n_final = 0
      )
      readr::write_csv(nota, file.path(carpeta_seg, "modelo_no_estimado.csv"))
      resultados[[paste(dim_nombre, cat_val, sep = "__")]] <- list(
        status = nota,
        resumen_base = tibble(), trazabilidad = tibble(), parametros = tibble(),
        resumen_modelo = tibble(), valores_ic = tibble(), percentiles = tibble(), tabla_pablo = tibble()
      )
      next
    }

    res <- estimar_mtuem_segmento(
      base_segmento_pre = base_seg,
      carpeta_segmento = carpeta_seg,
      dimension = dim_nombre,
      categoria = cat_val,
      orden_categoria = i,
      start_base = start_general
    )

    resultados[[paste(dim_nombre, cat_val, sep = "__")]] <- res
  }
}

# ---------------------------------------------------------------
# 13. Consolidados
# ---------------------------------------------------------------

consolidado_status <- bind_rows(lapply(resultados, function(x) x$status))
consolidado_resumen_base <- bind_rows(lapply(resultados, function(x) x$resumen_base))
consolidado_trazabilidad <- bind_rows(lapply(resultados, function(x) x$trazabilidad))
consolidado_parametros <- bind_rows(lapply(resultados, function(x) x$parametros))
consolidado_resumen_modelo <- bind_rows(lapply(resultados, function(x) x$resumen_modelo))
consolidado_valores_ic <- bind_rows(lapply(resultados, function(x) x$valores_ic))
consolidado_percentiles <- bind_rows(lapply(resultados, function(x) x$percentiles))
consolidado_tabla_pablo <- bind_rows(lapply(resultados, function(x) x$tabla_pablo))

readr::write_csv(consolidado_status, file.path(carpeta_salida, "03_consolidados", "00_status_estimaciones.csv"))
readr::write_csv(consolidado_resumen_base, file.path(carpeta_salida, "03_consolidados", "01_resumen_base_consolidado.csv"))
readr::write_csv(consolidado_trazabilidad, file.path(carpeta_salida, "03_consolidados", "01_trazabilidad_consolidada.csv"))
readr::write_csv(consolidado_parametros, file.path(carpeta_salida, "03_consolidados", "04_parametros_consolidado.csv"))
readr::write_csv(consolidado_resumen_modelo, file.path(carpeta_salida, "03_consolidados", "04_resumen_modelo_consolidado.csv"))
readr::write_csv(consolidado_valores_ic, file.path(carpeta_salida, "03_consolidados", "04_valores_tiempo_IC_consolidado.csv"))
readr::write_csv(consolidado_percentiles, file.path(carpeta_salida, "03_consolidados", "04_percentiles_consolidado.csv"))
readr::write_csv(consolidado_tabla_pablo, file.path(carpeta_salida, "03_consolidados", "04_tabla_estilo_pablo_consolidada.csv"))

writexl::write_xlsx(
  list(
    status_estimaciones = consolidado_status,
    valores_tiempo_IC = consolidado_valores_ic,
    tabla_estilo_pablo = consolidado_tabla_pablo,
    parametros = consolidado_parametros,
    resumen_modelo = consolidado_resumen_modelo,
    resumen_base = consolidado_resumen_base,
    trazabilidad = consolidado_trazabilidad,
    percentiles = consolidado_percentiles,
    recodificacion_modos = control_recodificacion,
    frecuencia_modos = freq_modos_muestra
  ),
  path = file.path(carpeta_salida, "03_consolidados", "v3_mtuem_consolidado_resultados.xlsx")
)

# Archivo general solicitado con todos los resultados propios de la muestra general y grupos.
writexl::write_xlsx(
  list(
    status_estimaciones = consolidado_status,
    valores_tiempo_IC = consolidado_valores_ic,
    tabla_estilo_pablo = consolidado_tabla_pablo,
    parametros = consolidado_parametros,
    resumen_modelo = consolidado_resumen_modelo,
    resumen_base = consolidado_resumen_base,
    trazabilidad = consolidado_trazabilidad,
    percentiles = consolidado_percentiles
  ),
  path = file.path(carpeta_salida, "v3_mtuem_resultados.xlsx")
)

# ---------------------------------------------------------------
# 14. Graficos consolidados
# ---------------------------------------------------------------

graficar_indicador_por_dimension <- function(df_valores, dimension_obj, indicador_obj, nombre_archivo, titulo, ylab) {
  df_plot <- df_valores %>%
    filter(dimension == dimension_obj, indicador == indicador_obj, is.finite(media)) %>%
    arrange(orden_categoria) %>%
    mutate(categoria = factor(categoria, levels = unique(categoria)))

  if (nrow(df_plot) == 0) return(invisible(NULL))

  g <- ggplot(df_plot, aes(x = categoria, y = media)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = ic95_inf, ymax = ic95_sup), width = 0.15) +
    labs(title = titulo, x = NULL, y = ylab) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  ggsave(file.path(carpeta_salida, "04_graficos", nombre_archivo), g, width = 8.5, height = 5, dpi = 300)
}

for (dim_nombre in names(definicion_grupos)) {
  graficar_indicador_por_dimension(
    consolidado_valores_ic,
    dim_nombre,
    "VoL_predicho",
    paste0("VoL_predicho_IC_", nombre_archivo_seguro(dim_nombre), ".png"),
    paste0("VoL predicho con IC 95% por ", dim_nombre),
    "VoL predicho"
  )
  graficar_indicador_por_dimension(
    consolidado_valores_ic,
    dim_nombre,
    "VTAW_predicho",
    paste0("VTAW_predicho_IC_", nombre_archivo_seguro(dim_nombre), ".png"),
    paste0("VTAW predicho con IC 95% por ", dim_nombre),
    "VTAW predicho"
  )
  graficar_indicador_por_dimension(
    consolidado_valores_ic,
    dim_nombre,
    "w",
    paste0("w_IC_", nombre_archivo_seguro(dim_nombre), ".png"),
    paste0("Salario horario con IC 95% por ", dim_nombre),
    "Salario horario"
  )
}

# Grafico combinado de VoL predicho para todas las dimensiones de grupos.
df_vol_todos <- consolidado_valores_ic %>%
  filter(indicador == "VoL_predicho", dimension != "muestra_general", is.finite(media)) %>%
  mutate(
    dimension = factor(dimension, levels = names(definicion_grupos)),
    etiqueta = categoria
  ) %>%
  arrange(dimension, orden_categoria)

if (nrow(df_vol_todos) > 0) {
  g_vol_todos <- ggplot(df_vol_todos, aes(x = etiqueta, y = media)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = ic95_inf, ymax = ic95_sup), width = 0.15) +
    facet_wrap(~dimension, scales = "free_x") +
    labs(
      title = "VoL predicho con IC 95% por grupos",
      x = NULL,
      y = "VoL predicho"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  ggsave(file.path(carpeta_salida, "04_graficos", "VoL_predicho_IC_todos_los_grupos.png"), g_vol_todos, width = 13, height = 8, dpi = 300)
}

# ---------------------------------------------------------------
# 15. Cierre
# ---------------------------------------------------------------

cat("\nV3 MTUEM terminado correctamente.\n")
cat("\nResultados guardados en:\n")
print(carpeta_salida)
cat("\nArchivo consolidado principal:\n")
print(file.path(carpeta_salida, "v3_mtuem_resultados.xlsx"))
cat("\nArchivo consolidado detallado:\n")
print(file.path(carpeta_salida, "03_consolidados", "v3_mtuem_consolidado_resultados.xlsx"))

