###### 0. Limpieza y controles ######

rm(list = ls())

archivo_base <- "enut_ii.xlsx"
hoja_base <- "enut_ii"

carpeta_salida <- file.path("prueba", "v5 - joint final")
carpeta_mnl <- file.path("mnl", "resultados v2")
carpeta_mtuem <- "mtuem"
carpeta_mtuem_general <- file.path(carpeta_mtuem, "01_muestra_general")
carpeta_mtuem_diag <- file.path(carpeta_mtuem, "00_base_y_diagnosticos")

USAR_RDS_EXISTENTE <- FALSE
USAR_CHECKPOINT_PASO3 <- TRUE

# Version v5: solo Paso 3 del modelo joint sin correlacion.
# Estrategia opcion 5: escalamiento progresivo de DEoptim + NM.
# La idea es buscar primero cerca del punto inicial, luego ampliar gradualmente el rango,
# puliendo con NM despues de cada fase y dejando un NM final largo.
# No se usan limites de tiempo. Pensado para correr en un computador mas potente.

ITER_DEOPTIM_MEDIO_AMPLIO <- 120
NP_DEOPTIM_MEDIO_AMPLIO <- 60
DECONST_MEDIO_AMPLIO <- 1.00
TRACE_DEOPTIM <- FALSE   # cambiar a TRUE solo si quieres que DEoptim imprima mucho en consola

ITER_NM_POST_DEOPTIM <- 30000
ITER_NM_FINAL <- 50000

# Si DEoptim encuentra algo peor que el start, se conserva el start para el NM.
TOL_MEJORA_LLIK <- 1e-6

T_RATIO_CORTE <- 1.96
set.seed(42)

# Controles propios de V5
FASES_DEOPTIM_V5 <- tibble::tribble(
  ~fase, ~descripcion, ~deconst, ~NP, ~itermax,
  1L, "DEoptim local estrecho", 0.25, 30L, 30L,
  2L, "DEoptim local medio",    0.50, 40L, 50L,
  3L, "DEoptim mas amplio",     1.00, 60L, 80L
)

ITER_NM_DESPUES_DE_FASE <- 8000
ITER_NM_FINAL_V5 <- 50000
GUARDAR_MODELO_COMPLETO_FINAL <- FALSE


###### 1. Paquetes ######

options(repos = c(CRAN = "https://cloud.r-project.org"))

instalar_si_falta <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete, dependencies = TRUE)
  }
}

# dejo tambien las dependencias que normalmente carga nmm
paquetes <- c(
  "readxl", "readr", "dplyr", "tidyr", "tibble", "stringr", "purrr",
  "writexl", "ggplot2", "numDeriv", "DEoptim", "maxLik", "scales",
  "systemfit", "AER", "mlogit", "Hmisc", "gsubfn", "abind", "Rdpack",
  "plyr", "data.table", "magrittr", "car", "carData", "lmtest", "zoo", "R.utils"
)

for (p in paquetes) instalar_si_falta(p)

if (!requireNamespace("nmm", quietly = TRUE)) {
  archivo_nmm <- file.path(getwd(), "nmm_0.9.tar.gz")
  if (!file.exists(archivo_nmm)) {
    stop("No esta instalado nmm y no encontre nmm_0.9.tar.gz en la carpeta de trabajo.")
  }
  install.packages(archivo_nmm, repos = NULL, type = "source")
}

for (p in paquetes) library(p, character.only = TRUE)
library("nmm", character.only = TRUE)

###### 2. Carpetas ######

crear_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

crear_dir(carpeta_salida)

carpeta_00 <- file.path(carpeta_salida, "00_inputs_y_base")
carpeta_01 <- file.path(carpeta_salida, "01_modelos_individuales_reutilizados")
carpeta_02 <- file.path(carpeta_salida, "02_paso3_joint_sin_correlacion")
carpeta_03 <- file.path(carpeta_salida, "03_paso4_joint_con_correlacion")
carpeta_04 <- file.path(carpeta_salida, "04_valores_tiempo")
carpeta_05 <- file.path(carpeta_salida, "05_metricas_ajuste")
carpeta_06 <- file.path(carpeta_salida, "06_comparacion_modelos")
carpeta_07 <- file.path(carpeta_salida, "07_logs")

carpeta_check <- file.path(carpeta_03, "00_checkpoints")
carpeta_check_inicio <- file.path(carpeta_check, "01_inicio_paso4")
carpeta_check_de <- file.path(carpeta_check, "02_deoptim")
carpeta_check_nm <- file.path(carpeta_check, "03_nm_post_deoptim")
carpeta_check_best <- file.path(carpeta_check, "04_mejor_modelo")
carpeta_check_logs <- file.path(carpeta_check, "logs")

for (x in c(carpeta_00, carpeta_01, carpeta_02, carpeta_03, carpeta_04,
            carpeta_05, carpeta_06, carpeta_07, carpeta_check,
            carpeta_check_inicio, carpeta_check_de, carpeta_check_nm,
            carpeta_check_best, carpeta_check_logs)) {
  crear_dir(x)
}

readr::write_csv(
  tibble::tibble(
    control = c(
      "ITER_DEOPTIM_MEDIO_AMPLIO",
      "NP_DEOPTIM_MEDIO_AMPLIO",
      "DECONST_MEDIO_AMPLIO",
      "TRACE_DEOPTIM",
      "ITER_NM_POST_DEOPTIM",
      "ITER_NM_FINAL",
      "TOL_MEJORA_LLIK"
    ),
    valor = c(
      ITER_DEOPTIM_MEDIO_AMPLIO,
      NP_DEOPTIM_MEDIO_AMPLIO,
      DECONST_MEDIO_AMPLIO,
      TRACE_DEOPTIM,
      ITER_NM_POST_DEOPTIM,
      ITER_NM_FINAL,
      TOL_MEJORA_LLIK
    )
  ),
  file.path(carpeta_00, "00_controles_v4_DEoptim_medio_amplio_NM.csv")
)

###### 3. Funciones auxiliares ######

num <- function(x) suppressWarnings(as.numeric(as.character(x)))

num0 <- function(x) dplyr::coalesce(num(x), 0)

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

archivo_requerido <- function(path, descripcion = NULL) {
  if (!file.exists(path)) {
    msg <- paste0("No se encontro el archivo requerido: ", path)
    if (!is.null(descripcion)) msg <- paste0(msg, "\n", descripcion)
    stop(msg)
  }
  path
}

buscar_archivo <- function(candidatos, descripcion = "archivo") {
  candidatos <- unique(candidatos)
  encontrados <- candidatos[file.exists(candidatos)]
  if (length(encontrados) == 0) {
    stop(paste0("No se encontro ", descripcion, ". Rutas revisadas:\n", paste(candidatos, collapse = "\n")))
  }
  encontrados[1]
}

leer_csv_seguro <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
}

extraer_loglik <- function(obj) {
  ll <- tryCatch(as.numeric(logLik(obj)), error = function(e) NA_real_)
  if (is.finite(ll)) return(ll)
  salida <- tryCatch(capture.output(summary(obj)), error = function(e) character(0))
  linea <- salida[grepl("Log-Likelihood", salida)]
  if (length(linea) > 0) {
    valor <- stringr::str_extract(linea[1], "-?[0-9]+\\.?[0-9]*")
    return(num(valor))
  }
  NA_real_
}

extraer_code <- function(obj) {
  out <- tryCatch(obj$code, error = function(e) NA_real_)
  if (length(out) == 0 || is.null(out)) return(NA_real_)
  as.numeric(out[1])
}

modelo_loglik_ok <- function(obj) {
  if (is.null(obj)) return(FALSE)
  is.finite(extraer_loglik(obj))
}

modelo_convergio <- function(obj) {
  if (!modelo_loglik_ok(obj)) return(FALSE)
  cd <- extraer_code(obj)
  if (is.na(cd)) return(TRUE)
  cd == 0
}

extraer_coeficientes_modelo <- function(modelo) {
  if (is.null(modelo)) return(tibble())
  est <- tryCatch(modelo$estimate, error = function(e) NULL)
  if (is.null(est)) return(tibble())
  se <- tryCatch(modelo$se, error = function(e) NULL)
  if (is.null(se) || length(se) != length(est)) se <- rep(NA_real_, length(est))
  tibble(
    parametro = names(est),
    Estimate = as.numeric(est),
    Std_error = as.numeric(se),
    t_ratio = as.numeric(est) / as.numeric(se),
    abs_t_ratio = abs(t_ratio)
  )
}

obtener_vcov <- function(modelo) {
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

safe_div <- function(x, y) {
  out <- x / y
  out[!is.finite(out)] <- NA_real_
  out
}

completar_start <- function(start_base, orden, fallback = NULL) {
  out <- rep(NA_real_, length(orden))
  names(out) <- orden

  if (!is.null(fallback)) {
    comunes_f <- intersect(names(fallback), orden)
    out[comunes_f] <- fallback[comunes_f]
  }

  if (!is.null(start_base)) {
    comunes_b <- intersect(names(start_base), orden)
    out[comunes_b] <- start_base[comunes_b]
  }

  out[is.na(out) | !is.finite(out)] <- 0
  out
}

registrar_tiempo <- function(nombre, inicio, fin, obj = NULL, carpeta = carpeta_07) {
  tibble(
    paso = nombre,
    inicio = as.character(inicio),
    fin = as.character(fin),
    duracion_min = round(as.numeric(difftime(fin, inicio, units = "mins")), 3),
    logLik = extraer_loglik(obj),
    code = extraer_code(obj),
    convergio = modelo_convergio(obj)
  ) %>%
    readr::write_csv(file.path(carpeta, paste0(nombre, "_tiempo.csv")))
}

guardar_log_modelo <- function(nombre, expr, carpeta, archivo_rds, timeout_seg = NULL) {
  crear_dir(carpeta)
  archivo_log <- file.path(carpeta, paste0(nombre, "_log.txt"))

  if (isTRUE(USAR_RDS_EXISTENTE) && file.exists(archivo_rds)) {
    cat("Usando RDS existente:", archivo_rds, "\n")
    return(readRDS(archivo_rds))
  }

  inicio <- Sys.time()
  sink(archivo_log, split = TRUE)
  cat("====================================================\n")
  cat("Ejecutando:", nombre, "\n")
  cat("Inicio:", format(inicio, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("====================================================\n")

  obj <- tryCatch(
    {
      if (!is.null(timeout_seg) && is.finite(timeout_seg)) {
        cat("Limite de tiempo de esta etapa:", round(timeout_seg / 3600, 2), "horas\n")
        R.utils::withTimeout(eval(expr), timeout = timeout_seg, onTimeout = "error")
      } else {
        eval(expr)
      }
    },
    TimeoutException = function(e) {
      cat("\nTIMEOUT:\n")
      cat("La etapa supero el limite de tiempo definido y fue interrumpida.\n")
      return(NULL)
    },
    error = function(e) {
      cat("\nERROR:\n")
      cat(conditionMessage(e), "\n")
      return(NULL)
    }
  )

  fin <- Sys.time()
  if (!is.null(obj)) {
    cat("\nResumen:\n")
    print(summary(obj))
    saveRDS(obj, archivo_rds)
  }
  cat("\nFin:", format(fin, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Duracion minutos:", round(as.numeric(difftime(fin, inicio, units = "mins")), 2), "\n")
  sink()

  registrar_tiempo(nombre, inicio, fin, obj)
  obj
}

calcular_ll_nulo_constantes <- function(choice) {
  tab <- table(choice)
  n <- sum(tab)
  p <- as.numeric(tab) / n
  sum(as.numeric(tab) * log(p))
}

ll_normal_multivariado_nulo <- function(df, vars) {
  dat <- df %>% dplyr::select(dplyr::all_of(vars)) %>% as.data.frame()
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  n <- nrow(dat)
  g <- length(vars)
  if (n <= g + 2) return(NA_real_)

  mu <- colMeans(dat)
  res <- sweep(as.matrix(dat), 2, mu, "-")
  sigma <- crossprod(res) / n
  sigma <- sigma + diag(1e-8, g)
  inv_sigma <- tryCatch(solve(sigma), error = function(e) NULL)
  det_sigma <- tryCatch(det(sigma), error = function(e) NA_real_)
  if (is.null(inv_sigma) || !is.finite(det_sigma) || det_sigma <= 0) return(NA_real_)

  qf <- rowSums((res %*% inv_sigma) * res)
  sum(-0.5 * (g * log(2 * pi) + log(det_sigma) + qf))
}

rho2_ajustado <- function(ll, ll0, k) {
  if (!is.finite(ll) || !is.finite(ll0) || ll0 == 0 || !is.finite(k)) return(NA_real_)
  1 - ((ll - k) / ll0)
}

rho2_simple <- function(ll, ll0) {
  if (!is.finite(ll) || !is.finite(ll0) || ll0 == 0) return(NA_real_)
  1 - (ll / ll0)
}

###### 4. Inputs ######

ruta_enut <- archivo_requerido(archivo_base, "Debe estar en el directorio de trabajo.")

ruta_mnl_modelo <- buscar_archivo(
  c(
    file.path(carpeta_mnl, "v10_modelo_final.rds"),
    file.path(carpeta_mnl, "v10_mnl_final.rds"),
    file.path(carpeta_mnl, "mnl_v2_modelo_final.rds"),
    list.files(carpeta_mnl, pattern = "(modelo_final|mnl_final).*\\.rds$", full.names = TRUE, recursive = FALSE)
  ),
  "RDS del MNL final"
)

ruta_mnl_coef <- buscar_archivo(
  c(
    file.path(carpeta_mnl, "v10_coeficientes_final.csv"),
    file.path(carpeta_mnl, "mnl_v2_coeficientes_final.csv"),
    list.files(carpeta_mnl, pattern = "coeficientes.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "coeficientes del MNL final"
)

ruta_mnl_ajuste <- buscar_archivo(
  c(
    file.path(carpeta_mnl, "v10_tabla_ajuste_final.csv"),
    file.path(carpeta_mnl, "mnl_v2_tabla_ajuste_final.csv"),
    list.files(carpeta_mnl, pattern = "tabla_ajuste.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "tabla de ajuste del MNL final"
)

ruta_mnl_par_d <- buscar_archivo(
  c(
    file.path(carpeta_mnl, "v10_par_d.csv"),
    file.path(carpeta_mnl, "mnl_v2_par_d.csv"),
    list.files(carpeta_mnl, pattern = "par_d.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "par_d del MNL final"
)

ruta_mnl_start <- buscar_archivo(
  c(
    file.path(carpeta_mnl, "v10_start.csv"),
    file.path(carpeta_mnl, "mnl_v2_start.csv"),
    list.files(carpeta_mnl, pattern = "start.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "start del MNL final"
)

ruta_mnl_eq <- buscar_archivo(
  c(
    file.path(carpeta_mnl, "v10_eq_final.txt"),
    file.path(carpeta_mnl, "mnl_v2_eq_final.txt"),
    list.files(carpeta_mnl, pattern = "eq.*\\.txt$", full.names = TRUE, recursive = FALSE)
  ),
  "ecuaciones del MNL final"
)

ruta_mnl_base_larga <- buscar_archivo(
  c(
    file.path(carpeta_mnl, "04_base_mnl_larga_v10_desde_enut_ii.csv"),
    file.path(carpeta_mnl, "04_base_mnl_larga_mnl_v2_desde_enut_ii.csv"),
    list.files(carpeta_mnl, pattern = "base_mnl_larga.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "base larga del MNL final"
)

ruta_mnl_data_nmm <- buscar_archivo(
  c(
    file.path(carpeta_mnl, "03_data_nmm_v10_desde_enut_ii.csv"),
    file.path(carpeta_mnl, "03_data_nmm_mnl_v2_desde_enut_ii.csv"),
    list.files(carpeta_mnl, pattern = "data_nmm.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "data_nmm del MNL final"
)

ruta_mtuem_modelo <- buscar_archivo(
  c(
    file.path(carpeta_mtuem_general, "03_cont_Tw_Tf1_MEJOR.rds"),
    file.path(carpeta_mtuem_general, "mtuem_modelo_final.rds"),
    list.files(carpeta_mtuem_general, pattern = "(MEJOR|modelo_final).*\\.rds$", full.names = TRUE, recursive = FALSE)
  ),
  "RDS del MTUEM final"
)

ruta_mtuem_base <- buscar_archivo(
  c(
    file.path(carpeta_mtuem_general, "01_base_mtuem_continua_preparada.csv"),
    list.files(carpeta_mtuem_general, pattern = "base_mtuem.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "base continua del MTUEM final"
)

ruta_mtuem_param <- buscar_archivo(
  c(
    file.path(carpeta_mtuem_general, "04_parametros_mtuem.csv"),
    list.files(carpeta_mtuem_general, pattern = "parametros_mtuem.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "parametros del MTUEM final"
)

ruta_mtuem_resumen <- buscar_archivo(
  c(
    file.path(carpeta_mtuem_general, "04_resumen_mtuem.csv"),
    list.files(carpeta_mtuem_general, pattern = "resumen_mtuem.*\\.csv$", full.names = TRUE, recursive = FALSE)
  ),
  "resumen del MTUEM final"
)

inputs_usados <- tibble(
  insumo = c(
    "enut_ii", "mnl_modelo", "mnl_coeficientes", "mnl_ajuste", "mnl_par_d",
    "mnl_start", "mnl_eq", "mnl_base_larga", "mnl_data_nmm", "mtuem_modelo",
    "mtuem_base", "mtuem_parametros", "mtuem_resumen"
  ),
  ruta = c(
    ruta_enut, ruta_mnl_modelo, ruta_mnl_coef, ruta_mnl_ajuste, ruta_mnl_par_d,
    ruta_mnl_start, ruta_mnl_eq, ruta_mnl_base_larga, ruta_mnl_data_nmm, ruta_mtuem_modelo,
    ruta_mtuem_base, ruta_mtuem_param, ruta_mtuem_resumen
  )
)

readr::write_csv(inputs_usados, file.path(carpeta_00, "00_inputs_usados.csv"))

###### 5. Leer modelos individuales ######

modelo_mnl <- readRDS(ruta_mnl_modelo)
modelo_mtuem <- readRDS(ruta_mtuem_modelo)

coef_mnl <- leer_csv_seguro(ruta_mnl_coef)
ajuste_mnl <- leer_csv_seguro(ruta_mnl_ajuste)
par_d_tbl <- leer_csv_seguro(ruta_mnl_par_d)
start_mnl_tbl <- leer_csv_seguro(ruta_mnl_start)
param_mtuem_tbl <- leer_csv_seguro(ruta_mtuem_param)
resumen_mtuem_tbl <- leer_csv_seguro(ruta_mtuem_resumen)

eq_d <- readLines(ruta_mnl_eq, warn = FALSE)
par_d <- par_d_tbl[[1]] %>% as.character()

if (!"ASC1" %in% par_d) {
  par_d <- c("ASC1", par_d)
}

par_c <- c("PH", "tw", "th1")

ruta_eq_c_1 <- file.path(carpeta_mtuem_general, "02_eq_c_mtuem_Tw_Tf1.txt")
ruta_eq_c_2 <- file.path(carpeta_mtuem_diag, "02_eq_c_mtuem_Tw_Tf1.txt")
if (file.exists(ruta_eq_c_1)) {
  eq_c <- readLines(ruta_eq_c_1, warn = FALSE)
} else if (file.exists(ruta_eq_c_2)) {
  eq_c <- readLines(ruta_eq_c_2, warn = FALSE)
} else {
  eq_c <- c(
    "Tw ~ ((((PH) + (tw)) * (ta - Tc + 2) + (1 + (tw)) * (Ec/w - 2/w) - (1 + (PH))) + sqrt((((PH) + (tw)) * (ta - Tc + 2) + (1 + (tw)) * (Ec/w - 2/w) - (1 + (PH)))^2 - 4 * (1 + (PH) + (tw)) * (-(PH) * (ta - Tc + 2) + (1 - (tw) * (ta - Tc + 2)) * (2/w - Ec/w))))/(2 * (1 + (PH) + (tw)))",
    "Tf1 ~ (th1) * (ta - (Tw) - Tc)"
  )
  eq_c[-1] <- gsub("Tw", gsub(".*~", "", eq_c[1]), eq_c[-1])
}

eq_Tw_rhs <- gsub(".*~", "", eq_c[1])

saveRDS(modelo_mnl, file.path(carpeta_01, "mnl_v2_modelo_reutilizado.rds"))
saveRDS(modelo_mtuem, file.path(carpeta_01, "mtuem_modelo_reutilizado.rds"))
writeLines(eq_d, file.path(carpeta_01, "mnl_v2_eq_d.txt"))
writeLines(eq_c, file.path(carpeta_01, "mtuem_eq_c.txt"))
readr::write_csv(coef_mnl, file.path(carpeta_01, "mnl_v2_coeficientes_reutilizados.csv"))
readr::write_csv(param_mtuem_tbl, file.path(carpeta_01, "mtuem_parametros_reutilizados.csv"))

###### 6. Base conjunta ######

base_mnl_larga <- leer_csv_seguro(ruta_mnl_base_larga) %>%
  dplyr::mutate(id_persona = as.character(id_persona))
base_mtuem <- leer_csv_seguro(ruta_mtuem_base) %>%
  dplyr::mutate(id_persona = as.character(id_persona))
base_enut <- readxl::read_excel(ruta_enut, sheet = hoja_base) %>% tibble::as_tibble()

if (!"id_persona" %in% names(base_mnl_larga)) {
  stop("La base larga del MNL debe tener id_persona para unir con MTUEM.")
}
if (!"id_persona" %in% names(base_mtuem)) {
  stop("La base del MTUEM debe tener id_persona para unir con MNL.")
}

vars_cont <- c(
  "id_persona", "Tw", "Tf1", "Tf2", "Tc", "w", "Ec", "ta",
  "Ec_monetario", "Ec_trabajo_no_remunerado", "ingreso_libre_observado",
  "ingreso_potencial_neto_Ec"
)
vars_cont <- intersect(vars_cont, names(base_mtuem))

base_mtuem_sel <- base_mtuem %>%
  dplyr::select(dplyr::all_of(vars_cont)) %>%
  dplyr::distinct(id_persona, .keep_all = TRUE)

ids_interseccion <- intersect(unique(base_mnl_larga$id_persona), unique(base_mtuem_sel$id_persona))

base_joint <- base_mnl_larga %>%
  dplyr::filter(id_persona %in% ids_interseccion) %>%
  dplyr::left_join(base_mtuem_sel, by = "id_persona") %>%
  dplyr::arrange(id_persona, WeID) %>%
  dplyr::group_by(id_persona) %>%
  dplyr::mutate(
    PeID = dplyr::cur_group_id(),
    WeID = dplyr::row_number()
  ) %>%
  dplyr::ungroup()

for (j in 1:7) {
  avl_j <- paste0("avl_", j)
  chc_j <- paste0("chc_", j)
  wd_j <- paste0("wd_", j)
  if (!avl_j %in% names(base_joint)) base_joint[[avl_j]] <- 1
  if (!chc_j %in% names(base_joint)) base_joint[[chc_j]] <- ifelse(num(base_joint$choice) == j, 1, 0)
  if (!wd_j %in% names(base_joint)) base_joint[[wd_j]] <- 1
}
if (!"wc_1" %in% names(base_joint)) base_joint$wc_1 <- 1
if (!"wc_2" %in% names(base_joint) && length(eq_c) >= 2) base_joint$wc_2 <- 1

vars_nmm <- unique(c(
  "PeID", "WeID", "choice", "Tw", "Tf1", "Tf2", "Tc", "w", "Ec", "ta",
  "mod_TD", "mod_TC", "mod_ED", "mod_CP", "T_mod_10h", "female",
  "zona_centro", "zona_sur", "edad_65mas", "n_trabajadores", "vive_pareja",
  "educ_secundaria", "educ_tecnica", "educ_universitaria", "quintil_3", "quintil_4", "quintil_5",
  paste0("avl_", 1:7), paste0("chc_", 1:7), paste0("wd_", 1:7), "wc_1", "wc_2"
))
vars_nmm <- intersect(vars_nmm, names(base_joint))

vars_requeridas_modelo <- unique(c(
  "PeID", "WeID", "choice", "Tw", "Tf1", "Tc", "w", "Ec", "ta",
  unique(unlist(stringr::str_extract_all(paste(eq_d, collapse = " "), "[a-zA-Z_][a-zA-Z0-9_]*"))),
  paste0("avl_", 1:7), paste0("chc_", 1:7), paste0("wd_", 1:7), "wc_1"
))
vars_requeridas_modelo <- setdiff(vars_requeridas_modelo, c("ASC1", par_d, par_c, "sqrt", "exp", "log"))
vars_requeridas_modelo <- intersect(vars_requeridas_modelo, names(base_joint))
vars_nmm <- unique(c(vars_nmm, vars_requeridas_modelo))

data_nmm <- base_joint %>%
  dplyr::select(dplyr::all_of(vars_nmm)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ num(.))) %>%
  dplyr::arrange(PeID, WeID) %>%
  as.data.frame()

data_nmm[is.na(data_nmm)] <- 0

faltantes_joint <- setdiff(c("PeID", "WeID", "choice", "Tw", "Tf1", "Tc", "w", "Ec", "ta"), names(data_nmm))
if (length(faltantes_joint) > 0) {
  stop(paste0("Faltan variables basicas para el joint: ", paste(faltantes_joint, collapse = ", ")))
}

if (any(!is.finite(as.matrix(data_nmm)))) {
  stop("La base joint tiene valores no finitos despues de preparar data_nmm.")
}

base_cont_persona <- base_joint %>%
  dplyr::group_by(PeID, id_persona) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

readr::write_csv(base_joint, file.path(carpeta_00, "01_base_joint_larga_preparada.csv"))
readr::write_csv(as_tibble(data_nmm), file.path(carpeta_00, "02_data_nmm_joint_preparada.csv"))
readr::write_csv(base_cont_persona, file.path(carpeta_00, "03_base_continua_persona_joint.csv"))
readr::write_csv(tibble(id_persona = ids_interseccion), file.path(carpeta_00, "04_ids_interseccion_mnl_mtuem.csv"))

resumen_base <- tibble(
  indicador = c(
    "N personas MNL final",
    "N personas MTUEM final",
    "N personas interseccion joint",
    "N filas joint",
    "Promedio filas por persona",
    "Media Tw",
    "Media Tf1",
    "Media Tc",
    "Media w",
    "Media Ec"
  ),
  valor = c(
    length(unique(base_mnl_larga$id_persona)),
    length(unique(base_mtuem_sel$id_persona)),
    length(ids_interseccion),
    nrow(data_nmm),
    nrow(data_nmm) / length(unique(data_nmm$PeID)),
    mean(base_cont_persona$Tw, na.rm = TRUE),
    mean(base_cont_persona$Tf1, na.rm = TRUE),
    mean(base_cont_persona$Tc, na.rm = TRUE),
    mean(base_cont_persona$w, na.rm = TRUE),
    mean(base_cont_persona$Ec, na.rm = TRUE)
  )
)
readr::write_csv(resumen_base, file.path(carpeta_00, "00_resumen_base_joint.csv"))

###### 7. Valores iniciales ######

est_mnl_rds <- tryCatch(modelo_mnl$estimate, error = function(e) NULL)
if (is.null(est_mnl_rds)) est_mnl_rds <- numeric(0)

est_mnl_csv <- coef_mnl$Estimate
names(est_mnl_csv) <- coef_mnl$parametro

start_mnl_csv <- start_mnl_tbl[[2]]
names(start_mnl_csv) <- start_mnl_tbl[[1]]

est_mtuem_rds <- tryCatch(modelo_mtuem$estimate, error = function(e) NULL)
if (is.null(est_mtuem_rds)) est_mtuem_rds <- numeric(0)

if (all(c("parametro", "estimacion") %in% names(param_mtuem_tbl))) {
  est_mtuem_csv <- param_mtuem_tbl$estimacion
  names(est_mtuem_csv) <- param_mtuem_tbl$parametro
} else {
  est_mtuem_csv <- numeric(0)
}

orden_nmm <- c(sort(par_c), setdiff(sort(par_d), "ASC1"))

start_joint_base <- completar_start(
  start_base = c(est_mtuem_rds, est_mnl_rds),
  orden = orden_nmm,
  fallback = c(est_mtuem_csv, est_mnl_csv, start_mnl_csv)
)

readr::write_csv(
  tibble(parametro = names(start_joint_base), start = as.numeric(start_joint_base)),
  file.path(carpeta_00, "05_start_joint_desde_modelos_solos.csv")
)

saveRDS(start_joint_base, file.path(carpeta_00, "05_start_joint_desde_modelos_solos.rds"))

###### 8. Paso 3: joint sin correlacion, estimacion por bloques ######

carpeta_bloques <- file.path(carpeta_02, "00_estimacion_por_bloques")
carpeta_checkpoints <- file.path(carpeta_02, "01_checkpoints_parametros")
carpeta_funciones <- file.path(carpeta_02, "02_funciones_joint")
for (x in c(carpeta_bloques, carpeta_checkpoints, carpeta_funciones)) crear_dir(x)

# Reviso que el punto inicial del MTUEM entregue valores finitos.
base_test_start <- data_nmm
base_test_start$PH <- as.numeric(start_joint_base["PH"])
base_test_start$th1 <- as.numeric(start_joint_base["th1"])
base_test_start$tw <- as.numeric(start_joint_base["tw"])
base_test_start$Tw_hat_inicio <- tryCatch(eval(parse(text = eq_Tw_rhs), envir = base_test_start), error = function(e) rep(NA_real_, nrow(base_test_start)))

validacion_inicio <- tibble(
  indicador = c("PH_inicio", "tw_inicio", "th1_inicio", "n_Tw_hat_no_finito", "min_Tw_hat", "max_Tw_hat"),
  valor = c(
    as.numeric(start_joint_base["PH"]),
    as.numeric(start_joint_base["tw"]),
    as.numeric(start_joint_base["th1"]),
    sum(!is.finite(base_test_start$Tw_hat_inicio)),
    suppressWarnings(min(base_test_start$Tw_hat_inicio, na.rm = TRUE)),
    suppressWarnings(max(base_test_start$Tw_hat_inicio, na.rm = TRUE))
  )
)
readr::write_csv(validacion_inicio, file.path(carpeta_02, "00_validacion_inicio_mtuem.csv"))

if (any(!is.finite(base_test_start$Tw_hat_inicio))) {
  stop("El punto inicial del MTUEM genera Tw_hat no finito. Revisar 00_validacion_inicio_mtuem.csv.")
}

readr::write_csv(
  tibble::tibble(parametro = names(start_joint_base), start = as.numeric(start_joint_base)),
  file.path(carpeta_02, "00_start_paso3.csv")
)

# Creo una vez la funcion de log-verosimilitud conjunta sin estimar con nmm.
# Luego uso esa funcion para optimizar subconjuntos de parametros.
cat("\nConstruyendo funcion joint sin correlacion desde nmm...\n")
obj_funciones <- nmm(
  data = data_nmm,
  eq_type = "joint",
  eq_c = eq_c,
  par_c = par_c,
  eq_d = eq_d,
  par_d = par_d,
  start_v = start_joint_base,
  corrl = FALSE,
  weight_paths = FALSE,
  weight_paths_cont = TRUE,
  fixed_term = FALSE,
  estimate = FALSE,
  numerical_deriv = FALSE
)

funciones_joint <- attributes(obj_funciones)$functions
joint_func <- funciones_joint$jfunc
writeLines("Funciones joint construidas en memoria. No se guardan como RDS para evitar archivos pesados.", file.path(carpeta_funciones, "00_funciones_joint_sin_correlacion.txt"))

orden_parametros <- names(start_joint_base)
params_mtuem <- intersect(c("PH", "tw", "th1"), orden_parametros)
params_mnl <- setdiff(orden_parametros, params_mtuem)
params_todos <- orden_parametros

ll_seguro <- function(par_full) {
  par_full <- as.numeric(par_full)
  names(par_full) <- orden_parametros
  val <- try(joint_func(par_full), silent = TRUE)
  if (inherits(val, "try-error") || length(val) == 0) return(NA_real_)
  val <- suppressWarnings(as.numeric(val[1]))
  if (!is.finite(val)) return(NA_real_)
  val
}

retornar_code <- function(res) {
  out <- tryCatch(res$code, error = function(e) NA_real_)
  if (length(out) == 0 || is.null(out)) NA_real_ else as.numeric(out[1])
}

retornar_mensaje <- function(res) {
  out <- tryCatch(maxLik::returnCode(res), error = function(e) NA_character_)
  if (length(out) == 0 || is.null(out)) NA_character_ else as.character(out[1])
}

extraer_vcov_maxlik <- function(res) {
  tryCatch(stats::vcov(res), error = function(e) NULL)
}

coeficientes_desde_estado <- function(par_full, vc = NULL) {
  se <- rep(NA_real_, length(par_full))
  names(se) <- names(par_full)
  if (is.matrix(vc) && nrow(vc) == length(par_full) && ncol(vc) == length(par_full)) {
    se_tmp <- suppressWarnings(sqrt(diag(vc)))
    se[seq_along(se_tmp)] <- se_tmp
  }
  tibble::tibble(
    parametro = names(par_full),
    Estimate = as.numeric(par_full),
    Std_error = as.numeric(se),
    t_ratio = Estimate / Std_error,
    abs_t_ratio = abs(t_ratio)
  )
}

guardar_estado_liviano <- function(nombre, par_full, ll, res = NULL, etapa = NA_character_, notas = "") {
  par_full <- as.numeric(par_full)
  names(par_full) <- orden_parametros
  vc <- NULL
  if (!is.null(res) && length(tryCatch(res$estimate, error = function(e) numeric(0))) == length(par_full)) {
    vc <- extraer_vcov_maxlik(res)
  }
  obj_liviano <- list(
    tipo = "paso3_joint_sin_correlacion_estimacion_por_bloques",
    etapa = etapa,
    notas = notas,
    momento = as.character(Sys.time()),
    logLik = as.numeric(ll),
    code = if (!is.null(res)) retornar_code(res) else NA_real_,
    mensaje = if (!is.null(res)) retornar_mensaje(res) else NA_character_,
    convergio = if (!is.null(res)) isTRUE(retornar_code(res) == 0) else FALSE,
    estimate = par_full,
    vcov = vc
  )
  saveRDS(obj_liviano, file.path(carpeta_checkpoints, paste0(nombre, ".rds")))
  readr::write_csv(
    tibble::tibble(parametro = names(par_full), valor = as.numeric(par_full)),
    file.path(carpeta_checkpoints, paste0(nombre, "_parametros.csv"))
  )
  readr::write_csv(
    coeficientes_desde_estado(par_full, vc),
    file.path(carpeta_checkpoints, paste0(nombre, "_coeficientes.csv"))
  )
  invisible(obj_liviano)
}

# Corre maxLik solo sobre un subconjunto de parametros; el resto queda fijo.
optimizar_subconjunto <- function(nombre, start_full, free_params, metodo = "NM", iterlim = 1000, aceptar_si_mejora = TRUE) {
  start_full <- as.numeric(start_full)
  names(start_full) <- orden_parametros
  free_params <- intersect(free_params, orden_parametros)
  if (length(free_params) == 0) stop("No hay parametros libres para ", nombre)

  carpeta_etapa <- file.path(carpeta_bloques, nombre_archivo_seguro(nombre))
  crear_dir(carpeta_etapa)
  archivo_log <- file.path(carpeta_etapa, paste0(nombre_archivo_seguro(nombre), "_log.txt"))

  ll_inicio <- ll_seguro(start_full)
  inicio <- Sys.time()

  sink(archivo_log, split = TRUE)
  cat("====================================================\n")
  cat("Etapa:", nombre, "\n")
  cat("Metodo:", metodo, "| iterlim:", iterlim, "\n")
  cat("Parametros libres:", length(free_params), "\n")
  cat("Inicio:", format(inicio, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("LogLik inicial:", ll_inicio, "\n")
  cat("====================================================\n")

  f_free <- function(x) {
    full <- start_full
    full[free_params] <- as.numeric(x)
    val <- ll_seguro(full)
    if (!is.finite(val)) return(-1e100)
    val
  }

  start_free <- start_full[free_params]
  res <- try(maxLik::maxLik(f_free, start = start_free, method = metodo, iterlim = iterlim), silent = TRUE)

  estado <- "error"
  par_candidato <- start_full
  ll_candidato <- ll_inicio
  code <- NA_real_
  mensaje <- NA_character_
  mejora <- FALSE

  if (!inherits(res, "try-error") && !is.null(res$estimate) && all(is.finite(res$estimate))) {
    par_candidato[free_params] <- as.numeric(res$estimate)
    ll_candidato <- ll_seguro(par_candidato)
    code <- retornar_code(res)
    mensaje <- retornar_mensaje(res)
    mejora <- is.finite(ll_candidato) && is.finite(ll_inicio) && (ll_candidato > ll_inicio + TOL_MEJORA_LLIK)
    if (is.finite(ll_candidato) && (!aceptar_si_mejora || ll_candidato >= ll_inicio - TOL_MEJORA_LLIK)) {
      estado <- "valido_aceptado"
    } else {
      estado <- "valido_rechazado_no_mejora"
      par_candidato <- start_full
      ll_candidato <- ll_inicio
    }
  } else {
    cat("La etapa produjo error o resultado no valido.\n")
    if (inherits(res, "try-error")) cat(as.character(res), "\n")
    res <- NULL
  }

  fin <- Sys.time()
  cat("\nFin:", format(fin, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Duracion minutos:", round(as.numeric(difftime(fin, inicio, units = "mins")), 3), "\n")
  cat("Estado:", estado, "\n")
  cat("Code:", code, "\n")
  cat("Mensaje:", mensaje, "\n")
  cat("LogLik candidato:", ll_candidato, "\n")
  cat("Mejora frente al inicio:", ll_candidato - ll_inicio, "\n")
  sink()

  readr::write_csv(
    tibble::tibble(parametro = names(par_candidato), valor = as.numeric(par_candidato)),
    file.path(carpeta_etapa, "parametros_salida.csv")
  )

  if (!is.null(res)) {
    # Para evitar RDS gigantes, guardo solo el objeto liviano, no el maxLik completo.
    guardar_estado_liviano(nombre_archivo_seguro(nombre), par_candidato, ll_candidato, NULL, nombre, estado)
  }

  tibble_resumen <- tibble::tibble(
    etapa = nombre,
    metodo = metodo,
    iterlim = iterlim,
    parametros_libres = length(free_params),
    inicio = as.character(inicio),
    fin = as.character(fin),
    duracion_min = round(as.numeric(difftime(fin, inicio, units = "mins")), 3),
    logLik_inicio = ll_inicio,
    logLik_salida = ll_candidato,
    mejora = ll_candidato - ll_inicio,
    code = code,
    mensaje = mensaje,
    estado = estado
  )
  readr::write_csv(tibble_resumen, file.path(carpeta_etapa, "resumen_etapa.csv"))

  list(
    par = par_candidato,
    ll = ll_candidato,
    res = res,
    resumen = tibble_resumen,
    estado = estado
  )
}

historial <- tibble::tibble()
actual_par <- start_joint_base
actual_ll <- ll_seguro(actual_par)
mejor_par <- actual_par
mejor_ll <- actual_ll
mejor_res <- NULL

registrar <- function(out) {
  historial <<- dplyr::bind_rows(historial, out$resumen)
  readr::write_csv(historial, file.path(carpeta_02, "03_historial_estimacion_por_bloques.csv"))

  if (is.finite(out$ll) && (!is.finite(mejor_ll) || out$ll > mejor_ll + TOL_MEJORA_LLIK)) {
    mejor_par <<- out$par
    mejor_ll <<- out$ll
    mejor_res <<- out$res
    guardar_estado_liviano("mejor_resultado_actual", mejor_par, mejor_ll, NULL, out$resumen$etapa[1], "mejor hasta esta etapa")
  }

  actual_par <<- out$par
  actual_ll <<- out$ll
  guardar_estado_liviano("ultimo_resultado_actual", actual_par, actual_ll, NULL, out$resumen$etapa[1], "ultimo resultado aceptado")
}

cat("\nLogLik start Paso 3:", actual_ll, "\n")
guardar_estado_liviano("00_start_paso3", actual_par, actual_ll, NULL, "start", "parametros iniciales desde modelos solos")


carpeta_de <- file.path(carpeta_02, "00_DEoptim_progresivo")
carpeta_nm <- file.path(carpeta_02, "01_NM_por_fase_y_final")
carpeta_final <- file.path(carpeta_02, "02_resultado_final")
for (x in c(carpeta_de, carpeta_nm, carpeta_final)) crear_dir(x)

readr::write_csv(
  FASES_DEOPTIM_V5,
  file.path(carpeta_00, "00_controles_v5_DEoptim_progresivo_NM.csv")
)

readr::write_csv(
  tibble::tibble(
    control = c("ITER_NM_DESPUES_DE_FASE", "ITER_NM_FINAL_V5", "TRACE_DEOPTIM", "TOL_MEJORA_LLIK"),
    valor = c(ITER_NM_DESPUES_DE_FASE, ITER_NM_FINAL_V5, TRACE_DEOPTIM, TOL_MEJORA_LLIK)
  ),
  file.path(carpeta_00, "00_controles_v5_NM.csv")
)

###### 8. DEoptim progresivo + NM por fase ######

f_min_de <- function(par) {
  par <- as.numeric(par)
  names(par) <- orden_parametros
  val <- ll_seguro(par)
  if (!is.finite(val)) return(1e100)
  -val
}

f_max_full <- function(par) {
  par <- as.numeric(par)
  names(par) <- orden_parametros
  val <- ll_seguro(par)
  if (!is.finite(val)) return(-1e100)
  val
}

crear_bounds_de <- function(start_full, deconst) {
  lower <- as.numeric(start_full) - deconst
  upper <- as.numeric(start_full) + deconst
  names(lower) <- names(upper) <- names(start_full)

  # Mantengo parametros estructurales del MTUEM en rangos razonables.
  if ("PH" %in% names(lower)) {
    lower["PH"] <- max(0.001, as.numeric(start_full["PH"]) - deconst)
    upper["PH"] <- min(2.0, as.numeric(start_full["PH"]) + deconst)
  }
  if ("tw" %in% names(lower)) {
    lower["tw"] <- max(-0.95, as.numeric(start_full["tw"]) - deconst)
    upper["tw"] <- min(2.0, as.numeric(start_full["tw"]) + deconst)
  }
  if ("th1" %in% names(lower)) {
    lower["th1"] <- max(0.05, as.numeric(start_full["th1"]) - deconst)
    upper["th1"] <- min(0.98, as.numeric(start_full["th1"]) + deconst)
  }

  list(lower = lower, upper = upper)
}

historial_v5 <- tibble::tibble()
actual_par <- start_joint_base
actual_ll <- ll_seguro(actual_par)
mejor_par <- actual_par
mejor_ll <- actual_ll
mejor_res <- NULL

agregar_historial <- function(fila) {
  historial_v5 <<- dplyr::bind_rows(historial_v5, fila)
  readr::write_csv(historial_v5, file.path(carpeta_02, "03_historial_v5_DEoptim_progresivo_NM.csv"))
}

actualizar_mejor <- function(par, ll, res = NULL, etapa = NA_character_, notas = "") {
  if (is.finite(ll) && (!is.finite(mejor_ll) || ll > mejor_ll + TOL_MEJORA_LLIK)) {
    mejor_par <<- par
    mejor_ll <<- ll
    mejor_res <<- res
    guardar_estado_liviano("mejor_resultado_actual", mejor_par, mejor_ll, mejor_res, etapa, notas)
    return(TRUE)
  }
  FALSE
}

correr_deoptim_fase <- function(fase_id, descripcion, start_full, deconst, NP, itermax) {
  start_full <- as.numeric(start_full)
  names(start_full) <- orden_parametros
  ll_inicio <- ll_seguro(start_full)

  carpeta_fase <- file.path(carpeta_de, paste0("fase_", fase_id, "_", nombre_archivo_seguro(descripcion)))
  crear_dir(carpeta_fase)
  bounds <- crear_bounds_de(start_full, deconst)

  readr::write_csv(
    tibble::tibble(
      parametro = orden_parametros,
      start = as.numeric(start_full),
      lower = as.numeric(bounds$lower),
      upper = as.numeric(bounds$upper)
    ),
    file.path(carpeta_fase, "bounds.csv")
  )

  inicio <- Sys.time()
  log_path <- file.path(carpeta_fase, "log_DEoptim.txt")
  sink(log_path, split = TRUE)
  cat("====================================================\n")
  cat("Ejecutando:", descripcion, "\n")
  cat("Fase:", fase_id, "\n")
  cat("Inicio:", format(inicio, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("itermax:", itermax, "| NP:", NP, "| deconst:", deconst, "\n")
  cat("LogLik inicio:", ll_inicio, "\n")
  cat("Trace DEoptim:", TRACE_DEOPTIM, "\n")
  cat("====================================================\n")

  res <- tryCatch(
    DEoptim::DEoptim(
      fn = f_min_de,
      lower = as.numeric(bounds$lower),
      upper = as.numeric(bounds$upper),
      control = DEoptim::DEoptim.control(
        itermax = itermax,
        NP = NP,
        trace = TRACE_DEOPTIM,
        storepopfrom = 1,
        storepopfreq = max(1, floor(itermax / 5))
      )
    ),
    error = function(e) e
  )

  fin <- Sys.time()
  cat("\nFin:", format(fin, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Duracion minutos:", round(as.numeric(difftime(fin, inicio, units = "mins")), 3), "\n")

  par_out <- start_full
  ll_out <- ll_inicio
  estado <- "error"
  mensaje <- NA_character_
  bestval <- NA_real_

  if (!inherits(res, "error")) {
    par_out <- as.numeric(res$optim$bestmem)
    names(par_out) <- orden_parametros
    ll_out <- ll_seguro(par_out)
    bestval <- as.numeric(res$optim$bestval)
    estado <- ifelse(is.finite(ll_out), "valido", "no_finito")
    mensaje <- "DEoptim termino"
    cat("Estado:", estado, "\n")
    cat("Bestval DEoptim (min -LL):", bestval, "\n")
    cat("LogLik salida:", ll_out, "\n")
    cat("Mejora:", ll_out - ll_inicio, "\n")
  } else {
    mensaje <- conditionMessage(res)
    cat("DEoptim dio error:\n")
    cat(mensaje, "\n")
  }
  sink()

  resumen <- tibble::tibble(
    etapa = paste0("DEoptim fase ", fase_id),
    descripcion = descripcion,
    metodo = "DEoptim",
    fase = fase_id,
    itermax = itermax,
    NP = NP,
    deconst = deconst,
    inicio = as.character(inicio),
    fin = as.character(fin),
    duracion_min = round(as.numeric(difftime(fin, inicio, units = "mins")), 3),
    logLik_inicio = ll_inicio,
    logLik_salida = ll_out,
    mejora = ll_out - ll_inicio,
    bestval = bestval,
    estado = estado,
    mensaje = mensaje
  )

  readr::write_csv(resumen, file.path(carpeta_fase, "resumen_DEoptim.csv"))
  readr::write_csv(
    tibble::tibble(parametro = names(par_out), valor = as.numeric(par_out)),
    file.path(carpeta_fase, "parametros_DEoptim.csv")
  )

  guardar_estado_liviano(
    paste0("fase_", fase_id, "_DEoptim"),
    par_out,
    ll_out,
    NULL,
    paste0("DEoptim fase ", fase_id),
    descripcion
  )

  list(par = par_out, ll = ll_out, res = NULL, resumen = resumen, estado = estado)
}

correr_nm_full <- function(nombre, start_full, iterlim, carpeta_destino = carpeta_nm) {
  start_full <- as.numeric(start_full)
  names(start_full) <- orden_parametros
  inicio <- Sys.time()
  carpeta_etapa <- file.path(carpeta_destino, nombre_archivo_seguro(nombre))
  crear_dir(carpeta_etapa)
  archivo_log <- file.path(carpeta_etapa, paste0(nombre_archivo_seguro(nombre), "_log.txt"))

  sink(archivo_log, split = TRUE)
  cat("====================================================\n")
  cat("Ejecutando:", nombre, "\n")
  cat("Inicio:", format(inicio, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Metodo: NM | iterlim:", iterlim, "\n")
  cat("LogLik inicio:", ll_seguro(start_full), "\n")
  cat("====================================================\n")

  res <- tryCatch(
    maxLik::maxLik(f_max_full, start = start_full, method = "NM", iterlim = iterlim),
    error = function(e) e
  )

  fin <- Sys.time()
  cat("\nFin:", format(fin, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Duracion minutos:", round(as.numeric(difftime(fin, inicio, units = "mins")), 3), "\n")

  par_out <- start_full
  ll_out <- ll_seguro(start_full)
  code <- NA_real_
  mensaje <- NA_character_
  estado <- "error"

  if (!inherits(res, "error") && !is.null(res$estimate) && all(is.finite(res$estimate))) {
    par_out <- as.numeric(res$estimate)
    names(par_out) <- orden_parametros
    ll_out <- ll_seguro(par_out)
    code <- retornar_code(res)
    mensaje <- retornar_mensaje(res)
    estado <- ifelse(is.finite(ll_out), "valido", "no_finito")
    cat("Estado:", estado, "\n")
    cat("Code:", code, "\n")
    cat("Mensaje:", mensaje, "\n")
    cat("LogLik salida:", ll_out, "\n")
    cat("Mejora:", ll_out - ll_seguro(start_full), "\n")
  } else {
    mensaje <- if (inherits(res, "error")) conditionMessage(res) else "resultado no valido"
    cat("NM dio error o resultado no valido:\n")
    cat(mensaje, "\n")
    res <- NULL
  }
  sink()

  resumen <- tibble::tibble(
    etapa = nombre,
    descripcion = nombre,
    metodo = "NM",
    fase = NA_integer_,
    itermax = iterlim,
    NP = NA_real_,
    deconst = NA_real_,
    inicio = as.character(inicio),
    fin = as.character(fin),
    duracion_min = round(as.numeric(difftime(fin, inicio, units = "mins")), 3),
    logLik_inicio = ll_seguro(start_full),
    logLik_salida = ll_out,
    mejora = ll_out - ll_seguro(start_full),
    bestval = NA_real_,
    estado = estado,
    mensaje = mensaje,
    code = code
  )

  readr::write_csv(resumen, file.path(carpeta_etapa, "resumen_NM.csv"))
  readr::write_csv(
    tibble::tibble(parametro = names(par_out), valor = as.numeric(par_out)),
    file.path(carpeta_etapa, "parametros_NM.csv")
  )

  guardar_estado_liviano(nombre_archivo_seguro(nombre), par_out, ll_out, res, nombre, estado)

  list(par = par_out, ll = ll_out, res = res, resumen = resumen, estado = estado, code = code, mensaje = mensaje)
}

cat("\n====================================================\n")
cat("Paso 3 V5: escalamiento progresivo DEoptim + NM\n")
cat("LogLik start:", actual_ll, "\n")
cat("Fases DEoptim:\n")
print(FASES_DEOPTIM_V5)
cat("====================================================\n")

guardar_estado_liviano("00_start_paso3", actual_par, actual_ll, NULL, "start", "parametros iniciales desde modelos solos")

for (i in seq_len(nrow(FASES_DEOPTIM_V5))) {
  fase <- FASES_DEOPTIM_V5[i, ]
  fase_id <- as.integer(fase$fase)
  descripcion <- as.character(fase$descripcion)
  deconst <- as.numeric(fase$deconst)
  NP <- as.integer(fase$NP)
  itermax <- as.integer(fase$itermax)

  cat("\n----------------------------------------------------\n")
  cat("FASE", fase_id, ":", descripcion, "\n")
  cat("LogLik entrada:", actual_ll, "\n")
  cat("----------------------------------------------------\n")

  out_de <- correr_deoptim_fase(fase_id, descripcion, actual_par, deconst, NP, itermax)
  agregar_historial(out_de$resumen)

  if (is.finite(out_de$ll) && out_de$ll > actual_ll + TOL_MEJORA_LLIK) {
    actual_par <- out_de$par
    actual_ll <- out_de$ll
    actualizar_mejor(actual_par, actual_ll, NULL, paste0("DEoptim fase ", fase_id), "DEoptim mejora frente al punto de entrada")
    cat("DEoptim fase", fase_id, "mejora. Nuevo logLik:", actual_ll, "\n")
  } else {
    cat("DEoptim fase", fase_id, "no mejora. Se mantiene punto anterior.\n")
  }

  nm_nombre <- paste0("NM_post_DEoptim_fase_", fase_id)
  out_nm <- correr_nm_full(nm_nombre, actual_par, ITER_NM_DESPUES_DE_FASE)
  agregar_historial(out_nm$resumen)

  if (is.finite(out_nm$ll) && out_nm$ll >= actual_ll - TOL_MEJORA_LLIK) {
    actual_par <- out_nm$par
    actual_ll <- out_nm$ll
    mejor_res <- out_nm$res
    actualizar_mejor(actual_par, actual_ll, mejor_res, nm_nombre, "NM posterior a fase DEoptim")
    cat("NM fase", fase_id, "valido. LogLik actual:", actual_ll, "\n")
  } else {
    cat("NM fase", fase_id, "no fue aceptado. Se mantiene punto anterior.\n")
  }

  guardar_estado_liviano(
    paste0("estado_despues_fase_", fase_id),
    actual_par,
    actual_ll,
    mejor_res,
    paste0("fin fase ", fase_id),
    paste0("resultado acumulado despues de ", descripcion)
  )
}

###### 9. NM final largo ######

cat("\n====================================================\n")
cat("Paso 3 V5: NM final largo\n")
cat("Iteraciones NM final:", ITER_NM_FINAL_V5, "\n")
cat("LogLik inicio NM final:", actual_ll, "\n")
cat("====================================================\n")

out_nm_final <- correr_nm_full("NM_final_largo_v5", actual_par, ITER_NM_FINAL_V5)
agregar_historial(out_nm_final$resumen)

if (is.finite(out_nm_final$ll) && out_nm_final$ll >= actual_ll - TOL_MEJORA_LLIK) {
  actual_par <- out_nm_final$par
  actual_ll <- out_nm_final$ll
  mejor_res <- out_nm_final$res
  actualizar_mejor(actual_par, actual_ll, mejor_res, "NM_final_largo_v5", "NM final largo posterior a fases DEoptim")
}

###### 10. Guardar resultado final ######

vc_final <- if (!is.null(mejor_res)) extraer_vcov_maxlik(mejor_res) else NULL
res_paso3_liviano <- guardar_estado_liviano(
  "03_joint_sin_correlacion_MEJOR",
  actual_par,
  actual_ll,
  mejor_res,
  "v5_DEoptim_progresivo_NM",
  "mejor resultado final v5"
)
saveRDS(res_paso3_liviano, file.path(carpeta_02, "03_joint_sin_correlacion_MEJOR.rds"))

if (isTRUE(GUARDAR_MODELO_COMPLETO_FINAL) && !is.null(mejor_res)) {
  saveRDS(mejor_res, file.path(carpeta_02, "03_joint_sin_correlacion_MEJOR_maxLik_completo.rds"))
}

coef_paso3 <- coeficientes_desde_estado(actual_par, vc_final) %>%
  dplyr::mutate(modelo = "paso3_joint_sin_correlacion_v5_DEoptim_progresivo_NM", .before = 1)
readr::write_csv(coef_paso3, file.path(carpeta_02, "03_coeficientes_paso3.csv"))

resumen_mejor <- tibble::tibble(
  modelo = "paso3_joint_sin_correlacion",
  estrategia = "v5_DEoptim_progresivo_NM",
  logLik_start = ll_seguro(start_joint_base),
  logLik_final = actual_ll,
  mejora_vs_start = actual_ll - ll_seguro(start_joint_base),
  n_parametros = length(actual_par),
  code_final = if (!is.null(mejor_res)) retornar_code(mejor_res) else NA_real_,
  mensaje_final = if (!is.null(mejor_res)) retornar_mensaje(mejor_res) else NA_character_,
  convergio_formal = if (!is.null(mejor_res)) isTRUE(retornar_code(mejor_res) == 0) else FALSE,
  nota = "V5 usa escalamiento progresivo de DEoptim: primero busqueda local estrecha, luego media y luego amplia, con NM posterior a cada fase y NM final largo. Comparar con v3, v4 y otras estrategias por logLik, code y errores estandar."
)
readr::write_csv(resumen_mejor, file.path(carpeta_02, "03_resumen_mejor_paso3.csv"))

sink(file.path(carpeta_02, "03_summary_paso3.txt"))
cat("Modelo paso 3 joint sin correlacion\n")
cat("Estrategia: v5 DEoptim progresivo + NM\n")
cat("Carpeta:", carpeta_salida, "\n")
cat("LogLik start:", ll_seguro(start_joint_base), "\n")
cat("LogLik final:", actual_ll, "\n")
cat("Mejora vs start:", actual_ll - ll_seguro(start_joint_base), "\n")
cat("Parametros:", length(actual_par), "\n")
cat("Code final:", resumen_mejor$code_final, "\n")
cat("Mensaje final:", resumen_mejor$mensaje_final, "\n")
cat("Convergio formal:", resumen_mejor$convergio_formal, "\n\n")
cat("Fases DEoptim usadas:\n")
print(FASES_DEOPTIM_V5)
cat("\nHistorial de estimacion:\n")
print(historial_v5)
cat("\nResumen final:\n")
print(resumen_mejor)
cat("\nCoeficientes finales:\n")
print(coef_paso3)
sink()

cat("\n====================================================\n")
cat("FIN PASO 3 - V5 DEOPTIM PROGRESIVO + NM\n")
cat("LogLik final paso 3:", actual_ll, "\n")
cat("Code final:", resumen_mejor$code_final, "\n")
cat("Resultado guardado en:", file.path(carpeta_02, "03_joint_sin_correlacion_MEJOR.rds"), "\n")
cat("Carpeta de salida:", carpeta_salida, "\n")
cat("====================================================\n")
