###### 0. Limpieza y controles ######

rm(list = ls())

archivo_base <- "enut_ii.xlsx"
hoja_base <- "enut_ii"

carpeta_salida <- file.path("prueba", "v6_p4_red_full")
carpeta_mnl <- file.path("mnl", "resultados v2")
carpeta_mtuem <- "mtuem"
carpeta_mtuem_general <- file.path(carpeta_mtuem, "01_muestra_general")
carpeta_mtuem_diag <- file.path(carpeta_mtuem, "00_base_y_diagnosticos")

USAR_RDS_EXISTENTE <- TRUE

# v6 joint (paso 4) - Opcion 6:
# Modelo reducido con correlacion -> modelo completo con correlacion
# Nota: el paso 3 NO se reestima; se usan los coeficientes del log como punto inicial.
# Primero se estima una version reducida del paso 4 para encontrar mejores valores
# iniciales; luego se estima el paso 4 completo con la muestra completa.
ITER_PASO4_REDUCIDO_NM <- 90000
ITER_PASO4_COMPLETO_NM <- 200000
TIEMPO_MAX_PASO4_HORAS <- 240

# Parametros que se excluyen temporalmente en el modelo reducido.
# Se eligieron los que en el paso 3 aparecen no significativos o marginales.
# Puedes agregar o quitar parametros si quieres hacer el reducido mas agresivo.
PARAMETROS_EXCLUIR_REDUCIDO <- c("ASC2", "ASC5", "BCP_4")

DECONST_MIN <- 3
DECONST_EXTRA <- 2.5

T_RATIO_CORTE <- 1.96
set.seed(42)

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
  "plyr", "data.table", "magrittr", "car", "carData", "lmtest", "zoo"
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
  if (is.null(path) || length(path) == 0 || is.na(path) || path == "") return(invisible(FALSE))
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("No se pudo crear la carpeta: ", path)
  }
  invisible(TRUE)
}

crear_dir(carpeta_salida)

carpeta_00 <- file.path(carpeta_salida, "00_inputs")
carpeta_01 <- file.path(carpeta_salida, "01_individuales")
carpeta_02 <- file.path(carpeta_salida, "02_p3_sin_corr")
carpeta_03 <- file.path(carpeta_salida, "03_p4_corr")
carpeta_04 <- file.path(carpeta_salida, "04_vot")
carpeta_05 <- file.path(carpeta_salida, "05_ajuste")
carpeta_06 <- file.path(carpeta_salida, "06_comp")
carpeta_07 <- file.path(carpeta_salida, "07_logs")

carpeta_check <- file.path(carpeta_03, "00_chk")
carpeta_check_inicio <- file.path(carpeta_check, "01_ini")
carpeta_check_red <- file.path(carpeta_check, "02_red")
carpeta_check_nm <- file.path(carpeta_check, "03_full_nm")
carpeta_check_best <- file.path(carpeta_check, "05_best")
carpeta_check_logs <- file.path(carpeta_check, "logs")

for (x in c(carpeta_00, carpeta_01, carpeta_02, carpeta_03, carpeta_04,
            carpeta_05, carpeta_06, carpeta_07, carpeta_check,
            carpeta_check_inicio, carpeta_check_red, carpeta_check_nm,
            carpeta_check_best, carpeta_check_logs)) {
  crear_dir(x)
}

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

guardar_log_modelo <- function(nombre, expr, carpeta, archivo_rds) {
  # Correccion v5: asegurar que exista la carpeta antes de abrir sink(),
  # y cerrar sink() aunque el modelo falle o se interrumpa dentro del tryCatch.
  crear_dir(carpeta)
  crear_dir(dirname(archivo_rds))

  carpeta_abs <- normalizePath(carpeta, winslash = "/", mustWork = FALSE)
  archivo_rds_abs <- normalizePath(archivo_rds, winslash = "/", mustWork = FALSE)
  nombre_log <- substr(nombre_archivo_seguro(nombre), 1, 45)
  archivo_log <- file.path(carpeta_abs, paste0(nombre_log, "_log.txt"))

  if (isTRUE(USAR_RDS_EXISTENTE) && file.exists(archivo_rds_abs)) {
    cat("Usando RDS existente:", archivo_rds_abs, "\n")
    return(readRDS(archivo_rds_abs))
  }

  inicio <- Sys.time()
  obj <- NULL

  sink(archivo_log, split = TRUE)
  on.exit({
    while (sink.number() > 0) sink()
  }, add = TRUE)

  cat("====================================================\n")
  cat("Ejecutando:", nombre, "\n")
  cat("Inicio:", format(inicio, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Carpeta log:", carpeta_abs, "\n")
  cat("Archivo RDS:", archivo_rds_abs, "\n")
  cat("====================================================\n")

  obj <- tryCatch(
    eval(expr),
    error = function(e) {
      cat("\nERROR:\n")
      cat(conditionMessage(e), "\n")
      return(NULL)
    },
    warning = function(w) {
      cat("\nWARNING:\n")
      cat(conditionMessage(w), "\n")
      invokeRestart("muffleWarning")
    }
  )

  fin <- Sys.time()

  if (!is.null(obj)) {
    cat("\nResumen:\n")
    tryCatch(print(summary(obj)), error = function(e) cat("No se pudo imprimir summary():", conditionMessage(e), "\n"))
    saveRDS(obj, archivo_rds_abs)
  }

  cat("\nFin:", format(fin, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Duracion minutos:", round(as.numeric(difftime(fin, inicio, units = "mins")), 2), "\n")

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

###### 8. Paso 3: resultados hardcodeados y salto directo a Paso 4 ######

# Este script NO reestima el paso 1, paso 2 ni paso 3.
# Usa los coeficientes obtenidos en el paso 3 ya ejecutado correctamente
# como valores iniciales para estimar directamente el paso 4 con correlacion.

logLik_paso3_manual <- -55281.94

est_paso3_manual <- c(
  PH = 0.262045,
  th1 = 0.799546,
  tw = 0.298995,
  ASC2 = -0.179930,
  ASC3 = -3.191150,
  ASC4 = -2.082054,
  ASC5 = 0.000985,
  ASC6 = -3.239496,
  ASC7 = -1.123655,
  BAGE65_2 = 0.518689,
  BAGE65_3 = 1.041958,
  BAGE65_5 = 0.443923,
  BCP_3 = 0.929983,
  BCP_4 = -3.912735,
  BED_2 = 1.018667,
  BED_6 = -2.146351,
  BESC_2 = -0.153447,
  BESC_4 = -0.374392,
  BESC_5 = -0.264282,
  BFEM_2 = 0.929887,
  BFEM_3 = 1.002655,
  BFEM_4 = -0.700687,
  BFEM_5 = 0.572787,
  BFEM_6 = -0.365720,
  BFEM_7 = 0.786992,
  BNTRAB_2 = 0.150073,
  BPAREJA_2 = -0.759705,
  BPAREJA_3 = -0.669102,
  BPAREJA_4 = -0.638883,
  BPAREJA_5 = -0.653182,
  BPAREJA_6 = -0.337214,
  BPAREJA_7 = -0.581879,
  BQ3_5 = -0.248110,
  BQ4_2 = -0.338801,
  BQ4_5 = -0.428244,
  BQ5_2 = -0.760064,
  BQ5_5 = -0.733498,
  BTC_2 = -1.102956,
  BTC_4 = -2.083162,
  BTC_6 = -1.247319,
  BTC_7 = -0.974333,
  BTD_2 = -0.842998,
  BTD_4 = -0.980837,
  BTD_5 = 1.047700,
  BTD_6 = -3.404748,
  BTEC_2 = -0.504892,
  BTEC_5 = -0.585841,
  BTEC_7 = -0.231050,
  BTMOD_5 = -0.045772,
  BTMOD_6 = 0.501305,
  BUNI_2 = -1.026269,
  BUNI_3 = -0.937372,
  BUNI_4 = -1.463743,
  BUNI_5 = -1.099633,
  BUNI_6 = -0.939133,
  BUNI_7 = -0.806642,
  BZCEN_4 = 0.941741,
  BZCEN_5 = 0.250749,
  BZCEN_6 = -0.568112,
  BZSUR_2 = -0.536295,
  BZSUR_3 = -0.634564,
  BZSUR_5 = -0.366953,
  BZSUR_6 = -0.634108,
  BZSUR_7 = -0.319479
)

# Objeto manual para que el resto del script pueda calcular metricas,
# valores de tiempo y coeficientes del paso 3 sin necesitar un RDS.
logLik.joint_manual <- function(object, ...) {
  structure(object$maximum, df = length(object$estimate), class = "logLik")
}

res_paso3 <- structure(
  list(
    estimate = est_paso3_manual,
    se = rep(NA_real_, length(est_paso3_manual)),
    maximum = logLik_paso3_manual,
    code = 0
  ),
  class = "joint_manual"
)

nombre_mejor_paso3 <- "NM_hardcodeado_desde_log"

readr::write_csv(
  tibble(parametro = names(est_paso3_manual), estimate_paso3 = as.numeric(est_paso3_manual)),
  file.path(carpeta_02, "03_coeficientes_paso3_hardcodeados.csv")
)

readr::write_csv(
  tibble(metodo = "NM_hardcodeado_desde_log", logLik = logLik_paso3_manual, code = 0, convergio = TRUE),
  file.path(carpeta_02, "03_comparacion_candidatos_paso3.csv")
)

saveRDS(res_paso3, file.path(carpeta_02, "03_joint_sin_correlacion_MEJOR_hardcodeado.rds"))

###### 9. Paso 4: joint con correlacion - Opcion 6 ######

# Estrategia v6 joint (paso 4):
# 1) Estimar paso 4 con correlacion en una especificacion reducida.
# 2) Usar esos coeficientes como valores iniciales para estimar paso 4 completo.
#
# Esta version no reestima los pasos 1, 2 ni 3. El paso 3 queda hardcodeado desde
# el log y se usa como base. El modelo reducido no es el resultado final; solo sirve
# para encontrar un punto inicial mas cercano y, eventualmente, acelerar la convergencia
# del modelo completo.

start_paso4 <- completar_start(est_paso3_manual, orden_nmm, fallback = start_joint_base)

deconst_paso4 <- max(DECONST_MIN, ceiling(max(abs(start_paso4), na.rm = TRUE) + DECONST_EXTRA))

readr::write_csv(
  tibble(parametro = names(start_paso4), start = as.numeric(start_paso4)),
  file.path(carpeta_check_inicio, "00_start_paso4_desde_paso3_hardcodeado.csv")
)
saveRDS(start_paso4, file.path(carpeta_check_inicio, "00_start_paso4_desde_paso3_hardcodeado.rds"))

# 9.0 Definicion de modelo reducido ------------------------------------------
# Se eliminan temporalmente algunos parametros del bloque discreto para estimar
# una version mas simple del paso 4. Luego, en el modelo completo, esos parametros
# vuelven a entrar con los valores del paso 3 como respaldo.

eliminar_terminos_parametros <- function(eq, params_excluir) {
  if (length(params_excluir) == 0) return(eq)
  purrr::map_chr(eq, function(linea) {
    if (!grepl("~", linea, fixed = TRUE)) return(linea)
    partes <- strsplit(linea, "~", fixed = TRUE)[[1]]
    lhs <- trimws(partes[1])
    rhs <- paste(partes[-1], collapse = "~")
    terminos <- unlist(strsplit(rhs, "\\+"))
    terminos <- trimws(terminos)
    terminos <- terminos[terminos != ""]

    mantener <- rep(TRUE, length(terminos))
    for (pp in params_excluir) {
      patron <- paste0("(^|[^A-Za-z0-9_])", pp, "([^A-Za-z0-9_]|$)")
      mantener <- mantener & !grepl(patron, terminos)
    }

    rhs_red <- paste(terminos[mantener], collapse = " + ")
    if (is.na(rhs_red) || rhs_red == "") rhs_red <- "0"
    paste(lhs, "~", rhs_red)
  })
}

params_excluir_reducido <- intersect(PARAMETROS_EXCLUIR_REDUCIDO, par_d)
par_d_reducido <- setdiff(par_d, params_excluir_reducido)
eq_d_reducido <- eliminar_terminos_parametros(eq_d, params_excluir_reducido)

orden_nmm_reducido <- c(sort(par_c), setdiff(sort(par_d_reducido), "ASC1"))
start_paso4_reducido <- completar_start(start_paso4, orden_nmm_reducido, fallback = start_joint_base)

crear_dir(carpeta_check_red)
crear_dir(carpeta_check_nm)
crear_dir(carpeta_check_best)
crear_dir(carpeta_check_logs)

writeLines(eq_d_reducido, file.path(carpeta_check_red, "00_eq_d_reducido.txt"))
readr::write_csv(tibble(parametro = par_d_reducido), file.path(carpeta_check_red, "00_par_d_reducido.csv"))
readr::write_csv(tibble(parametro_excluido_temporalmente = params_excluir_reducido), file.path(carpeta_check_red, "00_parametros_excluidos_temporalmente.csv"))
readr::write_csv(
  tibble(parametro = names(start_paso4_reducido), start = as.numeric(start_paso4_reducido)),
  file.path(carpeta_check_red, "00_start_reducido.csv")
)
saveRDS(start_paso4_reducido, file.path(carpeta_check_red, "00_start_reducido.rds"))

readr::write_csv(
  tibble(
    estrategia = "v6_joint_paso4_opcion6_reducido_completo",
    n_parametros_full = length(orden_nmm),
    n_parametros_reducido = length(orden_nmm_reducido),
    parametros_excluidos = paste(params_excluir_reducido, collapse = ", "),
    iter_reducido_nm = ITER_PASO4_REDUCIDO_NM,
    iter_completo_nm = ITER_PASO4_COMPLETO_NM,
    deconst_paso4 = deconst_paso4,
    tiempo_max_horas = TIEMPO_MAX_PASO4_HORAS,
    nota = "El modelo reducido se usa solo para obtener valores iniciales; el resultado final corresponde al modelo completo."
  ),
  file.path(carpeta_check_inicio, "00_control_paso4_v6_opcion6.csv")
)

setTimeLimit(cpu = Inf, elapsed = TIEMPO_MAX_PASO4_HORAS * 3600, transient = TRUE)

# 9.1 Paso 4 reducido con correlacion ----------------------------------------
# Si esta etapa converge o al menos devuelve un objeto valido con log-likelihood,
# sus coeficientes se usan como inicio de la estimacion completa. Los parametros
# que no fueron estimados en el reducido se completan con los valores del paso 3.

res_paso4_red <- guardar_log_modelo(
  nombre = "04A_red_NM_desde_p3",
  expr = quote(
    nmm(
      data = data_nmm,
      eq_type = "joint",
      eq_c = eq_c,
      par_c = par_c,
      eq_d = eq_d_reducido,
      par_d = par_d_reducido,
      start_v = start_paso4_reducido,
      corrl = TRUE,
      weight_paths = FALSE,
      weight_paths_cont = TRUE,
      fixed_term = FALSE,
      check_hess = FALSE,
      best_method = FALSE,
      DEoptim_run = FALSE,
      DEoptim_run_main = FALSE,
      try_last_DEoptim = FALSE,
      opt_method = "NM",
      numerical_deriv = FALSE,
      miterlim = ITER_PASO4_REDUCIDO_NM
    )
  ),
  carpeta = carpeta_check_red,
  archivo_rds = file.path(carpeta_check_red, "04A_red_nm.rds")
)

start_paso4_full <- if (modelo_loglik_ok(res_paso4_red)) {
  completar_start(res_paso4_red$estimate, orden_nmm, fallback = start_paso4)
} else {
  start_paso4
}

readr::write_csv(
  tibble(parametro = names(start_paso4_full), start = as.numeric(start_paso4_full)),
  file.path(carpeta_check_nm, "00_start_full.csv")
)
saveRDS(start_paso4_full, file.path(carpeta_check_nm, "00_start_full.rds"))

# 9.2 Paso 4 completo con correlacion ----------------------------------------
# Resultado final. Parte desde el reducido si fue valido; si no, desde paso 3.

res_paso4_full <- guardar_log_modelo(
  nombre = "04B_full_NM_post_red",
  expr = quote(
    nmm(
      data = data_nmm,
      eq_type = "joint",
      eq_c = eq_c,
      par_c = par_c,
      eq_d = eq_d,
      par_d = par_d,
      start_v = start_paso4_full,
      corrl = TRUE,
      weight_paths = FALSE,
      weight_paths_cont = TRUE,
      fixed_term = FALSE,
      check_hess = FALSE,
      best_method = FALSE,
      DEoptim_run = FALSE,
      DEoptim_run_main = FALSE,
      try_last_DEoptim = FALSE,
      opt_method = "NM",
      numerical_deriv = FALSE,
      miterlim = ITER_PASO4_COMPLETO_NM
    )
  ),
  carpeta = carpeta_check_nm,
  archivo_rds = file.path(carpeta_check_nm, "04B_full_nm.rds")
)

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

candidatos_paso4 <- list(
  NM_reducido = res_paso4_red,
  NM_completo_post_reducido = res_paso4_full
)
validos_paso4 <- candidatos_paso4[sapply(candidatos_paso4, modelo_loglik_ok)]

# Para resultados finales se prioriza el modelo completo si es valido.
# Si por algun motivo falla, se conserva el reducido solo como diagnostico, pero
# no deberia usarse como resultado final de la memoria.
if (modelo_loglik_ok(res_paso4_full)) {
  nombre_mejor_paso4 <- "NM_completo_post_reducido"
  res_paso4 <- res_paso4_full
  saveRDS(res_paso4, file.path(carpeta_check_best, "04_best.rds"))
  saveRDS(res_paso4, file.path(carpeta_03, "04_best.rds"))
} else if (length(validos_paso4) > 0) {
  nombre_mejor_paso4 <- names(validos_paso4)[1]
  res_paso4 <- validos_paso4[[1]]
  saveRDS(res_paso4, file.path(carpeta_check_best, "04_red_diag.rds"))
} else {
  nombre_mejor_paso4 <- NA_character_
  res_paso4 <- NULL
}

comparacion_paso4 <- tibble(
  candidato = names(candidatos_paso4),
  logLik = sapply(candidatos_paso4, extraer_loglik),
  code = sapply(candidatos_paso4, extraer_code),
  convergio = sapply(candidatos_paso4, modelo_convergio),
  valido = sapply(candidatos_paso4, modelo_loglik_ok),
  es_resultado_final_preferido = candidato == nombre_mejor_paso4
)
readr::write_csv(comparacion_paso4, file.path(carpeta_03, "04_comparacion_candidatos_paso4.csv"))
readr::write_csv(comparacion_paso4, file.path(carpeta_check_best, "04_comparacion_candidatos_paso4.csv"))

###### 10. Metricas de ajuste ######

ll_mnl <- ajuste_mnl$logLik[1]
ll0_mnl_const <- ajuste_mnl$LL0_constantes[1]
k_mnl <- ajuste_mnl$n_parametros_estimados[1]

ll_mtuem <- extraer_loglik(modelo_mtuem)
k_mtuem <- length(tryCatch(modelo_mtuem$estimate, error = function(e) numeric(0)))
ll0_mtuem <- ll_normal_multivariado_nulo(base_cont_persona, c("Tw", "Tf1"))

ll0_joint <- ll0_mnl_const + ll0_mtuem

metricas_modelo <- function(nombre, modelo, k_manual = NULL, ll0 = ll0_joint, n_obs = nrow(data_nmm), n_personas = dplyr::n_distinct(data_nmm$PeID)) {
  ll <- extraer_loglik(modelo)
  k <- if (is.null(k_manual)) length(tryCatch(modelo$estimate, error = function(e) numeric(0))) else k_manual
  tibble(
    modelo = nombre,
    logLik = ll,
    n_parametros = k,
    n_obs = n_obs,
    n_personas = n_personas,
    LL0 = ll0,
    AIC = -2 * ll + 2 * k,
    BIC = -2 * ll + log(n_obs) * k,
    rho2 = rho2_simple(ll, ll0),
    rho2_ajustado = rho2_ajustado(ll, ll0, k),
    code = extraer_code(modelo),
    convergio = modelo_convergio(modelo)
  )
}

metricas_mtuem <- tibble(
  modelo = "mtuem_final_reutilizado",
  logLik = ll_mtuem,
  n_parametros = k_mtuem,
  n_obs = nrow(base_cont_persona),
  n_personas = nrow(base_cont_persona),
  LL0 = ll0_mtuem,
  AIC = -2 * ll_mtuem + 2 * k_mtuem,
  BIC = -2 * ll_mtuem + log(nrow(base_cont_persona)) * k_mtuem,
  rho2 = rho2_simple(ll_mtuem, ll0_mtuem),
  rho2_ajustado = rho2_ajustado(ll_mtuem, ll0_mtuem, k_mtuem),
  code = extraer_code(modelo_mtuem),
  convergio = modelo_convergio(modelo_mtuem)
)

metricas_mnl <- tibble(
  modelo = "mnl_v2_reutilizado",
  logLik = ll_mnl,
  n_parametros = k_mnl,
  n_obs = ajuste_mnl$n_obs[1],
  n_personas = ajuste_mnl$n_personas[1],
  LL0 = ll0_mnl_const,
  AIC = ajuste_mnl$AIC[1],
  BIC = ajuste_mnl$BIC[1],
  rho2 = ajuste_mnl$rho2_constantes[1],
  rho2_ajustado = ajuste_mnl$rho2_ajustado_constantes[1],
  code = extraer_code(modelo_mnl),
  convergio = modelo_convergio(modelo_mnl)
)

metricas_paso3 <- metricas_modelo("paso3_joint_sin_correlacion", res_paso3)
metricas_paso4 <- if (!is.null(res_paso4)) metricas_modelo("paso4_joint_con_correlacion", res_paso4) else tibble()

metricas_ajuste <- dplyr::bind_rows(metricas_mnl, metricas_mtuem, metricas_paso3, metricas_paso4)
readr::write_csv(metricas_ajuste, file.path(carpeta_05, "05_metricas_ajuste_modelos.csv"))
readr::write_csv(metricas_ajuste %>% dplyr::select(modelo, logLik, n_parametros, LL0, rho2, rho2_ajustado), file.path(carpeta_05, "05_rho2_ajustado_resumen.csv"))

###### 11. Grupos para valores del tiempo ######

base_grupos <- base_enut %>%
  dplyr::mutate(
    id_persona = as.character(id_persona),
    sexo_num = num(sexo),
    edad_num = num(edad_anios),
    quintil_num = num(quintil),
    macrozona_limpia = limpiar_txt(macrozona),
    cluster_tiempo_num = num(cluster_tiempo),
    cluster_modo_num = num(cluster_modo),
    grupo_edad = dplyr::case_when(
      edad_num >= 18 & edad_num <= 29 ~ "18 a 29",
      edad_num >= 30 & edad_num <= 44 ~ "30 a 44",
      edad_num >= 45 & edad_num <= 59 ~ "45 a 59",
      edad_num >= 60 ~ "60 o mas",
      TRUE ~ NA_character_
    ),
    grupo_sexo = dplyr::case_when(
      sexo_num == 1 ~ "Mujeres",
      sexo_num == 2 ~ "Hombres",
      TRUE ~ NA_character_
    ),
    grupo_ingreso = dplyr::case_when(
      quintil_num %in% c(1, 2, 3) ~ "Quintil bajo",
      quintil_num == 4 ~ "Quintil medio",
      quintil_num == 5 ~ "Quintil alto",
      TRUE ~ NA_character_
    ),
    grupo_territorio = dplyr::case_when(
      macrozona_limpia %in% c("centro", "metropolitana", "region metropolitana", "rm") ~ "Centro",
      macrozona_limpia == "norte" ~ "Norte",
      macrozona_limpia == "sur" ~ "Sur",
      TRUE ~ NA_character_
    ),
    grupo_cluster_tiempo = dplyr::case_when(
      cluster_tiempo_num == 1 ~ "Cuidadores domesticos",
      cluster_tiempo_num == 2 ~ "Rutina flexible",
      cluster_tiempo_num == 3 ~ "Jovenes trabajadores",
      cluster_tiempo_num == 4 ~ "Trabajadores rutinarios",
      cluster_tiempo_num == 5 ~ "Trabajadores moviles",
      TRUE ~ NA_character_
    ),
    grupo_cluster_modo = dplyr::case_when(
      cluster_modo_num == 1 ~ "Usuarios publicos",
      cluster_modo_num == 2 ~ "Movilidad activa",
      cluster_modo_num == 3 ~ "Movilidad mixta",
      cluster_modo_num == 4 ~ "Motorizados privados",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(
    id_persona, grupo_edad, grupo_sexo, grupo_ingreso, grupo_territorio,
    grupo_cluster_tiempo, grupo_cluster_modo
  )

###### 12. Calculo de VoL ######

calcular_predicciones_tiempo <- function(modelo, base_persona, nombre_modelo) {
  pars <- tryCatch(modelo$estimate[c("PH", "tw", "th1")], error = function(e) NULL)
  if (is.null(pars) || any(!is.finite(pars))) {
    stop(paste0("No pude extraer PH, tw y th1 desde ", nombre_modelo))
  }

  df <- base_persona %>%
    dplyr::select(id_persona, PeID, Tw, Tf1, Tf2, Tc, w, Ec, ta) %>%
    dplyr::mutate(dplyr::across(c(Tw, Tf1, Tf2, Tc, w, Ec, ta), ~ num(.)))

  for (p in names(pars)) df[[p]] <- as.numeric(pars[p])

  df$Tw_hat <- eval(parse(text = eq_Tw_rhs), envir = df)
  df$Tf1_hat <- df$th1 * (df$ta - df$Tw_hat - df$Tc)
  df$Tf2_hat <- (1 - df$th1) * (df$ta - df$Tw_hat - df$Tc)

  df$error_Tw <- df$Tw - df$Tw_hat
  df$error_Tf1 <- df$Tf1 - df$Tf1_hat
  df$error_Tf2 <- df$Tf2 - df$Tf2_hat

  df$VoL_predicho <- safe_div(df$w * df$Tw_hat - df$Ec, df$PH * (df$ta - df$Tw_hat - df$Tc))
  df$VoL_observado <- safe_div(df$w * df$Tw - df$Ec, df$PH * (df$ta - df$Tw - df$Tc))
  df$VTAW_predicho <- df$VoL_predicho - df$w
  df$VTAW_observado <- df$VoL_observado - df$w
  df$VoL_predicho_sobre_w <- safe_div(df$VoL_predicho, df$w)
  df$VTAW_predicho_sobre_w <- safe_div(df$VTAW_predicho, df$w)
  df$ingreso_libre_predicho <- df$w * df$Tw_hat - df$Ec
  df$cierre_temporal_predicho <- df$Tw_hat + df$Tf1_hat + df$Tf2_hat + df$Tc

  df %>%
    dplyr::left_join(base_grupos, by = "id_persona") %>%
    dplyr::mutate(modelo = nombre_modelo)
}

ci_media_t <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2) {
    return(tibble(media = mean(x, na.rm = TRUE), se_muestral = NA_real_, ic_inf = NA_real_, ic_sup = NA_real_, n_valido = n))
  }
  media <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  crit <- stats::qt(0.975, df = n - 1)
  tibble(media = media, se_muestral = se, ic_inf = media - crit * se, ic_sup = media + crit * se, n_valido = n)
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

ic_delta_indicador <- function(modelo, base_sub, indicador) {
  par_names <- c("PH", "tw", "th1")
  par_vals <- tryCatch(modelo$estimate[par_names], error = function(e) NULL)
  if (is.null(par_vals) || any(!is.finite(par_vals))) {
    return(tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }

  vc <- obtener_vcov(modelo)
  if (is.null(vc)) {
    return(tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }
  if (is.null(rownames(vc)) || is.null(colnames(vc))) {
    rn <- names(tryCatch(modelo$estimate, error = function(e) numeric(0)))
    if (length(rn) == nrow(vc)) rownames(vc) <- colnames(vc) <- rn
  }
  if (!all(par_names %in% rownames(vc))) {
    return(tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }

  f_media <- function(pv) {
    names(pv) <- par_names
    df <- base_sub %>%
      dplyr::select(Tw, Tf1, Tf2, Tc, w, Ec, ta) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ num(.)))
    for (p in par_names) df[[p]] <- as.numeric(pv[p])
    df$Tw_hat <- eval(parse(text = eq_Tw_rhs), envir = df)
    vol <- safe_div(df$w * df$Tw_hat - df$Ec, df$PH * (df$ta - df$Tw_hat - df$Tc))
    if (indicador == "VoL_predicho") return(mean(vol, na.rm = TRUE))
    if (indicador == "VTAW_predicho") return(mean(vol - df$w, na.rm = TRUE))
    NA_real_
  }

  g <- gradiente_numerico(f_media, par_vals)
  vc_sub <- vc[par_names, par_names, drop = FALSE]
  var_delta <- as.numeric(t(g) %*% vc_sub %*% g)
  media <- f_media(par_vals)
  if (!is.finite(var_delta) || var_delta < 0) {
    return(tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE))
  }
  se <- sqrt(var_delta)
  tibble(se_delta = se, ic_delta_inf = media - 1.96 * se, ic_delta_sup = media + 1.96 * se, delta_ok = TRUE)
}

resumir_valores <- function(data_eval, modelo, nombre_modelo) {
  vars_ind <- c("VoL_predicho", "VTAW_predicho", "VoL_observado", "VTAW_observado", "w")
  dimensiones <- list(
    muestra_general = c("Muestra general" = "__general__"),
    edad = "grupo_edad",
    sexo = "grupo_sexo",
    ingresos = "grupo_ingreso",
    territorio = "grupo_territorio",
    cluster_tiempo = "grupo_cluster_tiempo",
    cluster_modo = "grupo_cluster_modo"
  )

  out <- list()
  pos <- 1

  for (dim_nm in names(dimensiones)) {
    var_grupo <- dimensiones[[dim_nm]]

    if (dim_nm == "muestra_general") {
      categorias <- "Muestra general"
      data_eval$.__grupo_tmp__ <- "Muestra general"
      var_usar <- ".__grupo_tmp__"
    } else {
      var_usar <- var_grupo
      categorias <- data_eval[[var_usar]] %>% unique() %>% na.omit() %>% as.character() %>% sort()
    }

    for (cat_i in categorias) {
      sub <- data_eval %>% dplyr::filter(.data[[var_usar]] == cat_i)
      if (nrow(sub) == 0) next

      for (ind in vars_ind) {
        x <- sub[[ind]]
        ci_t <- ci_media_t(x)
        qs <- stats::quantile(x[is.finite(x)], probs = c(0.25, 0.5, 0.75, 0.95, 0.99), na.rm = TRUE, names = FALSE)

        ci_d <- if (ind %in% c("VoL_predicho", "VTAW_predicho")) {
          ic_delta_indicador(modelo, sub, ind)
        } else {
          tibble(se_delta = NA_real_, ic_delta_inf = NA_real_, ic_delta_sup = NA_real_, delta_ok = FALSE)
        }

        metodo <- ifelse(isTRUE(ci_d$delta_ok[1]), "delta", "t_muestra")
        ic_inf <- ifelse(metodo == "delta", ci_d$ic_delta_inf[1], ci_t$ic_inf[1])
        ic_sup <- ifelse(metodo == "delta", ci_d$ic_delta_sup[1], ci_t$ic_sup[1])

        out[[pos]] <- tibble(
          modelo = nombre_modelo,
          dimension = dim_nm,
          categoria = cat_i,
          indicador = ind,
          media = ci_t$media[1],
          mediana = qs[2],
          p25 = qs[1],
          p75 = qs[3],
          p95 = qs[4],
          p99 = qs[5],
          se_muestral = ci_t$se_muestral[1],
          ic_muestral_inf = ci_t$ic_inf[1],
          ic_muestral_sup = ci_t$ic_sup[1],
          se_delta = ci_d$se_delta[1],
          ic_delta_inf = ci_d$ic_delta_inf[1],
          ic_delta_sup = ci_d$ic_delta_sup[1],
          metodo_ic_principal = metodo,
          ic95_inf = ic_inf,
          ic95_sup = ic_sup,
          n_valido = ci_t$n_valido[1]
        )
        pos <- pos + 1
      }
    }
  }

  dplyr::bind_rows(out)
}

guardar_graficos_valores <- function(tabla, nombre_modelo) {
  carpeta_graf <- file.path(carpeta_04, "graficos", nombre_modelo)
  crear_dir(carpeta_graf)

  tabla_vol <- tabla %>%
    dplyr::filter(indicador == "VoL_predicho", dimension != "muestra_general") %>%
    dplyr::mutate(categoria = factor(categoria, levels = unique(categoria)))

  if (nrow(tabla_vol) == 0) return(invisible(NULL))

  for (dim_i in unique(tabla_vol$dimension)) {
    p <- tabla_vol %>%
      dplyr::filter(dimension == dim_i) %>%
      ggplot2::ggplot(ggplot2::aes(x = media, y = reorder(categoria, media))) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = ic95_inf, xmax = ic95_sup), height = 0.15) +
      ggplot2::labs(x = "VoL predicho", y = NULL, title = paste0("VoL predicho - ", dim_i)) +
      ggplot2::theme_minimal()

    ggplot2::ggsave(
      filename = file.path(carpeta_graf, paste0("vol_predicho_", nombre_archivo_seguro(dim_i), ".png")),
      plot = p,
      width = 8,
      height = 4.8,
      dpi = 300
    )
  }
}

resultados_valores <- list()

pred_paso3 <- calcular_predicciones_tiempo(res_paso3, base_cont_persona, "paso3_joint_sin_correlacion")
valores_paso3 <- resumir_valores(pred_paso3, res_paso3, "paso3_joint_sin_correlacion")
resultados_valores[["paso3"]] <- valores_paso3

readr::write_csv(pred_paso3, file.path(carpeta_04, "04_predicciones_valores_tiempo_paso3.csv"))
readr::write_csv(valores_paso3, file.path(carpeta_04, "04_valores_tiempo_IC_paso3.csv"))
guardar_graficos_valores(valores_paso3, "paso3")

if (!is.null(res_paso4)) {
  pred_paso4 <- calcular_predicciones_tiempo(res_paso4, base_cont_persona, "paso4_joint_con_correlacion")
  valores_paso4 <- resumir_valores(pred_paso4, res_paso4, "paso4_joint_con_correlacion")
  resultados_valores[["paso4"]] <- valores_paso4

  readr::write_csv(pred_paso4, file.path(carpeta_04, "04_predicciones_valores_tiempo_paso4.csv"))
  readr::write_csv(valores_paso4, file.path(carpeta_04, "04_valores_tiempo_IC_paso4.csv"))
  guardar_graficos_valores(valores_paso4, "paso4")
}

valores_consolidados <- dplyr::bind_rows(resultados_valores)
readr::write_csv(valores_consolidados, file.path(carpeta_04, "04_valores_tiempo_IC_consolidado.csv"))

###### 13. Resultados finales ######

coef_paso3 <- extraer_coeficientes_modelo(res_paso3) %>% dplyr::mutate(modelo = "paso3_joint_sin_correlacion", .before = 1)
coef_paso4 <- if (!is.null(res_paso4)) extraer_coeficientes_modelo(res_paso4) %>% dplyr::mutate(modelo = "paso4_joint_con_correlacion", .before = 1) else tibble()
coef_consolidados <- dplyr::bind_rows(coef_paso3, coef_paso4)

readr::write_csv(coef_consolidados, file.path(carpeta_06, "06_coeficientes_joint_consolidado.csv"))

resumen_ejecucion <- tibble(
  elemento = c(
    "carpeta_salida", "muestra_joint_personas", "muestra_joint_filas",
    "mejor_paso3", "mejor_paso4", "deconst_paso4",
    "parametros_excluidos_reducido", "iter_paso4_reducido_nm", "iter_paso4_completo_nm",
    "tiempo_max_paso4_horas", "ASC1", "nota_parametros"
  ),
  valor = c(
    carpeta_salida,
    as.character(dplyr::n_distinct(data_nmm$PeID)),
    as.character(nrow(data_nmm)),
    nombre_mejor_paso3,
    ifelse(is.na(nombre_mejor_paso4), "sin_modelo_valido", nombre_mejor_paso4),
    as.character(deconst_paso4),
    paste(params_excluir_reducido, collapse = ", "),
    as.character(ITER_PASO4_REDUCIDO_NM),
    as.character(ITER_PASO4_COMPLETO_NM),
    as.character(TIEMPO_MAX_PASO4_HORAS),
    "Fijo por identificacion como alternativa de referencia",
    "Paso 3 no se reestima; se usan los coeficientes hardcodeados del log como start del paso 4. Paso 4 v6 opcion 6: modelo reducido con correlacion -> modelo completo."
  )
)
readr::write_csv(resumen_ejecucion, file.path(carpeta_salida, "00_resumen_ejecucion_joint.csv"))

writexl::write_xlsx(
  list(
    resumen_ejecucion = resumen_ejecucion,
    inputs = inputs_usados,
    resumen_base = resumen_base,
    metricas_ajuste = metricas_ajuste,
    coeficientes_joint = coef_consolidados,
    valores_tiempo = valores_consolidados,
    comparacion_paso3 = leer_csv_seguro(file.path(carpeta_02, "03_comparacion_candidatos_paso3.csv")),
    comparacion_paso4 = comparacion_paso4
  ),
  path = file.path(carpeta_salida, "joint_final_resultados.xlsx")
)

cat("\n====================================================\n")
cat("JOINT FINAL TERMINADO\n")
cat("====================================================\n")
cat("Carpeta de salida:\n")
cat(normalizePath(carpeta_salida), "\n\n")
cat("Mejor paso 3:\n")
print(nombre_mejor_paso3)
print(extraer_loglik(res_paso3))
cat("\nMejor paso 4:\n")
print(nombre_mejor_paso4)
if (!is.null(res_paso4)) print(extraer_loglik(res_paso4))
cat("\nMetricas:\n")
print(metricas_ajuste)
cat("\nArchivo principal:\n")
cat(normalizePath(file.path(carpeta_salida, "joint_final_resultados.xlsx")), "\n")
cat("====================================================\n")
