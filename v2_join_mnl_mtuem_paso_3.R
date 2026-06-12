###### 0. Limpieza y controles ######

rm(list = ls())

archivo_base <- "enut_ii.xlsx"
hoja_base <- "enut_ii"

carpeta_salida <- file.path("prueba", "v1 - SOLO PASO3 checkpoint 10h")
carpeta_mnl <- file.path("mnl", "resultados v2")
carpeta_mtuem <- "mtuem"
carpeta_mtuem_general <- file.path(carpeta_mtuem, "01_muestra_general")
carpeta_mtuem_diag <- file.path(carpeta_mtuem, "00_base_y_diagnosticos")

USAR_RDS_EXISTENTE <- TRUE
USAR_CHECKPOINT_PASO3 <- TRUE

# Version corta: solo estima el paso 3 del joint sin correlacion.
# La idea es tener algun resultado dentro de un maximo de 10 horas.
# El mejor resultado se va guardando cada 1 hora, y tambien se guarda el ultimo estado por ciclo.
EJECUTAR_PASO3_DEOPTIM <- TRUE

TIEMPO_MAX_PASO3_TOTAL_HORAS <- 10
CHECKPOINT_CADA_HORAS <- 1

# Controles internos. DEoptim se corre en ciclos cortos y el NM se usa para pulir los checkpoints.
ITER_DEOPTIM_POR_CICLO <- 20
ITER_NM_INICIAL_PASO3 <- 50
ITER_NM_CHECKPOINT_PASO3 <- 80
ITER_NM_FINAL_PASO3 <- 200
MAX_CICLOS_DEOPTIM_PASO3 <- 9999

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

###### 1.1 Parche de nmm para paso 3 con checkpoints horarios ######

parchar_nmm_deoptim_paso3_checkpoint <- function(iter_deoptim_ciclo = 20,
                                                  iter_nm_inicial = 50,
                                                  iter_nm_checkpoint = 80,
                                                  iter_nm_final = 200,
                                                  checkpoint_cada_horas = 1,
                                                  tiempo_max_horas = 10,
                                                  max_ciclos = 9999,
                                                  trace_deoptim = FALSE) {
  ns <- asNamespace("nmm")
  try_if_fun <- get("try_if", envir = ns)

  nmm_DEoptim_checkpoint <- function(joint_func, joint_grad, joint_hess,
                                     start_v = NULL, method = NULL, miterlim,
                                     possible_m, deconst = 2, eq_type, corrl) {
    cat("DEoptim + NM con checkpoints horarios dentro de nmm\n")
    cat("iteraciones DEoptim por ciclo:", iter_deoptim_ciclo, "\n")
    cat("NM inicial:", iter_nm_inicial, "| NM checkpoint:", iter_nm_checkpoint, "| NM final:", iter_nm_final, "\n")
    cat("checkpoint cada", checkpoint_cada_horas, "hora(s) | maximo", tiempo_max_horas, "horas\n")

    p <- length(start_v)
    if (is.null(start_v) || p == 0) stop("start_v vacio en nmm_DEoptim_checkpoint")

    # Carpeta de checkpoints. Se define globalmente antes de llamar a nmm().
    carpeta_cp <- get0("CHECKPOINT_PASO3_DIR", ifnotfound = file.path("prueba", "checkpoint_paso3"), inherits = TRUE)
    if (!dir.exists(carpeta_cp)) dir.create(carpeta_cp, recursive = TRUE, showWarnings = FALSE)

    historial_path <- file.path(carpeta_cp, "historial_checkpoints_paso3.csv")
    log_ciclos_path <- file.path(carpeta_cp, "log_ciclos_paso3.csv")

    es_modelo_ok <- function(obj) {
      if (is.null(obj)) return(FALSE)
      if (inherits(obj, "try-error")) return(FALSE)
      if (tryCatch(try_if_fun(obj), error = function(e) TRUE)) return(FALSE)
      est <- tryCatch(obj$estimate, error = function(e) NULL)
      ll <- tryCatch(as.numeric(obj$maximum), error = function(e) NA_real_)
      !is.null(est) && length(est) == p && all(is.finite(est)) && is.finite(ll)
    }

    get_ll <- function(obj) {
      ll <- tryCatch(as.numeric(obj$maximum), error = function(e) NA_real_)
      if (length(ll) == 0 || is.null(ll)) NA_real_ else ll[1]
    }

    evaluar_ll <- function(par) {
      val <- try(joint_func(par), silent = TRUE)
      if (inherits(val, "try-error") || length(val) == 0 || !is.finite(as.numeric(val))) return(NA_real_)
      as.numeric(val)
    }

    nm_corto <- function(st, iterlim) {
      if (is.null(iterlim) || !is.finite(iterlim) || iterlim <= 0) return(NULL)
      st <- as.numeric(st)
      names(st) <- names(start_v)
      if (any(!is.finite(st))) return(NULL)
      if (is.null(joint_hess)) {
        try(maxLik::maxLik(joint_func, start = st, iterlim = iterlim, method = "NM"), silent = TRUE)
      } else {
        try(maxLik::maxLik(joint_func, grad = joint_grad, hess = joint_hess,
                           start = st, iterlim = iterlim, method = "NM"), silent = TRUE)
      }
    }

    guardar_parametros <- function(par, nombre_base) {
      par <- as.numeric(par)
      names(par) <- names(start_v)
      saveRDS(par, file.path(carpeta_cp, paste0(nombre_base, ".rds")))
      readr::write_csv(
        tibble::tibble(parametro = names(par), valor = as.numeric(par)),
        file.path(carpeta_cp, paste0(nombre_base, ".csv"))
      )
    }

    guardar_modelo <- function(obj, nombre_base) {
      if (es_modelo_ok(obj)) {
        saveRDS(obj, file.path(carpeta_cp, paste0(nombre_base, ".rds")))
        readr::write_csv(
          tibble::tibble(parametro = names(obj$estimate), estimate = as.numeric(obj$estimate)),
          file.path(carpeta_cp, paste0(nombre_base, "_coeficientes.csv"))
        )
      }
    }

    registrar_estado <- function(tipo, ciclo, hora_checkpoint, par, obj = NULL, bestval = NA_real_, notas = "") {
      ll_par <- evaluar_ll(par)
      fila <- tibble::tibble(
        momento = as.character(Sys.time()),
        tipo = tipo,
        ciclo = ciclo,
        hora_checkpoint = hora_checkpoint,
        horas_transcurridas = round(as.numeric(difftime(Sys.time(), inicio_global, units = "hours")), 4),
        iter_deoptim_aprox = ciclo * iter_deoptim_ciclo,
        bestval_deoptim = as.numeric(bestval),
        logLik_parametros = ll_par,
        logLik_modelo = get_ll(obj),
        modelo_valido = es_modelo_ok(obj),
        notas = notas
      )

      if (file.exists(historial_path)) {
        viejo <- tryCatch(readr::read_csv(historial_path, show_col_types = FALSE), error = function(e) tibble::tibble())
        readr::write_csv(dplyr::bind_rows(viejo, fila), historial_path)
      } else {
        readr::write_csv(fila, historial_path)
      }
      fila
    }

    guardar_checkpoint <- function(tipo, ciclo, hora_checkpoint, par, obj = NULL, bestval = NA_real_, notas = "") {
      sufijo <- if (is.na(hora_checkpoint)) {
        sprintf("%s_ciclo_%03d", tipo, ciclo)
      } else {
        sprintf("hora_%02d_%s_ciclo_%03d", hora_checkpoint, tipo, ciclo)
      }

      guardar_parametros(par, paste0(sufijo, "_parametros"))
      guardar_parametros(par, "mejor_parametros_actual")

      if (es_modelo_ok(obj)) {
        guardar_modelo(obj, paste0(sufijo, "_modelo"))
        guardar_modelo(obj, "mejor_modelo_actual")
      }

      estado <- registrar_estado(tipo, ciclo, hora_checkpoint, par, obj, bestval, notas)
      saveRDS(estado, file.path(carpeta_cp, paste0(sufijo, "_estado.rds")))
      readr::write_csv(estado, file.path(carpeta_cp, paste0(sufijo, "_estado.csv")))
      invisible(estado)
    }

    make_initialpop <- function(center, lower, upper) {
      center <- as.numeric(center)
      names(center) <- names(start_v)
      step <- pmax(abs(center) * 0.002, 0.0001)
      mat0 <- matrix(rep(center, p), ncol = p, byrow = TRUE)
      pop <- rbind(
        start_v,
        center,
        mat0 + diag(step),
        mat0 - diag(step)
      )
      pop <- pmin(pmax(pop, lower), upper)
      pop <- unique(as.data.frame(pop)) |> as.matrix()
      colnames(pop) <- names(start_v)
      pop
    }

    # Si existe un checkpoint anterior, lo uso como punto de partida para reanudar.
    start_original <- start_v
    archivo_mejor_modelo <- file.path(carpeta_cp, "mejor_modelo_actual.rds")
    archivo_mejor_par <- file.path(carpeta_cp, "mejor_parametros_actual.rds")

    if (isTRUE(get0("USAR_CHECKPOINT_PASO3", ifnotfound = TRUE, inherits = TRUE)) && file.exists(archivo_mejor_modelo)) {
      obj_cp <- tryCatch(readRDS(archivo_mejor_modelo), error = function(e) NULL)
      if (es_modelo_ok(obj_cp)) {
        start_v <- obj_cp$estimate
        cat("Reanudando desde mejor_modelo_actual.rds | logLik:", get_ll(obj_cp), "\n")
      }
    } else if (isTRUE(get0("USAR_CHECKPOINT_PASO3", ifnotfound = TRUE, inherits = TRUE)) && file.exists(archivo_mejor_par)) {
      par_cp <- tryCatch(readRDS(archivo_mejor_par), error = function(e) NULL)
      if (!is.null(par_cp) && length(par_cp) == p && all(is.finite(par_cp))) {
        start_v <- par_cp
        cat("Reanudando desde mejor_parametros_actual.rds\n")
      }
    }

    inicio_global <- Sys.time()
    proximo_checkpoint_hora <- 1
    mejor_par <- as.numeric(start_v)
    names(mejor_par) <- names(start_v)
    mejor_ll <- evaluar_ll(mejor_par)
    mejor_bestval <- ifelse(is.finite(mejor_ll), -mejor_ll, Inf)
    mejor_modelo <- NULL

    # NM inicial corto para tener un modelo base guardable.
    cat("\nNM inicial corto...\n")
    obj_nm_ini <- nm_corto(mejor_par, iter_nm_inicial)
    if (es_modelo_ok(obj_nm_ini)) {
      mejor_modelo <- obj_nm_ini
      mejor_par <- obj_nm_ini$estimate
      mejor_ll <- get_ll(obj_nm_ini)
      mejor_bestval <- -mejor_ll
      cat("NM inicial valido | logLik:", mejor_ll, "\n")
      guardar_checkpoint("nm_inicial", 0, 0, mejor_par, mejor_modelo, mejor_bestval, "checkpoint inicial")
    } else {
      cat("NM inicial no entrego modelo valido. Sigo con los parametros iniciales.\n")
      guardar_checkpoint("inicio_sin_nm_valido", 0, 0, mejor_par, NULL, mejor_bestval, "solo parametros iniciales")
    }

    jj_f <- function(par) {
      val <- try(joint_func(par), silent = TRUE)
      if (inherits(val, "try-error") || length(val) == 0 || !is.finite(as.numeric(val))) return(1e100)
      -as.numeric(val)
    }

    ciclo <- 0
    log_ciclos <- tibble::tibble()

    repeat {
      ciclo <- ciclo + 1
      horas <- as.numeric(difftime(Sys.time(), inicio_global, units = "hours"))
      if (horas >= tiempo_max_horas) break
      if (ciclo > max_ciclos) break

      deconst_use <- max(ceiling(max(abs(mejor_par), na.rm = TRUE)), deconst)
      if (!is.finite(deconst_use) || deconst_use <= 0) deconst_use <- deconst
      lower <- rep(-deconst_use, p)
      upper <- rep(deconst_use, p)
      initialpop <- make_initialpop(mejor_par, lower, upper)

      cat("\nCiclo", ciclo, "| horas:", round(horas, 3), "| logLik actual:", round(mejor_ll, 6), "| deconst:", deconst_use, "\n")

      res_de <- try(
        DEoptim::DEoptim(
          jj_f,
          lower = lower,
          upper = upper,
          control = DEoptim::DEoptim.control(
            trace = trace_deoptim,
            itermax = iter_deoptim_ciclo,
            initialpop = initialpop
          )
        ),
        silent = TRUE
      )

      if (!inherits(res_de, "try-error")) {
        par_de <- as.numeric(res_de$optim$bestmem)
        names(par_de) <- names(start_v)
        val_de <- as.numeric(res_de$optim$bestval)
        ll_de <- -val_de
        if (is.finite(ll_de) && (!is.finite(mejor_ll) || ll_de > mejor_ll)) {
          mejor_par <- par_de
          mejor_ll <- ll_de
          mejor_bestval <- val_de
        }
      } else {
        cat("DEoptim del ciclo", ciclo, "dio error. Sigo con el mejor anterior.\n")
      }

      # Guardo siempre el ultimo estado liviano por si el PC se cae antes de la hora.
      guardar_parametros(mejor_par, "ultimo_estado_parametros")
      estado_ciclo <- tibble::tibble(
        momento = as.character(Sys.time()),
        ciclo = ciclo,
        horas_transcurridas = round(as.numeric(difftime(Sys.time(), inicio_global, units = "hours")), 4),
        iter_deoptim_aprox = ciclo * iter_deoptim_ciclo,
        bestval_deoptim = mejor_bestval,
        logLik_parametros = mejor_ll
      )
      log_ciclos <- dplyr::bind_rows(log_ciclos, estado_ciclo)
      readr::write_csv(log_ciclos, log_ciclos_path)

      cat("Fin ciclo", ciclo, "| logLik parametros:", round(mejor_ll, 6), "| bestval:", round(mejor_bestval, 6), "\n")

      horas <- as.numeric(difftime(Sys.time(), inicio_global, units = "hours"))
      if (horas >= proximo_checkpoint_hora * checkpoint_cada_horas) {
        cat("\nCheckpoint horario", proximo_checkpoint_hora, "| horas:", round(horas, 3), "\n")
        obj_cp <- nm_corto(mejor_par, iter_nm_checkpoint)
        if (es_modelo_ok(obj_cp)) {
          if (is.null(mejor_modelo) || get_ll(obj_cp) > get_ll(mejor_modelo)) {
            mejor_modelo <- obj_cp
            mejor_par <- obj_cp$estimate
            mejor_ll <- get_ll(obj_cp)
            mejor_bestval <- -mejor_ll
          }
          guardar_checkpoint("checkpoint_horario", ciclo, proximo_checkpoint_hora, mejor_par, mejor_modelo, mejor_bestval, "NM de checkpoint valido")
        } else {
          guardar_checkpoint("checkpoint_horario_solo_parametros", ciclo, proximo_checkpoint_hora, mejor_par, mejor_modelo, mejor_bestval, "NM de checkpoint no valido")
        }
        proximo_checkpoint_hora <- proximo_checkpoint_hora + 1
      }
    }

    cat("\nNM final corto desde el mejor punto encontrado...\n")
    obj_final <- nm_corto(mejor_par, iter_nm_final)
    if (es_modelo_ok(obj_final)) {
      if (is.null(mejor_modelo) || get_ll(obj_final) > get_ll(mejor_modelo)) {
        mejor_modelo <- obj_final
        mejor_par <- obj_final$estimate
        mejor_ll <- get_ll(obj_final)
        mejor_bestval <- -mejor_ll
      }
    }

    guardar_checkpoint("final_paso3", ciclo, NA_integer_, mejor_par, mejor_modelo, mejor_bestval, "fin de etapa o limite de tiempo")

    if (es_modelo_ok(mejor_modelo)) {
      mejor_modelo$type <- "DEoptim_NM_checkpoint_10h_paso3"
      return(mejor_modelo)
    }

    stop("No se obtuvo un modelo valido en paso 3. Revisa los parametros guardados en checkpoints.")
  }

  assignInNamespace("nmm_DEoptim", nmm_DEoptim_checkpoint, ns = "nmm")
  invisible(TRUE)
}

parchar_nmm_deoptim_paso3_checkpoint(
  iter_deoptim_ciclo = ITER_DEOPTIM_POR_CICLO,
  iter_nm_inicial = ITER_NM_INICIAL_PASO3,
  iter_nm_checkpoint = ITER_NM_CHECKPOINT_PASO3,
  iter_nm_final = ITER_NM_FINAL_PASO3,
  checkpoint_cada_horas = CHECKPOINT_CADA_HORAS,
  tiempo_max_horas = TIEMPO_MAX_PASO3_TOTAL_HORAS,
  max_ciclos = MAX_CICLOS_DEOPTIM_PASO3,
  trace_deoptim = FALSE
)

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
      "EJECUTAR_PASO3_DEOPTIM",
      "TIEMPO_MAX_PASO3_TOTAL_HORAS", "CHECKPOINT_CADA_HORAS",
      "ITER_DEOPTIM_POR_CICLO", "ITER_NM_INICIAL_PASO3",
      "ITER_NM_CHECKPOINT_PASO3", "ITER_NM_FINAL_PASO3",
      "MAX_CICLOS_DEOPTIM_PASO3"
    ),
    valor = c(
      EJECUTAR_PASO3_DEOPTIM,
      TIEMPO_MAX_PASO3_TOTAL_HORAS, CHECKPOINT_CADA_HORAS,
      ITER_DEOPTIM_POR_CICLO, ITER_NM_INICIAL_PASO3,
      ITER_NM_CHECKPOINT_PASO3, ITER_NM_FINAL_PASO3,
      MAX_CICLOS_DEOPTIM_PASO3
    )
  ),
  file.path(carpeta_00, "00_controles_solo_paso3_checkpoint_10h.csv")
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

###### 8. Paso 3: joint sin correlacion con checkpoints horarios ######

CHECKPOINT_PASO3_DIR <- file.path(carpeta_02, "00_checkpoints_horarios")
crear_dir(CHECKPOINT_PASO3_DIR)

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

start_paso3 <- start_joint_base
deconst_paso3 <- max(DECONST_MIN, ceiling(max(abs(start_paso3), na.rm = TRUE) + DECONST_EXTRA))

readr::write_csv(
  tibble::tibble(parametro = names(start_paso3), start = as.numeric(start_paso3)),
  file.path(carpeta_02, "00_start_paso3.csv")
)
readr::write_csv(
  tibble::tibble(
    control = c("deconst_paso3", "tiempo_max_horas", "checkpoint_cada_horas", "iter_deoptim_por_ciclo", "iter_nm_inicial", "iter_nm_checkpoint", "iter_nm_final"),
    valor = c(deconst_paso3, TIEMPO_MAX_PASO3_TOTAL_HORAS, CHECKPOINT_CADA_HORAS, ITER_DEOPTIM_POR_CICLO, ITER_NM_INICIAL_PASO3, ITER_NM_CHECKPOINT_PASO3, ITER_NM_FINAL_PASO3)
  ),
  file.path(carpeta_02, "00_control_paso3_checkpoint.csv")
)

res_paso3_deoptim <- NULL
if (isTRUE(EJECUTAR_PASO3_DEOPTIM)) {
  res_paso3_deoptim <- guardar_log_modelo(
    nombre = "03A_joint_sin_correlacion_DEoptim_NM_checkpoint_10h",
    expr = quote(
      nmm(
        data = data_nmm,
        eq_type = "joint",
        eq_c = eq_c,
        par_c = par_c,
        eq_d = eq_d,
        par_d = par_d,
        start_v = start_paso3,
        corrl = FALSE,
        weight_paths = FALSE,
        weight_paths_cont = TRUE,
        fixed_term = FALSE,
        best_method = FALSE,
        DEoptim_run = FALSE,
        DEoptim_run_main = TRUE,
        try_last_DEoptim = FALSE,
        deconst = deconst_paso3,
        opt_method = "NM",
        numerical_deriv = FALSE,
        miterlim = ITER_NM_INICIAL_PASO3
      )
    ),
    carpeta = carpeta_02,
    archivo_rds = file.path(carpeta_02, "03A_joint_sin_correlacion_DEoptim_NM_checkpoint_10h.rds"),
    timeout_seg = TIEMPO_MAX_PASO3_TOTAL_HORAS * 3600
  )
}

# Si la etapa se corto por timeout o por cierre del PC, intento rescatar el mejor checkpoint guardado.
res_paso3 <- NULL
origen_res_paso3 <- NA_character_

if (modelo_loglik_ok(res_paso3_deoptim)) {
  res_paso3 <- res_paso3_deoptim
  origen_res_paso3 <- "resultado_devuelto_por_nmm"
} else {
  archivo_cp_modelo <- file.path(CHECKPOINT_PASO3_DIR, "mejor_modelo_actual.rds")
  if (file.exists(archivo_cp_modelo)) {
    res_cp <- tryCatch(readRDS(archivo_cp_modelo), error = function(e) NULL)
    if (modelo_loglik_ok(res_cp)) {
      res_paso3 <- res_cp
      origen_res_paso3 <- "checkpoint_mejor_modelo_actual"
    }
  }
}

if (!modelo_loglik_ok(res_paso3)) {
  stop("El paso 3 no produjo un modelo valido. Revisa la carpeta 00_checkpoints_horarios; puede haber parametros guardados aunque no haya modelo.")
}

saveRDS(res_paso3, file.path(carpeta_02, "03_joint_sin_correlacion_MEJOR.rds"))
readr::write_csv(
  tibble(
    metodo = "paso3_DEoptim_NM_checkpoint_10h",
    origen = origen_res_paso3,
    logLik = extraer_loglik(res_paso3),
    code = extraer_code(res_paso3),
    convergio = modelo_convergio(res_paso3)
  ),
  file.path(carpeta_02, "03_resumen_mejor_paso3.csv")
)

coef_paso3 <- extraer_coeficientes_modelo(res_paso3) %>%
  dplyr::mutate(modelo = "paso3_joint_sin_correlacion", .before = 1)
readr::write_csv(coef_paso3, file.path(carpeta_02, "03_coeficientes_paso3.csv"))

# Guardado liviano del resumen final.
sink(file.path(carpeta_02, "03_summary_paso3.txt"))
cat("Modelo paso 3 joint sin correlacion\n")
cat("Origen resultado:", origen_res_paso3, "\n")
cat("LogLik:", extraer_loglik(res_paso3), "\n")
cat("Code:", extraer_code(res_paso3), "\n")
cat("Convergio:", modelo_convergio(res_paso3), "\n\n")
print(summary(res_paso3))
sink()

cat("\n====================================================\n")
cat("FIN SOLO PASO 3\n")
cat("Mejor logLik paso 3:", extraer_loglik(res_paso3), "\n")
cat("Resultado guardado en:", file.path(carpeta_02, "03_joint_sin_correlacion_MEJOR.rds"), "\n")
cat("Checkpoints horarios en:", CHECKPOINT_PASO3_DIR, "\n")
cat("====================================================\n")

