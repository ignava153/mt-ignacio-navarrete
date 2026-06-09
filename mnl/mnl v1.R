# ============================================================
# V9 - MNL FINAL DESDE ENUT_II CON RECODIFICACION PREVIA
#
# Archivo de entrada:
#   enut_ii.xlsx
#   hoja: enut_ii
#
# Objetivo:
#   Estimar el modelo final MNL usando directamente la base enut_ii,
#   sin leer resultados previos de V6 ni V7.
#
# Decisiones clave:
#   - El primer paso despues de leer enut_ii es recodificar las variables de modo.
#   - Codificacion original ENUT: 1 = transporte publico y 2 = auto/moto particular.
#   - Codificacion interna V9: 1 = auto/moto particular y 2 = transporte publico.
#   - Las alternativas 3 a 7 se mantienen iguales:
#       3 = Taxi o auto de aplicacion
#       4 = Bicicleta u otros ciclos
#       5 = A pie
#       6 = Otros
#       7 = Multimodal
#   - La alternativa de referencia es 1 = auto/moto particular.
#   - Se usa ing_personal en escala normal, sin dividir por 100.000.
#   - No se aplica disponibilidad modal.
#   - No se reestiman V1 a V6 ni se lee la carpeta V7.
#
# Salida:
#   prueba/v9 - mnl final/
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 0. Configuracion general
# ------------------------------------------------------------

archivo_base <- "enut_ii.xlsx"
hoja_base <- "enut_ii"
carpeta_salida <- "prueba"
carpeta_v9 <- file.path(carpeta_salida, "v9 - mnl final")

alts <- 1:7
ref_alt <- 1  # alternativa interna 1 = auto/moto particular
T_RATIO_CORTE <- 1.96

# TRUE usa valores iniciales hardcodeados desde la ultima V7 corregida,
# solo para acelerar y estabilizar la estimacion. El codigo NO lee archivos V7.
USAR_STARTS_V7_CORREGIDO <- FALSE

set.seed(42)

# ------------------------------------------------------------
# 1. Paquetes
# ------------------------------------------------------------

paquetes <- c(
  "readxl", "dplyr", "tidyr", "tibble", "readr", "writexl",
  "stringr", "magrittr", "data.table"
)

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
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

# ------------------------------------------------------------
# 2. Funciones auxiliares
# ------------------------------------------------------------

a_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

limpiar_txt <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("\\s+", " ", x)
  x[is.na(x)] <- ""
  x
}

crear_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

modelo_ok <- function(obj) {
  if (is.null(obj)) return(FALSE)
  ll <- tryCatch(as.numeric(logLik(obj)), error = function(e) NA_real_)
  if (is.finite(ll)) return(TRUE)
  salida <- tryCatch(capture.output(summary(obj)), error = function(e) character(0))
  any(grepl("Log-Likelihood", salida))
}

extraer_loglik <- function(modelo) {
  ll <- tryCatch(as.numeric(logLik(modelo)), error = function(e) NA_real_)
  if (is.finite(ll)) return(ll)

  salida <- tryCatch(capture.output(summary(modelo)), error = function(e) character(0))
  linea <- salida[grepl("Log-Likelihood", salida)]

  if (length(linea) > 0) {
    valor <- stringr::str_extract(linea[1], "-?[0-9]+\\.?[0-9]*")
    return(a_num(valor))
  }

  NA_real_
}

extraer_coeficientes <- function(modelo) {
  if (!modelo_ok(modelo)) {
    return(tibble(error = "Modelo nulo o sin Log-Likelihood valido"))
  }

  out <- tryCatch({
    sm <- summary(modelo)
    tb <- as.data.frame(sm$estimate)
    tb$parametro <- rownames(tb)
    rownames(tb) <- NULL
    tb <- tb %>% select(parametro, everything())

    nombres <- names(tb)
    col_est <- nombres[grepl("estimate|Estimate", nombres)][1]
    col_se <- nombres[grepl("std|Std|error|Error", nombres)][1]
    col_t <- nombres[grepl("t ratio|t-value|t value|t.ratio|t_ratio|tvalue|t-stat|t stat", nombres, ignore.case = TRUE)][1]

    if (is.na(col_est) || is.null(col_est)) {
      col_est <- nombres[2]
    }

    tb <- tb %>% mutate(Estimate = a_num(.data[[col_est]]))

    if (!is.na(col_t) && !is.null(col_t)) {
      tb <- tb %>% mutate(t_ratio = a_num(.data[[col_t]]))
    } else if (!is.na(col_se) && !is.null(col_se)) {
      tb <- tb %>% mutate(t_ratio = Estimate / a_num(.data[[col_se]]))
    } else {
      tb <- tb %>% mutate(t_ratio = NA_real_)
    }

    tb %>%
      mutate(
        abs_t_ratio = abs(t_ratio),
        odds_ratio = exp(Estimate)
      )
  }, error = function(e) {
    tibble(error = conditionMessage(e))
  })

  out
}

calcular_ll_nulo_constantes <- function(choice) {
  tab <- table(choice)
  n <- sum(tab)
  p <- as.numeric(tab) / n
  sum(as.numeric(tab) * log(p))
}

crear_metricas <- function(modelo, par_d, data_nmm, nombre_modelo) {
  coef <- extraer_coeficientes(modelo)
  ll <- extraer_loglik(modelo)

  n_obs <- nrow(data_nmm)
  n_personas <- dplyr::n_distinct(data_nmm$PeID)
  k_estimados <- if ("parametro" %in% names(coef)) nrow(coef) else NA_integer_
  k_par_d <- length(par_d)
  k_usado <- k_estimados

  ll0_constantes <- calcular_ll_nulo_constantes(data_nmm$choice)
  ll0_equiprobable <- n_obs * log(1 / 7)

  tibble(
    modelo = nombre_modelo,
    logLik = ll,
    n_parametros_estimados = k_estimados,
    n_parametros_par_d = k_par_d,
    n_obs = n_obs,
    n_personas = n_personas,
    LL0_constantes = ll0_constantes,
    LL0_equiprobable = ll0_equiprobable,
    AIC = -2 * ll + 2 * k_usado,
    BIC = -2 * ll + log(n_obs) * k_usado,
    rho2_constantes = 1 - (ll / ll0_constantes),
    rho2_ajustado_constantes = 1 - ((ll - k_usado) / ll0_constantes),
    rho2_equiprobable = 1 - (ll / ll0_equiprobable),
    rho2_ajustado_equiprobable = 1 - ((ll - k_usado) / ll0_equiprobable)
  )
}

crear_info_variable <- function(var, prefijo, alts_incluidas) {
  params <- paste0(prefijo, "_", alts_incluidas)
  names(params) <- as.character(alts_incluidas)
  list(var = var, prefijo = prefijo, params = params)
}

crear_eq_mnl_final <- function(infos, alts = 1:7, ref_alt = 1) {
  eq <- character(length(alts))

  for (j in alts) {
    if (j == ref_alt) {
      # Referencia: auto/moto particular. ASC1 queda fijo por identificacion.
      eq[j] <- paste0("ASC", j)
    } else {
      params_j <- unlist(lapply(infos, function(info) {
        p <- info$params[as.character(j)]
        v <- info$var
        if (is.na(p) || is.null(p) || length(p) == 0) return(NULL)
        paste0(p, "*", v)
      }))
      eq[j] <- paste(c(paste0("ASC", j), params_j), collapse = " + ")
    }
  }

  eq
}

obtener_par_d_final <- function(eq_d, ref_alt = 1) {
  param_pattern <- "[A-Z]+[A-Z0-9]*_[0-9]+|ASC[0-9]+"
  params <- unique(unlist(stringr::str_extract_all(eq_d, param_pattern)))
  params <- params[!is.na(params) & nzchar(params)]

  # nmm normaliza la constante asociada a la alternativa 1.
  # Como la base ya fue recodificada, ASC1 corresponde a auto/moto particular.
  c(paste0("ASC", ref_alt), setdiff(params, paste0("ASC", ref_alt)))
}

crear_start <- function(par_d, start_hardcodeado = NULL, usar_start_hardcodeado = TRUE) {
  start <- stats::setNames(rep(0, length(par_d)), par_d)

  if (usar_start_hardcodeado && !is.null(start_hardcodeado)) {
    comunes <- intersect(names(start), names(start_hardcodeado))
    start[comunes] <- start_hardcodeado[comunes]
  }

  start[paste0("ASC", ref_alt)] <- 0
  start[!is.finite(start)] <- 0
  start
}

ajustar_mnl <- function(nombre, carpeta, data_nmm, eq_d, par_d, start_v) {
  crear_dir(carpeta)

  archivo_rds <- file.path(carpeta, paste0(nombre, ".rds"))
  archivo_log <- file.path(carpeta, paste0(nombre, "_log.txt"))

  inicio <- Sys.time()

  sink(archivo_log, split = TRUE)
  cat("\n====================================================\n")
  cat("Ejecutando:", nombre, "\n")
  cat("Inicio:", format(inicio, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("N parametros indicados en par_d:", length(par_d), "\n")
  cat("Alternativas internas V9: 1=Auto/moto, 2=TP, 3=Taxi/app, 4=Bici/ciclos, 5=A pie, 6=Otros, 7=Multimodal\n")
  cat("Referencia fijada por construccion: ASC1 = Auto/moto particular\n")
  cat("Primer parametro en par_d:", par_d[1], "\n")
  cat("====================================================\n")

  res <- tryCatch({
    nmm(
      data = data_nmm,
      eq_type = "disc",
      eq_d = eq_d,
      par_d = par_d,
      start_v = start_v,
      fixed_term = FALSE,
      best_method = FALSE,
      DEoptim_run = FALSE,
      try_last_DEoptim = FALSE,
      opt_method = "BFGS",
      miterlim = 20000
    )
  }, error = function(e) {
    cat("\nERROR BFGS en", nombre, ":\n")
    cat(conditionMessage(e), "\n")
    NULL
  })

  if (!modelo_ok(res)) {
    cat("\nIntentando respaldo con Nelder-Mead...\n")

    res <- tryCatch({
      nmm(
        data = data_nmm,
        eq_type = "disc",
        eq_d = eq_d,
        par_d = par_d,
        start_v = start_v,
        fixed_term = FALSE,
        best_method = FALSE,
        DEoptim_run = FALSE,
        try_last_DEoptim = FALSE,
        opt_method = "NM",
        numerical_deriv = FALSE,
        miterlim = 50000
      )
    }, error = function(e) {
      cat("\nERROR NM en", nombre, ":\n")
      cat(conditionMessage(e), "\n")
      NULL
    })
  }

  fin <- Sys.time()

  if (!is.null(res)) {
    cat("\nResumen del modelo:\n")
    print(summary(res))
    saveRDS(res, archivo_rds)
  }

  cat("\nFin:", format(fin, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Duracion minutos:", round(as.numeric(difftime(fin, inicio, units = "mins")), 2), "\n")
  sink()

  res
}

# ------------------------------------------------------------
# 3. Carpetas
# ------------------------------------------------------------

crear_dir(carpeta_salida)
crear_dir(carpeta_v9)

# ------------------------------------------------------------
# 4. Lectura directa de enut_ii
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 4.1 Recodificacion previa de modos de transporte
# ------------------------------------------------------------
# Este es el primer ajuste metodologico sobre la base:
# se intercambian los codigos 1 y 2 en todas las variables de modo
# usadas por el MNL, antes de construir filtros, base larga o modelo.
#
# Original ENUT: 1 = transporte publico; 2 = auto/moto particular.
# Interno V9:    1 = auto/moto particular; 2 = transporte publico.
# Codigos 3 a 7 se conservan iguales.

vars_modo_mnl <- c("modo_to", "modo_ed", "modo_tdnr", "modo_tcnr", "modo_cp")

recodificar_modo_auto_ref <- function(x) {
  x_num <- a_num(x)
  dplyr::case_when(
    x_num == 1 ~ 2,
    x_num == 2 ~ 1,
    x_num %in% 3:7 ~ x_num,
    TRUE ~ x_num
  )
}

# Se guardan copias de control de los codigos originales.
for (v in vars_modo_mnl) {
  if (v %in% names(base_original)) {
    base_original[[paste0(v, "_original_enut")]] <- a_num(base_original[[v]])
    base_original[[v]] <- recodificar_modo_auto_ref(base_original[[v]])
  }
}

control_recodificacion <- dplyr::bind_rows(lapply(vars_modo_mnl, function(v) {
  tibble::tibble(
    variable = v,
    original_enut = base_original[[paste0(v, "_original_enut")]],
    interno_v9 = base_original[[v]]
  ) %>%
    dplyr::filter(original_enut %in% 1:7 | interno_v9 %in% 1:7) %>%
    dplyr::count(variable, original_enut, interno_v9, name = "n")
})) %>%
  dplyr::arrange(variable, original_enut, interno_v9)

# ------------------------------------------------------------
# 5. Verificacion de variables necesarias
# ------------------------------------------------------------

vars_necesarias <- c(
  "id_persona",
  "modo_to", "modo_ed", "modo_tdnr", "modo_tcnr", "modo_cp",
  "t_paid_work", "t_education", "t_domestic_work", "t_care_work", "t_personal_care",
  "sexo", "macrozona", "edad_anios",
  "n_menores_18", "n_trabajadores", "n_profesionales", "vive_pareja",
  "nivel_escolaridad", "quintil", "ing_personal"
)

faltantes <- setdiff(vars_necesarias, names(base_original))
if (length(faltantes) > 0) {
  stop(paste0("Faltan variables necesarias:\n", paste(faltantes, collapse = "\n")))
}

# ------------------------------------------------------------
# 6. Variables explicativas a nivel de persona
# ------------------------------------------------------------

base_persona <- base_original %>%
  mutate(
    # Se mantiene la codificacion usada en el flujo V7 para comparabilidad.
    female = as.integer(a_num(sexo) == 1),

    macrozona_limpia = limpiar_txt(macrozona),
    zona_centro = as.integer(macrozona_limpia %in% c("centro", "metropolitana")),
    zona_sur = as.integer(macrozona_limpia == "sur"),
    # Norte queda como referencia.

    edad_tmp = a_num(edad_anios),
    edad_18_24 = as.integer(!is.na(edad_tmp) & edad_tmp >= 18 & edad_tmp <= 24),
    edad_45_64 = as.integer(!is.na(edad_tmp) & edad_tmp >= 45 & edad_tmp <= 64),
    edad_65mas = as.integer(!is.na(edad_tmp) & edad_tmp >= 65),
    # 25 a 44 queda como referencia.

    n_menores_18 = a_num(n_menores_18),
    n_trabajadores = a_num(n_trabajadores),
    n_profesionales = a_num(n_profesionales),
    vive_pareja = as.integer(a_num(vive_pareja) == 1),

    nivel_escolaridad_limpia = limpiar_txt(nivel_escolaridad),
    educ_secundaria = as.integer(nivel_escolaridad_limpia == "secundaria"),
    educ_tecnica = as.integer(nivel_escolaridad_limpia == "tecnica"),
    educ_universitaria = as.integer(nivel_escolaridad_limpia == "universitaria"),
    # Primaria/ninguna queda como referencia.

    quintil_num = a_num(quintil),
    quintil_2 = as.integer(quintil_num == 2),
    quintil_3 = as.integer(quintil_num == 3),
    quintil_4 = as.integer(quintil_num == 4),
    quintil_5 = as.integer(quintil_num == 5),
    # Quintil 1 queda como referencia.

    ing_personal = a_num(ing_personal)
  ) %>%
  select(
    id_persona,
    female,
    zona_centro,
    zona_sur,
    edad_18_24,
    edad_45_64,
    edad_65mas,
    n_menores_18,
    n_trabajadores,
    n_profesionales,
    vive_pareja,
    educ_secundaria,
    educ_tecnica,
    educ_universitaria,
    quintil_2,
    quintil_3,
    quintil_4,
    quintil_5,
    ing_personal
  )

# ------------------------------------------------------------
# 7. Base larga persona-modulo para MNL
# ------------------------------------------------------------

mapa_modulos <- tibble::tribble(
  ~modulo, ~modo_var, ~tiempo_modulo_var,
  "TO", "modo_to", "t_paid_work",
  "ED", "modo_ed", "t_education",
  "TD", "modo_tdnr", "t_domestic_work",
  "TC", "modo_tcnr", "t_care_work",
  "CP", "modo_cp", "t_personal_care"
)

base_long <- dplyr::bind_rows(lapply(seq_len(nrow(mapa_modulos)), function(i) {
  tibble(
    id_persona = base_original$id_persona,
    modulo = mapa_modulos$modulo[i],
    choice = a_num(base_original[[mapa_modulos$modo_var[i]]]),
    T_modulo_sem = a_num(base_original[[mapa_modulos$tiempo_modulo_var[i]]])
  )
})) %>%
  mutate(
    modulo = factor(modulo, levels = c("TO", "TD", "TC", "ED", "CP")),

    # Alternativas internas V9, con auto/moto como 1 y transporte publico como 2.
    choice_interno_v9 = ifelse(choice %in% 1:7, choice, NA_real_),
    choice = choice_interno_v9,

    mod_TD = as.integer(modulo == "TD"),
    mod_TC = as.integer(modulo == "TC"),
    mod_ED = as.integer(modulo == "ED"),
    mod_CP = as.integer(modulo == "CP"),
    # TO queda como modulo de referencia.

    T_mod_10h = T_modulo_sem / 10
  ) %>%
  left_join(base_persona, by = "id_persona")

# ------------------------------------------------------------
# 8. Filtros para MNL
# ------------------------------------------------------------

base_joint <- base_long %>%
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
  arrange(id_persona, modulo) %>%
  group_by(id_persona) %>%
  mutate(
    PeID = cur_group_id(),
    WeID = row_number()
  ) %>%
  ungroup()

for (j in 1:7) {
  base_joint[[paste0("avl_", j)]] <- 1
  base_joint[[paste0("chc_", j)]] <- ifelse(base_joint$choice == j, 1, 0)
  base_joint[[paste0("wd_", j)]] <- 1
}
base_joint$wc_1 <- 1

# Variables que usara nmm.
data_nmm <- base_joint %>%
  transmute(
    PeID = as.numeric(PeID),
    WeID = as.numeric(WeID),
    choice = as.numeric(choice),

    mod_TD = as.numeric(mod_TD),
    mod_TC = as.numeric(mod_TC),
    mod_ED = as.numeric(mod_ED),
    mod_CP = as.numeric(mod_CP),
    T_mod_10h = as.numeric(T_mod_10h),
    female = as.numeric(female),
    zona_centro = as.numeric(zona_centro),
    zona_sur = as.numeric(zona_sur),
    edad_18_24 = as.numeric(edad_18_24),
    edad_45_64 = as.numeric(edad_45_64),
    edad_65mas = as.numeric(edad_65mas),
    n_menores_18 = as.numeric(n_menores_18),
    n_trabajadores = as.numeric(n_trabajadores),
    n_profesionales = as.numeric(n_profesionales),
    vive_pareja = as.numeric(vive_pareja),
    educ_secundaria = as.numeric(educ_secundaria),
    educ_tecnica = as.numeric(educ_tecnica),
    educ_universitaria = as.numeric(educ_universitaria),
    quintil_2 = as.numeric(quintil_2),
    quintil_3 = as.numeric(quintil_3),
    quintil_4 = as.numeric(quintil_4),
    quintil_5 = as.numeric(quintil_5),
    ing_personal = as.numeric(ing_personal),

    avl_1 = as.numeric(avl_1), avl_2 = as.numeric(avl_2), avl_3 = as.numeric(avl_3),
    avl_4 = as.numeric(avl_4), avl_5 = as.numeric(avl_5), avl_6 = as.numeric(avl_6), avl_7 = as.numeric(avl_7),
    chc_1 = as.numeric(chc_1), chc_2 = as.numeric(chc_2), chc_3 = as.numeric(chc_3),
    chc_4 = as.numeric(chc_4), chc_5 = as.numeric(chc_5), chc_6 = as.numeric(chc_6), chc_7 = as.numeric(chc_7),
    wd_1 = as.numeric(wd_1), wd_2 = as.numeric(wd_2), wd_3 = as.numeric(wd_3),
    wd_4 = as.numeric(wd_4), wd_5 = as.numeric(wd_5), wd_6 = as.numeric(wd_6), wd_7 = as.numeric(wd_7),
    wc_1 = as.numeric(wc_1)
  ) %>%
  arrange(PeID, WeID)

# ------------------------------------------------------------
# 9. Resumen y exportacion de base preparada
# ------------------------------------------------------------

resumen_base <- tibble(
  indicador = c(
    "N personas base original enut_ii",
    "N filas persona-modulo MNL",
    "N personas MNL",
    "Promedio filas persona-modulo por persona",
    "Media T_mod_10h",
    "Proporcion female",
    "Proporcion zona centro+metropolitana",
    "Proporcion zona sur",
    "Proporcion edad 18 a 24",
    "Proporcion edad 45 a 64",
    "Proporcion edad 65 o mas",
    "Media n_menores_18",
    "Media n_trabajadores",
    "Media n_profesionales",
    "Proporcion vive pareja",
    "Proporcion educ secundaria",
    "Proporcion educ tecnica",
    "Proporcion educ universitaria",
    "Proporcion quintil 2",
    "Proporcion quintil 3",
    "Proporcion quintil 4",
    "Proporcion quintil 5",
    "Media ingreso personal normal"
  ),
  valor = c(
    nrow(base_original),
    nrow(data_nmm),
    dplyr::n_distinct(data_nmm$PeID),
    nrow(data_nmm) / dplyr::n_distinct(data_nmm$PeID),
    mean(data_nmm$T_mod_10h, na.rm = TRUE),
    mean(data_nmm$female, na.rm = TRUE),
    mean(data_nmm$zona_centro, na.rm = TRUE),
    mean(data_nmm$zona_sur, na.rm = TRUE),
    mean(data_nmm$edad_18_24, na.rm = TRUE),
    mean(data_nmm$edad_45_64, na.rm = TRUE),
    mean(data_nmm$edad_65mas, na.rm = TRUE),
    mean(data_nmm$n_menores_18, na.rm = TRUE),
    mean(data_nmm$n_trabajadores, na.rm = TRUE),
    mean(data_nmm$n_profesionales, na.rm = TRUE),
    mean(data_nmm$vive_pareja, na.rm = TRUE),
    mean(data_nmm$educ_secundaria, na.rm = TRUE),
    mean(data_nmm$educ_tecnica, na.rm = TRUE),
    mean(data_nmm$educ_universitaria, na.rm = TRUE),
    mean(data_nmm$quintil_2, na.rm = TRUE),
    mean(data_nmm$quintil_3, na.rm = TRUE),
    mean(data_nmm$quintil_4, na.rm = TRUE),
    mean(data_nmm$quintil_5, na.rm = TRUE),
    mean(data_nmm$ing_personal, na.rm = TRUE)
  )
)

freq_choice <- base_joint %>%
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

readr::write_csv(control_recodificacion, file.path(carpeta_v9, "00_control_recodificacion_modos_v9.csv"))
readr::write_csv(resumen_base, file.path(carpeta_v9, "01_resumen_base_v9.csv"))
readr::write_csv(freq_choice, file.path(carpeta_v9, "02_frecuencia_choice_por_modulo_v9.csv"))
readr::write_csv(data_nmm, file.path(carpeta_v9, "03_data_nmm_v9_desde_enut_ii.csv"))
readr::write_csv(base_joint, file.path(carpeta_v9, "04_base_mnl_larga_v9_desde_enut_ii.csv"))

# ------------------------------------------------------------
# 10. Especificacion final V9 con codificacion interna recodificada
# ------------------------------------------------------------

infos_final <- list(
  crear_info_variable("mod_TD", "BTD", c(2, 4, 5, 6)),
  crear_info_variable("mod_TC", "BTC", c(2, 4, 6, 7)),
  crear_info_variable("mod_ED", "BED", c(2, 6)),
  crear_info_variable("mod_CP", "BCP", c(3, 4)),
  crear_info_variable("T_mod_10h", "BTMOD", c(5, 6)),
  crear_info_variable("female", "BFEM", c(2, 3, 4, 5, 6, 7)),
  crear_info_variable("zona_centro", "BZCEN", c(4, 5, 6)),
  crear_info_variable("zona_sur", "BZSUR", c(2, 3, 5, 6, 7)),
  crear_info_variable("edad_18_24", "BAGE18", c(2, 5, 7)),
  crear_info_variable("edad_65mas", "BAGE65", c(2, 3, 5)),
  crear_info_variable("n_trabajadores", "BNTRAB", c(2)),
  crear_info_variable("n_profesionales", "BNPROF", c(4)),
  crear_info_variable("vive_pareja", "BPAREJA", c(2, 3, 4, 5, 6, 7)),
  crear_info_variable("educ_secundaria", "BESC", c(2, 4, 5)),
  crear_info_variable("educ_tecnica", "BTEC", c(2, 5, 7)),
  crear_info_variable("educ_universitaria", "BUNI", c(2, 3, 4, 5, 6, 7)),
  crear_info_variable("quintil_3", "BQ3", c(5)),
  crear_info_variable("quintil_4", "BQ4", c(2, 5)),
  crear_info_variable("quintil_5", "BQ5", c(2, 5)),
  crear_info_variable("ing_personal", "BING", c(2, 4, 5, 7))
)

eq_final <- crear_eq_mnl_final(infos_final, alts = alts, ref_alt = ref_alt)
par_final <- obtener_par_d_final(eq_final, ref_alt = ref_alt)

# Valores iniciales equivalentes al V7 corregido, coherentes con la codificacion interna V9.
start_v7_corregido <- c(
  ASC1 = 0,
  ASC2 = 0.0000002,
  ASC3 = -3.1539714,
  ASC4 = -1.4307888,
  ASC5 = 0.1411883,
  ASC6 = -2.9892158,
  ASC7 = -1.0168771,
  BAGE18_2 = 0.4308654,
  BAGE18_5 = 0.3283859,
  BAGE18_7 = 0.3280946,
  BAGE65_2 = 0.5420080,
  BAGE65_3 = 0.9365897,
  BAGE65_5 = 0.5229731,
  BCP_3 = 0.7407117,
  BCP_4 = -1.9280787,
  BED_2 = 0.7462276,
  BED_6 = -1.5843620,
  BESC_2 = -0.1901781,
  BESC_4 = -0.6423730,
  BESC_5 = -0.3061155,
  BFEM_2 = 0.8114234,
  BFEM_3 = 0.9491702,
  BFEM_4 = -0.9825673,
  BFEM_5 = 0.4548880,
  BFEM_6 = -0.4678432,
  BFEM_7 = 0.7080116,
  BING_2 = -0.0011857,
  BING_4 = -0.0017287,
  BING_5 = -0.0011506,
  BING_7 = -0.0007730,
  BNPROF_4 = -0.3289174,
  BNTRAB_2 = 0.1363932,
  BPAREJA_2 = -0.7030557,
  BPAREJA_3 = -0.7677645,
  BPAREJA_4 = -0.4007936,
  BPAREJA_5 = -0.6260755,
  BPAREJA_6 = -0.3926549,
  BPAREJA_7 = -0.5321743,
  BQ3_5 = -0.2564893,
  BQ4_2 = -0.2621708,
  BQ4_5 = -0.3328515,
  BQ5_2 = -0.5590338,
  BQ5_5 = -0.5138385,
  BTC_2 = -1.1842457,
  BTC_4 = -2.2255365,
  BTC_6 = -1.1577014,
  BTC_7 = -0.9910250,
  BTD_2 = -0.8287957,
  BTD_4 = -0.8887835,
  BTD_5 = 1.1323118,
  BTD_6 = -3.0055440,
  BTEC_2 = -0.5017511,
  BTEC_5 = -0.5912050,
  BTEC_7 = -0.2648342,
  BTMOD_5 = -0.0494564,
  BTMOD_6 = 0.4751504,
  BUNI_2 = -0.8074480,
  BUNI_3 = -0.7918691,
  BUNI_4 = -0.8346559,
  BUNI_5 = -0.8365787,
  BUNI_6 = -0.8910167,
  BUNI_7 = -0.5397468,
  BZCEN_4 = 0.9125662,
  BZCEN_5 = 0.2693668,
  BZCEN_6 = -0.5894464,
  BZSUR_2 = -0.4713234,
  BZSUR_3 = -0.4528524,
  BZSUR_5 = -0.3315791,
  BZSUR_6 = -0.6673909,
  BZSUR_7 = -0.2968551
)

start_final <- crear_start(
  par_d = par_final,
  start_hardcodeado = start_v7_corregido,
  usar_start_hardcodeado = USAR_STARTS_V7_CORREGIDO
)

writeLines(eq_final, con = file.path(carpeta_v9, "v9_eq_final.txt"))
readr::write_csv(tibble(parametro = par_final), file.path(carpeta_v9, "v9_par_d.csv"))
readr::write_csv(tibble(parametro = par_final, start = start_final[par_final]), file.path(carpeta_v9, "v9_start.csv"))

# ------------------------------------------------------------
# 11. Estimar modelo V9 independiente
# ------------------------------------------------------------

modelo_v9 <- ajustar_mnl(
  nombre = "v9_mnl_final",
  carpeta = carpeta_v9,
  data_nmm = data_nmm,
  eq_d = eq_final,
  par_d = par_final,
  start_v = start_final
)

if (!modelo_ok(modelo_v9)) {
  stop("El modelo V9 independiente no pudo estimarse correctamente.")
}

# ------------------------------------------------------------
# 12. Guardar resultados
# ------------------------------------------------------------

coef_v9 <- extraer_coeficientes(modelo_v9)
metricas_v9 <- crear_metricas(
  modelo = modelo_v9,
  par_d = par_final,
  data_nmm = data_nmm,
  nombre_modelo = "v9_mnl_final"
)

no_sig <- coef_v9 %>%
  filter(
    !grepl("^ASC", parametro),
    !is.na(abs_t_ratio),
    abs_t_ratio < T_RATIO_CORTE
  ) %>%
  select(parametro, Estimate, t_ratio, abs_t_ratio)

param_pattern <- "[A-Z]+[A-Z0-9]*_[0-9]+|ASC[0-9]+"
params_eq <- unique(unlist(stringr::str_extract_all(eq_final, param_pattern)))
chequeo_params <- tibble(
  parametro = par_final,
  aparece_en_eq = parametro %in% params_eq,
  aparece_en_summary = parametro %in% coef_v9$parametro,
  es_constante = grepl("^ASC", parametro),
  comentario = case_when(
    parametro == "ASC1" & !aparece_en_summary ~ "Correcto: ASC1 es auto/moto y queda fijo como referencia",
    TRUE ~ ""
  )
)

asc_estimados <- coef_v9 %>%
  filter(grepl("^ASC", parametro)) %>%
  select(parametro, Estimate, t_ratio, abs_t_ratio, everything())

asc_final <- bind_rows(
  tibble(
    parametro = "ASC1",
    alternativa = "1 Auto/moto particular",
    Estimate = 0,
    t_ratio = NA_real_,
    abs_t_ratio = NA_real_,
    condicion = "Fijo como referencia"
  ),
  asc_estimados %>%
    transmute(
      parametro,
      alternativa = case_when(
        parametro == "ASC2" ~ "2 Transporte publico",
        parametro == "ASC3" ~ "3 Taxi/app",
        parametro == "ASC4" ~ "4 Bicicleta/ciclos",
        parametro == "ASC5" ~ "5 A pie",
        parametro == "ASC6" ~ "6 Otros",
        parametro == "ASC7" ~ "7 Multimodal",
        TRUE ~ parametro
      ),
      Estimate,
      t_ratio,
      abs_t_ratio,
      condicion = "Estimado respecto a auto/moto"
    )
) %>%
  arrange(as.integer(stringr::str_extract(parametro, "[0-9]+")))

advertencias <- tibble(
  chequeo = c(
    "Base de estimacion",
    "Archivo de entrada de datos",
    "Numeracion de alternativas",
    "Referencia",
    "Parametro fijo esperado",
    "Constantes esperadas estimadas",
    "Constantes faltantes entre no referencia",
    "Parametros no constantes con |t-ratio| < 1.96",
    "Ingreso usado",
    "Valores iniciales"
  ),
  resultado = c(
    "V9 independiente: se prepara la base desde enut_ii.xlsx y no se leen resultados previos V6/V7",
    archivo_base,
    "Interna V9: 1 auto/moto, 2 TP, 3 taxi/app, 4 bicicleta/ciclos, 5 a pie, 6 otros, 7 multimodal",
    "1 Auto/moto particular",
    "ASC1",
    paste(intersect(c("ASC2", "ASC3", "ASC4", "ASC5", "ASC6", "ASC7"), coef_v9$parametro), collapse = "; "),
    paste(setdiff(c("ASC2", "ASC3", "ASC4", "ASC5", "ASC6", "ASC7"), coef_v9$parametro), collapse = "; "),
    ifelse(nrow(no_sig) == 0, "Ninguno", paste(no_sig$parametro, collapse = "; ")),
    "ing_personal normal, sin dividir por 100.000",
    ifelse(USAR_STARTS_V7_CORREGIDO, "Hardcodeados desde V7 corregido solo para acelerar; no se leyo ningun archivo V7", "Todos en cero")
  )
)

readr::write_csv(coef_v9, file.path(carpeta_v9, "v9_coeficientes_final.csv"))
readr::write_csv(metricas_v9, file.path(carpeta_v9, "v9_tabla_ajuste_final.csv"))
readr::write_csv(no_sig, file.path(carpeta_v9, "v9_no_significativos.csv"))
readr::write_csv(chequeo_params, file.path(carpeta_v9, "v9_chequeo_parametros.csv"))
readr::write_csv(advertencias, file.path(carpeta_v9, "v9_advertencias.csv"))
readr::write_csv(asc_final, file.path(carpeta_v9, "v9_ASC_final.csv"))
capture.output(summary(modelo_v9), file = file.path(carpeta_v9, "v9_summary_final.txt"))
saveRDS(modelo_v9, file.path(carpeta_v9, "v9_modelo_final.rds"))

writexl::write_xlsx(
  list(
    ajuste_final = metricas_v9,
    ASC_final = asc_final,
    coeficientes_final = coef_v9,
    no_significativos = no_sig,
    chequeo_parametros = chequeo_params,
    advertencias = advertencias,
    resumen_base = resumen_base,
    frecuencia_choice = freq_choice,
    ecuaciones = tibble(alternativa = alts, ecuacion = eq_final),
    start = tibble(parametro = par_final, start = start_final[par_final])
  ),
  path = file.path(carpeta_v9, "v9_resultados_finales.xlsx")
)

cat("\n====================================================\n")
cat("V9 - MNL FINAL TERMINADO\n")
cat("====================================================\n")
cat("Carpeta de salida:\n")
cat(normalizePath(carpeta_v9), "\n\n")
cat("Resultados principales:\n")
cat(normalizePath(file.path(carpeta_v9, "v9_resultados_finales.xlsx")), "\n\n")
cat("Referencia usada:\n")
cat("ASC1 = 0 fijo, correspondiente a auto/moto particular.\n")
cat("El transporte publico queda como ASC2.\n\n")
cat("Frecuencia global de alternativas internas V9:\n")
print(table(data_nmm$choice))
cat("\nMetricas V9:\n")
print(metricas_v9)
cat("\nASC final:\n")
print(asc_final)
cat("\nParametros no constantes con |t-ratio| < 1.96:\n")
print(no_sig)
cat("====================================================\n")
