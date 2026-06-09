# ============================================================
# V9 - DIAGNOSTICO DE CORRELACIONES PARA MODELO MNL FINAL
#
# Objetivo:
#   Calcular matrices de correlacion entre las variables explicativas
#   presentes en el MNL V9 final, sin volver a estimar el modelo.
#
# Archivos de entrada esperados:
#   Preferente:
#     prueba/v9 - mnl final/04_base_mnl_larga_v9_desde_enut_ii.csv
#   Respaldo:
#     prueba/v9 - mnl final/03_data_nmm_v9_desde_enut_ii.csv
#
# Tambien funciona si los archivos estan en el directorio de trabajo.
#
# Salida:
#   prueba/v9 - diagnostico correlaciones/
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 0. Configuracion general
# ------------------------------------------------------------

carpeta_salida <- file.path("prueba", "v9 - diagnostico correlaciones")
carpeta_graficos <- file.path(carpeta_salida, "graficos")

archivo_base_larga <- "04_base_mnl_larga_v9_desde_enut_ii.csv"
archivo_data_nmm <- "03_data_nmm_v9_desde_enut_ii.csv"

UMBRAL_MODERADO <- 0.50
UMBRAL_ALTO <- 0.70

# ------------------------------------------------------------
# 1. Paquetes
# ------------------------------------------------------------

paquetes <- c(
  "dplyr", "tidyr", "tibble", "readr", "data.table", "stringr",
  "writexl", "psych", "ggplot2"
)

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# ------------------------------------------------------------
# 2. Funciones auxiliares
# ------------------------------------------------------------

crear_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

crear_dir(carpeta_salida)
crear_dir(carpeta_graficos)

a_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

buscar_archivo <- function(nombre_archivo) {
  candidatos <- c(
    file.path(".", nombre_archivo),
    file.path("prueba", "v9 - mnl final", nombre_archivo),
    file.path("v9 - mnl final", nombre_archivo)
  )

  candidatos <- candidatos[file.exists(candidatos)]
  if (length(candidatos) > 0) {
    return(normalizePath(candidatos[1], winslash = "/", mustWork = TRUE))
  }

  encontrados <- list.files(
    path = ".",
    pattern = paste0("^", gsub("([.])", "\\\\.", nombre_archivo), "$"),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = FALSE
  )

  if (length(encontrados) > 0) {
    return(normalizePath(encontrados[1], winslash = "/", mustWork = TRUE))
  }

  NA_character_
}

leer_csv_seguro <- function(path) {
  data.table::fread(path, showProgress = FALSE, data.table = FALSE, encoding = "UTF-8")
}

matriz_a_df <- function(mat) {
  if (is.null(mat) || length(mat) == 0) return(tibble())
  mat <- as.matrix(mat)
  tibble::as_tibble(mat, rownames = "variable")
}

matriz_a_pares <- function(mat, metodo, valor_abs = TRUE) {
  if (is.null(mat) || length(mat) == 0) return(tibble())

  mat <- as.matrix(mat)
  if (nrow(mat) == 0 || ncol(mat) == 0) return(tibble())

  mat[lower.tri(mat, diag = TRUE)] <- NA_real_

  as.data.frame(as.table(mat), stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    rename(variable_1 = Var1, variable_2 = Var2, correlacion = Freq) %>%
    filter(!is.na(correlacion)) %>%
    mutate(
      metodo = metodo,
      correlacion_abs = if (valor_abs) abs(correlacion) else correlacion,
      nivel_alerta = case_when(
        correlacion_abs >= UMBRAL_ALTO ~ "alta",
        correlacion_abs >= UMBRAL_MODERADO ~ "moderada",
        TRUE ~ "baja"
      )
    ) %>%
    arrange(desc(correlacion_abs))
}

agregar_familias <- function(pares, info_vars) {
  if (nrow(pares) == 0) return(pares)

  pares %>%
    left_join(
      info_vars %>% select(variable, familia, tipo_variable) %>%
        rename(variable_1 = variable, familia_1 = familia, tipo_1 = tipo_variable),
      by = "variable_1"
    ) %>%
    left_join(
      info_vars %>% select(variable, familia, tipo_variable) %>%
        rename(variable_2 = variable, familia_2 = familia, tipo_2 = tipo_variable),
      by = "variable_2"
    ) %>%
    mutate(
      misma_familia = ifelse(!is.na(familia_1) & !is.na(familia_2) & familia_1 == familia_2, "SI", "NO"),
      comentario = case_when(
        misma_familia == "SI" ~ "Correlacion esperable dentro del mismo bloque de variables",
        correlacion_abs >= UMBRAL_ALTO ~ "Revisar: posible asociacion fuerte entre bloques distintos",
        correlacion_abs >= UMBRAL_MODERADO ~ "Revisar con cautela: asociacion moderada entre bloques distintos",
        TRUE ~ "Baja o manejable"
      )
    )
}

calcular_cor_segura <- function(df, vars, metodo = "pearson") {
  vars_ok <- vars[vars %in% names(df)]
  vars_ok <- vars_ok[sapply(df[vars_ok], function(x) sd(a_num(x), na.rm = TRUE) > 0)]

  if (length(vars_ok) < 2) return(matrix(numeric(0), nrow = 0, ncol = 0))

  mat <- df %>%
    select(all_of(vars_ok)) %>%
    mutate(across(everything(), a_num)) %>%
    as.data.frame()

  suppressWarnings(cor(mat, use = "pairwise.complete.obs", method = metodo))
}

calcular_tetracorica_segura <- function(df, vars_binarias) {
  vars_ok <- vars_binarias[vars_binarias %in% names(df)]

  vars_ok <- vars_ok[sapply(df[vars_ok], function(x) {
    ux <- sort(unique(na.omit(a_num(x))))
    length(ux) == 2 && all(ux %in% c(0, 1)) && sd(a_num(x), na.rm = TRUE) > 0
  })]

  if (length(vars_ok) < 2) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = "No hay suficientes variables dicotomicas validas."))
  }

  x <- df %>%
    select(all_of(vars_ok)) %>%
    mutate(across(everything(), a_num)) %>%
    filter(if_all(everything(), ~ !is.na(.x))) %>%
    as.data.frame()

  if (nrow(x) < 50) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = "Muestra insuficiente para tetracorica."))
  }

  res <- tryCatch({
    psych::tetrachoric(x, correct = 0.5, smooth = TRUE, global = FALSE)
  }, error = function(e) {
    attr(e, "mensaje") <- conditionMessage(e)
    e
  })

  if (inherits(res, "error")) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = paste("Error tetracorica:", attr(res, "mensaje"))))
  }

  list(rho = res$rho, status = "OK")
}

reconstruir_categoricas <- function(base) {
  base_cat <- base

  # Identificador para poder construir base persona unica cuando exista.
  if (!("PeID" %in% names(base_cat))) {
    if ("id_persona" %in% names(base_cat)) {
      base_cat$PeID <- as.numeric(as.factor(base_cat$id_persona))
    } else {
      base_cat$PeID <- seq_len(nrow(base_cat))
    }
  }

  # Modulo del modelo. Si no existe la columna modulo, se reconstruye desde dummies.
  if ("modulo" %in% names(base_cat)) {
    base_cat$modulo_modelo <- as.character(base_cat$modulo)
  } else {
    base_cat$modulo_modelo <- dplyr::case_when(
      "mod_TD" %in% names(base_cat) & a_num(base_cat$mod_TD) == 1 ~ "TD",
      "mod_TC" %in% names(base_cat) & a_num(base_cat$mod_TC) == 1 ~ "TC",
      "mod_ED" %in% names(base_cat) & a_num(base_cat$mod_ED) == 1 ~ "ED",
      "mod_CP" %in% names(base_cat) & a_num(base_cat$mod_CP) == 1 ~ "CP",
      TRUE ~ "TO"
    )
  }

  base_cat <- base_cat %>%
    mutate(
      sexo_modelo = case_when(
        "female" %in% names(base_cat) & a_num(female) == 1 ~ "mujer",
        "female" %in% names(base_cat) & a_num(female) == 0 ~ "hombre",
        TRUE ~ NA_character_
      ),
      territorio_modelo = case_when(
        "zona_centro" %in% names(base_cat) & a_num(zona_centro) == 1 ~ "centro",
        "zona_sur" %in% names(base_cat) & a_num(zona_sur) == 1 ~ "sur",
        "zona_centro" %in% names(base_cat) & "zona_sur" %in% names(base_cat) ~ "norte",
        TRUE ~ NA_character_
      ),
      edad_modelo = case_when(
        "edad_18_24" %in% names(base_cat) & a_num(edad_18_24) == 1 ~ "18_24",
        "edad_65mas" %in% names(base_cat) & a_num(edad_65mas) == 1 ~ "65_mas",
        "edad_18_24" %in% names(base_cat) & "edad_65mas" %in% names(base_cat) ~ "25_64",
        TRUE ~ NA_character_
      ),
      educacion_modelo = case_when(
        "educ_universitaria" %in% names(base_cat) & a_num(educ_universitaria) == 1 ~ "universitaria",
        "educ_tecnica" %in% names(base_cat) & a_num(educ_tecnica) == 1 ~ "tecnica",
        "educ_secundaria" %in% names(base_cat) & a_num(educ_secundaria) == 1 ~ "secundaria",
        "educ_secundaria" %in% names(base_cat) & "educ_tecnica" %in% names(base_cat) & "educ_universitaria" %in% names(base_cat) ~ "base_educativa",
        TRUE ~ NA_character_
      ),
      quintil_modelo = case_when(
        "quintil_5" %in% names(base_cat) & a_num(quintil_5) == 1 ~ "quintil_5",
        "quintil_4" %in% names(base_cat) & a_num(quintil_4) == 1 ~ "quintil_4",
        "quintil_3" %in% names(base_cat) & a_num(quintil_3) == 1 ~ "quintil_3",
        "quintil_2" %in% names(base_cat) & a_num(quintil_2) == 1 ~ "quintil_2",
        "quintil_3" %in% names(base_cat) & "quintil_4" %in% names(base_cat) & "quintil_5" %in% names(base_cat) ~ "quintil_1_o_2",
        TRUE ~ NA_character_
      ),
      pareja_modelo = case_when(
        "vive_pareja" %in% names(base_cat) & a_num(vive_pareja) == 1 ~ "vive_con_pareja",
        "vive_pareja" %in% names(base_cat) & a_num(vive_pareja) == 0 ~ "no_vive_con_pareja",
        TRUE ~ NA_character_
      )
    )

  base_cat$modulo_modelo <- factor(base_cat$modulo_modelo, levels = c("TO", "TD", "TC", "ED", "CP"))
  base_cat$sexo_modelo <- factor(base_cat$sexo_modelo, levels = c("hombre", "mujer"))
  base_cat$territorio_modelo <- factor(base_cat$territorio_modelo, levels = c("norte", "centro", "sur"), ordered = TRUE)
  base_cat$edad_modelo <- factor(base_cat$edad_modelo, levels = c("18_24", "25_64", "65_mas"), ordered = TRUE)
  base_cat$educacion_modelo <- factor(base_cat$educacion_modelo, levels = c("base_educativa", "secundaria", "tecnica", "universitaria"), ordered = TRUE)

  # Si quintil_2 existe, se puede ordenar como quintiles exactos. Si no existe, Q1 y Q2 quedan juntos por construccion del V9.
  niveles_quintil <- if ("quintil_2" %in% names(base_cat)) {
    c("quintil_1_o_2", "quintil_2", "quintil_3", "quintil_4", "quintil_5")
  } else {
    c("quintil_1_o_2", "quintil_3", "quintil_4", "quintil_5")
  }
  base_cat$quintil_modelo <- factor(base_cat$quintil_modelo, levels = niveles_quintil, ordered = TRUE)
  base_cat$pareja_modelo <- factor(base_cat$pareja_modelo, levels = c("no_vive_con_pareja", "vive_con_pareja"))

  base_cat
}

calcular_polychoric_segura <- function(df, vars_ordinales) {
  vars_ok <- vars_ordinales[vars_ordinales %in% names(df)]
  vars_ok <- vars_ok[sapply(df[vars_ok], function(x) length(unique(na.omit(x))) >= 2)]

  if (length(vars_ok) < 2) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = "No hay suficientes variables ordinales validas."))
  }

  x <- df %>%
    select(all_of(vars_ok)) %>%
    filter(if_all(everything(), ~ !is.na(.x))) %>%
    mutate(across(everything(), ~ as.numeric(.x))) %>%
    as.data.frame()

  if (nrow(x) < 50) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = "Muestra insuficiente para policorica."))
  }

  res <- tryCatch({
    psych::polychoric(x, correct = 0.5, smooth = TRUE, global = FALSE)
  }, error = function(e) {
    attr(e, "mensaje") <- conditionMessage(e)
    e
  })

  if (inherits(res, "error")) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = paste("Error policorica:", attr(res, "mensaje"))))
  }

  list(rho = res$rho, status = "OK")
}

calcular_cramers_v <- function(x, y) {
  x <- droplevels(as.factor(x))
  y <- droplevels(as.factor(y))
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]

  if (length(unique(x)) < 2 || length(unique(y)) < 2) return(NA_real_)

  tab <- table(x, y)
  if (any(dim(tab) < 2)) return(NA_real_)

  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n <- sum(tab)
  r <- nrow(tab)
  k <- ncol(tab)

  as.numeric(sqrt((chi / n) / min(k - 1, r - 1)))
}

calcular_matriz_cramer <- function(df, vars_cat) {
  vars_ok <- vars_cat[vars_cat %in% names(df)]
  vars_ok <- vars_ok[sapply(df[vars_ok], function(x) length(unique(na.omit(x))) >= 2)]

  if (length(vars_ok) < 2) return(matrix(numeric(0), nrow = 0, ncol = 0))

  mat <- matrix(NA_real_, nrow = length(vars_ok), ncol = length(vars_ok), dimnames = list(vars_ok, vars_ok))
  for (i in seq_along(vars_ok)) {
    for (j in seq_along(vars_ok)) {
      if (i == j) {
        mat[i, j] <- 1
      } else {
        mat[i, j] <- calcular_cramers_v(df[[vars_ok[i]]], df[[vars_ok[j]]])
      }
    }
  }
  mat
}

calcular_mixedCor_segura <- function(df, vars_continuas, vars_dicotomicas) {
  vars_continuas <- vars_continuas[vars_continuas %in% names(df)]
  vars_dicotomicas <- vars_dicotomicas[vars_dicotomicas %in% names(df)]

  vars_continuas <- vars_continuas[sapply(df[vars_continuas], function(x) sd(a_num(x), na.rm = TRUE) > 0)]
  vars_dicotomicas <- vars_dicotomicas[sapply(df[vars_dicotomicas], function(x) {
    ux <- sort(unique(na.omit(a_num(x))))
    length(ux) == 2 && all(ux %in% c(0, 1)) && sd(a_num(x), na.rm = TRUE) > 0
  })]

  vars_total <- c(vars_continuas, vars_dicotomicas)
  if (length(vars_total) < 2) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = "No hay suficientes variables para mixedCor."))
  }

  x <- df %>%
    select(all_of(vars_total)) %>%
    mutate(across(everything(), a_num)) %>%
    filter(if_all(everything(), ~ !is.na(.x))) %>%
    as.data.frame()

  if (nrow(x) < 50) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = "Muestra insuficiente para mixedCor."))
  }

  res <- tryCatch({
    psych::mixedCor(
      data = x,
      c = vars_continuas,
      d = vars_dicotomicas,
      p = NULL,
      correct = 0.5,
      smooth = TRUE
    )
  }, error = function(e) {
    attr(e, "mensaje") <- conditionMessage(e)
    e
  })

  if (inherits(res, "error")) {
    return(list(rho = matrix(numeric(0), nrow = 0, ncol = 0), status = paste("Error mixedCor:", attr(res, "mensaje"))))
  }

  list(rho = res$rho, status = "OK")
}

calcular_vif_aproximado <- function(df, vars) {
  vars_ok <- vars[vars %in% names(df)]
  vars_ok <- vars_ok[sapply(df[vars_ok], function(x) sd(a_num(x), na.rm = TRUE) > 0)]

  x <- df %>%
    select(all_of(vars_ok)) %>%
    mutate(across(everything(), a_num)) %>%
    filter(if_all(everything(), ~ !is.na(.x))) %>%
    as.data.frame()

  if (length(vars_ok) < 2 || nrow(x) < 50) {
    return(tibble(variable = vars_ok, VIF_aproximado = NA_real_, comentario = "No calculado"))
  }

  res <- lapply(vars_ok, function(v) {
    otros <- setdiff(vars_ok, v)
    form <- as.formula(paste(v, "~", paste(otros, collapse = " + ")))

    ajuste <- tryCatch(lm(form, data = x), error = function(e) NULL)
    if (is.null(ajuste)) {
      return(tibble(variable = v, R2_auxiliar = NA_real_, VIF_aproximado = NA_real_))
    }

    r2 <- tryCatch(summary(ajuste)$r.squared, error = function(e) NA_real_)
    vif <- ifelse(is.finite(r2) & r2 < 1, 1 / (1 - r2), Inf)

    tibble(variable = v, R2_auxiliar = r2, VIF_aproximado = vif)
  }) %>%
    bind_rows() %>%
    mutate(
      nivel_alerta = case_when(
        is.infinite(VIF_aproximado) ~ "alto",
        VIF_aproximado >= 10 ~ "alto",
        VIF_aproximado >= 5 ~ "moderado",
        TRUE ~ "bajo"
      ),
      comentario = case_when(
        nivel_alerta == "alto" ~ "Revisar posible colinealidad fuerte",
        nivel_alerta == "moderado" ~ "Revisar posible colinealidad moderada",
        TRUE ~ "Sin alerta relevante por VIF aproximado"
      )
    ) %>%
    arrange(desc(VIF_aproximado))

  res
}

graficar_heatmap <- function(mat, archivo, titulo, tipo = "correlacion") {
  if (is.null(mat) || length(mat) == 0) return(invisible(NULL))
  mat <- as.matrix(mat)
  if (nrow(mat) < 2 || ncol(mat) < 2) return(invisible(NULL))

  df <- as.data.frame(as.table(mat), stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    rename(variable_1 = Var1, variable_2 = Var2, valor = Freq)

  p <- ggplot(df, aes(x = variable_2, y = variable_1, fill = valor)) +
    geom_tile(color = "white", linewidth = 0.15) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(title = titulo, fill = ifelse(tipo == "cramer", "V", "r"))

  if (tipo == "cramer") {
    p <- p + scale_fill_gradient(limits = c(0, 1), low = "white", high = "steelblue", na.value = "grey90")
  } else {
    p <- p + scale_fill_gradient2(limits = c(-1, 1), low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, na.value = "grey90")
  }

  ggsave(filename = archivo, plot = p, width = 11, height = 9, dpi = 300)
}

# ------------------------------------------------------------
# 3. Lectura de base ya preparada del MNL V9
# ------------------------------------------------------------

ruta_base_larga <- buscar_archivo(archivo_base_larga)
ruta_data_nmm <- buscar_archivo(archivo_data_nmm)

if (!is.na(ruta_base_larga)) {
  ruta_usada <- ruta_base_larga
  tipo_base_usada <- "04_base_mnl_larga_v9_desde_enut_ii.csv"
} else if (!is.na(ruta_data_nmm)) {
  ruta_usada <- ruta_data_nmm
  tipo_base_usada <- "03_data_nmm_v9_desde_enut_ii.csv"
} else {
  stop(paste0(
    "No se encontro la base preparada del MNL V9.\n",
    "Deja en el directorio de trabajo o en prueba/v9 - mnl final/ alguno de estos archivos:\n",
    archivo_base_larga, "\n", archivo_data_nmm
  ))
}

base_v9 <- leer_csv_seguro(ruta_usada)

# ------------------------------------------------------------
# 4. Variables explicativas efectivamente presentes en el MNL V9
# ------------------------------------------------------------

info_vars_modelo <- tibble::tribble(
  ~variable,              ~etiqueta,                                      ~familia,              ~tipo_variable,
  "mod_TD",              "Modulo trabajo domestico no remunerado",       "modulo",              "dicotomica",
  "mod_TC",              "Modulo trabajo de cuidados no remunerado",     "modulo",              "dicotomica",
  "mod_ED",              "Modulo educacion",                            "modulo",              "dicotomica",
  "mod_CP",              "Modulo cuidados personales",                   "modulo",              "dicotomica",
  "T_mod_10h",           "Tiempo del modulo en decenas de horas",        "tiempo_modulo",       "continua",
  "female",              "Sexo femenino",                               "sexo",                "dicotomica",
  "zona_centro",         "Territorio centro o metropolitana",            "territorio",          "dicotomica",
  "zona_sur",            "Territorio sur",                               "territorio",          "dicotomica",
  "edad_18_24",          "Edad entre 18 y 24 anos",                      "edad",                "dicotomica",
  "edad_65mas",          "Edad 65 anos o mas",                           "edad",                "dicotomica",
  "n_trabajadores",      "Numero de trabajadores en el hogar",           "hogar",               "continua",
  "n_profesionales",     "Numero de profesionales en el hogar",          "hogar",               "continua",
  "vive_pareja",         "Vive con pareja",                              "hogar",               "dicotomica",
  "educ_secundaria",     "Educacion secundaria",                         "educacion",           "dicotomica",
  "educ_tecnica",        "Educacion tecnica",                            "educacion",           "dicotomica",
  "educ_universitaria",  "Educacion universitaria",                      "educacion",           "dicotomica",
  "quintil_3",           "Quintil 3",                                    "ingresos",            "dicotomica",
  "quintil_4",           "Quintil 4",                                    "ingresos",            "dicotomica",
  "quintil_5",           "Quintil 5",                                    "ingresos",            "dicotomica",
  "ing_personal",        "Ingreso personal",                             "ingresos",            "continua"
)

vars_modelo <- info_vars_modelo$variable
vars_presentes <- intersect(vars_modelo, names(base_v9))
vars_faltantes <- setdiff(vars_modelo, names(base_v9))

if (length(vars_presentes) < 2) {
  stop("La base encontrada no contiene suficientes variables explicativas del MNL V9 para calcular correlaciones.")
}

base_modelo <- base_v9 %>%
  mutate(across(any_of(vars_presentes), a_num))

info_vars_usadas <- info_vars_modelo %>%
  filter(variable %in% vars_presentes)

# ------------------------------------------------------------
# 5. Resumen base y variables analizadas
# ------------------------------------------------------------

id_persona_var <- case_when(
  "PeID" %in% names(base_v9) ~ "PeID",
  "id_persona" %in% names(base_v9) ~ "id_persona",
  TRUE ~ NA_character_
)

resumen_base <- tibble(
  indicador = c(
    "Archivo base usado",
    "Ruta base usada",
    "N filas base analizada",
    "N personas aproximado",
    "Variables explicativas esperadas en MNL V9",
    "Variables explicativas encontradas",
    "Variables explicativas faltantes",
    "Nota metodologica"
  ),
  valor = c(
    tipo_base_usada,
    ruta_usada,
    as.character(nrow(base_v9)),
    ifelse(is.na(id_persona_var), NA_character_, as.character(dplyr::n_distinct(base_v9[[id_persona_var]]))),
    as.character(length(vars_modelo)),
    as.character(length(vars_presentes)),
    ifelse(length(vars_faltantes) == 0, "Ninguna", paste(vars_faltantes, collapse = "; ")),
    "Diagnostico de correlaciones. No se reestima el MNL V9."
  )
)

resumen_variables <- info_vars_modelo %>%
  mutate(
    presente_en_base = variable %in% names(base_v9),
    n_no_na = ifelse(presente_en_base, sapply(variable, function(v) sum(!is.na(base_modelo[[v]]))), NA_integer_),
    media = ifelse(presente_en_base, sapply(variable, function(v) mean(a_num(base_modelo[[v]]), na.rm = TRUE)), NA_real_),
    sd = ifelse(presente_en_base, sapply(variable, function(v) sd(a_num(base_modelo[[v]]), na.rm = TRUE)), NA_real_),
    min = ifelse(presente_en_base, sapply(variable, function(v) suppressWarnings(min(a_num(base_modelo[[v]]), na.rm = TRUE))), NA_real_),
    max = ifelse(presente_en_base, sapply(variable, function(v) suppressWarnings(max(a_num(base_modelo[[v]]), na.rm = TRUE))), NA_real_)
  )

readr::write_csv(resumen_base, file.path(carpeta_salida, "00_resumen_base_diagnostico.csv"))
readr::write_csv(resumen_variables, file.path(carpeta_salida, "00_variables_modelo_v9_analizadas.csv"))

# ------------------------------------------------------------
# 6. Matrices de correlacion sobre variables del modelo
# ------------------------------------------------------------

vars_dicotomicas <- info_vars_usadas %>% filter(tipo_variable == "dicotomica") %>% pull(variable)
vars_continuas <- info_vars_usadas %>% filter(tipo_variable == "continua") %>% pull(variable)

mat_pearson <- calcular_cor_segura(base_modelo, vars_presentes, metodo = "pearson")
mat_spearman <- calcular_cor_segura(base_modelo, vars_presentes, metodo = "spearman")
res_tetra <- calcular_tetracorica_segura(base_modelo, vars_dicotomicas)
res_mixed <- calcular_mixedCor_segura(base_modelo, vars_continuas, vars_dicotomicas)

readr::write_csv(matriz_a_df(mat_pearson), file.path(carpeta_salida, "01_matriz_pearson_variables_modelo.csv"))
readr::write_csv(matriz_a_df(mat_spearman), file.path(carpeta_salida, "02_matriz_spearman_variables_modelo.csv"))
readr::write_csv(matriz_a_df(res_tetra$rho), file.path(carpeta_salida, "03_matriz_tetracorica_dicotomicas.csv"))
readr::write_csv(matriz_a_df(res_mixed$rho), file.path(carpeta_salida, "06_matriz_mixta_mixedCor.csv"))

# ------------------------------------------------------------
# 7. Variables categoricas reconstruidas para policorica y Cramer
# ------------------------------------------------------------

base_cat_larga <- reconstruir_categoricas(base_modelo)

base_cat_persona <- if (!is.na(id_persona_var) || "PeID" %in% names(base_cat_larga)) {
  base_cat_larga %>%
    arrange(PeID) %>%
    distinct(PeID, .keep_all = TRUE)
} else {
  base_cat_larga
}

vars_ordinales_persona <- c("edad_modelo", "educacion_modelo", "quintil_modelo", "territorio_modelo")
vars_categoricas_persona <- c("sexo_modelo", "territorio_modelo", "edad_modelo", "educacion_modelo", "quintil_modelo", "pareja_modelo")
vars_categoricas_larga <- c("modulo_modelo", vars_categoricas_persona)

res_poly_persona <- calcular_polychoric_segura(base_cat_persona, vars_ordinales_persona)
mat_cramer_persona <- calcular_matriz_cramer(base_cat_persona, vars_categoricas_persona)
mat_cramer_larga <- calcular_matriz_cramer(base_cat_larga, vars_categoricas_larga)

readr::write_csv(matriz_a_df(res_poly_persona$rho), file.path(carpeta_salida, "04_matriz_policorica_ordinales_persona.csv"))
readr::write_csv(matriz_a_df(mat_cramer_persona), file.path(carpeta_salida, "05_cramers_v_categoricas_persona.csv"))
readr::write_csv(matriz_a_df(mat_cramer_larga), file.path(carpeta_salida, "05b_cramers_v_categoricas_base_larga.csv"))

info_vars_categoricas <- tibble::tribble(
  ~variable,             ~familia,       ~tipo_variable,
  "modulo_modelo",      "modulo",       "categorica_nominal",
  "sexo_modelo",        "sexo",         "categorica_nominal",
  "territorio_modelo",  "territorio",   "categorica_ordinal_impuesta",
  "edad_modelo",        "edad",         "categorica_ordinal",
  "educacion_modelo",   "educacion",    "categorica_ordinal",
  "quintil_modelo",     "ingresos",     "categorica_ordinal",
  "pareja_modelo",      "hogar",        "categorica_nominal"
)

info_vars_extendida <- bind_rows(
  info_vars_usadas %>% select(variable, familia, tipo_variable),
  info_vars_categoricas
) %>%
  distinct(variable, .keep_all = TRUE)

# ------------------------------------------------------------
# 8. Pares de correlacion moderada y alta
# ------------------------------------------------------------

pares_pearson <- matriz_a_pares(mat_pearson, "Pearson")
pares_spearman <- matriz_a_pares(mat_spearman, "Spearman")
pares_tetra <- matriz_a_pares(res_tetra$rho, "Tetracorica")
pares_mixed <- matriz_a_pares(res_mixed$rho, "MixedCor")
pares_poly <- matriz_a_pares(res_poly_persona$rho, "Policorica ordinales persona")
pares_cramer_persona <- matriz_a_pares(mat_cramer_persona, "Cramer V persona", valor_abs = FALSE)
pares_cramer_larga <- matriz_a_pares(mat_cramer_larga, "Cramer V base larga", valor_abs = FALSE)

pares_consolidados <- bind_rows(
  pares_pearson,
  pares_spearman,
  pares_tetra,
  pares_mixed,
  pares_poly,
  pares_cramer_persona,
  pares_cramer_larga
) %>%
  agregar_familias(info_vars_extendida) %>%
  arrange(desc(correlacion_abs))

pares_moderados_altos <- pares_consolidados %>%
  filter(correlacion_abs >= UMBRAL_MODERADO)

pares_altos <- pares_consolidados %>%
  filter(correlacion_abs >= UMBRAL_ALTO)

pares_altos_interfamilia <- pares_altos %>%
  filter(misma_familia == "NO")

readr::write_csv(pares_consolidados, file.path(carpeta_salida, "07_pares_correlacion_consolidado_todos.csv"))
readr::write_csv(pares_moderados_altos, file.path(carpeta_salida, "08_pares_correlacion_moderada_alta.csv"))
readr::write_csv(pares_altos, file.path(carpeta_salida, "09_pares_correlacion_alta.csv"))
readr::write_csv(pares_altos_interfamilia, file.path(carpeta_salida, "10_pares_correlacion_alta_interfamilia.csv"))

# ------------------------------------------------------------
# 9. VIF aproximado sobre matriz de diseno del MNL V9
# ------------------------------------------------------------

vif_aprox <- calcular_vif_aproximado(base_modelo, vars_presentes) %>%
  left_join(info_vars_usadas %>% select(variable, familia, tipo_variable), by = "variable")

readr::write_csv(vif_aprox, file.path(carpeta_salida, "11_diagnostico_colinealidad_vif_aproximado.csv"))

# ------------------------------------------------------------
# 10. Graficos de calor
# ------------------------------------------------------------

graficar_heatmap(mat_pearson, file.path(carpeta_graficos, "01_heatmap_pearson.png"), "Matriz Pearson variables MNL V9")
graficar_heatmap(mat_spearman, file.path(carpeta_graficos, "02_heatmap_spearman.png"), "Matriz Spearman variables MNL V9")
graficar_heatmap(res_tetra$rho, file.path(carpeta_graficos, "03_heatmap_tetracorica.png"), "Matriz tetracorica variables dicotomicas")
graficar_heatmap(res_mixed$rho, file.path(carpeta_graficos, "06_heatmap_mixedCor.png"), "Matriz mixta mixedCor")
graficar_heatmap(res_poly_persona$rho, file.path(carpeta_graficos, "04_heatmap_policorica_persona.png"), "Matriz policorica variables ordinales")
graficar_heatmap(mat_cramer_persona, file.path(carpeta_graficos, "05_heatmap_cramer_persona.png"), "V de Cramer variables categoricas persona", tipo = "cramer")
graficar_heatmap(mat_cramer_larga, file.path(carpeta_graficos, "05b_heatmap_cramer_base_larga.png"), "V de Cramer variables categoricas base larga", tipo = "cramer")

# ------------------------------------------------------------
# 11. Estado, nota metodologica y Excel consolidado
# ------------------------------------------------------------

estado_calculos <- tibble(
  calculo = c(
    "Pearson",
    "Spearman",
    "Tetracorica",
    "Policorica ordinales persona",
    "Cramer V persona",
    "Cramer V base larga",
    "MixedCor",
    "VIF aproximado"
  ),
  estado = c(
    ifelse(nrow(as.data.frame(mat_pearson)) > 0, "OK", "No calculado"),
    ifelse(nrow(as.data.frame(mat_spearman)) > 0, "OK", "No calculado"),
    res_tetra$status,
    res_poly_persona$status,
    ifelse(nrow(as.data.frame(mat_cramer_persona)) > 0, "OK", "No calculado"),
    ifelse(nrow(as.data.frame(mat_cramer_larga)) > 0, "OK", "No calculado"),
    res_mixed$status,
    ifelse(nrow(vif_aprox) > 0, "OK", "No calculado")
  )
)

nota_metodologica <- c(
  "V9 - Diagnostico de correlaciones para MNL final",
  "",
  "Este script no reestima el MNL V9. Lee la base larga ya preparada del modelo final y calcula diagnosticos de asociacion entre variables explicativas.",
  "",
  "Matrices generadas:",
  "1. Pearson: diagnostico general para variables numericas y dicotomicas.",
  "2. Spearman: diagnostico robusto basado en rangos.",
  "3. Tetracorica: variables dicotomicas del modelo.",
  "4. Policorica: variables ordinales reconstruidas a nivel persona. Para territorio se impone el orden norte, centro, sur solo como diagnostico complementario.",
  "5. V de Cramer: variables categoricas nominales, util especialmente para territorio y modulo.",
  "6. MixedCor: matriz mixta que combina continuas y dicotomicas segun el tipo de variable.",
  "7. VIF aproximado: regresion auxiliar de cada variable explicativa contra las demas, como diagnostico de colinealidad de la matriz de diseno.",
  "",
  "Criterios de alerta:",
  paste0("|correlacion| >= ", UMBRAL_ALTO, ": alta"),
  paste0(UMBRAL_MODERADO, " <= |correlacion| < ", UMBRAL_ALTO, ": moderada"),
  "",
  "La eliminacion de variables no debe ser automatica. Correlaciones dentro de una misma familia, por ejemplo educacion, territorio o quintiles, pueden ser esperables por construccion de las dummies."
)

writeLines(nota_metodologica, con = file.path(carpeta_salida, "99_nota_metodologica.txt"))
readr::write_csv(estado_calculos, file.path(carpeta_salida, "12_estado_calculos.csv"))

# Para Excel se reducen algunas hojas a pares relevantes para evitar archivos innecesariamente pesados.
writexl::write_xlsx(
  list(
    resumen_base = resumen_base,
    variables_analizadas = resumen_variables,
    estado_calculos = estado_calculos,
    pearson = matriz_a_df(mat_pearson),
    spearman = matriz_a_df(mat_spearman),
    tetracorica = matriz_a_df(res_tetra$rho),
    policorica = matriz_a_df(res_poly_persona$rho),
    cramer_persona = matriz_a_df(mat_cramer_persona),
    mixedCor = matriz_a_df(res_mixed$rho),
    pares_moderados_altos = pares_moderados_altos,
    pares_altos_interfamilia = pares_altos_interfamilia,
    vif_aproximado = vif_aprox
  ),
  path = file.path(carpeta_salida, "v9_diagnostico_correlaciones.xlsx")
)

cat("\n====================================================\n")
cat("DIAGNOSTICO DE CORRELACIONES MNL V9 TERMINADO\n")
cat("====================================================\n")
cat("No se reestimo el modelo.\n")
cat("Base usada:\n", ruta_usada, "\n\n")
cat("Carpeta de salida:\n", normalizePath(carpeta_salida, winslash = "/"), "\n\n")
cat("Variables explicativas encontradas:\n")
print(vars_presentes)
cat("\nVariables faltantes:\n")
print(vars_faltantes)
cat("\nEstado de calculos:\n")
print(estado_calculos)
cat("\nPares con correlacion alta entre familias distintas:\n")
print(pares_altos_interfamilia)
cat("====================================================\n")
