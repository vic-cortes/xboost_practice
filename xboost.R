library(here)
library(dplyr)


# Descarga de archivos
url <- "https://raw.githubusercontent.com/jboscomendoza/rpubs/xgboost_r/xgboost_en_r/agaricus-lepiota.data"
destination <- here("data", "agaricus-lepiota.data")
download.file(url, destfile = destination, mode = "wb")

# Nombres de las variables
url_names <- "https://raw.githubusercontent.com/jboscomendoza/rpubs/xgboost_r/xgboost_en_r/agaricus-lepiota.names"
destination_names <- here("data", "agaricus-lepiota.names")
download.file(url_names, destfile = destination_names, mode = "wb")


file <- here("data", "agaricus-lepiota.data")

# Inspeccionar encabezado del archivo
file %>%
    readr::read_lines() %>%
    head()

original <-
    readr::read_csv(file, col_names = FALSE) %>% tbl_df()

# Agregar los nombres
nombres <-
    c(
        "target", "cap_shape", "cap_surface", "cap_color", "bruises", "odor",
        "gill_attachment", "gill_spacing", "gill_size", "gill_color",
        "stalk_shape","stalk_root", "stalk_surface_above_ring",
        "stalk_surface_below_ring", "stalk_color_above_ring",
        "stalk_color_below_ring", "veil_type",
        "veil_color", "ring_number", "ring_type", "spore_print_color",
        "population", "habitat"
    )

names(original) <- nombres

#? Conversión a factores, es necesario por el paquete xboost

original <-
    original %>%
        mutate(
            across(everything(), function(column) {
                column %>%
                    as.factor() %>%
                    as.numeric() %>%
                    { . - 1 }
            })
        )

#? Creación de una lista

hongo <- list()
hongo$original <- original

#? Sets de entrenmaineto
set.seed(1919)

samples <- ceiling(nrow(original) * 0.7)

hongo$test_df <-
    original %>%
        slice_sample(n = samples)

#* Protip: Hacer uso de la función "within"
#* cuando se trabaja con las listas.

hongo <- within(hongo, {
        # Encuentra la diff entre dfs
        train_df <-
            original %>%
            setdiff(test_df)
    })

#? Convertir a DMatrix para el modelo

.make_xboost_matrix <- function(data_frame) {
    #* Construye la matríz para xboost
    .labels <- data_frame %>% pull(target)

    data_frame %>%
        select(-target) %>%
        as.matrix() %>%
        xgboost::xgb.DMatrix(
            data = .,
            label = .labels
        )

}

hongo <-
    within(hongo, {
        train_mat <- .make_xboost_matrix(train_df)
        test_mat <- .make_xboost_matrix(test_df)

        # Generar el modelo
        modelo_01 <- xgboost::xgboost(
                data = train_mat,
                objective = "binary:logistic",
                nrounds = 10,
                max.depth = 2,
                eta = 0.3,
                nthread = 2
            )

        # Testeo del modelo
        predict_01 <- modelo_01 %>% predict(test_mat)

    })

#? Entrenamiento del modelo Predictivo

hongo <- within(hongo, {
    #* Obtener el modelo y guardarlo en la lista

    modelo_01 <-
        xgboost::xgboost(
            data = train_mat,
            objective = "binary:logistic",
            nrounds = 10,
            max.depth = 2,
            eta = 0.3,
            nthread = 2
        )
})


#? Predicciones

hongo <-
    within(hongo, {
        #* Obtener predicciones del modelo

        predict_01 <-
            modelo_01 %>% predict(test_mat)
    })


#? Matriz de confusión

with(hongo, {
    #* Matriz de confusión

    .predict <- predict_01 > 0.5
    .target <- test_df %>% pull(target)

    bind_cols(predict = .predict, target = .target) %>%
        mutate(predict = ifelse(predict, 1, 0)) %>%
        table() %>%
        caret::confusionMatrix()
})