
all_models <- list.files(path="data-raw")

all_models <- all_models[!(all_models %in% c("dataset.csv", "model_library.R"))]

model_library <- list()

for (model in all_models) {
  fileName <- file.path("data-raw", model)
  str <- readChar(fileName, file.info(fileName)$size)
  model_library[model] <- str
}
usethis::use_data(model_library, overwrite=TRUE)

