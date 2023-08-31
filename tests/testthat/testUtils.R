
# setwd("C:/prj/campsistrans/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsistrans/tests/")
# testFolder <<- "C:/prj/campsistrans/tests/testthat/"


# # Uninstall pharmpy (needed to install a newer version)
# python -m pip uninstall pharmpy-core

# # Install pharmpy
# python -m pip install pharmpy-core

# # Checkout Pharmpy tag
# git checkout v0.45.0

# # Install pharmpy from local git repository (always uninstall first)
# python setup.py install

# Link specific version of Python with R at startup
# Add line in Rprofile.site: Sys.setenv("RETICULATE_PYTHON"="C:/PsN-5.0.0/python/python-3.7.7.amd64/")

# OR explicitely tell R to use a specific version of python
# reticulate::use_python("C:/PsN-5.0.0/python/python-3.7.7.amd64/python.exe", required=TRUE)
# reticulate::use_python("C:/Users/nicolas.luyckx.CALVAGONE/AppData/Local/Programs/Python/Python39/python.exe", required=TRUE)
# or Tools/Global Options/Python/ -> select python distribution

# # Check Python version
# reticulate::py_config()
# 
# # Load Pharmy through reticulate
# pharmpy <- reticulate::import("pharmpy")
# 
# # Check Pharmpy version
# version <- pharmpy["__version__"]



advan <- 1
trans <- 1
mapping <- mapping(theta=c(K=1, V=2), omega=c(K=1, V=2), sigma=c(PROP=1))
template <- getNONMEMModelTemplate(advan, trans)
#object <- importNONMEM(file=template, mapping=mapping)

modelContent <- campsistrans::model_library[[paste0("advan", advan, "_trans", trans)]]

modelContent <- "$PROBLEM 1-compartment model\r\n$INPUT\r\n$DATA dataset.csv IGNORE=I\r\n$SUBROUTINE ADVAN1 TRANS1\r\n$PK\r\n K = THETA(1) * EXP(ETA(1))\r\n V = THETA(2) * EXP(ETA(2))\r\n S1 = V\r\n$ERROR \r\n CP = F\r\n OBS_CP = CP *(1+EPS(1))\r\n Y = OBS_CP\r\n$THETA 0.0625 ; THETA_K \r\n$THETA 80     ; THETA_V \r\n$OMEGA 0.025  ; OMEGA_K\r\n$OMEGA 0.025  ; OMEGA_V\r\n$SIGMA 0.025  ; PROP\r\n"

pharmpy <- reticulate::import("pharmpy")
model <- pharmpy$modeling$read_model_from_string(modelContent)


model <- pharmpy$modeling$read_model(path="C:/Calvagone/Clients/KUL/22KUL0203_HeartTransplant/ModelDevelopment/run006/run006.mod")


class(model$parameters)

parameters <- convertParameters(model, mapping=mapping, estimate=FALSE)

rv0 <- model$random_variables[[0]]
rv0$names
rv0$parameter_names

rv1 <- model$random_variables[[1]]
rv1$names
rv1$parameter_names

rv2 <- model$random_variables[[2]]
rv2$names
rv2$parameter_names

