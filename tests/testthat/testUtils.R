
# setwd("C:/prj/campsistrans/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsistrans/tests/")
# testFolder <<- "C:/prj/campsistrans/tests/testthat/"


# # Uninstall pharmpy (needed to install a newer version)
# python -m pip uninstall pharmpy-core

# # Install pharmpy
# python -m pip install pharmpy-core

# Link specific version of Python with R at startup
# Add line in Rprofile.site: Sys.setenv("RETICULATE_PYTHON"="C:/PsN-5.0.0/python/python-3.7.7.amd64/")

# OR explicitely tell R to use a specific version of python
# reticulate::use_python("C:/PsN-5.0.0/python/python-3.7.7.amd64/python.exe", required=TRUE)
# 
# # Check Python version
# reticulate::py_config()
# 
# # Load Pharmy through reticulate
# pharmpy <- reticulate::import("pharmpy")
# 
# # Check Pharmpy version
# version <- pharmpy["__version__"]
