#'
#' Get the Pharmpy requirements. 
#' 
#' @return a character vector
#' @export
#' 
getPharmpyRequirements <- function() {
retValue <- "alabaster==0.7.16
altair==5.2.0
appdirs==1.4.4
attrs==23.2.0
Babel==2.14.0
beautifulsoup4==4.12.3
certifi==2024.2.2
charset-normalizer==3.3.2
click==8.1.7
cloudpickle==3.0.0
colorama==0.4.6
csscompressor==0.9.5
dask==2024.2.0
distributed==2024.2.0
docutils==0.20.1
fsspec==2024.2.0
idna==3.6
imagesize==1.4.1
importlib-metadata==7.0.1
Jinja2==3.1.3
jsonschema==4.21.1
jsonschema-specifications==2023.12.1
lark-parser==0.12.0
locket==1.0.0
lxml==5.1.0
MarkupSafe==2.1.5
mpmath==1.3.0
msgpack==1.0.7
networkx==3.2.1
numexpr==2.9.0
numpy==1.26.4
packaging==23.2
pandas==2.2.0
partd==1.4.1
psutil==5.9.8
Pygments==2.17.2
python-dateutil==2.8.2
pytz==2024.1
PyYAML==6.0.1
referencing==0.33.0
requests==2.31.0
rpds-py==0.18.0
scipy==1.12.0
six==1.16.0
snowballstemmer==2.2.0
sortedcontainers==2.4.0
soupsieve==2.5
Sphinx==7.2.6
sphinxcontrib-applehelp==1.0.8
sphinxcontrib-devhelp==1.0.6
sphinxcontrib-htmlhelp==2.0.5
sphinxcontrib-jsmath==1.0.1
sphinxcontrib-qthelp==1.0.7
sphinxcontrib-serializinghtml==1.1.10
symengine==0.11.0
sympy==1.12
tblib==3.0.0
toolz==0.12.1
tornado==6.4
typing_extensions==4.9.0
tzdata==2024.1
urllib3==2.2.0
zict==3.0.0
zipp==3.17.0
pharmpy-core @ git+https://github.com/Calvagone/pharmpy@5ef5633ba837cf2f0cede1c90b608064057392ec"
return(strsplit(retValue, split="\n")[[1]])
}
