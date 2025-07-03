# campsistrans 1.4.0

* Error when importing some rxode2 models #92
* Rework installation of Pharmpy #93
* Update method 'prepareNONMEMFiles' for Pharmpy v1.7.2 (removed) #94
* Update NONMEM import with Pharmpy v1.7.2 #95
* Fix R CMD Check #96
* Rework importDataset method to get rid of Pharmpy #97
* Implement method 'importPredictions' #98
* Rework method executeSimulationCtl to get rid of pharmpy #99
* Add option to exclude tests relying on Pharmpy engine #101
* Issue in conversion from rxode2 when no ODE (e.g. $PRED in NONMEM) #102
* Add GPL license #103
* Scale factors to uppercase when importing NONMEM via nonmem2rx #104
* TIME and T both converted to t with nonmem2rx #105

# campsistrans 1.3.0

* Translator for rxode2 #51
* Translator for Monolix #52
* Test monolix importer based on the monolix tutorial #67
* Translate initial conditions from rxode2 to Campsis #68
* Parse complex if statement stuctures from rxode2 model code #69
* Monolix importer bug: duplicate compartment properties when same model is imported twice #70
* Monolix importer: automatically convert time into t #71
* Rxode2 importer: parse error model #72
* Monolix importer: import variance-covariance matrix #74
* NONMEM importer using nonmem2rx: implementation #75
* Support several equations in an if/else if/else statement #76
* NONMEM importer using monolix2rx: testing #77
* NONMEM importer using nonmem2rx: identify subroutines #78
* NONMEM importer using nonmem2rx: many dots in variable names #79
* NONMEM importer using nonmem2rx: detect subroutine #80
* Monolix importer: reduce number of decimals? #82
* Rxode2 import: get rid of dots in parameter names #83
* Monolix importer: add non regression tests #84
* Rxode2 importer: convert power symbol to pow #85
* NONMEM importer using nonmem2rx: import SIGMA matrix #86
* NONMEM importer using nonmem2rx: import cov file #87
* Rxode2 importer: import variance-covariance from thetaMat #88
* Return rxode model as well in addition to the Campsis model #90

# campsistrans 1.2.3

* Do not skip tests anymore #63
* Add NEWS file #64

# campsistrans 1.2.2

* Remove call to virtualenv_install #61

# campsistrans 1.2.1

* Patch version to Campsistrans v1.2.0.

# campsistrans 1.2.1

* Pharmpy must be installed with specific package versions #58

# campsistrans 1.1.1

* Name covariance values after import on demand #53
* Convert covariance values to correlation values after import on demand #54
* Issue with NONMEM importer due to $ABBREVIATED REPLACE #55

# campsistrans 1.1.0

* Error when variables in NONMEM are occuring multiple times #31
* Error when dataset is not found #32
* Expand test database #45
* ImportNONMEM: arguments 'copy_dir' and 'rem_rate' should be FALSE by default #47
* SAME omegas and import of cov file #49

# campsistrans 1.0.0
* NONMEM import: issue with SAME omegas #43
* Import crashes because error is returned by fixOmega #42
* Simulation time T not translated automatically to t #40
