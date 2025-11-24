# ==============================================================================
# Unit-Tests for the R package clustVarACC
# ==============================================================================
# Ce fichier lance automatiquement tous les tests unitaires du package
# lors de l'exécution de devtools::test() ou R CMD check
#
# Pour exécuter les tests :
# - Depuis R : devtools::test()
# - En ligne de commande : R CMD check
# ==============================================================================

library(testthat)
library(clustVarACC)

# Lancer tous les tests dans le répertoire testthat/
test_check("clustVarACC")
