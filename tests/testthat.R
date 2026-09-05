# -----------------------------------------------------------------------------
# AssumptionsLab
# A Jamovi module for statistical assumptions assessment and methodological
# decision support.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of AssumptionsLab.
#
# AssumptionsLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# AssumptionsLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with AssumptionsLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# testthat entry point.
# ES: Punto de entrada de testthat.
#
# Standard R CMD check / devtools::test() harness entry point. Discovers and
# runs every test-*.R file under tests/testthat/ against the installed
# AssumptionsLab package.
#
# ES: Punto de entrada estándar del arnés de R CMD check / devtools::test().
# Descubre y ejecuta cada archivo test-*.R bajo tests/testthat/ contra el
# paquete AssumptionsLab instalado.
# -----------------------------------------------------------------------------

library(testthat)
library(AssumptionsLab)

test_check("AssumptionsLab")
