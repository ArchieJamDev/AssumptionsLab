# -----------------------------------------------------------------------------
# Run the test suite in English by default, so assertions on report text do
# not depend on the machine's locale. Language tests switch it explicitly.
# ES: Ejecutar las pruebas en inglés por defecto, para que las aserciones
# sobre el texto del informe no dependan del locale de la máquina. Las
# pruebas de idioma lo cambian de forma explícita.
# -----------------------------------------------------------------------------
withr::local_envvar(c(LANGUAGE = "en"), .local_envir = testthat::teardown_env())
