# -----------------------------------------------------------------------------
# Run an analysis wrapper with jamovi's language set to `lang` and return the
# printed results as one string.
# ES: Ejecutar un análisis con el idioma de jamovi fijado en `lang` y devolver
# los resultados impresos como una sola cadena.
# -----------------------------------------------------------------------------
runAnalysisIn <- function(lang, fun, ...) {
    withr::local_envvar(c(LANGUAGE = lang))
    res <- fun(...)
    paste(utils::capture.output(print(res)), collapse = "\n")
}
