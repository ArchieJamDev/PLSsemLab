# Tests read English messages unless a test sets another language.
# ES: Las pruebas leen los mensajes en inglés salvo que una prueba fije otro idioma.
withr::local_envvar(c(LANGUAGE = "en"), .local_envir = testthat::teardown_env())
