devtools::load_all()

datexemplo <- leFROMurl(c("202001", "202012"))

usethis::use_data("datexemplo" = datexemplo, overwrite = TRUE)
