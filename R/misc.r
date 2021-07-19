########################### FUNCOES MISC PARA AUXILIO DO PACOTE EM GERAL ###########################

# Funcao para tratar input de datas nas funcoes de leitura, extracao e plot

dateparse <- function(string) {

    janela <- grepl("[[:digit:]]{6}\\:[[:digit:]]{6}", string)[1]

    if(janela) {
        data1 <- sub("\\:.*", "", string)
        data2 <- sub(".*\\:", "", string)

        data1 <- as.Date(paste0(data1, "01"), format = "%Y%m%d")
        data2 <- as.Date(paste0(data2, "01"), format = "%Y%m%d")

        datas <- seq(data1, data2, by = "month")
    } else {
        datas <- as.Date(paste0(string, "01"), format = "%Y%m%d")
    }

    return(datas)
}

# Processador de wrap do dado de temperatura

aplicawrap <- function(dado, wrap) {

    dado <- copy(dado)
    lon0 <- dado$LON
    dado[lon0 < min(wrap), LON := LON + 360]
    dado[lon0 > max(wrap), LON := LON - 360]
    setorder(dado, LON)

    return(dado)
}