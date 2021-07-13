##################################### FUNCOES PARA PLOT DO DADO ####################################

#' Plot da temperatura na superficie
#' 
#' Funcao facilitadora para visualizacao do dado de temperatura na superficie 
#' 
#' @param dado o dado do qual extrair serie. Deve ter formato igual ao retornado por 
#'     \code{\link{leFROMdir}} ou \code{\link{leFROMurl}}
#' @param data escalar string no formato AAAAMM indicando o ano-mes que deve ser plotado
#' @param outfile opcional, indicando um caminho de arquivo para escrita. Ver Exemplos
#' 
#' @examples 
#' 
#' # plot do ano-mes 202001
#' plotamapa(datexemplo, "202001")
#' 
#' \dontrun{
#' # escrita do arquivo em um diretorio qualquer
#' plotamapa(datexemplo, "202001", "C:/Users/usuario_qualquer/Imagens/plot.png")
#' }
#' 
#' @return objeto ggplot contendo grafico criado ou vazio no caso de \code{outfile} fornecido
#' 
#' @import ggplot2
#' 
#' @export

plotamapa <- function(dado, data, outfile) {

    if(missing("data")) {
        data <- dado$DATE[1]
    } else {
        data <- as.Date(paste0(data, "01"), format = "%Y%m%d")
    }

    if(length(data) > 1) {
        data <- data[1]
        warning("Foi fornecido um vetor no parametro 'data' -- apenas o primeiro elemento sera usado")
    }

    dplot <- dado[DATE == data]
    dplot[, DATE := factor(format(DATE, format = "%Y-%b"))]

    # O raster assume que cada par LON, LAT e um centro do retangulo a ser desenhado. Isso cria uma
    # ilusao de offset entre os mapas e raster, porque o primeiro quadrado em LON = 0 fica esticado
    # pra uma longitude negativa
    # Para corrigir isso temos que repetir o trecho de LON = 0 para LON = 360, pois o quadrante em
    # LON = 0 inclui trechos do outro lado do mapa (maps o plot nao faz essa cicliidade)
    aux <- dplot[LON == 0]
    aux[, LON := max(dplot$LON) + 2]
    dplot <- rbind(dplot, aux)

    # Algo similar acontece pros limites de lat
    aux <- dplot[LAT == min(LAT)]
    aux <- aux[, LAT := LAT - 2]
    dplot <- rbind(dplot, aux)

    mapa <- map_data("world", wrap = c(0, 360))

    g <- ggplot(dplot, aes(LON, LAT, fill = SST)) + geom_raster() +
        geom_polygon(data = mapa, aes(long, lat, group = group), fill = "gray50", color = "grey20") +
        scale_fill_gradient2(low = "blue", mid = "yellow", high = "red",
            midpoint = datexemplo[, mean(SST, na.rm = TRUE)], na.value = "grey50") +
        labs(title = paste0("Data: ", dplot$DATE[1]), x = "", y = "") +
        theme_minimal() +
        coord_cartesian(xlim = range(mapa$long), ylim = c(min(mapa$lat), max(dplot$LAT)), expand = FALSE)

    if(missing("outfile")) {
        return(g)
    } else {
        ggsave(outfile, g, width = 9, height = 6)
    }
}