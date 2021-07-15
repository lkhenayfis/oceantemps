##################################### FUNCOES PARA PLOT DO DADO ####################################

#' Plot da temperatura na superficie
#' 
#' Funcao facilitadora para visualizacao do dado de temperatura na superficie 
#' 
#' O parametro \code{wrap} permite plotar diferentes centralizacoes do mapa mundi. Supondo que 
#' observamos o globo de cima, a primeira posivao do vetor \code{wrap} indica o grau inicial do mapa
#' e, a segunda, o final. O padrao de \code{wrap = c(0, 360)} corresponde ao plot centrado no oceano
#' pacifico, enquanto \code{wrap = c(-180, 180)} corresponde a centralizacao no atlantico. Veja os
#' exemplos.
#' 
#' @param dado o dado do qual extrair serie. Deve ter formato igual ao retornado por 
#'     \code{\link{leFROMdir}} ou \code{\link{leFROMurl}}
#' @param data escalar string no formato AAAAMM indicando o ano-mes que deve ser plotado
#' @param warp \emph{EXPERIMENTAL} vetor indicando os limites do mapa. Ver Detalhes
#' @param outdir opcional, indicando diretorio para escrita dos graficos. Ver Exemplos
#' 
#' @examples 
#' 
#' # plot do ano-mes 202001
#' plots <- plotamapa(datexemplo, "202001")
#' 
#' # janela 202001:202003
#' plots <- plotamapa(datexemplo, "202001:202003")
#' 
#' # plot centralizado no atlantico
#' plotamapa(datexemplo, "202001", wrap = c(-180, 180))
#' 
#' \dontrun{
#' # escrita do arquivo em um diretorio qualquer
#' plotamapa(datexemplo, "202001", "C:/Users/usuario_qualquer/Imagens/plot.png")
#' }
#' 
#' @return Caso nao seja fornecido \code{outdir}, retorna objetos ggplot numa lista ou escalar caso
#'     apenas uma data tenha sido fornecida. Caso \code{outdir} seja fornecido, escreve os plots no
#'     diretorio com nomes "mapa_AAAA-MM.png"
#' 
#' @import ggplot2
#' 
#' @export

plotamapa <- function(dado, data, wrap = c(0, 360), outdir) {

    # distorcao pelo wrap fornecido
    if(!(abs(diff(wrap)) == 360)) stop("Wrap nao representa um arco de 360 graus")

    if(missing("data")) {
        data <- dado$DATE[1]
    } else {
        data <- dateparse(data)
    }

    dplot <- dado[DATE %in% data]
    mapa  <- map_data("world", wrap = wrap)

    lon0 <- dplot$LON
    dplot[lon0 < min(wrap), LON := LON + 360]
    dplot[lon0 > max(wrap), LON := LON - 360]
    setorder(dplot, LON)

    # O raster assume que cada par LON, LAT e um centro do retangulo a ser desenhado. Isso cria uma
    # ilusao de offset entre os mapas e raster, porque o primeiro quadrado em LON = 0 fica esticado
    # pra uma longitude negativa
    # Para corrigir isso temos que repetir o trecho de LON = 0 para LON = 360, pois o quadrante em
    # LON = 0 inclui trechos do outro lado do mapa (maps o plot nao faz essa cicliidade)
    if((min(dplot$LON) - 1) < min(mapa$long)) {
        aux <- dplot[LON == min(LON)]
        aux[, LON := max(dplot$LON) + 2]
        dplot <- rbind(dplot, aux)
    } else if((max(dplot$LON) + 1) > max(mapa$long)) {
        aux <- dplot[LON == max(LON)]
        aux[, LON := min(dplot$LON) - 2]
        dplot <- rbind(dplot, aux)
    }

    # Algo similar acontece pros limites de lat
    aux <- dplot[LAT == min(LAT)]
    aux <- aux[, LAT := LAT - 2]
    dplot <- rbind(dplot, aux)

    limsst <- dplot[, range(SST, na.rm = TRUE)]
    midsst <- dplot[, mean(SST, na.rm = TRUE)]

    gs <- lapply(data, function(di) {
        ggplot() + geom_raster(data = dplot[DATE == di], aes(LON, LAT, fill = SST)) +
            geom_polygon(data = mapa, aes(long, lat, group = group), fill = "gray50", color = "grey20") +
            scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", limits = limsst,
                midpoint = midsst, na.value = "grey50") +
            labs(title = paste0("Data: ", di), x = "", y = "") +
            theme_minimal() +
            coord_cartesian(xlim = range(mapa$long), ylim = c(min(mapa$lat), max(dplot$LAT)), expand = FALSE)
    })

    if(missing("outdir")) {
        for(g in gs) {dev.new(); print(g)}
        if(length(gs) == 1) invisible(gs[[1]]) else invisible(gs)
    } else {
        for(i in seq_along(data)) {
            di <- format(data[i], format = "%Y-%m")
            g <- gs[[i]]
            ggsave(file.path(outdir, paste0("mapa_", di, ".png")), g, width = 9, height = 6)
        }
    }
}
