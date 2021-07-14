############################# LEITURA E FORMATACAO DE DADOS DE ENTRADA #############################

#' Leitura dos dados de sst
#' 
#' Funcoes para leitura de dados netCDF ja baixados ou a partir da internet
#' 
#' @param dir diretorio onde se encontram os arquivos .nc no formato ersst.v5.AAAAMM.nc, onde 
#'     \emph{AAAA} indica o ano e \emph{MM} o mes referente as informacoes
#' @param data string no formato AAAAMM indicando ano-mes do dado a ser baixado. Pode ser um 
#'     vetor de um ou mais elementos, indicando as datas, ou uma string no formato "AAAAMM:AAAAMM"
#'     indicando uma faixa de datas
#' @param url url de onde baixar os dados. Se nao forncida, sera usado
#'     \url{https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/}
#' @param outdir opcional; diretorio onde salvar os arquivos baixados. Caso nao seja fornecido, os
#'     arquivos serao salvos em um diretorio temporario na raiz padrao do sistema operacional
#' 
#' @examples 
#' 
#' # LEITURA DE UM DIRETORIO LOCAL CONTENDO ARQUIVOS ---------------------
#' 
#' # usando o diretorio dos dados inclusos no pacote
#' diretorio <- system.file("extdata", package = "oceantemps")
#' 
#' # o pacote so inclui dois meses (2020-01 e 2020-02)
#' list.files(diretorio)
#' 
#' # realiza a leitura
#' leFROMdir(diretorio)
#' 
#' # LEITURA DA MESMA JANELA DE DADOS A PARTIR DA URL PADRAO -------------
#' 
#' leFROMurl(data = "202001:202002")
#' 
#' # leitura de datas nao sequenciais
#' leFROMurl(data = c("202001", "202005", "202011"))
#' 
#' @return data.table contendo informacoes formatadas para uso
#' \describe{
#'     \item{DATE}{data do registro}
#'     \item{LON}{longitude do registro}
#'     \item{LAT}{latitude do registro}
#'     \item{SST}{temperatura na superficie do oceano}
#' }
#' 
#' @seealso \code{\link{plotamapa}} para metodos de visualizacao deste dado
#' 
#' @name leitura
NULL


#' @rdname leitura
#' @export

leFROMdir <- function(dir) {

    arqs <- list.files(dir, full.names = TRUE, pattern = "\\.nc$")

    dats  <- lapply(arqs, ncdf4::nc_open)
    names <- sub(".*v5\\.", "", sub(".nc", "", arqs))

    oceantemp <- mapply(dats, names, SIMPLIFY = FALSE, FUN = function(d, n) {

        lat <- ncdf4::ncvar_get(d, "lat")
        lon <- ncdf4::ncvar_get(d, "lon")
        sst <- ncdf4::ncvar_get(d, "sst")

        date <- as.Date(paste0(n, "01"), format = "%Y%m%d")

        out <- data.table(DATE = date, LON = rep(lon, length(lat)), LAT = rep(lat, each = length(lon)), SST = c(sst))
    })
    lapply(dats, ncdf4::nc_close)

    oceantemp <- rbindlist(oceantemp)

    return(oceantemp)
}

#' @rdname leitura
#' @export

leFROMurl <- function(data, url, outdir) {

    if(missing("url")) url <- "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/netcdf/"
    if(missing("outdir")) outdir <- tempdir()

    data <- dateparse(data)
    data <- format(data, format = "%Y%m")

    arqs <- paste0("ersst.v5.", data, ".nc")
    arqsdown <- file.path(url, arqs)
    arqsdest <- file.path(outdir, arqs)

    mode <- ifelse(Sys.info()["sysname"] == "Windows", "wb", "w")

    for(i in seq(arqs)) download.file(arqsdown[i], arqsdest[i], mode = mode)

    leFROMdir(outdir)
}