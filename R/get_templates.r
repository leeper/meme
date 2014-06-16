get_templates <- function(site = "memecaptain", type = NULL, query = NULL, ...){
    if(site == "imgflip") {
        u <- "https://api.imgflip.com/get_memes"
    } else if(site == "memegenerator") {
        base <- 'http://version1.api.memegenerator.net/'
        if(is.null(type))
            type <- 'popular'
        if(type == 'new') {
            u <- paste0(base,'Generators_Select_ByNew?pageIndex=0&pageSize=24')
        } else if(type == 'popular') {
            u <- paste0(base,'Generators_Select_ByPopular?pageIndex=0&pageSize=24&days=7')
        } else if(type == 'trending') {
            u <- paste0(base,'Generators_Select_ByTrending')
        } else if(type == 'related') {
            u <- paste0(base,'Generators_Select_Related_ByDisplayName?displayName=', curlEscape(query))
        } else if(type == 'search') {
            u <- paste0(base,'Generators_Select_Related_ByDisplayName?pageIndex=0&pageSize=24&q=', curlEscape(query))
        } else {
            stop("Specified 'type' not recognized")
        }
    } else if(site == "memecaptain") {
        u <- "http://v1.memecaptain.com/source_images.json"
    } else if(site == "imgur") {
        u <- "https://api.imgur.com/3/memegen/defaults"
        stop("'imgur' not currently supported")
    } else {
        stop("Only 'imgflip', 'memegenerator', and 'memecaptain' are currently supported!")
    }
    b <- basicTextGatherer()
    h <- basicHeaderGatherer()
    curlPerform(url = u, writefunction = b$update, headerfunction = h$update,
                ssl.verifypeer = 1L, ssl.verifyhost = 2L,
                cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ...)
    if(h$value()["status"] != "200")
        warning("HTTP Status", h$value()["status"], ": ", h$value()["statusMessage"])
    out <- fromJSON(b$value())
    if(site == "imgflip") {
        if(out$success)
            return(lapply(out$data$memes, structure, class = "meme_template", site = site))
        else
            stop(out$error_message)
    } else if(site == 'memegenerator'){
        if(out$success)
            return(lapply(out$result, structure, class = "meme_template", site = site))
        else
            return(NULL)
    } else if(site == "memecaptain") {
        return(lapply(out$images, structure, class = "meme_template", site = site))
    } else {
        return(NULL)
    }
}

plot.meme_template <- function(x, ...){
    if(attr(x,'site') %in% c('imgflip','memecaptain'))
        u <- x$url
    else if(attr(x,'site') == 'memegenerator')
        u <- x$imageUrl
    j <- readJPEG(u)
    rasterImage(j, ...)
    invisible(x)
}
