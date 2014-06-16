get_memes <- function(site = "memegenerator", type = NULL, query = NULL, ...){
    if(site == "imgflip") {
        stop("'imgflip' not currently supported")
    } else if(site == "memegenerator") {
        base <- 'http://version1.api.memegenerator.net/'
        if(is.null(type))
            type <- 'popular'
        if(type == 'new') {
            u <- paste0(base,'Instances_Select_ByNew?pageIndex=0&pageSize=24')
        } else if(type == 'popular') {
            u <- paste0(base,'Instances_Select_ByPopular?pageIndex=0&pageSize=24&days=7')
        } else if(type == 'trending') {
            u <- paste0(base,'Instances_Select_ByTopics')
        } else if(type == 'topic') {
            u <- paste0(base,'Instances_Select_ByTopic')
        } else {
            stop("Specified 'type' not recognized")
        }
    } else if(site == "memecaptain") {
        stop("'memecaptain' not currently supported")
    } else if(site == "imgur") {
        u <- "https://api.imgur.com/3/memegen/defaults"
        stop("'imgur' not currently supported")
    } else {
        stop("Only 'memegenerator' is currently supported!")
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
        return(NULL)
    } else if(site == 'memegenerator'){
        if(out$success)
            return(lapply(out$result, structure, class = "meme", site = site))
        else
            return(NULL)
    } else if(site == "memecaptain") {
        return(NULL)
    } else {
        return(NULL)
    }
}
