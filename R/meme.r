# imgflip
# api docs: https://api.imgflip.com/
# signup: https://imgflip.com/signup

# get_memes
# https://api.imgflip.com/get_memes


get_templates <- function(site = "imgflip", type = NULL, query = NULL, ...){
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
        } else if(type == 'name') {
            u <- paste0(base,'Generators_Select_Related_ByDisplayName?displayName=', curlEscape(query))
        } else if(type == 'related') {
            u <- paste0(base,'Generators_Select_Related_ByDisplayName?displayName=', curlEscape(query))
        } else if(type == 'search') {
            u <- paste0(base,'Generators_Select_Related_ByDisplayName?pageIndex=0&pageSize=24&q=', curlEscape(query))
        else {
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

create_meme <-
function(template, upper, lower, username, password, site = NULL, 
         font = NULL, language = NULL, ...){
    if(class(template) != 'meme_template' & class(template == 'character')) {
        warning("'template' is not of class 'meme_template'. Template assumted to be an image URL and meme will be generated with site 'memecaptain'.")
        site <- 'memecaptain'
    }
    if(site == "imgflip") {
        base <- "https://api.imgflip.com/caption_image?"
        u <- paste(base, 'template_id=', template,
                   '&username=', username, '&password=', password,
                   '&text0=',upper, '&text1=', lower,
                   if(!is.null(font)) paste('&font=', font))
    } else if(site == "memegenerator") {
        base <- 'https://version1.api.memegenerator.net/Instance_Create?'
        u <- paste(base, 'generatorID=', template,
                   '&username=', username, '&password=', password,
                   '&text0=',upper, '&text1=', lower,
                   if(!is.null(language)) paste('&languageCode=', language))
    } else if(site == "memecaptain") {
        u <- paste0("http://v1.memecaptain.com/i?",'u=',template,'&t1=',upper,'&t2=',lower)
    } else {
        stop("Only 'imgflip', 'memegenerator', and 'memecaptain' are currently supported!")
    }
    
    if(site %in% c('imgflip', 'memegenerator'){
        b <- dynCurlReader()
        h <- basicHeaderGatherer()
        curlPerform(url = u, writefunction = b$update, headerfunction = h$update,
                    ssl.verifypeer = 1L, ssl.verifyhost = 2L,
                    cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ...)
        if(h$value()["status"] != "200")
            warning("HTTP Status", h$value()["status"], ": ", h$value()["statusMessage"])
        
        if(site == "imgflip") {
            out <- fromJSON(b$value())
            if(out$success)
                return(structure(c(as.list(out$data), meme = getBinaryURL(out$data$url)), 
                                 class = "meme", site = site))
            else
                stop(out$error_message)
        } else if(site == 'memegenerator'){
            out <- fromJSON(b$value())
            if(out$success)
                return(structure(out, class = "meme", site = site))
            else
                return(NULL)
        }
    } else if(site == "memecaptain") {
        out <- getBinaryURL(u)
        return(structure(list(url = u, meme = out), class = "meme", site = site))
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

plot.meme <- function(x, ...){
    if('meme' %in% names(x)) {
        j <- readJPEG(x$meme)
    } else {
        if(attr(x,'site') == 'imgflip')
            j <- readJPEG(getBinaryURL(x$url))
        else if(attr(x,'site') == 'memegenerator')
            j <- readJPEG(x$imageUrl)
        else if(attr(x,'site') == 'memecaptain')
            stop("If site=='memecaptain', x must supply 'meme' field")
    }
    par(mar=rep(0,4), mgp=rep(0,3))
    plot(NULL, xlim=c(0,1), ylim=c(0,1), xaxs='i', yaxs='i')
    rasterImage(j, 0,0,1,1)
    invisible(x)
}

get_memes <- function(site = 'imgflip'){
    if(site == "imgflip") {
        stop("'imgflip' not currently supported")
    } else if(site == "memegenerator") {
        
    }
}
