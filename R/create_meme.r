create_meme <-
function(template, upper, lower, username, password, site = NULL, 
         font = NULL, language = NULL, ...){
    if(class(template) != 'meme_template' & class(template == 'character')) {
        warning("'template' is not of class 'meme_template'. Template assumted to be an image URL and meme will be generated with site 'memecaptain'.")
        site <- 'memecaptain'
    }
    if(site == "imgflip") {
        base <- "https://api.imgflip.com/caption_image?"
        u <- paste(base, 'template_id=', template$id,
                   '&username=', username, '&password=', password,
                   '&text0=',upper, '&text1=', lower,
                   if(!is.null(font)) paste('&font=', font))
    } else if(site == "memegenerator") {
        base <- 'https://version1.api.memegenerator.net/Instance_Create?'
        u <- paste(base, 'generatorID=', template$generatorID,
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
