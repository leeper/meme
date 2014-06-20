create_meme <-
function(template, upper = NULL, lower = NULL, username = NULL, password = NULL, site = NULL, 
         font = NULL, language = NULL, ...){
    if(class(template) != 'meme_template' & class(template) == 'character') {
        message("'template' is not of class 'meme_template'. Template assumed to be an image URL and meme will be generated with site 'memecaptain'.")
        template <- list(url = template)
        site <- 'memecaptain'
    } else {
        site <- attr(template, 'site')
    }
    if(site == "imgflip") {
        base <- "https://api.imgflip.com/caption_image?"
        u <- paste0(base, 'template_id=', template$id,
                    '&username=', username, '&password=', password,
                    '&text0=',curlEscape(upper), '&text1=', curlEscape(lower),
                    if(!is.null(font)) paste('&font=', font))
    } else if(site == "memegenerator") {
        base <- 'https://version1.api.memegenerator.net/Instance_Create?'
        u <- paste0(base, 'generatorID=', template$generatorID,
                    '&username=', username, '&password=', password,
                    '&text0=',curlEscape(upper), '&text1=', curlEscape(lower),
                    if(!is.null(language)) paste('&languageCode=', language))
    } else if(site == "memecaptain") {
        u <- paste0("http://v1.memecaptain.com/i?",'u=',curlEscape(template$url),'&t1=',curlEscape(upper),'&t2=',curlEscape(lower))
    } else {
        stop("Only 'imgflip', 'memegenerator', and 'memecaptain' are currently supported!")
    }
    
    if(site %in% c('imgflip', 'memegenerator')){
        b <- dynCurlReader()
        h <- basicHeaderGatherer()
        curlPerform(url = u, writefunction = b$update, headerfunction = h$update,
                    ssl.verifypeer = 1L, ssl.verifyhost = 2L,
                    cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ...)
        if(h$value()["status"] != "200")
            warning("HTTP Status ", h$value()["status"], ": ", h$value()["statusMessage"])
        
        if(site == "imgflip") {
            out <- fromJSON(b$value())
            if(out$success)
                return(structure(c(as.list(out$data), meme = getBinaryURL(out$data["url"])), 
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
        h <- basicHeaderGatherer()
        out <- getBinaryURL(u, headerfunction = h$update)
        if(h$value()["status"] != "200")
            warning("HTTP Status ", h$value()["status"], ": ", h$value()["statusMessage"])
        return(structure(list(url = u, meme = out), class = "meme", site = site))
    } else {
        return(NULL)
    }
    
}

plot.meme <- function(x, ...){
    j <- .readimage(x)
    plot(NULL, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', 
         xlab = "", ylab = "", main = "",
         xaxs='i', yaxs='i', mar=rep(0,4), mgp=rep(0,3), ...)
    rasterImage(j, 0,0,1,1)
    invisible(x)
}
