.readimage <- function(x){
    if('meme' %in% names(x)) {
        out <- readJPEG(x$meme)
    } else {
        if(attr(x,'site') == 'imgflip')
            u <- x$url
        else if(attr(x,'site') == 'memegenerator')
            u <- x$imageUrl
        else if(attr(x,'site') == 'memecaptain')
            u <- x$url
        contents <- getBinaryURL(u)
        ext <- tolower(file_ext(u))
        readfun <- switch(ext, 'jpg'=readJPEG, 'jpeg'=readJPEG, 'png'=readPNG)
        out <- do.call(readfun, list(contents))
    }
    out
}
