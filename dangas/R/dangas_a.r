# this would extract a single substring.

extract_a <- function(from,to,string,delim) {
  if(!missing(delim)) {
    if(delim == TRUE) {
          x <- regmatches(string, 
                  regexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",from,perl=T),"[\\s\\S]*?",gsub("([^\\s\\w])", "\\\\\\1",to,perl=T)), 
                                  string, perl=TRUE))
      
    }else {
      x <- regmatches(string, 
                  regexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",from,perl=T),"\\K[\\s\\S]*?(?=",gsub("([^\\s\\w])", "\\\\\\1",to,perl=T), ")"), 
                                  string, perl=TRUE))
    }
    }else {
    x <- regmatches(string, 
                  regexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",from,perl=T),"\\K[\\s\\S]*?(?=",gsub("([^\\s\\w])", "\\\\\\1",to,perl=T), ")"), 
                                  string, perl=TRUE))
}
return(x)
}

# this would extract all the matched substrings.

extract_all_a <- function(from,to,string,delim) {
  if(!missing(delim)) {
    if(delim == TRUE) {
          x <- regmatches(string, 
                  gregexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",from,perl=T),"[\\s\\S]*?",gsub("([^\\s\\w])", "\\\\\\1",to,perl=T)), 
                                  string, perl=TRUE))
      
    }else {
      x <- regmatches(string, 
                  gregexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",from,perl=T),"\\K[\\s\\S]*?(?=",gsub("([^\\s\\w])", "\\\\\\1",to,perl=T), ")"), 
                                  string, perl=TRUE))
    }
    }else {
    x <- regmatches(string, 
                  gregexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",from,perl=T),"\\K[\\s\\S]*?(?=",gsub("([^\\s\\w])", "\\\\\\1",to,perl=T), ")"), 
                                  string, perl=TRUE))
}
return(x)
}

