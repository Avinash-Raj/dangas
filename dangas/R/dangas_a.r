#' Helps to extract a substring present within two delimiters.
#'
#' @param start point from which the matching starts
#' @param end   point where matching ends
#' @param string input string
#' @param delim  Turn \code{delim = TRUE} to include also the start and end delimiters in the final output.
#' @return substring present in-between the start and end delimiters. 
#' @description 
#' \code{extract_a} returns the substring present in-between the start and end delimiters. 
#' Note that it would return only the first occurance.
#' @examples
#' extract_a("<?", "?>", "foo <?php info?> bar")
#' extract_a("<?", "?>", "foo <?php info?> bar", delim = TRUE)

extract_a <- function(start,end,string,delim) {
  if(!missing(delim)) {
    if(delim == TRUE) {
          x <- regmatches(string, 
                  regexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",start,perl=T),"[\\s\\S]*?",gsub("([^\\s\\w])", "\\\\\\1",end,perl=T)), 
                                  string, perl=TRUE))
      
    }else {
      x <- regmatches(string, 
                  regexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",start,perl=T),"\\K[\\s\\S]*?(?=",gsub("([^\\s\\w])", "\\\\\\1",end,perl=T), ")"), 
                                  string, perl=TRUE))
    }
    }else {
    x <- regmatches(string, 
                  regexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",start,perl=T),"\\K[\\s\\S]*?(?=",gsub("([^\\s\\w])", "\\\\\\1",end,perl=T), ")"), 
                                  string, perl=TRUE))
}
return(x)
}

#' Helps to extract all the substrings present within two delimiters.
#'
#' @param start point from which the matching starts.
#' @param end   point where matching ends.
#' @param string input string.
#' @param delim  Turn \code{delim = TRUE} to include also the start and end delimiters in the final output.
#' @return substring present in-between the from and to delimiters. Note that it would return all the occurances. Final output would be in the form of list.
#' @description 
#' \code{extract_all_a} returns all the substrings present in-between the start and end delimiters. 
#' Note that it would return all the occurances.
#' @examples
#' extract_all_a("<?", "?>", "foo <?php info?> bar <?r info?>")
#' extract_all_a("<?", "?>", "foo <?php info?> bar <?r info?>", delim = TRUE)

extract_all_a <- function(start,end,string,delim) {
  if(!missing(delim)) {
    if(delim == TRUE) {
          x <- regmatches(string, 
                  gregexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",start,perl=T),"[\\s\\S]*?",gsub("([^\\s\\w])", "\\\\\\1",end,perl=T)), 
                                  string, perl=TRUE))
      
    }else {
      x <- regmatches(string, 
                  gregexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",start,perl=T),"\\K[\\s\\S]*?(?=",gsub("([^\\s\\w])", "\\\\\\1",end,perl=T), ")"), 
                                  string, perl=TRUE))
    }
    }else {
    x <- regmatches(string, 
                  gregexpr(
                    paste0(gsub("([^\\s\\w])", "\\\\\\1",start,perl=T),"\\K[\\s\\S]*?(?=",gsub("([^\\s\\w])", "\\\\\\1",end,perl=T), ")"), 
                                  string, perl=TRUE))
}
return(x)
}

