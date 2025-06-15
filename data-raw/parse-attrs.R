
library(stringr)
library(glue)

doc <- readLines("./data-raw/attrs.txt")
doc <- doc[!grepl("^#", doc)]


doc <- gsub("\\{", "\\\\{", doc)
doc <- gsub("\\}", "\\\\}", doc)
doc <- gsub("<B>(.*?)</B>", "\\\\code{\\1}", doc)
doc <- gsub("<TT>(.*?)</TT>", "\\\\code{\\1}", doc)
doc <- gsub("<.*?>", "", doc)
doc <- gsub("%%", "", doc)


start <- which(grepl("^:", doc))
end   <- c(tail(start, -1) - 1, length(doc))

att <- lapply(seq_along(start), function(i) {
  
  header <- doc[start[i]]
  header <- sub(";.*?$", "", header)
  # print(header)
  
  bits <- str_split(header, ":")[[1]]
  bits <- bits[-1]
  
  res <- list(
    name      = bits[1],
    scope     = bits[2],
    type      = bits[3],
    default   = bits[4],
    min_value = bits[5]
  )
  
  res$txt <- doc[(start[i]+1):end[i]]
  
  
  if (res$name == 'label') {
    res$default = ""
    res$txt <- gsub("&#92;N", "", res$txt)
  }
  
  
  res
})


natt <- purrr::keep(att, \(x) grepl("G|C", x$scope))
natt <- purrr::discard(natt, \(x) startsWith(x$name, "_"))


rox <- lapply(natt, \(att) {
txt <- glue_data(r"(#' @param {name} Type: [{type}]. Default: [{default}].
#'
{paste("#'       ", txt, collapse='\n')}
#'
)", .x = att)
as.character(txt)
})

rox <- unlist(rox)
rox <- paste(rox, collapse = "\n")
clipr::write_clip(rox)

purrr::map_chr(natt, "name") |> 
  sort() |> 
  paste(collapse = ", ")
