.doc_parse <- function(all_r_files) {
  lst <- lapply(all_r_files,
         function(file){
           file_lines <- readLines(file)
           roxy_blocks <- roxygen2::parse_file(file, env = NULL)
           roxy_block_list <- lapply(roxy_blocks, function(roxy_block){
             # length(deparse(...) - 1 is to keep call intact if it's > 1 line
             # but 1 line only is the typical case (e.g. NULL, '_PACKAGE')
             end_line <- roxy_block$line + length(deparse(roxy_block$call)) - 1L
             roxy_lines <- vapply(roxy_block$tag,
                                  function(tag) as.integer(tag$line), 1L)
             roxy_line_seq <- seq(from = min(roxy_lines), to = end_line)
             out_lines <- file_lines[roxy_line_seq]
             # 2 line breaks between roxygen2 sections in final file write
             c(out_lines, rep("", 2))
           })
           names(roxy_block_list) <- vapply(roxy_blocks,
                                            roxygen2::block_get_tag_value,
                                            "", tag = 'name')
           roxy_block_list
         }
  )
  Reduce(c, lst)
}
