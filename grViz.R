fls <- list.files(".", pattern = ".gv")
fnm <- fls[1]

# grViz(fnm)
for (fnm in fls) {
    grViz(diagram = fnm) %>%
        export_svg %>% charToRaw %>% 
        rsvg_pdf(gsub(".gv", ".pdf", fnm))
}
