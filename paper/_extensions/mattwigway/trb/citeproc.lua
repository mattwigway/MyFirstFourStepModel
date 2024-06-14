-- force running citeproc before word counting, see https://github.com/quarto-dev/quarto-cli/issues/2294
return {
    Pandoc = function(doc)
        return pandoc.utils.citeproc(doc)
    end 
}