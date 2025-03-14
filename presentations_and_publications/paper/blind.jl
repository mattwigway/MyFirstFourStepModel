open("my_first_model.qmd") do inp
    open("my_first_model_blind.qmd", "w") do out
        text = read(inp, String)
        outtext = replace(
            text,
            r"Matt(hew)?" => "",
            r"Bhagat-Conway" => "",
            r"(?<=email:).*" => "",
            r"(?<=orcid:).*" => "",
            r"(?<=name:).*" => "",
            r"(?<=department:).*" => "",
            r"<?http[^\s]+mattwigway[^\s]+" => "link removed for review",
            r"<?http[^\s]+indicatrix[^\s]+" => "link removed for review",
            r"mattwigway/[\"]+" => "link removed for review"
            )

        write(out, outtext)
    end
end