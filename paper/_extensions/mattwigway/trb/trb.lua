--[[
    Counts words in a document

    Images and tables are ignored; words in text body do not include referece section

    This filter is an adapted mixture of
    https://github.com/pandoc/lua-filters/blob/master/wordcount/wordcount.lua
    and
    https://github.com/pandoc/lua-filters/blob/master/section-refs/section-refs.lua
]]

local body_words = 0
local ref_words = 0
local note_words = 0
local appendix_words = 0
local total_words = 0
local ntables = 0

function set_meta(meta)
    nwords = total_words

    if meta.nwords ~= nil then
        nwords = tonumber(pandoc.utils.stringify(meta.nwords))
    end

    if meta.ntables ~= nil then
        ntables = tonumber(pandoc.utils.stringify(meta.ntables))
    end

    meta.nwords = nwords
    if tables == 1 then
        meta.ntables = "1 table"
    else
        meta.ntables = tostring(ntables) .. " tables"
    end
    meta.nwords_with_tables = nwords + ntables * 250
    return meta
end

function set_author_meta(meta)
    -- Create the author header
    -- see https://github.com/quarto-journals/jss/blob/a5b928a765e0d87cc40aedcbf4302b8ce2ad5c9c/_extensions/jss/jss.lua#L49-L65
    if (meta.authorheader == nil) and (meta['by-author'] ~= nil) then
        local byAuthor = meta['by-author']

        if #byAuthor == 1 then
            meta.authorheader = pandoc.utils.stringify(byAuthor[1].name.family)
        elseif #byAuthor == 2 then
            meta.authorheader = pandoc.utils.stringify(byAuthor[1].name.family) .. " and " .. pandoc.utils.stringify(byAuthor[2].name.family)
        elseif #byAuthor == 3 then
            meta.authorheader = pandoc.utils.stringify(byAuthor[1].name.family) .. ", " .. pandoc.utils.stringify(byAuthor[2].name.family) .. ", and " .. pandoc.utils.stringify(byAuthor[3].name.family)
        else
            meta.authorheader = pandoc.utils.stringify(byAuthor[1].name.family) .. " et al."
        end
    end

    return meta
end

function count_words(blks)
    local count = 0
    pandoc.walk_block(pandoc.Div(blks), {
        Str = function(el)
            if is_word(el.text) then
                count = count + 1
            end
        end
    })
    return count
end

-- Ignore content in `no-count` divs
function is_no_count_div (blk)
    if (blk and blk.t=="Div" and blk.classes and #blk.classes > 0) then
        for _, class in pairs(blk.classes) do
            if class=="no-count" then
                return true
            end
        end
    end
    return false
end

function is_table (blk)
    return (blk.t == "Table")
end

function is_image (blk)
    return (blk.t == "Image" or blk.t == "Figure")
end

function is_word (text)
    return text:match("%P") and not (text:match("“") or text:match("”"))
end

function remove_all_tables_images (blks)
    return pandoc.walk_block(pandoc.Div(blks),
    {
        Table = function(el)
            return {}
        end,
        Image = function(el)
            return {}
        end,
        Figure = function(el)
            return {}
        end,
        Div = function(el)
            if is_no_count_div(el) then
                return {}
            end
            return el
        end
    }).content
end

function is_ref_div (blk)
    return (blk.t == "Div" and blk.identifier == "refs")
end

function get_all_notes (blks)
    local all_notes = {}

    pandoc.walk_block(pandoc.Div(blks),
    {
        Note = function(el)
            table.insert(all_notes, el)
        end
    })
    return all_notes
end

function remove_all_notes (blks)
    return pandoc.walk_block(
    pandoc.Div(blks),
    {
        Note = function(el)
            return {}
        end
    }
).content
end

function get_all_refs (blks)
    local out = {}
    for _, b in pairs(blks) do
        if is_ref_div(b) then
            table.insert(out, b)
        end
    end
    return out
end

function remove_all_refs (blks)
    local out = {}
    for _, b in pairs(blks) do
        if not (is_ref_div(b)) then
            table.insert(out, b)
        end
    end
    return out
end

-- Check if the block is an appendix div
function is_appendix_div (blk)
    return (blk.t == "Div" and blk.identifier == "appendix-count")
end

-- Get all appendix divs
function get_all_appendix (blks)
    local out = {}
    for _, b in pairs(blks) do
        if is_appendix_div(b) then
            table.insert(out, b)
        end
    end
    return out
end

-- Remove all appendix divs
function remove_all_appendix (blks)
    local out = {}
    for _, b in pairs(blks) do
        if not is_appendix_div(b) then
            table.insert(out, b)
        end
    end
    return out
end

-- Function for printing word counts to the terminal
function print_word_counts()
    
    local manuscript_words = body_words + note_words
    
    -- Format these different numbers         
    local total_words_out = string.format(
        "%d total %s",
        total_words, total_words == 1 and "word" or "words"
    )

    local manuscript_words_out = string.format(
        "%d %s in body and notes",
        manuscript_words, manuscript_words == 1 and "word" or "words"
    )

    local body_words_out = string.format(
        "%d %s in text body",
        body_words, body_words == 1 and "word" or "words"
    )

    local note_words_out = note_words > 0 and string.format(
        "%d %s in notes",
        note_words, note_words == 1 and "word" or "words"
    ) or ""

    local ref_words_out = ref_words > 0 and string.format(
        "%d %s in reference section",
        ref_words, ref_words == 1 and "word" or "words"
    ) or ""

    local appendix_words_out = appendix_words > 0 and string.format(
        "%d %s in appendix section",
        appendix_words, appendix_words == 1 and "word" or "words"
    ) or ""
    
    local longest_out = math.max(
        #total_words_out,
        #manuscript_words_out,
        #body_words_out,
        #note_words_out,
        #ref_words_out,
        #appendix_words_out
    )

    print("Overall totals:")
    print(string.rep("-", longest_out + 3))
    print("- " .. total_words_out)
    print("- " .. manuscript_words_out)

    print("\nSection totals:")
    print(string.rep("-", longest_out + 3))
    print("- " .. body_words_out)

    if note_words_out ~= "" then
        print("- " .. note_words_out)
    end

    if ref_words_out ~= "" then
        print("- " .. ref_words_out)
    end

    if appendix_words_out ~= "" then
        print("- " .. appendix_words_out)
    end

    print()
end

body_count = {
    Str = function(el)
        -- we don't count a word if it's entirely punctuation:
        if is_word(el.text) then
            body_words = body_words + 1
        end
    end,

    Code = function(el)
        _, n = el.text:gsub("%S+", "")
        body_words = body_words + n
    end
}

ref_count = {
    Str = function(el)
        -- we don't count a word if it's entirely punctuation:
        if is_word(el.text) then
            ref_words = ref_words + 1
        end
    end
}

-- Count words in the appendix
appendix_count = {
    Str = function(el)
        if is_word(el.text) then
            appendix_words = appendix_words + 1
        end
    end,
    
    Code = function(el)
        _, n = el.text:gsub("%S+", "")
        appendix_words = appendix_words + n
    end
}

note_count = {
    Str = function(el)
        if is_word(el.text) then
            note_words = note_words + 1
        end
    end,
    
    Code = function(el)
        _, n = el.text:gsub("%S+", "")
        note_words = note_words + n
    end
}


count_tables = {
    Table = function (el)
        ntables = ntables + 1
    end
}

-- Actual word counting
function Pandoc(el)
    if PANDOC_VERSION == nil then -- if pandoc_version < 2.1
        io.stderr:write("WARNING: pandoc >= 2.1 required for wordcount filter\n")
        return el
    end
    
    -- Count code blocks in body, notes, and appendix if needed
    if el.meta["count-code-blocks"] ~= nil then
        count_code_blocks = el.meta["count-code-blocks"]
    else
        count_code_blocks = true
    end
    
    -- Add these functions to the respective section counting functions
    if count_code_blocks then
        body_count.CodeBlock = function(el)
            _, n = el.text:gsub("%S+", "")
            body_words = body_words + n
        end
        
        appendix_count.CodeBlock = function(el)
            _, n = el.text:gsub("%S+", "")
            appendix_words = appendix_words + n
        end
        
        note_count.CodeBlock = function(el)
            _, n = el.text:gsub("%S+", "")
            note_words = note_words + n
        end
    end
        
    -- Get all notes
    local all_notes = get_all_notes(el.blocks)
    -- Count words in notes
    pandoc.walk_block(pandoc.Div(all_notes), note_count)
    
    -- Remove tables, images, and {.no-count} contents
    local untabled = remove_all_tables_images(el.blocks)
    -- Next, remove notes
    local unnote = remove_all_notes(untabled)
    
    refs_title = el.meta["reference-section-title"]
    local unreffed = remove_all_refs(unnote)
    
    -- Remove appendix divs from the blocks
    local unappended = remove_all_appendix(unreffed)
    
    -- Walk through the unappended blocks and count the words
    pandoc.walk_block(pandoc.Div(unappended), body_count)
    
    local refs = get_all_refs(unnote)
    pandoc.walk_block(pandoc.Div(refs), ref_count)
    
    -- Get all appendix divs
    local appendix = get_all_appendix(unreffed)
    -- Walk through the appendix divs and count the words
    pandoc.walk_block(pandoc.Div(appendix), appendix_count)
    
    -- count tables
    pandoc.walk_block(pandoc.Div(el.blocks), count_tables)

    -- Calculate total
    total_words = body_words + note_words + ref_words + appendix_words
    
    -- Show counts in terminal
    print_word_counts()
    
    -- Modify metadata for use in document
    el.meta = set_meta(el.meta)
    el.meta = set_author_meta(el.meta)

    return el
end