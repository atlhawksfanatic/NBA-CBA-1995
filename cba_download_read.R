# Download the CBA from Github website

# ---- start --------------------------------------------------------------

# Parse the CBA to a raw text file:
library(textreadr)
library(tidyverse)
library(tikzDevice)

# Create a directory for the data
local_dir    <- "raw"
data_source <- paste0(local_dir, "/articles")
man_dir     <- paste0(local_dir, "/manual")
if (!file.exists(local_dir)) dir.create(local_dir, recursive = T)
if (!file.exists(data_source)) dir.create(data_source)
if (!file.exists(man_dir)) dir.create(man_dir)


# ---- download -----------------------------------------------------------

cba_url <- paste0("https://github.com/atlhawksfanatic/",
                  "atlhawksfanatic.github.io/raw/master/research/CBA/",
                  "1995-NBA-NBPA-Collective-Bargaining-Agreement.pdf")

cba_file <- paste(local_dir,
                  "1995-NBA-NBPA-Collective-Bargaining-Agreement.pdf",
                  sep = "/")
if (!file.exists(cba_file)) download.file(cba_url, cba_file)

# ---- parse --------------------------------------------------------------

cba <- read_pdf(cba_file, trim = T)

# Remove the table of contents and the index
cba <- cba[13:282,]


# Make the txt_top include two lines so they can include the article??

cba_texts <- map2(cba$text, seq(cba$text), function(x, y) {
  print(y)
  txt_blank<-str_locate(x,
                        "^[^[[:digit:]]|[[:upper:]]]*([[:digit:]]|[[:upper:]])")
  # Remove everything before a page number
  txt <- str_replace(x, "^[^[[:digit:]]|[[:upper:]]]*([[:digit:]]|[[:upper:]])",
                     "\\1")
  # Remove if it starts with 1 space number
  txt <- str_replace(txt, "^1\\s([[:digit:]])", "\\1")
  
  # Were any of the pages blank?
  if (all(is.na(txt_blank))) txt <- ""
  # Any page have fewer than 3 line breaks?
  if (str_count(txt, "\n") < 3) txt <- ""
  
  
  txt     <- str_remove(x, "^.*\n")
  txt_top <- str_extract(x, "^.*\n.*\n") %>%
    str_remove("\n")
  txt_art <- str_extract(x, "^.*\n.*\n") %>%
    str_remove("\n") %>% 
    str_remove_all("[:digit:]") %>% 
    str_trim()
  
  # Punctuation that is in a different font
  txt <- str_replace_all(txt, "“|”", '"')
  txt <- str_replace_all(txt, "’", "'")
  txt <- gsub("$", "\\$", txt, fixed = TRUE)
  txt <- gsub("%", "\\%", txt, fixed = TRUE)

  # If linebreak doesn't have blank before and after then replace with space
  # txt <- str_replace_all(txt, "([^\\h]+)(\n)([^\\h]+)", "\\1 \\3")

  # Page number at top: odd is last word, even is first except for some exhibits
  if (y %% 2 == 1) {
    txt_page <- word(txt_top, -1)
  } else {
    txt_page <- word(txt_top, 1)
  }

  # Exhibit page numbers are funky
  if (grepl("Exhibit", txt_top, ignore.case = T)) {
    txt_pg1 <- word(str_trim(str_remove(txt_top, "Exhibit")), 1)
    txt_pg2 <- word(str_trim(str_remove(txt_top, "Exhibit")), -1)

    if (nchar(txt_pg1) > nchar(txt_pg2)) {
      txt_page <- txt_pg1
      txt_top  <- paste0("Exhibit ", str_sub(txt_pg2, 1, 1))
    } else {
      txt_page <- txt_pg2
      txt_top  <- paste0("Exhibit ", str_sub(txt_pg1, 1, 1))
    }
  }
  
  # If the pages are blank, then just make them blank, if it's an article then
  if (is.na(txt_page)) {
    article <- NA
  } else if (!grepl("(ART)|(EXH)", txt_art)) {
    article <- NA
  } else if (grepl("EXH", txt_art)) {
    article <- "EXHIBIT"
  } else {
    # Hack for double forwardslash
    if (txt_page == "\\") txt_page <- "\\\\"
    
    article <- str_trim(str_remove(txt_art, txt_page))
    
  }
  
  # data.frame(txt, article, txt_page)
  return(data.frame(txt, y, article))
}) %>% 
  bind_rows() %>% 
  # replace(article == "", NA_character_) %>% 
  fill(article)

# As simple text files
cba_texts %>% 
  group_by(article) %>% 
  summarise(text = paste0(str_trim(txt), collapse = "")) %>% 
  as.list() %>% 
  pmap(function(article, text) {
    temp_num  <- str_pad(as.numeric(as.roman(word(article, 2))), 2, pad = "0")
    
    temp_file <- paste0(data_source, "/", article, ".txt")
    temp_rmd  <- paste0(man_dir, "/", temp_num, "-", article, ".Rmd")
    
    cat(text, file = temp_file)
    cat(text, file = temp_rmd)
  })


