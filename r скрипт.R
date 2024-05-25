library(tidyverse)
library(openalexR)

# get list of the journals ------------------------------------------------
read.csv("abcd_list_annotated.csv") |> 
  filter(linguistics == "linguistics",
         !(title %in% list.files("openalex2/"))) |> 
  mutate(title = str_replace(title, 
                             "CONCENTRIC: STUDIES IN LINGUISTICS",
                             "CONCENTRIC")) |> 
  mutate(title = str_replace(title, 
                             "QUADERNS DE FILOLOGIA: ESTUDIS LITERARIS",
                             "QUADERNS DE FILOLOGIA")) |> 
  mutate(title = str_replace(title, 
                             "ONOMA: JOURNAL OF THE INTERNATIONAL COUNCIL OF ONOMASTIC SCIENCES",
                             "JOURNAL OF THE INTERNATIONAL COUNCIL OF ONOMASTIC SCIENCE")) |> 
  mutate(title = str_replace(title, 
                             "¬≈—“Õ»  ÕŒ¬Œ—»¡»–— Œ√Œ √Œ—”ƒ¿–—“¬≈ÕÕŒ√Œ ”Õ»¬≈–—»“≈“¿. —≈–»ﬂ: À»Õ√¬»—“» ¿ » Ã≈∆ ”À‹“”–Õ¿ﬂ  ŒÃÃ”Õ» ¿÷»ﬂ",
                             "¬≈—“Õ»  ÕŒ¬Œ—»¡»–— Œ√Œ √Œ—”ƒ¿–—“¬≈ÕÕŒ√Œ ”Õ»¬≈–—»“≈“¿")) ->
  df

# retrieve data by issn ---------------------------------------------------

walk(seq_along(df$issn1), function(i){
  print(str_c(i, " (", round(i/nrow(df)*100), "%) ", df$title[i]))
  
  t <- oa_fetch(primary_location.source.issn = df$issn1[i], 
                entity = "works", 
                abstract = TRUE,
                verbose = TRUE)
  
files <- fs::dir_ls("openalex2")
  
  if(!is.null(t)){
    dir.create(str_c("openalex2/", df$title[i]))
    
    t |> 
      select(id, doi, author, display_name, publication_year, so, issn_l, 
             first_page, last_page, volume, issue, is_retracted, cited_by_count, 
             ab, concepts) |> 
      mutate(author = t$author |> map("au_display_name"),
             concepts = t$concepts |> map("display_name"),
             tags_level = t$concepts |> map("level"),
             tags_score = t$concepts |> map("score")) |> 
      rowwise() |> 
      mutate(author = str_c(author, collapse = "; "),
             concepts = str_c(concepts, collapse = "; "),
             tags_level = str_c(tags_level, collapse = "; "),
             tags_score = str_c(tags_score, collapse = "; "),
             retrieved = today(),
             source = "openalex") |> 
      rename(title = display_name,
             journal = so,
             abstract = ab) |> 
      write_csv(str_c("openalex2/", df$title[i], "/", df$title[i], ".csv"))
  }
})

# redownload missing journals using the second issn 

df |> 
  filter(!(title %in% list.files("openalex2/")),
         !is.na(issn2)) |> 
  select(-issn1) |> 
  rename(issn1 = issn2) ->
  df

walk(seq_along(df$issn1), function(i){
  print(str_c(i, " (", round(i/nrow(df)*100), "%) ", df$title[i]))
  
  t <- oa_fetch(primary_location.source.issn = df$issn1[i], 
                entity = "works", 
                abstract = TRUE,
                verbose = TRUE)
  
  if(!is.null(t)){
    dir.create(str_c("openalex2/", df$title[i]))
    
    t |> 
      select(id, doi, author, display_name, publication_year, so, issn_l, 
             first_page, last_page, volume, issue, is_retracted, cited_by_count, 
             ab, concepts) |> 
      mutate(author = t$author |> map("au_display_name"),
             concepts = t$concepts |> map("display_name"),
             tags_level = t$concepts |> map("level"),
             tags_score = t$concepts |> map("score")) |> 
      rowwise() |> 
      mutate(author = str_c(author, collapse = "; "),
             concepts = str_c(concepts, collapse = "; "),
             tags_level = str_c(tags_level, collapse = "; "),
             tags_score = str_c(tags_score, collapse = "; "),
             retrieved = today(),
             source = "openalex") |> 
      rename(title = display_name,
             journal = so,
             abstract = ab) |> 
      write_csv(str_c("openalex2/", df$title[i], "/", df$title[i], ".csv"))
  }
})

library(fs)

# «‡„ÛÁÍ‡ ‰‡Ú‡ÒÂÚ‡ df
#df <- read.csv("openalex2/to/df.csv")


#files <- dir_ls("openalex2")

# ŒÚ·Ó Ù‡ÈÎÓ‚, Ì‡Á‚‡ÌËÈ ÍÓÚÓ˚ı ÌÂÚ ‚ ‰‡Ú‡ÒÂÚÂ df
#new_files <- files[!basename(files) %in% df$filename]

# ≈ÒÎË ÂÒÚ¸ ÌÓ‚˚Â Ù‡ÈÎ˚, ‰Ó·‡‚ÎˇÂÏ Ëı ‚ ‰‡Ú‡ÒÂÚ df
#if (length(new_files) > 0) {
  #new_data <- data.frame(filename = basename(new_files))
  #df <- rbind(df, new_data)
  #write_csv(str_c("openalex2/", df$title[i], "/", df$title[i], ".csv"))
#}