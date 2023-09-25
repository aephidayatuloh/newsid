
# tempo.co terpopular
tempo_url <- sprintf("https://tempo.co/indeks/%s-%s-%s", param$y, param$m, param$d)

nlinkmax_tempo <- 100
tempo_link_headlines <- tempo_url |>  
  read_html() |> 
  html_elements("article.text-card") |> 
  html_elements("h2.title") |> 
  html_elements("a") |>  
  html_attr("href")

if(length(tempo_link_headlines) > nlinkmax_tempo){
  tempo_link_headlines <- tempo_link_headlines[1:nlinkmax_tempo]
} else {
  tempo_link_headlines <- tempo_link_headlines
}

tictoc::tic(msg = "Tempo")
tempo_headlines <- purrr::map(tempo_link_headlines, .f = function(.x){
  page <- read_html(.x)
  
  tibble(
    link = .x, 
    title = page |> 
      html_elements("article.detail-artikel") |> 
      html_elements("div.detail-title") |> 
      html_elements("h1.title") |> 
      html_text2(), 
    datetime = page |> 
      html_element("p.date") |> 
      html_text(), 
    content = page |> 
      html_elements("div#isi") |> 
      html_elements("div.detail-konten") |> 
      html_elements("p") |>
      html_text2() %>% 
      .[str_detect(., "Baca Juga", negate = TRUE)] %>% 
      .[str_detect(., "Pilihan editor:", negate = TRUE)] %>% 
      paste(collapse = " "), 
    penulis = page |> 
      html_element("div.text-avatar") |> 
      html_elements("p.title") |> 
      html_elements("a") |>
      html_elements("span") |> 
      html_text() |> 
      _[1],
    editor = page |> 
      html_elements("div.text-avatar") |> 
      html_elements("p.title") |> 
      html_elements("a") |>
      html_elements("span") |> 
      html_text() |> 
      _[2],
    portal = "Tempo"
  ) |> 
    mutate(
      datetime = datetime |> 
        str_remove_all(pattern = ".*, ") |> 
        str_remove_all(pattern = " WIB") |> 
        # str_remove_all(pattern = ",") |> 
        dmy_hm(tz = "Asia/Bangkok"), 
      content = content %>% 
        str_sub(., start = str_locate(., "TEMPO.CO, ")[2] + 1, end = -1L)
    )
}) |> 
  list_rbind()
tictoc::toc()
