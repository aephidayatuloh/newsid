
# Kompas.com terpopular
kompas_url <- sprintf("https://news.kompas.com/search/%s-%s-%s/", param$y, param$m, param$d)

npagemax_kompas <- 5
kompas_npage <- kompas_url |> 
  read_html() %>% 
  html_elements("div.paging__item")

if(length(kompas_npage) == 0){
  kompas_npage <- 1 
} else {
  kompas_npage <- kompas_npage |> 
    html_elements("a.paging__link--next") |>
    html_attr("data-ci-pagination-page") |> 
    as.numeric() %>% 
    ifelse(. > npagemax_kompas, npagemax_kompas, .)
}

kompas_link_headlines <- purrr::map(1:kompas_npage, function(.x){
  paste0(kompas_url, .x) |> 
    read_html() |> 
    html_elements("div.article__list__title") |> 
    # html_elements("div.most__list") |>
    # html_elements("a.most__link") |> 
    html_elements("h3.article__title") |>
    html_elements("a") |> 
    html_attr("href")
  # html_text()
}) |> 
  unlist() 

tictoc::tic(msg = "Kompas")
kompas_headlines <- purrr::map(kompas_link_headlines, function(.x){
  # .x <- kompas_link_headlines[1]
  page <- read_html(.x)
  tibble(
    link = .x, 
    title = page |> 
      html_elements("h1.read__title") |> 
      # html_elements("div.most__list") %>% 
      # html_elements("a.most__link") %>% 
      # html_elements("h3.article__title") %>%
      # html_elements("a") %>% 
      # html_attr("href")
      html_text2(), 
    datetime = page |> 
      html_elements("div.read__time") |> 
      html_text(),
    content = page |> 
      html_elements("div.read__content") |> 
      # html_elements("div.most__list") %>% 
      # html_elements("a.most__link") %>% 
      # html_elements("h3.article__title") %>%
      html_elements("p") |>
      # html_attr("href")
      html_text2() %>% 
      .[str_detect(., "Baca juga", negate = TRUE)] %>% 
      paste(collapse = " "), 
    penulis = page |>  
      html_elements("div.credit-title-name") |> 
      html_elements("h6") |> 
      html_text() |> 
      _[1] |> 
      str_remove(","), 
    editor = page |>  
      html_elements("div.credit-title-name") |> 
      html_elements("h6") |> 
      html_text() |> 
      _[2], 
    portal = "Kompas"
  ) |> 
    mutate(
      datetime = datetime |> 
        str_remove_all(pattern = "Kompas.com - ") |> 
        str_remove_all(pattern = " WIB") |> 
        str_remove_all(pattern = ",") |> 
        dmy_hm(tz = "Asia/Bangkok"), 
      content = content %>% 
        str_sub(., start = str_locate(., ".com ")[2] + 3, end = -1L)
    )
}) |> 
  list_rbind()
tictoc::toc()

