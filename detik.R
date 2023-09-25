
# Detik.com terpopular
detik_url <- sprintf("https://news.detik.com/indeks?date=%s/%s/%s", param$m, param$d, param$y)

npagemax_detik <- 3
detik_npage <- detik_url |>  
  read_html() |> 
  html_elements("a.pagination__item") |> 
  html_text() |> 
  as.numeric() |> 
  suppressWarnings() |>
  max(na.rm = TRUE) %>% 
  ifelse(. > npagemax_detik, npagemax_detik, .)


detik_link_headlines <- purrr::map(.x = 1:detik_npage, .f = function(.x){
  read_html(sprintf("https://news.detik.com/indeks/%s?date=%s/%s/%s", .x, param$m, param$d, param$y)) |> 
    html_elements("h3.media__title") |> 
    html_elements("a.media__link") |> 
    html_attr("href")
}) |> 
  list_c()

tictoc::tic(msg = "Detik")
detik_headlines <- purrr::map(.x = detik_link_headlines, .f = function(.x){
  page <- read_html(.x)
  
  tibble(
    link = .x, 
    title = page |> 
      html_elements("h1.detail__title") |> 
      html_text2(), 
    content = page |>
      html_elements("div.detail__body-text") |> 
      html_elements("p") |> 
      html_text2() |>  
      paste0(collapse = " "), 
    datetime = page |> 
      html_elements("div.detail__date") |> 
      html_text(), 
    penulis = page |> 
      html_elements("div.detail__author") |> 
      html_text() |> 
      str_extract(regex("^(.+?) -")) |> 
      str_sub(end = -3L),
    portal = "Detik"
  ) |> 
    mutate(
      datetime = datetime |> 
        str_remove_all(pattern = ".*, ") |> 
        str_remove_all(pattern = " WIB") |> 
        # str_remove_all(pattern = ",") |> 
        dmy_hm(tz = "Asia/Bangkok")
    )
  }) |> 
  list_rbind()
tictoc::toc()
