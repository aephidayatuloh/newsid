message(sprintf("Newsid update : %s", Sys.time()))
library(tidyverse) |> suppressPackageStartupMessages()
library(tidytext) |> suppressPackageStartupMessages()
library(katadasaR) |> suppressPackageStartupMessages()
library(rvest) |> suppressPackageStartupMessages()
library(dbplyr) |> suppressPackageStartupMessages()
library(RPostgres) |> suppressPackageStartupMessages()

tictoc::tic(msg = "Process")
tgl <- Sys.Date() - 0

param <- list(d = format(tgl, "%d"),
              m = format(tgl, "%m"),
              y = format(tgl, "%Y"))

sw <- read.table("D:/aephidayatuloh/shiny/newsid/stopword_list_id_2.txt", header = FALSE, col.names = "word")
# sw <- data.frame(word = c("di", "ada", "ke", "dari", "buat", "ini", "dalam"))

source("D:/aephidayatuloh/shiny/newsid/kompas.R")
source("D:/aephidayatuloh/shiny/newsid/detik.R")
source("D:/aephidayatuloh/shiny/newsid/tempo.R")

headlines <- bind_rows(
  kompas_headlines, 
  detik_headlines, 
  tempo_headlines
)

tictoc::tic(msg = "tokenize")

proc_headlines <- headlines |>  
  mutate(content = str_remove_all(content, "-"),
         content = str_remove_all(content, "[:digit:]"),
         content = str_remove_all(content, "[:punct:]")) |> 
  unnest_tokens(output = "word", input = "content", 
                to_lower = TRUE, token = "words")
tictoc::toc()

tictoc::tic(msg = "antijoin")

proc_headlines <- proc_headlines |> 
  transmute(portal, 
            word = word |> 
           purrr::map_chr(katadasar)) |> 
  mutate(
    word = case_when(word == "milu" ~ "pemilu", 
                     TRUE ~ word)
  ) |> 
  anti_join(sw, by = "word")
tictoc::toc()

tictoc::tic(msg = "upload to DB")
dbcon <- dbConnect(Postgres(), 
                   host = "rosie.db.elephantsql.com", 
                   port = 5432, 
                   dbname = "puxgpqon", 
                   user = "puxgpqon", 
                   password = "Uzz11fMtsa6FF7HiVlgbsoEMQY7aCU1l")
dbcon |> 
  dbWriteTable("headlines", headlines, overwrite = TRUE)
dbcon |> 
  dbWriteTable("proc_headlines", proc_headlines, overwrite = TRUE)
dbListTables(dbcon)
dbDisconnect(dbcon)
tictoc::toc()

tictoc::toc()
message(sprintf("Newsid update Done : %s", Sys.time()))
