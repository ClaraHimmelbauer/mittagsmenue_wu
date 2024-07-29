# rm(list = ls()); gc()

# packages
packages <- c("readxl", "tidyverse", "flextable")
sapply(packages, library, character.only = T)

# source functions
# files.sources = list.files(path="R", full.names = T)
# sapply(files.sources, source, encoding = "utf-8")

# data --------------------------------------------------------------------------------------------

data <- readxl::read_xlsx("data/menues.xlsx")

# get metadata: when was file last updated --------------------------------------------------------

# mtime = modified
# ctime = created
# atime = accessed
last_modified <- unlist(file.info("data/menues.xlsx")$mtime)

# make nicely looking table -----------------------------------------------------------------------

# mutate df
df <- data %>%
  group_by(Restaurant, Wochentag) %>%
  mutate(id = row_number()) %>%
  ungroup() %>% 
  select(Restaurant, Wochentag, Gericht, id) %>% 
  pivot_wider(names_from = Wochentag, values_from = Gericht) %>% 
  select(Restaurant, Täglich, Montag, Dienstag, Mittwoch, Donnerstag, Freitag)

# welche zellen sind vegetarisch/vegan
veggie <- data %>%
  group_by(Restaurant, Wochentag) %>%
  mutate(id = row_number()) %>%
  ungroup() %>% 
  select(Restaurant, Wochentag, `Vegetarisch/Vegan`, id) %>% 
  pivot_wider(names_from = Wochentag, values_from = `Vegetarisch/Vegan`) %>% 
  select(Restaurant, Täglich, Montag, Dienstag, Mittwoch, Donnerstag, Freitag)

vegetarisch <- which(veggie == "vegetarisch")
vegan <- which(veggie == "vegan")

vegetarisch_i <- vegetarisch %% nrow(veggie)
vegetarisch_j <- ceiling(vegetarisch / nrow(veggie))
vegan_i <- vegan %% nrow(veggie)
vegan_j <- ceiling(vegan / nrow(veggie))

# make flextable
flex <- flextable::flextable(df) %>% 
  flextable::theme_box()

# color vegetarian and vegan cells
for(k in 1:length(vegetarisch_i)){
  flex <- flex %>% 
    color(i = vegetarisch_i[k], j = vegetarisch_j[k], color = "#72bf6a")
}
for(k in 1:length(vegan_i)){
  flex <- flex %>% 
    color(i = vegan_i[k], j = vegan_j[k], color = "#46923c")
}

# merge
flex <- flex %>% 
  merge_v(j = ~Restaurant + Täglich + Montag + Dienstag+ Mittwoch + Donnerstag + Freitag)

# table design
flex <- flex %>% 
  bold(j = 1) %>% 
  bg(j = 1, bg = "grey") %>% 
  bg(i = 1, part = "header", bg = "grey50") %>% 
  border(border = officer::fp_border(color = "grey"), part = "body") %>% 
  border(border = officer::fp_border(color = "grey50"), part = "header")


# # try to estimate borders
# x <- as.data.frame(table(df$Restaurant)[rank(unique(df$Restaurant))])
# names(x) <- c("Restaurant", "Freq")
# y <- df %>%
#   group_by(Restaurant) %>%
#   summarize(Täglich = n_distinct(Täglich),
#             Montag = n_distinct(Montag),
#             Dienstag = n_distinct(Dienstag),
#             Mittwoch = n_distinct(Mittwoch),
#             Donnerstag = n_distinct(Donnerstag),
#             Freitag = n_distinct(Freitag))
# dfy <- left_join(x, y)
# 
# for(k in 2:7){
#   name <- names(df)[k]
#   x <- as.numeric(unlist(dfy[name]))
#   x <- cumsum(x)
#   x[length(x)] <- x[length(x)] + 1
#   flex <- flex %>% 
#     border(i = x, j = k, border.bottom = officer::fp_border(color = "black"), part = "body")
# }
# 
