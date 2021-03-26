
library(tidyverse)
library(maps)
library(lubridate)
library(glue)


# Download and read data --------------------------------------------------


if(FALSE){
  date_today = today() %>% format('%Y-%m-%d')
  #https://www.data.gouv.fr/fr/datasets/chiffres-cles-concernant-lepidemie-de-covid19-en-france/
  download.file("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv", 
                destfile=glue("data/chiffres-cles_{date_today}.csv"))
  #https://www.ign.fr/reperes/centre-geographique-des-departements-metropolitains
  download.file("https://www.ign.fr/publications-de-l-ign/centre-departements/Centre_departement.xlsx", 
                destfile=glue("data/Centre_departement_{date_today}.xlsx"), mode="wb")
  #https://www.insee.fr/fr/statistiques/1893198
  download.file("https://www.insee.fr/fr/statistiques/fichier/1893198/estim-pop-dep-sexe-gca-1975-2021.xls", 
                destfile=glue("data/estim-pop-dep-sexe-gca-1975-2021_{date_today}.xls"), mode="wb")
}


data_departements_centre = rio::import("data/Centre_departement_2021-03-26.xlsx", skip=2,
                                  col_names=c("num", "nom", "aire", "long", "lat", "ville")) %>% 
  as_tibble() %>% 
  mutate(long = str_replace(long, "O", "W")) %>%
  transmute(
    num,
    nom=tolower(nom) %>% str_replace_all("'", "") %>% 
      str_replace_all(" ", "-") %>% iconv(from="UTF-8", to="ASCII//TRANSLIT"), 
    centre_long=sp::char2dms(long, "°") %>% as.numeric(), 
    centre_lat=sp::char2dms(lat, "°") %>% as.numeric()
  )

data_departements_pop2021 = rio::import("data/estim-pop-dep-sexe-gca-1975-2021_2021-03-26.xls", 
                                   sheet="2021", skip=4) %>% 
  as_tibble() %>% 
  select(num=...1, pop_total=Total...8)



data_covid0 = rio::import("data/chiffres-cles_2021-03-26.csv", encoding="UTF-8", na.strings=c("NA", "")) %>%
  as_tibble()
# data_covid0 %>% filter(granularite=="departement") %>% View()
data_covid = data_covid0 %>%
  filter(granularite=="departement", 
         source_nom=="Santé publique France Data") %>%
  mutate(
    date=ymd(date),
    departement=str_extract(maille_code, "\\d+[AB]?")
  ) %>% 
  filter(!departement %in% c("971", "972", "973", "974", "976")) %>%
  left_join(data_departements_centre, by=c("departement"="num")) %>% 
  left_join(data_departements_pop2021, by=c("departement"="num")) %>% 
  select(date, departement, nom, deces, pop_total, hospitalises, reanimation, gueris, 
         nouvelles_hospitalisations, nouvelles_reanimations, #reste=trop de manquants
         centre_long, centre_lat, source_nom) %>%
  identity()


data_covid2 = data_covid %>% 
  # mutate_at(c("deces", "hospitalises", "reanimation", "gueris"), na_if, 0) %>% 
  arrange(departement, date) %>% 
  group_by(departement) %>% 
  complete(nesting(date, departement)) %>% 
  mutate(across(c("hospitalises", "reanimation", "deces", "gueris"), 
                zoo::rollmean, k=7, na.pad=TRUE, align="right", 
                .names="roll7_{col}")) %>% 
  filter(date >= ymd("2020-04-01")) %>%
  identity()


data_covid0 %>% dim #[1] 48254    20
data_covid %>% dim  #[1] 35808    13
data_covid2 %>% dim #[1] 34464    14



# Maps --------------------------------------------------------------------


print_p=FALSE
#'get_plot(data_covid2, unique(data_covid2$date)[3], "roll7_hospitalises")
#'get_plot(data_covid2, unique(data_covid2$date)[179], "roll7_hospitalises")
get_plot = function(data, date_extraction, variable="deces"){
  data_covid_date = data %>% 
    filter(date==date_extraction) %>% 
    transmute(nom, 
              centre_long, centre_lat,
              variable = !!sym(variable), 
              variable_100k = round(!!sym(variable)/pop_total*100000))
  if(data_covid_date$nom %>% anyDuplicated()>0)
    warning("Duplicate in data_covid_date: date=", date_extraction, ", variable=", variable)
  
  data_carte = map_data("france") %>% 
    as_tibble() %>% 
    mutate_if(is.character, tolower) %>% 
    mutate(region = str_replace_all(region, " ", "-")) %>% 
    left_join(data_covid_date, 
              by=c("region"="nom"), suffix=c("", "_cvd"))
  
  p = ggplot(data_carte, aes(x=long, y=lat, group=group))  +
    geom_polygon(aes_string(fill="variable_100k"), colour = "grey") +
    geom_text(aes_string(x="centre_long", y="centre_lat", label="variable_100k"),
              data=data_covid_date, inherit.aes=FALSE,
              size=2) +
    theme_void() +
    scale_fill_gradient(low="green4", high="red4") +
    ggtitle(label=as.character(date_extraction), 
            subtitle=glue("{variable} pour 100k habitants")) +
    geom_frise(date_extraction, min(data$date), max(data$date), -3, 7.5, 41.8) +
    coord_map()
  
  if(print_p) print(p)
  
  filename=paste0("images/img_", date_extraction, ".png")
  ggsave(filename, p, width=7, height=7)
  filename
}

#une petite frise temporelle
geom_frise = function(date_current, date_start, date_end, x_start, x_end, y){
  percent = as.numeric(date_current-date_start)/as.numeric(date_end-date_start)
  current = x_start + (x_end-x_start)*percent
  list(
    annotate("segment", x=x_start, xend=x_end, y=y+0.1, yend=y+0.1), 
    annotate("point", x=current, y=y+0.1), 
    annotate("text", x=x_start, y=y, label=date_start, angle=30, hjust=1), 
    annotate("text", x=x_end, y=y, label=date_end, angle=30, hjust=1), 
    annotate("text", x=current, y=y+0.3, label=date_current)
  )
}

save_plot_list = function(dates, variable, step=1){
  dates = dates[seq.int(0, length(dates), by=step)]
  pb = progress::progress_bar$new(format = "[:bar] image :current/:total (:percent) in :elapsed (:eta)", 
                                  total = length(dates))
  pb$tick(0)
  tictoc::tic("saving plotlist")
  plot_list = purrr::map_chr(dates, ~{
    pb$tick()
    get_plot(data_covid2, .x, variable=variable)
  })
  filename = glue("output/covid_{variable}_100k_x{step}.gif")
  tictoc::toc()
  tictoc::tic("creating gif")
  gifski::gifski(plot_list, filename, width=800, height=700, 
                 loop=TRUE, delay=0.1)
  tictoc::toc()
  beepr::beep()
}


dates = unique(data_covid2$date) %>% sort() #358/372 le 25/03/2021, de 04/2020 à 03/2021

#utiliser step=25 pour les tests (n=14 img)
save_plot_list(dates, "roll7_hospitalises", step=1) #4.3 + 7.9 = 12.2 min par gif
save_plot_list(dates, "roll7_reanimation", step=1)
save_plot_list(dates, "roll7_deces", step=1)
save_plot_list(dates, "roll7_gueris", step=1)

