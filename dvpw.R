library(rnaturalearth)
library(rnaturalearthdata)
library(pdftools)
library(stringr)
library(dplyr)
library(tidyverse)
library(rvest)
library(xml2)
library(httr)
library(tidygeocoder)
library(gender)

# myhtmltidygeocoder# myhtml <- "https://www.dvpw.de/dvpw2024/programm/panels" %>% GET %>% content

# myhtml %>% write_html("myhtml.html")
myhtml <- read_html("myhtml.html")

mynodes <- (myhtml %>% html_nodes("div.col-md-8 div"))[7:27]

# mynodes[1] %>% html_text2

# mynodes[1] %>% html_nodes("h2")

pnodes <- lapply(mynodes, function (x) html_nodes(x, "p"))
pnodes %>% unlist %>% length

pnodes <- lapply(pnodes, function (x) x <- x[html_text2(x) != " "])
pnodes %>% unlist %>% length

pnodes <- lapply(pnodes, html_text2)
pnodes %>% unlist %>% length

pnodes <- unlist(pnodes)

panels <- split(pnodes, pnodes %>% str_detect("\\|") %>% cumsum)

# Unique panels
panels <- panels[!duplicated(lapply(panels, function (x) x[1] %>% str_extract(".*(?= \\|)")))]

# Panel ID
panels %>% lapply(function (x) x[1] %>% str_extract(".*(?= \\|)"))

# Panel title
panels %>% lapply(function (x) x[1] %>% str_extract("(?<= \\| ).*"))


panels_df <- data.frame(panel = names(panels),
                        panel_id = unlist(panels %>% lapply(function (x) x[1] %>% str_extract(".*(?= \\|)"))),
                        panel_title = panels %>% lapply(function (x) x[1] %>% str_extract("(?<= \\| ).*")) %>% unlist
)
panels_df

# Chair
panels %>% lapply(function (x) x[x %>% lapply(function (y) str_detect(y, "Chair")) %>% unlist] %>% str_remove("Chair\\: ") %>% str_remove("Chair, Discussant\\: "))

# Discussant
panels %>% lapply(function (x) x[x %>% lapply(function (y) str_detect(y, "Discussant")) %>% unlist] %>% str_remove(" \\(tbc\\)") %>% str_remove("Discussant\\: ") %>% str_remove("Chair, Discussant\\: "))
      
# Moderation
panels %>% lapply(function (x) x[x %>% lapply(function (y) str_detect(y, "Moderation")) %>% unlist] %>% str_remove("Moderation\\: ") %>% 
                    
                    str_replace_all("(?<=Medienforschung),(?= Hans)", " +"))

# chair_df <- data.frame(chair = panels %>% lapply(function (x) x[x %>% lapply(function (y) str_detect(y, "Chair")) %>% unlist] %>% str_remove("Chair\\: ") %>% str_remove("Chair, Discussant\\: ")) %>% unlist)

# Papers
papers <- panels %>% lapply(function (x) x[x %>% lapply(function (y) str_detect(y, "\n")) %>% unlist])

papers[authors %>% lapply(function (x) length(x)) == 0] <- panels[authors %>% lapply(function (x) length(x)) == 0] %>% lapply(function (x) x[x %>% lapply(function (y) !str_detect(y, "(\\||Zuordnung|Chair|Discussant|Moderation)")) %>% unlist])

# Authors
papers %>% lapply(function (x) str_extract(x, ".*(?=:\n)"))

# Author names
authors <- papers %>% lapply(function (x) x %>% str_remove_all("(?<= \\(CAIS\\)),") %>%
                               str_replace_all("Laila Riedmüller:", "Laila Riedmüller, Friedrich-Alexander-Universität Erlangen-Nürnberg:") %>%
                               str_replace_all("Tiziano Zgaga:", "Tiziano Zgaga, Universität Konstanz:") %>%
                               str_replace_all("Nicolas Fliess, Karen Schönwälder, beide Max-Planck-Institut zur Erforschung multireligiöser und multiethnischer Gesellschaften:", "Nicolas Fliess, Max-Planck-Institut zur Erforschung multireligiöser und multiethnischer Gesellschaften, Karen Schönwälder, Max-Planck-Institut zur Erforschung multireligiöser und multiethnischer Gesellschaften:") %>%
                               str_replace_all("Armin von Schiller, Karina Mross, beide German Institute of Development and Sustainability \\(IDOS\\):", "Armin von Schiller, German Institute of Development and Sustainability \\(IDOS\\), Karina Mross, German Institute of Development and Sustainability \\(IDOS\\):
") %>%
                               str_replace_all("Freiburg Institute for Advanced Studies \\(FRIAS\\), Albert-Ludwigs-Universität Freiburg", "Albert-Ludwigs-Universität Freiburg") %>% 
                               str_replace_all("Eda Keremoglu:", "Eda Keremoglu, Universität Konstanz:") %>%
                               str_replace_all("Alexander De Juan:", "Alexander De Juan, Universität Osnabrück:") %>%
                               str_replace_all("Nils Bormann:", "Nils Bormann, Universität Witten/Herdecke:") %>%
                               str_replace_all("Christian Gläßel:", "Christian Gläßel, Hertie School:") %>%
                               str_replace_all("Myriam Ahmed:", "Myriam Ahmed, Freie Universität Berlin:") %>%
                               str_replace_all("CARPO e.V., Humboldt Universität Berlin", "Humboldt-Universität zu Berlin") %>%
                               str_replace_all("ESPOL-L.., Université Catholique de Lille", "Université Catholique de Lille") %>%
                               str_replace_all("University of Allahabad, India", "University of Allahabad") %>%
                               str_replace_all("Priska Daphi:", "Priska Daphi, :") %>%
                               str_replace_all("Larissa Meier, Priska", "Larissa Meier, Universität Bielefeld, Priska") %>%
                               str_replace_all("Martin Baesler:", "Martin Baesler, Albert-Ludwigs-Universität Freiburg:") %>% # Martin Baesler, Albert-Ludwigs-Universität Freiburg
                               str_replace_all("Universitat Pompeu Fabra, Barcelona", "Universitat Pompeu Fabra") %>%
                               str_replace_all("Cranfield University, UK", "Cranfield University") %>%
                               str_replace_all("Huhnholz, FAU Erlangen-Nürnberg, LMU München, LUH Hannover", "Huhnholz, FAU Erlangen-Nürnberg - LMU München - LUH Hannover") %>%
                               str_replace_all("Europäische Akademie der Arbeit, Frankfurt am Main", "Europäische Akademie der Arbeit") %>%
                               str_replace_all("Jan Wilkens, Jan Wilkens", "Jan Wilkens") %>%
                               str_replace_all("Spring Hill College, USA / Universität Bonn", "Spring Hill College / Universität Bonn") %>% 

                               str_remove_all("(?<=Bard College),(?= Berlin)") %>%
                               str_remove_all("(?<=Europäisches Hochschulinstitut),(?= Florenz)") %>%
                               str_replace_all("(?<=Freiburg Institute for Advanced Studies (FRIAS)
),(?= Albert-Ludwigs-Universität Freiburg)", " -") %>%
                               str_remove_all("(?<=Akademie für Politische Bildung),(?= Tutzing)") %>%
                               str_remove_all("(?<=Institut für Europäische Politik),(?= Berlin / Eberhard Karls Universität Tübingen:)") %>% 
                               str_extract(".*(?=:\n)") 
) %>%

  lapply(function (x) str_split(x, "\\, "))

authors[['275']]
papers[['275']]  


authors[authors %>% sapply(function (x) all(is.na(x)))] <- papers[authors %>% sapply(function (x) all(is.na(x)))]  %>%
  lapply(function (x) x %>% 
                                  str_replace_all("Laila Riedmüller:", "Laila Riedmüller, Friedrich-Alexander-Universität Erlangen-Nürnberg:") %>% 
                                  str_replace_all("Tiziano Zgaga:", "Tiziano Zgaga, Universität Konstanz:") %>% 
                                  str_replace_all("Nicolas Fliess, Karen Schönwälder, beide Max-Planck-Institut zur Erforschung multireligiöser und multiethnischer Gesellschaften", "Nicolas Fliess, Max-Planck-Institut zur Erforschung multireligiöser und multiethnischer Gesellschaften, Karen Schönwälder, Max-Planck-Institut zur Erforschung multireligiöser und multiethnischer Gesellschaften") %>% 
           str_replace_all("Armin von Schiller, Karina Mross, beide German Institute of Development and Sustainability \\(IDOS\\)", "Armin von Schiller, German Institute of Development and Sustainability \\(IDOS\\), Karina Mross, German Institute of Development and Sustainability \\(IDOS\\)
") %>% 
                                  str_replace_all("Eda Keremoglu:", "Eda Keremoglu, Universität Konstanz:") %>% 
                                  str_replace_all("Alexander De Juan:", "Alexander De Juan, Universität Osnabrück:") %>% 
                                  str_replace_all("Nils Bormann:", "Nils Bormann, Universität Witten/Herdecke:") %>% 
                                  str_replace_all("Christian Gläßel:", "Christian Gläßel, Hertie School:") %>% 
                                  str_replace_all("Myriam Ahmed:", "Myriam Ahmed, Freie Universität Berlin:") %>% 
                                  str_replace_all("CARPO e.V., Humboldt Universität Berlin", "Humboldt-Universität zu Berlin") %>% 
                                  str_replace_all("ESPOL-L.., Université Catholique de Lille", "Université Catholique de Lille") %>% 
                                  str_replace_all("Institut für Europäische Politik, Berlin", "Institut für Europäische Politik") %>%
                                  
                                  str_replace_all("University of Allahabad,  India", "University of Allahabad") %>% 
                                  str_replace_all("Priska Daphi:", "Priska Daphi, :") %>% 
                                  str_replace_all("Larissa Meier, Priska", "Larissa Meier, Universität Bielefeld, Priska") %>% 
                                  str_replace_all("Martin Baesler:", "Martin Baesler, Albert-Ludwigs-Universität Freiburg:") %>% # Martin Baesler, Albert-Ludwigs-Universität Freiburg
                                  str_replace_all("Universitat Pompeu Fabra, Barcelona", "Universitat Pompeu Fabra") %>%
                                  str_replace_all("Cranfield University, UK", "Cranfield University") %>%
                                  str_replace_all("Huhnholz, FAU Erlangen-Nürnberg, LMU München", "Huhnholz, FAU Erlangen-Nürnberg - LMU München") %>%
                                  str_replace_all("Europäische Akademie der Arbeit, Frankfurt am Main", "Europäische Akademie der Arbeit") %>%
                                  str_replace_all("Jan Wilkens, Jan Wilkens", "Jan Wilkens") %>%
           str_replace_all("Spring Hill College, USA / Universität Bonn", "Spring Hill College / Universität Bonn") %>% 
           
                                  str_remove_all("(?<=Bard College),(?= Berlin)") %>%
                                  str_remove_all("(?<=Europäisches Hochschulinstitut),(?= Florenz)") %>% 
                                  str_replace_all("(?<=Freiburg Institute for Advanced Studies (FRIAS)
),(?= Albert-Ludwigs-Universität Freiburg)", " -") %>%
                                  str_remove_all("(?<=Akademie für Politische Bildung),(?= Tutzing)") %>%
                                  str_remove_all("(?<=Institut für Europäische Politik),(?= Berlin / Eberhard Karls Universität Tübingen:)") %>%  
         str_split("\\, "))

# authors

# (authors %>% unlist %>% table %>% sort(decreasing = T)) %>% data.frame %>% View

# authors %>% sapply(function (x) str_detect(x, "Erfort")) %>% unlist %>% table

# authors[(authors %>% sapply(function (x) ifelse(length(x) > 0, min(sapply(x, function (y) length (y))), 0))) > 1] %>% lapply(function (x) x[[1]][seq(1, length(x[[1]]), 2)]) 

# authors[(authors %>% sapply(function (x) ifelse(length(x) > 0, min(sapply(x, function (y) length (y))), 0))) > 1] %>% lapply(function (x) x[[1]][seq(1, length(x[[1]]), 2)]) %>% unlist %>% data.frame %>% View

# Author affilitation
# authors[(authors %>% sapply(function (x) ifelse(length(x) > 0, min(sapply(x, function (y) length (y))), 0))) > 1] %>% lapply(function (x) x[[1]][seq(2, length(x[[1]]), 2)])

# (authors[(authors %>% sapply(function (x) ifelse(length(x) > 0, min(sapply(x, function (y) length (y))), 0))) > 1] %>% lapply(function (x) x[[1]][seq(2, length(x[[1]]), 2)]) %>% unlist %>% table %>% sort(decreasing = T))[1:50]

# authors[(authors %>% sapply(function (x) ifelse(length(x) > 0, max(sapply(x, function (y) length (y))), 0))) > 1] %>% lapply(function (x) ifelse(length(x[[1]]) > 1, x[[1]][seq(2, length(x[[1]]), 2)], NA)) 

# (authors[(authors %>% sapply(function (x) ifelse(length(x) > 0, max(sapply(x, function (y) length (y))), 0))) > 1] %>% lapply(function (x) ifelse(length(x[[1]]) > 1, x[[1]][seq(2, length(x[[1]]), 2)], NA)) %>% unlist %>% table %>% sort(decreasing = T))[1:50]

# author=1


affils_df <- data.frame()
for(author in names(authors)) {
  authors[[author]]
    # print(authors[[author]][1])

    for (i in 1:length(authors[[author]])) {
      thisaffil <- c()
      thisauthor <- c()
      if(length(authors[[author]]) > 0) { # (length(authors[[author]][[i]]) %% 2) == 0 & 
        
      for (j in 1:length(authors[[author]][[i]])) {
        if((j %% 2) == 1) {
          print(authors[[author]][[i]][j])
          thisauthor <- c(thisauthor, authors[[author]][[i]][j])
        }
        if((j %% 2) == 0) {
          print(authors[[author]][[i]][j])
          thisaffil <- c(thisaffil, authors[[author]][[i]][j])
        }
      }
      affils_df <- data.frame(panel = author, 
                           paper = i, 
                           author = thisauthor, 
                           affil = c(thisaffil, rep(NA, length(thisauthor)-length(thisaffil)))
) %>% 
        rbind(affils_df, .)
    }
    
  }
  
}

(affils_df$affil %>% table %>% sort(decreasing = T))[1:50]

# affils %>% filter(is.na(author) | is.na(affil)) %>% View
affils_df <- affils_df %>% group_by(panel, paper) %>% mutate(authors_on_paper = n())

# Paper titles
papers_df <- data.frame(paper_title = papers %>% lapply(function (x) str_extract(x, "(?<=:\n).*")) %>% unlist, 
           panel = lapply(names(papers)[!(names(papers) == "351")], function (x) rep(x, length(papers[[x]]))) %>% unlist,
           paper = lapply(papers, function (x) seq(1:length(x)))[!(names(papers) == "351")] %>% unlist) %>% 
  group_by(panel) %>% mutate(papers_on_panel = n())

# papers[papers %>% lapply(length) == 0]
# Make list to data frame but keep list index as column

# Zuordnung
assignm_df <- data.frame(assignm = (panels %>% lapply(function (x) x[x %>% lapply(function (y) str_detect(y, "Zuordnung")) %>% unlist] %>% str_remove("Zuordnung\\: "))) %>% unlist,
                      panel = names(panels)[lapply((panels %>% lapply(function (x) x[x %>% lapply(function (y) str_detect(y, "Zuordnung")) %>% unlist] %>% str_remove("Zuordnung\\: "))), length) != 0])



all_df <- merge(panels_df, papers_df, by = "panel") 

all_df <- merge(all_df, affils_df, by = c("panel", "paper")) %>% merge(assignm_df, by = "panel")

all_df <- all_df %>% group_by(author) %>%  mutate(w_author = sum(1/authors_on_paper)) # %>% select(author, w_author)
all_df$authors_on_paper_inv <- 1/all_df$authors_on_paper


save(all_df, file = "all_df.RData")

library(openxlsx)
write.xlsx(all_df, "dvpw_panels_2024.xlsx")
save(all_df, file = "all_df.RData")


# EVAL

all_df %>% group_by(affil) %>% summarise(authors_on_paper_inv = sum(authors_on_paper_inv), m_authors_on_paper = mean(authors_on_paper)) %>% arrange(desc(authors_on_paper_inv)) %>% head(50) %>% View

all_df %>% group_by(affil) %>% summarise(n_affil = n()) %>% arrange(desc(n_affil)) %>% head(15) %>% View

all_df %>% group_by(author) %>% summarise(n_affil = n()) %>% arrange(desc(n_affil)) %>% head(15) %>% View

all_df %>% group_by(author) %>% summarise(n_affil = n()) %>% filter(n_affil == 3) %>% View

# Get author first names
all_df$author_first <- all_df$author %>% str_extract("^[A-Z][a-z]+")
# all_df$author[is.na(all_df$author_first)]


# Detect gender of first name using gender package
# remotes::install_github("lmullen/genderdata")
all_df$gender <- all_df$author_first %>% gender

test <- all_df$author_first %>% gender
table(test$gender)

test <- test[!duplicated(test$name), ]
table(test$gender)

test2 <- merge(all_df, test, by.x = "author_first", by.y = "name", all.x = T)

table(test2$gender)/nrow(test2[!is.na(test2$gender),])
table(test2$gender[!is.na(test2$gender)], test2$assignm[!is.na(test2$gender)])  # , useNA = "ifany"
# Show shares of gender within assignm


test2$section <- test2$assignm %>% str_extract(".*?(?=,)") %>% str_remove("Sektion ")

# ggplot
test2 %>% filter(!is.na(gender)) %>%  group_by(section) %>% summarise(female = mean(gender == "female", na.rm = T), n = n()) %>% 
ggplot(aes(x = fct_reorder(section, -female), y = female, size = n)) +
  geom_point() + coord_flip() +
  # Add horizontal line at 50%
  geom_hline(yintercept = mean((filter(test2, !is.na(gender))$gender == "female"), na.rm = T), linetype = "dashed") + 
  xlab("")

ggsave("gender_section.pdf", height = 4*2^.5, width = 10, device = cairo_pdf)
mean((filter(test2, !is.na(gender))$gender == "female"))

test2 %>% group_by(author_first) %>% summarise(n = n()) %>% arrange(desc(n)) %>% head(15) %>% View

all_df$affil %>% unique %>% length
all_df$author %>% unique %>% length

(all_df$affil %>% table %>% sort(decreasing = T))[1:50]





affil_geo <- all_df %>% group_by(affil) %>% summarise(n_affil = n()) %>% arrange(desc(n_affil)) # %>% head(100) 
mycoords <- geo(affil_geo$affil)

# Manually code viadrina
# lat = 52.34229, long = 14.55402
mycoords$lat[mycoords$address == "Europa-Universität Viadrina Frankfurt (Oder)"] <- 52.34229
mycoords$long[mycoords$address == "Europa-Universität Viadrina Frankfurt (Oder)"] <- 14.55402

affil_geo <- merge(affil_geo, mycoords, by.x = "affil", by.y = "address", all.x = T)
save(affil_geo, file = "affil_geo.RData")

world <- ne_countries(scale = "medium", returnclass = "sf") # %>% filter(iso_a2_eh %in% c(country_analysis$country, "GB"," PT", "HR", "FR"))



# world <- left_join(world, country_analysis, by = c("iso_a2_eh" = "country"), keep = T)

ggplot(data = world, fill = NA) +
  geom_sf() + xlim(c(5,16)) + ylim(c(46,56)) + 
  geom_point(aes(x = long,y =  lat), data = affil_geo, size = affil_geo$n_affil*0.5, alpha = .5)
  # geom_sf_label(aes(label = round(mean_vote_diff, 3)), color = "white") + xlab("") + ylab("")
ggsave("map.pdf", width = 4*2^.5, height = 6, device = cairo_pdf)

# Vorläufiges Panel-Programm (20.05.2024)
# 
# Di D | Dienstag, 24.09.2024, 14:00-15:30 Uhr
# 
# Mi A | Mittwoch, 25.09.2024, 9:00-10:30 Uhr
# 
# Mi E | Mittwoch, 25.09.2024, 16:00-17:30 Uhr
# 
# Do A | Donnerstag, 26.09.2024, 9:00-10:30 Uhr
# 
# Do D | Donnerstag, 26.09.2024, 14:00-15:30 Uhr
# 
# Fr A | Freitag, 27.09.2024, 9:00-10:30 Uhr
# 
# Fr D | Freitag, 27.09.2024, 14:00-15:30 Uhr
