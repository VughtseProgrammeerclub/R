
rm(list=ls())

# Installeer en 'load' de package genaamd "librarian": 
# Deze package maakt het eenvoudig om andere packages te installeren, "loaden" en updaten:
install.packages("librarian")
library(librarian)

librarian::shelf(cbsodataR, compiler, dplyr, ggcorrplot, ggplot2, ggpp, ggpubr, 
                 ggrain, grid, gridExtra, installr, pacman, patchwork, readxl, 
                 reshape2, rstudioapi, scales, sf, stats, tidyr, tidyverse)

# Stel de map van dit specifieke R script in als "working directory":
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#############################

# Simpele rekensommetjes in R:
5+7
6-3
6*3
6/3

# Variabelen creëren:
a <- 6
b <- 4
c <- 7
d <- 9
# Je ziet dat deze variabelen nu zichtbaar zijn in het "Environment" panel.


# Aan de slag gaan met je nieuwe variabelen:
a+b
d-c
c*b

sum(a,b)
sum(a,b,c,d)

min(a,b,c,d)
max(a,b,c,d)

# We maken een vector genaamd "values". De "c" staat voor "compiler"
values <- c(a,b,c,d)  
mean(values)


# We maken nog een paar vectors, waarmee we zometeen een dataframe gaan maken:
naam <- c("Jan", "Truus", "Piet", "Maartje", "Lisa", "Jochem")  # "chr" stands for character
leeftijd <- c(65, 62, 39, 36, 10, 8)
sport <- c("Fietsen", "Tennis", "Voetbal", "Waterpolo", "Hockey", "Volleybal")

# We voegen deze objecten (vectors) samen in één dataframe:
familie <- data.frame(naam, leeftijd, sport)
# Deze dataframe heeft 6 rows, 3 columns. Oftewel: "6 observations of 3 variables"


# We willen een lijstje maken van de namen van de familieleden:
familie$naam

# We willen de gemiddelde leeftijd van de zes familieleden berekenen:
mean(familie$leeftijd)


# We willen één "cell" (observatie) binnen de dataframe aanpassen:
# Voorbeeld 1: Maartje is niet 36, maar 37 jaar oud:
familie[4, 2] <- 37
# Voorbeeld 2: Lisa zit niet op hockey, maar op ijshockey:
familie[5, 3] <- "IJshockey"

##############

# Het gebruiken van tidyverse en dplyr om te werken met dataframes:
# Voorbeeld 1: We willen een selectie maken, met alleen de familieleden die jonger zijn dan 60:
selectie <- familie %>%
  filter(leeftijd < 60)

# Voorbeeld 2: We willen een kolom toevoegen, waarin de lengtes (in cm) van ieder familielid staan: 
familie <- familie %>%
  mutate(lengte = c(182, 179, 191, 186, 140, 131))

# Voorbeeld 3: We willen de kolom met de sporten uit de dataframe verwijderen:
familie <- familie %>%
  select(-sport)



#################################################
#################################################


# Voorbeeld: data vanuit mijn eigen PhD project:
# (let op: deze data zijn "nep"; synthetic)
# (Deze dataset is helemaal compleet; er zijn dus geen "missing data")

dataset_scholen <- read_excel("scholen_onderzoek.xlsx")

# Er zitten 205 kinderen in deze dataset. Hoe veel zijn er jongens / meisjes?
table(dataset_scholen$geslacht)

# Hoe veel kinderen deden er mee per school?
table(dataset_scholen$school)

# Wat is de gemiddelde score (van alle kinderen) op de rekentaak?
mean(dataset_scholen$rekenen)

# Wat zijn de hoogste en de laagste scores die voorkomen op de rekentaak?
max(dataset_scholen$rekenen)
min(dataset_scholen$rekenen)


# Zelf doen: wat is de gemiddelde score (van alle kinderen) op de leestaak?


##############


# Wat is de gemiddelde score per school op de leestaak?
# Hiervoor maken we een nieuwe dataset (genaamd "scholen_lezen") met de dplyr package.
scholen_lezen <- dataset_scholen %>%
  group_by(school) %>%
  summarise(gemiddelde_lezen = mean(lezen))

# Nu maken we een barplot (histogram) van de gemiddelde leesscores per school:
ggplot(scholen_lezen, aes(x = school, y = gemiddelde_lezen)) + 
  geom_col(fill = "grey30") 

# Deze histogram is nog niet zo mooi... We maken hem dus wat mooier:
ggplot(scholen_lezen, aes(x = school, y = gemiddelde_lezen)) +
  geom_col(fill = "seagreen") +            # de staafjes krijgen een mooie kleur
  labs(title = "Gemiddelde leesscores per school",
       x = "School",
       y = "Gemiddelde Leesscore") +
  scale_y_continuous(limits = c(0, 90)) +  # y-as van 0 tot 90
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Zelf doen: maak een mooie histogram van de gemiddelde rekenscores per school.
#  Geef de staafjes een blauwe kleur, en zorg ervoor dat de labels kloppen.
#  Tip: kijk in "scholen_rekenen" dataframe wat een goede y-as limiet zou zijn.
#  Tip: je moet eerst een nieuwe dataframe maken, met de gemiddelde rekenscores per school:

scholen_rekenen <- dataset_scholen %>%
  group_by(school) %>%
  summarise(gemiddelde_rekenen = mean(rekenen))


##############


# We willen kijken naar de relatie tussen de "getallen" taak en de "reken" taak.
# Om deze relatie in beeld te brengen, kunnen we een "scatterplot" maken.
ggplot(dataset_scholen, aes(x = getallen, y = rekenen)) +
  geom_point(color = "black", size = 1.5) +
  labs(title = "scatterplot",
       x = "getallen",
       y = "rekenen")


# Dat kan wel wat mooier!
# We maken hem nu wat mooier, en we voegen een "regressielijn" toe. 
ggplot(dataset_scholen, aes(x = getallen, y = rekenen)) +
  geom_point(color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Relatie tussen getallentaak en rekentaak",
       x = "Score op getallentaak",
       y = "Score op rekentaak") +
  scale_x_continuous(limits = c(0, 70)) +  # x-as van 0 tot 70
  scale_y_continuous(limits = c(0, 60)) +  # y-as van 0 tot 60
  theme_classic()


# Zelf doen: maak een mooie scatterplot (met regressielijn) voor de relatie tussen de leestaak en de rekentaak.



##############


# Nu willen we een soortgelijk plotje maken, maar nu willen we de kinderen op
#  school "D" een andere kleur geven dan de kinderen van de andere scholen.

# Eerst moeten we dan een nieuwe variabele (genaamd "highlight_school") toevoegen aan de dataframe:
dataset_scholen$highlight_school <- ifelse(dataset_scholen$school == "D", "D", "Andere")

# Nu kunnen we de scatterplot met de aangepaste kleuren maken:
ggplot(dataset_scholen, aes(x = getallen, y = rekenen, color = highlight_school)) +
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c("D" = "orange2", "Andere" = "steelblue"),
    labels = c("D" = "School D", "Andere" = "Andere scholen")) +
  labs(
    title = "Relatie tussen getallentaak en rekentaak (school D in oranje)",
    x = "Score op getallentaak",
    y = "Score op rekentaak",
    color = "Schoolgroep") +  # titel van de legenda 
  scale_x_continuous(limits = c(0, 70)) +  # x-as van 0 tot 70
  scale_y_continuous(limits = c(0, 60)) +  # y-as van 0 tot 60
  theme_classic()


##############


# We hebben nu gekeken naar de samenhang (correlatie) tussen allerlei taken,
#  maar nog niet tussen alle vijf de taakjes. 
# Om dat te doen, kunnen we een correlatie-matrix maken van alle taakjes:

# Stap 1: selecteer alleen de relevante kolommen:
cor_data <- dataset_scholen %>%
  select(getallen, rekenen, geheugen, snelheid, lezen)

# Stap 2: maak de correlatie-matrix:
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

# Stap 3: maak de correlatie-plot:
#  (Zowel horizontaal als verticaal staan vier taakjes, omdat je niet de correlatie met zichzelf wilt weergeven)

ggcorrplot(cor_matrix,
           method = "square",        # zorgt ervoor dat het vierkantjes zijn
           type = "lower",           # alleen onderste helft tonen
           lab = TRUE,               # toont de getallen in de vakjes
           lab_size = 4,
           colors = c("blue", "white", "red"),  # rood = positief, blauw = negatief
           ggtheme = theme_classic(),
           show.legend = TRUE,
           legend.title = "Correlatie",  # titel van de legenda
           title = "Correlaties tussen de cognitieve taakjes")  # titel van de plot


#################################################
#################################################


# De volgende dataset bevat data van alle 342 Nederlandse gemeentes. 
dataset_gemeentes <- read_excel("gemeente_datafile.xlsx")

# We willen een top 10 maken van de gemeentes met de meeste inwoners.
# Dit kan op verschillende manieren. Hier maken we een nieuwe dataframe: 
top10_inwoners <- dataset_gemeentes %>%
  arrange(desc(Aantal_inwoners)) %>%  # sorteren op aantal inwoners, aflopend
  select(Gemeente_naam, Aantal_inwoners) %>%  # selecteer alleen de relevante kolommen
  slice_head(n = 10)  # selecteer alleen de bovenste 10 rijen
print(top10_inwoners)


##############


# We willen kijken naar de verdeling van de gemiddelde WOZ-waardes per gemeente:
ggplot(dataset_gemeentes, aes(x = reorder(Gemeente_naam, Gemiddelde_WOZ_2024), 
                              y = Gemiddelde_WOZ_2024)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Gemiddelde WOZ-waarde per gemeente (2024)",
       x = "Gemeente",
       y = "Gemiddelde WOZ-waarde (*1000 euro)") +
  theme_classic()


# Deze staafdiagram is wel heel erg vol! Het is totaal niet meer leesbaar!
# Laten we dus een diagram maken met alleen de gemeentes die helemaal rechts staan,
#  oftewel de 20 gemeentes met de hoogste WOZ-waardes.
# Kijk eens goed: we zien dat Vught ook hierin staat!
dataset_gemeentes %>%
  arrange(desc(Gemiddelde_WOZ_2024)) %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(Gemeente_naam, Gemiddelde_WOZ_2024),
             y = Gemiddelde_WOZ_2024)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 20 gemeentes met hoogste WOZ-waarde (2024)",
       x = "Gemeente",
       y = "Gemiddelde WOZ-waarde (*1000 euro)") +
  theme_classic()


##############


# Nu willen we een selectie maken van een aantal gemeentes waarin we specifiek geïnteresseerd zijn:
# Hiervoor moeten we eerst een lijstje met namen maken (dit is een vector):
gemeentes_lijstje <- c("Vught", "'s-Hertogenbosch", "Eindhoven", "Amsterdam", 
                      "Texel", "Nijmegen", "Maastricht", "Bloemendaal")

# Daarna kunnen we dan de selectie maken (in een nieuwe dataframe):
gemeentes_selectie <- dataset_gemeentes %>%
  filter(Gemeente_naam %in% gemeentes_lijstje)


# Nu kunnen we een histogram maken van het aantal inwoners in deze acht gemeentes:
ggplot(gemeentes_selectie, aes(x = Gemeente_naam, y = Aantal_inwoners)) +
  geom_col(fill = "lightblue3") +   
  labs(title = "Aantal inwoners per gemeente",
       x = "Gemeente",
       y = "Aantal inwoners") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 1000000)) +  # y-as van 0 tot 60
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Zelf doen: maak een histogram van gemiddelde WOZ-waarde in deze acht gemeentes:


##############


# We gaan nu weer kijken naar alle 342 gemeentes in Nederland.
# We gaan kijken naar de relatie tussen het gemiddelde inkomen (per inwoner) en de gemiddelde WOZ-waarde van de koophuizen:

ggplot(dataset_gemeentes, aes(x = Gemiddeld_inkomen, y = Gemiddelde_WOZ_2024)) +
  geom_point(color = "grey20", size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(title = "Relatie tussen gemiddeld inkomen (per inwoner) en gemiddelde WOZ-waarde (koophuizen)",
       x = "Gemiddeld inkomen (per inwoner) (*1000 euro)",
       y = "Gemiddelde WOZ waarde (koophuizen) (*1000 euro)") +
  scale_x_continuous(limits = c(10, 55)) +      # inkomens tussen 10.000 en 55.000 per jaar
  scale_y_continuous(limits = c(100, 1000)) +   # WOZ waardes tussen 100.000 en 1 miljoen
  theme_classic()


# Zelf doen: een plotje met de relatie tussen bevolkingsdichtheid en WOZ-waarde:
#  Tip: de bevolkingsdichtheid heeft de volgende min. en max.:
min(dataset_gemeentes$Bevolkingsdichtheid)
max(dataset_gemeentes$Bevolkingsdichtheid)


########################

# Nu willen we nog één gaaf ding doen!
# We willen een kaart van Nederland laten zien, waarop we de gemeentes een kleur geven
#  op basis van de variabelen die we in onze dataset hebben. 

# Eerst gebruiken we de onderstaande code een kaart van Nederland te "downloaden":
gemeente_2023 <- cbs_get_sf("gemeente", 2023)

# De "lege" kaart ziet er als volgt uit:
plot(gemeente_2023, max.plot = 1) 

# Nu maken we eerst een nieuwe dataframe, met daarin de "statcode" en "RegioS" variabelen,
#  die gebruikt kunnen worden bij het maken van de landkaart.
dataset_landkaart <- dataset_gemeentes %>%
  mutate(statcode = Gemeente_code,
         RegioS = Gemeente_code)

##############

# In deze landkaart gaan we de gemeentes een kleur geven o.b.v. de bevolkingsdichtheid:
map_of_dichtheid <- 
  cbs_join_sf_with_data("gemeente", 2022, dataset_landkaart) |> 
  transform(dichtheid = Bevolkingsdichtheid)
map_of_dichtheid |> 
  ggplot() + 
  geom_sf(aes(fill=dichtheid), color="#FFFFFF99") +
  scale_fill_viridis_c(direction = -1)+ 
  labs(fill="% Gemiddelde bevolkingsdichtheid") + 
  theme_void()

##############

# We kunnen nog zo'n kaart maken, met kleuren o.b.v. het gemiddelde inkomen per inwoner:
map_of_inkomen <- 
  cbs_join_sf_with_data("gemeente", 2022, dataset_landkaart) |> 
  transform(inkomen = Gemiddeld_inkomen)
map_of_inkomen |> 
  ggplot() + 
  geom_sf(aes(fill=inkomen), color="#FFFFFF99") +
  scale_fill_viridis_c(direction = -1)+ 
  labs(fill="% Gemiddeld inkomen") + 
  theme_void()

##############

# En nog zo'n kaart, met kleuren o.b.v. de gemiddelde WOZ-waarde:
map_of_WOZwaarde <- 
  cbs_join_sf_with_data("gemeente", 2022, dataset_landkaart) |> 
  transform(wozdata = Gemiddelde_WOZ_2024)
map_of_WOZwaarde |> 
  ggplot() + 
  geom_sf(aes(fill=wozdata), color="#FFFFFF99") +
  scale_fill_viridis_c(direction = -1)+ 
  labs(fill="% Gemiddelde WOZ-waarde") + 
  theme_void()

