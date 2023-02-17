library(httr)
library(dplyr)
library(jsonlite)
library(purrr)

# Auf Grundlage einer in einem Query-String übergebenen Adresse werden Geo-Daten ermittelt
query_location <- function(queryString) {
GET("https://v5.bvg.transport.rest/", 
    path = "locations", 
    query = list(query = queryString)) %>% 
    pluck(content) %>% 
    pluck(1)
}

# Alle von einem Ausgangsort (loc) innerhalb von 60 Minuten erreichbaren Stationen werden abgerufen (sortiert nach Fahrtdauer)
get_reachables <- function(loc) {
  GET("https://v5.bvg.transport.rest/", 
      path = "stops/reachable-from", 
      query = list(latitude = loc$latitude, 
                   longitude = loc$longitude, 
                   address = loc$address, 
                   maxDuration = 60,
                   when = "tomorrow 2pm")) %>% 
    pluck(content)
}

# Auf Grundlage der abgerufenen reachables wird ermittelt welche Stationen in duration (in Minuten) oder weniger erreicht werden können
get_reach_with_duration <- function(reach, duration) {
  reach %>% 
    keep(~.x$duration < duration) %>% 
    map(~.x %>% 
          pluck("stations") %>% 
          map_chr("name")) %>% 
    unlist()
}

# Als Beispiel werden die Geo-Daten der Geschäftsstellen von AWO, Caritas und DRK ermittelt
loc1 <- query_location("'Blücherstraße 62-63, 10961 Berlin'")
loc2 <- query_location("'Reinhardtstraße 13, 10117 Berlin'")
loc3 <- query_location("'Carstennstraße 58, 12205 Berlin'")

# Die von den jeweiligen Locations erreichbaren Stationen werden abgerufen
reach1 <- get_reachables(loc1)
reach2 <- get_reachables(loc2)
reach3 <- get_reachables(loc3)

# Starte die Iteration mit 1 Minute Fahrzeit für alle
dur = 1

# Initialisiere die Ergebnisvariable hits
hits = NULL

# Initiiere eine Schleife die wiederholt wird, solange es keine Ergebnisse gibt
while (length(hits) == 0) {
  
  cat("Teste mit Entfernung ", dur, " min", "\n")
  
  # Ermittle die in duration von Location 1 erreichbaren Stationen
  hits = get_reach_with_duration(reach1, dur) %>% 
    # Prüfe welche davon auch von Location 2 in duration erreichbar sind
    keep(~.x %in% get_reach_with_duration(reach2, dur)) %>% 
    # Prüfe welche davon auch von Location 3 in duration erreichbar sind 
    keep(~.x %in% get_reach_with_duration(reach3, dur))
  
  # Erhöhe die Fahrtdauer um eine Minute
  dur = dur +1 
  
}

# Gib das Ergebnis aus
hits


