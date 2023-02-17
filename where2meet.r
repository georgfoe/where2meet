library(httr)
library(dplyr)
library(jsonlite)
library(purrr)


query_location <- function(queryString) {
GET("https://v5.bvg.transport.rest/", 
    path = "locations", 
    query = list(query = queryString)) %>% 
    pluck(content) %>% 
    pluck(1)
}

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

get_reach_with_duration <- function(reach, duration) {
  reach %>% 
    keep(~.x$duration < duration) %>% 
    map(~.x %>% 
          pluck("stations") %>% 
          map_chr("name")) %>% 
    unlist()
}

loc1 <- query_location("'Blücherstraße 62-63, 10961 Berlin'")
loc2 <- query_location("'Reinhardtstraße 13, 10117 Berlin'")
loc3 <- query_location("'Carstennstraße 58, 12205 Berlin'")

reach1 <- get_reachables(loc1)
reach2 <- get_reachables(loc2)
reach3 <- get_reachables(loc3)

dur = 1
hits = NULL

while (length(hits) == 0) {
  
  cat("Teste mit Entfernung ", dur, " min", "\n")
  
  hits = get_reach_with_duration(reach1, dur) %>% 
    keep(~.x %in% get_reach_with_duration(reach2, dur)) %>% 
    keep(~.x %in% get_reach_with_duration(reach3, dur))
  
  dur = dur +1 
  
}

hits


