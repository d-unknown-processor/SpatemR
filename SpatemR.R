library(ggmap)
library(httr)
library(rjson)
library(RCurl)
library(grid)
library(png)
library(jpeg)

# Abfragen der Instagarm Daten

## httr Rederiction URI
full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
print(full_url)

#Zugangsdaten für den Instragram Client laden
source("secret.R",chdir=T)

## Konfiguration der authentifizierten Verbindung
instagram <- oauth_endpoint(
  authorize = "https://api.instagram.com/oauth/authorize",
  access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app(app_name, client_id, client_secret)

ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic",  type = "application/x-www-form-urlencoded",cache=FALSE)
tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]
## Daten abrufen
username <- "Inventionate"
user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,'&access_token=',token,sep="")),unexpected.escape = "keep")
id <- user_info$data[[1]]$id
# Follower erfragen
followers <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',id,'/follows?access_token=',token,sep="")),unexpected.escape = "keep")
anniID  <- followers$data[[1]]$id
### Kompletten eigenen Feed laden
media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',id,'/media/recent/?access_token=',token,'&count=33',sep="")),unexpected.escape = "keep")
# Nutzernamen auslesen
for (i in 1:length(media$data) ) {
  print(paste(i,': ',media$data[[i]]$user$full_name))
}
### Kompletten Feed eines anderen Nutzers laden
mediaAnni <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',anniID,'/media/recent/?access_token=',token,'&count=33',sep="")),unexpected.escape = "keep")
mediaAnni
# Nutzernamen auslesen
for (i in 1:length(mediaAnni$data) ) {
  print(paste(i,': ',mediaAnni$data[[i]]$user$full_name))
}

# Koordinatenpunkte pro Nutzen plotten

# Geokoordinaten der Bilder abrufen
df = data.frame(no = 1:length(media$data))

for (i in 1:length(media$data) ) {
  if ( is.null(media$data[[i]]$location$name) )
  {
    df$name[i] <- "Nicht benannt"
  }
  else
  {
    df$name[i] <- media$data[[i]]$location$name
  }
  df$latitude[i] <- media$data[[i]]$location$latitude
  df$longitude[i] <- media$data[[i]]$location$longitude
  date <- as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01")
  df$created_date[i] <- format(date, "%d.%m.%Y")
  # Stunden: %H
  # Minuten: %M
  # Datum nach drei Kategorien ordnen  
  hour <- as.numeric(format(date, "%H"))
  # Morgens: 5 bis 11 Uhr
  if (5 < hour && hour <= 11)
  {
    df$created_time[i] <- "morgens"
  }
  # Mittags: 11 bis 15 Uhr
  if (11 < hour && hour <= 15)
  {
    df$created_time[i] <- "mittags"
  }
  # Nachmittags: 15 bis 18 Uhr
  if (15 < hour && hour <= 18)
  {
    df$created_time[i] <- "nachmittags"
  }
  # Abends: 18 bis 23 Uhr
  if (18 < hour && hour <= 23)
  {
    df$created_time[i] <- "abends"
  }
  # Nachts: 23 bis 5 Uhr
  if (23 < hour || hour <= 5)
  {
    df$created_time[i] <- "nachts"
  }
  df$image[i] <- media$data[[i]]$images$standard_resolution$url  
}
df

# Geografische Analyse der sozialen Orte Chronologie
## Erstellen einer Karte für den geografischen Raum Karlsruhe
myMap <- get_map(location = c(lat = 49.01345, lon = 8.39451), source="stamen", zoom=17, maptype="watercolor", crop=FALSE) 
map <- ggmap(myMap, maprange=FALSE, extent = 'device', base_layer=ggplot(aes(x = longitude, y = latitude), data=df))
# legend="topright" 
plot  <- map + 
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
    ) +
  geom_path(size=1.5, alpha=0.75, colour="darkred") +
  geom_point(aes(colour=created_time), shape=15, alpha = 1, size = 25) +
  guides(colour = guide_legend(override.aes = list(size=5)))
plot

# Bilder in Karte einfügen
# Bilder abrufen
for (i in 1:nrow(df) ) {
  myurl = toString(df$image[i])
  z <- tempfile()
  download.file(myurl,z,mode="wb")
  assign( paste0("pic_",i,sep=""), readJPEG(z))
  file.remove(z) # cleanup
  assign( paste0("g_",i,sep=""), rasterGrob(paste("pic_",i,sep=""), interpolate=T))
}
# Bilder hinzufügen
margin <- 0.0005
plot.pic <- plot
for (i in 1:nrow(df)) {
  plot.pic <- plot.pic + inset(rasterGrob(get(paste("pic_",i,sep="")), interpolate=T), xmin=df$longitude[i]-margin, xmax=df$longitude[i]+margin, ymin=df$latitude[i]-margin,ymax=df$latitude[i]+margin)
}
plot.pic


# Abstrakte Analyse der sozialen Orte Chronologie
plot <- ggplot(df, aes(x=longitude, y=latitude)) + 
  geom_path(size=1.5,alpha=0.75,linetype="solid",colour="black") +
  geom_point(aes(colour=created_time), shape=15, alpha = 0.85, size = 24) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) 
plot
# Bilder in abstrakten Plot einfügen
# Bilder abrufen
for (i in 1:nrow(df) ) {
  myurl = toString(df$image[i])
  z <- tempfile()
  download.file(myurl,z,mode="wb")
  assign( paste0("pic_",i,sep=""), readJPEG(z))
  file.remove(z) # cleanup
  assign( paste0("g_",i,sep=""), rasterGrob(paste("pic_",i,sep=""), interpolate=T))
}
# Bilder hinzufügen
margin <- 0.05
plot.pic <- plot
for (i in 1:nrow(df)) {
  plot.pic <- plot.pic + annotation_custom(rasterGrob(get(paste("pic_",i,sep="")), interpolate=T), xmin=df$longitude[i]-margin, xmax=df$longitude[i]+margin, ymin=df$latitude[i]-margin,ymax=df$latitude[i]+margin)
}
plot.pic
