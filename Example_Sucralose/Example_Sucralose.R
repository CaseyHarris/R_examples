#A ggplot graph example (followed by a leaflet map example)

library(readxl) #reads excel files
trace <- read_excel("Sucralose.xlsx", sheet="data") #read excel file

#Define the detection categories (quantified, below quantitation, estimated, etc...)
trace$detected[is.na(trace$quality) | trace$quality=="Q" | trace$quality=="Y"] <- "quantified"
trace$detected[trace$quality=="I" | trace$quality=="IQ" | trace$quality=="IQJ"] <- "below quantitation"
trace$detected[trace$quality=="J"] <- "estimated"
trace$detected[trace$quality=="U" | trace$quality=="UJ" | trace$quality=="UQ"] <- "not detected"

#Order the detection categories
trace$detected <- ordered(trace$detected, levels=c("quantified", "estimated", "below quantitation", "not detected"))

#Order the sites that will be on the x-axis
trace$site <- ordered(trace$site, levels=c("EHREUSE",
                                           "EH05TBOX",
                                           "EH15TBOX",
                                           "EH88SP",
                                           "EH48SP",
                                           "EH63SP",
                                           "EH02",
                                           "EH54",
                                           "EH43",
                                           "EH68",
                                           "EH60",
                                           "EHBRTN",
                                           "EHSCHOOL",
                                           "EGHBR-S2",
                                           "EGHBR-LSH",
                                           "EGHBR5",
                                           "EGHBR-W",
                                           "EGHBR-N",
                                           "EGHBR-TRL",
                                           "EGHBR-SSH",
                                           "EGHBR-SSP",
                                           "DUCKCR",
                                           "UNNAMETR1",
                                           "UNNAMETR2",
                                           "INDIGODLD",
                                           "LUCYBR",
                                           "D-DOGWOOD",
                                           "DTL"))

#Define the site categories
trace$category[trace$site=="EHREUSE" | trace$site=="EH05TBOX" | trace$site=="EH15TBOX" | trace$site=="EH48SP" | trace$site=="EH63SP" | trace$site=="EH88SP"] <- "Reclaimed water system"
trace$category[trace$site=="EH54" | trace$site=="EH43" | trace$site=="EH68" | trace$site=="EH60" | trace$site=="EH02" | trace$site=="EHBRTN" | trace$site=="EHSCHOOL"] <- "Retention ponds"
trace$category[trace$site=="D-DOGWOOD" | trace$site=="LUCYBR" | trace$site=="INDIGODLD" | trace$site=="UNNAMETR2" | trace$site=="UNNAMETR1" | trace$site=="DUCKCR" | trace$site=="EGHBR5" | trace$site=="EGHBR-W" | trace$site=="EGHBR-N" | trace$site=="EGHBR-SSH" | trace$site=="EGHBR-SSP" | trace$site=="EGHBR-TRL" | trace$site=="EGHBR-LSH" | trace$site=="EGHBR-S2"] <- "Storm runoff"
trace$category[trace$site=="DTL"] <- "Doctors Lake"
trace$category <- ordered(trace$category, levels=c("Reclaimed water system", "Storm runoff", "Retention ponds", "Doctors Lake"))

library(ggplot2)
library(scales)
show_col(viridis_pal()(4)) #the color palette I want to use

ggplot(trace, aes(site, value, group=site, color=detected)) +
  geom_point(size=3) +
  scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100), labels=c("0.01", "0.1", "1", "10", "100")) +
  scale_colour_manual(values=c("#440154FF", "#35B779FF", "#FDE725FF")) +
  ylab(expression(Concentration ~ (mu~ g ~ l^{-1}))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1),
        legend.title=element_blank(),
        legend.position="top",
        axis.title.x=element_blank()) +
  facet_grid(component ~ category, scales="free")
ggsave("Sucralose.png", width=7.5, height=3) #save

###############################################################################

#An example of making a map of the same data

loclabels <- read.csv("locations.csv")

library(sf)
library(dplyr)
all_pts <- st_as_sf(loclabels, coords=c("lon", "lat"), crs=4326) #load the locations
all_pts$site <- all_pts$STN_NAME
all_pts$STN_NAME <- NULL

#Summarize the data (get site means, etc.)
trace_summary <- trace %>% group_by(site, component, units, category) %>%
  summarize(mean = mean(value, na.rm=TRUE),
            geoMean = exp(mean(log(value), na.rm=TRUE)), 
            meth_det_limit_mean = mean(meth_det_limit, na.rm=TRUE),
            meth_det_limit_geoMean = exp(mean(log(meth_det_limit), na.rm=TRUE)),
            prac_quant_limit_mean = mean(prac_quant_limit, na.rm=TRUE),
            prac_quant_limit_geoMean = exp(mean(log(prac_quant_limit), na.rm=TRUE)))

#Define the value categories
trace_summary$detected <- "quantified"
trace_summary$detected[trace_summary$geoMean<=trace_summary$prac_quant_limit_geoMean] <- "below quantitation"
trace_summary$detected[trace_summary$geoMean<=trace_summary$meth_det_limit_geoMean] <- "not detected"
trace_summary$detected <- as.factor(trace_summary$detected)
trace_summary$detected <- ordered(trace_summary$detected, levels=c("not detected", "below quantitation", "quantified"))

#Filter to include only storm runoff sites
trace_summary <- filter(trace_summary, site=="D-DOGWOOD" | site=="LUCYBR" | site=="INDIGODLD" | site=="UNNAMETR2" | site=="UNNAMETR1" | site=="DUCKCR" | site=="EGHBR-S2" | site=="EGHBR-LSH" | site=="EGHBR5" | site=="EGHBR-W" | site=="EGHBR-N" | site=="EGHBR-SSH" | site=="EGHBR-SSP" | site=="EGHBR-TRL")
trace_summary$site <- ordered(trace_summary$site, levels=c("D-DOGWOOD", "LUCYBR", "INDIGODLD", "UNNAMETR2", "UNNAMETR1", "DUCKCR", "EGHBR-S2", "EGHBR-LSH", "EGHBR5", "EGHBR-W", "EGHBR-N", "EGHBR-SSH", "EGHBR-SSP", "EGHBR-TRL"))

#Make a map
library(leaflet)
library(htmltools)
library(viridis)
library(mapview)

i="Sucralose"

map_pts <- left_join(all_pts, trace_summary, by="site") #join location information to the rest of the data
map_pts <- filter(map_pts, !is.na(mean)) #remove extra sites
  
binpal <- colorFactor(viridis_pal()(3), map_pts$detected, reverse=TRUE) #set cateogry colors
  
m <- leaflet(map_pts) %>%
    setView(lng = -81.743959, lat = 30.129, zoom = 13) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addTiles() %>%
    addCircleMarkers(radius = 12, 
                     label = ~htmlEscape(site),
                     labelOptions = labelOptions(noHide=F, textOnly=T, textsize="12px", direction="left", offset=c(-12,0), style=list(color="black")),
                     stroke = TRUE,
                     color = "black",
                     weight = 1,
                     fillColor = ~binpal(map_pts$detected),
                     fillOpacity = FALSE) %>%
    addLegend("topleft", pal=binpal, values=~detected, 
              title=paste0("Storm runoff ", i), 
              opacity=1, 
              labFormat = labelFormat(suffix=paste0(" ", map_pts$units[1])))
m
  
webshot::install_phantomjs() #had to do this
mapshot(m, file=paste0("Map ", i, ".png"), vwidth=450, vheight=475) #save as png
mapshot(m, url=paste0("Map ", i, ".html")) #save as html
