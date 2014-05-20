download.file("https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2", "StormData.csv.bz2", method = "curl")
storm.data <- read.csv(bzfile("StormData.csv.bz2"), stringsAsFactors = FALSE)

## names(storm.data) <- tolower(names(storm.data))

storm.data$date <- as.Date(storm.data$BGN_DATE, format = "%m/%d/%Y")

storm.data$prop.damage <- rep(0, nrow(storm.data))
storm.data$prop.damage <- ifelse(storm.data$PROPDMGEXP == "B", storm.data$PROPDMG * 1e+09, ifelse(storm.data$PROPDMGEXP %in% c("M", "m"), storm.data$PROPDMG * 1e+06, ifelse(storm.data$PROPDMGEXP == "K", storm.data$PROPDMG * 1000, ifelse(storm.data$PROPDMGEXP %in% c("H", "h"), storm.data$PROPDMG * 100, ifelse(storm.data$PROPDMGEXP %in% seq("0":"9"), storm.data$PROPDMG * 10^as.numeric(as.character(storm.data$PROPDMGEXP)), storm.data$PROPDMG)))))

storm.data$crop.damage <- rep(0, nrow(storm.data))
storm.data$crop.damage <- ifelse(storm.data$CROPDMGEXP == "B", storm.data$CROPDMG * 1e+09, ifelse(storm.data$CROPDMGEXP %in% c("M", "m"), storm.data$CROPDMG * 1e+06, ifelse(storm.data$CROPDMGEXP == "K", storm.data$CROPDMG * 1000, ifelse(storm.data$CROPDMGEXP %in% c("H", "h"), storm.data$CROPDMG * 100, ifelse(storm.data$CROPDMGEXP %in% seq("0":"9"), storm.data$CROPDMG * 10^as.numeric(as.character(storm.data$CROPDMGEXP)), storm.data$CROPDMG)))))

ind.convection <- "\\bL\\S+?G\\b|(NADO)|(\\bTOR\\S+?O\\b|(\\bFUN))|THUNDERSTORM|TSTM|(WIND)|(WND)|HAIL"
ind.ext.temp <- "COLD|HEAT|HYPERTHERMIA|HYPOTHERMIA|LOW TEMPERATURE|RECORD HIGH|RECORD LOW|Record temperature|RECORD WARM|Temperature record|UNSEASONABLY COOL|UNSEASONABLY HOT|UNUSUAL WARMTH|UNUSUAL/RECORD WARMTH|UNUSUALLY WARM|VERY WARM|WARM WEATHER|WARM DRY CONDITIONS"
ind.flood <- "(\\bFL\\S+?D)|RAIN|PRECIP|SHOWER"
ind.marine <- "^COASTAL(\\s)?STORM$|TSUNAMI|^RIP CUR"
ind.trop.cycl <- "HURRICANE|TROPICAL STORM"
ind.winter <- "(SNOW)|(ICE)|(ICY)|(FREEZ)|(WINT)|AVALAN|FROST|LOW TEMP|BLIZZARD"

storm.data$category <- rep(0, nrow(storm.data))
storm.data$category <- ifelse(grepl(ind.convection, storm.data$EVTYPE, ignore.case = TRUE), 1, ifelse(grepl(ind.ext.temp, storm.data$EVTYPE, ignore.case = TRUE), 2, ifelse(grepl(ind.flood, storm.data$EVTYPE, ignore.case = TRUE), 3, ifelse(grepl(ind.marine, storm.data$EVTYPE, ignore.case = TRUE), 4, ifelse(grepl(ind.trop.cycl, storm.data$EVTYPE, ignore.case = TRUE), 5, ifelse(grepl(ind.winter, storm.data$EVTYPE, ignore.case = TRUE), 6, 7))))))
storm.data$category <- factor(storm.data$category)
levels(storm.data$category) <- c("convection", "extreme.temp", "flood", "marine", "tropical.cycl", "winter", "other")

## subset(storm.data, PROPDMGEXP %in% seq("0" : "9"), select = c(PROPDMG, PROPDMGEXP, prop.damage))

min(subset(storm.data, category == "convection")$date)
min(subset(storm.data, category != "convection")$date)
storm.data.restricted <- subset(storm.data, date >= "1993-01-01")

storm.data.restricted <- subset(storm.data.restricted, STATE %in% state.abb)

library(ggplot2)

sum.fatalities <- aggregate(storm.data.restricted$FATALITIES, list(category = storm.data.restricted$category, state = storm.data.restricted$STATE), sum)
colnames(sum.fatalities)[3] <- "fatalities"
ggplot(sum.fatalities, aes(x = state, y = fatalities, fill = category)) + geom_bar(stat = "identity") + ggtitle("Fatalities by state and by category") + labs(x = "State", y = "Fatalities") + theme(plot.title = element_text(size = 20)) + scale_fill_discrete("Weather Event", labels = c("Convection", "Extreme Temp", "Flood", "Marine", "Tropical cyclones", "Winter", "Other"))

sum.injuries <- aggregate(storm.data.restricted$INJURIES, list(category = storm.data.restricted$category, state = storm.data.restricted$STATE), sum)
colnames(sum.injuries)[3] <- "injuries"
ggplot(sum.injuries, aes(x = state, y = injuries, fill = category)) + geom_bar(stat = "identity") + ggtitle("Injuries by state and by category") + labs(x = "State", y = "Injuries") + theme(plot.title = element_text(size = 20)) + scale_fill_discrete("Weather Event", labels = c("Convection", "Extreme Temp", "Flood", "Marine", "Tropical cyclones", "Winter", "Other"))

storm.data.restricted$material.damage <- (storm.data.restricted$crop.damage + storm.data.restricted$prop.damage) / 1000000
sum.damage <- aggregate(storm.data.restricted$material.damage, list(category = storm.data.restricted$category, state = storm.data.restricted$STATE), sum)
colnames(sum.damage)[3] <- "damages"
library(scales)
ggplot(sum.damage, aes(x = state, y = damages, fill = category)) + geom_bar(stat = "identity") + ggtitle("Damages (in million $) by state and by category") + labs(x = "State", y = "Damages (in million $)") + theme(plot.title = element_text(size = 20)) + scale_fill_discrete("Weather Event", labels = c("Convection", "Extreme Temp", "Flood", "Marine", "Tropical cyclones", "Winter", "Other")) + scale_y_continuous(labels = comma)

sum.fatalities.USA <- aggregate(sum.fatalities$fatalities, list(category = sum.fatalities$category), sum)
colnames(sum.fatalities.USA)[2] <- "fatalities"
sum.fatalities.USA[order(sum.fatalities.USA$fatalities, decreasing = TRUE), ]
library(xtable)
print(xtable(sum.fatalities.USA, digits = 0), type = "html", include.rownames = FALSE)

sum.injuries.USA <- aggregate(sum.injuries$injuries, list(category = sum.injuries$category), sum)
colnames(sum.injuries.USA)[2] <- "injuries"
sum.injuries.USA[order(sum.injuries.USA$injuries, decreasing = TRUE), ]
library(xtable)
print(xtable(sum.injuries.USA, digits = 0), type = "html", include.rownames = FALSE)

sum.damage.USA <- aggregate(sum.damage$damages, list(category = sum.damage$category), sum)
colnames(sum.damage.USA)[2] <- "damages"
sum.damage.USA[order(sum.damage.USA$damages, decreasing = TRUE), ]
library(xtable)
print(xtable(sum.damage.USA, digits = 0), type = "html", include.rownames = FALSE)

