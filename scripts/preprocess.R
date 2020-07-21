
library(dplyr)

discarded_columns <- c('REMARKS', 'REFNUM', 'LATITUDE', 'LONGITUDE',
                       'LATITUDE_E', 'LONGITUDE_', 'ZONENAMES', 'STATEOFFIC',
                       'WFO', 'BGN_TIME', 'TIME_ZONE', 'COUNTY', 'COUNTYNAME',
                       'END_TIME', 'COUNTY_END', 'COUNTYENDN', 'END_RANGE',
                       'END_LOCATI', 'BGN_RANGE', 'BGN_LOCATI', 'BGN_AZI',
                       'END_AZI', 'LENGTH', 'WIDTH', 'STATE__', 'MAG', 'F',
                       'BGN_DATE', 'END_DATE', 'STATE'
)

for (colname in discarded_columns) {
    storm_data[colname] <- NULL
}

original_event_types <- list('tornado', 'hail', 'rain', 'snow', 'storm', 'dry',
                             'sleet', 'cloud', 'thunder', 'lightning', 'flood',
                             'drizzle', 'drought', 'frost', 'cold', 'eruption',
                             'fog', 'dust', 'waterspout', 'blizzard', 'tsunami',
                             c('wind', 'gust'),
                             c('tidal', 'tide'),
                             c('erosion', 'erosin'),
                             c('avalanche', 'avalance'),
                             c('tide', 'wave', 'surge'),
                             c('hurricane', 'whirlwind', 'typhoon'),
                             c('landslump', 'landslide', 'mudslide')
                          )

storm_data$EVTYPE <- tolower(storm_data$EVTYPE)
construct_new_data <- function (other_storm_data) {
    new_variable_name <- 'event_type'
    new_storm_data <- list()
    counter <- 1
    for (i in 1:nrow(other_storm_data)) {
        key_words <- strsplit(other_storm_data[i, 'EVTYPE'],
                              paste(c('/', ' '), collapse = '|'))
        for (original_event_type in original_event_types) {
            found <- F
            for (my_type in original_event_type) {
                if (my_type %in% key_words) {
                    found = T
                    break
                }
            }
            if (found) {
                new_row <- other_storm_data[i, -1]
                new_row[new_variable_name] <- original_event_type[[1]][1]
                new_storm_data[[counter]] <- new_row
                counter <- counter + 1
            }
        }
    }
    dplyr::bind_rows(new_storm_data)
}

system.time(new_storm_data <- construct_new_data(storm_data))
new_storm_data$PROPDMGEXP <- tolower(new_storm_data$PROPDMGEXP)
new_storm_data$CROPDMGEXP <- tolower(new_storm_data$CROPDMGEXP)

################################################################################

create_heath_data <- function (input_data) {
    heath_properties <- input_data[, c('FATALITIES', 'INJURIES', 'event_type')]
    fatalities <- heath_properties[, c('FATALITIES', 'event_type')]
    fatalities$heath_type <- rep('fatalities', nrow(heath_properties))
    names(fatalities) <- c('count', 'event_type', 'heath_type')
    
    injuries <- heath_properties[, c('INJURIES', 'event_type')]
    injuries$heath_type <- rep('injuries', nrow(heath_properties))
    names(injuries) <- c('count', 'event_type', 'heath_type')
    
    rbind(fatalities, injuries)
}
heath_data <- create_heath_data(new_storm_data)

################################################################################

system.time(
    economic_data <- apply(new_storm_data, MARGIN = 1, function (row) {
        
        calc_exp <- function (ch) {
            if (ch == 'k') {
                return (1e3)
            } else {
                if (ch == 'm') {
                    return (1e6)
                } else {
                    if (ch == 'b') {
                        return (1e9)
                    } else {
                        return (0)
                    }
                }
            }
        }
        calc_origin <- function (num) {
            if (!is.na(as.numeric(num))) {
                return (as.numeric(num))
            }
            return (0)
        }
        
        economic_damage <- calc_origin(row[['PROPDMG']]) * calc_exp(row[['PROPDMGEXP']]) +
                           calc_origin(row[['CROPDMG']]) * calc_exp(row[['CROPDMGEXP']])
        
        data.frame(event_type = row[['event_type']], damage = economic_damage)
    }) %>% 
        dplyr::bind_rows()
)
