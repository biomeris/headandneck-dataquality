##### Data Quality HN Core Data Registry 2.0 #####

############### DEFINE THE SITE ID(S) AND THE RUN_ID YOU WANT TO DELETE FROM THE HISTORY FILE #############
# Site IDs 
siteIDs <- c(19, 28, 32, 48, 29, 49)

# run ID(s)
run_id <- c("TEST_FULL240724_3")

#######################################################################################################################

####### *********** DO NOT CHANGE THE UNDERLYING CODE *********** #######

#### MAIN ####
# read properties file
file_prop <- "/opt/redcap_dq/environment/config/config.properties"
properties <- properties::read.properties(file_prop)

if(substring(properties$uri, first = nchar(properties$uri), last = nchar(properties$uri)) != "/") {
  properties$uri <- paste0(properties$uri, "/")
}

# Stop if id_centro is not in siteIDs
if (!properties$id_centro %in% siteIDs) {
  stop()
}

# Read list of script previously run
file_scripts <- "/opt/redcap_dq/environment/logs/history.txt"
list_of_scripts <- readtext::readtext(file_scripts)
history <- unlist(strsplit(list_of_scripts$text, split = "\n"))

# Remove the specified ID analysis from the history
history_new <- history[!history %in% run_id]

# Overwrite history file
if(length(history_new) != length(history)){
  sink(file_scripts)
  for (i in history_new) {
    cat(i, file = file_scripts, sep = "\n", append = TRUE)
  }
  sink()
}