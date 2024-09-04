##### Data Quality HN Core Data Registry 2.0 #####

# Nome file originale: DQrules_1.R

# analysis id --------- CHANGE THE ANALYSIS ID HERE
id_analysis <- "TEST"

# run id ------ CHANGE THE RUN ID HERE
run_id <- paste0("TEST_HUM", format(Sys.Date(), "%y%m%d"),"_1")

# id_centro IEO
id_ieo <- 19

# read properties file
file_prop <- "/opt/redcap_dq/environment/config/config.properties"
properties <- properties::read.properties(file_prop)

# Stop if id_centro is not IEO
if (properties$id_centro != id_ieo) {
  stop()
}

# Read list of script previously run
file_scripts <- "/opt/redcap_dq/environment/logs/history.txt"
list_of_scripts <- readtext::readtext(file_scripts)

if (length(grep(run_id, list_of_scripts$text)) != 0) {
  message("You have already run this script")
  opt <- options(show.error.messages=FALSE)
  on.exit(options(opt))
  stop()
}

##### If not stopped run the code
# Add this script to the list of scripts previously run
sink(file_scripts, append = TRUE)
cat(run_id, file = file_scripts, sep = "\n", append = TRUE)
sink()

# Write fake DQ reports
library(openxlsx)

# Individual report
wb <- createWorkbook()
addWorksheet(wb, "Checks")
writeData(wb,sheet = "Checks", iris, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold")) 

# write individual report
file_name <- paste0(properties$id_centro,'-',run_id,'-QC-individual-', format(Sys.Date(), "%y%m%d"), '.xlsx')

# save to xlsx
saveWorkbook(wb, file_name, overwrite = TRUE)

# Copy in /opt/redcap_dq/environment/data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data", overwrite = TRUE)

# Copy in /vantage6-starter_head_and_neck-user-vol/_data and in /data
system(paste0("echo \'datafile=\"/opt/redcap_dq/environment/data/", file_name,"\";
if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then
      docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data/", run_id, "-QC-individual.xlsx
fi;
cp $datafile /data' | bash"))

# Summary report
wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb,sheet = "Summary", cars, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold")) 

# write summary report
file_name <- paste0(properties$id_centro,'-',run_id,'-QC-summary-', format(Sys.Date(), "%y%m%d"), '.xlsx')

# save to xlsx
saveWorkbook(wb, file_name, overwrite = TRUE)

# Copy in /opt/redcap_dq/environment/data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data", overwrite = TRUE)

# Copy in /vantage6-starter_head_and_neck-user-vol/_data and in /data
system(paste0("echo \'datafile=\"/opt/redcap_dq/environment/data/", file_name,"\";
if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then
      docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data/", run_id, "-QC-summary.xlsx
fi;
cp $datafile /data' | bash"))