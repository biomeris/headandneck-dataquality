##### Data Quality HN Core Data Registry 2.0 #####

# Nome file originale: Summary_Tables.R

# analysis id --------- CHANGE THE ANALYSIS ID HERE
id_analysis <- "TEST"

# run id ------ CHANGE THE RUN ID HERE
run_id <- paste0("Summary_tables", format(Sys.Date(), "%y%m%d"))

# id_centro AIOCC
id_aiocc <- 49

#######################################################################################################################

####### *********** DO NOT CHANGE THE UNDERLYING CODE *********** #######

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

# load R packages
library(REDCapR)
library(plyr)
library(dplyr)
library(openxlsx)

#### FUNCTIONS #####
# create table summary "By hospital all years"
createTableSummary1 <- function(redcap_data) {
  count_histo_x_site <- redcap_data %>%
    filter(site %in% c("Nasal cavity and paranasal sinuses", "Nasopharynx"))%>%
    group_by(site, d05_histo) %>%
    count()
  
  count_site <- redcap_data %>%
    filter(site %in% c("Nasal cavity and paranasal sinuses", "Nasopharynx")) %>%
    group_by(site) %>%
    count()
  
  tableSummary1 <- setNames(data.frame(matrix(ncol = 7, nrow = 8), row.names = c("Nasal cavity and paranasal sinuses", "Nasopharynx", "Salivary glands", "Major salivary gland", "Minor salivary gland", "Neuroendocrine", "Odontogenic carcinoma", "Total")),
                          c("Total", "Adenocarcinoma", "Squamous", "NUT carcinoma", "Sinonasal Undifferentiated Carcinoma (SNUC)", "Carcinoma/Carcinoma undifferentiated", "Adenosquamous carcinoma"))
  
  if(nrow(count_histo_x_site)) {
    for (i in 1:nrow(count_histo_x_site)) {
      if(count_histo_x_site$d05_histo[i] %in% colnames(tableSummary1)) {
        tableSummary1[count_histo_x_site$site[i], count_histo_x_site$d05_histo[i]] <- count_histo_x_site$n[i]
      }
    }
  }
  
  if(nrow(count_site)) {
    for (i in 1:nrow(count_site)) {
      tableSummary1[count_site$site[i], "Total"] <- count_site$n[i]
    }
  }
  
  tableSummary1["Salivary glands", "Total"] <- redcap_data %>%
    filter(d05_histo == "Adenocarcinoma") %>%
    nrow()
  
  tableSummary1["Major salivary gland", "Total"] <- redcap_data %>%
    filter(d05_histo == "Adenocarcinoma") %>%
    filter(site %in% c("Parotid gland", "Submandibular gland", "Sublingual gland")) %>%
    nrow()
  
  tableSummary1["Minor salivary gland", "Total"] <- redcap_data %>%
    filter(d05_histo == "Adenocarcinoma") %>%
    filter(!(site %in% c("Parotid gland", "Submandibular gland", "Sublingual gland"))) %>%
    nrow()
  
  tableSummary1["Neuroendocrine", "Total"] <- redcap_data %>%
    filter(d05_histo == "Neuroendocrine") %>%
    nrow()
  
  tableSummary1["Odontogenic carcinoma", "Total"] <- redcap_data %>%
    filter(d05_histo == "Odontogenic carcinoma") %>%
    filter(site %in% c("Nasal cavity and paranasal sinuses", "Nasopharynx", "Oral cavity")) %>%
    nrow()
  
  tableSummary1[1:2, ] <- replace(tableSummary1[1:2, ], is.na(tableSummary1[1:2, ]), 0)
  
  tableSummary1["Total", "Total"] <- sum(tableSummary1["Nasal cavity and paranasal sinuses", "Total"], tableSummary1["Nasopharynx", "Total"]) %>%
    sum(., redcap_data %>%
          filter(d05_histo == "Adenocarcinoma") %>%
          filter(!(site %in% c("Nasal cavity and paranasal sinuses", "Nasopharynx"))) %>%
          nrow()) %>%
    sum(., redcap_data %>%
          filter(d05_histo == "Neuroendocrine") %>%
          filter(!(site %in% c("Nasal cavity and paranasal sinuses", "Nasopharynx"))) %>%
          nrow()) %>%
    sum(., redcap_data %>%
          filter(d05_histo == "Odontogenic carcinoma") %>%
          filter(site %in% c("Oral cavity")) %>%
          nrow())
  
  return(tableSummary1)
}

# create table summary "By hospital and year"
createTableSummary2 <- function(redcap_data)  {
  tableSummary2 <- setNames(data.frame(matrix(ncol = as.numeric(format(Sys.Date(), "%Y")) - 2017, nrow = 8), row.names = c("Nasal cavity and paranasal sinuses", "Nasopharynx", "Salivary glands", "Major salivary gland", "Minor salivary gland", "Neuroendocrine", "Odontogenic carcinoma", "Total")),
                            c(2018:as.numeric(format(Sys.Date(), "%Y"))))
  
  for (i in colnames(tableSummary2)) {
    tableSummary2["Nasal cavity and paranasal sinuses", i] <- redcap_data %>%
      filter(year_diag == i & site == "Nasal cavity and paranasal sinuses") %>%
      nrow()
    
    tableSummary2["Nasopharynx", i] <- redcap_data %>%
      filter(year_diag == i & site == "Nasopharynx") %>%
      nrow()
    
    tableSummary2["Salivary glands", i] <- redcap_data %>%
      filter(year_diag == i & d05_histo == "Adenocarcinoma") %>%
      nrow()
    
    tableSummary2["Major salivary gland", i] <- redcap_data %>%
      filter(year_diag == i & d05_histo == "Adenocarcinoma") %>%
      filter(site %in% c("Parotid gland", "Submandibular gland", "Sublingual gland")) %>%
      nrow()
    
    tableSummary2["Minor salivary gland", i] <- redcap_data %>%
      filter(year_diag == i & d05_histo == "Adenocarcinoma") %>%
      filter(!(site %in% c("Parotid gland", "Submandibular gland", "Sublingual gland"))) %>%
      nrow()
    
    tableSummary2["Neuroendocrine", i] <- redcap_data %>%
      filter(year_diag == i & d05_histo == "Neuroendocrine") %>%
      nrow()
    
    tableSummary2["Odontogenic carcinoma", i] <- redcap_data %>%
      filter(year_diag == i & d05_histo == "Odontogenic carcinoma") %>%
      filter(site %in% c("Nasal cavity and paranasal sinuses", "Nasopharynx", "Oral cavity")) %>%
      nrow()
    
    tableSummary2["Total", i] <- sum(tableSummary2["Nasal cavity and paranasal sinuses", i], tableSummary2["Nasopharynx", i]) %>%
      sum(., redcap_data %>%
            filter(year_diag == i & d05_histo == "Adenocarcinoma") %>%
            filter(!(site %in% c("Nasal cavity and paranasal sinuses", "Nasopharynx"))) %>%
            nrow()) %>%
      sum(., redcap_data %>%
            filter(year_diag == i & d05_histo == "Neuroendocrine") %>%
            filter(!(site %in% c("Nasal cavity and paranasal sinuses", "Nasopharynx"))) %>%
            nrow()) %>%
      sum(., redcap_data %>%
            filter(year_diag == i & d05_histo == "Odontogenic carcinoma") %>%
            filter(site %in% c("Oral cavity")) %>%
            nrow())
  }
  
  return(tableSummary2)
}

#### MAIN ####

# read properties file
setwd('/opt/redcap_dq/environment/scripts')
file_prop <- "../config/config.properties"
properties <- properties::read.properties(file_prop)

if(substring(properties$uri, first = nchar(properties$uri), last = nchar(properties$uri)) != "/") {
  properties$uri <- paste0(properties$uri, "/")
}

# get patients' ids filtered by tumor site
pts_id <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id, d11_siterare, d11_sitecomrar",
  filter_logic = '([non_repeatable_arm_1][d11_siterare] < "6" AND [non_repeatable_arm_1][d11_siterare] <> "") OR ([non_repeatable_arm_1][d11_sitecomrar] < "11" AND [non_repeatable_arm_1][d11_sitecomrar] <> "")',
  export_data_access_groups = TRUE
)$data 

if(properties$id_centro == id_aiocc) {
  pt_dag <- pts_id %>%
    select(a01_id, redcap_data_access_group) 
}

pts_id <- pts_id$a01_id

# extract data from REDCap
redcap_data <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  records_collapsed = paste(pts_id, collapse = ","),
  fields_collapsed = "a01_id, d01_diagdate, d05_histo, d11_siterare, d11_sitecomrar",
  events_collapsed = "non_repeatable_arm_1",
  filter_logic = "[d34_complete] = 1",
  raw_or_label = "label",
  export_data_access_groups = TRUE)$data %>%
  mutate(site = ifelse(!is.na(d11_siterare), d11_siterare, d11_sitecomrar)) %>%
  mutate(year_diag = format(d01_diagdate, format = '%Y')) %>%
  filter(year_diag >= 2018) %>%
  select(-c(d01_diagdate, d11_siterare, d11_sitecomrar))

# create workbook
wb <- createWorkbook()

if(properties$id_centro == id_aiocc) {
  dags <- unique(pt_dag$redcap_data_access_group)
  
  for (j in dags) {
    
    redcap_data_j <- redcap_data %>%
      left_join(., pt_dag, by = "a01_id") %>%
      mutate(redcap_data_access_group = redcap_data_access_group.y) %>%
      select(-c(redcap_data_access_group.y, redcap_data_access_group.x)) %>%
      filter(redcap_data_access_group == j)
    
    if(nrow(redcap_data_j)) {
      # create table "By hospital all years"
      tableSummary1 <- createTableSummary1(redcap_data_j)
      # create table "By hospital and year"
      tableSummary2 <- createTableSummary2(redcap_data_j)
      
      # add worksheet
      addWorksheet(wb, paste0(toupper(j), "-Tables"))
      
      # Write tables 
      curr_row <- 1
      # Table "By hospital all years"
      writeData(wb, paste0(toupper(j), "-Tables"), "By hospital all years", startCol = 1, startRow = curr_row)
      addStyle(wb, sheet = paste0(toupper(j), "-Tables"), createStyle(textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#999999", fontSize = 12, fontColour = "#cc0000"), rows = curr_row, cols = 1)
      writeData(wb, paste0(toupper(j), "-Tables"), tableSummary1, startCol = 1, startRow = curr_row + 1, rowNames = TRUE, borders = "all", borderColour = "#999999",
                headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
      curr_row <- curr_row + nrow(tableSummary1) + 4
      # Table "By hospital and year"
      writeData(wb, paste0(toupper(j), "-Tables"), "By hospital and year", startCol = 1, startRow = curr_row)
      addStyle(wb, sheet = paste0(toupper(j), "-Tables"), createStyle(textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#999999", fontSize = 12, fontColour = "#cc0000"), rows = curr_row, cols = 1)
      writeData(wb, paste0(toupper(j), "-Tables"), tableSummary2, startCol = 1, startRow = curr_row + 1, rowNames = TRUE, borders = "all", borderColour = "#999999",
                headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
      
      setColWidths(wb,sheet = paste0(toupper(j), "-Tables"),cols = 1:(max(ncol(tableSummary1), ncol(tableSummary2)) + 1), widths = "auto")
    }
  }
} else {
  # create table "By hospital all years"
  tableSummary1 <- createTableSummary1(redcap_data)
  # create table "By hospital and year"
  tableSummary2 <- createTableSummary2(redcap_data)
  
  # add worksheet
  addWorksheet(wb, "Summary Tables")
  
  # Write tables 
  curr_row <- 1
  # Table "By hospital all years"
  writeData(wb, "Summary Tables", "By hospital all years", startCol = 1, startRow = curr_row)
  addStyle(wb, sheet = "Summary Tables", createStyle(textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#999999", fontSize = 12, fontColour = "#cc0000"), rows = curr_row, cols = 1)
  writeData(wb, "Summary Tables", tableSummary1, startCol = 1, startRow = curr_row + 1, rowNames = TRUE, borders = "all", borderColour = "#999999",
            headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
  curr_row <- curr_row + nrow(tableSummary1) + 4
  # Table "By hospital and year"
  writeData(wb, "Summary Tables", "By hospital and year", startCol = 1, startRow = curr_row)
  addStyle(wb, sheet = "Summary Tables", createStyle(textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#999999", fontSize = 12, fontColour = "#cc0000"), rows = curr_row, cols = 1)
  writeData(wb, "Summary Tables", tableSummary2, startCol = 1, startRow = curr_row + 1, rowNames = TRUE, borders = "all", borderColour = "#999999",
            headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
  
  setColWidths(wb,sheet = "Summary Tables",cols = 1:(max(ncol(tableSummary1), ncol(tableSummary2)) + 1), widths = "auto")
}

# write summary report
file_name <- paste0(properties$id_centro,'-',run_id,'-QC-summary-', format(Sys.Date(), "%y%m%d"), '.xlsx')

# save to xlsx
saveWorkbook(wb, file_name, overwrite = TRUE)

# Copy in /opt/redcap_dq/environment/data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data", overwrite = TRUE)

# Copy in /vantage6-starter_head_and_neck-user-vol/_data and in /data
# system(paste0("echo \'datafile=\"/opt/redcap_dq/environment/data/", file_name,"\";
# if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then
#       docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data/", run_id, "-QC-summary.xlsx
# fi;
# cp $datafile /data' | bash"))
file.copy(from = paste0("/opt/redcap_dq/environment/data/", file_name), "/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data", overwrite = TRUE)
file.rename(from = paste0("/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/", file_name), to = paste0("/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/", run_id, "-QC-summary.xlsx"))