##### Data Quality HN Core Data Registry 2.0 #####

# analysis id --------- CHANGE THE ANALYSIS ID HERE
id_analysis <- "TEST"

# run id ------ CHANGE THE RUN ID HERE
run_id <- paste0("TEST", format(Sys.Date(), "%y%m%d"),"_2")

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

#### FUNCTIONS ####

## function that produces the df to write in the individual output file 
summary.rules <- function(data, method = 'count', decreasing = TRUE){
  ## METHOD:
  #  'count' : conteggio dei check FAIL per ogni regola 
  #  'patients' : lista di ogni regola (per cui c'? almeno un FAIL) e pazienti su ogni colonna (con una X se per quel paziente il check ? FAIL) 
  
  ## DECREASING:
  #  se TRUE, ordina le regole in ordine decrescente (da quella con pi? FAIL a quella con meno FAIL)
  
  check_rules <- plyr::count(data, c("category","description"))
  
  if(decreasing)
    check_rules <- check_rules %>% arrange(desc(freq))
  
  if(method == 'count'){
    check_rules <- check_rules %>%
      mutate(perc = formattable::percent(freq/nrow(data))) %>% # aggiungo percentuale sul totale degli errori
      bind_rows(dplyr::summarise(.,
                                 across(where(is.numeric), sum),
                                 across(where(is.character), ~"Total")))
  } else if (method == 'patients'){
    patients <- unique(data[,1]) # lista dei pazienti
    for(i in 1:length(patients)){
      checks <- c() # vettore dei check per ogni paziente
      for (j in 1:nrow(check_rules)) {
        # controllo se c'è la coppia paziente-check nel dataframe con i check FAIL: se c'è (paziente non supera il check) metto 1, altrimenti 0 (il paziente ha superato il check)
        checks[j] <- nrow(plyr::match_df(data[,c(1,3)], data.frame("a01_id" = patients[i], "description" = check_rules[j,2])))
      }
      check_rules[as.character(patients[i])] <- as.vector(matrix(checks)) #aggiungo al df colonna con check per ogni paziente 
    }
    # totali di colonna (totale per ogni paziente + totale check)
    check_rules <- check_rules %>%
      bind_rows(dplyr::summarise(.,
                                 across(where(is.numeric), sum),
                                 across(where(is.character), ~"Total")))
    # ordino pazienti in ordine decrescente di numero di errori e metto colonna freq in ultima posizione
    check_rules <- check_rules[,order(as.numeric(check_rules[nrow(check_rules),]), decreasing = T)] %>%
      select(c(category, description), everything()) %>%
      select(-freq, everything())
    # sostituisco 0 con NA
    check_rules[check_rules == 0] <- NA
  } else {
    stop("Please enter a valid method for this function")
  }
  return(check_rules)
}

## function to write the legend for each check 
getLegend <- function(data){
  legend <- data.frame()
  checks <- unique(data[c("description")])
  for (i in 1:nrow(checks)) {
    legend_check <- data[which(data$description == checks$description[i]),][1,-1] %>% select(grep("instance",colnames(.), invert = TRUE))
    legend <- rbind.fill(legend, legend_check)
  }
  return(legend)
}

## Translate branching logic to logical expression
renderLogic <- function(br_logic_str){
  expr_str <- br_logic_str %>% gsub("(\\[[[:graph:]]+\\])(<>\\'\\')", '!is.na(\\1)', .) %>%
    gsub("(\\[[[:graph:]]+\\])(=\\'\\')", 'is.na(\\1)', .) %>% 
    gsub("(\\[[[:graph:]]+\\])(<>\\\"\\\")", '!is.na(\\1)', .) %>%
    gsub("(\\[[[:graph:]]+\\])(=\\\"\\\")", 'is.na(\\1)', .) %>% 
    gsub(' OR | or ', ' | ', .) %>%
    gsub(' AND | and ', ' & ', .) %>%
    gsub('=', '==', .) %>%
    gsub('\\[', '', .) %>%
    gsub('\\]', '', .) %>%
    gsub('<>', '!=', .) %>%
    gsub("(\\()([[:digit:]]+)(\\))", "___\\2",.) %>% 
    gsub("rounddown", "floor", .) %>% 
    gsub("datediff", "difftime", .) %>%
    gsub("(difftime\\([[:graph:]]+\\,[[:graph:]]+)(\\,[[:graph:]]+\\,[[:graph:]]+)(\\)\\))","\\1\\3", .)
  return(expr_str)
}

## Get difference between dates in months
getDiffMonths <- function(start_date, end_date){
  if (end_date >= start_date){
    date_seq = seq(from = start_date, to = end_date, by = 'month')
    diff_months = length(date_seq) - 1
  } else {
    diff_months = -1
  }
  return(diff_months)
}

## calcolo missing e missing + unknown
getMissUnk <- function(ps, var, check_summary){
  check_summary <- rbind.fill(check_summary, data.frame("var" = var, "var_lab" = metadata$field_label[which(metadata$field_name == var)],
                                                        "form" = metadata$form_name[which(metadata$field_name == var)], "den" = nrow(ps),
                                                        "perc_miss" = nrow(filter(ps, is.na(get(var))))/nrow(ps),
                                                        "perc_miss_unk" = nrow(subset(ps, is.na(get(var)) | 
                                                                                        as.numeric(get(var)) == 999 | as.numeric(get(var)) == 9999))/nrow(ps)))
  return(check_summary)
}

## calcolo missing e missing + unknown checkbox
getMissUnkCheckbox <- function(ps_tmp, var_patt, check_summary){
  ps_tmp <- ps_tmp[, grep(pattern = var_patt, colnames(ps_tmp))] %>% cbind(., "sum" = rowSums(.))
  ind_unk <- grep(pattern = "999", colnames(ps_tmp))
  check_summary <- rbind.fill(check_summary, data.frame("var" = var_patt, "var_lab" = metadata$field_label[which(metadata$field_name == var_patt)],
                                                        "form" = metadata$form_name[which(metadata$field_name == var_patt)], "den" = nrow(ps_tmp),
                                                        "perc_miss" = nrow(filter(ps_tmp, sum == 0))/nrow(ps_tmp),
                                                        "perc_miss_unk" = ifelse(length(ind_unk) == 0,
                                                                                 nrow(filter(ps_tmp, sum == 0))/nrow(ps_tmp),
                                                                                 (nrow(filter(ps_tmp, sum == 0)) + sum(ps_tmp[,ind_unk]))/nrow(ps_tmp))))
  return(check_summary)
}

#### MAIN ####

# read properties file
setwd('/opt/redcap_dq/environment/scripts')
file_prop <- "../config/config.properties"
properties <- properties::read.properties(file_prop)

# read PS-CTRL file
file_name <- list.files(path = '../data', pattern = paste0(id_analysis, "-pset-ctrl"))
ps_ctrl <- read.csv(paste0('../data/',tail(file_name, n = 1)))

# get patients' ids filtered by tumor site
pts_id <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id, d11_siterare, d11_sitecomrar",
  filter_logic = '[non_repeatable_arm_1][d11_siterare] < "7" AND [non_repeatable_arm_1][d11_sitecomrar] < "12"'
)$data$a01_id

# extract data from REDCap
redcap_data_new <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  records_collapsed = paste(pts_id, collapse = ",")
  )$data

# extract metadata from REDCap
metadata <- redcap_metadata_read(
  redcap_uri = properties$uri,
  token = properties$token)$data

############################################## SELECT PS-CTRL ############################################## 
## New dataframe with new data (corrected) but old completeness status (from ps_ctrl)
# Exclude new repeatable events for old patients and all events for new patients
redcap_data <- redcap_data_new[prodlim::row.match(replace(ps_ctrl[,1:4],is.na(ps_ctrl[,1:4]), ""), replace(redcap_data_new[,1:4],is.na(redcap_data_new[,1:4]), "")),]

# Replace new completeness status with old completeness status
redcap_data[,c("a07_complete", "b24_complete","c28_complete", "d34_complete", "e57_complete", "f107_complete",
        grep("g[[:digit:]]+_119_complete", colnames(redcap_data), value = TRUE), "h06_complete", "i07_complete")] <- ps_ctrl[,c("a07_complete", "b24_complete","c28_complete", "d34_complete", "e57_complete", "f107_complete",
                                                                                                                                                        grep("g[[:digit:]]+_119_complete", colnames(ps_ctrl), value = TRUE), "h06_complete", "i07_complete")]

############################################## QC REPORT INDIVIDUAL ##############################################
### Rule #1 --------------------------------------------------------------------
# Cancer phase managed by the hospital (a06_phase): at least one flag has to be present

data_rule1 <- ddply(subset(redcap_data, a07_complete == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Cancer phase managed by the hospital is not specified",
                    form1 = "Preliminary",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = gsub(" \\(.*?\\)","",gsub("<.*?>","",metadata$field_label[which(metadata$field_name == "a06_phase")])),
                    status = ifelse(a06_phase___1 != 0 | a06_phase___2 != 0 | a06_phase___3 != 0|
                                      a06_phase___4 != 0 | a06_phase___5 != 0 | a06_phase___6 != 0,'PASS','FAIL'))


### Rule #2 --------------------------------------------------------------------
# Date of birth (b01_birdate) prior (<) to date of first contact with the hospital (a05_date)

data_rule2 <- ddply(subset(redcap_data, b24_complete	 == 1 & a07_complete == 1 &
                             !is.na(b01_birdate) & !is.na(a05_date)), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Date of birth is not prior to first contact with the hospital",
                    form1 = "Demographic & life style",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "b01_birdate")],
                    value1 = b01_birdate,
                    form2 = "Preliminary",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "a05_date")],
                    value2 = ifelse(!is.na(a05_date_unk) & format(a05_date, "%d") == "15", a05_date - 14, a05_date),
                    status = ifelse(value1 < value2,'PASS','FAIL'))

# join data and select only failed check ---------------------------------------

data_rules <- ls(all.names = FALSE, pattern = "data_rule") # list of data_rule*
data_rules <- data_rules[order(nchar(data_rules), data_rules)] # sort in sequential order
data_rules <- mget(data_rules) # create a list
data_rules <- Filter(function(x) dim(x)[1] > 0, data_rules) # remove empty dataframes

check_failed <- do.call(rbind.fill, data_rules) %>% select(- status, everything()) %>% 
  subset(status == 'FAIL') %>% select(-c(grep("value", colnames(.)), status))

# write and save individual report ---------------------------------------------
# create workbook
wb <- createWorkbook()

# add worksheet
addWorksheet(wb, "Checks")
addWorksheet(wb, "Legend")

if(!(empty(check_failed))){
  # if not empty build new dataframe
  err_regole <- summary.rules(check_failed, method = "patients")
  
  # set new columns names
  data.table::setnames(err_regole, old = c("category","description", "freq"), new = c("Rule category", "Rule description", "Total"))
  
  # get checks legend
  legend <- getLegend(check_failed)
  columnNames <- colnames(legend) %>% 
    gsub("([[:alpha:]]+)([[:digit:]])", "\\1 \\2", .) %>% 
    gsub("var", "Variable", .) %>%
    gsub("event", "Event", .) %>% 
    gsub("form", "Form", .) %>% 
    gsub("category", "Rule category", .) %>%
    gsub("description", "Rule description", .)
  colnames(legend) <- columnNames
  
  # write data on worksheet
  writeData(wb,sheet = "Checks", err_regole, borders = "all", borderColour = "#999999",
            headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))                             
  setColWidths(wb,sheet = "Checks",cols = 1:ncol(err_regole),widths = "auto")
  
  writeData(wb,sheet = "Legend", legend, borders = "all", borderColour = "#999999",
            headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))                             
  setColWidths(wb,sheet = "Legend",cols = 1:ncol(legend),widths = "auto")
} else {
  # if empty write "No errors found" on the excel file
  writeData(wb,sheet = "Checks", "No errors found") 
}

# write individual report
file_name <- paste0(properties$id_centro,'-',run_id,'-QC-individual-', format(Sys.Date(), "%y%m%d"), '.xlsx')

# save to xlsx
saveWorkbook(wb, file_name, overwrite = TRUE)

# Copy in /opt/redcap_dq/environment/data/
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data", overwrite = TRUE)

# Copy in /vantage6-starter_head_and_neck-user-vol/_data and in /data
system(paste0("echo \'datafile=\"/opt/redcap_dq/environment/data/", file_name,"\";
if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then
      docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data/", run_id, "-QC-individual.xlsx
fi;
cp $datafile /data' | bash"))

############################################## QC REPORT SUMMARY ##############################################
# Missing and unknown ---------
# create a new dataframe to count % of missing and % of missing+unknown
check_summary <- data.frame("var" = character(0), "var_lab" = character(0), "form" = character(0), "den" = integer(0), "perc_miss" = double(0), "perc_miss_unk" = double(0))

## Select variables with field type != descriptive, calculated or checkbox
metadata_tmp <- subset(metadata,!(field_type %in% c("descriptive","calc", "checkbox"))) %>%
  # tolgo anche a01_id e tutte quelle del tipo *_dayunk
  filter(!(field_name %in% c("a01_id", grep("(date)([[:graph:]]*)(_unk)", metadata$field_name, value = TRUE)))) %>%
  slice(1:3)


for (i in 1:nrow(metadata_tmp)){
  var_tmp = metadata_tmp$field_name[i]
  form_complete = ifelse(substring(var_tmp,1,1) == "g",
                         grep(paste0(substring(var_tmp,1,2),"_[[:digit:]]+_complete"), metadata$field_name, value = TRUE),
                         grep(paste0(substring(var_tmp,1,1),"[[:digit:]]+_complete"), metadata$field_name, value = TRUE))
  expr_str = renderLogic(metadata_tmp$branching_logic[i])
  if(!is.na(form_complete)){
    if(is.na(expr_str))
      ps_tmp <- subset(redcap_data, get(form_complete) == 1)
    else
      ps_tmp <- subset(redcap_data, get(form_complete) == 1 & eval(parse(text = expr_str)))
  } else {
    event <- case_when(substring(var_tmp,1,1) == "j" ~ "imaging_available",
                       substring(var_tmp,1,1) == "k" ~ "tumor_specimen_available",
                       substring(var_tmp,1,1) == "l" ~ "gene_test_expression_analysis")

    if(is.na(expr_str))
      ps_tmp <- subset(redcap_data, redcap_repeat_instrument == event)
    else
      ps_tmp <- subset(redcap_data, redcap_repeat_instrument == event & eval(parse(text = expr_str)))
  }

  if(nrow(ps_tmp) != 0){
    check_summary <- getMissUnk(ps_tmp, var_tmp, check_summary)
  }
  
  gc()
}

# Sort in alphabetic order and change columns names ----
check_summary <- check_summary[gtools::mixedorder(check_summary$var),] %>%
  mutate(perc_miss = formattable::percent(perc_miss)) %>%
  mutate(perc_miss_unk = formattable::percent(perc_miss_unk)) %>%
  mutate(form = case_when(form == "preliminary" ~ "Preliminary",
                          form == "demographic_life_style" ~ "Demographic & life style",
                          form == "previous_cancer_gensyndrome" ~ "Previous cancer gen.syndrome",
                          form == "cancer_under_study" ~ "Cancer under study",
                          form == "staging_procedures_and_stage" ~ "Staging procedures and stage",
                          form == "cancer_under_study_treatment" ~ "Cancer Under Study Treatment",
                          form %in% grep("progressionrecurrencepersistent_disease", unique(metadata$form_name), value = TRUE) ~ "Progression/Recurrence/Persistent disease",
                          form == "status_of_patient_at_fup" ~ "Status of patient at fup",
                          form == "adverse_events" ~ "Adverse events",
                          form == "imaging_available" ~ "Imaging available",
                          form == "tumor_specimen_available" ~ "Tumor specimen available",
                          form == "gene_test_expression_analysis" ~ "Gene test expression analysis")) %>%
  select(-c("var")) %>%
  mutate(var_lab = gsub("<.*?>", "", var_lab)) %>%
  filter(var_lab != "Instrument status:")

colnames(check_summary) <- c("Variable", "Form", "Denominator","% missing", "% missing + unknown")

# write summary report
file_name <- paste0(properties$id_centro,'-',run_id,'-QC-summary-', format(Sys.Date(), "%y%m%d"), '.xlsx')

# create workbook
wb <- createWorkbook()

# add worksheet
addWorksheet(wb, "Missing & Unknown")

# write data on worksheet
# Missing & Unknown
writeData(wb,sheet = "Missing & Unknown", check_summary, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
setColWidths(wb,sheet = "Missing & Unknown",cols = 1:ncol(check_summary), widths = "auto")

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

############################################## PS- PASS ############################################## 
# Salvo la lista dei pazienti che hanno passato i check in un csv (elimino tutti i pazienti che hanno almeno un check FAIL)
ps_pass <- redcap_data %>%
  filter(!(a01_id %in% check_failed$a01_id)) %>% #patients without errors
  mutate(core = 1) %>%
  select(c("a01_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", "a07_complete", "b24_complete","c28_complete",
           "d34_complete", "e57_complete", "f107_complete", grep("g[[:digit:]]+_119_complete", colnames(redcap_data), value = TRUE), "h06_complete", "i07_complete", "core")) #save completeness status and core

file_name <- paste0(properties$id_centro,'-',id_analysis,'-pset-pass-', format(Sys.Date(), "%y%m%d"), '.csv')
write.csv(ps_pass, file_name, row.names = FALSE, na = "")

# Copy in /opt/redcap_dq/environment/data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data", overwrite = TRUE)