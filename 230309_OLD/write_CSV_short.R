##### Data Quality HN Core Data Registry 2.0 #####

# analysis id --------- CHANGE THE ANALYSIS ID HERE
id_analysis <- "TEST"

# run id ------ CHANGE THE RUN ID HERE
run_id <- paste0("TEST", format(Sys.Date(), "%y%m%d"),"_3")

# repeatable events -------- CHANGE THE REPEATABLE EVENTS TO INCLUDE IN YOUR ANALYSIS
# AE: adverse events
# IM: imaging
# TS: tumor specimen
rep_events <- data.frame("AE" = FALSE, "IM" = FALSE, "TS" = FALSE)

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
library(dplyr)

#### FUNCTIONS ####
# get only completed instruments (non repeatable instruments)
getComplete <- function(data, ps_pass, formStr){
  form_complete <- paste0(formStr, "_complete")
  data <- data %>% filter(a01_id %in% ps_pass$a01_id) 
  data[,form_complete] = ps_pass[ps_pass$redcap_repeat_instrument == "",form_complete] 
  
  for (i in 1:nrow(data)) {
    if (data[i,ncol(data)] != 1 | is.na(data[i,ncol(data)])){
      data[i,2:(ncol(data)-1)] = NA
    }
  }
  
  return(data)
  
}

# get horizontal data
horizontal_data <- function(data_non_rep, data_rep){
  # orizzontalizzazione dei dati degli eventi ripetibili:
  # 1 - rinomino le variabili degli eventi ripetibili inserendo il numero di instance nel nome e metto tutto su una riga
  # 2 - match dei pazienti con dati evento base e porto tutti i dati di un paziente su un'unica riga
  data_final <- data.frame()
  for (i in 1:nrow(data_non_rep)) {
    data_id_hor <- data_non_rep[i,]
    data_id <- data_rep %>% subset(a01_id == data_non_rep$a01_id[i])
    if(!plyr::empty(data_id)){
      for (j in 1:nrow(data_id)) {
        instance <- data_id$redcap_repeat_instance[j]
        data_tmp <- data_id[j,] %>% select(-c(1:4))
        colnames(data_tmp) <- ifelse(grepl("\\d", colnames(data_tmp)), paste0(substring(colnames(data_tmp), first = 1, last = 1), "_",
                                                                              instance, "_",substring(colnames(data_tmp), first = 2)), 
                                     paste(instance, colnames(data_tmp), sep = "_"))
        data_id_hor <- cbind(data_id_hor, data_tmp)
      }
    }
    data_final <- plyr::rbind.fill(data_final, data_id_hor)
  }
  return(data_final)
}

#### MAIN ####

# read properties file
setwd('/opt/redcap_dq/environment/scripts')
file_prop <- "../config/config.properties"
properties <- properties::read.properties(file_prop)

# read the most recent PS-PASS file
file_name <- list.files(path = '../data', pattern = paste0(id_analysis, "-pset-pass"))
ps_pass <- read.csv(paste0('../data/',tail(file_name, n = 1)))

# get patients' ids filtered by tumor site
pts_id <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id, d11_siterare, d11_sitecomrar",
  filter_logic = '[non_repeatable_arm_1][d11_siterare] < "7" AND [non_repeatable_arm_1][d11_sitecomrar] < "12"'
)$data$a01_id

# extract data from REDCap -------- non repatable instruments
data_preliminary <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  forms_collapsed = "preliminary",
  events_collapsed = "non_repeatable_arm_1",
  records_collapsed = paste(pts_id, collapse = ","))$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_demo <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "demographic_life_style",
  events_collapsed = "non_repeatable_arm_1",
  records_collapsed = paste(pts_id, collapse = ","))$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_gensyndrome <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "previous_cancer_gensyndrome",
  events_collapsed = "non_repeatable_arm_1",
  records_collapsed = paste(pts_id, collapse = ","))$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_cancer <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "cancer_under_study",
  events_collapsed = "non_repeatable_arm_1",
  records_collapsed = paste(pts_id, collapse = ","))$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_stage <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "staging_procedures_and_stage",
  events_collapsed = "non_repeatable_arm_1",
  records_collapsed = paste(pts_id, collapse = ","))$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_cancer_tr <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "cancer_under_study_treatment",
  events_collapsed = "non_repeatable_arm_1",
  records_collapsed = paste(pts_id, collapse = ","))$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr1 <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease",
  # forms_collapsed = "progressionrecurrencepersistent_disease_1",
  events_collapsed = "non_repeatable_arm_1",
  records_collapsed = paste(pts_id, collapse = ","))$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_fup <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "status_of_patient_at_fup",
  events_collapsed = "non_repeatable_arm_1",
  records_collapsed = paste(pts_id, collapse = ","))$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

# estraggo dati da REDCap -------- strumenti ripetibili (Adverse events, Imaging available, Tumor specimen available)
if(rep_events$AE){
  data_ae <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri, 
    token = properties$token,
    fields_collapsed = "a01_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance",
    forms_collapsed = "adverse_events",
    events_collapsed = "repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data 
}

if(rep_events$IM){
  data_imaging <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri, 
    token = properties$token,
    fields_collapsed = "a01_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance",
    forms_collapsed = "imaging_available",
    events_collapsed = "repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data 
}
 
if(rep_events$TS){
  data_mat <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri, 
    token = properties$token,
    fields_collapsed = "a01_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance",
    forms_collapsed = "tumor_specimen_available",
    events_collapsed = "repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data
}

################################ SELECT FROM PS-PASS ################################ 
# get only instruments (non repeatable) with status = complete
data_preliminary <- getComplete(data_preliminary[,-ncol(data_preliminary)], ps_pass, "a07")
data_demo <- getComplete(data_demo[,-ncol(data_demo)], ps_pass, "b24")
data_gensyndrome <- getComplete(data_gensyndrome[,-ncol(data_gensyndrome)], ps_pass, "c28")
data_cancer <- getComplete(data_cancer[,-ncol(data_cancer)], ps_pass, "d34")
data_stage <- getComplete(data_stage[,-ncol(data_stage)], ps_pass, "e57")
data_cancer_tr <- getComplete(data_cancer_tr[,-ncol(data_cancer_tr)], ps_pass, "f107")
data_progr1 <- getComplete(data_progr1[,-ncol(data_progr1)], ps_pass, "g1_119")
data_fup <- getComplete(data_fup[,-ncol(data_fup)], ps_pass, "h06")

data_non_rep <- cbind(data_preliminary, data_demo[,-1], data_gensyndrome[,-1], data_cancer[,-1],
                      data_stage[,-1], data_cancer_tr[,-1], data_progr1[,-1], data_fup[,-1])

# if TRUE get adverse event instruments with status = complete
if(rep_events$AE){
  data_ae <- data_ae %>% filter(!(is.na(prodlim::row.match(data_ae[,1:4], ps_pass[which(ps_pass$i07_complete == 1),1:4]))))
}

if(rep_events$IM){
  data_imaging <- data_imaging %>% filter(!(is.na(prodlim::row.match(data_imaging[,1:4], ps_pass[which(ps_pass$j06_complete == 1),1:4]))))
}

if(rep_events$TS){
  data_mat <- data_mat %>% filter(!(is.na(prodlim::row.match(data_mat[,1:4], ps_pass[which(ps_pass$k06_complete == 1),1:4]))))
}

################################ CSV ################################ 
# apply horizontalization function if TRUE
if(rep_events$AE){
  data_final <- horizontal_data(data_non_rep, data_ae)
} else {
  data_final <- data_non_rep
}

if(rep_events$IM){
  data_final <- horizontal_data(data_final, data_imaging)
}

if(rep_events$TS){
  data_final <- horizontal_data(data_final, data_mat)
}

# Get list of patients with core variables complete
ps_core <- ps_pass %>%
  filter(core == 1) %>%
  select("a01_id") %>%
  unique(.)

# Filter data final
data_final <- data_final %>%
  mutate(hospital_id = properties$id_centro) %>%
  # add "age" variable
  mutate(age = ifelse(!is.na(d03_diagage_au), d03_diagage_au, d04_diagage_man)) %>%
  # add "tumor_site" variable
  mutate(site = ifelse(!is.na(d11_siterare), d11_siterare, d11_sitecomrar)) %>%
  select(c("a01_id","hospital_id", "age", "site"), everything()) %>%
  # inclusion criteria
  filter(a04_consent %in% c(1,3)) %>% #informed consent signed for clinical data
  filter(d03_diagage_au >= 18 | d04_diagage_man >= 18) %>% #age >= 18
  filter(as.integer(format(d01_diagdate, "%Y")) >= 2018) %>% #year of diagnosis >= 2018
  filter(a01_id %in% ps_core$a01_id) %>%  #core variables completed
  filter(!(is.na(d11_siterare) & is.na(d11_sitecomrar))) %>% #d11siterare and d11_sitecomerar not blank
  select(- grep("_complete", colnames(.), value = TRUE))

# write CSV 
file_name <- paste0(properties$id_centro,'-', run_id,'-DATA-csv-', format(Sys.Date(), "%y%m%d"), '.csv')
write.csv(data_final, file_name, na = "", row.names = F)

# Copy in /opt/redcap_dq/environment/data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data", overwrite = TRUE)

# Copy to /data
file.copy(from = paste0("/opt/redcap_dq/environment/data/", file_name), "/data", overwrite = TRUE)
file.rename(from = paste0("/data/", file_name), to = "/data/default.csv")
system("VTG_DATA_DIR=/data; 
       chmod 766 $VTG_DATA_DIR/default.csv")

# write JSON to /data
json_data <- paste0('{
  "original_name": "', file_name, '",
  "run_id": "', run_id, '",
  "copied_on": "', format(Sys.Date(), "%Y-%m-%d"),'"\n}')

json_file <- "data_version.json"
write(json_data, paste0('/data/', json_file))
system("VTG_DATA_DIR=/data; 
       chmod 766 $VTG_DATA_DIR/data_version.json")
       
# Copy in /vantage6-starter_head_and_neck-user-vol/_data
system("echo \'datafile=\"/data/default.csv\"; if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data; fi' | bash")
system("echo \'datafile=\"/data/data_version.json\"; if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data; fi' | bash")