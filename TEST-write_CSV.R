##### Data Quality HN Core Data Registry 2.0 #####

# Nome file originale: TEST-write_CSV.R

# analysis id --------- CHANGE THE ANALYSIS ID HERE
id_analysis <- "TEST"

# run id ------ CHANGE THE RUN ID HERE
run_id <- paste0("TEST_", format(Sys.Date(), "%y%m%d"),"_3")

# Node to exclude from the analyses
# If not AIOCC, use the node id (as a string, e.g., '28'), if AIOCC use node_id-unique_group_name (e.g., '49-int_milano')
exclude_from_analyses <- c()

# repeatable events -------- CHANGE THE REPEATABLE EVENTS TO INCLUDE IN YOUR ANALYSIS
# AE: adverse events
# IM: imaging
# TS: tumor specimen
rep_events <- data.frame("AE" = FALSE, "IM" = FALSE, "TS" = FALSE)

rep_events_max <- data.frame("AE" = 0, "IM" = 0, "TS" = 0)

#######################################################################################################################

####### *********** DO NOT CHANGE THE UNDERLYING CODE *********** #######

# id_centro AIOCC
id_aiocc <- 49

# read properties file
setwd('/opt/redcap_dq/environment/scripts')
file_prop <- "../config/config.properties"
properties <- properties::read.properties(file_prop)

if(substring(properties$uri, first = nchar(properties$uri), last = nchar(properties$uri)) != "/") {
  properties$uri <- paste0(properties$uri, "/")
}

# Stop execution if node is excluded from analyses
if (properties$id_centro %in% exclude_from_analyses) {
  message("Node excluded from analyses.")
  opt <- options(show.error.messages=FALSE)
  on.exit(options(opt))
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
        # colnames(data_tmp) <- paste(colnames(data_tmp), instance, sep = "_")
        data_id_hor <- cbind(data_id_hor, data_tmp)
      }
    }
    data_final <- plyr::rbind.fill(data_final, data_id_hor)
  }
  return(data_final)
}

#### MAIN ####

#extract metadata from REDCap
var_aiocc <- c("inf_consent_air_protocol","type_of_collection","ethnicity","info","tracheostomy","extent_of_pni","location_of_pni","largest_nerve_diameter","bone_invasion","depth_invasion","tumor_size_height","tumor_size_width",
               "tumor_size_depth","karnofsky_treat_1","karnofsky_treat_2","karnofsky_treat_3","other_specify","mean_parotid_dose",
               "mean_constrictor_dose","mean_oral_cavity_dose","mean_larynx_dose","mean_esophagus_dose","brachial_plexus_r_max_dose","brachial_plexus_l_max_dose",
               "mean_thyroid_dose", "mean_submandibular_r","mean_submandibular_l","spinal_cord_max_0_03_cc","spinal_cord_5_mm_ptv_0_03","brainstem_max_0_03_cc",  
               "brainstem_5_mm_prv_0_03_cc","mandible_max_0_03_cc","pituitary_mean","optic_chiasm_0_03_cc","optic_chiasm_5_mm_prv","optic_nerve_r_0_03_cc",  
               "optic_nerve_r_5_mm_prv","optic_nerve_l_0_03_cc","optic_nerve_l_5_mm_prv","temporal_lobe_r_0_03_cc","temporal_lobe_l_0_03_cc", "b20_comorb_nci")

metadata <- redcap_metadata_read(
  redcap_uri = properties$uri,
  token = properties$token)$data %>%
  filter(!(field_name %in% var_aiocc))

redcap_var <- redcap_variables(properties$uri, properties$token)$data %>%
  filter(!(original_field_name %in% var_aiocc)) %>%
  left_join(., metadata[,c("field_name", "form_name")], by = c('original_field_name' = 'field_name')) %>%
  rowwise() %>%
  mutate(form_name = ifelse(is.na(form_name), gsub("_complete", "", original_field_name), form_name))

# build final dataframe with fixed columns
var_non_rep <- redcap_var %>%
  filter(!(form_name %in% c("adverse_events", "imaging_available", "tumor_specimen_available", "gene_test_expression_analysis")))

dataframe_final <- setNames(data.frame(matrix(ncol = nrow(var_non_rep), nrow = 0)), var_non_rep$export_field_name)

if(rep_events$AE){
  var_ae <- redcap_var %>%
    filter(form_name %in% c("adverse_events"))
  
  for (i in 1:rep_events_max$AE) {
    colnames_df <- ifelse(grepl("\\d", var_ae$export_field_name), paste0(substring(var_ae$export_field_name, first = 1, last = 1), "_",
                                                                         i, "_",substring(var_ae$export_field_name, first = 2)), 
                          paste(i, var_ae$export_field_name, sep = "_"))
    dataframe_final <- cbind(dataframe_final, setNames(data.frame(matrix(ncol = length(colnames_df), nrow = 0)), colnames_df))
  }
}

if(rep_events$IM){
  var_im <- redcap_var %>%
    filter(form_name %in% c("imaging_available"))
  
  for (i in 1:rep_events_max$IM) {
    colnames_df <- ifelse(grepl("\\d", var_im$export_field_name), paste0(substring(var_im$export_field_name, first = 1, last = 1), "_",
                                                                         i, "_",substring(var_im$export_field_name, first = 2)), 
                          paste(i, var_im$export_field_name, sep = "_"))
    dataframe_final <- cbind(dataframe_final, setNames(data.frame(matrix(ncol = length(colnames_df), nrow = 0)), colnames_df))
  }
}

if(rep_events$TS){
  var_ts <- redcap_var %>%
    filter(form_name %in% c("tumor_specimen_available"))
  
  for (i in 1:rep_events_max$TS) {
    colnames_df <- ifelse(grepl("\\d", var_ts$export_field_name), paste0(substring(var_ts$export_field_name, first = 1, last = 1), "_",
                                                                         i, "_",substring(var_ts$export_field_name, first = 2)), 
                          paste(i, var_ts$export_field_name, sep = "_"))
    dataframe_final <- cbind(dataframe_final, setNames(data.frame(matrix(ncol = length(colnames_df), nrow = 0)), colnames_df))
  }
}

# read the most recent PS-PASS file
file_name <- list.files(path = '../data', pattern = paste0(id_analysis, "-pset-ctrl"))
ps_pass <- read.csv(paste0('../data/',tail(file_name, n = 1)))

# Check if there is at least one patient who passed all the checks --> if yes, extract data, otherwise create a txt output
if (nrow(ps_pass)) {
  # get patients' ids filtered by tumor site
  pts_id <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id, d11_siterare, d11_sitecomrar",
    filter_logic = '([non_repeatable_arm_1][d11_siterare] <= "6" AND [non_repeatable_arm_1][d11_siterare] <> "") OR ([non_repeatable_arm_1][d11_sitecomrar] <= "11" AND [non_repeatable_arm_1][d11_sitecomrar] <> "")',
    export_data_access_groups = TRUE
  )$data
  
  # get patients DAG
  if(properties$id_centro == id_aiocc) {
    pt_dag <- pts_id %>%
      select(a01_id, redcap_data_access_group)
  }
  
  pts_id <- pts_id$a01_id
  
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
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr2 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_a9bc",
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr3 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_1aa9",
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr4 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_d93f",
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr5 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_f74f",
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr6 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_3672",
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr7 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_1bf2",
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr8 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_aba2",
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr9 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_5f7c",
    events_collapsed = "non_repeatable_arm_1",
    records_collapsed = paste(pts_id, collapse = ","))$data %>%
    select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))
  
  data_progr10 <- redcap_read(
    batch_size = 10L,
    interbatch_delay = 0.5,
    redcap_uri = properties$uri,
    token = properties$token,
    fields_collapsed = "a01_id",
    forms_collapsed = "progressionrecurrencepersistent_disease_859b",
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
  
  # estraggo dati da REDCap -------- strumenti ripetibili (Adverse events, Imaging available, Tumor specimen available )
  if(rep_events$AE){
    data_ae <- redcap_read(
      batch_size = 10L,
      interbatch_delay = 0.5,
      redcap_uri = properties$uri, 
      token = properties$token,
      fields_collapsed = "a01_id",
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
      fields_collapsed = "a01_id",
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
      fields_collapsed = "a01_id",
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
  data_progr2 <- getComplete(data_progr2[,-ncol(data_progr2)], ps_pass, "g2_119")
  data_progr3 <- getComplete(data_progr3[,-ncol(data_progr3)], ps_pass, "g3_119")
  data_progr4 <- getComplete(data_progr4[,-ncol(data_progr4)], ps_pass, "g4_119")
  data_progr5 <- getComplete(data_progr5[,-ncol(data_progr5)], ps_pass, "g5_119")
  data_progr6 <- getComplete(data_progr6[,-ncol(data_progr6)], ps_pass, "g6_119")
  data_progr7 <- getComplete(data_progr7[,-ncol(data_progr7)], ps_pass, "g7_119")
  data_progr8 <- getComplete(data_progr8[,-ncol(data_progr8)], ps_pass, "g8_119")
  data_progr9 <- getComplete(data_progr9[,-ncol(data_progr9)], ps_pass, "g9_119")
  data_progr10 <- getComplete(data_progr10[,-ncol(data_progr10)], ps_pass, "g10_119")
  data_fup <- getComplete(data_fup[,-ncol(data_fup)], ps_pass, "h06")
  
  data_non_rep <- cbind(data_preliminary, data_demo[,-1], data_gensyndrome[,-1], data_cancer[,-1],
                        data_stage[,-1], data_cancer_tr[,-1], data_progr1[,-1], data_progr2[,-1], data_progr3[,-1],
                        data_progr4[,-1], data_progr5[,-1], data_progr6[,-1], data_progr7[,-1], data_progr8[,-1],
                        data_progr9[,-1], data_progr10[,-1], data_fup[,-1])
  
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
  
  date_col <- grep( "date", colnames(dataframe_final),value = TRUE) %>% grep("_unk", ., value = TRUE, invert = TRUE)
  
  data_final <- data_final %>%
    select(colnames(.)[colnames(.) %in% colnames(dataframe_final)]) %>%
    plyr::rbind.fill(dataframe_final, .) %>%
    mutate(across(all_of(date_col), ~ as.Date(., , origin = "1970-01-01")))

  # Create final data frame
  data_final <- data_final %>%
    mutate(hospital_id = properties$id_centro) %>% 
    # add "age" variable
    mutate(age = ifelse(!is.na(d03_diagage_au), d03_diagage_au, d04_diagage_man)) %>%
    # add "tumor_site" variable
    mutate(site = ifelse(!is.na(d11_siterare), d11_siterare, d11_sitecomrar)) %>%
    #add year of diagnosis
    mutate(year_diag = format(d01_diagdate, format = '%Y')) %>%
    #add year of followup
    mutate(yearF =  format(h02_datelasfup, format = '%Y')) %>%
    #add year of start Syst 
    mutate(yearSt =  format(f33_1_startdate_syst, format = '%Y')) %>%
    #add year start radio
    mutate(yearRadio =  format(f80_radiostartdate, format = '%Y')) %>%
    #add year end sys treat
    mutate(yearESt =  format(f34_1_enddate_syst, format = '%Y')) %>%
    #add year end radio
    mutate(yearERadio =  format(f81_radioendate, format = '%Y')) %>%
    #add year of surgery
    mutate(yearSurg =  format(f02_datesurg, format = '%Y')) %>%
    # add "treatment" variable
    mutate(treatment = case_when(
      # SOLO UN TRATTAMENTO
      f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & (min(f02_datesurg, f12_datenecksurg, na.rm = TRUE) <= (d01_diagdate + 90)) ~ "Only surgery",
      f53_radio %in% c(1,2) & !(f01_surgery %in% c(1,2)) & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f80_radiostartdate <= (d01_diagdate + 90) ~ "Only radiotherapy",
      f30_1_combofsystrt == 1 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 90) ~ "Only chemo",
      f30_1_combofsystrt == 2 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 90) ~ "Only immuno",
      f30_1_combofsystrt == 3 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 90) ~ "Only target",
      # SOLO CONCOMITANTI
      !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & (f33_1_startdate_syst <= d01_diagdate + 90 | f80_radiostartdate <= d01_diagdate + 90) &
        (abs(as.Date(f33_1_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_1_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & ((as.Date(f34_1_enddate_syst) - as.Date(f81_radioendate)) <= 7 | (as.Date(f49_1_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) ~ "Concomitant chemo/radio",
      # CHIRURGIA + altro
      f01_surgery %in% c(1,2) & f55_radiosett == 3 & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & (min(f02_datesurg, f12_datenecksurg, na.rm = TRUE) <= (d01_diagdate + 90)) & f80_radiostartdate <= (max(f02_datesurg, f12_datenecksurg, na.rm = TRUE) + 75) ~ "Surgery + postoperative radiotherapy",
      f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1 & f32_1_systsetting == 3 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & min(f02_datesurg, f12_datenecksurg, na.rm = TRUE) <= (d01_diagdate + 90) & f33_1_startdate_syst <= (max(f02_datesurg, f12_datenecksurg, na.rm = TRUE) + 90) ~ "Surgery + adjuvant chemo",
      f01_surgery %in% c(1,2) & f55_radiosett == 4 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & (min(f02_datesurg, f12_datenecksurg, na.rm = TRUE) <= (d01_diagdate + 90)) &
        (f33_1_startdate_syst <= (max(f02_datesurg, f12_datenecksurg, na.rm = TRUE) + 75) | f80_radiostartdate <= (max(f02_datesurg, f12_datenecksurg, na.rm = TRUE) + 75)) & (abs(as.Date(f33_1_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_1_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & ((as.Date(f34_1_enddate_syst) - as.Date(f81_radioendate)) <= 7 | (as.Date(f49_1_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) ~ "Surgery + Postoperative radio concomitant to chemotherapy",
      # RADIO + altro
      !(f01_surgery %in% c(1,2)) & f53_radio %in% c(1,2) & f30_1_combofsystrt == 1 & f32_1_systsetting == 3 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f80_radiostartdate <= (d01_diagdate + 90) & f33_1_startdate_syst <= (f81_radioendate + 45) ~ "Radio + adjuvant chemo",
      !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & f30_2_combofsystrt == 1 & f32_2_systsetting == 3 & !(f29_3_systrt %in% c(1,2)) & (f33_1_startdate_syst <= (d01_diagdate + 90) | f80_radiostartdate <= (d01_diagdate + 90)) &
        ((as.Date(f33_1_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_1_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & ((as.Date(f34_1_enddate_syst) - as.Date(f81_radioendate)) <= 7 | (as.Date(f49_1_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) & (f33_2_startdate_syst <= f34_1_enddate_syst + 45 | f33_2_startdate_syst <= f49_1_reg2endate_syst + 45) &
        f33_2_startdate_syst <= f81_radioendate + 45 ~ "Concomitant chemo/radio + adjuvant chemo",
      # CHEMO + altro QUESTO ANDR° cambiat nelle nuova CRF poichè non avremo queste combosystrt_cosi strutturata
      !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1 & f30_2_combofsystrt == 2 & !(f29_3_systrt %in% c(1,2)) &
        f33_1_startdate_syst <= (d01_diagdate + 90) & f33_2_startdate_syst <= (d01_diagdate + 90) ~ "Chemo + immuno",
      !(f01_surgery %in% c(1,2)) & f53_radio %in% c(1,2) & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) &
        f33_1_startdate_syst <= (d01_diagdate + 90) & (f80_radiostartdate <= f34_1_enddate_syst + 30  | f80_radiostartdate <= f49_1_reg2endate_syst + 30) ~ "Neo-adjuvant chemo + radio",
      f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 &
        !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 90) & (min(f02_datesurg, f12_datenecksurg, na.rm = TRUE) <= f34_1_enddate_syst + 30  | min(f02_datesurg, f12_datenecksurg, na.rm = TRUE) <= f49_1_reg2endate_syst + 30) ~ "Neo-adjuvant chemo + surgery",
      !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_2_systsetting == 2 & !(f29_3_systrt %in% c(1,2)) &
        f33_1_startdate_syst <= (d01_diagdate + 90) & (f33_2_startdate_syst <= f34_1_enddate_syst + 30 | f80_radiostartdate <= f34_1_enddate_syst + 30 | f33_2_startdate_syst <= f49_1_reg2endate_syst + 30 | f80_radiostartdate <= f49_1_reg2endate_syst + 30) & (abs(as.Date(f33_2_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_2_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & ((as.Date(f34_2_enddate_syst) - as.Date(f81_radioendate)) <= 7 | (as.Date(f49_2_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) ~ "Neo-adjuvant chemo + concomitant radio/chemo",
      !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_2_systsetting == 2 & f30_3_combofsystrt == 1 & f32_3_systsetting == 3 &
        f33_1_startdate_syst <= (d01_diagdate + 90) & (f33_2_startdate_syst <= f34_1_enddate_syst + 30 | f80_radiostartdate <= f34_1_enddate_syst + 30 | f33_2_startdate_syst <= f49_1_reg2endate_syst + 30 | f80_radiostartdate <= f49_1_reg2endate_syst + 30) &
        (abs(as.Date(f33_2_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_2_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & ((as.Date(f34_2_enddate_syst) - as.Date(f81_radioendate)) <= 7 | (as.Date(f49_2_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) &
        (f33_3_startdate_syst <= f34_2_enddate_syst + 45 | f33_3_startdate_syst <= f49_2_reg2endate_syst + 45) & f33_3_startdate_syst <= f81_radioendate + 45 ~ "Neo-adjuvant chemo + concomitant radio/chemo + chemo adjuvant",
      !(f01_surgery %in% c(1,2)) & f55_radiosett == 5 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_2_systsetting == 3 & !(f29_3_systrt %in% c(1,2)) &
        f33_1_startdate_syst <= (d01_diagdate + 90) & (f80_radiostartdate <= f34_1_enddate_syst + 30  | f80_radiostartdate <= f49_1_reg2endate_syst + 30) &
        f33_2_startdate_syst <= f81_radioendate + 45 ~ "Neo-adjuvant chemo + radio + chemo adjuvant",
      # IMMUNO + altro
      !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 2 & f30_2_combofsystrt == 3 & !(f29_3_systrt %in% c(1,2)) &
        f33_1_startdate_syst <= (d01_diagdate + 90) & f33_2_startdate_syst <= (d01_diagdate + 90) ~ "Immuno + target",
      # SOLO CLINICAL TRIAL
      !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & !(f99_othertrt %in% c(1,2)) & f105_clinicltrial == 1 ~ "Clinical trial only",
      # NON TRATTATO
      !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2))  & !(f99_othertrt %in% c(1,2)) ~ "No treatment",
      # OTHER
      TRUE ~ "Other")) %>%
    select(c("a01_id","hospital_id", "age", "site","year_diag","yearF", "yearSt", "yearRadio", "yearERadio", "yearESt","yearSurg","treatment"), everything()) %>%
    # inclusion criteria
    # filter(a04_consent %in% c(1,3)) %>% #informed consent signed for clinical data
    filter(d03_diagage_au >= 18 | d04_diagage_man >= 18) %>% #age >= 18
    filter(d01_diagdate >= as.Date("2018-01-01")) %>% #year of diagnosis >= 2018
    # filter(as.integer(format(d01_diagdate, "%Y")) >= 2018) %>% #year of diagnosis >= 2018
    select(- grep("_complete", colnames(.), value = TRUE))
  
  # if AIOCC, add DAG name to hospital_id 
  if(properties$id_centro == id_aiocc) {
    data_final <- data_final %>%
      left_join(., pt_dag, by = "a01_id") %>%
      mutate(hospital_id = paste(hospital_id, redcap_data_access_group, sep = "-")) %>%
      select(-redcap_data_access_group)
  }
  
  data_final <- data_final %>%
    filter(!(hospital_id %in% exclude_from_analyses))
  
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
  # system("echo \'datafile=\"/data/default.csv\"; if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data; fi' | bash")
  # system("echo \'datafile=\"/data/data_version.json\"; if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data; fi' | bash")
  file.copy(from = "/data/default.csv", to = "/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data", overwrite = TRUE)
  file.copy(from = "/data/data_version.json", to = "/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data", overwrite = TRUE)
} else {
  # write txt
  file_name <- paste0(properties$id_centro,'-', run_id,'-DATA-report-', format(Sys.Date(), "%y%m%d"), '.txt')
  fileConn <- file(file_name)
  if("core" %in% colnames(ps_pass)) {
    writeLines(c("None of the patients have successfully passed all the data quality checks, and as a result, they cannot be included in the analysis", 
                 "Please review and correct your data as necessary to ensure its suitability for future analyses."), fileConn) 
    message("No records can be included in the analysis. Execution stopped.")
  } else {
    writeLines(c("No records exist yet."), fileConn)
    message("No records exist yet. Execution stopped.")
  }
  close(fileConn)
  
  # Copy in /opt/redcap_dq/environment/data
  file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data", overwrite = TRUE)
}