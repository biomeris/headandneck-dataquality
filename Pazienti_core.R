##### Data Quality HN Core Data Registry 2.0 #####

# Nome file originale: Pazienti_core.R

# analysis id --------- CHANGE THE ANALYSIS ID HERE
id_analysis <- "TEST"

# run id ------ CHANGE THE RUN ID HERE
run_id <- paste0("Pazienti_core", format(Sys.Date(), "%y%m%d"))

# id_centro AIOCC
id_aiocc <- 49

# DAG INT
dag_int <- "int_milano"

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

#### MAIN ####

# read properties file
setwd('/opt/redcap_dq/environment/scripts')
file_prop <- "../config/config.properties"
properties <- properties::read.properties(file_prop)

# Controllo se il centro è AIOCC. Se non lo è blocco l'esecuzione
if (properties$id_centro != id_aiocc) {
  stop()
}

if(substring(properties$uri, first = nchar(properties$uri), last = nchar(properties$uri)) != "/") {
  properties$uri <- paste0(properties$uri, "/")
}

#extract metadata from REDCap
metadata <- redcap_metadata_read(
  redcap_uri = properties$uri,
  token = properties$token)$data

# get patients' ids filtered by tumor site
pts_id <- redcap_read(
  batch_size = 10L,
  interbatch_delay = 0.5,
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id, d11_siterare, d11_sitecomrar",
  filter_logic = '([non_repeatable_arm_1][d11_siterare] < "6" AND [non_repeatable_arm_1][d11_siterare] <> "") OR ([non_repeatable_arm_1][d11_sitecomrar] < "11" AND [non_repeatable_arm_1][d11_sitecomrar] <> "")',
  export_data_access_groups = TRUE
)$data %>%
  filter(redcap_data_access_group == dag_int)

# get patients DAG
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
  export_data_access_groups = TRUE)$data

############################################## QC REPORT SUMMARY ##############################################
# Missing and unknown ---------
## Define core variables
core_var <- c("a05_date", "a06_phase", "b04_sex", "b06_resid", "b07_smoke", "b11_smokepy_auto", "b13_alc", "b14_carc", "b19_comorb",
              "c01_prevc", "c02_prevcsite", "d01_diagdate", "d03_diagage_au", "d05_histo", "d06_histosubg_sq", "d07_histosubg_ade", "d08_histosubg_net",
              "d09_histosubg_odo", "d11_siterare", "d11_sitecomrar", "d14_subsite_nasal", "d15_subsite_naso", "d16_subsite_hypo", "d17_subsite_oro",
              "d18_subsite_lar", "d19_subsite_oc", "d20_subsite_lip", "d21_ebv", "e01_stagehosp", "e02_im_primary", "e12_im_neck", "e21_im_m", "e32_c_ene",
              "e34_cstage", "e34_cstageed", "e39_p_ene", "e41_margins", "e43_sentnode", "e54_pstage", "e54_pstageed", "f01_surgery", "f02_datesurg", "f03_surgintent",
              "f07_reconstr", "f11_necksurg", "f12_datenecksurg", "f20_surgm", "f22_datesurgm", "f23_complic1", "f24_complic2", "f25_unplsurg", "f26_dateunplsurg",
              grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), grep("f30_[[:digit:]]_combofsystrt", colnames(redcap_data), value = TRUE), grep("f31_[[:digit:]]_systintent", colnames(redcap_data), value = TRUE),
              grep("f32_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("f33_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], colnames(redcap_data)[grepl("f34_[[:digit:]]_enddate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))],
              grep("f35_[[:digit:]]_numcycle", colnames(redcap_data), value = TRUE), grep("f36_[[:digit:]]_regimen", colnames(redcap_data), value = TRUE), grep("f44_[[:digit:]]_syst_reasonend", colnames(redcap_data), value = TRUE),
              grep("f45_[[:digit:]]_regchange", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("f48_[[:digit:]]_reg2stdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], colnames(redcap_data)[grepl("f49_[[:digit:]]_reg2endate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))],
              "f53_radio", "f54_radiointent", "f55_radiosett", "f57_beamqual", "f63_totdose", "f64_fractionsize", "f65_numfraction", "f80_radiostartdate", "f81_radioendate", "f84_ott", "f84_ottman",
              "f85_radiosite", "f92_trtcomplete", grep("g[[:digit:]]+_01_progrel", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("g[[:digit:]]+_03_date", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], grep("g[[:digit:]]+_04_local", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_05_regional", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_06_meta", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_08_surgery", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("g[[:digit:]]+_09_datesurg", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))],
              grep("g[[:digit:]]+_10_surgintent", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_13_margins", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_14_reconstr", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_15_necksurg", colnames(redcap_data), value = TRUE),
              colnames(redcap_data)[grepl("g[[:digit:]]+_16_datenecksurg", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], grep("g[[:digit:]]+_24_ene", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_30_surgm", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("g[[:digit:]]+_32_datesurgm", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))],
              grep("g[[:digit:]]+_33_1complic", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_34_2complic", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_35_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_36_[[:digit:]]_combofsystrt", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_37_[[:digit:]]_systintent", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_38_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE),
              colnames(redcap_data)[grepl("g[[:digit:]]+_39_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], colnames(redcap_data)[grepl("g[[:digit:]]+_40_[[:digit:]]_enddate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], grep("g[[:digit:]]+_41_[[:digit:]]_numcycle", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_42_[[:digit:]]_regimen", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_52_[[:digit:]]_syst_reasonend", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_53_[[:digit:]]_regchange", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_63_radio", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_64_reradiation", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_65_rerad_reason", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_66_radiointent", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_67_radiosett", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_69_beamqual", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_75_totd", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_76_fractionsize", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_77_numfraction", colnames(redcap_data), value = TRUE),
              colnames(redcap_data)[grepl("g[[:digit:]]+_92_radiostartdate", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], colnames(redcap_data)[grepl("g[[:digit:]]+_93_radioendate", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], grep("g[[:digit:]]+_96_ott", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_97_radiosite", metadata$field_name, value = TRUE),
              grep("g[[:digit:]]+_104_radio_complete", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_119_wfw", colnames(redcap_data), value = TRUE), "h01_status", "h02_datelasfup")

metadata_core <- metadata %>% subset(field_name %in% core_var)

# create workbook
wb <- createWorkbook()

# Core variables --------
instr_complete <- metadata %>%
  filter(field_label == "Instrument status:") %>%
  pull(field_name) 

# remove patients with at least one missining or unknown core variable
core_pts <- redcap_data %>% 
  subset(redcap_event_name == "non_repeatable_arm_1") %>%
  filter(if_any(all_of(instr_complete), ~ . == 1))

for (i in 1:nrow(metadata_core)) {
  var_tmp = metadata_core$field_name[i]
  form_complete = ifelse(substring(var_tmp,1,1) == "g",
                         grep(paste0(substring(var_tmp,1,2),"_[[:digit:]]+_complete"), metadata$field_name, value = TRUE),
                         grep(paste0(substring(var_tmp,1,1),"[[:digit:]]+_complete"), metadata$field_name, value = TRUE))
  expr_str = renderLogic(metadata_core$branching_logic[i])
  if(metadata_core$field_type[i] != "checkbox"){
    if(!is.na(expr_str)){
      pts_tmp <- core_pts %>% subset(get(form_complete) == 1 & eval(parse(text = expr_str))) %>%
        subset(is.na(get(var_tmp)) | get(var_tmp) %in% c(999, 9999))
    } else {
      pts_tmp <- core_pts %>% subset(get(form_complete) == 1) %>%
        subset(is.na(get(var_tmp)) | get(var_tmp) %in% c(999, 9999))
    }
  } else {
    cond <- ifelse(length(grep(paste0(var_tmp, "___999"), colnames(core_pts))),
                   'all(is.na(mget(grep(var_tmp, colnames(core_pts), value = TRUE), inherits = TRUE))) | get(grep(paste0(var_tmp, "___999"), colnames(core_pts), value = TRUE)) == 1',
                   'all(is.na(mget(grep(var_tmp, colnames(core_pts), value = TRUE), inherits = TRUE)))')
    if(!is.na(expr_str)){
      pts_tmp <- core_pts %>% subset(get(form_complete) == 1 & eval(parse(text = expr_str))) %>%
        subset(eval(parse(text = cond)))
    } else {
      pts_tmp <- core_pts %>% subset(get(form_complete) == 1) %>%
        subset(eval(parse(text = cond)))
    }
  }
  
  core_pts <- subset(core_pts, !(a01_id %in% pts_tmp$a01_id))
  
  gc()
}

# dataframe for summary report
core <- data.frame("patients" = length(unique(redcap_data$a01_id)), "pts_core" = nrow(core_pts),
                   "perc" = formattable::percent(nrow(core_pts)/length(unique(redcap_data$a01_id))))

colnames(core) <- c("N patients", "N patients with core variables", "% patients with core variables")

lista_paz <- as.data.frame(core_pts$a01_id)

# add worksheet
addWorksheet(wb, "Core variables")
addWorksheet(wb, "Lista pazienti")

# Core variables
writeData(wb,sheet = "Core variables", core, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
setColWidths(wb,sheet = "Core variables",cols = 1:ncol(core), widths = "auto")

writeData(wb,sheet = "Lista pazienti", lista_paz, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
setColWidths(wb,sheet = "Lista pazienti",cols = 1:ncol(lista_paz), widths = "auto")

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