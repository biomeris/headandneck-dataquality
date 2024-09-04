##### Data Quality HN Core Data Registry 2.0 #####

# Nome file originale: Stage_treatment.R

# analysis id --------- CHANGE THE ANALYSIS ID HERE
id_analysis <- "TEST"

# run id ------ CHANGE THE RUN ID HERE
run_id <- paste0("Stage_treatment", format(Sys.Date(), "%y%m%d"))

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
# Staging & Treatments ---------
###### N.B. per trattamento considerare sempre e solo clinical stage
data_trt_full <- redcap_data %>% subset(redcap_event_name == "non_repeatable_arm_1") %>%
  subset(e57_complete == 1 & f107_complete == 1) %>%
  mutate(treatment = case_when(
    # SOLO UN TRATTAMENTO
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) ~ "Only surgery",
    f53_radio %in% c(1,2) & !(f01_surgery %in% c(1,2)) & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f80_radiostartdate <= (d01_diagdate + 60) ~ "Only radiotherapy",
    f30_1_combofsystrt == 1 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only chemo",
    f30_1_combofsystrt == 2 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only immuno",
    f30_1_combofsystrt == 3 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only target",
    # SOLO CONCOMITANTI
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & (f33_1_startdate_syst <= d01_diagdate + 60 | f80_radiostartdate <= d01_diagdate + 60) &
      (abs(as.Date(f33_1_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_1_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & (abs(as.Date(f34_1_enddate_syst) - as.Date(f81_radioendate)) <= 7 | abs(as.Date(f49_1_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) ~ "Concomitant chemo/radio",
    # CHIRURGIA + altro
    f01_surgery %in% c(1,2) & f55_radiosett == 3 & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) & f80_radiostartdate <= (f02_datesurg + 75) ~ "Surgery + postoperative radiotherapy",
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1 & f32_1_systsetting == 3 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) & f33_1_startdate_syst <= (f02_datesurg + 60) ~ "Surgery + adjuvant chemo",
    f01_surgery %in% c(1,2) & f55_radiosett == 4 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) &
      (f33_1_startdate_syst <= (f02_datesurg + 75) | f80_radiostartdate <= (f02_datesurg + 75)) & (abs(as.Date(f33_1_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_1_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & (abs(as.Date(f34_1_enddate_syst) - as.Date(f81_radioendate)) <= 7 | abs(as.Date(f49_1_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) ~ "Surgery + Postoperative radio concomitant to chemotherapy",
    # RADIO + altro
    !(f01_surgery %in% c(1,2)) & f53_radio %in% c(1,2) & f30_1_combofsystrt == 1 & f32_1_systsetting == 3 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f80_radiostartdate <= (d01_diagdate + 60) & f33_1_startdate_syst <= (f81_radioendate + 45) ~ "Radio + adjuvant chemo",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & f30_2_combofsystrt == 1 & f32_2_systsetting == 3 & !(f29_3_systrt %in% c(1,2)) & (f33_1_startdate_syst <= (d01_diagdate + 60) | f80_radiostartdate <= (d01_diagdate + 60)) &
      (abs(as.Date(f33_1_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_1_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & (abs(as.Date(f34_1_enddate_syst) - as.Date(f81_radioendate)) <= 7 | abs(as.Date(f49_1_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) & (f33_2_startdate_syst <= f34_1_enddate_syst + 45 | f33_2_startdate_syst <= f49_1_reg2endate_syst + 45) &
      f33_2_startdate_syst <= f81_radioendate + 45 ~ "Concomitant chemo/radio + adjuvant chemo",
    # CHEMO + altro
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1 & f30_2_combofsystrt == 2 & !(f29_3_systrt %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & f33_2_startdate_syst <= (d01_diagdate + 60) ~ "Chemo + immuno",
    !(f01_surgery %in% c(1,2)) & f53_radio %in% c(1,2) & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & (f80_radiostartdate <= f34_1_enddate_syst + 30  | f80_radiostartdate <= f49_1_reg2endate_syst + 30) ~ "Neo-adjuvant chemo + radio",
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 &
      !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) & (f02_datesurg <= f34_1_enddate_syst + 30  | f02_datesurg <= f49_1_reg2endate_syst + 30) ~ "Neo-adjuvant chemo + surgery",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_2_systsetting == 2 & !(f29_3_systrt %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & (f33_2_startdate_syst <= f34_1_enddate_syst + 30 | f80_radiostartdate <= f34_1_enddate_syst + 30 | f33_2_startdate_syst <= f49_1_reg2endate_syst + 30 | f80_radiostartdate <= f49_1_reg2endate_syst + 30) & (abs(as.Date(f33_2_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_2_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & (abs(as.Date(f34_2_enddate_syst) - as.Date(f81_radioendate)) <= 7 | abs(as.Date(f49_2_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) ~ "Neo-adjuvant chemo + concomitant radio/chemo",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_2_systsetting == 2 & f30_3_combofsystrt == 1 & f32_3_systsetting == 3 &
      f33_1_startdate_syst <= (d01_diagdate + 60) & (f33_2_startdate_syst <= f34_1_enddate_syst + 30 | f80_radiostartdate <= f34_1_enddate_syst + 30 | f33_2_startdate_syst <= f49_1_reg2endate_syst + 30 | f80_radiostartdate <= f49_1_reg2endate_syst + 30) &
      (abs(as.Date(f33_2_startdate_syst) - as.Date(f80_radiostartdate)) <= 7 | abs(as.Date(f48_2_reg2stdate_syst) - as.Date(f80_radiostartdate)) <= 7) & (abs(as.Date(f34_2_enddate_syst) - as.Date(f81_radioendate)) <= 7 | abs(as.Date(f49_2_reg2endate_syst) - as.Date(f81_radioendate)) <= 7) &
      (f33_3_startdate_syst <= f34_2_enddate_syst + 45 | f33_3_startdate_syst <= f49_2_reg2endate_syst + 45) & f33_3_startdate_syst <= f81_radioendate + 45 ~ "Neo-adjuvant chemo + concomitant radio/chemo + chemo adjuvant",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 5 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_2_systsetting == 3 & !(f29_3_systrt %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & (f80_radiostartdate <= f34_1_enddate_syst + 30  | f80_radiostartdate <= f49_1_reg2endate_syst + 30) &
      f33_2_startdate_syst <= f81_radioendate + 45 ~ "Neo-adjuvant chemo + radio + chemo adjuvant",
    # IMMUNO + altro
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 2 & f30_2_combofsystrt == 3 & !(f29_3_systrt %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & f33_2_startdate_syst <= (d01_diagdate + 60) ~ "Immuno + target",
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 2 & f30_2_combofsystrt == 1 & !(f29_3_systrt %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & f33_2_startdate_syst <= (d01_diagdate + 60) ~ "Immuno + chemo",
    # SOLO CLINICAL TRIAL
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2)) & !(f99_othertrt %in% c(1,2)) & f105_clinicltrial == 1 ~ "Clinical trial only",
    # NON TRATTATO
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !(f29_1_systrt %in% c(1,2)) & !(f29_2_systrt %in% c(1,2)) & !(f29_3_systrt %in% c(1,2))  & !(f99_othertrt %in% c(1,2)) ~ "No treatment",
    # OTHER
    TRUE ~ "Other")) %>%
  select(c("a01_id", "d11_siterare", "d11_sitecomrar","e34_cstage", "treatment")) %>%
  mutate(e34_cstage = case_when(e34_cstage == 999 ~ "Unknown",
                                e34_cstage == 1 ~ "0",
                                e34_cstage == 2 ~ "I",
                                e34_cstage == 3 ~ "II",
                                e34_cstage == 4 ~ "III",
                                e34_cstage == 5 ~ "IV",
                                e34_cstage == 6 ~ "IVA",
                                e34_cstage == 7 ~ "IVB",
                                e34_cstage == 8 ~ "IVC")) %>%
  mutate(site = case_when(d11_siterare == 1 | d11_sitecomrar == 1 ~ "Nasal cavity and paranasal sinuses",
                          d11_siterare == 2 | d11_sitecomrar == 2 ~ "Nasopharynx",
                          d11_siterare %in% c(3,4,5) | d11_sitecomrar %in% c(3,4,5) ~ "Parotid gland; Submandibular gland; Sublingual gland",
                          d11_siterare == 6 | d11_sitecomrar == 6 ~ "Middle ear",
                          TRUE ~ "Other"))

data_trt <- data_trt_full %>%
  plyr::count(c("e34_cstage", "treatment", "site")) %>%
  mutate(e34_cstage = ifelse(is.na(e34_cstage), "Stage not defined", e34_cstage))
  
lev_stage <- factor(unique(data_trt$e34_cstage),
                    levels = c("0", "I", "II", "III", "IV", "IVA", "IVB", "IVC", "Unknown", "Stage not defined"))
lev_trt <- factor(unique(data_trt$treatment),
                  levels = c("Only surgery", "Only radiotherapy", "Only chemo", "Only immuno", "Only target", "Concomitant chemo/radio", "Surgery + postoperative radiotherapy", "Surgery + adjuvant chemo", "Surgery + Postoperative radio concomitant to chemotherapy", "Radio + adjuvant chemo", "Concomitant chemo/radio + adjuvant chemo", "Chemo + immuno",
                             "Neo-adjuvant chemo + radio", "Neo-adjuvant chemo + surgery", "Neo-adjuvant chemo + concomitant radio/chemo", "Neo-adjuvant chemo + concomitant radio/chemo + chemo adjuvant", "Neo-adjuvant chemo + radio + chemo adjuvant", "Immuno + target", "Immuno + chemo", "Clinical trial only", "No treatment", "Other"))
lev_site <- factor(unique(data_trt$site),
                   levels = c("Nasal cavity and paranasal sinuses", "Nasopharynx", "Parotid gland; Submandibular gland; Sublingual gland", "Middle ear", "Other"))

stage_per_treat_tot <- setNames(vector("list", length(levels(lev_site))), levels(lev_site))

for (k in names(stage_per_treat_tot)) {
  data_trt_tmp <- data_trt %>%
    filter(site == k)
  
  stage_per_treat <- setNames(data.frame(matrix(ncol = length(levels(lev_trt)), nrow = length(levels(lev_stage))), row.names = levels(lev_stage)), levels(lev_trt))
  
  for (i in 1:nrow(stage_per_treat)) {
    stage <- rownames(stage_per_treat)[i]
    for (j in 1:ncol(stage_per_treat)) {
      trt <- colnames(stage_per_treat)[j]
      stage_per_treat[i,j] <- ifelse(length(data_trt_tmp[which(data_trt_tmp$e34_cstage == stage & data_trt_tmp$treatment == trt), 'freq']),
                                     data_trt_tmp[which(data_trt_tmp$e34_cstage == stage & data_trt_tmp$treatment == trt), 'freq'], NA)
    }
  }
  
  stage_per_treat <- stage_per_treat %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(`No treatment` = `No treatment`  + `Clinical trial only`)
  
  stage_per_treat_tot[[k]] <- stage_per_treat
  
  gc()
}

data2write <- data_trt_full %>%
  select(c(a01_id,site, e34_cstage, treatment)) %>%
  setNames(c("Patient ID", "Site", "Clinical stage", "Treatment"))

###############
# create workbook
wb <- createWorkbook()

addWorksheet(wb, "Staging & Treatment")
addWorksheet(wb, "Lista pazienti")

curr_row <- 1
for(i in seq_along(stage_per_treat_tot)[-length(seq_along(stage_per_treat_tot))]) {
  writeData(wb, "Staging & Treatment", names(stage_per_treat_tot)[i], startCol = 1, startRow = curr_row)
  addStyle(wb, sheet = "Staging & Treatment", createStyle(textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#999999", fontSize = 12, fontColour = "#cc0000"), rows = curr_row, cols = 1)
  writeData(wb, "Staging & Treatment", stage_per_treat_tot[[i]], startCol = 1, startRow = curr_row + 1, rowNames = TRUE, borders = "all", borderColour = "#999999",
            headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
  curr_row <- curr_row + nrow(stage_per_treat_tot[[i]]) + 4
}
setColWidths(wb,sheet = "Staging & Treatment",cols = 1:ncol(stage_per_treat_tot[[1]]), widths = "auto")

writeData(wb,sheet = "Lista pazienti", data2write, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
setColWidths(wb,sheet = "Lista pazienti",cols = 1:ncol(data2write), widths = "auto")

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