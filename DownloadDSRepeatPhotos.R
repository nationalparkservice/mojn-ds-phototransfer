library(magrittr)
keyring::key_set("AGOL", "mojn_data")
source(here::here("utils.R"))
PhotoTable <- DownloadAGOLAttachments("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer/5", agol_username = "mojn_data", test_run = FALSE, custom_name = TRUE, prefix = parentglobalid)


