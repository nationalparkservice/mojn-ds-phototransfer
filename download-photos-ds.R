library(magrittr)
keyring::key_set("AGOL", "mojn_data")
source(here::here("utils.R"))
##Download internal photos from AGOL for each internal photo table
VegPhotoTable <- DownloadAGOLAttachments("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer/11", dest_folder = "~/R/mojn-ds_phototransfer/photo_downloads", agol_username = "mojn_data", test_run = FALSE, custom_name = FALSE)

InvPhotoTable <- DownloadAGOLAttachments("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer/13", dest_folder = "~/R/mojn-ds_phototransfer/photo_downloads", agol_username = "mojn_data", test_run = FALSE, custom_name = FALSE)

AddPhotoTable <- DownloadAGOLAttachments("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer/16", dest_folder = "~/R/mojn-ds_phototransfer/photo_downloads", agol_username = "mojn_data", test_run = FALSE, custom_name = FALSE)

RptPhotoTable <- DownloadAGOLAttachments("https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/MOJN_DS_SpringVisit/FeatureServer/5", dest_folder = "~/R/mojn-ds_phototransfer/photo_downloads", agol_username = "mojn_data", test_run = FALSE, custom_name = FALSE)
