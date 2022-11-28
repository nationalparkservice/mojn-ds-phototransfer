library(desertsprings)
library(dplyr)
library(devtools)
##install desert springs package from github
install_github("nationalparkservice/mojn-ds-rpackage", ref = "agol-import")
##Get raw desert springs layers from AGOL database
agol_layers <- desertsprings::FetchAGOLLayers()

## must run download-photos-ds before running the Internal Photo part of this script

#### Move and rename External Photos ####
## Settings for photo import/export:
OriginalPath <- "M://MONITORING//_FieldPhotoOriginals_DoNotModify//"

# Create photo tables to be populated with filepaths
repeat_photos_ext <- agol_layers$repeats_ext %>% 
  dplyr::inner_join(agol_layers$repeats, by = c("parentglobalid" = "globalid")) %>% 
  dplyr::inner_join(agol_layers$visit, by = c("parentglobalid.y" = "globalid")) %>%
  dplyr::mutate(VisitDate = lubridate::as_date(DateTime),
                PhotoSOP = "RPT") %>%
  dplyr::select(Park, 
         SiteCode,
         VisitDate,
         Camera,
         CameraCard,
         ExternalFileNumber, 
         PhotoType,
         PhotoSOP,
         OriginalFilePath,
         renamedfilepath,
         globalid,
         objectid = objectid.x)

veg_photos_ext <- agol_layers$riparian_veg_ext %>% 
  dplyr::inner_join(agol_layers$riparian_veg, by = c("parentglobalid" = "globalid")) %>% 
  dplyr::inner_join(agol_layers$visit, by = c("parentglobalid.y" = "globalid")) %>%
  dplyr::mutate(VisitDate = lubridate::as_date(DateTime),
                PhotoSOP = "RVG") %>%
  dplyr::select(Park, 
                SiteCode,
                VisitDate,
                Camera,
                CameraCard,
                ExternalFileNumber = ExternalFileNumbers,
                PhotoType = LifeForm,
                PhotoSOP,
                OriginalFilePath,
                renamedfilepath,
                globalid,
                objectid = objectid.x)

invasive_photos_ext <- agol_layers$invasives_ext %>%
  dplyr::inner_join(agol_layers$invasives, by = c("parentglobalid" = "globalid")) %>%
  dplyr::inner_join(agol_layers$visit, by = c("parentglobalid.y" = "globalid")) %>%
  dplyr::mutate(VisitDate = lubridate::as_date(DateTime),
                PhotoSOP = "INV") %>%
  dplyr::select(Park,
                SiteCode,
                VisitDate,
                Camera,
                CameraCard,
                ExternalFileNumber = ExternalFileNumbersInv,
                PhotoType = InvasiveSpecies,
                PhotoSOP,
                OriginalFilePath,
                renamedfilepath,
                globalid,
                objectid = objectid.x)

additional_photos_ext <-agol_layers$additional_photos_ext %>%
  dplyr::inner_join(agol_layers$additional_photos, by = c("parentglobalid" = "globalid")) %>%
  dplyr::inner_join(agol_layers$visit, by = c("parentglobalid.y" = "globalid")) %>%
  dplyr::mutate(VisitDate = lubridate::as_date(DateTime),
                PhotoSOP = "MSC") %>%
  dplyr::select(Park,
                SiteCode,
                VisitDate,
                Camera,
                CameraCard,
                ExternalFileNumber = ExternalFileNumbersMisc,
                PhotoType = AdditionalPhotoType,
                PhotoSOP,
                OriginalFilePath,
                renamedfilepath,
                globalid,
                objectid = objectid.x)

photos_external <- dplyr::bind_rows(repeat_photos_ext, veg_photos_ext, invasive_photos_ext, additional_photos_ext)


# Locate external files in incoming photos folder and generate path names

### change visit date filter based on beginning of field season ###
photo_paths_external <- photos_external %>%
  filter(VisitDate >= "2021-10-01") %>%
  mutate(NewFilename = paste(SiteCode, format.Date(VisitDate, "%Y%m%d"), PhotoType, ExternalFileNumber, sep = "_"),
         NewFilename = paste0(NewFilename, ".JPG"),
         RenamedFileDir = file.path(normalizePath("M:/MONITORING/DS_Water/Data/Images/", winslash = .Platform$file.sep), SiteCode),
         OrigFileDir = file.path(normalizePath(OriginalPath, winslash = .Platform$file.sep), CameraCard, format.Date(VisitDate, "%Y_%m_%d")))


photo_paths_external$OrigFileName <- NA
for(i in 1:nrow(photo_paths_external)) {
  OrigFile = list.files(photo_paths_external$OrigFileDir[[i]], pattern = paste0("*.", photo_paths_external$ExternalFileNumber[[i]], ".JPG"), ignore.case = TRUE)
  if (length(OrigFile) == 1) {
    photo_paths_external$OrigFileName[[i]] <- OrigFile
  } else if (length(OrigFile) == 0) {
    warning(paste0("File number ", photo_paths_external$ExternalFileNumber[[i]], ", taken at ", photo_paths_external$SiteCode[[i]], ", not found in ", photo_paths_external$OrigFileDir[[i]]), immediate. = TRUE)
  } else if (length(OrigFile) > 1) {
    warning(paste0("More than one photo found matching file number ", photo_paths_external$ExternalFileNumber[[i]],  ", taken at ", photo_paths_external$SiteCode[[i]], " found in ", photo_paths_external$OrigFileDir[[i]]))
  }
}

# Filter out missing photos and populate file path fields
rows_before <- nrow(photo_paths_external)
photo_paths_external %<>% filter(!is.na(OrigFileName)) %>%
  mutate(OriginalFilePath = file.path(OrigFileDir, OrigFileName),
         renamedfilepath = file.path(RenamedFileDir, NewFilename))
if (rows_before > nrow(photo_paths_external)) {
  warning("Some photos could not be located (see warnings above). The records for these photos will NOT be uploaded to the database.")
}

# Copy photos from Original File Path to Renamed
for (i in 1:nrow(photo_paths_external)) {
  if (!is.na(photo_paths_external$OrigFileName[i])) {
    orig.path <- photo_paths_external$OriginalFilePath[i]
    new.path <- photo_paths_external$renamedfilepath[i]
    if (!dir.exists(dirname(new.path))) {
      dir.create(dirname(new.path), recursive = TRUE)
      # cat(dirname(new.path))
    }
    file.copy(from = orig.path, to = new.path, overwrite = FALSE, copy.mode = FALSE, copy.date = FALSE)
  }
}

# Create output csvs to use for updating the RenamedFilePath and OriginalFilePath fields in the desert springs AGOL database.
photo_paths_external_rpt <- photo_paths_external %>%
  filter(PhotoSOP == "RPT") %>%
  select(-OrigFileName, -OrigFileDir, -RenamedFileDir, -NewFilename)
write.csv(photo_paths_external_rpt, "C:/Users/sierramoore/Documents/R/mojn-ds-phototransfer/Output/photo_paths_external_rpt.csv")

photo_paths_external_veg <- photo_paths_external %>%
  filter(PhotoSOP == "RVG") %>%
  select(-OrigFileName, -OrigFileDir, -RenamedFileDir, -NewFilename)
write.csv(photo_paths_external_veg, "C:/Users/sierramoore/Documents/R/mojn-ds-phototransfer/Output/photo_paths_external_veg.csv")

photo_paths_external_inv <- photo_paths_external %>%
  filter(PhotoSOP == "INV") %>%
  select(-OrigFileName, -OrigFileDir, -RenamedFileDir, -NewFilename)
write.csv(photo_paths_external_inv, "C:/Users/sierramoore/Documents/R/mojn-ds-phototransfer/Output/photo_paths_external_inv.csv")

photo_paths_external_add <- photo_paths_external %>%
  filter(PhotoSOP == "MSC") %>%
  select(-OrigFileName, -OrigFileDir, -RenamedFileDir, -NewFilename)
write.csv(photo_paths_external_add, "C:/Users/sierramoore/Documents/R/mojn-ds-phototransfer/Output/photo_paths_external_add.csv")


####Move and Rename Internal Photos####

### must run download-photos-ds before running this part of the script ###

# Settings for photo import/export:
OriginalPath <- "M://MONITORING//_FieldPhotoOriginals_DoNotModify//"
#path to downloaded internal photos (from download-photos-ds script)
InternalPath <- "~/R/mojn-ds_phototransfer/photo_downloads"

# Create photo tables to populate filepath fields
repeat_photos_int <- agol_layers$repeats_int %>% 
  dplyr::inner_join(agol_layers$repeats, by = c("parentglobalid" = "globalid")) %>% 
  dplyr::inner_join(agol_layers$visit, by = c("parentglobalid.y" = "globalid")) %>%
  dplyr::inner_join(RptPhotoTable, by = c("globalid" = "parentGlobalId")) %>%
  dplyr::mutate(VisitDate = lubridate::as_date(DateTime), 
                PhotoSOP = "RPT", 
                internalFilePath = paste0(InternalPath, "/", RptPhotoTable$name),
                filenumber = substring(RptPhotoTable$name, nchar(RptPhotoTable$name) - 7, nchar(RptPhotoTable$name) - 4)) %>%
  dplyr::select(Park, 
                SiteCode,
                VisitDate,
                Camera,
                CameraCard,
                filenumber,
                PhotoType,
                OriginalFilePath = OriginalFilePath.x,
                renamedfilepath,
                internalFilePath,
                globalid,
                objectid = objectid.x,
                PhotoSOP)

veg_photos_int <- agol_layers$riparian_veg_int %>% 
  dplyr::inner_join(agol_layers$riparian_veg, by = c("parentglobalid" = "globalid")) %>% 
  dplyr::inner_join(agol_layers$visit, by = c("parentglobalid.y" = "globalid")) %>%
  dplyr::inner_join(VegPhotoTable, by = c("globalid" = "parentGlobalId")) %>%
  dplyr::mutate(VisitDate = lubridate::as_date(DateTime), 
                PhotoSOP = "RVG", 
                internalFilePath = paste0(InternalPath, "/", VegPhotoTable$name),
                filenumber = substring(VegPhotoTable$name, nchar(VegPhotoTable$name) - 7, nchar(VegPhotoTable$name) - 4)) %>%
  dplyr::select(Park, 
                SiteCode,
                VisitDate,
                Camera,
                CameraCard,
                filenumber,
                PhotoType = LifeForm,
                PhotoSOP,
                OriginalFilePath = OriginalFilePath.x,
                renamedfilepath,
                internalFilePath,
                globalid,
                objectid = objectid.x)

invasive_photos_int <- agol_layers$invasives_int %>%
  dplyr::inner_join(agol_layers$invasives, by = c("parentglobalid" = "globalid")) %>%
  dplyr::inner_join(agol_layers$visit, by = c("parentglobalid.y" = "globalid")) %>%
  dplyr::inner_join(InvPhotoTable, by = c("globalid" = "parentGlobalId")) %>%
  dplyr::mutate(VisitDate = lubridate::as_date(DateTime), 
                PhotoSOP = "INV", internalFilePath = paste0(InternalPath, "/", InvPhotoTable$name),
                filenumber = substring(InvPhotoTable$name, nchar(InvPhotoTable$name) - 7, nchar(InvPhotoTable$name) - 4)) %>%
  dplyr::select(Park,
                SiteCode,
                VisitDate,
                Camera,
                CameraCard,
                filenumber,
                PhotoType = InvasiveSpecies,
                PhotoSOP,
                OriginalFilePath = OriginalFilePath.x,
                renamedfilepath,
                internalFilePath,
                globalid,
                objectid = objectid.x)

additional_photos_int <-agol_layers$additional_photos_int %>%
  dplyr::inner_join(agol_layers$additional_photos, by = c("parentglobalid" = "globalid")) %>%
  dplyr::inner_join(agol_layers$visit, by = c("parentglobalid.y" = "globalid")) %>%
  dplyr::inner_join(AddPhotoTable, by = c("globalid" = "parentGlobalId")) %>%
  dplyr::mutate(VisitDate = lubridate::as_date(DateTime), 
                PhotoSOP = "MSC", 
                internalFilePath = paste0(InternalPath, "/", AddPhotoTable$name),
                filenumber = substring(AddPhotoTable$name, nchar(AddPhotoTable$name) - 7, nchar(AddPhotoTable$name) - 4)) %>%
  dplyr::select(Park,
                SiteCode,
                VisitDate,
                Camera,
                CameraCard,
                filenumber,
                PhotoType = AdditionalPhotoType,
                PhotoSOP,
                OriginalFilePath = OriginalFilePath.x,
                renamedfilepath,
                internalFilePath,
                globalid,
                objectid = objectid.x)

photos_internal <- dplyr::bind_rows(repeat_photos_int, veg_photos_int, invasive_photos_int, additional_photos_int)

# Populate photo file paths and file names
### change visit date filter based on beginning of field season ###
photo_paths_internal <- photos_internal %>%
  #filter(VisitDate >= "2021-10-01") %>%
  mutate(fileName = paste(SiteCode, format.Date(VisitDate, "%Y%m%d"), PhotoType, filenumber, sep = "_"),
         fileName = paste0(fileName, ".JPG"),
         RenamedFileDir = file.path(normalizePath("M:/MONITORING/DS_Water/Data/Images/", winslash = .Platform$file.sep), SiteCode),
         OrigFileDir = file.path(normalizePath(OriginalPath, winslash = .Platform$file.sep), CameraCard, format.Date(VisitDate, "%Y_%m_%d")),
         OriginalFilePath = file.path(OrigFileDir, fileName),
         renamedfilepath = file.path(RenamedFileDir, fileName))

# Import photos into "original" folder on M drive
for (i in 1:nrow(photo_paths_internal)) {
  orig.path <- photo_paths_internal$internalFilePath[i]
  new.path <- photo_paths_internal$OriginalFilePath[i]
  if (!dir.exists(dirname(new.path))) {
    dir.create(dirname(new.path), recursive = TRUE)
    # cat(dirname(new.path))
  }
  file.copy(from = orig.path, to = new.path, overwrite = FALSE, copy.mode = FALSE, copy.date = FALSE)
}

# Copy photos to "new" folder on M drive
for (i in 1:nrow(photo_paths_internal)) {
  orig.path <- photo_paths_internal$OriginalFilePath[i]
  new.path <- photo_paths_internal$renamedfilepath[i]
  if (!dir.exists(dirname(new.path))) {
    dir.create(dirname(new.path), recursive = TRUE)
    # cat(dirname(new.path))
  }
    file.copy(from = orig.path, to = new.path, overwrite = FALSE, copy.mode = FALSE, copy.date = FALSE)
  }


# Create output csvs to use for updating the RenamedFilePath and OriginalFilePath fields in the desert springs AGOL database
photo_paths_internal_rpt <- photo_paths_internal %>%
  filter(PhotoSOP == "RPT") %>%
  select(-OrigFileDir, -RenamedFileDir, -fileName, -internalFilePath)
write.csv(photo_paths_internal_rpt, "C:/Users/sierramoore/Documents/R/mojn-ds-phototransfer/Output/photo_pathsInt_rpt.csv")

photo_paths_internal_veg <- photo_paths_internal %>%
  filter(PhotoSOP == "RVG") %>%
  select(-OrigFileDir, -RenamedFileDir, -fileName, -internalFilePath)
write.csv(photo_paths_internal_veg, "C:/Users/sierramoore/Documents/R/mojn-ds-phototransfer/Output/photo_pathsInt_veg.csv")

photo_paths_internal_inv <- photo_paths_internal %>%
  filter(PhotoSOP == "INV") %>%
  select(-OrigFileDir, -RenamedFileDir, -fileName, -internalFilePath)
write.csv(photo_paths_internal_inv, "C:/Users/sierramoore/Documents/R/mojn-ds-phototransfer/Output/photo_pathsInt_inv.csv")

photo_paths_internal_add <- photo_paths_internal %>%
  filter(PhotoSOP == "MSC") %>%
  select(-OrigFileDir, -RenamedFileDir, -fileName, -internalFilePath)
write.csv(photo_paths_internal_add, "C:/Users/sierramoore/Documents/R/mojn-ds-phototransfer/Output/photo_pathsInt_add.csv")
