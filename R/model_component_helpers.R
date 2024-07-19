library(jsonlite)
library(magrittr)


#' @title Reset OWL template
#' @description Overwrite the template.owl file in the main working directory by the blank template
#' @param voc Character string. Which vocabulary is reset?. Currently available
#'  choices are \code{"models"} (the default), \code{"institutions"} and \code{"variables"}.
#' @note
#' For safety, the function will ask for confirmation 
#' @author juaco
#' @keywords internal

restart.owl <- function(voc = "models") {
    voc <- match.arg(voc, choices = c("models", "institutions", "variables"))
    
    owl.file <- switch(voc,
                       "models" = "CMIP6-models.owl",
                       "institutions" = "CMIP6-institutions.owl",
                       "variables" = "CMIP6-variables.owl")
    choice <- menu(choices = c("yes", "no"),
                   title = paste("This will reset", owl.file, 
                   "in main working dir by a blank template... are you sure?"))
    if (choice == 1) {
        system(paste("cp -f aux/owl_template/template.owl", owl.file))
        
        Sys.setenv(TZ = "GMT")
        chartime <- Sys.time() %>% as.character()
        date <- paste0(substr(chartime, 1 , 10), "T",
                       substr(chartime, 12, 19), "Z")
        
        l <- readLines(owl.file)
        l <- gsub("XXX", date, l)
        writeLines(l, owl.file)
        
        if (voc == "institutions") {
            lines <- readLines(owl.file)
            lines <- gsub("CMIP6-models.owl", owl.file, lines)
            lines <- gsub("CMIP6 model entities", "CMIP6 institutions", lines)
            lines <- gsub("CMIP6 models", "CMIP6 institutions", lines)
            writeLines(lines, owl.file)
        } else if (voc == "variables") {
            lines <- readLines(owl.file)
            lines <- gsub("CMIP6-models.owl", owl.file, lines)
            lines <- gsub("CMIP6 model entities", "CMIP6 C3S single-level variables", lines)
            lines <- gsub("CMIP6 models", "CMIP6 C3S single-level variables", lines)
            lines <- gsub("https://wcrp-cmip.github.io/CMIP6_CVs",
                          "https://github.com/PCMDI/cmip6-cmor-tables/tree/main", lines)
            writeLines(lines, owl.file)
        }
        message(owl.file, " was reset")
    } else {
        message("action declined, nothing was done")
    }
}




#' @title e-mail character string helper
#' @description Fixes the 'CMIP6_source_id' bugreports e-mail format
#' @importFrom magrittr %>% 
#' @return It should return a syntactically valid email address
#' @author juaco
#' @keywords internal

fix.contact.info <- function(gcm.bugreports) {
    strsplit(gcm.bugreports,
             split = "<-", )[[1]][2:1] %>% gsub(" ",
                                                "", .) %>% paste(collapse = "")
}




#' @title Individual labels for CMIP6 atmospheric aerosol models
#' @description A helper to simplify the character string to identify the
#' atmospheric aerosol model, as retrieved from
#' the controlled vocabulary 'CMIP6_source_id' 
#' @param aerosol.model.description Character string, as retrieved from
#'  the controlled vocabulary 'CMIP6_source_id' of the aerosol model description
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @note Spaces are not allowed in URIs according to the URI syntax rules.
#'  If a space appears in a URI, it must be percent-encoded as %20.
#'   This is because spaces can cause issues with parsing and can lead
#'    to ambiguities when URIs are used in different contexts. 
#' @author juaco
#' @keywords internal

# ## CMIP6 Activities
# url <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/main/CMIP6_source_id.json"
# ## List of all models
# model.list <- fromJSON(url) %>% extract2("source_id")
# ## Modify grep pattern to match activity accordingly
# ml <- sapply(model.list, "[[", "activity_participation")
# ind <- sapply(ml, "grepl",
#               pattern = "^CMIP$") %>% sapply(., "any") %>% which(isTRUE(.)) 
# CMIP.models <- model.list[ind] 
# ## Atmos chem
# sapply(1:length(scenMIP.models), function(i) {
#     scenMIP.models[[i]][["model_component"]][["aerosol"]]$description
# }) %>% unique()

set.aerosol.individual <- function(aerosol.model.description) {
    switch(aerosol.model.description,
           "UKCA-GLOMAP-mode" = "UKCA-GLOMAP-mode",
           "CLASSIC (v1.0)" = "CLASSIC",
           "none" = "none",
           "IAP AACM" = "IAP-AACM",
           "MAM4 (same grid as atmos)" = "MAM4",
           "MAM4" = "MAM4",                                                                            
           "MAM3" = "MAM3",
           "prescribed monthly fields computed by TACTIC_v2 scheme" = "prescribed",
           "TACTIC_v2" = "TACTIC_v2",
           "interactive" = "interactive",
           "MAM4 with resuspension, marine organics, and secondary organics (same grid as atmos)" = "MAM4",
           "MAM4 with new resuspension, marine organics, secondary organics, and dust (atmos grid)" = "MAM4",
           "TM5 (3 x 2 degrees; 120 x 90 longitude/latitude; 34 levels; top level: 0.1 hPa)" = "TM5",
           "Prescribed monthly fields" = "prescribed",
           "Varies with physics-version (p==1 none, p==3 OMA, p==4 TOMAS, p==5 MATRIX)" = "varies-with-physics-version",
           "varies with physics-version (p==1 none, p==3 OMA, p==4 TOMAS, p==5 MATRIX)" = "varies-with-physics-version",
           "prescribed MAC-v2" = "prescribedMAC-v2",
           "INM-AER1" = "INM-AER1",
           "INCA v6 NMHC-AER-S" = "INCAv6-NMHC-AER-S",
           "Modifies surface albedoes (Haywood et al. 1997, doi: 10.1175/1520-0442(1997)010&lt;1562:GCMCOT&gt;2.0.CO;2)" = "Modified-surface-albedoes",
           "SPRINTARS6.0" = "SPRINTARS6.0",
           "HAM2.3" = "HAM2.3",
           "none, prescribed MACv2-SP" = "prescribed",
           "MASINGAR mk2r4 (TL95; 192 x 96 longitude/latitude; 80 levels; top level 0.01 hPa)" = "MASINGAR-mk2r4",
           "OsloAero" = "OsloAero",
           "SNAP (same grid as atmos)" = "SNAP",
           "MAM3 (same grid as atmos)" = "MAM3",
           "prescribed MACv2-SP" = "prescribed",
           "MAM4 w/ new resuspension, marine organics, secondary organics, and dust (atmos grid)" = "MAM4",
           "MAM4 w/ new resuspension, marine organics, secondary organics, and dust (atmos physics grid)" = "MAM4",
           "OsloAero4.1 (same grid as atmos)" = "OsloAero4.1",
           "Prescribed from MRI-ESM2.0" = "prescribed"
    )
} 




#' @title Individual labels for CMIP6 atmospheric models
#' @description A helper to simplify the character string to identify the atmospheric model,
#' as retrieved from the controlled vocabulary 'CMIP6_source_id' 
#' @param atmos.model.description Character string, as retrieved from
#'  the controlled vocabulary 'CMIP6_source_id'
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @note Spaces are not allowed in URIs according to the URI syntax rules.
#'  If a space appears in a URI, it must be percent-encoded as %20.
#'   This is because spaces can cause issues with parsing and can lead
#'    to ambiguities when URIs are used in different contexts. 
#' @author juaco
#' @keywords internal

# ## CMIP6 Activities
# url <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/main/CMIP6_source_id.json"
# ## List of all models
# model.list <- fromJSON(url) %>% extract2("source_id")
# ## Modify grep pattern to match activity accordingly
# ml <- sapply(model.list, "[[", "activity_participation")
# ind <- sapply(ml, "grepl",
#               pattern = "^CMIP$") %>% sapply(., "any") %>% which(isTRUE(.))
# CMIP.models <- model.list[ind]
# ## Atmos 
# sapply(1:length(scenMIP.models), function(i) {
#     scenMIP.models[[i]][["model_component"]][["atmos"]]$description
# }) %>% unique()

set.atmos.individual <- function(atmos.model.description) {
    switch(atmos.model.description,
           "MetUM-HadGEM3-GA7.1 (N96; 192 x 144 longitude/latitude; 85 levels; top level 85 km)" = "MetUM-HadGEM3-GA7.1",
           "HadGAM2 (r1.1, N96; 192 x 145 longitude/latitude; 38 levels; top level 39255 m)" = "HadGAM2",
           "ECHAM6.3.04p1 (T63L47 native atmosphere T63 gaussian grid; 192 x 96 longitude/latitude; 47 levels; top level 80 km)" = "ECHAM6.3",
           "ECHAM6.3.04p1 (T127L95 native atmosphere T127 gaussian grid; 384 x 192 longitude/latitude; 95 levels; top level 80 km)" = "ECHAM6.3",
           "BCC_AGCM3_MR (T106; 320 x 160 longitude/latitude; 46 levels; top level 1.46 hPa)" = "BCC_AGCM3_MR",
           "ECHAM5_CAMS (T106; 320 x 160 longitude/latitude; 31 levels; top level 10 mb)" = "ECHAM5",
           "IAP AGCM 5.0 (Finite difference dynamical core; 256 x 128 longitude/latitude; 35 levels; top level 2.2 hPa)" = "IAP-AGCM5.0",
           "CAM6 (0.9x1.25 finite volume grid; 288 x 192 longitude/latitude; 32 levels; top level 2.25 mb)" = "CAM6",
           "CAM6 (1.9x2.5 finite volume grid; 144 x 96 longitude/latitude; 32 levels; top level 2.25 mb)" = "CAM6",
           "WACCM6 (0.9x1.25 finite volume grid; 288 x 192 longitude/latitude; 70 levels; top level 4.5e-06 mb)" = "WACCM6",
           "CIESM-AM (FV/FD; 288 x 192 longitude/latitude; 30 levels; top level 2.255 hPa)" = "CIESM-AM",
           "CAM5.3 (1deg; 288 x 192 longitude/latitude; 30 levels; top at ~2 hPa)" = "CAM5.3",
           "Arpege 6.3 (T127; Gaussian Reduced with 24572 grid points in total distributed over 128 latitude circles (with 256 grid points per latitude circle between 30degN and 30degS reducing to 20 grid points per latitude circle at 88.9degN and 88.9degS); 91 levels; top level 78.4 km)" = "Arpege6.3",
           "Arpege 6.3 (T359; Gaussian Reduced with 181724 grid points in total distributed over 360 latitude circles (with 720 grid points per latitude circle between 32.2degN and 32.2degS reducing to 18 grid points per latitude circle at 89.6degN and 89.6degS); 91 levels; top level 78.4 km)" = "Arpege6.3",
           "CanAM5 (T63L49 native atmosphere, T63 Linear Gaussian Grid; 128 x 64 longitude/latitude; 49 levels; top level 1 hPa)" = "CanAM5",
           "CanAM5.1 (T63L49 native atmosphere, T63 Linear Gaussian Grid; 128 x 64 longitude/latitude; 49 levels; top level 1 hPa)" = "CanAM5",
           "EAM (v1.0, cubed sphere spectral-element grid; 5400 elements with p=3; 1 deg average grid spacing; 90 x 90 x 6 longitude/latitude/cubeface; 72 levels; top level 0.1 hPa)" = "EAM",
           "EAM (v1.1, cubed sphere spectral-element grid; 5400 elements with p=3; 1 deg average grid spacing; 90 x 90 x 6 longitude/latitude/cubeface; 72 levels; top level 0.1 hPa)" = "EAM",
           "EAM (v2.0, cubed sphere spectral-element grid; 5400 elements, 30x30 per cube face. Dynamics: degree 3 (p=3) polynomials within each spectral element, 112 km average resolution. Physics: 2x2 finite volume cells within each spectral element, 1.5 degree (168 km) average grid spacing; 72 vertical layers; top level 60 km)" = "EAM",
           "IFS cy36r4 (TL255, linearly reduced Gaussian grid equivalent to 512 x 256 longitude/latitude; 91 levels; top level 0.01 hPa)" = "IFS",
           "IFS cy36r4 (TL255, linearly reduced Gaussian grid equivalent to 512 x 256 longitude/latitude; 91 levels; top level 0.01 hPa) and co2box v1.0 (CO2 box model; global grid)" = "IFS",
           "IFS cy36r4 (TL159, linearly reduced Gaussian grid equivalent to 320 x 160 longitude/latitude; 62 levels; top level 5 hPa)" = "IFS",
           "FAMIL2.2 (Cubed-sphere, c96; 360 x 180 longitude/latitude; 32 levels; top level 2.16 hPa)" = "FAMIL2.2",
           "GAMIL3 (180 x 80 longitude/latitude; 26 levels; top level 2.19hPa)" = "GAMIL3",
           "CAM4 (0.9x1.25 finite volume grid; 192 x 288 longitude/latitude; 26 levels; top level ~2 hPa)" = "CAM4",
           "GFDL-AM4.0.1 (Cubed-sphere (c96) - 1 degree nominal horizontal resolution; 360 x 180 longitude/latitude; 33 levels; top level 1 hPa)" = "GFDL-AM4",
           "GFDL-AM4.1 (Cubed-sphere (c96) - 1 degree nominal horizontal resolution; 360 x 180 longitude/latitude; 49 levels; top level 1 Pa)" = "GFDL-AM4",
           "GISS-E2.1 (2.5x2 degree; 144 x 90 longitude/latitude; 40 levels; top level 0.1 hPa)" = "GISS-E2",
           "GISS-E2.1 (2 x 2.5 degrees; 144 x 90 longitude/latitude; 40 levels; top level 0.1 hPa)" = "GISS-E2",
           "GISS-E2.2 (High-top, 2 x 2.5 degrees; 144 x 90 longitude/latitude; 102 levels; top level 0.002 hPa)" = "GISS-E2",
           "GISS-E2.2 (High Top, 2.5x2 degree; 144 x 90 longitude/latitude; 102 levels; top level 0.002 hPa)" = "GISS-E2",
           "GISS-E3 (Cubed sphere, C90; 90 x 90 x 6 gridboxes/cubeface, grid resolution aligns with longitude/latitude along central lines for each cubeface; 102 levels; top level 0.002 hPa)" = "GISS-E3",
           "MetUM-HadGEM3-GA7.1 (N216; 432 x 324 longitude/latitude; 85 levels; top level 85 km)" = "MetUM-HadGEM3-GA7.1",
           "IITM-GFSv1 (T62L64, Linearly Reduced Gaussian Grid; 192 x 94 longitude/latitude; 64 levels; top level 0.2 mb)" = "IITM-GFSv1",
           "INM-AM4-8 (2x1.5; 180 x 120 longitude/latitude; 21 levels; top level sigma = 0.01)" = "INM-AM4-8",
           "INM-AM5-0 (2x1.5; 180 x 120 longitude/latitude; 73 levels; top level sigma = 0.0002)" = "INM-AM5-0",
           "LMDZ (APv5; 96 x 96 longitude/latitude; 39 levels; top level 80000 m)" = "LMDZ",
           "LMDZ (NPv6, N96; 144 x 143 longitude/latitude; 79 levels; top level 80000 m)" = "LMDZ",
           "GFDL-AM2.0 (cubed sphere (C48); 192 x 96 longitude/latitude; 32 vertical levels; top level 2 hPa)" = "GFDL-AM2",
           "R30L14 (3.75 X 2.5 degree (long-lat) configuration; 96 x 80 longitude/latitude; 14 levels; top level 0.015 sigma, 15 mb)" = "R30L14",
           "CCSR AGCM (T85; 256 x 128 longitude/latitude; 81 levels; top level 0.004 hPa)" = "CCSR-AGCM",
           "CCSR AGCM (T42; 128 x 64 longitude/latitude; 40 levels; top level 3 hPa)" = "CCSR-AGCM",
           "ECHAM6.3 (spectral T63; 192 x 96 longitude/latitude; 47 levels; top level 0.01 hPa)" = "ECHAM6.3",
           "ECHAM6.3 (spectral T127; 384 x 192 longitude/latitude; 95 levels; top level 0.01 hPa)" = "ECHAM6.3",
           "MRI-AGCM3.5 (TL159; 320 x 160 longitude/latitude; 80 levels; top level 0.01 hPa)" = "MRI-AGCM3.5",
           "ECHAM v6.3 (T63; 192 x 96 longitude/latitude; 47 levels; top level 1 Pa)" = "ECHAM6.3",
           "CAM-OSLO (2 degree resolution; 144 x 96; 32 levels; top level 3 mb)" = "CAM-OSLO",
           "CAM-OSLO (1 degree resolution; 288 x 192; 32 levels; top level 3 mb)" = "CAM-OSLO",
           "CAM5.3 with UNICON (1deg; 288 x 192 longitude/latitude; 30 levels; top level ~2 hPa)" = "CAM5.3-UNICON",
           "TaiAM1 (0.9x1.25 degree; 288 x 192 longitude/latitude; 30 levels; top level ~2 hPa)" = "TaiAM1",
           "BCC_AGCM3_HR (T266; 800 x 400 longitude/latitude; 56 levels; top level 0.1 hPa)" = "BCC_AGCM3_HR",
           "BCC_AGCM3_LR (T42; 128 x 64 longitude/latitude; 26 levels; top level 2.19 hPa)" = "BCC_AGCM3_LR",
           "IAP AGCM 5.0 (Finite difference dynamical core; 256 x 128 longitude/latitude; 35 levels; top level 2.2 hPa)" = "IAP-AGCM-5.0",
           "CAM5.2 (0.9x1.25 finite volume grid; 288 x 192 longitude/latitude; 32 levels; top level 2.25 mb)" = "CAM5.2",
           "WACCM6 (1.9x2.5 finite volume grid; 144 x 96 longitude/latitude; 70 levels; top level 4.5e-06 mb)" = "WACCM6",
           "CAM4 (1deg; 288 x 192 longitude/latitude; 26 levels; top at ~2 hPa)" = "CAM4",
           "CAM4 (1/4deg; 1152 x 768 longitude/latitude; 26 levels; top at ~2 hPa)" = "CAM4",
           "EAM (v2.0, Dynamics: cubed sphere spectral-element grid, 130,088 columns; Physics: 2x2 finite volume cells within each spectral element, 57,816 columns. N. American (NA): 25 to 100 km; outside ~100 km. 72 vertical layers w/ top at 60 km)" = "EAM",
           "EAM (E3SMv2.1, cubed sphere spectral-element; 5400 els., 30x30 per cube face. Dynamics: degree 3 (p=3) polynomials within each spectral els., 112 km ave. resolution. Physics: 2x2 finite volume cells within each spectral els., 1.5 degree (168 km) average grid spacing; 72 vertical layers w/ top at 60 km)" = "EAM",
           "IFS cy36r4 (TL511, linearly reduced Gaussian grid equivalent to 1024 x 512 longitude/latitude; 91 levels; top level 0.01 hPa)" = "IFS",
           "FAMIL2.2 (Cubed-sphere, c384; 1440 x 720 longitude/latitude; 32 levels; top level 2.16 hPa)" = "FAMIL2.2",
           "GFDL-AM4.0 (Cubed-sphere (c96) - 1 degree nominal horizontal resolution; 360 x 180 longitude/latitude; 33 levels; top level 1 hPa)" = "GFDL-AM4.0",
           "MetUM-HadGEM3-GA7.1 (N512; 1024 x 768 longitude/latitude; 85 levels; top level 85 km)" = "MetUM-HadGEM3-GA7.1",
           "ICON-A (icosahedral/triangles; 160 km; 47 levels; top level 80 km)" = "ICON-A",
           "INM-AM5-H (0.67x0.5; 540 x 360 longitude/latitude; 73 levels; top level sigma = 0.0002)" = "INM-AM5-H",
           "LMDZ (NPv6; 256 x 256 longitude/latitude; 79 levels; top level 80000 m)" = "LMDZ",
           "GFDL-AM2.0 (cubed sphere (C48); 192 x 96 longitude/latitude; 32 vertical levels; top level 2 hPa)" = "GFDL-AM2.0",
           "MRI-AGCM3.2S (TL959; 1920 x 960 longitude/latitude; 64 levels; top level 0.01 hPa)" = "MRI-AGCM3.2S",
           "NICAM.16 (56km icosahedral grid; 163,842 grid cells (=10*4^7+2); 38 levels; top level 40 km)" = "NICAM.16",
           "NICAM.16 (28km icosahedral grid; 655,362 grid cells (=10*4^8+2); 38 levels; top level 40 km)" = "NICAM.16",
           "NICAM.16 (14km icosahedral grid; 2,621,442 grid cells (=10*4^9+2); 38 levels; top level 40 km)" = "NICAM.16",
           "CAM-OSLO4.1 (2 degree resolution; 144 x 96 longitude/latitude; 26 levels; top level ~2 hPa)" = "CAM-OSLO4.1",
           "CAM4 (2 degree resolution; 144 x 96; 32 levels; top level 3 mb)" = "CAM4",
           "Earth1.0-gettingHotter (360 x 180 longitude/latitude; 50 levels; top level 0.1 mb)" = "Earth1.0-gettingHotter"
    )
}

#' @title Individual labels for CMIP6 atmospheric chemistry models
#' @description A helper to simplify the character string to identify the atmospheric chem model,
#' as retrieved from the controlled vocabulary 'CMIP6_source_id' 
#' @param atmosChem.model.description Character string, as retrieved from
#'  the controlled vocabulary 'CMIP6_source_id'
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @note Spaces are not allowed in URIs according to the URI syntax rules.
#'  If a space appears in a URI, it must be percent-encoded as %20.
#'   This is because spaces can cause issues with parsing and can lead
#'    to ambiguities when URIs are used in different contexts. 
#' @author juaco
#' @keywords internal

# ## CMIP6 Activities
# url <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/main/CMIP6_source_id.json"
# ## List of all models
# model.list <- fromJSON(url) %>% extract2("source_id")
# ## Modify grep pattern to match activity accordingly
# ml <- sapply(model.list, "[[", "activity_participation")
# ind <- sapply(ml, "grepl",
#               pattern = "^CMIP$") %>% sapply(., "any") %>% which(isTRUE(.))
# CMIP.models <- model.list[ind]
# ## Atmos chem 
# sapply(1:length(CMIP.models), function(i) {
#     scenMIP.models[[i]][["model_component"]][["atmosChem"]]$description
# }) %>% unique()

set.atmosChem.individual <- function(atmosChem.model.description) {
    switch(atmosChem.model.description,
           "none" = "none",
           "IAP AACM" = "IAP-AACM",
           "MAM4 (same grid as atmos)" = "MAM4",
           "trop_mam4" = "trop_mam4",
           "OZL_v2" = "OZLv2",
           "REPROBUS-C_v2" = "REPROBUS-C_v2",
           "specified oxidants for aerosols" = "specified-oxidants-for-aerosols",
           "Troposphere specified oxidants for aerosols. Stratosphere linearized interactive ozone (LINOZ v2) (same grid as atmos)" = "troposhere-specified-oxidants-for-aerosols_LINOZv2",
           "Troposphere specified oxidants (except passive ozone with the lower boundary sink) for aerosols. Stratosphere linearized interactive ozone (LINOZ v2) (atmos grid)" =  "troposhere-specified-oxidants-for-aerosols_LINOZv2",
           "TM5 (3 x 2 degrees; 120 x 90 longitude/latitude; 34 levels; top level: 0.1 hPa)" = "TM5",
           "fast chemistry, aerosol only" = "fast-chemistry-aerosol-only",
           "GFDL-ATMCHEM4.1 (full atmospheric chemistry)" = "GFDL-ATMCHEM4.1",
           "Varies with physics-version (p==1 Non-interactive, p>1 GPUCCINI)" = "Varies-with-physics-version",
           "varies with physics-version (p==1 Non-interactive, p>1 GPUCCINI)" = "Varies-with-physics-version",
           "INCA v6 NMHC-AER-S" = "INCAv6-NMHC-AER-S",
           "Simple carbon aerosol model (emission type)" = "Simple-carbon-aerosol-model",
           "CHASER4.0 (T42; 128 x 64 longitude/latitude; 81 levels; top level 0.004 hPa)" = "CHASER4.0",
           "sulfur chemistry (unnamed)" = "sulfur-chemistry",
           "MRI-CCM2.1 (T42; 128 x 64 longitude/latitude; 80 levels; top level 0.01 hPa)" = "MRI-CCM2.1",
           "OsloChemSimp" = "OsloChemSimp",
           "SNAP (same grid as atmos)" = "SNAP",
           "UKCA-StratTrop" = "UKCA-StratTrop",
           "BCC-AGCM3-Chem" = "BCC-AGCM3-Chem",
           "MAM3 (same grid as atmos)" = "MAM3 (same grid as atmos)",
           "Troposphere specified oxidants (except passive ozone with the lower boundary sink) for aerosols. Stratosphere linearized interactive ozone (LINOZ v2) (atmos physics grid)" = "troposhere-specified-oxidants-for-aerosols_LINOZv2",
           "OsloChemSimp4.1 (same grid as atmos)" = "OsloChemSimp4.1"
    )
}




#' @title Individual labels for CMIP6 land models
#' @description A helper to simplify the character string to identify the land model,
#' as retrieved from the controlled vocabulary 'CMIP6_source_id' 
#' @param land.model.description Character string, as retrieved from
#'  the controlled vocabulary 'CMIP6_source_id'
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @note Spaces are not allowed in URIs according to the URI syntax rules.
#'  If a space appears in a URI, it must be percent-encoded as %20.
#'   This is because spaces can cause issues with parsing and can lead
#'    to ambiguities when URIs are used in different contexts. 
#' @author juaco
#' @keywords internal

# ## CMIP6 Activities
# url <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/main/CMIP6_source_id.json"
# ## List of all models
# model.list <- fromJSON(url) %>% extract2("source_id")
# ## Modify grep pattern to match activity accordingly
# ml <- sapply(model.list, "[[", "activity_participation")
# ind <- sapply(ml, "grepl",
#               pattern = "^CMIP$") %>% sapply(., "any") %>% which(isTRUE(.))
# CMIP.models <- model.list[ind]
## Land
# sapply(1:length(CMIP.models), function(i) {
#     scenMIP.models[[i]][["model_component"]][["land"]]$description
# }) %>% unique()

set.land.individual <- function(land.model.description) {
    switch(land.model.description,
           "CABLE2.5" = "CABLE2",
           "CABLE2.4" = "CABLE2",
           "JSBACH 3.20" = "JSBACH3.20",
           "JSBACH 3.20 with dynamic vegetation" = "JSBACH3.20-dynamic-veg",
           "BCC_AVIM2" = "BCC_AVIM2",
           "CoLM 1.0" = "CoLM1.0",
           "CoLM" = "CoLM",
           "CLM5 (same grid as atmos)" = "CLM5",
           "CIESM-LM (modified CLM4.5)" = "CIESM-LM",
           "CLM4.5 (BGC mode)" = "CLM4.5",
           "Surfex 8.0c" = "Surfex8.0c",
           "CLASS3.6/CTEM1.2" = "CLASS3.6-CTEM1.2",
           "ELM (v1.0, cubed sphere spectral-element grid; 5400 elements with p=3; 1 deg average grid spacing; 90 x 90 x 6 longitude/latitude/cubeface; satellite phenology mode), MOSART (v1.0, 0.5 degree latitude/longitude grid)" = "ELM_MOSART",
           "ELM (v1.1, same grid as atmos; active biogeochemistry using the Converging Trophic Cascade plant and soil carbon and nutrient mechanisms to represent carbon, nitrogen and phosphorus cycles), MOSART (v1.1, 0.5 degree latitude/longitude grid)" = "ELM_MOSART",
           "ELM (v1.1, same as atmos; active biogeochemistry using the Equilibrium Chemistry Approximation to represent plant and soil carbon and nutrient mechanisms especially carbon, nitrogen and phosphorus limitation), MOSART (v1.1, 0.5 degree latitude/longitude grid)" = "ELM_MOSART",
           "ELM (v1.0, satellite phenology mode, atmos grid), MOSART (v1.0, 0.5 degree latitude/longitude)" = "ELM_MOSART",
           "HTESSEL (land surface scheme built in IFS)" = "HTESSEL",
           "HTESSEL (land surface scheme built in IFS) and LPJ-GUESS v4" = "HTESSEL",
           "HTESSEL (land surface scheme built in IFS) and LPJ-GUESS v4.1.2 (same grid as atmos)" = "HTESSEL",
           "CLM4.0" = "CLM4.0",
           "CAS-LSM" = "CAS-LSM",
           "CLM4.0 (same grid at atmos)" = "CLM4.0",
           "GFDL-LM4.0.1 (1 degree nominal horizontal resolution; 360 x 180 longitude/latitude; 20 levels; bottom level 10m); land-Veg:unnamed (dynamic vegetation, dynamic land use); land-Hydro:unnamed (soil water and ice, multi-layer snow, rivers and lakes)" = "GFDL-LM4.0.1",
           "GFDL-LM4.1" = "GFDL-LM4.1",
           "GISS LSM" = "GISS-LSM",
           "JULES-HadGEM3-GL7.1" = "JULES-HadGEM3-GL7.1",
           "NOAH LSMv2.7.1" = "NOAH-LSMv2.7.1",
           "INM-LND1" = "INM-LND1",
           "ORCHIDEE (IPSLCM5A2.1, Water/Carbon/Energy mode)" = "ORCHIDEE",
           "ORCHIDEE (v2.0, Water/Carbon/Energy mode)" = "ORCHIDEE2.0",
           "NCAR-CLM4" = "NCAR-CLM4",
           "Standard Manabe bucket hydrology scheme (Manabe 1969, doi: 10.1175/1520-0493(1969)097&lt;0739:CATOC&gt;2.3.CO;2)" = "Manabe",
           "MATSIRO6.0+VISIT-e ver.1.0" = "MATSIRO6.0_VISIT-e",
           "MATSIRO6.0" = "MATSIRO6.0",
           "JSBACH3.20" = "JSBACH3.20",
           "HAL 1.0" = "HAL1.0",
           "JSBACH v3.1" = "JSBACH3.1",
           "CLM" = "CLM",
           "CLM4.0 (same grid as atmos)" = "CLM4.0",
           "JULES-ES-1.0" = "JULES-ES-1.0",
           "CLM4 (same grid as atmos)" = "CLM4",
           "CLM4.5 (SP mode)" = "CLM4.5",
           "ELM (v1.0, satellite phenology mode, atmos grid), MOSART (v1.0, 0.125 degree latitude/longitude)" = "ELM_MOSART",
           "ELM (E3SMv2.1, atmos physics grid, satellite phenology mode), MOSART (E3SMv2.1, 0.5 deg lat/lon grid)" = "ELM_MOSART",
           "GFDL-LM4.0" = "GFDL-LM4.0",
           "JSBACH4.20" = "JSBACH4.20",
           "ORCHIDEE (v2.2, Water/Carbon/Energy mode; same grid as atmos)" = "ORCHIDEE2.0",
           "MATSIRO6" = "MATSIRO6",
           "SIB0109" = "SIB0109",
           "MATSIRO6 (w/o MOSAIC)" = "MATSIRO6-MOSAIC",
           "CLM4" = "CLM4",
           "Earth1.0" = "Earth1.0"
    )
}




#' @title Individual labels for CMIP6 land-ice models
#' @description A helper to simplify the character string to identify the land-ice model,
#' as retrieved from the controlled vocabulary 'CMIP6_source_id' 
#' @param landice.model.description Character string, as retrieved from
#'  the controlled vocabulary 'CMIP6_source_id'
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @note Spaces are not allowed in URIs according to the URI syntax rules.
#'  If a space appears in a URI, it must be percent-encoded as %20.
#'   This is because spaces can cause issues with parsing and can lead
#'    to ambiguities when URIs are used in different contexts. 
#' @author juaco
#' @keywords internal

# ## CMIP6 Activities
# url <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/main/CMIP6_source_id.json"
# ## List of all models
# model.list <- fromJSON(url) %>% extract2("source_id")
# ## Modify grep pattern to match activity accordingly
# ml <- sapply(model.list, "[[", "activity_participation")
# ind <- sapply(ml, "grepl",
#               pattern = "^CMIP$") %>% sapply(., "any") %>% which(isTRUE(.))
# CMIP.models <- model.list[ind]
# landice
# sapply(1:length(CMIP.models), function(i) {
#     scenMIP.models[[i]][["model_component"]][["landIce"]]$description
# }) %>% unique()

set.landice.individual <- function(landice.model.description) {
    switch(landice.model.description,
           "none" = "none",
           "CISM2.1" = "CISM2.1",
           "specified ice sheets" = "specified-ice-sheets",
           "PISM v1.2 (5 km x 5 km for Greenland, 31 levels)" = "PISM1.2",
           "GFDL-LM4.0.1" = "GFDL-LM4.0.1",
           "GFDL-LM4.1" = "GFDL-LM4.1",
           "Fixed" = "Fixed",
           "NCAR-CLM4" = "NCAR-CLM4",
           "Specified location - invariant in time, has high albedo and latent heat capacity" = "Time-invariant",
           "none/prescribed" = "none_prescribed",
           "CISM" = "CISM",
           "GFDL-LM4.0" = "GFDL-LM4.0"
    )
}



#' @title Individual labels for CMIP6 ocean models
#' @description A helper to simplify the character string to identify the ocean model,
#' as retrieved from the controlled vocabulary 'CMIP6_source_id' 
#' @param ocean.model.description Character string, as retrieved from
#'  the controlled vocabulary 'CMIP6_source_id'
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @note Spaces are not allowed in URIs according to the URI syntax rules.
#'  If a space appears in a URI, it must be percent-encoded as %20.
#'   This is because spaces can cause issues with parsing and can lead
#'    to ambiguities when URIs are used in different contexts. 
#' @author juaco
#' @keywords internal

# ## CMIP6 Activities
# url <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/main/CMIP6_source_id.json"
# ## List of all models
# model.list <- fromJSON(url) %>% extract2("source_id")
# ## Modify grep pattern to match activity accordingly
# ml <- sapply(model.list, "[[", "activity_participation")
# ind <- sapply(ml, "grepl",
#               pattern = "^CMIP$") %>% sapply(., "any") %>% which(isTRUE(.))
# CMIP.models <- model.list[ind]
# ## ocean
# sapply(1:length(CMIP.models), function(i) {
#     scenMIP.models[[i]][["model_component"]][["ocean"]]$description
# }) %>% unique()

set.ocean.individual <- function(ocean.model.description) {
    switch(ocean.model.description,
           "ACCESS-OM2 (GFDL-MOM5, tripolar primarily 1deg; 360 x 300 longitude/latitude; 50 levels; top grid cell 0-10 m)" = "ACCESS-OM2_GFDL-MOM5",
           "ACCESS-OM2 (MOM5, tripolar primarily 1deg; 360 x 300 longitude/latitude; 50 levels; top grid cell 0-10 m)" = "ACCESS-OM2_MOM5",
           "FESOM 1.4 (unstructured grid in the horizontal with 126859 wet nodes; 46 levels; top grid cell 0-5 m)" = "FESOM1.4",
           "FESOM 1.4 (unstructured grid in the horizontal with 830305 wet nodes; 46 levels; top grid cell 0-5 m)" = "FESOM1.4",
           "MOM4 (1/3 deg 10S-10N, 1/3-1 deg 10-30 N/S, and 1 deg in high latitudes; 360 x 232 longitude/latitude; 40 levels; top grid cell 0-10 m)" = "MOM4",
           "MOM4 (tripolar; 360 x 200 longitude/latitude, primarily 1deg latitude/longitude, down to 1/3deg within 30deg of the equatorial tropics; 50 levels; top grid cell 0-10 m)" = "MOM4",
           "LICOM2.0 (LICOM2.0, primarily 1deg; 362 x 196 longitude/latitude; 30 levels; top grid cell 0-10 m)" = "LICOM2.0",
           "POP2 (320x384 longitude/latitude; 60 levels; top grid cell 0-10 m)" = "POP2",
           "POP2 (320 x 384 longitude/latitude; 60 levels; top grid cell 0-10 m)" = "POP2",                                                                                                                                                                                    
           "CIESM-OM (FD, SCCGrid Displaced Pole; 720 x 560 longitude/latitude; 46 levels; top grid cell 0-6 m)" = "CIESM-OM",
           "NEMO3.6 (ORCA1 tripolar primarly 1 deg lat/lon with meridional refinement down to 1/3 degree in the tropics; 362 x 292 longitude/latitude; 50 vertical levels; top grid cell 0-1 m)" = "NEMO3.6_ORCA1",
           "Nemo 3.6 (eORCA1, tripolar primarily 1deg; 362 x 294 longitude/latitude; 75 levels; top grid cell 0-1 m)" = "NEMO3.6_eORCA1",
           "Nemo 3.6 (eORCA025, tripolar primarily 1/4deg; 1442 x 1050 longitude/latitude; 75 levels; top grid cell 0-1 m)" = "NEMO3.6_eORCA025",
           "NEMO3.4.1 (ORCA1 tripolar grid, 1 deg with refinement to 1/3 deg within 20 degrees of the equator; 361 x 290 longitude/latitude; 45 vertical levels; top grid cell 0-6.19 m)" = "NEMO3.4.1_ORCA1",
           "MPAS-Ocean (v6.0, oEC60to30 unstructured SVTs mesh with 235160 cells and 714274 edges, variable resolution 60 km to 30 km; 60 levels; top grid cell 0-10 m)" = "MPAS-Ocean6.0",
           "MPAS-Ocean (E3SMv2.0, EC30to60E2r2 unstructured SVTs mesh with 236853 cells, 719506 edges, variable resolution 60 to 30 km; 60 levels; top grid cell 0-10 m)" = "MPAS-Ocean_E3SMv2.0",
           "NEMO3.6 (ORCA1 tripolar primarily 1 deg with meridional refinement down to 1/3 degree in the tropics; 362 x 292 longitude/latitude; 75 levels; top grid cell 0-1 m)" = "NEMO3.6_ORCA1",
           "NEMO3.6 (ORCA1 tripolar primarily 1 degree with meridional refinement down to 1/3 degree in the tropics; 362 x 292 longitude/latitude; 75 levels; top grid cell 0-1 m)" = "NEMO3.6_ORCA1",
           "NEMO3.6 (ORCA1 tripolar primarily 1 degree with meridional refinement down to 1/3 degree in the tropics; 362 x 292 longitude/latitude; 75 levels; top grid cell 0-1 m) and MWE v1.0 (Melt Water Emulator; same grid as ocean for Antarctic surroundings)" = "NEMO3.6_ORCA1",
           "LICOM3.0 (LICOM3.0, tripolar primarily 1deg; 360 x 218 longitude/latitude; 30 levels; top grid cell 0-10 m)" = "LICOM3.0",
           "POP2-W (POP2 coupled with MASNUM surface wave model, Displaced Pole; 320 x 384 longitude/latitude; 60 levels; top grid cell 0-10 m)" = "POP2-W",
           "GFDL-OM4p25 (GFDL-MOM6, tripolar - nominal 0.25 deg; 1440 x 1080 longitude/latitude; 75 levels; top grid cell 0-2 m)" = "GFDL-OM4p25",
           "GFDL-OM4p5 (GFDL-MOM6, tripolar - nominal 0.5 deg; 720 x 576 longitude/latitude; 75 levels; top grid cell 0-2 m)" = "GFDL-OM4p5",
           "GISS Ocean (GO1, 1 degree; 360 x 180 longitude/latitude; 40 levels; top grid cell 0-10 m)" = "GISS-Ocean",
           "GISS Ocean (1 deg; 360 x 180 longitude/latitude; 40 levels; top grid cell 0-10m)" = "GISS-Ocean",
           "HYCOM Ocean (~1 degree tripolar grid; 360 x 180 longitude/latitude; 33 levels; top grid cell 0-10 m)" = "HYCOM-Ocean",
           "GISS Ocean (GO1, 1 degree; 360 x 180 longitude/latitude; 40 levels; top grid cell 0-10m)" = "GISS-Ocean",
           "GISS Ocean (1 degree; 360 x 180 longitude/latitude; 32 levels; top grid cell 0-10 m)" = "GISS-Ocean",
           "NEMO-HadGEM3-GO6.0 (eORCA1 tripolar primarily 1 deg with meridional refinement down to 1/3 degree in the tropics; 360 x 330 longitude/latitude; 75 levels; top grid cell 0-1 m)" = "NEMO-HadGEM3-GO6.0",
           "NEMO-HadGEM3-GO6.0 (eORCA025 tripolar primarily 0.25 deg; 1440 x 1205 longitude/latitude; 75 levels; top grid cell 0-1 m)" = "NEMO-HadGEM3-GO6.0",
           "MOM4p1 (tripolar, primarily 1deg; 360 x 200 longitude/latitude; 50 levels; top grid cell 0-10 m)" = "MOM4p1",
           "INM-OM5 (North Pole shifted to 60N, 90E; 360 x 318 longitude/latitude; 40 levels; sigma vertical coordinate)" = "INM-OM5",
           "INM-OM5 (North Pole shifted to 60N, 90E. 0.5x0.25; 720 x 720 longitude/latitude; 40 levels; vertical sigma coordinate)" = "INM-OM5",
           "NEMO-OPA (v3.6, ORCA2 tripolar primarily 2deg; 182 x 149 longitude/latitude; 31 levels; top grid cell 0-10 m)" = "NEMO-OPA3.6-ORCA2",
           "NEMO-OPA (eORCA1.3, tripolar primarily 1deg; 362 x 332 longitude/latitude; 75 levels; top grid cell 0-2 m)" = "NEMO-OPA-eORCA1.3",
           "MOM4p1 (tripolar primarily 1deg; 360 x 200 longitude/latitude; 50 levels; top grid cell 0-10 m)" = "MOM4p1",                                                                                                                                                         
           "GFDL-MOM5.0 (tripolar - nominal 1.0 deg; 360 x 200 longitude/latitude; 52 levels; top grid cell 0-2 m; NK mixed layer scheme)" = "GFDL-MOM5.0",
           "MOM1.0 (MOM1, 1.875 X 2.5 deg; 192 x 80 longitude/latitude; 18 levels; top grid cell 0-40 m)" = "MOM1.0",
           "COCO4.9 (tripolar primarily 1deg; 360 x 256 longitude/latitude; 63 levels; top grid cell 0-2 m)" = "COCO4.9",
           "MPIOM1.63 (bipolar GR1.5, approximately 1.5deg; 256 x 220 longitude/latitude; 40 levels; top grid cell 0-12 m)" = "MPIOM1.63",
           "MPIOM1.63 (tripolar TP04, approximately 0.4deg; 802 x 404 longitude/latitude; 40 levels; top grid cell 0-12 m)" = "MPIOM1.63",
           "MRI.COM4.4 (tripolar primarily 0.5 deg latitude/1 deg longitude with meridional refinement down to 0.3 deg within 10 degrees north and south of the equator; 360 x 364 longitude/latitude; 61 levels; top grid cell 0-2 m)" = "MRI.COM4.4",
           "NEMO v3.4 (NEMO v3.4, tripolar primarily 1deg; 384 x 362 longitude/latitude; 46 levels; top grid cell 0-6 m)" = "NEMO3.4",
           "MICOM (1 degree resolution; 360 x 384; 70 levels; top grid cell minimum 0-2.5 m [native model uses hybrid density and generic upper-layer coordinate interpolated to z-level for contributed data])" = "MICOM",
           "POP2 (Displaced Pole; 320 x 384 longitude/latitude; 60 levels; top grid cell 0-10 m)" = "POP2",
           "FESOM 1.4 (unstructured grid in the horizontal with 1306775 wet nodes; 46 levels; top grid cell 0-5 m)" = "FESOM1.4",
           "FESOM 2 (unstructured grid in the horizontal with 126858 wet nodes; 48 levels; top grid cell 0-5 m)" = "FESOM2",
           "NEMO3.6 (ORCA0.25 1/4 deg from the Equator degrading at the poles; 1442 x 1051 longitude/latitude; 50 vertical levels; top grid cell 0-1 m)" = "NEMO3.6_ORCA0.25",
           "MPAS-Ocean (E3SMv2.0, WC14to60E2r5 unstructured SCVTs mesh with 407420 cells, 1240672 edges, NA: ~14 km; outside: 30 to 60 km; 60 levels; top grid cell 0-10 m)" = "MPAS-Ocean_E3SMv2.0",
           "MPAS-Ocean (E3SMv2.1, EC30to60E2r2 unstructured SVTs mesh with 236853 cells and 719506 edges, variable resolution 60 km to 30 km; 60 levels; top grid cell 0-10 m)" = "MPAS-Ocean_E3SMv2.1",
           "NEMO3.6 (ORCA025 tripolar primarily 0.25 degrees; 1442 x 1921 longitude/latitude; 75 levels; top grid cell 0-1 m)" = "NEMO3.6_ORCA025",
           "LICOM3.0 (LICOM3.0, tripolar primarily 0.1deg; 3600 x 2302 longitude/latitude; 55 levels; top grid cell 0-5 m)" = "LICOM3.0",
           "none" = "none",
           "NEMO-HadGEM3-GO6.0 (eORCA12 tripolar primarily 1/12 deg; 4320 x 3604 longitude/latitude; 75 levels; top grid cell 0-1 m)" = "NEMO-HadGEM3-GO6.0_eORCA12",
           "ICON-O (icosahedral/triangles; 40 km; 40 levels; top grid cell 0-12 m)" = "ICON-O",
           "INM-OM5-H (North Pole shifted to 60N, 90E. 0.167x0.125; 2160x1440 longitude/latitude; 40 levels; vertical sigma coordinate)" = "INM-OM5-H",
           "MICOM1.1 (1 degree resolution; 320 x 384 longitude/latitude; 53 levels; top grid cell 0-2.5 m [native model uses hybrid density and generic upper-layer coordinate interpolated to z-level for contributed data])" = "MICOM1.1",
           "BlueMarble1.0-warming (360 x 180 longitude/latitude; 50 levels; top grid cell 0-10 m)" = "BlueMarble1.0-warming",
           "POP2 (Displaced Pole; 320 x 384 longitude/latitude; 60 levels; top grid cell 0-10 m)" = "POP2",
           "TIMCOM (TIMCOMv1.7, primarily 1deg; 360 x 288 longitude/latitude; 45 levels; top grid cell 0-10 m)" = "TIMCOM1.7",
           "TIMCOM (TIMCOMv2.2, primarily 1deg; 320 x 288 longitude/latitude; 55 levels; top grid cell 0-10 m)" = "TIMCOM2.2"
    )
}




#' @title Individual labels for CMIP6 ocean biogeochem models
#' @description A helper to simplify the character string to identify the ocnBgchem model,
#' as retrieved from the controlled vocabulary 'CMIP6_source_id' 
#' @param ocnBgchem.model.description Character string, as retrieved from
#'  the controlled vocabulary 'CMIP6_source_id'
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @note Spaces are not allowed in URIs according to the URI syntax rules.
#'  If a space appears in a URI, it must be percent-encoded as %20.
#'   This is because spaces can cause issues with parsing and can lead
#'    to ambiguities when URIs are used in different contexts. 
#' @author juaco
#' @keywords internal

# ## ocnBgchem
# sapply(1:length(scenMIP.models), function(i) {
#     scenMIP.models[[i]][["model_component"]][["ocnBgchem"]]$description
# }) %>% unique()
# cbind.data.frame(
#     "desc" = sapply(1:length(scenMIP.models), function(i) scenMIP.models[[i]][["model_component"]][["ocean"]]$description),
#     "res" = sapply(1:length(scenMIP.models), function(i) scenMIP.models[[i]][["model_component"]][["ocean"]]$native_nominal_resolution)
# )

set.ocnBgchem.individual <- function(ocnBgchem.model.description) {
    switch(ocnBgchem.model.description,
           "none" = "none",
           "WOMBAT (same grid as ocean)" = "WOMBAT",
           "REcoM2 (same grid as ocean component)" = "REcoM2",
           "IAP OBGCM" = "IAP-OBGCM",
           "MARBL (same grid as ocean)" = "MARBL",
           "BFM5.2" = "BFM5.2",
           "Pisces 2.s" = "PISCES",
           "Canadian Model of Ocean Carbon (CMOC); NPZD ecosystem with OMIP prescribed carbonate chemistry" = "CMOC",
           "Canadian Ocean Ecosystem (CanOE) with OMIP prescribed carbon chemistry" = "CanOE",
           "BEC (Biogeochemical Elemental Cycling model, NPZD-type with C/N/P/Fe/Si/O; same grid as ocean)" = "BEC",
           "PISCES v2" = "PISCES",
           "PISCES v2 (same grid as ocean)" = "PISCES",
           "GFDL-BLINGv2" = "GFDL-BLINGv2",
           "GFDL-COBALTv2" = "GFDL-COBALTv2",
           "NOBM (NASA Ocean Biogeochemistry Model; same grid as ocean)" = "NOBM",
           "TOPAZv2.0" = "TOPAZ2.0",
           "NEMO-PISCES" = "NEMO-PISCES",
           "TOPAZ2" = "TOPAZ2.0",
           "OECO ver.2.0; NPZD-type with C/N/P/Fe/O cycles" = "OECO2.0",
           "HAMOCC6" = "HAMOCC",
           "MRI.COM4.4" = "MRI.COM4.4",
           "HAMOCC" = "HAMOCC",
           "MEDUSA2" = "MEDUSA2"
    )
}



#' @title Individual labels for CMIP6 sea-ice models
#' @description A helper to simplify the character string to identify the sea-ice model,
#' as retrieved from the controlled vocabulary 'CMIP6_source_id' 
#' @param seaice.model.description Character string, as retrieved from
#'  the controlled vocabulary 'CMIP6_source_id'
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @note Spaces are not allowed in URIs according to the URI syntax rules.
#'  If a space appears in a URI, it must be percent-encoded as %20.
#'   This is because spaces can cause issues with parsing and can lead
#'    to ambiguities when URIs are used in different contexts. 
#' @author juaco
#' @keywords internal

# ## seaIce
# sapply(1:length(scenMIP.models), function(i) {
#     scenMIP.models[[i]][["model_component"]][["seaIce"]]$description
# }) %>% unique()
# cbind.data.frame(
#     "desc" = sapply(1:length(scenMIP.models), function(i) scenMIP.models[[i]][["model_component"]][["ocean"]]$description),
#     "res" = sapply(1:length(scenMIP.models), function(i) scenMIP.models[[i]][["model_component"]][["ocean"]]$native_nominal_resolution)
# )

set.seaice.individual <- function(seaice.model.description) {
    switch(seaice.model.description,
           "CICE5.1.2 (same grid as ocean)" = "CICE5.1.2",
           "CICE4.1 (same grid as ocean)" = "CICE4.1",
           "FESOM 1.4" = "FESOM1.4",
           "FESOM 1.4 (same grid as ocean component)" = "FESOM1.4",
           "SIS2" = "SIS2",
           "SIS 1.0" = "SIS1.0",
           "CICE4" = "CICE4",
           "CICE5.1 (same grid as ocean)" = "CICE5.1",
           "CICE4.0" = "CICE4.0",
           "Gelato 6.1" = "Gelato6.1",
           "LIM2" = "LIM2",
           "MPAS-Seaice (v6.0, same grid as ocean)" = "MPAS-Seaice",
           "MPAS-Seaice (v6.0; same grid as ocean)" = "MPAS-Seaice",
           "MPAS-Seaice (E3SMv2.0, ocean grid; 5 ice categories; 7 ice, 5 snow layers)" = "MPAS-Seaice",
           "LIM3" = "LIM3",
           "LIM3 (same grid as ocean)" = "LIM3",
           "CICE4.0 (same grid as ocean)" = "CICE4.0",
           "GFDL-SIM4p25 (GFDL-SIS2.0, tripolar - nominal 0.25 deg; 1440 x 1080 longitude/latitude; 5 layers; 5 thickness categories)" = "GFDL-SIM4p25",
           "GFDL-SIM4p5 (GFDL-SIS2.0, tripolar - nominal 0.5 deg; 720 x 576 longitude/latitude; 5 layers; 5 thickness categories)" = "GFDL-SIM4p5",
           "GISS SI" = "GISS-SI",
           "GISS SI (same grid as ocean)" = "GISS-SI",
           "GISS SI (same grid as atmos)" = "GISS-SI",
           "CICE-HadGEM3-GSI8 (eORCA1 tripolar primarily 1 deg; 360 x 330 longitude/latitude)" = "CICE-HadGEM3-GSI8",
           "CICE-HadGEM3-GSI8 (eORCA025 tripolar primarily 0.25 deg; 1440 x 1205 longitude/latitude)" = "CICE-HadGEM3-GSI8",
           "SISv1.0" = "SIS1.0",
           "INM-ICE1" = "INM-ICE1",
           "NEMO-LIM2" = "NEMO-LIM2",
           "NEMO-LIM3" = "NEMO-LIM3",
           "CICE-HadGEM3-GSI8 (tripolar primarily 1deg; 360 x 200 longitude/latitude)" = "CICE-HadGEM3-GSI8",
           "GFDL-SIS" = "GFDL-SIS",
           "Thermodynamic ice model (free drift dynamics)" = "thermodynamic",
           "COCO4.9" = "COCO4.9",
           "unnamed (thermodynamic (Semtner zero-layer) dynamic (Hibler 79) sea ice model)",
           "MRI.COM4.4" = "thermodynamic",
           "CICE4.1" = "CICE4.1",
           "CICE" = "CICE"
    )
}


#' @title Individual labels for ScenarioMIP6 model components
#' @description A helper to simplify the character string to identify the model
#' components, as retrieved from the controlled vocabulary 'CMIP6_source_id' 
#' @param model.comp Character string, as retrieved from
#' the controlled vocabulary 'CMIP6_source_id'. One of the following:
#' \code{c("aerosol", "atmos", "atmosChem", "land", "landIce", "ocean", "ocnBgchem", "seaIce")}.
#' @param descr Model description list item,
#' as encoded in 'CMIP6_source_id.json' 
#' @return A 'simplified' character string to identify the individual,
#'  leaving details for the dc:description field.
#' @author juaco
#' @keywords internal

setIndividual <- function(model.comp, descr) {
    model.comp <- match.arg(model.comp,
                            choices = c("aerosol", "atmos", "atmosChem",
                                        "land", "landIce", "ocean",
                                        "ocnBgchem", "seaIce")
    )
    switch (model.comp,
            "aerosol" = set.aerosol.individual(descr),
            "atmos" =  set.atmos.individual(descr),
            "atmosChem" = set.atmosChem.individual(descr),
            "land" =  set.land.individual(descr),
            "landIce" = set.landice.individual(descr),
            "ocean" = set.ocean.individual(descr),
            "ocnBgchem" = set.ocnBgchem.individual(descr),
            "seaIce" = set.seaice.individual(descr)
    )
}




#' @title Set Model Component Class
#' @description
#' Assigns the appropriate model component according to datasource ontology
#' @param model.com Character string, as retrieved from
#' the controlled vocabulary 'CMIP6_source_id'. One of the following:
#' \code{c("aerosol", "atmos", "atmosChem", "land", "landIce", "ocean", "ocnBgchem", "seaIce")}.
#' @return A character string asserting the appropriate class
#' @references \url{http://www.metaclip.org/datasource/0.26/datasource.owl}
#' @note Classes codified for datasource version>=0.26 
#' @author juaco
#' @keywords internal

setModelComponentClass <- function(model.comp) {
    model.comp <- match.arg(model.comp,
                            choices = c("aerosol", "atmos", "atmosChem",
                                        "land", "landIce", "ocean",
                                        "ocnBgchem", "seaIce")
    )
    switch (model.comp,
            "aerosol" = "AerosolModel",
            "atmos" = "AtmosModel",
            "atmosChem" = "AtmosChemModel",
            "land" = "LandSurfaceModel",
            "landIce" = "LandIceModel",
            "ocean" = "OceanModel",
            "ocnBgchem" = "OceanBgchemModel",
            "seaIce" = "SeaIceModel"
    )
}




#' @title CMIP6 CMOR tables URL retrieval
#' @description
#' Retrieve the URL of CMIP6 CMOR tables given their short name identifier 
#' @param cmip6.table  character string with table designation. 
#' Currently accepted values are: \code{"day","fx","Ofx","Emon","LImon","Amon","Omon","SImon"}
#' @return A valid url to read the table from
#' @details
#' The URLs correspond to the main branch of the PCDMI Github repo
#' @source \url{https://github.com/PCMDI/cmip6-cmor-tables/}
#' @author juaco
#' @keywords internal

set.CMORtable.url <- function (cmip6.table) {
    switch(cmip6.table,
           "day" = "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_day.json",
           "fx" = "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_fx.json",
           "Ofx" = "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_Ofx.json",
           "Emon" = "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_Emon.json",
           "LImon" = "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_LImon.json", 
           "Amon" = "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_Amon.json",
           "Omon" = "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_Omon.json",
           "SImon" = "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_SImon.json"
    )
}



#' @title Special symbol treatment
#' @description
#' Replaces special symbols in RDF (currently handles \dQuote{>} and \dQuote{<})
#' replacing them by an adequate XML escape sequence in strings
#' @param string String to check and fix if needed
#' @return Character string with XML escaped sequences where needed
#' @author juaco
#' @keywords internal

fix.XML.char <- function(string) {
    if (grepl("<", string)) {
        string <- gsub("<", "&lt;", string)    
    }
    if (grepl(">", string)) {
        string <- gsub("<", "&gt;", string)    
    }
}

