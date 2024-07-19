
source("R/model_component_helpers.R")

## /////////////////////////////////////////////////////////////////////////////
## READ FROM REMOTE CMIP6 CONTROLLED VOCABULARY
## /////////////////////////////////////////////////////////////////////////////

## CMIP6 Activities
url <- "https://raw.githubusercontent.com/WCRP-CMIP/CMIP6_CVs/main/CMIP6_source_id.json"

## List of all models
model.list <- fromJSON(url) %>% extract2("source_id")

## Filter Models participating in "ScenarioMIP" AND/OR "CMIP" activities
ml <- sapply(model.list, "[[", "activity_participation")
ind <- sapply(ml, "grepl",
              pattern = "ScenarioMIP|CMIP") %>% sapply(., "any") %>% which(isTRUE(.)) 

## Final list 
## NOTE: this includes models participating in ScenarioMIP and CMIP activities
## object name preserved for backwards script compatibility
scenMIP.models <- model.list[ind] 
# names(scenMIP.models)


## /////////////////////////////////////////////////////////////////////////////
## CREATE OWL FILE
## /////////////////////////////////////////////////////////////////////////////

## OWL template file (contains header and imports)
## Use 'restart.owl()' to start from blank template
## The template already contains the OWL header with relevant imports and metadata
## Please check the 'datasource' ontology version dependency

owl.file <- "CMIP6-models.owl"
restart.owl(voc = "models", version = "0.0")
# Open file in add mode

con <- file(owl.file, open = "a")

# Vocabulary name

# voc <- "http://www.metaclip.org/ScenarioMIP-models.owl"
voc <- "https://raw.githubusercontent.com/metaclip/CMIP6/devel/CMIP6-models.owl"

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Include individuals ---------------------------------------------------------
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

model.components <- c("aerosol", "atmos", "atmosChem",
                      "land", "landIce", "ocean",
                      "ocnBgchem", "seaIce")

# cat("\t<!--\n", file = con, append = TRUE) 
# cat("\t///////////////////////////////////////////////////////////////////////////////////////\n",
#     file = con, append = TRUE)
# cat("\t//\n", file = con, append = TRUE)
# cat("\t// Individuals\n", file = con, append = TRUE)
# cat("\t//\n", file = con, append = TRUE)
# cat("\t///////////////////////////////////////////////////////////////////////////////////////\n",
#     file = con, append = TRUE)
# cat("\t-->\n\n", file = con, append = TRUE)

cat("\n\n\n", file = con, append = TRUE)

for (i in 1:length(scenMIP.models)) {
    
    ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    ## GCMs --------------------------------------------------------------------
    ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    gcm.name <- scenMIP.models[[i]]$source_id
    
    if (gcm.name != "PCMDI-test-1-0") {
        
        gcm.longname <- scenMIP.models[[i]]$label_extended
        gcm.rel.year <- scenMIP.models[[i]]$release_year
        gcm.lic <- scenMIP.models[[i]]$license_info$license
        gcm.bugreports <- scenMIP.models[[i]]$license_info$exceptions_contact
        gcm.institution <- scenMIP.models[[i]]$institution_id
        
        # NamedIndividual instance:
        instance <- paste0(voc, "#", gcm.name)
        
        cat("\t<!--\n", file = con, append = TRUE)
        cat("\t\t", instance, "\n", file = con, append = TRUE)
        cat("\t-->\n", file = con, append = TRUE)
        
        ## Write GCM individual and class ------------------------------------------
        cat("\n", file = con, append = TRUE)
        cat("\t<owl:NamedIndividual rdf:about=\"", instance, "\">",
            sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        cat("\t\t<rdf:type rdf:resource=\"http://www.metaclip.org/datasource/datasource.owl#GCM\"/>",
            file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        ## Additional properties:
        ## GCM label
        cat("\t\t<rdfs:label>", gcm.name, "</rdfs:label>", sep = "",
            file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        ## GCM long name
        cat("\t\t<dc:description xml:lang=\"en\">", gcm.longname,
            "</dc:description>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        ## GCM release year
        cat("\t\t<ds:release_year>", gcm.rel.year, "</ds:release_year>",
            sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        ## GCM License
        cat("\t\t<ds:license>", gcm.lic, "</ds:license>",
            sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        ## GCM bugreports
        if (!is.null(gcm.bugreports)) {
            if (grepl("@", gcm.bugreports)) {
                gcm.bugreports <- fix.contact.info(gcm.bugreports)
            }
        }
        cat("\t\t<ds:exceptions_contact>", gcm.bugreports,
            "</ds:exceptions_contact>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        cat("\t</owl:NamedIndividual>", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        ## GCM components ----------------------------------------------------------
        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        
        
        for (j in 1:length(model.components)) {
            
            # print(paste0(i, "-", j)) # for debugging
            
            model <- scenMIP.models[[i]][["model_component"]][[model.components[j]]]
            desc <- model$description
            name <- setIndividual(model.comp = model.components[j], descr = desc)
            
            ## NamedIndividual not created if component is none:
            if (name != "none") {
                
                res <- model$native_nominal_resolution
                
                ## NamedIndividual instance:
                ## This is the unique individual identifier in the vocabulary
                ## It is built upon the GCM name + its specific model component
                ## Note that the same model component may be used in different GCMs, 
                ## but in this case the nominal resolution may differ (as well
                ## as -possibly- other model characteristics), and therefore each unique
                ## combination of GCM and model component is defined as a
                ## unique individual in the vocabulary
                
                ind.name <- paste(gcm.name, model.components[j], name, sep = "_")
                instance <- paste0(voc, "#", ind.name)
                
                cat("\n", file = con, append = TRUE)
                cat("\t<!--\n", file = con, append = TRUE)
                cat("\t\t", instance, "\n", file = con, append = TRUE)
                cat("\t-->\n", file = con, append = TRUE)
                cat("\n", file = con, append = TRUE)
                
                cat("\t<owl:NamedIndividual rdf:about=\"", instance, "\">", sep = "",
                    file = con, append = TRUE)
                cat("\n", file = con, append = TRUE)
                
                ## Individual Parent Class
                mc.class <- setModelComponentClass(model.components[j])
                cat("\t\t<rdf:type rdf:resource=\"http://www.metaclip.org/datasource/datasource.owl#",
                    mc.class, "\"/>", sep = "", file = con, append = TRUE)
                cat("\n", file = con, append = TRUE)
                cat("\t\t<dc:description xml:lang=\"en\">", desc, "</dc:description>",
                    sep = "", file = con, append = TRUE)
                cat("\n", file = con, append = TRUE)
                cat("\t\t<rdfs:label>", name, "</rdfs:label>", sep = "", file = con, append = TRUE)
                cat("\n", file = con, append = TRUE)
                cat("\t\t<ds:hasNominalResolution>", res, "</ds:hasNominalResolution>",
                    sep = "", file = con, append = TRUE)
                cat("\n", file = con, append = TRUE)
                cat("\t\t<rdfs:comment xml:lang=\"en\">Nominal resolution corresponds to the native model's resolution</rdfs:comment>",
                    sep = "", file = con, append = TRUE)
                cat("\n", file = con, append = TRUE)
                cat("\t</owl:NamedIndividual>", file = con, append = TRUE)
                cat("\n", file = con, append = TRUE)
            }
        }
    }
}

cat("</rdf:RDF>\n\n", sep = "", file = con, append = TRUE)

cat("<!-- ", file = con, append = TRUE)
cat("Automatically generated by '0_import_CMIP6_models.R' on", as.character(Sys.Date()),
    "using", gsub("\\(.*", "", R.version.string),
    "<https://github.com/METACLIP/CMIP6>", file = con, append = TRUE)
cat(" -->\n", file = con, append = TRUE)
## Close connection 
close(con)

