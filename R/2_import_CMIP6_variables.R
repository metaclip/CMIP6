
source("R/model_component_helpers.R")

## /////////////////////////////////////////////////////////////////////////////
## READ FROM REMOTE CMIP6 CONTROLLED VOCABULARY
## /////////////////////////////////////////////////////////////////////////////

# ## Monthly variables Amon (realm: atmos, atmosChem)
# url.Amon <- "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_Amon.json"
# 
# ## Monthly variables Omon (realm: ocnBgchem)
# url.Omon <- "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_Omon.json"
# 
# ## Monthly variables Emon (realm: land)
# url.Emon <- "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_Emon.json"
# 
# ## Monthly variables SImon (realm: seaIce)
# url.SImon <- "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_SImon.json"
# 
# ## Monthly variables LImon (realm: landIce land)
# url.LImon <- "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_LImon.json"
# 
# ## Daily variables day (realm: atmos)
# url.day <- "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_day.json"
# 
# ## Fixed variables fx
# url.fx <- "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_fx.json"
# 
# ## Fixed variables Ofx (realm: ocean)
# url.Ofx <- "https://raw.githubusercontent.com/PCMDI/cmip6-cmor-tables/main/Tables/CMIP6_Ofx.json"


## Reference table of C3S CMIP6 variables and corresponding CMIP6 tables
ref <- read.csv("aux/C3S_singleLevel_vars/refVarTables.csv")
cmip6.tables <- ref$CMIP6_table %>% unique()

## /////////////////////////////////////////////////////////////////////////////
## CREATE OWL FILE
## /////////////////////////////////////////////////////////////////////////////

## OWL template file (contains header and imports)
## Use 'restart.owl()' to start from blank template
## The template already contains the OWL header with relevant imports and metadata
## Please check the 'datasource' ontology version dependency

owl.file <- "CMIP6-variables.owl"
restart.owl(voc = "variables", version = "0.0")

# Open file in add mode
con <- file(owl.file, open = "a")

## VOC URI
voc <- "https://raw.githubusercontent.com/metaclip/CMIP6/main/CMIP6-variables.owl"



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

for (i in 1:length(cmip6.tables)) {
    
    c6table <- cmip6.tables[i]
    
    table.url <- set.CMORtable.url(c6table)
    data <- fromJSON(table.url) 
    header <- data$Header
    variable.info <- data$variable_entry
    
    
    vars <- ref[which(ref$CMIP6_table == c6table), 4]
    
    for (j in 1:length(vars)) {
        
        shortname <- vars[j]
        inst <- paste0("CMIP6_", c6table, "-", shortname)
        
        # NamedIndividual instance:
        instance <- paste0(voc, "#", inst)
        
        cat("\t<!--\n", file = con, append = TRUE)
        cat("\t\t", instance, "\n", file = con, append = TRUE)
        cat("\t-->\n", file = con, append = TRUE)
        
        ## Write Variable individual and class ---------------------------------
        
        cat("\n", file = con, append = TRUE)
        cat("\t<owl:NamedIndividual rdf:about=\"", instance, "\">",
            sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        cat("\t\t<rdf:type rdf:resource=\"https://metaclip.org/datasource/datasource.owl#Variable\"/>",
            file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## Additional properties -----------------------------------------------
        var.attrs <- variable.info[[shortname]]
        
        ## Var label
        cat("\t\t<rdfs:label>", inst, "</rdfs:label>", sep = "",
            file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## dimensions
        cat("\t\t<ds:dimensions rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">",
            var.attrs$dimensions, "</ds:dimensions>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## Comment
        comment <- fix.XML.char(var.attrs$comment) 
        cat("\t\t<rdfs:comment xml:lang=\"en\">", comment, "</rdfs:comment>",
            sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## Standard name
        cat("\t\t<ds:standard_name rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">",
            var.attrs$standard_name, "</ds:standard_name>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## Var long name
        cat("\t\t<ds:hasLongName rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">",
            var.attrs$long_name, "</ds:hasLongName>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## Units
        cat("\t\t<ds:withUnits rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">",
            var.attrs$units, "</ds:withUnits>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## Cell methods
        cat("\t\t<ds:hasCellMethods rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">",
            var.attrs$cell_methods, "</ds:hasCellMethods>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## Cell measures
        cat("\t\t<ds:cell_measures rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">",
            var.attrs$cell_measures, "</ds:cell_measures>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        
        ## Modelling realm
        cat("\t\t<ds:modelling_realm rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">",
            var.attrs$modeling_realm, "</ds:modelling_realm>", sep = "", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
        cat("\t</owl:NamedIndividual>\n", file = con, append = TRUE)
        cat("\n", file = con, append = TRUE)
    }
}

cat("</rdf:RDF>\n\n", sep = "", file = con, append = TRUE)

cat("<!-- ", file = con, append = TRUE)
cat("Automatically generated by '2_import_CMIP6_variables.R' on", as.character(Sys.Date()),
    "using", gsub("\\(.*", "", R.version.string),
    "<https://github.com/METACLIP/CMIP6>", file = con, append = TRUE)
cat(" -->\n", file = con, append = TRUE)
## Close connection 
close(con)

