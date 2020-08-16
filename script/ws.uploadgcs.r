library(googleCloudStorageR)



#Set environment variables
Sys.setenv("GCS_DEFAULT_BUCKET" = 'appraisals-data-dev-c55fa4-wholesale-value-etl',
           "GCS_AUTH_FILE" = "C:/Users/vanessa.li/AppData/Roaming/gcloud/application_default_credentials.json")

Sys.setenv("GCS_DEFAULT_BUCKET" = 'appraisals-data-dev-c55fa4-wholesale-value-etl',
           "GCS_AUTH_FILE" = "C:/Users/vanessa.li/Documents/GitHub/Wholesale-values/script/application_default_credentials.json")


gcs_auth('C:/Users/vanessa.li/AppData/Roaming/gcloud/application_default_credentials.json')
## GCS_AUTH_FILE set so auto-authentication
gcs_get_bucket("GCS_DEFAULT_BUCKET")


## upload an R data.frame directly, with a custom function
f <- function(input, output) write.csv(input, row.names = FALSE, file = output)

gcs_upload(importtable, name = "upload/WholeSaleImport.csv", object_function = f)
