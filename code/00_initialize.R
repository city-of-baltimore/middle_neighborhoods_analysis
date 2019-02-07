

suppressMessages(library(tidyverse))
suppressMessages(library(here))
suppressMessages(library(ggiteam))


VARS <- list()

# Initialize directories
VARS$CODE <- here("code/")
VARS$RAW_DATA <- here("data/raw/")
VARS$PROCESSED_DATA <- here("data/processed/")
VARS$NOTEBOOKS <- here("notebooks/")
VARS$OUTPUT_FIGS <- here("output/figs/")
VARS$OUTPUT_TABLES <- here("output/tables/")
#VARS$GOOGLE_DRIVE <- Sys.getenv("GOOGLE_DRIVE")


# Server credentials in env variables
VARS$EGIS_SERVER <- Sys.getenv("EGIS_SERVER")
VARS$EGIS_SERVER_USER <- Sys.getenv("EGIS_SERVER_USERNAME")
VARS$EGIS_SERVER_PWD <-  Sys.getenv("EGIS_SERVER_PWD")

VARS$SQL_SERVER <- Sys.getenv("SQL_SERVER")

source(here("code", "01_helper_functions.R"))