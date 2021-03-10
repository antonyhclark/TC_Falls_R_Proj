# get credential from file in unix home ####
pwd_file <- read_csv("~/pwd.csv")
usr <- as.character(pwd_file[1, "user"])
pwd <- as.character(pwd_file[1, "pwd"])

# create channel to connect to Oracle DB 
channel <- suppressWarnings(
  dbConnect(
    odbc(),
    dsn = "SMRA",
    uid = usr,
    pwd = pwd,
    # pwd = .rs.askForPassword("What is your LDAP password?"),
    port = "1527",
    host = "nssstats01.csa.scot.nhs.uk",
    SVC = "SMRA.nss.scot.nhs.uk"
  )
)
get_smr_data(query = "SELECT * FROM v$version;")

get_smr_data(query = 
               "SELECT LINK_NO, CIS_MARKER, MAIN_CONDITION
             FROM ANALYSIS.SMR01_PI
             WHERE REGEXP_LIKE (MAIN_CONDITION, 'W[00-19]')")


# query by dplyr ####
start_time <- Sys.time()
tbl_SMR01_PI <- tbl(channel, "SMR01_PI") %>% 
  select(
    LINK_NO,
    URI,
    UPI_NUMBER,
    CIS_MARKER,
    ADMISSION_TYPE,
    ADMISSION_DATE,
    DISCHARGE_DATE,
    MAIN_CONDITION,
    OTHER_CONDITION_1,
    OTHER_CONDITION_2,
    OTHER_CONDITION_3,
    OTHER_CONDITION_4,
    OTHER_CONDITION_5,
    AGE_IN_YEARS,
    SEX,
    LENGTH_OF_STAY,
    DATAZONE_2011
  ) %>%
  group_by(LINK_NO,CIS_MARKER) %>% 
  filter(ADMISSION_DATE>=to_date('2015-04-01','YYYY/MM/DD'),
         ADMISSION_DATE<=to_date('2020-03-01','YYYY/MM/DD')) %>% 
  arrange(LINK_NO,
          CIS_MARKER,
          ADMISSION_DATE,
          DISCHARGE_DATE,
          ADMISSION_TYPE)
end_time <- Sys.time()
exec_time <- end_time - start_time

tbl_SMR01_PI %>% data.table::as.data.table()



