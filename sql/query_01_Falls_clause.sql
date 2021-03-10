SELECT
  LINK_NO, URI, UPI_NUMBER, CIS_MARKER, ADMISSION_TYPE,
  ADMISSION_DATE, DISCHARGE_DATE,
  MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2,
  OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,
  AGE_IN_YEARS, SEX, LENGTH_OF_STAY, COUNCIL_AREA_2019, DATAZONE_2011
FROM
  ANALYSIS.SMR01_PI
WHERE
  (ADMISSION_DATE >= TO_DATE('%s','yyyy/mm/dd')) AND
  (ADMISSION_DATE <= TO_DATE('%s','yyyy/mm/dd')) AND
  (
  REGEXP_LIKE (MAIN_CONDITION, 'W[00-19]') OR
  REGEXP_LIKE (OTHER_CONDITION_1, 'W[00-19]') OR
  REGEXP_LIKE (OTHER_CONDITION_2, 'W[00-19]') OR
  REGEXP_LIKE (OTHER_CONDITION_3, 'W[00-19]') OR
  REGEXP_LIKE (OTHER_CONDITION_4, 'W[00-19]') OR
  REGEXP_LIKE (OTHER_CONDITION_5, 'W[00-19]')
  
  )
  
ORDER BY
  LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION_TYPE DESC