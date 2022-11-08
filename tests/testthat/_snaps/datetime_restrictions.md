# correct functioning of apply time restrictions

    Code
      test
    Output
      $sql
      [1] "SELECT patient_num FROM i2b2miracum.patient_dimension"
      
      $sql_extended
      [1] "-- Create the VIEWs:\nCREATE TEMPORARY VIEW patient_dimension__dqa_tmp AS (SELECT * FROM patient_dimension WHERE start_date BETWEEN '2010-01-01' AND '2015-12-31');\nCREATE TEMPORARY VIEW visit_dimension__dqa_tmp AS (SELECT * FROM visit_dimension WHERE start_date BETWEEN '2010-01-01' AND '2015-12-31');\n\n-- The actual SQL to extract the data:\nSELECT patient_num FROM i2b2miracum.patient_dimension;\n\n-- If needed, drop the temporal VIEWs:\nDROP VIEW patient_dimension__dqa_tmp;\nDROP VIEW visit_dimension__dqa_tmp;"
      
      $sql_create_view_all
      $sql_create_view_all$patient_dimension__dqa_tmp
      [1] "CREATE TEMPORARY VIEW patient_dimension__dqa_tmp AS (SELECT * FROM patient_dimension WHERE start_date BETWEEN '2010-01-01' AND '2015-12-31')"
      
      $sql_create_view_all$visit_dimension__dqa_tmp
      [1] "CREATE TEMPORARY VIEW visit_dimension__dqa_tmp AS (SELECT * FROM visit_dimension WHERE start_date BETWEEN '2010-01-01' AND '2015-12-31')"
      
      

# correct functioning of get_restricting_date_info

    Code
      test
    Output
      [1] "Period considered: 2010-01-01, 00:00:00 to 2011-01-01, 00:00:00"

