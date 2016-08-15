
#' Test rendering query
#' 
#' @details
#' for testing
#' 
#' @return
#' a query form string
#' 
#' @export
renderTest<-function(){
  SqlRender::loadRenderTranslateSql("temp.sql",
                                    packageName="Cert",
                                    dbms="sql server")
}

#' Test query database
#' 
#' @details
#' for testing
#' 
#' @param connectionDetails
#' connectionDetails information
#' 
#' @return
#' a data frame
#' 
#' @export
queryTest<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  data<-DatabaseConnector::querySql(conn, "select top 10 * from [OMOP_5].[dbo].CONCEPT;")
  dbDisconnect(conn)
  data
}

#' Generate Cert dataset
#' 
#' @details
#' initial testing
#' 
#' @return
#' nothing
#' 
#' @export
generateCertDataSet<-function(connectionDetails, drug_list=NA, labtest_list=NA
                              , date_from='2001-01-01', date_to='2010-03-31'){
  conn<-DatabaseConnector::connect(connectionDetails)

  if(!is.na(drug_list)){
    DatabaseConnector::insertTable(conn, "TARGET_DRUG", drug_list)
  }
  if(!is.na(labtest_list)){
    DatabaseConnector::insertTable(conn, "LABTEST_LIST", labtest_list)
  }
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.6_CDMv4_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database,
                                                 date_from=date_from,
                                                 date_to=date_to)
  
  DatabaseConnector::executeSql(conn, renderedSql)
  dbDisconnect(conn)
}

#' Get Cert result dataset
#' 
#' @details
#' initial testing
#' 
#' @return
#' a data frame from summary table
#' 
#' @export
getCertResultDataSet<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.5_CDMv4_09.Summary_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database)
  data<-DatabaseConnector::querySql(conn, renderedSql)
  dbDisconnect(conn)
  data
}

#' Get Cert demographics dataset
#' 
#' @details
#' initial testing
#' 
#' @return
#' a data frame from demographics table
#' 
#' @export
getCertDemographics<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.5_CDMv4_10.Demographics_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database)
  data<-DatabaseConnector::querySql(conn, renderedSql)
  dbDisconnect(conn)
  data
}

#' Get Cert result dataset for paired t-test
#' 
#' @details
#' initial testing
#' 
#' @return
#' a data frame from summary table
#' 
#' @export
getDataForPairedTTest<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.5_CDMv4_07.Paired t-test_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database)
  data<-DatabaseConnector::querySql(conn, renderedSql)
  dbDisconnect(conn)
  data
}

#' Get Cert result dataset for McNemar's test
#' 
#' @details
#' initial testing
#' 
#' @return
#' a data frame from summary table
#' 
#' @export
getDataForMcNemarTest<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.5_CDMv4_08.McNemar's test_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms=connectionDetails$dbms,
                                                 target_database=connectionDetails$target_database,
                                                 cdm_database=connectionDetails$cdm_database)
  data<-DatabaseConnector::querySql(conn, renderedSql)
  dbDisconnect(conn)
  data
}

#' Run paried t-test
#' 
#' @details
#' initial testing
#' 
#' @return
#' a data frame
#' 
#' @export
runPairedTTest<-function(connectionDetails){
  paired<-getDataForPairedTTest(connectionDetails)
  
  plyr::ddply(paired, .(DRUG_NAME,LAB_NAME,RESULT_TYPE), function(x){
    broom::tidy(t.test(x$RESULT_BEFORE,x$RESULT_AFTER, paired=T))
  })
}

#' Run McNemar's test
#' 
#' @details
#' initial testing
#' 
#' @return
#' a data frame
#' 
#' @export
runMcNemarTest<-function(connectionDetails){
  mcnemar<-getDataForMcNemarTest(connectionDetails)
  
  plyr::ddply(mcnemar, .(DRUG_NAME,LAB_NAME,RESULT_TYPE), function(x){
    broom::tidy(mcnemar.test(x$JUDGE_BEFORE,x$JUDGE_AFTER))
  })
}

#' Create data frame for set a target drug
#' 
#' @details
#' initial testing
#' 
#' @return
#' nothing
#' 
#' @export
createTargetDrugDataFrame<-function(name,class,code){
  df<-data.frame(DRUG_NAME=c(name),DRUG_CLASS=c(class),DRUG_CODE=c(code))
}

#' Add into TargetDrugDataFrame
#' 
#' @details
#' initial testing
#' 
#' @return
#' nothing
#' 
#' @export
addTargetDrugDataFrame<-function(origin,name,class,code){
  df<-data.frame(DRUG_NAME=c(name),DRUG_CLASS=c(class),DRUG_CODE=c(code))
  rbind(origin,df)
}

#' Create data frame for set a target labtest
#' 
#' @details
#' initial testing
#' 
#' @return
#' nothing
#' 
#' @export
createLabtestDataFrame<-function(id,name,type){
  df<-data.frame(LAB_ID=c(id),LAB_NAME=c(name),ABNORM_TYPE=c(type))
}

