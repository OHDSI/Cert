
renderTest<-function(){
  SqlRender::loadRenderTranslateSql("temp.sql",
                                    packageName="Cert",
                                    dbms="sql server")
}

queryTest<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  data<-DatabaseConnector::querySql(conn, "select top 10 * from [OMOP_5].[dbo].CONCEPT;")
  dbDisconnect(conn)
  data
}

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

runPairedTTest<-function(connectionDetails){
  paired<-getDataForPairedTTest(connectionDetails)
  
  plyr::ddply(paired, .(DRUG_NAME,LAB_NAME,RESULT_TYPE), function(x){
    broom::tidy(t.test(x$RESULT_BEFORE,x$RESULT_AFTER, paired=T))
  })
}

runMcNemarTest<-function(connectionDetails){
  mcnemar<-getDataForMcNemarTest(connectionDetails)
  
  plyr::ddply(mcnemar, .(DRUG_NAME,LAB_NAME,RESULT_TYPE), function(x){
    broom::tidy(mcnemar.test(x$JUDGE_BEFORE,x$JUDGE_AFTER))
  })
}

createTargetDrugDataFrame<-function(name,class,code){
  df<-data.frame(DRUG_NAME=c(name),DRUG_CLASS=c(class),DRUG_CODE=c(code))
}

addTargetDrugDataFrame<-function(origin,name,class,code){
  df<-data.frame(DRUG_NAME=c(name),DRUG_CLASS=c(class),DRUG_CODE=c(code))
  rbind(origin,df)
}

createLabtestDataFrame<-function(id,name,type){
  df<-data.frame(LAB_ID=c(id),LAB_NAME=c(name),ABNORM_TYPE=c(type))
}

