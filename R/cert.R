
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

generateCertDataSet<-function(connectionDetails, drug_list=NA, labtest_list=NA){
  conn<-DatabaseConnector::connect(connectionDetails)

  if(!is.na(labtest_list)){
    DatabaseConnector::insertTable(conn, "LABTEST_LIST", labtest_list)
  }
  if(is.na(drug_list)){
    renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_Formatted.sql",
                                                   packageName="Cert",
                                                   dbms=connectionDetails$dbms,
                                                   target_database=connectionDetails$target_database,
                                                   cdm_database=connectionDetails$cdm_database)
  }else{
    renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_Formatted.sql",
                                                   packageName="Cert",
                                                   dbms=connectionDetails$dbms,
                                                   target_database=connectionDetails$target_database,
                                                   cdm_database=connectionDetails$cdm_database,
                                                   drug_list=paste("'",drug_list,"'",sep=""))
  }
  
  DatabaseConnector::executeSql(conn, renderedSql)
  dbDisconnect(conn)
}

getCertResultDataSet<-function(connectionDetails){
  conn<-DatabaseConnector::connect(connectionDetails)
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_09.Summary_Formatted.sql",
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
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_07.Paired t-test_Formatted.sql",
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
  
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_08.McNemar's test_Formatted.sql",
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
    c(paired_t_test=t.test(x$RESULT_BEFORE,x$RESULT_AFTER, paired=T)$p.value)
  })
}

runMcNemarTest<-function(connectionDetails){
  mcnemar<-getDataForMcNemarTest(connectionDetails)
  
  plyr::ddply(mcnemar, .(DRUG_NAME,LAB_NAME,RESULT_TYPE), function(x){
    c(mcnemar_test=mcnemar.test(x$JUDGE_BEFORE,x$JUDGE_AFTER)$p.value)
  })
}

createLabtestDataFrame<-function(id,name,type){
  df<-data.frame(LAB_ID=c(id),LAB_NAME=c(name),ABNORM_TYPE=c(type))
}

