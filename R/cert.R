
renderTest<-function(){
  SqlRender::loadRenderTranslateSql("temp.sql",
                                    packageName="Cert",
                                    dbms="sql server")
}

queryTest<-function(){
  DatabaseConnector::querySql(conn, "select top 10 * from [OMOP_5].[dbo].CONCEPT;")
}

generateCertDataSet<-function(conn){
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms="sql server")
  DatabaseConnector::executeSql(conn, renderedSql)
}

getCertResultDataSet<-function(conn){
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_09.Summary_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms="sql server")
  DatabaseConnector::executeSql(conn, renderedSql)
}

getDataForPairedTTest<-function(conn){
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_07.Paired t-test_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms="sql server")
  DatabaseConnector::querySql(conn, renderedSql)
}

getDataForMcNemarTest<-function(conn){
  renderedSql<-SqlRender::loadRenderTranslateSql("CERT_0.4_CDMv4_08.McNemar's test_Formatted.sql",
                                                 packageName="Cert",
                                                 dbms="sql server")
  DatabaseConnector::querySql(conn, renderedSql)
}

runPairedTTest<-function(conn){
  paired<-getDataForPairedTTest(conn)
  
  library(plyr)
  plyr::ddply(paired, .(DRUG_NAME,LAB_NAME,RESULT_TYPE), function(x){
    c(paired_t_test=t.test(x$RESULT_BEFORE,x$RESULT_AFTER, paired=T)$p.value)
  })
}

runMcNemarTest<-function(conn){
  mcnemar<-getDataForMcNemarTest(conn)
  
  library(plyr)
  plyr::ddply(mcnemar, .(DRUG_NAME,LAB_NAME,RESULT_TYPE), function(x){
    c(mcnemar_test=mcnemar.test(x$JUDGE_BEFORE,x$JUDGE_AFTER)$p.value)
  })
}

