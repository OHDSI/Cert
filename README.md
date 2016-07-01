# Cert
[Under development] R package for running the Comparison on Extreme Laboratory Test results (CERT) algorithm.


A novel algorithm for detection of adverse drug reaction signals using a hospital electronic medical record database. Park MY, Yoon D, Lee K, Kang SY, Park I, Lee SH, Kim W, Kam HJ, Lee YH, Kim JH, Park RW. Pharmacoepidemiol Drug Saf. 2011 Jun;20(6):598-607. doi: 10.1002/pds.2139.

PURPOSE: Quantitative analytic methods are being increasingly used in postmarketing surveillance. However, currently existing methods are limited to spontaneous reporting data and are inapplicable to hospital electronic medical record (EMR) data. The principal objectives of this study were to propose a novel algorithm for detecting the signals of adverse drug reactions using EMR data focused on laboratory abnormalities after treatment with medication, and to evaluate the potential use of this method as a signal detection tool.
METHODS: We developed an algorithm referred to as the Comparison on Extreme Laboratory Test results, which takes an extreme representative value pair according to the types of laboratory abnormalities on the basis of each patient's medication point. We used 10 years' EMR data from a tertiary teaching hospital, containing 32,033,710 prescriptions and 115,241,147 laboratory tests for 530,829 individual patients. Ten drugs were selected randomly for analysis, and 51 laboratory values were matched. The sensitivity, specificity, positive predictive value, and negative predictive value of the algorithm were calculated.
RESULTS: The mean number of detected laboratory abnormality signals for each drug was 27 (±7.5). The sensitivity, specificity, positive predictive value, and negative predictive value of the algorithm were 64-100%, 22-76%, 22-75%, and 54-100%, respectively.
CONCLUSION: The results of this study demonstrated that the Comparison on Extreme Laboratory Test results algorithm described herein was extremely effective in detecting the signals characteristic of adverse drug reactions. This algorithm can be regarded as a useful signal detection tool, which can be routinely applied to EMR data.

reference: http://www.ncbi.nlm.nih.gov/pubmed/?term=21472818

Getting Started
===============
```r
library(Cert)
library(SqlRender)
library(DatabaseConnector)
library(plyr)
connectionDetails<-DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                              server="IP",
                                                              port="PORT",
                                                              schema="SCHEMA",
                                                              user="ID",
                                                              password="PW")
conn<-DatabaseConnector::connect(connectionDetails)
renderTest()
queryTest()
generateCertDataSet(conn)
paired_t<-runPairedTTest(conn)
paired_t
mcnemars<-runMcNemarTest(conn)
mcnemars
```

