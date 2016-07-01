/* 
	CERT Query - 0.4
	
	Written and Confirmed by SUNGJAE JUNG
	Created at 2016.03.31
	Edited 1st: 2016.04.08
	Edited 2nd: 2016.05.27
	Edited 3rd: 2016.05.30
	Edited 4rd: 2016.06.17
	Edited 4rd for CDMv4: 2016.06.17

*/
{DEFAULT @table = 'SJ_CERT_CDM4' }
{DEFAULT @drug_class = "'Anatomical Therapeutic Chemical Classification'" }
{DEFAULT @drug_list = "'Ciprofloxacin','Clopidogrel','Ketorolac','Levofloxacin','Ranitidine','Rosuvastatin','Valproic acid','Etoposide','Fluorouracil','Methotrexate'" }

USE [@table];

-- Running
-- # CASE: ATC(Anatomical Therapeutic Chemical Classification)
IF OBJECT_ID('[@table].[dbo].DRUG_LIST', 'U') IS NOT NULL
	DROP TABLE DRUG_LIST;
SELECT DISTINCT B.CONCEPT_NAME DRUG_NAME, A.DESCENDANT_CONCEPT_ID DRUG_ID
INTO DRUG_LIST
FROM [SCAN].[DBO].CONCEPT_ANCESTOR A
	INNER JOIN (
		SELECT *
		FROM [SCAN].[DBO].CONCEPT
		WHERE CONCEPT_CLASS IN (@drug_class)
			AND CONCEPT_NAME IN (@drug_list)
	) B
	ON A.ANCESTOR_CONCEPT_ID=B.CONCEPT_ID

-- Running
-- LABTEST_LIST: LOINC Code
IF OBJECT_ID('[@table].[dbo].LABTEST_LIST', 'U') IS NOT NULL
	DROP TABLE LABTEST_LIST;
CREATE TABLE LABTEST_LIST(
	LAB_ID			INT NOT NULL,
	LAB_NAME		VARCHAR(50) NOT NULL,
	ABNORM_TYPE		VARCHAR(20) NOT NULL
);
INSERT INTO LABTEST_LIST VALUES(3018677,'aPTT','Both');
INSERT INTO LABTEST_LIST VALUES(3006923,'ALT','Hyper');
INSERT INTO LABTEST_LIST VALUES(3013721,'AST','Hyper');

/* Use Exposure */
-- VISIT
-- Running
IF OBJECT_ID('[@table].[dbo].VISIT_EXPOSURE', 'U') IS NOT NULL
	DROP TABLE VISIT_EXPOSURE;
-- 00:02
SELECT DISTINCT A.PERSON_ID, A.VISIT_START_DATE START_DATE, A.VISIT_END_DATE END_DATE, MIN(B.DRUG_EXPOSURE_START_DATE) FIRST_DRUG_ORDDATE, B.DRUG_NAME
INTO VISIT_EXPOSURE
FROM [SCAN].[DBO].VISIT_OCCURRENCE A
	INNER JOIN (
		SELECT DISTINCT AA.PERSON_ID, AA.DRUG_EXPOSURE_START_DATE
			, BB.DRUG_NAME
		FROM [SCAN].[DBO].DRUG_EXPOSURE AA
			INNER JOIN DRUG_LIST BB
			ON AA.DRUG_CONCEPT_ID=BB.DRUG_ID
	) B
	ON A.PERSON_ID=B.PERSON_ID
		AND B.DRUG_EXPOSURE_START_DATE BETWEEN A.VISIT_START_DATE AND A.VISIT_END_DATE
WHERE A.PLACE_OF_SERVICE_SOURCE_VALUE IN ('I')
GROUP BY A.PERSON_ID, A.VISIT_START_DATE, A.VISIT_END_DATE, B.DRUG_NAME

-- LAB
-- Running
IF OBJECT_ID('[@table].[dbo].LAB_EXPOSURE', 'U') IS NOT NULL
	DROP TABLE LAB_EXPOSURE;
-- 00:04
SELECT DISTINCT A.*, B.OBSERVATION_CONCEPT_ID, B.OBSERVATION_DATETIME, B.RESULT, B.UNIT_CONCEPT_ID, B.RANGE_LOW, B.RANGE_HIGH, B.LAB_NAME, B.ABNORM_TYPE
	,CASE WHEN OBSERVATION_DATETIME <= A.FIRST_DRUG_ORDDATE THEN 'Y'
		ELSE 'N'
	END IS_BEFORE
INTO LAB_EXPOSURE
FROM VISIT_EXPOSURE A
	INNER JOIN (
		SELECT DISTINCT AA.PERSON_ID, AA.OBSERVATION_CONCEPT_ID, CAST(AA.OBSERVATION_DATE AS DATETIME)+CAST(AA.OBSERVATION_TIME AS DATETIME) OBSERVATION_DATETIME
			, AA.VALUE_AS_NUMBER RESULT, AA.UNIT_CONCEPT_ID, AA.RANGE_LOW, AA.RANGE_HIGH
			, BB.LAB_NAME, BB.ABNORM_TYPE
		FROM [SCAN].[DBO].OBSERVATION AA
			INNER JOIN LABTEST_LIST BB
			ON AA.OBSERVATION_CONCEPT_ID=BB.LAB_ID 
		WHERE AA.UNIT_CONCEPT_ID IS NOT NULL
			AND AA.RANGE_LOW IS NOT NULL
			AND AA.RANGE_HIGH IS NOT NULL
	) B
	ON A.PERSON_ID=B.PERSON_ID
		AND B.OBSERVATION_DATETIME BETWEEN A.START_DATE AND A.END_DATE

-- CERT Dataset
-- Running
IF OBJECT_ID('[@table].[dbo].CERT_EXPOSURE', 'U') IS NOT NULL
	DROP TABLE CERT_EXPOSURE;
-- 00:02
SELECT A.PERSON_ID, A.FIRST_DRUG_ORDDATE, A.DRUG_NAME, A.LAB_NAME, A.OBSERVATION_CONCEPT_ID, A.IS_BEFORE, A.ABNORM_TYPE, A.RANGE_LOW, A.RANGE_HIGH
	,CASE WHEN A.ABNORM_TYPE IN ('HYPER','BOTH') THEN MAX(A.RESULT) ELSE NULL END AS MAXRESULT
	,CASE WHEN A.ABNORM_TYPE IN ('HYPO','BOTH') THEN MIN(A.RESULT) ELSE NULL END AS MINRESULT
	,COUNT(*) CNT
	,B.Y, B.N
INTO CERT_EXPOSURE
FROM LAB_EXPOSURE A
	INNER JOIN (
		SELECT *
		FROM (
			SELECT PERSON_ID, FIRST_DRUG_ORDDATE, DRUG_NAME, OBSERVATION_CONCEPT_ID, IS_BEFORE, RANGE_LOW, RANGE_HIGH
			FROM LAB_EXPOSURE
		) A
		PIVOT (
			COUNT(IS_BEFORE) FOR IS_BEFORE IN (Y,N)
		) PV
		WHERE Y>0 AND N>0) B
	ON A.PERSON_ID=B.PERSON_ID
		AND A.FIRST_DRUG_ORDDATE=B.FIRST_DRUG_ORDDATE
		AND A.DRUG_NAME=B.DRUG_NAME
		AND A.OBSERVATION_CONCEPT_ID=B.OBSERVATION_CONCEPT_ID
		AND A.RANGE_LOW=B.RANGE_LOW
		AND A.RANGE_HIGH=B.RANGE_HIGH
GROUP BY A.PERSON_ID, A.FIRST_DRUG_ORDDATE, A.DRUG_NAME, A.LAB_NAME, A.OBSERVATION_CONCEPT_ID, A.IS_BEFORE, A.ABNORM_TYPE, A.RANGE_LOW, A.RANGE_HIGH, B.Y, B.N

-- Summary result
IF OBJECT_ID('[@table].[dbo].SUMMARY', 'U') IS NOT NULL
	DROP TABLE SUMMARY;
SELECT PERSON_ID, DRUG_NAME, LAB_NAME, ABNORM_TYPE, RANGE_LOW, RANGE_HIGH, RESULT_BEFORE=Y, RESULT_AFTER=N, RESULT AS RESULT_TYPE
	,CASE WHEN Y BETWEEN RANGE_LOW AND RANGE_HIGH THEN 'NORMAL'
		ELSE 'ABNORMAL'
	END JUDGE_BEFORE
	,CASE WHEN N BETWEEN RANGE_LOW AND RANGE_HIGH THEN 'NORMAL'
		ELSE 'ABNORMAL'
	END JUDGE_AFTER
	,CASE WHEN RESULT IN ('MAX') AND Y<N THEN 'INCREASED'
		WHEN RESULT IN ('MIN') AND Y>N THEN 'DECREASED'
		ELSE 'STAY'
	END JUDGE
	,CASE WHEN RESULT IN ('MAX') AND Y<RANGE_HIGH AND N>RANGE_HIGH THEN 'ABNORMAL'
		WHEN RESULT IN ('MIN') AND Y>RANGE_LOW AND N<RANGE_LOW THEN 'ABNORMAL'
		ELSE 'NORMAL'
	END IS_ABNORMAL
INTO SUMMARY
FROM(
	SELECT *
	FROM(
		SELECT PERSON_ID, FIRST_DRUG_ORDDATE, DRUG_NAME, LAB_NAME, ABNORM_TYPE, RANGE_LOW, RANGE_HIGH, MAXRESULT, IS_BEFORE, 'MAX' RESULT
		FROM CERT_EXPOSURE
		WHERE ABNORM_TYPE IN ('HYPER','BOTH')
	) A
	PIVOT(
		MIN(MAXRESULT) FOR IS_BEFORE IN (Y,N)
	) PV
	UNION
	SELECT *
	FROM(
		SELECT PERSON_ID, FIRST_DRUG_ORDDATE, DRUG_NAME, LAB_NAME, ABNORM_TYPE, RANGE_LOW, RANGE_HIGH, MINRESULT, IS_BEFORE, 'MIN' RESULT
		FROM CERT_EXPOSURE
		WHERE ABNORM_TYPE IN ('HYPO','BOTH')
	) A
	PIVOT(
		MIN(MINRESULT) FOR IS_BEFORE IN (Y,N)
	) PV
) T
