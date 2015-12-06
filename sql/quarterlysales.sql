DROP TABLE IF EXISTS qsales;
CREATE TABLE qsales (
        REPORTING_COUNTRY_GROUP VARCHAR(12) NOT NULL, 
        REPORTING_COUNTRY_SUBGROUP VARCHAR(14) NOT NULL, 
        COUNTRY_CLUSTER VARCHAR(20) NOT NULL, 
        COUNTRY VARCHAR(20) NOT NULL, 
        BM VARCHAR(10), 
        REPORTING_SEGMENT VARCHAR(4), 
        RTM VARCHAR(17) NOT NULL, 
        PH1_NAME VARCHAR(28), 
        PH2_NAME VARCHAR(28), 
        PH3_NAME VARCHAR(40), 
        PH4_NAME VARCHAR(40), 
        PH5_NAME VARCHAR(22), 
        MTM VARCHAR(30) NOT NULL, 
        ORDNO BIGINT NOT NULL, 
        TOPSELL_FLAG VARCHAR(10), 
        CUR VARCHAR(3) NOT NULL, 
        Revenue_Local FLOAT NOT NULL, 
        Plan_USD_Revenue FLOAT NOT NULL, 
        Actual_USD_Revenue FLOAT NOT NULL, 
        Net_USD_Revenue FLOAT, 
        STATUS VARCHAR(6) NOT NULL, 
        SBO VARCHAR(10), 
        CUSTNAME VARCHAR(40), 
        CUSTNO VARCHAR(10) NOT NULL, 
        Account_Owner VARCHAR(29), 
        x86_MANAGER VARCHAR(21), 
        x86_F2F_NAME VARCHAR(28), 
        GLN VARCHAR(10), 
        GLOBAL_ACCT_NAME VARCHAR(29), 
        END_CUSTOMER_NO VARCHAR(10), 
        END_CUSTOMER_NAME VARCHAR(40), 
        ORDER_WEEK INTEGER NOT NULL
);

SELECT DISTINCT PH1_NAME FROM qsales;

SELECT DISTINCT custname FROM qsales
WHERE PH1_NAME='Enterprise Product Group' OR PH1_NAME='Server and Storage%'
ORDER BY custname;

SELECT DISTINCT custname FROM qsales
WHERE (PH1_NAME='Enterprise Product Group' OR PH1_NAME='Server and Storage%') AND (custname LIKE '%IBM%' OR custname LIKE 'INTERNATIONAL%')
ORDER BY custname;

SELECT SUM(Actual_USD_Revenue) FROM qsales
WHERE PH1_NAME='Enterprise Product Group' OR PH1_NAME='Server and Storage%';

SELECT SUM(Actual_USD_Revenue) FROM qsales 
WHERE (PH1_NAME='Enterprise Product Group' OR PH1_NAME='Server and Storage%') AND (custname LIKE '%IBM%' OR custname LIKE 'INTERNATIONAL%');

SELECT SUM(Actual_USD_Revenue) FROM qsales 
WHERE (BM='GA' OR BM='Enterprise') AND (custname LIKE '%IBM%' OR custname LIKE 'INTERNATIONAL%');
