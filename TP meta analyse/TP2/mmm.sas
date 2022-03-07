PROC IMPORT OUT= MYPUBLIC.mmm 
            DATAFILE= "C:\Users\id1647\Desktop\For students\Practical 2 
Logistic models diabetes\Practical 2_3_Discontinuations data.xls" 
            DBMS=EXCEL REPLACE;
     RANGE="Data$"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
