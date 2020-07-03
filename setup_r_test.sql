create database checkin_co;

\connect checkin_co;

create table timeinfo(userid int NOT NULL, 
                   employee VARCHAR(50), 
                   datework date,
                   login time without time zone,
                   logout time without time zone
		   );

create table shiftinfo(userid int NOT NULL, 
	           employee VARCHAR(50), 
                   datework date,
                   workday varchar (50),
                   expected_login time without time zone,
                   expected_logout time without time zone);


\copy shiftinfo from 'shiftinfo.csv' with DELIMITER ',' CSV HEADER
\copy timeinfo  from 'samproj.csv'  with DELIMITER ',' CSV HEADER
