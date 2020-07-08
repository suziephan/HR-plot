create database checkin_co;

\connect checkin_co;

drop table timeinfo;
drop table shiftinfo;

create table timeinfo(userid int NOT NULL, 
                   employee VARCHAR(50), 
                   datework date,
                   type varchar(50),
                   time1 time without time zone
		   );

create table shiftinfo(userid int NOT NULL, 
	           employee VARCHAR(50), 
                   datework date,
                   workday varchar (50),
                   expected_login time without time zone,
                   expected_logout time without time zone);

