# Cat schema
 
# --- !Ups

create sequence cat_id_seq;

create table cat (
    id integer not null default nextval('cat_id_seq'),
    name varchar(255) not null,
  	color varchar(255),
	  race varchar(255),
	  gender varchar(255),
	  picture blob
	
);


 
# --- !Downs
 

drop table cat;

drop sequence cat_id_seq;