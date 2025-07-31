--- Provider Mapping

-- Step 1: check the number of observation you currntly have
select count(*) from oncology_disease_groupings
 

-- Step 2: check the number of new records you must get by matching
select * from oncology_disease_groupings a right join oncology_provider_jun b on a.EPIC_PROVIDER_ID = b.EPIC_PROVIDER_ID where a.EPIC_PROVIDER_ID is null;

-- Merge the new records to oncology_disease_groupings
merge into oncology_disease_groupings a
using oncology_provider_jun b
on( a.EPIC_PROVIDER_ID = b.EPIC_PROVIDER_ID)
when matched then
update set 
a.PROVIDER_NAME = b.PROVIDER_NAME, 
a.DISEASE_GROUP = b.DISEASE_GROUP,
a.DISEASE_GROUP_B = b.DISEASE_GROUP_B,
a.SITE = b.SITE, 
a.PROVIDER_TYPE = B.PROVIDER_TYPE
WHEN NOT MATCHED THEN
INSERT (a.PROVIDER_NAME, a.EPIC_PROVIDER_ID, a.DISEASE_GROUP, a.DISEASE_GROUP_B, a.PROVIDER_TYPE, a.SITE)
VALUES(b.PROVIDER_NAME, b.EPIC_PROVIDER_ID, b.DISEASE_GROUP, b.DISEASE_GROUP_B, b.PROVIDER_TYPE, b.SITE)

-- check again you must have step 1 + step 2 observations
select count(*) from oncology_disease_groupings

-- drop the new mapping data if all is good
drop table oncology_provider_jun;

-- Commit your job
commit;


-- Sometimes you may need to add one provider
-- Insert one row
insert into oncology_disease_groupings a (a.PROVIDER_NAME, a.EPIC_PROVIDER_ID, a.DISEASE_GROUP, a.DISEASE_GROUP_B, a.PROVIDER_TYPE, a.SITE) 
                                   values('KAUR, GURBAKHASH', '297815', 'Liquid Tumors', 'MM', 'Physician' , 'RTC');
                                 
-- check the new row
select * from oncology_disease_groupings where PROVIDER_NAME = 'KAUR, GURBAKHASH';



-- Visit Type Mapping
-- check the data first
select count(*) from oncology_prc_groupings; 

-- check the number of observations you need to add
select * from oncology_prc_groupings a right join oncology_visit_type_dec b on a.prc_name = b.prc_name where a.prc_name is null;

-- Merge
merge into oncology_prc_groupings a
using oncology_visit_type_dec b
on(a.PRC_NAME = b.PRC_NAME)
when matched then
update set 
a.ASSOCIATIONLISTA = b.ASSOCIATIONLISTA, 
a.ASSOCIATIONLISTB = b.ASSOCIATIONLISTB, 
a.ASSOCIATIONLISTT = b.ASSOCIATIONLISTT, 
a.INPERSONVSTELE = b.INPERSONVSTELEHEALTH 
WHEN NOT MATCHED THEN
INSERT (a.PRC_NAME, a.ASSOCIATIONLISTA, a.ASSOCIATIONLISTB, a.ASSOCIATIONLISTT, a.INPERSONVSTELE)
VALUES(b.PRC_NAME, b.ASSOCIATIONLISTA, b.ASSOCIATIONLISTB, b.ASSOCIATIONLISTT, b.INPERSONVSTELEHEALTH)

-- check the result
select count(*) from oncology_prc_groupings; 

drop table oncology_visit_type_dec;

commit;



-- Department Mapping
select * from oncology_department_groupings a 
right join oncology_department_jul b 
on a.DEPARTMENT_ID = b.DEPARTMENT_ID where a.DEPARTMENT_ID is null;

merge into oncology_department_groupings a
using oncology_department_july b
on(a.DEPARTMENT_ID = b.DEPARTMENT_ID)
when matched then
update set 
a.DEPARTMENT_NAME = b.DEPARTMENT_NAME, 
a.SITE = b.SITE
WHEN NOT MATCHED THEN
INSERT (a.DEPARTMENT_NAME, a.DEPARTMENT_ID, a.SITE)
VALUES(b.DEPARTMENT_NAME, b.DEPARTMENT_ID, b.SITE)

drop table oncology_department_july