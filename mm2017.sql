create database march_madness2017;

/*
Step 1 - reformat data for matching.  Run once.
*/

/*
-- reformat team names in kenpom dataset
create table kenpom2 as
select regexp_replace(team, '\s+$', '') as team_format, * FROM kenpom;
alter table kenpom2 drop column team;
alter table kenpom2 rename column team_format to team;
drop table kenpom;
create table kenpom as (select * from kenpom2);
drop table kenpom2;

select * from kenpom;

-- reformat team names in kenpom dataset
create table kenpom_final2 as
select regexp_replace(team, '\s+$', '') as team_format, * FROM kenpom_final;
alter table kenpom_final2 drop column team;
alter table kenpom_final2 rename column team_format to team;
drop table kenpom_final;
create table kenpom_final as (select * from kenpom_final2);
drop table kenpom_final2;

-- update tourneyseeds to include seed number
create table tourneyseeds2 as
(select distinct *
	,cast(substring(seed,2,2) as int) as seed_num
from tourneyseeds);
drop table tourneyseeds;
create table tourneyseeds as (select * from tourneyseeds2);
drop table tourneyseeds2;

-- update tourneyslots to include seed number
create table tourneyslot2 as
select distinct sl.*
	,cast(substring(strongseed,2,2) as int) as strongseed_num
	,cast(substring(weakseed,2,2) as int) as weakseed_num
	from tourneyslots as sl
	inner join tourneyseeds as ses on sl.strongseed = ses.seed and sl.season = ses.season
	inner join tourneyseeds as sew on sl.weakseed = sew.seed and sl.season = sew.season;
drop table tourneyslots;
create table tourneyslots as (select * from tourneyslot2);
drop table tourneyslot2;


*/
-------------------------------------------------------------------

/*
Step 2 - create submission file
*/


-- declare year as variable
create table year as 
(select 2017 as season);

-- select all teams in 2017
create table teams1 as
select distinct y.season
	,t.team
from tourneyseeds t
inner join year as y on t.season = y.season;

-- select all teams from 2017
create table teams2 as
select distinct y.season
	,t.team
from tourneyseeds t
inner join year as y on t.season = y.season;

-- combine each so that you get every possible combination for this tournament
create table submission_start as
select distinct x.season as season
	,team as team1
	,team2 as team2
	,x.season || '_' || team || '_' || team2 as combine
from (
select *
from (select * from (select distinct season, team from teams1) t1
left outer join (select distinct team as team2 from teams2) as t2 on T1.team < T2.team2) t3
where t3.team is not null
) x;

/*
Step 3 - add kenpom data
*/

-- populate with kenpom team names
create table submission_1 as
select distinct s.*
	,t1.kenpom_name as team1_name
	,t2.kenpom_name as team2_name
from submission_start as s
inner join teams as t1 on s.team1 = t1.team_id
inner join teams as t2 on s.team2 = t2.team_id;

-- populate with kenpom statistics
create table submission_2 as
select s.*
	,kp1.conference as team1_conf
	,kp1.wins as team1_win
	,kp1.losses as team1_loss
	,kp1.pyth as team1_pyth
	,kp1.adjusto as team1_adjusto
	,kp1.adjustd as team1_adjustd
	,kp1.adjustt as team1_adjustt
	,kp1.luck as team1_luck
	,kp1.sos_pyth as team1_sospyth
	,kp1.sos_oppo as team1_sosoppo
	,kp2.wins as team2_win
	,kp2.losses as team2_loss
	,kp2.pyth as team2_pyth
	,kp2.adjusto as team2_adjusto
	,kp2.adjustd as team2_adjustd
	,kp2.adjustt as team2_adjustt
	,kp2.luck as team2_luck
	,kp2.sos_pyth as team2_sospyth
	,kp2.sos_oppo as team2_sosoppo
	,kp2.conference as team2_conf
from submission_1 as s
left join kenpom as kp1 on s.team1_name = kp1.team and s.season = kp1.year
left join kenpom as kp2 on s.team2_name = kp2.team and s.season = kp2.year;



/*
Step 4 - add seeding
*/
create table submission_export as
select distinct s.*
	,ts1.seed_num as team1_seed
	,ts2.seed_num as team2_seed
from submission_2 as s
inner join tourneyseeds as ts1 on s.team1 = ts1.team and s.season = ts1.season
inner join tourneyseeds as ts2 on s.team2 = ts2.team and s.season = ts2.season;

/*
Step 5 - rearrnage
*/

create table submission_export_final as
select distinct season
	,team1
	,team2
	,combine
	,team1_name
	,team2_name
	,team1_conf
	,team2_conf
	,team1_seed
	,team1_win
	,team1_loss
	,team1_pyth
	,team1_adjusto
	,team1_adjustd
	,team1_adjustt
	,team1_luck
	,team1_sospyth
	,team1_sosoppo
	,team2_seed
	,team2_win
	,team2_loss
	,team2_pyth
	,team2_adjusto
	,team2_adjustd
	,team2_adjustt
	,team2_luck
	,team2_sospyth
	,team2_sosoppo
from submission_export;


drop table year;
drop table teams1;
drop table teams2;
drop table submission_start;
drop table submission_1;
drop table submission_2;

select * from submission_export;

COPY submission_export_final TO '/Users/ntmill/OneDrive/Data/March Madness/2017/submission_test.csv' DELIMITER ',' CSV HEADER;

drop table final_submit
/*
drop table submission_export;
drop table submission_export_final;
*/

/*
Step 5 - build training dataset using historical tournament results
*/

-- randomly select half the tourney results, select winning team as team1 from this group
create table training_step1 as
select cast(season as varchar) || cast(wteam as varchar) || cast(lteam as varchar) as match_key
	,season
	,wteam as team1
	,lteam as team2
	,wscore as team1_score
	,lscore as team2_score
from tourneydetailedresults
order by random()
limit (select count(*) from tourneydetailedresults)/2;

-- select the other half of tournament results, selecting the losing team as team1
create table training_step2 as
select cast(tr.season as varchar) || cast(tr.wteam as varchar) || cast(tr.lteam as varchar) as match_key
	,tr.season
	,tr.lteam as team1
	,tr.wteam as team2
	,tr.lscore as team1_score
	,tr.wscore as team2_score
from tourneydetailedresults as tr
left join training_step1 as ts1 on cast(tr.season as varchar) || cast(tr.wteam as varchar) || cast(tr.lteam as varchar) = ts1.match_key
where ts1.team1 is null;



-- combine the previous two tables
create table training_step3 as
select * from training_step1
union all
select * from training_step2;

-- add kenpom data and results for final output
create table training_export as
select distinct ts.*
	,t1.kenpom_name as team1_name
	,t2.kenpom_name as team2_name
	,kp1.conference as team1_conf
	,kp2.conference as team2_conf
	,tse1.seed_num as team1_seed
	,tse2.seed_num as team2_seed
	,kp1.wins as team1_win
	,kp1.losses as team1_loss
	,kp1.pyth as team1_pyth
	,kp1.adjusto as team1_adjusto
	,kp1.adjustd as team1_adjustd
	,kp1.adjustt as team1_adjustt
	,kp1.luck as team1_luck
	,kp1.sos_pyth as team1_sospyth
	,kp1.sos_oppo as team1_sosoppo
	,kp2.wins as team2_win
	,kp2.losses as team2_loss
	,kp2.pyth as team2_pyth
	,kp2.adjusto as team2_adjusto
	,kp2.adjustd as team2_adjustd
	,kp2.adjustt as team2_adjustt
	,kp2.luck as team2_luck
	,kp2.sos_pyth as team2_sospyth
	,kp2.sos_oppo as team2_sosoppo
	,case when ts.team1_score > ts.team2_score then 1 else 0 end as team1_victory
from training_step3 as ts
left join teams as t1 on ts.team1 = t1.team_id
left join kenpom as kp1 on t1.kenpom_name = kp1.team and ts.season = kp1.year
left join tourneyseeds as tse1 on tse1.team = t1.team_id and ts.season = tse1.season
left join teams as t2 on ts.team2 = t2.team_id
left join kenpom as kp2 on t2.kenpom_name = kp2.team and ts.season = kp2.year
left join tourneyseeds as tse2 on tse2.team = t2.team_id and ts.season = tse2.season;

drop table training_step1;
drop table training_step2;
drop table training_step3;

-- drop table training_export;

select * from training_export;

-- export file
copy training_export to '/Users/ntmill/OneDrive/Data/March Madness/2016/training_export.csv' delimiter ',' csv header;


/*
Step 6 - create final prediction files
*/
alter table final_submit
add column avg_ens decimal;

update final_submit set avg_ens = (nn + rf + glm_step + xgb)/4;

create table submit_nn_ens as select distinct combine as id, ens_nn as pred from final_submit;
copy submit_nn_ens to '/Users/ntmill/OneDrive/Data/March Madness/2017/submit_nn_ens.csv' delimiter ',' csv header;

create table submit_avg_ens as select distinct combine as id, avg_ens as pred from final_submit;
copy submit_avg_ens to '/Users/ntmill/OneDrive/Data/March Madness/2017/submit_avg_ens.csv' delimiter ',' csv header;

create table submit_glm_step as select distinct combine as id, glm_step as pred from final_submit;
copy submit_glm_step to '/Users/ntmill/OneDrive/Data/March Madness/2017/submit_glm_step.csv' delimiter ',' csv header;

create table submit_rf as select distinct combine as id, rf as pred from final_submit;
copy submit_rf to '/Users/ntmill/OneDrive/Data/March Madness/2017/submit_rf.csv' delimiter ',' csv header;

create table submit_xgb as select distinct combine as id, xgb as pred from final_submit;
copy submit_xgb to '/Users/ntmill/OneDrive/Data/March Madness/2017/submit_xgb.csv' delimiter ',' csv header;

create table submit_nn as select distinct combine as id, nn as pred from final_submit;
copy submit_nn to '/Users/ntmill/OneDrive/Data/March Madness/2017/submit_nn.csv' delimiter ',' csv header;

drop table submit_nn_ens;
drop table submit_glm_step;
drop table submit_rf;
drop table submit_xgb;
drop table submit_nn;
