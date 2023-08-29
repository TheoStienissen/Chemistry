
https://www.humane-endpoints.info/nl/rat/fysiologische-parameters
https://www.humane-endpoints.info/nl/muis/fysiologische-parameters

/*
Business rules:
  1. Elk dier neemt hoogstens aan 1 experiment deel. Dit om vervuiling tegen te gaan.


*/

create table experiment 
( experiment_id  number (3,0) not null, 
  omschrijving   varchar2 (200));
  
create sequence experiment_seq start with 49;
create or replace trigger experiment_briu
before insert or update on experiment
for each row
begin
  :new.experiment_id := nvl (:new.experiment_id, experiment_seq.nextval);
end experiment_briu;
/
 
alter table experiment add constraint experiment_pk primary key (experiment_id) using index experiment_pk ;

create table genus 
( genus_id   number (3,0) not null, 
  naam       varchar2 (7) not null);
   
create sequence genus_seq start with 6;
create or replace trigger genus_briu
before insert or update on genus
for each row
begin
  :new.genus_id := nvl (:new.genus_id, genus_seq.nextval);
end genus_briu;
/
    
create unique index genus_pk on genus (genus_id);
alter table genus add constraint genus_pk primary key (genus_id) using index genus_pk ;

create table species 
( species_id   number (3,0)    not null, 
  genus_id     number (3,0)    not null, 
  naam         varchar2 (30)   not null, 
  levensverwachting number (4) not null,
  plaatje       blob
	 constraint species_fk1 foreign key (genus_id);
	 
alter table species add constraint species_pk primary key (species_id)   using index species_pk ;
create index species_genus_idx1 on species (genus_id);

create sequence species_seq;
create or replace trigger species_briu 
before insert or update on species
for each row
begin
  :new.species_id := nvl (:new.species_id, species_seq.nextval);
end species_briu;
/

create table dieren 
(dier_id		number (6,0) not null, 
species_id		number (3,0) not null, 
geboortedatum	date         not null, 
sexe			varchar2 (1) not null,
dataset_id      number(6)    not null,
experiment_id	number (3,0), 
sterfdatum		date, 
doodsoorzaak	varchar2 (1), 
  constraint dieren_ck1 check (sexe in ('F', 'M')), 
  constraint dieren_ck3 check (geboortedatum <= sterfdatum), 
  constraint dieren_ck2 check (doodsoorzaak in ('N', 'O','E')), -- Natuurlijk, Onbekend, Einde experiment
  constraint dieren_experiment_fk1 foreign key (experiment_id)  references experiment (experiment_id), 
  constraint dieren_fk2 foreign key (species_id) references species (species_id));
   
alter table dieren add constraint dieren_pk primary key (dier_id) using index dieren_pk;
create index dieren_experiment_idx1 on dieren (experiment_id);
create index dieren_species_idx1 on dieren (species_id);

-- ToDo
create or replace trigger dieren_briu 
before insert or update on dieren
for each row
declare
l_levensverwachting species.levensverwachting%type;
begin
select levensverwachting into l_levensverwachting from species where species_id = :new.species_id;
  if :new.sexe = 'F'
  then
    if :new.geboortedatum < sysdate - 1000
    then
      raise_application_error (-20000, 'vrouwtjes kunnen niet ouder worden dan 1000 dagen.');
    end if;
  elsif :new.sexe = 'M'
  then
    if :new.geboortedatum < sysdate - 1000
    then
      raise_application_error (-20001, 'mannetjes kunnen niet ouder worden dan 1000 dagen.');
    end if;
  end if;
end;
/

create table metingen 
( dier_id        number (6,0) not null, 
  kwaliteit_id   number (2,0) not null, 
  datum          date         not null, 
  waarde         number (4,1) not null, 
	 constraint meting_kwaliteit_fk1 foreign key (kwaliteit_id) references kwaliteiten (kwaliteit_id), 
	 constraint meting_dier_fk1 foreign key (dier_id) references dieren (dier_id));
	  
alter table metingen add constraint meting_pk primary key (dier_id, kwaliteit_id, datum) using index meting_pk ;

create table waarnemingen 
( dier_id      number (6,0)  not null, 
  datum        date          not null, 
  volgnummer   number (3,0)  not null, 
  omschrijving varchar2 (40) not null, 
    constraint waarneming_dier_fk1 foreign key (dier_id) references dieren (dier_id)   );

create unique index waarneming_pk on waarnemingen (dier_id, datum, volgnummer);	
alter table nwaarnemingen add constraint waarneming_pk primary key (dier_id, datum, volgnummer)  using index waarneming_pk;


create table werknemers 
( werknemer_id number(2,0)  not null, 
  voornaam     varchar2(20) not null, 
  achternaam   varchar2(50) not null, 
  straat       varchar2(50) not null, 
  huisnummer   varchar2(10) not null, 
  postcode     varchar2(6)  not null, 
  plaats       varchar2(25) not null, 
  telefoon     varchar2(11), 
  mobiel       varchar2(11), 
  email        varchar2(75), 
  salaris      number(6,2)  not null, 
  functie      varchar2(20) not null, 
  leidinggevende_id number(2,0), 
  geboortedatum date, 
	 constraint werknemers_fk foreign key (leidinggevende_id) references werknemers (werknemer_id));
	 
alter table werknemers add constraint werknemers_pk primary key (werknemer_id)   using index werknemers_pk ;
alter table werknemers add constraint werknemers__un unique (email) using index werknemers__un ;
alter table werknemers add constraint werknemers__unv2 unique (mobiel) using index werknemers__unv2 ;

create index werknemers_leidinggevende_idx1 on werknemers (leidinggevende_id);

create table kwaliteiten 
( kwaliteit_id number(2,0)  not null, 
  omschrijving varchar2(30) not null);
alter table kwaliteiten add constraint kwaliteit_pk primary key (kwaliteit_id) using index kwaliteit_pk;

create sequence kwaliteiten_seq start with 9;
create or replace trigger kwaliteiten_briu
before insert or update on kwaliteiten
for each row
begin
  :new.kwaliteit_id := nvl (:new.kwaliteit_id, kwaliteiten_seq.nextval);
end kwaliteiten_briu;
/

create table koppeltabel 
( werknemer_id number(2,0) not null
, experiment_id number(3,0) not null, 
	 constraint koppeltabel_werknemers_fk foreign key (werknemer_id) references werknemers (werknemer_id) on delete cascade enable, 
	 constraint koppeltabel_experiment_fk foreign key (experiment_id) references experiment (experiment_id) enable);
alter table koppeltabel add constraint koppeltabel_pk primary key (werknemer_id, experiment_id)  using index koppeltabel_pk  enable;

create index koppeltabel_experiment_ix on koppeltabel (experiment_id);


create or replace package lab_pkg
is
procedure fix_datum_metingen (p_min_leeftijd in integer default 90);

procedure fix_sterf_datum;

function f_get_species (p_dier_id in number) return varchar2;

function f_get_genus (p_dier_id in number) return varchar2;

procedure update_datums (p_months in integer default 1);

procedure verwijder_experiment (p_experiment_id in integer);

procedure create_dataset (p_species_id in integer, p_experiment_id in integer, p_begin_metingen date,
     p_min_leeftijd in integer, p_aantal_dieren in integer, p_metingen_per_week in integer, p_looptijd_weken in integer, p_sterftekans in integer, p_sexe in varchar2);

procedure genereer_metingen (p_dataset in integer, p_kwaliteit in integer, p_gemiddelde_waarde_begin in number, p_gemiddelde_waarde_eind in number);

function f_count_metingen (p_dataset_id in integer, p_kwaliteit_id in integer) return integer;

function f_count_species (p_genus_id in integer) return integer;
end lab_pkg;
/


create or replace package body lab_pkg
is
--
-- Zorg dat alle metingen na de geboortedatum liggen
--
procedure fix_datum_metingen (p_min_leeftijd in integer default 90)
is
l_min  date;
l_max  date;
begin
  for j in (select dier_id, species_id, geboortedatum, sterfdatum from dieren)
  loop
    select min (datum) into l_min from metingen where dier_id = j.dier_id;
    for k in (select rowid row_id, dier_id, datum + p_min_leeftijd + (j.geboortedatum - l_min) corr_date from metingen where dier_id = j.dier_id)
    loop
      update metingen set datum = k.corr_date where rowid = k.row_id;
    end loop;
  end loop;
  commit;
exception when others then
  util.show_error ('Error in procedure fix_datum_metingen.', sqlerrm);
end fix_datum_metingen;

/*************************************************************************************************************************************************/

--
-- Zorg dat alle metingen voor de sterfdatum liggen
--
procedure fix_sterf_datum
is
l_max  date;
begin
  for j in (select dier_id, sterfdatum from dieren)
  loop
    select max (datum) into l_max from metingen where dier_id = j.dier_id;
    if j.sterfdatum <= l_max then update dieren set sterfdatum = trunc (l_max + dbms_random.value (2,50)) where dier_id = j.dier_id; end if;
  end loop;
  commit;
 
exception when others then
  util.show_error ('Error in procedure fix_sterf_datum.', sqlerrm);
end fix_sterf_datum;

/*************************************************************************************************************************************************/

--
-- Wat voor diersoort beftreft het?
--
function f_get_species (p_dier_id in number) return varchar2
is
l_naam  species.naam%type;
begin
  select s.naam into l_naam from dieren d, species s where d.species_id = s.species_id;
  return l_naam;

exception when others then
  util.show_error ('Error in function f_get_species.', sqlerrm);
end f_get_species;

/*************************************************************************************************************************************************/

--
-- Tot welke genus behoort het dier?
--
function f_get_genus (p_dier_id in number) return varchar2
is 
l_naam  genus.naam%type;
begin
  select g.naam into l_naam from dieren d, species s, genus g where d.species_id = s.species_id and s.genus_id = g.genus_id;
  return l_naam;

exception when others then
  util.show_error ('Error in function f_get_genus.', sqlerrm);
end f_get_genus;

/*************************************************************************************************************************************************/

--
-- Hoeveel dieren zijn er van deze species?
--
function f_count_species (p_species_id in integer) return integer
is
l_count integer(6);
begin
  select count(*) into l_count from dieren where species_id = p_species_id;
  return l_count;
  
exception when others then
  util.show_error ('Error in function f_count_species.', sqlerrm);
end f_count_species;

/*************************************************************************************************************************************************/

--
-- schuif datums "p_months" maanden verder of terug
--
procedure update_datums (p_months in integer default 1)
is
begin
  -- Triggers uit
  execute immediate 'alter table dieren   disable all triggers';
  execute immediate 'alter table metingen disable all triggers';
  update dieren set geboortedatum = add_months (geboortedatum, p_months), sterfdatum = add_months (sterfdatum, p_months);
  update metingen set datum = add_months (datum, p_months);
  -- Triggers aan + commit.
  execute immediate 'alter table dieren enable  all triggers';
  execute immediate 'alter table metingen enable all triggers';

exception when others then
  util.show_error ('Error in procedure update_datums.', sqlerrm);
end update_datums;

/*************************************************************************************************************************************************/

--
-- Verwijderen experiment
--
procedure verwijder_experiment (p_experiment_id in integer)
is
begin
  delete koppeltabel where experiment_id = p_experiment_id;
  delete metingen    where dier_id in (select dier_id from dieren d, datasets s where d.dataset_id = s.id and s.experiment_id = p_experiment_id);
  delete dieren      where dataset_id in (select id from datasets where experiment_id = p_experiment_id);
  delete datasets    where experiment_id = p_experiment_id;
  commit;

exception when others then
  util.show_error ('Error in procedure verwijder_experiment.', sqlerrm);
end verwijder_experiment;

/*************************************************************************************************************************************************/

--
-- Aanmaken dataset + toevoegen dieren.
--
procedure create_dataset (p_species_id in integer, p_experiment_id in integer, p_begin_metingen date,
     p_min_leeftijd in integer, p_aantal_dieren in integer, p_metingen_per_week in integer, p_looptijd_weken in integer, p_sterftekans in integer, p_sexe in varchar2)
is 
 l_max_dier_id  integer;
l_datum         date;
l_geboortedatum date;
l_sterfdatum    date;
l_meet_datum    date;
l_meetwaarde    number (4, 1);
l_count         integer;
l_sexe          varchar2(1);
l_levensverwachting number(4);
l_dataset       integer;
begin 
  -- Data validatie deel.
  if    p_species_id              is null then raise_application_error (-20010, 'Species is leeg');
  elsif p_experiment_id           is null then raise_application_error (-20011, 'Experiment is leeg');
  elsif p_begin_metingen          is null then raise_application_error (-20012, 'Begin metingen is leeg');
  elsif p_min_leeftijd            is null then raise_application_error (-20013, 'Min leeftijd is leeg');
  elsif p_aantal_dieren           is null then raise_application_error (-20014, 'Aantal dieren is leeg');
  elsif p_metingen_per_week       is null then raise_application_error (-20015, 'Metingen per week is leeg');
  elsif p_looptijd_weken          is null then raise_application_error (-20016, 'Looptijd is leeg');
  elsif p_sterftekans             is null then raise_application_error (-20018, 'Sterftekans is leeg');
  end if;

  if    p_min_leeftijd      not between 30 and 1000 then raise_application_error (-20001, 'Minimum leeftijd niet tussen 30 en 1000 dagen');
  elsif p_sterftekans       not between  2 and 95   then raise_application_error (-20002, 'Minimum leeftijd niet tussen 2 en 95');
  elsif p_metingen_per_week not between  1 and 7    then raise_application_error (-20003, 'Metingen per week niet tussen 1 en 7');
  elsif p_aantal_dieren     not between  1 and 1000 then raise_application_error (-20004, 'Aantal dieren niet tussen 1 en 1000');
  elsif p_looptijd_weken    not between  1 and 200  then raise_application_error (-20004, 'Looptijd niet tussen 1 en 200');
  end if;

  select count(*) into l_count from species where species_id = p_species_id;
  if l_count = 0 then raise_application_error (-20005, 'Species ' || p_species_id || ' bestaat niet'); end if;

  select count(*) into l_count from experiment where experiment_id = p_experiment_id;
  if l_count = 0 then raise_application_error (-20006, 'Experiment ' || p_experiment_id || ' bestaat niet'); end if;

  execute immediate 'alter table dieren   disable all triggers';
  -- logging
  insert into datasets (generated, species_id, experiment_id, begin_metingen, min_leeftijd, aantal_dieren, metingen_per_week, looptijd_weken, sterftekans, sexe)
	 values (sysdate, p_species_id, p_experiment_id, p_begin_metingen, p_min_leeftijd, p_aantal_dieren, p_metingen_per_week, p_looptijd_weken, p_sterftekans, p_sexe)
	 returning id into l_dataset;

  select nvl(max (dier_id), 0) into l_max_dier_id from dieren;
  select levensverwachting into l_levensverwachting from species where species_id = p_species_id;
  for j in 1 .. p_aantal_dieren
  loop 
     l_max_dier_id   := l_max_dier_id + 1;  -- Increment dier_id
     l_geboortedatum := p_begin_metingen - p_min_leeftijd - trunc (dbms_random.value (1, 7));
     l_sterfdatum    := p_begin_metingen + trunc ((1 - p_sterftekans / 100) * dbms_random.value (2, l_levensverwachting));
     if l_sterfdatum > sysdate then l_sterfdatum := null; end if;
     if    p_sexe = 'M' then l_sexe := 'M';
     elsif p_sexe = 'F' then l_sexe := 'F';
     else  l_sexe  := case when mod (trunc(dbms_random.value (1,100)), 2) = 0 then 'F' else 'M' end;
     end if;
     insert into dieren (dier_id, species_id, geboortedatum, sexe, dataset_id, sterfdatum)
        values (l_max_dier_id, p_species_id, l_geboortedatum, l_sexe, l_dataset, l_sterfdatum);
  end loop;
  execute immediate 'alter table dieren enable   all triggers';

exception when others then
  util.show_error ('Error in procedure create_dataset.', sqlerrm);
end create_dataset;

/*************************************************************************************************************************************************/

--
-- Aanmaken metingen
--
procedure genereer_metingen (p_dataset in integer, p_kwaliteit in integer, p_gemiddelde_waarde_begin in number, p_gemiddelde_waarde_eind in number)
is
l_experiment        datasets.experiment_id%type;
l_begin_metingen    date;
l_min_leeftijd      datasets.min_leeftijd%type;
l_metingen_per_week datasets.metingen_per_week%type;
l_looptijd_weken    datasets.looptijd_weken%type;
l_datum             date;
l_meet_datum        date;
l_meetwaarde        number (6,1);
begin
  select experiment_id, begin_metingen, min_leeftijd, metingen_per_week, looptijd_weken
    into l_experiment, l_begin_metingen, l_min_leeftijd, l_metingen_per_week, l_looptijd_weken
  from datasets where id = p_dataset;

  insert into meet_variabelen ( dataset_id, kwaliteit_id, gem_waarde_begin, gem_waarde_eind)
    values (p_dataset, p_kwaliteit, p_gemiddelde_waarde_begin, p_gemiddelde_waarde_eind);

  for j in (select dier_id, sterfdatum from dieren where dataset_id = p_dataset)
  loop
     <<dood>>
     for w in 0 .. l_looptijd_weken - 1
     loop
       l_datum := l_begin_metingen + w * 7;
       for a in 0 .. l_metingen_per_week - 1
              loop 
                 l_meet_datum := l_datum + a * trunc (7 / l_metingen_per_week);
                 exit dood when j.sterfdatum <= l_meet_datum;
                 l_meetwaarde := p_gemiddelde_waarde_begin - 1 + w * (p_gemiddelde_waarde_eind - p_gemiddelde_waarde_begin) / l_looptijd_weken * dbms_random.value (0,2);
                 insert into metingen (dier_id, datum, kwaliteit_id, waarde) values (j.dier_id, l_meet_datum, p_kwaliteit, l_meetwaarde);    
               end loop;
     end loop;
  end loop;
  commit;
  
exception when others then
  util.show_error ('Error in procedure genereer_metingen.', sqlerrm);
end genereer_metingen;

/*************************************************************************************************************************************************/

--
-- Aantal metingen per kwaliteit
--
function f_count_metingen (p_dataset_id in integer, p_kwaliteit_id in integer) return integer
is
l_metingen  number (6);
begin
  select count(*) into l_metingen from meet_variabelen mv, datasets ds, dieren d, metingen m
  where d.dier_id = m.dier_id and ds.id = p_dataset_id and mv.dataset_id = p_dataset_id  and mv.kwaliteit_id = m.kwaliteit_id
    and mv.kwaliteit_id = p_kwaliteit_id;
  return l_metingen;
  
exception when others then
  util.show_error ('Error in function f_count_metingen.', sqlerrm);
end f_count_metingen;

/*************************************************************************************************************************************************/

--
-- Aantal species per genus
--
function f_count_species (p_genus_id in integer) return integer
is
l_aantal  number (6);
begin
  select count (*) into l_aantal from species where genus_id = p_genus_id;
  return l_aantal;
 
exception when others then
  util.show_error ('Error in function f_count_species.', sqlerrm);
end f_count_species;
end lab_pkg;
/

select g.naam genus, s.naam species, e.omschrijving, d.sexe, to_char (datum, 'YY/WW') datum, e.waarde
from dieren d
join species s  on (d.species_id = s.species_id)
join genus g on (g.genus_id = s.genus_id)
join experiment e on (d.experiment_id = e.experiment_id)
where g.genus_id      = nvl(:P12_GENUS, g.genus_id)
  and d.species_id    = nvl(:P12_SPECIES, d.species_id)
  and e.experiment_id = nvl(:P12_EXPERIMENT, e.experiment_id)
  and d.sexe          = nvl(:P12_SEXE, d.sexe)
group by g.naam, s.naam, e.omschrijving, d.sexe

select to_char (datum, 'YY/WW') from metingen order by 1;
select to_char (datum, 'J') from metingen;

with min_date as (select to_char (min(datum), 'J') md from metingen)
select to_char (datum, 'J') -  md from metingen, min_date
group by to_char (datum, 'J') -  md;

select d.sexe, to_char (datum, 'YY/WW') datum, avg(e.waarde) waarde
from metingen m
join dieren d on (m.dier_id = d.dier_id)
join species s  on (d.species_id = s.species_id)
join genus g on (g.genus_id = s.genus_id)
join experiment e on (d.experiment_id = e.experiment_id)
where g.genus_id      = nvl(:P12_GENUS, g.genus_id)
  and d.species_id    = nvl(:P12_SPECIES, d.species_id)
  and e.experiment_id = nvl(:P12_EXPERIMENT, e.experiment_id)
  and d.sexe          = nvl(:P12_SEXE, d.sexe)
group by d.sexe, to_char (datum, 'YY/WW')
order by to_char (datum, 'YY/WW'), sexe


-- Dieren per experiment
select min(datum), max(datum) from metingen;
select min(geboortedatum), max(geboortedatum) from dieren;

declare
l_min  date;
l_max  date;
l_min_age number(2) := 90;
begin
for j in (select dier_id, species_id, geboortedatum, sterfdatum from dieren)
loop
  select min(datum) into l_min from metingen where dier_id = j.dier_id;
  for k in (select rowid row_id, dier_id, datum + l_min_age + (geboortedatum - l_min) corr_date from metingen where dier_id = j.dier_id)
  loop
    update metingen set datum = k.corr_date where rowid = k.row_id;
  end loop;
end loop;
commit;
end;

----------------

create or replace type my_analysis_ty
as object
( datum    varchar2(10),
  waarde    number(4,1));
/

create or replace type my_analysis_row as table of my_analysis_ty;
/

create table datasets
 ( id                number generated always as identity
 , generated         date
 , specied_id        number(3)
 , experiment_id     number(3)
 , begin_metingen    date
 , min_leeftijd      number(3)
 , aantal_dieren     number(4)
 , metingen_per_week number(2)
 , looptijd_weken    number(3)
 , sterftekans       number(2)
 , sexe              varchar2(1));
 
alter table datasets add constraint datasets_pk primary key (id) using index;

create table meet_variabelen
( dataset_id        number
 , kwaliteit        number (2)
 , gem_waarde_begin number (6,1)
 , gem_waarde_eind  number (6,1));


create or replace function f_analyse
             (p_genus_id   in integer default null,
              p_species_id in integer  default null,
              p_experiment in integer  default null,
              p_kwaliteit  in integer  default null,
              p_sexe       in varchar2 default null,
              p_aggr_level in varchar2 default 'W',
              p_stats_type in varchar2 default 'A',
			  p_records    in integer  default 15) return my_analysis_row pipelined
is 
l_format   varchar2(10) := case when p_aggr_level = 'W' then 'YYYY/IW'
                                when p_aggr_level = 'D' then 'YYYY/MM/DD'
                                when p_aggr_level = 'M' then 'YYYY/MM'    end;
l_count integer   := 0;
begin 
  <<done>>
  for j in (select to_char (datum, l_format) datum, stats_mode (waarde) sm , median (waarde) med, avg (waarde) gem
            from metingen m, dieren d, genus g, species s, kwaliteiten k, experiment e, datasets ds
            where s.genus_id      = g.genus_id 
              and d.species_id    = s.species_id
              and ds.experiment_id = e.experiment_id
              and m.dier_id       = d.dier_id
			  and d.dataset_id    = ds.id
			  and m.kwaliteit_id  = nvl (p_kwaliteit , m.kwaliteit_id)
			  and g.genus_id      = nvl (p_genus_id  , g.genus_id)
			  and s.species_id    = nvl (p_species_id, s.species_id)
              and e.experiment_id = nvl (p_experiment, e.experiment_id)
              and k.kwaliteit_id  = nvl (p_kwaliteit , k.kwaliteit_id)
              and d.sexe          = nvl (p_sexe      , d.sexe)
              group by to_char (datum, l_format)
		      order by 1 desc)
loop
  pipe row (my_analysis_ty (j.datum, case when p_stats_type = 'M' then j.sm when p_stats_type = 'D' then j.med else j.gem end));
  l_count := l_count + 1;
  exit done when l_count >= p_records;
end loop;

exception when others then
  util.show_error ('Error in function f_analyse.', sqlerrm);
end f_analyse;
/

select * from table (f_analyse (p_genus_id=> 1, p_experiment=>14,p_kwaliteit=> 1, p_records => 12)) order by datum desc;
 
create table datasets
 ( id                number generated always as identity
 , generated         date
 , specied_id        number(3)
 , experiment_id     number(3)
 , begin_metingen    date
 , min_leeftijd      number(3)
 , aantal_dieren     number(4)
 , metingen_per_week number(2)
 , looptijd_weken    number(3)
 , sterftekans       number(2)
 , sexe              varchar2(1));
 
alter table datasets add constraint datasets_pk primary key (id) using index;

create table meet_variabelen
( dataset_id        number
 , kwaliteit        number (2)
 , gem_waarde_begin number (6,1)
 , gem_waarde_eind  number (6,1));

alter table meet_variabelen add constraint meet_variabelen_pk primary key (dataset_id, kwaliteit);
alter table meet_variabelen add constraint meet_variabelen_fk1 foreign key (dataset_id) references datasets (id) on delete cascade;

-- Aaanmaken set
create or replace procedure create_dataset (p_species_id in integer, p_experiment_id in integer, p_begin_metingen date,
     p_min_leeftijd in integer, p_aantal_dieren in integer, p_metingen_per_week in integer, p_looptijd in integer,
     p_kwaliteit in integer, p_sterftekans in integer, p_gemiddelde_waarde_begin in number, p_gemiddelde_waarde_eind in number, p_sexe in varchar2)
is 
 l_max_dier_id  integer;
l_datum         date;
l_geboortedatum date;
l_sterfdatum    date;
l_meet_datum    date;
l_meetwaarde    number (4, 1);
l_count         integer;
l_sexe          varchar2(1);
l_levensverwachting number(4);
begin 
 -- Data validatie deel.
if    p_species_id              is null then raise_application_error (-20010, 'Species is leeg');
elsif p_experiment_id           is null then raise_application_error (-20011, 'Experiment is leeg');
elsif p_begin_metingen          is null then raise_application_error (-20012, 'Begin metingen is leeg');
elsif p_min_leeftijd            is null then raise_application_error (-20013, 'Min leeftijd is leeg');
elsif p_aantal_dieren           is null then raise_application_error (-20014, 'Aantal dieren is leeg');
elsif p_metingen_per_week       is null then raise_application_error (-20015, 'Metingen per week is leeg');
elsif p_looptijd                is null then raise_application_error (-20016, 'Looptijd is leeg');
elsif p_kwaliteit               is null then raise_application_error (-20017, 'Kwaliteit is leeg');
elsif p_sterftekans             is null then raise_application_error (-20018, 'Sterftekans is leeg');
elsif p_gemiddelde_waarde_begin is null then raise_application_error (-20019, 'Beginwaarde is leeg');
elsif p_gemiddelde_waarde_eind  is null then raise_application_error (-20020, 'Eindwaarde is leeg');
end if;

if    p_min_leeftijd      not between 30 and 1000 then raise_application_error (-20001, 'Minimum leeftijd niet tussen 30 en 1000 dagen');
elsif p_sterftekans       not between  2 and 95   then raise_application_error (-20002, 'Minimum leeftijd niet tussen 2 en 95');
elsif p_metingen_per_week not between  1 and 7    then raise_application_error (-20003, 'Metingen per week niet tussen 1 en 7');
elsif p_aantal_dieren     not between  1 and 1000 then raise_application_error (-20004, 'Aantal dieren niet tussen 1 en 1000');
elsif p_looptijd          not between  1 and 200  then raise_application_error (-20004, 'Looptijd niet tussen 1 en 200');
end if;

select count(*) into l_count from species where species_id = p_species_id;
if l_count = 0 then raise_application_error (-20005, 'Species ' || p_species_id || ' bestaat niet'); end if;

 select count(*) into l_count from experiment where experiment_id = p_experiment_id;
if l_count = 0 then raise_application_error (-20006, 'Experiment ' || p_experiment_id || ' bestaat niet'); end if;

-- Triggers uit
execute immediate 'alter table dieren   disable all triggers';
execute immediate 'alter table metingen disable all triggers';

-- logging
insert into datasets (generated, specied_id, experiment_id, begin_metingen, min_leeftijd, aantal_dieren, metingen_per_week, looptijd_w,
                      kwaliteit, sterftekans, gem_waarde_begin, gem_waarde_eind, sexe)
	values (sysdate, p_species_id, p_experiment_id, p_begin_metingen, p_min_leeftijd, p_aantal_dieren, p_metingen_per_week, p_looptijd,
            p_kwaliteit, p_sterftekans, p_gemiddelde_waarde_begin, p_gemiddelde_waarde_eind, p_sexe);

select nvl(max (dier_id), 0) into l_max_dier_id from dieren;
select levensverwachting into l_levensverwachting from species where species_id = p_species_id;
for j in 1 .. p_aantal_dieren
loop 
   l_max_dier_id   := l_max_dier_id + 1;  -- Increment dier_id
   l_geboortedatum := p_begin_metingen - p_min_leeftijd - trunc (dbms_random.value (1, 7));
   l_sterfdatum    := p_begin_metingen + trunc ((1 - p_sterftekans / 100) * dbms_random.value (2, l_levensverwachting));
   if l_sterfdatum > sysdate then l_sterfdatum := null; end if;
   if    p_sexe = 'M' then l_sexe := 'M';
   elsif p_sexe = 'F' then l_sexe := 'F';
   else  l_sexe  := case when mod (trunc(dbms_random.value (1,100)), 2) = 0 then 'F' else 'M' end;
   end if;
   insert into dieren (dier_id, species_id, geboortedatum, sexe, experiment_id, sterfdatum)
      values (l_max_dier_id, p_species_id, l_geboortedatum, l_sexe, p_experiment_id, l_sterfdatum);
   <<dood>>
   for w in 0 .. p_looptijd - 1
   loop
     l_datum := p_begin_metingen + w * 7;
     for a in 0 .. p_metingen_per_week - 1
              loop 
                 l_meet_datum := l_datum + a * trunc (7 / p_metingen_per_week);
                 exit dood when l_sterfdatum <= l_meet_datum;
                 l_meetwaarde := p_gemiddelde_waarde_begin - 1 + w * (p_gemiddelde_waarde_eind - p_gemiddelde_waarde_begin) / p_looptijd * dbms_random.value (0,2);
                 insert into metingen (dier_id, datum, kwaliteit_id, waarde) values (l_max_dier_id, l_meet_datum, p_kwaliteit, l_meetwaarde);    
               end loop;
   end loop;

 -- Triggers aan + commit.
execute immediate 'alter table dieren enable   all triggers';
execute immediate 'alter table metingen enable all triggers';
end loop;
end create_dataset;
/

 create or replace force editionable view v_sum_medewerkers_genus (voornaam, achternaam, functie, experimenten, muis, rat, cavia, hamster, konijn, totaal) as 
 select voornaam, achternaam, functie,
          count(distinct e.experiment_id) experimenten,
          sum(decode(g.naam, 'Muis', 1, 0)) muis,
          sum(decode(g.naam, 'Rat', 1, 0)) rat,
	      sum(decode(g.naam, 'Cavia', 1, 0)) cavia,
	      sum(decode(g.naam, 'Hamster', 1, 0)) hamster,
	      sum(decode(g.naam, 'Konijn', 1, 0)) konijn,
	      count(*) totaal
from  werknemers w, koppeltabel k, experiment e, dieren d, species s, genus g
where k.experiment_id = e.experiment_id
  and k.werknemer_id  = w.werknemer_id
  and d.experiment_id = e.experiment_id
  and s.species_id    = d.species_id
  and s.genus_id      = g.genus_id
  group by voornaam, achternaam, functie
order by functie, achternaam, voornaam ;

exec create_dataset (1, 12, trunc(sysdate) - 150,  90, 200, 2, 20, 1, 7, 42.7, 38.4)

-- Eerste maandag van de maand. Noodzakelijk ivm LOV
select next_day (add_months(trunc(sysdate,'mm'), - level) - 1, 'Monday') from dual connect by level <= 20;

-- Leeftijd dieren
select s.naam, trunc (sterfdatum - geboortedatum) leeftijd, count(*) aantal
from dieren d, species s where
s.species_id = d.species_id
and sterfdatum is not null
group by s.naam, trunc (sterfdatum - geboortedatum)
order by 1,2;

--
-- Naar welke beroepen gaat het meeste salaris. dense_rank geeft alle distincte waarden
select functie, rank () over (order by sum(salaris)), dense_rank () over (order by sum(salaris)) from werknemers group by functie;
select functie, plaats, rank () over (order by plaats), dense_rank () over (order by plaats) from werknemers group by functie, plaats;

-- Lineaire regressie datum / kwaliteit
with kwaliteit as (select kwaliteit_id, min(datum) datum from metingen group by kwaliteit_id)
select distinct k.kwaliteit_id,
  regr_slope(m.waarde, m.datum - k.datum) over (partition by k.kwaliteit_id) slope,
  regr_intercept(m.waarde, m.datum - k.datum) over (partition by k.kwaliteit_id) intcpt,
    regr_count(m.waarde, m.datum - k.datum) over (partition by k.kwaliteit_id) aantal
   from metingen m, kwaliteit k
where k.kwaliteit_id = m.kwaliteit_id;

with min_kw as (select kwaliteit_id, min(datum) datum from metingen group by kwaliteit_id)
select k.kwaliteit_id,
  regr_slope(m.waarde, m.datum - k.datum) over (partition by k.kwaliteit_id) slope,
  regr_intercept(m.waarde, m.datum - k.datum) over (partition by k.kwaliteit_id) intcpt,
    regr_count(m.waarde, m.datum - k.datum) over (partition by k.kwaliteit_id) aantal
            from metingen m, dieren d, genus g, species s, kwaliteiten k, experiment e, datasets ds, min_kw mk
            where s.genus_id      = g.genus_id 
              and d.species_id    = s.species_id
              and ds.experiment_id = e.experiment_id
              and m.dier_id       = d.dier_id
			  and d.dataset_id    = ds.id
			  and m.kwaliteit_id  = nvl (p_kwaliteit , m.kwaliteit_id)
			  and mk.kwaliteit_id = m.kwaliteit_id;
			  and g.genus_id      = nvl (:P14_GENUS  , g.genus_id)
			  and s.species_id    = nvl (:P14_SPECIES, s.species_id)
              and e.experiment_id = nvl (:P14_EXPERIMENT, e.experiment_id)
              and k.kwaliteit_id  = nvl (:P14_KWALITEIT , k.kwaliteit_id)
              and d.sexe          = nvl (:P14_SEXE  , d.sexe)
              group by k.kwaliteit_id
		      order by 1 desc
			  fetch nvl(:P14_RECORDS, 12) only


-- Quartile + row number
select werknemer_id, plaats, ntile(4) over (partition by plaats order by werknemer_id) as tile4, 
row_number() over (partition by plaats order by werknemer_id) as row_number  from werknemers
order by plaats;

-- Windowing functions. Salaris per woonplaats
select plaats, salaris, first_value (salaris) over (partition by plaats order by salaris desc) max_sal_per_plaats,
last_value (salaris) over (partition by plaats order by salaris desc range between unbounded preceding and current row) min_sofar,
last_value (salaris) over (partition by plaats order by salaris desc range between unbounded preceding and current row) min_partition,
lag (salaris) over (partition by plaats order by salaris desc),
lead (salaris) over (partition by plaats order by salaris desc)
from werknemers
order by plaats;

-- Pagina 6. Werkt nog niet goed. Geen rechte lijn ivm avg functie
select * from table (f_analyse (p_genus_id=> :P14_GENUS, p_species_id => :P14_SPECIES, p_experiment => :P14_EXPERIMENT,
   p_kwaliteit => :P14_KWALITEIT, p_sexe => :P14_SEXE, p_aggr_level => nvl(:P14_AGGR_LEVEL, 'W'),
    p_stats_type => NVL (:P14_STATS_TYPE,'A'), p_records => nvl(:P14_RECORDS, 12)))
   order by datum


create or replace function f_regressie
             (p_genus_id   in integer default null,
              p_species_id in integer  default null,
              p_experiment in integer  default null,
              p_kwaliteit  in integer  default null,
              p_sexe       in varchar2 default null,
              p_aggr_level in varchar2 default 'W',
			  p_records    in integer  default 15) return my_analysis_row pipelined
is 
l_format   varchar2(10) := case when p_aggr_level = 'W' then 'YYYY/IW'
                                when p_aggr_level = 'D' then 'YYYY/MM/DD'
                                when p_aggr_level = 'M' then 'YYYY/MM'    end;
l_count     integer   := 0;
begin 
  <<done>>
  for r in (with min_kw as (select kwaliteit_id, min(datum) datum from metingen where kwaliteit_id = p_kwaliteit group by kwaliteit_id)
          select distinct k.kwaliteit_id, mk.datum,
            regr_slope     (m.waarde, m.datum - mk.datum) over (partition by k.kwaliteit_id) slope,
            regr_intercept (m.waarde, m.datum - mk.datum) over (partition by k.kwaliteit_id) intcpt,
            regr_count     (m.waarde, m.datum - mk.datum) over (partition by k.kwaliteit_id) aantal
            from metingen m, dieren d, genus g, species s, kwaliteiten k, experiment e, datasets ds, min_kw mk
            where s.genus_id       = g.genus_id 
              and d.species_id     = s.species_id
              and ds.experiment_id = e.experiment_id
              and m.dier_id        = d.dier_id
			  and d.dataset_id     = ds.id
			  and mk.kwaliteit_id  = m.kwaliteit_id
			  and m.kwaliteit_id   = p_kwaliteit
			  and k.kwaliteit_id   = mk.kwaliteit_id
			  and g.genus_id       = nvl (p_genus_id  , g.genus_id)
			  and s.species_id     = nvl (p_species_id, s.species_id)
              and e.experiment_id  = nvl (p_experiment, e.experiment_id)
              and (d.sexe          = nvl (p_sexe, d.sexe) or p_sexe = 'B'))
   loop
     for j in (select to_char (m.datum, l_format) datum, r.intcpt + r.slope * avg (m.datum - r.datum) y_val
            from metingen m, dieren d, genus g, species s, kwaliteiten k, experiment e, datasets ds
            where s.genus_id       = g.genus_id 
              and d.species_id     = s.species_id
              and ds.experiment_id = e.experiment_id
              and m.dier_id        = d.dier_id
			  and d.dataset_id     = ds.id
			  and m.kwaliteit_id   = r.kwaliteit_id
			  and k.kwaliteit_id   = r.kwaliteit_id
			  and g.genus_id       = nvl (p_genus_id  , g.genus_id)
			  and s.species_id     = nvl (p_species_id, s.species_id)
              and e.experiment_id  = nvl (p_experiment, e.experiment_id)
              and (d.sexe          = nvl (p_sexe, d.sexe) or p_sexe = 'B')
              group by to_char (datum, l_format)
		      order by 1 desc)
    loop
      pipe row (my_analysis_ty (j.datum, j.y_val));
      l_count := l_count + 1;
      exit done when l_count >= p_records;
    end loop;
  end loop;

exception when others then
  util.show_error ('Error in function f_regressie.', sqlerrm);
end f_regressie;
/

select * from table (f_regressie (p_genus_id=> 1, p_species_id => 1, p_experiment => 2, p_kwaliteit => 1, p_sexe => 'B', p_records => 20)) order by datum;
   
   
