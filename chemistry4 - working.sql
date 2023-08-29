/*************************************************************************************************************************************************

Name:        chemistry_pkg.sql

Created      October  2022
Last update  October  2022

Author:      Theo stienissen

E-mail:      theo.stienissen@gmail.com

Link:   @C:\Users\Theo\OneDrive\Theo\Project\Chemistry\chemistry4.sql

Todo:

alter trigger molecule_ari disable;
insert into molecule (formula,name_eng,name_dutch) select formula,name_eng,name_dutch from molecule_t order by id;
alter trigger molecule_ari enable;
insert into molecule_elements  (molecule_id, atom_nr, occurences) select molecule_id, atom_nr, occurences from molecule_elements_t
where molecule_id in (select id from molecule);
insert into reaction (exercise, reaction,info) select exercise, reaction,info from reaction_t;
insert into reagent (reaction_id,molecule_id,in_out, factor) select reaction_id,molecule_id,in_out, factor from reagent_t;

*************************************************************************************************************************************************/

set serveroutput on size unlimited

alter session set plsql_warnings = 'ENABLE:ALL'; 

drop table molecule_elements cascade constraints;
drop table molecule          cascade constraints;
drop table reagent           cascade constraints;
drop table reaction          cascade constraints;
drop table molecule_elements_tmp cascade constraints;

create table molecule
( id         number (6)
, formula    varchar2(50) not null
, name_eng   varchar2(50) not null
, name_dutch varchar2(50) not null);

alter table molecule add constraint molecule_pk primary key (id) using index;
create unique index molecule_uk1 on molecule (formula);

create sequence molecule_seq start with 16;

create or replace trigger molecule_briu
before insert or update on molecule
for each row
begin
  :new.id         := nvl (:new.id, molecule_seq.nextval);
  :new.name_eng   := initcap (:new.name_eng);
  :new.name_dutch := initcap (:new.name_dutch);
end molecule_ari;
/

create or replace trigger molecule_ari
after insert on molecule
for each row
begin
  chemistry_pkg.parse_formula (:new.id, :new.formula);
exception when others then null;
end molecule_briu;
/

create global temporary table molecule_elements_tmp
( molecule_id integer not null
, atom_nr     integer not null
, occurences  integer not null)
on commit delete rows;

create table molecule_elements
( molecule_id integer not null
, atom_nr     integer not null
, occurences  integer not null);


--create or replace trigger molecule_elements_briu
--before insert or update on molecule_elements
--for each row 
--begin 
-- if :new.atom_name   is null then :new.atom_name   := chemistry_pkg.get_atom_name(:new.atom_nr);   end if;
 --if :new.atom_weight is null then :new.atom_weight := chemistry_pkg.get_atom_weight(:new.atom_nr); end if;
--end molecule_elements_briu;
--/ 
 

alter table molecule_elements add constraint molecule_elements_pk primary key (molecule_id, atom_nr) using index;
alter table molecule_elements add constraint molecule_elements_fk1 foreign key (molecule_id) references molecule (id) on delete cascade;

create table reaction
( id         number (6)
, exercise   varchar2 (100)
, reaction   varchar2 (200)
, info       varchar2 (400));

create sequence reaction_seq start with 2;

create or replace trigger reaction_briu
before insert or update on reaction
for each row
begin
  :new.id := nvl (:new.id, reaction_seq.nextval);
end reaction_briu;
/

alter table reaction add constraint reaction_pk primary key (id) using index;

create table reagent
( reaction_id  integer
, molecule_id  integer
, in_out       number (1) -- 1 = in, 0 = out
, factor       number(6, 0));

alter table reagent add constraint reagent_pk primary key (reaction_id, molecule_id) using index;
alter table reagent add constraint reagent_fk1 foreign key (reaction_id) references reaction (id) on delete cascade;
alter table reagent add constraint reagent_fk2 foreign key (molecule_id) references molecule (id) on delete set null;

create or replace package chemistry_pkg
is
function lcm (p_reaction_id in integer, p_atom_nr in integer) return integer;

procedure parse_formula (p_molecule_id in number, p_string in varchar2);

function molecule_weight (p_molecule_id in integer) return number deterministic;

function get_atom_name (p_atom_nr in integer, p_lang in varchar2 default 'E') return varchar2 deterministic;

function get_atom_weight (p_atom_nr in integer) return number deterministic;

function show_reaction (p_reaction_id in integer) return varchar2;

function f_chemistry_matrix return equation_matrix_tab pipelined;

function convert_to_matrix (p_reaction_id in integer) return types_pkg.matrix_Q_ty;

procedure balance_equation (p_reaction_id in integer);
end chemistry_pkg;
/

create or replace package body chemistry_pkg
is
function lcm (p_reaction_id in integer, p_atom_nr in integer) return integer
is
l_lcm   integer;
begin
  for j in (select me.molecule_id, me.occurences from molecule m, molecule_elements me
          where m.id          = me.molecule_id
            and me.atom_nr    = p_atom_nr)
  loop
    l_lcm := maths.lcm (j.occurences, nvl (l_lcm, j.occurences));
  end loop;
  return l_lcm;

exception when others then
  util.show_error ('Error in function lcm for reaction: ' || p_reaction_id || ' and atom: ' || p_atom_nr || '.', sqlerrm);
end lcm;

/*************************************************************************************************************************************************/

procedure parse_formula (p_molecule_id in number, p_string in varchar2)
is
l_symbol     varchar2 (3);
l_occurences  number (3);
l_atom_number number (3);
begin
  for j in 1 .. length (p_string)
  loop
    l_occurences := 1;
    if substr (p_string, j, 1) between 'A' and 'Z'
    then
      if j < length (p_string)
      then
        if substr (p_string, j + 1, 1) between 'a' and 'z'
                  then l_symbol := substr (p_string, j, 2); if substr (p_string, j + 2, 1) between '1' and '9' then l_occurences := substr (p_string, j + 2, 1); end if;
                  else l_symbol := substr (p_string, j, 1); if substr (p_string, j + 1, 1) between '1' and '9' then l_occurences := substr (p_string, j + 1, 1); end if;
                  end if;
      else l_symbol := substr (p_string, j, 1);
      end if;
        select max (atom_nr) into l_atom_number from periodic_system where symbol = l_symbol;
        if l_atom_number is not null
        then
          insert into molecule_elements_tmp (molecule_id, atom_nr, occurences) values (p_molecule_id, l_atom_number, l_occurences);
        end if;
    end if;
  end loop;
  delete molecule_elements where molecule_id = p_molecule_id;
  insert into molecule_elements (molecule_id, atom_nr, occurences) select molecule_id, atom_nr, sum (occurences) from molecule_elements_tmp group by molecule_id, atom_nr;

exception when others then
  util.show_error ('Error in procedure parse_formula for molecule ID: ' || p_molecule_id || ' and string: ' || p_string || '.', sqlerrm);
end parse_formula;

/*************************************************************************************************************************************************/

function molecule_weight (p_molecule_id in integer) return number deterministic
is
l_sum number;
begin
  select sum (me.occurences * get_atom_weight (me.atom_nr))
  into l_sum from molecule m, molecule_elements me, periodic_system ps
  where m.id = me.molecule_id and me.molecule_id = p_molecule_id and me.atom_nr = ps.atom_nr;
  return l_sum;

exception when others then
  util.show_error ('Error in function molecule_weight for molecule ID: ' || p_molecule_id || '.', sqlerrm);
end molecule_weight;

/*************************************************************************************************************************************************/

function get_atom_name (p_atom_nr in integer, p_lang in varchar2 default 'E') return varchar2 deterministic
is
l_atom_name periodic_system.element_name_eng%type;
begin
  if p_lang = 'E'
  then select element_name_eng   into l_atom_name from periodic_system where atom_nr = p_atom_nr;
  else select element_name_dutch into l_atom_name from periodic_system where atom_nr = p_atom_nr;
  end if;
  return l_atom_name;

exception when others then
  util.show_error ('Error in function get_atom_name for atomnumer: ' || p_atom_nr || '.', sqlerrm);
  return null;
end get_atom_name;

/*************************************************************************************************************************************************/

function get_atom_weight (p_atom_nr in integer) return number deterministic
is
l_atom_weight periodic_system.atomic_weight%type;
begin
  select atomic_weight into l_atom_weight from periodic_system where atom_nr = p_atom_nr;
  return l_atom_weight;

exception when others then
  util.show_error ('Error in function get_atom_weight for NR: ' || p_atom_nr || '.', sqlerrm);
  return null;
end get_atom_weight;

/*************************************************************************************************************************************************/

function show_reaction (p_reaction_id in integer) return varchar2
is
l_string varchar2(400) := '';
l_first  boolean := true;
function display (p_factor in integer) return varchar2
is 
begin 
  if p_factor is null then return ''; else return to_char (p_factor) || ' '; end if;
end display;
begin
  for j in (select m.formula, r.factor from molecule m, reagent r where m.id = r.molecule_id and r.in_out = 1 and r.reaction_id = p_reaction_id order by m.id)
  loop
    if  l_first
    then l_first  := false; l_string := display (j.factor) || j.formula;
    else l_string := l_string || ' + ' || display (j.factor) || j.formula;
    end if;
  end loop;
  if not l_first then l_string := l_string || ' --> '; end if;
  l_first  := true;
  for j in (select m.formula, r.factor from molecule m, reagent r where m.id = r.molecule_id and r.in_out = 0 and r.reaction_id = p_reaction_id order by m.id)
  loop
    if   l_first
    then l_first  := false; l_string := l_string || display (j.factor) || j.formula;
    else l_string := l_string || ' + ' || display (j.factor) || j.formula;
    end if;
  end loop;
  return l_string;

exception when others then
  util.show_error ('Error in function show_reaction for ID: ' || p_reaction_id || '.', sqlerrm);
  return null;
end show_reaction;

/*************************************************************************************************************************************************/

function f_chemistry_matrix return equation_matrix_tab pipelined
is
l_counter          integer;
l_molecule_counter integer;
l_symbol           varchar2 (3);
type int_array     is table of pls_integer index by pls_integer;
type string_array  is table of varchar2 (50) index by pls_integer;
l_int_array        int_array;
l_string_array     string_array;
procedure reset_int_array
is
begin
  l_int_array.delete;
  for j in 1 .. 10
  loop
    if j <= l_molecule_counter then l_int_array (j) := 0; else l_int_array (j) := null; end if;
  end loop;
end reset_int_array;
--
begin
for i in (select id from reaction order by id)
loop
  select m.formula bulk collect into l_string_array from reagent rg, molecule m where rg.molecule_id = m.id and i.id = rg.reaction_id order by rg.in_out desc, m.formula;
  l_molecule_counter := l_string_array.count;
  for j in l_string_array.count + 1 .. 10
  loop
      l_string_array (j) := '';
  end loop;
 
 -- Get the atoms for this reaction.
  for s in (select distinct me.atom_nr, ps.symbol from reagent rg, molecule_elements me, periodic_system ps
            where i.id = rg.reaction_id and rg.molecule_id = me.molecule_id and ps.atom_nr = me.atom_nr order by ps.symbol)
  loop   
    reset_int_array;
 -- Get the molecules for this reaction
    for m in (select m.id, m.formula,
              row_number () over (partition by rg.reaction_id order by rg.in_out desc, m.formula) rnk from molecule m, reagent rg
              where m.id = rg.molecule_id and i.id = rg.reaction_id)
    loop 

      -- Check if atom is present in this molecule
      for a in (select me.atom_nr, case rg.in_out when 0 then sum (me.occurences) else - sum (me.occurences) end occurences
                from molecule_elements me, reagent rg
				where me.molecule_id = m.id and rg.reaction_id = i.id and rg.molecule_id = me.molecule_id and s.atom_nr = me.atom_nr
				group by me.atom_nr, rg.in_out)
      loop  
        l_int_array (m.rnk) := a.occurences;
      end loop;
    end loop;
	
    pipe row (equation_matrix_row (i.id, s.symbol, l_int_array (1), l_string_array (1), l_int_array (2), l_string_array (2), l_int_array (3), l_string_array (3),
                                l_int_array (4), l_string_array (4), l_int_array (5), l_string_array (5), l_int_array (6), l_string_array (6),
                                l_int_array (7), l_string_array (7), l_int_array (8), l_string_array (8), l_int_array (9), l_string_array (9),
                                l_int_array (10), l_string_array (10)));
  end loop;
end loop;

exception when no_data_needed then null;
  when others then
    util.show_error ('Error in function f_chemistry_matrix.', sqlerrm);
end f_chemistry_matrix;

/*************************************************************************************************************************************************/

--
-- Assume that the value for a = 1 and put in the last column. Code is not optimal.
--
function convert_to_matrix (p_reaction_id in integer) return types_pkg.matrix_Q_ty
is
l_vector     types_pkg.vector_Q_ty;
l_matrix     types_pkg.matrix_Q_ty;
l_count_m    pls_integer;
l_count      pls_integer := 1;
begin
  select count(*) into l_count_m from molecule m, reagent rg where m.id = rg.molecule_id and rg.reaction_id = p_reaction_id;
  if l_count_m >= 2
  then
  case l_count_m
  when 2 then 
    for j in (select b, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.a));	   
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 3 then 
    for j in (select b, c, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 3 then 
    for j in (select b, c, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 4 then 
    for j in (select b, c, d, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.d), fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 5 then 
    for j in (select b, c, d, e, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.d),
       fractions_pkg.to_fraction(j.e), fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 6 then 
    for j in (select b, c, d, e, f, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.d),
       fractions_pkg.to_fraction(j.e), fractions_pkg.to_fraction(j.f), fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 7 then 
    for j in (select b, c, d, e, f, g, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.d),
       fractions_pkg.to_fraction(j.e), fractions_pkg.to_fraction(j.f), fractions_pkg.to_fraction(j.g), fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 8 then 
    for j in (select b, c, d, e, f, g, h, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.d),
       fractions_pkg.to_fraction(j.e), fractions_pkg.to_fraction(j.f), fractions_pkg.to_fraction(j.g), fractions_pkg.to_fraction(j.h),
	   fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 9 then 
    for j in (select b, c, d, e, f, g, h, i, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.d),
       fractions_pkg.to_fraction(j.e), fractions_pkg.to_fraction(j.f), fractions_pkg.to_fraction(j.g), fractions_pkg.to_fraction(j.h),
	   fractions_pkg.to_fraction(j.i), fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  when 10 then 
    for j in (select b, c, d, e, f, g, h, i, j, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop 
	   l_vector := matrix_Q_pkg.to_vector (fractions_pkg.to_fraction(j.b), fractions_pkg.to_fraction(j.c), fractions_pkg.to_fraction(j.d),
       fractions_pkg.to_fraction(j.e), fractions_pkg.to_fraction(j.f), fractions_pkg.to_fraction(j.g), fractions_pkg.to_fraction(j.h),
	   fractions_pkg.to_fraction(j.i), fractions_pkg.to_fraction(j.j), fractions_pkg.to_fraction(j.a));
       l_matrix := matrix_Q_pkg.add_row (l_matrix, l_count, l_vector);
       l_count  := l_count + 1;
    end loop;
  end case;
  l_matrix := matrix_Q_pkg.gauss_jordan_elimination (l_matrix);
  end if;
  return l_matrix;

exception when others then
  util.show_error ('Error in function convert_to_matrix for reaction ID: ' || p_reaction_id, sqlerrm);
  return constants_pkg.empty_matrix;
end convert_to_matrix;

/*************************************************************************************************************************************************/

--
-- Todo
--
procedure balance_equation (p_reaction_id in integer)
is
l_string     varchar2(200);
l_first      boolean := true;
l_matrix     types_pkg.matrix_Q_ty := chemistry_pkg.convert_to_matrix (p_reaction_id);
l_vector     types_pkg.vector_Q_ty;
l_factor     integer := 1;
l_index      pls_integer := 0;
l_count      integer;
begin
  l_count := l_matrix (1).count;
  for j in 1.. l_matrix.count 
  loop 
    l_factor := maths.lcm (l_factor, l_matrix (j)(l_count).denominator);
  end loop;
  l_vector := matrix_Q_pkg.scalar_times_vector (fractions_pkg.to_fraction (l_factor, 1), matrix_Q_pkg.column_to_vector (l_matrix, l_count));
--

  for j in (select m.formula, rg.molecule_id from molecule m, reagent rg where m.id = rg.molecule_id and rg.reaction_id = p_reaction_id and rg.in_out = 1 order by m.formula)
  loop
    if   l_first
	then l_string := to_char (l_factor) || ' ' || j.formula;
	else l_factor := l_vector (l_index).numerator * l_vector (l_index).denominator;
	     l_string := l_string || ' + ' || to_char (l_factor) || ' ' || j.formula;
	end if;
   update reagent set factor = l_factor where reaction_id = p_reaction_id and molecule_id = j.molecule_id;
	 l_index := l_index + 1;
	 l_first := false; 
  end loop;
--
  if not l_first then l_string := l_string || ' --> '; end if;
  l_first  := true;
  for j in (select m.formula, rg.molecule_id from molecule m, reagent rg where m.id = rg.molecule_id and rg.reaction_id = p_reaction_id and rg.in_out = 0 order by m.formula)
  loop
    l_factor := l_vector (l_index).numerator * l_vector (l_index).denominator;
    if not l_first then l_string := l_string || ' + '; end if;
	l_string := l_string || to_char (l_factor) || ' ' || j.formula;
	update reagent set factor = l_factor where reaction_id = p_reaction_id and molecule_id = j.molecule_id;
	 l_index := l_index + 1;
	 l_first := false; 
  end loop;
  update reaction set reaction = l_string where id = p_reaction_id;
  commit;

exception when others then
  util.show_error ('Error in procedure balance_equation for reaction ID: ' || p_reaction_id, sqlerrm);
end balance_equation;

end chemistry_pkg;
/


select chemistry_pkg.show_reaction(1) from dual;
set serveroutput on size unlimited
set lines 200
col a for 90
col b for 90
col c for 90
col d for 90
col e for 90
col f for 90
col g for 90
col h for 90
col i for 90
col j for 90
col molecule_a for a15
col molecule_b for a15
col molecule_c for a15
col molecule_d for a15
col molecule_e for a15
col molecule_f for a15
col molecule_g for a15
col molecule_h for a15
col molecule_i for a15
col molecule_j for a15
col symbol for a10
select distinct reaction_id,molecule_a,molecule_b,molecule_c, molecule_d, molecule_e, molecule_f, molecule_g, molecule_h, molecule_i, molecule_j from table (chemistry_pkg.f_chemistry_matrix) order by reaction_id;
select reaction_id, symbol,a,b,c,d,e,f,g,h,i,j from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = 23 order by reaction_id, symbol;
exec chemistry_pkg.balance_equation(1)


select ps.symbol, m.in_out,  me.molecule_id, occurences
from molecule m, molecule_elements me, periodic_system ps
where m.id = me.molecule_id
and me.atom_nr = ps.atom_nr
and exercise_id = 1
order by ps.symbol, m.in_out desc;

drop type equation_matrix_tab;
drop type equation_matrix_row;


create or replace type equation_matrix_row as object
( reaction_id   integer 
, symbol        varchar2 (3)
, a             integer (3)
, molecule_a    varchar2(50)
, b             integer (3)
, molecule_b    varchar2(50)
, c             integer (3)
, molecule_c    varchar2(50)
, d             integer (3)
, molecule_d    varchar2(50)
, e             integer (3)
, molecule_e    varchar2(50)
, f             integer (3)
, molecule_f    varchar2(50)
, g             integer (3)
, molecule_g    varchar2(50)
, h             integer (3)
, molecule_h    varchar2(50)
, i             integer (3)
, molecule_i    varchar2(50)
, j             integer (3)
, molecule_j    varchar2(50));
/
  
create or replace type equation_matrix_tab as table of equation_matrix_row;
/

select * from table (chemistry_pkg.f_chemistry_matrix) where exercise_id = 3;

create or replace view v_molecules
as 
select m.id, m.formula molecule, m.name_eng, m.name_dutch, chemistry_pkg.molecule_weight (m.id) molecular_mass
from molecule m;

create or replace view v_molecule_elements
as 
select m.id, m.formula, me.atom_nr, chemistry_pkg.get_atom_name (me.atom_nr) atom_name, chemistry_pkg.get_atom_name (me.atom_nr, 'D') atom_name_dutch,
      chemistry_pkg.get_atom_weight(me.atom_nr) atom_weight, me.occurences,
      me.occurences * chemistry_pkg.get_atom_weight(me.atom_nr) total_atom_weight
from molecule m, molecule_elements me 
where m.id = me.molecule_id
order by m.id, me.atom_nr;

create or replace view v_molecule_reactions
as 
select r.id, r.reaction, rg.in_out, m.formula molecule, chemistry_pkg.molecule_weight (m.id) molecular_mass
from reaction r, reagent rg, molecule m
where r.id = rg.reaction_id and m.id = rg.molecule_id
order by rg.in_out, m.formula;