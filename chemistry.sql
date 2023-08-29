/*************************************************************************************************************************************************

Name:        chemistry_pkg.sql

Created      October  2022
Last update  October  2022

Author:      Theo stienissen

E-mail:      theo.stienissen@gmail.com

Link:   @C:\Users\Theo\OneDrive\Theo\Project\Chemistry\chemistry.sql

Todo:

*************************************************************************************************************************************************/

set serveroutput on size unlimited

alter session set plsql_warnings = 'ENABLE:ALL'; 

drop table molecule_elements cascade constraints;
drop table molecule cascade constraints;
drop table exercise cascade constraints;

create table exercise
( id       integer generated always as identity
, name     varchar2(50)
, reaction varchar2(200));

insert into exercise (name) values ('Difficult');

alter table exercise add constraint exercise_pk primary key (id) using index;

create table molecule
( id       integer generated always as identity
, exercise_id integer
, in_out   integer
, name     varchar2(50));

insert into molecule (exercise_id, name) select e.id, m2.name from molecule2 m2, exercise e;

alter table molecule add constraint molecule_pk primary key (id) using index;
alter table molecule add constraint molecule_fk1 foreign key (exercise_id) references exercise (id) on delete set null;
alter table molecule modify exercise_id not null;
alter table molecule modify in_out not null;
alter table molecule modify name not null;

create table molecule_elements
( molecule_id integer
, atom_nr     integer
, occurences  integer);

insert into molecule_elements (molecule_id, atom_nr, occurences) select molecule_id, atom_nr, occurences from molecule_elements2;

alter table molecule_elements add constraint molecule_elements_pk primary key (molecule_id, atom_nr) using index;
alter table molecule_elements add constraint molecule_elements_fk1 foreign key (molecule_id) references molecule (id) on delete cascade;
alter table molecule_elements modify occurences not null;
select symbol, atom_nr from periodic_system order by atom_nr;


create or replace package chemistry_pkg
is
function lcm (p_exercise_id in integer, p_atom_nr in integer) return integer;

procedure parse_formula (p_molecule_id in number, p_string in varchar2);

function molecule_weight (p_molecule_id in integer) return number deterministic;

function show_reaction (p_exercise_id in integer) return varchar2;

function f_chemistry_matrix return equation_matrix_tab pipelined;

function convert_to_matrix (p_exercise_id in integer) return types_pkg.matrix_Q_ty;

procedure balance_equation (p_exercise_id in integer);
end chemistry_pkg;
/

create or replace package body chemistry_pkg
is
function lcm (p_exercise_id in integer, p_atom_nr in integer) return integer
is
l_lcm   integer;
begin
  for j in (select m.in_out, me.molecule_id, me.occurences from molecule m, molecule_elements me
          where m.id = me.molecule_id
            and m.exercise_id = p_exercise_id
            and me.atom_nr = p_atom_nr)
  loop
    l_lcm := maths.lcm (j.occurences, nvl (l_lcm, j.occurences));
  end loop;
  return l_lcm;  

exception when others then
  util.show_error ('Error in function lcm.', sqlerrm);
end lcm;

/*************************************************************************************************************************************************/

procedure parse_formula (p_molecule_id in number, p_string in varchar2)
is
l_element     varchar2 (3);
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
                  then l_element := substr (p_string, j, 2); if substr (p_string, j + 2, 1) between 1 and 9 then l_occurences := substr (p_string, j + 2, 1); end if;
                  else l_element := substr (p_string, j, 1); if substr (p_string, j + 1, 1) between 1 and 9 then l_occurences := substr (p_string, j + 1, 1); end if;
                  end if;
      else l_element := substr (p_string, j, 1);
      end if;
        select max (atom_nr) into l_atom_number from periodic_system where symbol = l_element;
        if l_atom_number is not null
        then
          insert into molecule_elements (molecule_id, atom_nr, occurences) values (p_molecule_id, l_atom_number, l_occurences);
        end if;
    end if;
  end loop;

exception when others then
  util.show_error ('Error in procedure parse_formula.', sqlerrm);
end parse_formula;

/*************************************************************************************************************************************************/

function molecule_weight (p_molecule_id in integer) return number deterministic
is
l_sum number;
begin
  select round(sum(me.occurences * to_number(rtrim(ltrim(mass, '('), ')'), '99999D9999999', 'NLS_NUMERIC_CHARACTERS=''.,''')), 3)
  into l_sum from molecule m, molecule_elements me, periodic_system ps
  where m.id = me.molecule_id
    and me.molecule_id = p_molecule_id
    and me.atom_nr = ps.atom_nr;
    return l_sum;
  
exception when others then
  util.show_error ('Error in function molecule_weight.', sqlerrm);
end molecule_weight;

/*************************************************************************************************************************************************/

function show_reaction (p_exercise_id in integer) return varchar2
is
l_string varchar2(400) := '';
l_first  boolean := true;
begin
  select reaction into l_string from exercise where id = p_exercise_id;
  if l_string is null
  then
    for j in (select name from molecule where exercise_id = p_exercise_id and in_out = 1 order by id)
    loop
      if  l_first
      then l_first := false; l_string := j.name;
      else l_string := l_string || ' + ' || j.name;
      end if;
    end loop;
    if not l_first then l_string := l_string || ' --> '; end if;
    l_first  := true;
    for j in (select name from molecule where exercise_id = p_exercise_id and in_out = 0 order by id)
    loop
      if   l_first
      then l_first := false; l_string := l_string || j.name;
      else l_string := l_string || ' + ' || j.name;
      end if;
    end loop;
  end if;
  return l_string;  

exception when others then
  util.show_error ('Error in function show_reaction.', sqlerrm);
end show_reaction;

/*************************************************************************************************************************************************/

function f_chemistry_matrix return equation_matrix_tab pipelined
is
l_counter          integer;
l_in_out           integer (1);
l_molecule_counter integer;
l_symbol           varchar2 (3);
type int_array is table of pls_integer index by pls_integer;
type string_array is table of varchar2 (50) index by pls_integer;
l_int_array        int_array;
l_molecule_id      int_array;
l_string_array     string_array;
procedure reset_int_array
is
begin
  l_int_array.delete;
  for j in 1 .. 10 
  loop
    if j <= l_counter
	then l_int_array (j) := 0;
	else l_int_array (j) := null;
	end if;
  end loop;
end reset_int_array;
procedure reset_string_array
is
begin
  l_string_array.delete;
  for j in 1 .. 10 
  loop
      l_string_array (j) := '';
  end loop;
end reset_string_array;
begin
for i in (select id from exercise)
loop
  l_counter := 0;
  reset_string_array;
  for c in (select m.id, m.name from molecule m where m.exercise_id = i.id order by m.id)
  loop
    l_counter                  := l_counter + 1;
    l_molecule_id (c.id)       := l_counter;
	l_string_array (l_counter) := c.name;
  end loop;
--
  for s in (select distinct me.atom_nr, ps.symbol from molecule m, molecule_elements me, periodic_system ps
            where m.exercise_id = i.id and m.id = me.molecule_id and me.atom_nr = ps.atom_nr)
  loop
--
    reset_int_array;
    for j in (select m.id, m.in_out, case in_out when 0 then sum (me.occurences) else - sum (me.occurences) end occurences
            from  molecule m, molecule_elements me
            where m.exercise_id = i.id and m.id = me.molecule_id and me.atom_nr = s.atom_nr
			group by m.id, m.in_out)
    loop
	  l_int_array (l_molecule_id (j.id)) := j.occurences;
    end loop;
--
  pipe row (equation_matrix_row (i.id, s.symbol, l_int_array (1), l_string_array (1), l_int_array (2), l_string_array (3), l_int_array (3), l_string_array (3),
                                l_int_array (4), l_string_array (4), l_int_array (5), l_string_array (5), l_int_array (6), l_string_array (6),
								l_int_array (7), l_string_array (7), l_int_array (8), l_string_array (8), l_int_array (9), l_string_array (9),
								l_int_array (10), l_string_array (10)));
  end loop;			
end loop;

exception when others then
    util.show_error ('Error in function f_chemistry_matrix.', sqlerrm);
end f_chemistry_matrix;

/*************************************************************************************************************************************************/

function convert_to_matrix (p_exercise_id in integer) return types_pkg.matrix_Q_ty
is
l_matrix     types_pkg.matrix_Q_ty;
l_dimension  pls_integer;
begin
  for j in (select rownum id,a,b,c,d,e,f,g,h,i,j from table (chemistry_pkg.f_chemistry_matrix) c where exercise_id = p_exercise_id)
  loop
	if j.a is not null then l_matrix (j.id) (1)  := fractions_pkg.to_fraction (j.a, 1); else goto next_loop; end if;
	if j.b is not null then l_matrix (j.id) (2)  := fractions_pkg.to_fraction (j.b, 1); else goto next_loop; end if;
	if j.c is not null then l_matrix (j.id) (3)  := fractions_pkg.to_fraction (j.c, 1); else goto next_loop; end if;
	if j.d is not null then l_matrix (j.id) (4)  := fractions_pkg.to_fraction (j.d, 1); else goto next_loop; end if;
	if j.e is not null then l_matrix (j.id) (5)  := fractions_pkg.to_fraction (j.e, 1); else goto next_loop; end if;
	if j.f is not null then l_matrix (j.id) (6)  := fractions_pkg.to_fraction (j.f, 1); else goto next_loop; end if;
	if j.g is not null then l_matrix (j.id) (7)  := fractions_pkg.to_fraction (j.g, 1); else goto next_loop; end if;
	if j.h is not null then l_matrix (j.id) (8)  := fractions_pkg.to_fraction (j.h, 1); else goto next_loop; end if;
	if j.i is not null then l_matrix (j.id) (9)  := fractions_pkg.to_fraction (j.i, 1); else goto next_loop; end if;
	if j.j is not null then l_matrix (j.id) (10) := fractions_pkg.to_fraction (j.j, 1); end if;
	<<next_loop>>
	null;
  end loop;
  return l_matrix;

exception when others then
  util.show_error ('Error in function convert_to_matrix.', sqlerrm);
end convert_to_matrix;

/*************************************************************************************************************************************************/

procedure balance_equation (p_exercise_id in integer)
is
l_string varchar2(200) := '';
l_first  boolean := true;
l_fraction   types_pkg.fraction_ty;
l_matrix     types_pkg.matrix_Q_ty := chemistry_pkg.convert_to_matrix (p_exercise_id);
l_vector     types_pkg.vector_Q_ty;
l_factor     integer;
l_index      pls_integer := 1;
begin
  l_vector := matrix_Q_pkg.scalar_times_vector (fractions_pkg.to_fraction(-1, 1), matrix_Q_pkg.column_to_vector (l_matrix, 1));
  l_matrix := matrix_Q_pkg.remove_column (l_matrix, 1);
  l_matrix := matrix_Q_pkg.invert (l_matrix);
  l_vector := matrix_Q_pkg.matrix_times_vector (l_matrix, l_vector);
  l_factor := matrix_Q_pkg.lcm_denominator (l_vector);
  l_vector := matrix_Q_pkg.scalar_times_vector (fractions_pkg.to_fraction(l_factor, 1), l_vector);
--
  for j in (select name from molecule where exercise_id = p_exercise_id and in_out = 1 order by id)
  loop
    if  l_first
    then l_first := false; l_string := to_char(l_factor) || ' ' || j.name;
    else l_string := l_string || ' + ' || to_char(l_vector (l_index).numerator) || ' ' || j.name; l_index := l_index + 1;
    end if;
  end loop;
--
  if not l_first then l_string := l_string || ' --> '; end if;
  l_first  := true;
  for j in (select name from molecule where exercise_id = p_exercise_id and in_out = 0 order by id)
  loop
    if   l_first
    then l_first := false; l_string := l_string || to_char (l_vector (l_index).numerator) || ' ' || j.name; l_index := l_index + 1;
    else l_string := l_string || ' + ' || to_char (l_vector (l_index).numerator) || ' ' || j.name; l_index := l_index + 1;
    end if;
  end loop;
--  dbms_output.put_line (substr(l_string, 1, 255));
  update exercise set reaction = l_string where id = p_exercise_id;
  commit;

exception when others then
  util.show_error ('Error in procedure balance_equation.', sqlerrm);
end balance_equation;

end chemistry_pkg;
/

exec chemistry_pkg.balance_equation(1);

create or replace trigger molecule_ari
after insert on molecule
for each row
begin
  chemistry_pkg.parse_formula (:new.id, :new.name);
end molecule_ari;
/


select ps.symbol, m.in_out,  me.molecule_id, occurences
from molecule m, molecule_elements me, periodic_system ps
where m.id = me.molecule_id
and me.atom_nr = ps.atom_nr
and exercise_id = 1
order by ps.symbol, m.in_out desc;

drop type equation_matrix_row;
drop type equation_matrix_tab;

create or replace type equation_matrix_row as object
( exercise_id   integer 
, symbol        varchar2 (3)
, a             integer (3)
, name_a        varchar2(50)
, b             integer (3)
, name_b        varchar2(50)
, c             integer (3)
, name_c        varchar2(50)
, d             integer (3)
, name_d        varchar2(50)
, e             integer (3)
, name_e        varchar2(50)
, f             integer (3)
, name_f        varchar2(50)
, g             integer (3)
, name_g        varchar2(50)
, h             integer (3)
, name_h        varchar2(50)
, i             integer (3)
, name_i        varchar2(50)
, j             integer (3)
, name_j        varchar2(50));
/
  
create or replace type equation_matrix_tab as table of equation_matrix_row;
/

select * from table (chemistry_pkg.f_chemistry_matrix) where exercise_id = 3;

