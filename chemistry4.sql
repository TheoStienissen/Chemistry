/*************************************************************************************************************************************************

Name:        chemistry_pkg.sql

Created      October  2022
Last update  March    2023

Author:      Theo stienissen
E-mail:      theo.stienissen@gmail.com

Link:   @C:\Users\Theo\OneDrive\Theo\Project\Chemistry\chemistry4.sql

Reload data:
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
end molecule_briu;
/

create or replace trigger molecule_ari
after insert on molecule
for each row
begin
  chemistry_pkg.parse_formula (:new.id, :new.formula);
exception when others
 then
   chemistry_pkg.parse_formula_simple (:new.id, :new.formula);
end molecule_ari;
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

alter table reagent add constraint reagent_pk  primary key (reaction_id, molecule_id) using index;
alter table reagent add constraint reagent_fk1 foreign key (reaction_id) references reaction (id) on delete cascade;
alter table reagent add constraint reagent_fk2 foreign key (molecule_id) references molecule (id) on delete set null;

create or replace package chemistry_pkg
is
function  lcm (p_reaction_id in integer, p_atom_nr in integer) return integer;

function  molecule_weight (p_molecule_id in integer) return number deterministic;

function  get_atom_name (p_atom_nr in integer, p_lang in varchar2 default 'E') return varchar2 deterministic;

function  get_atom_weight (p_atom_nr in integer) return number deterministic;

function  show_reaction (p_reaction_id in integer) return varchar2;

function  f_chemistry_matrix return equation_matrix_tab pipelined;

function  convert_to_matrix (p_reaction_id in integer) return types_pkg.matrix_Q_ty;

function  f_get_number (p_string in varchar2)   return integer;

function  f_get_symbol (p_molecule in varchar2) return varchar2;

function  to_symbol (p_atom_nr in integer) return varchar2;

function  f_get_atom_nr (p_symbol in varchar2) return integer;

procedure parse_formula_simple (p_molecule_id in number, p_string in varchar2);

procedure partial_parse (p_molecule_id in number, p_string in varchar2);

procedure parse_formula (p_molecule_id in number, p_string in varchar2);

procedure balance_equation (p_reaction_id in integer);

end chemistry_pkg;
/

create or replace package body chemistry_pkg
is

--
-- Multplication factor to be used after Gausss - Jordan elimination
--
function lcm (p_reaction_id in integer, p_atom_nr in integer) return integer
is
l_lcm   integer;
begin
  for j in (select me.molecule_id, me.occurences from molecule m, molecule_elements me
            where m.id = me.molecule_id and me.atom_nr = p_atom_nr)
  loop
    l_lcm := maths.lcm (j.occurences, nvl (l_lcm, j.occurences));
  end loop;
  return l_lcm;

exception when others then
  util.show_error ('Error in function lcm for reaction: ' || p_reaction_id || ' and atom: ' || p_atom_nr || '.', sqlerrm);
end lcm;

/*************************************************************************************************************************************************/

--
-- Determine the molecule mass
--
function molecule_weight (p_molecule_id in integer) return number
is
l_sum_weight number;
begin
  select sum (me.occurences * get_atom_weight (me.atom_nr))
  into l_sum_weight from molecule m, molecule_elements me, periodic_system ps
  where m.id = me.molecule_id and me.molecule_id = p_molecule_id and me.atom_nr = ps.atom_nr;
  return l_sum_weight;

exception when others then
  util.show_error ('Error in function molecule_weight for molecule ID: ' || p_molecule_id || '.', sqlerrm);
end molecule_weight;

/*************************************************************************************************************************************************/

--
-- Get the atom name. Dutch is default
--
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

--
-- Get the atomic weight
--
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

--
-- Can be used after the query has been balanced
--
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

--
-- Can be used after the query has been balanced
--
function f_chemistry_matrix return equation_matrix_tab pipelined
is
l_molecule_counter integer;
l_int_array        types_pkg.pls_int_array_ty;
l_string_array     types_pkg.string_array;
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
-- Assume that the value for a = 1 and put this value in the last column
--
function convert_to_matrix (p_reaction_id in integer) return types_pkg.matrix_Q_ty
is
l_vector     types_pkg.vector_Q_ty;
l_matrix     types_pkg.matrix_Q_ty;
l_count_m    pls_integer;
l_count      pls_integer := 1;
begin
  select count(*) into l_count_m from molecule m, reagent rg where m.id = rg.molecule_id and rg.reaction_id = p_reaction_id;
  if l_count_m between 2 and 10 
  then  
    for j in (select b, c, d, e, f, g, h, i, j, -a  a from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = p_reaction_id order by symbol)
	loop
      l_matrix (l_count)(1) := fractions_pkg.to_fraction(j.b);	
	  if l_count_m = 2  then l_matrix (l_count)(2) := fractions_pkg.to_fraction(j.a); goto done; else l_matrix (l_count)(2) := fractions_pkg.to_fraction(j.c); end if;
	  if l_count_m = 3  then l_matrix (l_count)(3) := fractions_pkg.to_fraction(j.a); goto done; else l_matrix (l_count)(3) := fractions_pkg.to_fraction(j.d); end if;
	  if l_count_m = 4  then l_matrix (l_count)(4) := fractions_pkg.to_fraction(j.a); goto done; else l_matrix (l_count)(4) := fractions_pkg.to_fraction(j.e); end if;
	  if l_count_m = 5  then l_matrix (l_count)(5) := fractions_pkg.to_fraction(j.a); goto done; else l_matrix (l_count)(5) := fractions_pkg.to_fraction(j.f); end if;
	  if l_count_m = 6  then l_matrix (l_count)(6) := fractions_pkg.to_fraction(j.a); goto done; else l_matrix (l_count)(6) := fractions_pkg.to_fraction(j.g); end if;
	  if l_count_m = 7  then l_matrix (l_count)(7) := fractions_pkg.to_fraction(j.a); goto done; else l_matrix (l_count)(7) := fractions_pkg.to_fraction(j.h); end if;
	  if l_count_m = 8  then l_matrix (l_count)(8) := fractions_pkg.to_fraction(j.a); goto done; else l_matrix (l_count)(8) := fractions_pkg.to_fraction(j.i); end if;
	  if l_count_m = 9  then l_matrix (l_count)(9) := fractions_pkg.to_fraction(j.a); goto done; else l_matrix (l_count)(9) := fractions_pkg.to_fraction(j.j); end if;
	  if l_count_m = 10 then l_matrix (l_count)(10) := fractions_pkg.to_fraction(j.a); end if;	  
      <<done>>
      l_count  := l_count + 1;
    end loop;
    l_matrix := matrix_Q_pkg.gauss_jordan_elimination (l_matrix);
  end if;
  return l_matrix;

exception when others then
  util.show_error ('Error in function convert_to_matrix for reaction ID: ' || p_reaction_id, sqlerrm);
  return constants_pkg.empty_matrix;
end convert_to_matrix;

/*************************************************************************************************************************************************/

--
-- Balancing chemical equations based on Gauss - Jordan elimination
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

/******************************************************************************************************************************************/

--
-- Counts the occurence of a character in a text
--
function f_count_char (p_string in varchar2, p_char in varchar2) return integer
is 
begin 
  if p_string is null or instr (p_string, p_char) = 0 then return 0;
  else return 1 + f_count_char (substr (p_string, instr (p_string, p_char) + 1), p_char);
  end if;
  
exception when others then
  util.show_error ('Error in function f_count_char for string: ' || p_string || '. Char: ' || p_char || '.', sqlerrm);
  return null;
end f_count_char;

/******************************************************************************************************************************************/

--
-- Tries to convert the starting digits of a string to an integer
--
function f_get_number (p_string in varchar2) return integer
is 
l_int_val           number (3) := 0;
l_pos               number (3) := 2;
begin
  if p_string is null or substr (p_string, 1, 1) not between '0' and '9' then return 1; -- Default value
  else
    while substr (p_string, l_pos, 1) between '0' and '9' and l_pos <= length (p_string)
    loop 
      l_pos := l_pos + 1;
    end loop;
    return substr (p_string, 1, l_pos - 1);
  end if;

exception when others then
  util.show_error ('Error in function f_get_number for string: ' || p_string || '.', sqlerrm);
  return null;
end f_get_number;

/******************************************************************************************************************************************/

--
-- Tries to convert the starting characters of a string to a chemical symbol
--
function f_get_symbol (p_molecule in varchar2) return varchar2 
is
begin 
  if    substr (p_molecule, 1, 1)    between 'A' and 'Z'
  then  if substr (p_molecule, 2, 1) between 'a' and 'z' then return substr (p_molecule, 1, 2); else return substr (p_molecule, 1, 1); end if;
  elsif substr (p_molecule, 1, 1)    between 'a' and 'z' then return upper (substr (p_molecule, 1, 1));
  end if;
  return '?';

exception when others then
  util.show_error ('Error in function f_get_symbol for molecule: ' || p_molecule || '.', sqlerrm);
  return null;
end f_get_symbol;

/******************************************************************************************************************************************/

--
-- Translate atom into a chemical symbol
--
function  to_symbol (p_atom_nr in integer) return varchar2
is
l_symbol periodic_system.symbol%type;
begin 
  select nvl (max (symbol), '?') into l_symbol from periodic_system where atom_nr = p_atom_nr;
  return l_symbol;

exception when others then
  util.show_error ('Error in function to_symbol for atom nr: ' || p_atom_nr || '.', sqlerrm);
  return null;
end to_symbol;

/******************************************************************************************************************************************/

--
-- Translates a chemical symbol to an atom number 
--
function f_get_atom_nr (p_symbol in varchar2) return integer 
is 
l_atom_number  number (3);
begin 
  select nvl(max (atom_nr), -1) into l_atom_number from periodic_system where upper (symbol) = upper (p_symbol);
  return l_atom_number;

exception when others then
  util.show_error ('Error in function f_get_atom_nr for symbol: ' || p_symbol || '.', sqlerrm);
  return null;
end f_get_atom_nr;

/*************************************************************************************************************************************************/

--
-- Formula cannot handle brackets
--
procedure parse_formula_simple (p_molecule_id in number, p_string in varchar2)
is
l_symbol      varchar2 (3);
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
  util.show_error ('Error in procedure parse_formula_simple for molecule ID: ' || p_molecule_id || ' and string: ' || p_string || '.', sqlerrm);
end parse_formula_simple;

/******************************************************************************************************************************************/

--
-- Parses a string that does not contain any brackets. So only characters and digits
--
procedure partial_parse (p_molecule_id in number, p_string in varchar2)
is
l_occurences      number (3)   := 1;
l_string          varchar2(50) := p_string;
l_result          varchar2(50) := '';
l_symbol          varchar2 (3);
l_dummy           integer := 1;
begin 
  if p_string is null or instr (l_string, '(') != 0 or instr (l_string, ')') != 0 
  then raise_application_error (-20001, 'Error in partial_parse for string: ' || p_string || '.');
  else
    while l_string is not null and l_dummy <= 100
	loop 
	  if substr (l_string, 1, 1) between 'A' and 'Z'
	  then
        l_symbol := f_get_symbol (l_string);
		l_string := substr (l_string, length (l_symbol) + 1);
		l_occurences := f_get_number (l_string);
		if l_occurences != 1 then l_string := substr (l_string, length (l_occurences) + 1); end if;
        insert into molecule_elements_tmp (molecule_id, atom_nr, occurences) values (p_molecule_id, f_get_atom_nr (l_symbol), l_occurences);
	  end if;
	  l_dummy := l_dummy + 1;
	end loop;
  end if;

exception when others then
  util.show_error ('Error in procedure partial_parse for molecule id: ' || p_molecule_id || ' and string: ' || p_string || '.', sqlerrm);
end partial_parse;

/******************************************************************************************************************************************/

--
--  Molecule: string {blank*  | A-Z | a-z* | digit*| ( | ) | digit*}
--  Symbol {  A-Z | a-z* }
--
procedure parse_formula (p_molecule_id in number, p_string in varchar2)
is
l_string          varchar2(50) := replace (p_string, ' '); -- First step is to remove all blanks
l_open_brackets   number (3) := f_count_char (p_string, '(');
l_close_brackets  number (3) := f_count_char (p_string, '(');
l_count           number (3) := 1;
l_start           integer (3);
l_end             integer (3);
l_last            varchar2(50);
begin
  if    l_open_brackets != l_close_brackets
  then  raise_application_error (-20001, 'Bracket problem. Opening brackets: ' || l_open_brackets || '. Closing brackets: ' || l_close_brackets || '.');
  else
	for j in 1 .. l_open_brackets
	loop
	-- 1. Search last opening bracket.
	  select length (l_string) - instr (reverse (l_string), '(') into l_start from dual;
	-- 2. Search corresponding closing bracket
	  select instr (l_string, ')', l_start + 1) into l_end from dual;
	-- 3. Evaluate the string between the brackets.
	  delete from molecule_elements_tmp;
	  partial_parse (p_molecule_id, substr (l_string,l_start + 2, l_end - l_start - 2));
	-- 4. Evaluate integer after closing bracket. Default = 1
	  l_count := f_get_number (substr (l_string, l_end + 1));
	-- 5. Build the new string without the brackets.
	   if   l_count = 1
	   then l_last := substr (l_string, l_end + 1);
	   else l_last := substr (l_string, l_end + 1 + length (l_count));
	   end if;	   
	   l_string := substr (l_string, 1, l_start);
	   for j in (select to_symbol (atom_nr) symbol, sum (l_count * occurences) occurences from molecule_elements_tmp where molecule_id = p_molecule_id group by atom_nr)
	   loop 
		 l_string := l_string || j.symbol || j.occurences;
	   end loop;
	   l_string := l_string || l_last;
	end loop;
	delete molecule_elements where molecule_id = p_molecule_id;
	delete from molecule_elements_tmp;
    partial_parse (p_molecule_id, l_string);
    insert into molecule_elements (molecule_id, atom_nr, occurences) select molecule_id, atom_nr, sum (occurences) from molecule_elements_tmp group by molecule_id, atom_nr;
  end if;

exception when others then
  util.show_error ('Error in procedure parse_formula for molecule ID: ' || p_molecule_id || ' and string: ' || p_string || '.', sqlerrm); 
end parse_formula;

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
select reaction_id, symbol,a,b,c,d,e,f,g,h,i,j from table (chemistry_pkg.f_chemistry_matrix) where reaction_id = 104 order by reaction_id, symbol;
exec chemistry_pkg.balance_equation(1)

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

create or replace view v_reaction_weight
as
  select rg.reaction_id, m.formula, decode (rg.in_out, 1, 'In', 'Out') in_out, rg.factor, chemistry_pkg.molecule_weight (rg.molecule_id) molecular_mass,
  case rg.in_out when 1 then -1 else 1 end * rg.factor * chemistry_pkg.molecule_weight(rg.molecule_id) weight from molecule m, reagent rg where m.id = rg.molecule_id
  order by rg.reaction_id, rg.in_out desc, m.formula;
 
 select  molecule_id from molecule_elements me, periodic_system ps
 where ps.atom_nr = me.atom_nr and me.molecule_id = m.id 
 and ps.atom_nr = nvl (
 order by ps.atom_nr
 
 

set serveroutput on size UNLIMITED
-- string {blank* | digit* | a-z | A-Z | ( | ) }

[A-Z][a-z]*\d*
select id, formula from molecule order by id;

exec chemistry_pkg.parse_formula(41,'NaCl')
select * from molecule_elements where molecule_id = 41;
select * from molecule_elements_tmp;
rollback;

exec chemistry_pkg.parse_formula(14,'K4Fe(SCN)6')
select * from molecule_elements_tmp;
select * from molecule_elements where molecule_id = 14;
rollback;


exec chemistry_pkg.parse_formula(116,'(NH4)2Cr2O7')
select * from molecule_elements_tmp;
select * from molecule_elements where molecule_id = 116;
rollback;


 select distinct rpad(ps.symbol, 4) || ': ' || element_name_eng symbol, ps.atom_nr
  from molecule m, molecule_elements me, periodic_system ps
 where ps.atom_nr = me.atom_nr and me.molecule_id = m.id order by 1
 
 select m.formula, m.id  from reagenr r, molecule m
 where m.id = r.molecule_id
 and r.reaction_id = :P46_REACTION_ID
 order by r.in_out desc, formula
 
 
 begin 
   :P46_FACTOR :=  to_number (:P46_D1 || :P46_D2 || :P46_D3 || :P46_D4 || :P46_D5 || :P46_D6) / chemistry_pkg.molecule_weight (:P46_MOLECULES);
 exception when others then null;
 end;
 
 
 select formula, in_out, molecular_mass / :P46_FACTOR from v_reaction_weight
 reaction_id = :P46_REACTION_ID and in_out = 'In'