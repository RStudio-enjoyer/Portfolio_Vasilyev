--HER2.csv

drop table if exists her1;
CREATE TABLE her1 (
   Site integer,
   Subject_Screening text,
   Visit_name text,
   Form_name text,
   Table_row text,
   Variable_name text,
   Common_Variable_Name text,
   Variable_Value text,
   Decoded_Value text,
   Variable_label text,
   Field_Key text,
   Uniq_Id integer PRIMARY KEY,
   Visit_Code text,
   Form_Code text,
   Last_update_timestamp_UTC text
);

select * from her1;

--приводим в "порядок" дату
update her1
set Last_update_timestamp_UTC = to_timestamp(Last_update_timestamp_UTC, 'MM-DD-YYYY HH24:MI');
update her1
set  Last_update_timestamp_UTC =  Last_update_timestamp_UTC::TIMESTAMP

select * from her1

--пациенты с незавершеной линией
select subject_screening, variable_name, 1 as line_order from her1
where Variable_Name = 'THL1_METSTLTERMINATE' and Variable_Value = 'N'
group by subject_screening, variable_name

union

select subject_screening, variable_name, 2 as line_order from her1
where Variable_Name = 'THL2_METSTLTERMINATE' and Variable_Value = 'N'
group by subject_screening, variable_name

union

select subject_screening, variable_name, 3 as line_order from her1
where Variable_Name = 'THL3_METSTLTERMINATE' and Variable_Value = 'N'
group by subject_screening, variable_name

order by line_order, subject_screening;


--Выборка пациентов из датасета 
select subject_screening
from her1
group by subject_screening
having 
    -- Первая линия
   		sum(case
            when Variable_Name = 'THL1_METSTLTERMINATE' and Variable_Value = 'Y' then 1 
            else 0 
        end) = 1
    -- Вторая линия
    and sum(case 
                when Variable_Name = 'THL2_METSTLTERMINATE' and Variable_Value = 'N' then 1 
                else 0 
            end) = 1
	-- Третья линия
	and sum(case
                when Variable_Name = 'THL3_METSTLTERMINATE' and Variable_Value = 'Y' then 1 
                else 0 
            end) = 1




--создаем CTE-запрос для удобства 
with therapy_dates as (
select a.subject_screening, a.uniq_id, b.uniq_id,
       a.Last_update_timestamp_UTC as start_date, 
       b.Last_update_timestamp_UTC as end_date,
	   'THL1' as therapy_line
from her1 a
join her1 b on a.subject_screening = b.subject_screening
where a.variable_name = 'THL1_METSTLSTDTC'  -- Начало линии
  and b.variable_name = 'THL1_METSTLENDTC'  -- Окончание линии

union

select a.subject_screening, a.uniq_id, b.uniq_id,
       a.Last_update_timestamp_UTC as start_date, 
       b.Last_update_timestamp_UTC as end_date,
	   'THL2' as therapy_line
from her1 a
join her1 b on a.subject_screening = b.subject_screening
where a.variable_name = 'THL2_METSTLSTDTC'  -- Начало линии
  and b.variable_name = 'THL2_METSTLENDTC'  -- Окончание линии
  
  
union

select a.subject_screening, a.uniq_id, b.uniq_id,
       a.Last_update_timestamp_UTC as start_date, 
       b.Last_update_timestamp_UTC as end_date,
	   'THL3' as therapy_line
from her1 a
join her1 b on a.subject_screening = b.subject_screening
where a.variable_name = 'THL3_METSTLSTDTC'  -- Начало линии
  and b.variable_name = 'THL3_METSTLENDTC'  -- Окончание линии
  
  union

 select a.subject_screening, a.uniq_id, b.uniq_id,
       a.Last_update_timestamp_UTC as start_date, 
       b.Last_update_timestamp_UTC as end_date,
	   'THL4' as therapy_line
from her1 a
join her1 b on a.subject_screening = b.subject_screening
where a.variable_name = 'THL4_METSTLSTDTC'  -- Начало линии
  and b.variable_name = 'THL4_METSTLENDTC') -- Окончание линии
  
  --выбираем пациентов, где дата начала позже, чем дата окончания
select 
    subject_screening, 
    start_date, 
    end_date, 
    therapy_line
from therapy_dates
where start_date > end_date
order by therapy_line, subject_screening;



