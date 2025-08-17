drop table if exists vk_posts
create table vk_posts ( 
    post_id SERIAL PRIMARY KEY, --присваиваем уникальные id посту
    post_date TIMESTAMP, -- дата и время публикации
    likes_count INTEGER   -- количество лайков на посте
);
insert into vk_posts (post_date, likes_count) values
('2021-06-13 16:28:00', 21),
('2024-06-27 12:23:00', 0),
('2024-06-08 17:53:00', 1),
('2024-05-31 20:56:00', 1),
('2024-03-01 12:07:00', 40),
('2024-01-20 23:25:00', 0),
('2024-01-18 17:48:00', 0),
('2023-11-12 10:48:00', 2),
('2023-10-11 13:56:00', 78),
('2023-07-17 23:14:00', 15),
('2023-07-14 18:20:00', 2),
('2023-06-04 23:16:00', 24),
('2023-05-31 12:39:00', 2),
('2023-04-21 20:50:00', 1),
('2023-02-17 23:14:00', 91),
('2023-01-17 01:00:00', 0),
('2022-09-18 20:16:00', 114),
('2022-09-14 07:25:00', 1),
('2022-09-11 13:53:00', 36),
('2022-08-24 01:21:00', 0),
('2022-07-19 21:20:00', 11),
('2022-07-19 16:16:00', 1),
('2022-06-30 10:23:00', 2),
('2022-04-19 23:21:00', 2),
('2022-03-29 22:56:00', 99),
('2022-03-13 13:04:00', 4),
('2022-02-19 08:43:00', 5),
('2021-11-21 21:22:00', 7),
('2021-08-13 22:05:00', 25),
('2021-07-19 09:50:00', 5),
('2020-07-20 10:59:00', 8),
('2019-09-07 11:38:00', 11),
('2019-08-27 20:31:00', 20),
('2019-07-19 13:26:00', 8),
('2018-07-19 14:08:00', 13)

select * from vk_posts

--влияние времени суток на кол-во лайков
select
    case 
        when extract(hour from post_date) between 6 and 11 then 'Утро'
        when extract(hour from post_date) between 12 and 17 then 'День'
        when extract(hour from post_date) between 18 and 22 then 'Вечер'
		else 'Ночь'
    end as time_of_day,
    avg(likes_count) as avg_likes
from vk_posts
group by time_of_day
order by avg_likes desc
--я разбил время постов на 4 временных отрезков, назвал их "утро", "день", "вечер", "ночь" и посчитал для каждого отрезка среднее кол-во лайков на постах

--влияние дня недели на кол-во лайков
select
    to_char(post_date, 'Day') as day_of_week,
    avg(likes_count) as avg_likes
from vk_posts
group by day_of_week
order by avg_likes desc
--я перевёл даты постов в дни, посчитал для каждого дня среднее кол-во лайков

--влияние промежутка между постами на кол-во лайков
select post_id, date_trunc('day', next_date) - date_trunc('day', post_date) as time_stamp, likes_count
from (select post_id, post_date, likes_count,
lead(post_date) Over(order by post_date) next_date
from vk_posts)
as vk_date
group by post_id, likes_count, post_date, next_date
Order by likes_count desc
--в этом примере кода я решил использовать date_trunc для определение дней между постами и кол-во лайков
--сортировка кол-во лайков от большего к меньшему


select post_id, next_date::date - post_date::date as time_stamp, 
likes_count from (select post_id, post_date, likes_count,
lead(post_date) Over(order by post_date) next_date
from vk_posts)
as vk_date
group by post_id, likes_count, post_date, next_date
Order by likes_count desc
--в этом примере кода я решил использовать ::date в случае дальнейших манипуляций с данными о промежутках между постами
--сортировка кол-во лайков от большего к меньшему