import
  p "./testCsv/problems/8.1/P.csv",
  q "./testCsv/problems/8.1/Q.csv"

take q
inner join p on p.4=q.4
select
  coalesce(q.1, p.1),
  coalesce(q.2, p.2),
  coalesce(q.3, p.3),
  coalesce(q.4, p.4)
order lexical
