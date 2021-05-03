import
  s "./testCsv/problems/10.2/S.csv",
  t "./testCsv/problems/10.2/T.csv"

take t
cross join s
where s.1=s.2 and t.2=t.3
select s.3, t.1
order lexical