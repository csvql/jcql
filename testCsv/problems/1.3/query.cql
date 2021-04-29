import
  a './testCsv/problems/1.3/A.csv',
  b './testCsv/problems/1.3/B.csv'
take a
cross join b
select a.1,a.2,b.1,b.2
order lexical