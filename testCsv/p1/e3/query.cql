import
  a './testCsv/p1/e3/A.csv',
  b './testCsv/p1/e3/B.csv'
take a
cross join b
select a.1,a.2,b.1,b.2
