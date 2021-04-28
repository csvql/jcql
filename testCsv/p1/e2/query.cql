import
  a './testCsv/p1/e2/A.csv',
  b './testCsv/p1/e2/B.csv'
take a
cross join b
select a.1,a.2,b.1,b.2
