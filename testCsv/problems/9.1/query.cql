import
  r "./testCsv/problems/9.1/R.csv",
  r2 "./testCsv/problems/9.1/R.csv",
  r3 "./testCsv/problems/9.1/R.csv",
  r4 "./testCsv/problems/9.1/R.csv"

take r
inner join (
  take r2 inner join (
    take r3
    inner join r4 on r3.2=r4.1
  ) on r3.1=r2.2
) on r.2=r2.1
select r.1, r3.2
order lexical
