import
    p './testCsv/problems/3.2/P.csv',
    q './testCsv/problems/3.2/Q.csv'
take p
inner join q on p.1=q.1
select
    p.1,
    coalesce(p.2, q.2),
    coalesce(p.3, q.3),
    coalesce(p.4, q.4)
order lexical