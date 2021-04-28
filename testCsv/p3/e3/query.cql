import
    p './testCsv/p3/e3/P.csv',
    q './testCsv/p3/e3/Q.csv'
take p
inner join q on p.1=q.1
select p.1, coalesce(p.2, q.2), coalesce(p.3, q.3), coalesce(p.4, q.4)
    
