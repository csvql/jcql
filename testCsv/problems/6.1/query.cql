import
    p "./testCsv/problems/6.1/P.csv",
    q "./testCsv/problems/6.1/Q.csv",
    s "./testCsv/problems/6.1/S.csv",
    r "./testCsv/problems/6.1/R.csv",
    t "./testCsv/problems/6.1/T.csv"
take p
    cross join q,
    cross join s,
    cross join r,
    cross join t
select p.*, q.*, s.*, r.*, t.*
order lexical