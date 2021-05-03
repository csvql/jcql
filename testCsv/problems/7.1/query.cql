import f './testCsv/problems/7.1/F.csv', g './testCsv/problems/7.1/G.csv'

take f
cross join g
where f.2 != "" and f.3 != "" and g.1=f.2 and g.2=f.3
select f.1, g.3