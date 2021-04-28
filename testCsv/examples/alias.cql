import
  user "testCsv/user.csv",
  user2 "testCsv/user.csv",
  country "testCsv/country.csv"

// generate matches
take user
cross join (take user2 inner join country on country.1=user2.3 select user2.1, user2.2, country.1)
where user.1 != user2.1
select user.2, user2.2, country.2