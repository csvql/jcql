import
  "./testCsv/user.csv",
  country "./testCsv/country2.csv"

take user
left join country on user.1=user.1
select user.1
order lexical