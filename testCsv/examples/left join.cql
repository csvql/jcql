import
  "./testCsv/user.csv",
  "./testCsv/country.csv"

take user
left join country on country.1=user.3
select user.1, country.2
order lexical