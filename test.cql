import
  "./testCsv/user.csv",
  "./testCsv/country.csv"

take user
inner join country on country.1=user.3
select user.2, country.0