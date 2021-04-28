import
  "testCsv/country.csv", //(id, population),
  "testCsv/user.csv" //(id, name, country)

take user
inner join country on country.1=user.3
select user.2, country.2