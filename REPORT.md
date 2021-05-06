---
geometry: margin=2cm
---

# JCQL - Just a CSV Query Language

## Introduction

Declarative programming language designed to work with CSV files. The syntax is designed to be SQL-like, but with some differences:

### Top to bottom scope evaluation

Unlike SQL, in JCQL you have to first import your files, then join, filter, and only then select the columns.

For example, the following SQL:

```sql
select a.1
from a
inner join b on b.1=a.1
where b.1 is not null
```

Would be written as:

![Equivalent CQL syntax](img/intro.png){width=500}

This allows for a much more linear evaluation structure that is (in our opinion) much easier to follow. Just like in most general purpose languages, an identifier only comes into scope for expression below its declaration.

## Features

### Import

To use data stored in a comma separated values (csv) file, it first must be imported using the `import` command.

```sql
import
	a 'A.csv'
	'B.csv'
```

The first imported file, A.csv, has been aliased and can now be referenced as 'a' in the rest of the program. The second imported file, B.csv, is unaliased so has to be referred as 'B' in the program. JCQL imports allow the use of both relative and absolute paths.

### Joins

JCQL allows for three types of joins.

```sql
cross join b
inner join b on a.1 = b.1
left join b on a.1 = b.1
```

- `cross join` is the cartesian product of the table defined after the take expression, with the table specified after cross join.
- `inner join` combines the table defined after the take expression, with the table specified after inner join, on the identified columns.
- `left join` returns all the data from the table defined after the take expression, with the matching data from the second table, on the identified columns.

### Select

A `select` statement allows the user to specify the variables or columns that will be outputted. This feature is the same as in SQL.

```sql
select *
select a.1,0,a.2*10
```

- - on its own is a wildcard this returns all the columns of the table.
- `a.1` is column 1 from table a, this is how columns are represented in JCQL.
- Integer, boolean and string values are able to be returned.
- It is possible to select an expression, for example `a.2*10` multiplies the value in `a.2` by 10 and returns this value.

### Case

A `case` statement works the same as in SQL, by going through the conditions and returning the value after the then if the condition is met. If none of the conditions are satisfied then the returned value is the one in the `else` clause. It is an expression that can be used in either `where` or `select` clauses.

```sql
case when a.1 == 0 then 1
	 when a.1 != 0 then 2
	 else 0
```

### Order

JCQL allows the either lexical or default order, default ordering is the order that the data is in the csv file. Lexical ordering is also known as dictionary order, and orders the values depending on ASCii codes. If no `order` clause is present, default ordering is assumed.

```sql
order lexical
```

### Functions

- Coalesce
  - The `coalesce` function returns the first non null value in a column.
- Length
  - The `length` function returns the number of characters in a string.
- Nested
  - This allows subqueries to be nested inside other queries.

## Syntactic Features

### Automatically named imports & full paths

We don't have to specify the table name when file names are valid identifiers, however when an invalid filename is used (e.g. `b$d.csv`) you will see the error: `illegal characters used in the import`

![Automatic import names](img/sugar-imports.png){width=500}

Notice that we also allow to specify the full path to the file to be flexible to different scenarios.

### Select wildcard

If you want to select all values from a table (or all tables), you can use the `*` (aka "wildcard"):

![Wildcard syntax](img/sugar-wildcard.png){width=500}

### Comments & Whitespace

JCQL allows for using whitespace and comments, so that you can use it to write simple one liners as well as complex multi-line queries:

![Comments & Whitespace demo](img/space.png){width=500}

## Evaluation

As mentioned before, the evaluation is top to bottom. As the interpreter receives the parsed `AST`, first of all it opens all the relevant files and imports the relevant files. All of this is store is a map that is used throughout most of a program

From them on, the evaluation takes the same structure as a pipe in Shell. We can `take` a particular table, join it with the other table (that can be another nested `take` with joins) which would return a final joined table. This table can then be optionally passed to the `where` clause, that takes each row and evaluates it against a given expression (boolean that is type checked) and returns a newly filtered table.

We then get to the point of selection where all the select statements are converted to expressions (each expression must result in a String type) and is evaluated against each row resulting in yet another table. Finally, we can optionally pipe it to the `order` and resort all the rows.

Each pipe is a Haskell function that takes on a table from the previous pipe and additional arguments (usually an expression) which varies depending on the function, and reconstructs the table from it, which is then passed on to the next pipe. This structure ensures the further scalability of the language, where even more additional pipes can be defined that will tweak the final output to the more desired format.

## Error Messages and Type System

JCQL is a strongly typed dynamic language perfect for scripting small queries when dealing with csv files. There are 3 main types defined: `String`, `Boolean` and `Integers`.

- `String` is the default type for the table and is assumed for all tables and is expected in `select` statement
- `Integer` is the type used generally in a bigger picture of comparison, and can take place in the `where` or `select` clause
- `Boolean` is the most used type used extensively in `inner` and `left` joins, `where` clause and `case` clause

All the errors are formatted before being output to `stderr`

### Lexer

![Example of lexical error](img/lexical-error.png){width=200}

Lexical error in JCQL

This error occurs when an unknown character such as £ is used in a program. This error informs the user where the unknown character is in the program, as seen in the error above.

### Parser

```sql
import a 'A.csv'
```

![Example program of incomplete expression error](img/invalid-expression.png){width=200}

JCQL error for incomplete expression

A program just containing an `import` statement without a `take` clause would result in a incomplete expression error as both `import` and `take` clauses are mandatory.

```sql
import a 'A.csv'
take a v
```

![Example program of incomplete expression error](img/invalid-expression.png){width=200}

JCQL error for unrecognised token

The above program would result in an unexpected token error. This error informs the user where the unexpected token is as seen in the error above.

### Interpreter

```haskell
data Result v =
  Ok v
  | Error String
  deriving (Eq, Show)
```

1. Expressions errors (inside `where`, `select` and `inner/left join`)

   The error is being _built up_ from bottom to the top. To explain this, here is an example:

   ```
   Error in expression: case statement 'case when ((test.1 = 'O')) then ('Frank') when ((test.2 = 'O')) then ('Kyle') else (3) end': expected type 'string' in expression 3 but got 'integer'
   ```

   The error takes place when type checking the `else` statement and return an error displaying the expression. As we go up to the expression evaluator, we combine the error with the overall `case` statement and then finally this is output to `stderr`. Simil

2. Import errors

   - Error relating to missing files for imports:

     ```
     Import error: No such file or directory - <filename>.csv
     ```

   - Error relating to the illegal character used at implicit naming of the file:

     ```
     illegal characters used in the import
     ```

3. Invalid table reference error

   - When addressing a non-existing table in any part of the code:

     ```
     table '<tablename>' not found
     ```

   - Invalid column error

     ```
     could not find column <given_column> in table '<tablename>' (of length <actual_length>)
     ```

4. Invalid order request

   ```
   invalid order: '<given_order>'
   ```

## Tooling

### Syntax Highlighting

We wrote a syntax highlighting plugin for VsCode that provides basic highlighting of keywords, strings, as well as numbers (for `.cql` files). All the screenshots you are seeing above are using the syntax highlighting plugin.

### REPL

We also added a simple REPL tool that allows programmers to experiment with queries without having to write them into a file:

![REPL demo](./img/repl.png){width=500}
