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

TODO: joins, selects, case when, functions: coalesce, length, nested

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

As mentioned before, the evaluation is top to bottom in the similar fashion as a _pipe_.

- Firstly we start off with the `import` section, where all the global _variables_ (these are the only variables in our language) are defined that store the contents of the cql files. These can be accessed throughout the whole query.
- The next step is where the imported files can be accessed via `take` and joined together with the other tables. The final table is passed down the pipe for the other functions to make use of
  - Note that each `join` can have a nested `take` in it with other joins which extends the flexibility of combining tables in our language
- `where` clause (optional) takes on the joined table and an expression and filters the table out row by row, adding it to the final table if the predicate is satisfied
- `select` clause takes on the piped table from `where` (or the joined table in case `where` is skipped) and `select` statements, converts them to expressions that are to be evaluated against each row and again goes through each row one by one, outputting the resulting row
- `order` clause (optional, default is _as is_) allows the table to be sorted in a lexicographical order, in case that is required

Each pipe is a Haskell function that takes on an Environment, which varies depending on the function, and reconstructs the table from it, which is then passed on to the next pipe. In the future, this allows even more additional pipes to be defined that will tweak the final output to the more flexible format.

## Type Checking

TODO: explain strict type checking (in select, filter, function args, and function outputs), show examples of different error messages

## Tooling

### Syntax Highlighting

We wrote a syntax highlighting plugin for VsCode that provides basic highlighting of keywords, strings, as well as numbers (for `.cql` files). All the screenshots you are seeing above are using the syntax highlighting plugin.

### REPL

We also added a simple REPL tool that allows programmers to experiment with queries without having to write them into a file:

![REPL demo](./img/repl.png){width=500}
