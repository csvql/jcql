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

As mentioned before, the evaluation is top to bottom.

### More Details

Every query starts off with an `import` statement. The imports can either be

## Type Checking

TODO: explain strict type checking (in select, filter, function args, and function outputs), show examples of different error messages

## Tooling

### Syntax Highlighting

We wrote a syntax highlighting plugin for VsCode that provides basic highlighting of keywords, strings, as well as numbers (for `.cql` files). All the screenshots you are seeing above are using the syntax highlighting plugin.

### REPL

We also added a simple REPL tool that allows programmers to experiment with queries without having to write them into a file:

![REPL demo](./img/repl.png){width=500}

