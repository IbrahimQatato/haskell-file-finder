**To Run:**

`ghc --make Main.hs -o filefinder`

**Usage**

`./filefinder`

Give the program one of the following prompt or more joined by an `and` or `or` but only one type of those conjunctions should be used for the whole command

**Prompts**

-- `find files (beginning/starting/ending) with <string>`

-- `find files (named/containing) <string>`

-- `find files changed (<today/yesterday> / <this/last> (week, month, year) / on <day of week> )`

-- `find files (larger than/ smaller than) <size in kb mb gb>`

at the end of the prompt you can specify where to look for files by saying

-- `<find files prompt> in <directory>`
the `<directory>`can be either relative or full.
If no `directory` is provided the program will look in the current directory and it's subdirectories.
