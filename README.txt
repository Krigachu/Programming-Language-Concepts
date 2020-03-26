# JorKri -PLC lang

the bestest language ever maded!!11!1.

Input and Output conventions: 
> streams of ints inputs
> file ends with EOF command
> 32 bit integers (+ve and -ve)
> columns are all the same i.e all lines have same number of args
> number of args in each test may be large but still finite
> empty inputs are allowed
> The values in each row are separated by a single space character.
> form "a_nk" where n = no. columns, k = number of rows
> output should be a stdout in a single column
> Every line must be terminated with the Unix new line character EOL
> Streams are vertical in the doc, i.e a column = 1 stream

Language rules (Maybe look at simple languages for inspiration) :
> "¬" ending character for line (EOL)
> encapsulate looping with these brackets {}
> for (i = number to number) {}
> for (something in list) {}
> if (conditions) (number of options) {}
> [] indexing -> var.name[number] , needs to be a string or list, returns type of list / string, indexes from 0.
> (type) (var. name) -> making the variable of type T
> List (type) (var. name) -> making a variable a list of type T
> while (condition is true) {}
> (return type) (func. name) ((Type) (parameter name)) {} 

------Types------
> Bool -> True and False
> Str -> "this is a string", accepts double quotes only , needs to be able to recognise escape characters e.g \" or \n, 
> Num -> accepts both decimal point numbers and integers.

------Operations------
> concat, ++ takes two strings and concat them together , (str) ++ (str)
> + , takes two numbers (num) + (num) and add them together, returns num
> equals , (expr) == (expr), returns true when two things are the exactly the same (not the type matching but the values)
> not equals ,(expr) != (expr) , returns true when two things are the not same.
> less than, (expr) < (expr)
> greater than, (expr) > (expr)
> less than or equal to, (expr) <= (expr) 
> greater than or equal to, (expr) >= (expr)
