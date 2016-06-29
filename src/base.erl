
-module(base).

-export_type([ natural/0, cardinal/0, ordinal/0 ]).


%%%%% ------------------------------------------------------- %%%%%


-type natural() :: non_neg_integer().
-type cardinal() :: non_neg_integer().  % quantity of things numbers ie 0, 1, ...
-type ordinal() :: pos_integer().       % order of things numbers 1st, 2nd, 3rd



