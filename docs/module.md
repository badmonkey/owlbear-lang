

defmodule NAME [implements B1, B2, ...]

-version "STRING"
-on_load(func/arity).
-compile([options]).

end


defbehaviour NAME
	count( enumerable() ) -> {ok, non_neg_integer()} | {error, module()}.
    member( enumerable(), term() ) -> {ok, boolean} | {error, module()}.
    reduce( term(), acc(), reducer() ) -> result().
end


