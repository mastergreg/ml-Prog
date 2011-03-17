fun vowfirst (a::m) = if a=#"a" orelse a=#"e" orelse a=#"i" orelse a=#"o"orelse a=#"u" orelse a=#"y" then true else false ;
vowfirst(explode("asdf"));
