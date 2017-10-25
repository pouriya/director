-compile([{parse_transform, lager_transform}]).

-define(info(Txt, Args), lager:info(Txt, Args)).
-define(warning(Txt, Args), lager:warning(Txt, Args)).
-define(error(Txt, Args), lager:error(Txt, Args)).