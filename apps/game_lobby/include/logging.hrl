-ifndef(LOGGING_HRL).
-define(LOGGING_HRL, ok).

-compile([
    {parse_transform, lager_transform}
]).

-ifdef(WITH_DEBUG_LOGS).
-define(DEBUG(Message, Args), lager:debug(Message, Args)).
-else.
-define(DEBUG(Message, Args), ok).
-endif.


-define(INFO(Message, Args), lager:info(Message, Args)).
-define(NOTICE(Message, Args), lager:notice(Message, Args)).

-endif.
