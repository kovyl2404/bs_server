-ifndef(LOGGING_HRL).
-define(LOGGING_HRL, ok).

-ifdef(WITH_DEBUG_LOGS).
-define(DEBUG(Message, Args), lager:debug(Message, Args)).
-else.
-define(DEBUG(Message, Args), ok).
-endif.

-endif.