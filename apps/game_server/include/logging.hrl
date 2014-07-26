-ifndef(LOGGING_HRL).
-define(LOGGING_HRL, ok).

-ifdef(WITH_DEBUG_LOGS).
-define(DEBUG(Message, Args), lager:debug(Message, Args)).
-else.
-define(DEBUG(Message, Args), ok).
-endif.

-define(WARNING(Message, Args), lager:warning(Message, Args)).
-define(CRITICAL(Message, Args), lager:critical(Message, Args)).
-define(ERROR(Message, Args), lager:error(Message, Args)).
-endif.