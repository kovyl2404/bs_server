-ifndef(CLIENT_PROTOCOL_HRL).
-define(CLIENT_PROTOCOL_HRL, ok).

-author("Viacheslav V. Kovalev").

-define(START_GAME_TAG, 63).

-define(
    START_GAME_PACKET(Data),
    <<?START_GAME_TAG,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Data
    >>
).

-define(START_GAME_PACKET(), ?START_GAME_PACKET(0)).


-define(NEW_GAME_FLAG, 0).
-define(RECONNECT_GAME_FLAG, 1).

-define(CANCEL_GAME_TAG, 129).
-define(
    CANCEL_GAME_PACKET,
    <<?CANCEL_GAME_TAG,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    >>
).

-define(SURRENDER_TAG, 123).
-define(
    SURRENDER_PACKET,
    <<
        ?SURRENDER_TAG,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    >>
).
-define(
    SURRENDER_PACKET(A, B, C),
    <<?SURRENDER_TAG,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    >>
).

-define(PING_TAG, 111).
-define(SERVER_TAG, 222).


-define(PING_PACKET, <<?PING_TAG, _:29/binary-unit:8, _:2/unsigned-integer-unit:8>>).
-define(PING_PACKET(SeqId),
    <<?PING_TAG,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        SeqId:2/big-unsigned-integer-unit:8
    >>
).
-define(
    PING_SEQ_ID(PingPacket),
    (fun() ->
        <<?PING_TAG, _:29/binary-unit:8, SeqId:2/unsigned-integer-unit:8>> = PingPacket,
        SeqId
    end)()
).


-define(SERVER_PACKET(MessageLength),
    <<?SERVER_TAG,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        MessageLength:2/big-unsigned-integer-unit:8
    >>
).

-define(
    REGISTER_TAG, 1
).

-define(
    LOGIN_TAG, 0
).

-endif.
