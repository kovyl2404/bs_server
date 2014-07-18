-ifndef(CLIENT_PROTOCOL_HRL).
-define(CLIENT_PROTOCOL_HRL, ok).

-author("Viacheslav V. Kovalev").

-define(BID_GAME_TAG, 63).
-define(CANCEL_BID_GAME_TAG, 64).

-define(PING_TAG, 111).
-define(SERVER_TAG, 222).


-define(PING_PACKET, <<?PING_TAG, 0, _:2/unsigned-integer-unit:8>>).
-define(PING_PACKET(SeqId), <<?PING_TAG, 0, SeqId:2/big-unsigned-integer-unit:8>>).
-define(
    PING_SEQ_ID(PingPacket),
    (fun() ->
        <<?PING_TAG, 0, SeqId:2/unsigned-integer-unit:8>> = PingPacket,
        SeqId
    end)()
).


-define(SERVER_PACKET(MessageLength), <<?SERVER_TAG, 0, MessageLength:2/big-unsigned-integer-unit:8>>).

-endif.
