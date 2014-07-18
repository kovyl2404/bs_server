
-module(protocol_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("client_session/include/client_protocol.hrl").

feed_command_at_once_test_() ->
    Parser = protocol_parser:init(),
    {ok, _, Messages} = protocol_parser:feed(Parser, <<1,2,3,4>>),
    [
        ?_assertEqual([{command, <<1,2,3,4>>}], Messages)
    ].

feed_redundant_bytes_command_test_() ->
    Parser = protocol_parser:init(),
    {ok, Parser1, Messages1} = protocol_parser:feed(Parser, <<1,2,3,4, 5, 6>>),
    {ok, _, Messages2} = protocol_parser:feed(Parser1, <<7, 8>>),
    [
        ?_assertEqual([{command, <<1,2,3,4>>}], Messages1),
        ?_assertEqual([{command, <<5,6,7,8>>}], Messages2)
    ].

feed_insufficient_bytes_command_test_() ->
    Parser = protocol_parser:init(),
    {ok, Parser1, Messages1} = protocol_parser:feed(Parser, <<1,2>>),
    {ok, _, Messages2} = protocol_parser:feed(Parser1, <<3, 4, 5, 6, 7, 8>>),
    [
        ?_assertEqual([], Messages1),
        ?_assertEqual([{command, <<1,2,3,4>>}, {command, <<5,6,7,8>>}], Messages2)
    ].

feed_data_at_once_test_() ->
    Parser = protocol_parser:init(),
    DataHeader = ?SERVER_PACKET(5),
    Data = <<1,2,3,4,5>>,
    {ok, _, Messages} = protocol_parser:feed(Parser, <<DataHeader/binary, Data/binary>>),
    [
        ?_assertEqual(Messages, [{data, <<1,2,3,4,5>>}])
    ].

feed_data_by_chunks_test_() ->
    DataHeader = ?SERVER_PACKET(5),
    Data = <<1,2,3,4,5>>,
    Frame = <<DataHeader/binary, Data/binary>>,
    FrameLength = byte_size(Frame),
    {_, ResultMessages} =
        lists:foldl(
            fun(I, {Parser, Acc}) ->
                Byte = binary:at(Frame, I-1),
                {ok, NewParser, Messages} = protocol_parser:feed(Parser, <<Byte>>),
                {NewParser, Acc++Messages}
            end, {protocol_parser:init(), []}, lists:seq(1, FrameLength)
        ),
    [
        ?_assertEqual([{data, <<1,2,3,4,5>>}], ResultMessages)
    ].
