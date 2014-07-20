
-module(protocol_parser).
-author("Viacheslav V. Kovalev").


-include_lib("game_server/include/client_protocol.hrl").

-export([
    init/0,
    feed/2,
    parse/1
]).


-record(
    read_command, {
        buffer = <<>>       :: binary()
    }
).

-record(
    read_data, {
        buffers         = []                                    :: list(),
        bytes_needed    = erlang:error(required, bytes_needed)  :: non_neg_integer()
    }
).

init() ->
    #read_command{}.

parse(Data) ->
    {ok, _, Messages} = feed(init(), Data),
    Messages.


feed(
    #read_command{
        buffer = Buffer
    } = CurState,
    Data
) ->
    case <<Buffer/binary, Data/binary>> of
        <<Command:4/binary-unit:8, Rest/binary>> ->
            case Command of
                ?SERVER_PACKET(MessageLength) ->
                    feed(#read_data{bytes_needed = MessageLength}, Rest);
                Command ->
                    {ok, NewState, Messages} = feed(#read_command{}, Rest),
                    {ok, NewState, [{command, Command} | Messages]}
            end;
        NewBuffer ->
            {ok, CurState#read_command{buffer =  NewBuffer}, []}
    end;


feed(
    #read_data{
        buffers = Buffers,
        bytes_needed = BytesNeeded
    } = CurState,
    Data
) ->
    case Data of
        <<MessageTail:BytesNeeded/binary, Rest/binary>> ->
            CurMessage = iolist_to_binary(lists:reverse( [MessageTail | Buffers] )),
            {ok, NewState, Messages} = feed(#read_command{}, Rest),
            {ok, NewState, [{data, CurMessage} | Messages]};
        _ ->
            {ok,
                CurState#read_data{
                    bytes_needed = BytesNeeded - byte_size(Data),
                    buffers = [ Data | Buffers ]
                },
                []
            }
    end.