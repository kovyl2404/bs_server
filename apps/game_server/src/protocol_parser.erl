
-module(protocol_parser).
-author("Viacheslav V. Kovalev").


-include_lib("game_server/include/client_protocol.hrl").

-define(DATA_SIZE_LIMIT, 1024).

-export([
    init/0,
    init/1,
    feed/2,
    parse/1
]).


-record(
    read_command, {
        buffer = <<>>       :: binary(),
        command_length      :: error:required()
    }
).

-record(
    read_data, {
        buffers         = []                                    :: list(),
        bytes_needed    = erlang:error(required, bytes_needed)  :: non_neg_integer(),
        next_command_length = erlang:error(required)
    }
).

init() ->
    init(32).

init(CommandLength) ->
    #read_command{
        command_length = CommandLength
    }.

parse(Data) ->
    {ok, _, Messages} = feed(init(), Data),
    Messages.


feed(
    #read_command{
        buffer = Buffer,
        command_length = CommandLength
    } = CurState,
    Data
) ->
    case <<Buffer/binary, Data/binary>> of
        <<Command:CommandLength/binary-unit:8, Rest/binary>> ->
            case Command of
                ?SERVER_PACKET(MessageLength) ->
                    case MessageLength >= ?DATA_SIZE_LIMIT of
                        true ->
                            {error, {message_too_long, MessageLength}};
                        false ->
                            feed(#read_data{bytes_needed = MessageLength, next_command_length = CommandLength}, Rest)
                    end;
                Command ->
                    {ok, NewState, Messages} = feed(#read_command{command_length = CommandLength}, Rest),
                    {ok, NewState, [{command, Command} | Messages]}
            end;
        NewBuffer ->
            {ok, CurState#read_command{buffer =  NewBuffer}, []}
    end;


feed(
    #read_data{
        buffers = Buffers,
        bytes_needed = BytesNeeded,
        next_command_length = CommandLength
    } = CurState,
    Data
) ->
    case Data of
        <<MessageTail:BytesNeeded/binary, Rest/binary>> ->
            CurMessage = iolist_to_binary(lists:reverse( [MessageTail | Buffers] )),
            {ok, NewState, Messages} = feed(#read_command{command_length = CommandLength}, Rest),
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