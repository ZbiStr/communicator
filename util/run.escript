#!/usr/bin/env escript

main([String]) ->
    try
        Lang = list_to_atom(String),
        client:start(Lang)
    catch
        _:_ ->
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("Wrong language\n"),
    halt(1).
