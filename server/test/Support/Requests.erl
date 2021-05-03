-module(test_support_requests@foreign).

-export([requestPage/1
       , postFormImpl/2]).

requestPage(Path) ->
  fun() ->
      {ok, ConnPid} = gun:open("localhost", 3000),
      {ok, _Protocol} = gun:await_up(ConnPid),

      StreamRef = gun:get(ConnPid, Path, []),
      handleResult(ConnPid, StreamRef)
  end.

handleResult(ConnPid, StreamRef) ->
  receive
    {gun_response, ConnPid, StreamRef, fin, 302, _Headers} ->
      gun:shutdown(ConnPid),
      {no_data, {just, ""}};
    {gun_response, ConnPid, StreamRef, fin, _Status, _Headers} ->
      gun:shutdown(ConnPid),
      {no_data, {nothing}};
    {gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
      ReceivedString = iolist_to_binary(receive_data(ConnPid, StreamRef)),
      gun:shutdown(ConnPid),
      {ok, {just, ReceivedString}};
    {'DOWN', _MRef, process, ConnPid, Reason} ->
      error_logger:error_msg("Oops!"),
      exit(Reason)
  after 1000 ->
          gun:shutdown(ConnPid),
          exit(timeout)
  end.


receive_data(ConnPid, StreamRef) ->
  receive
    {gun_data, ConnPid, StreamRef, nofin, Data} ->
      [receive_data(ConnPid, StreamRef) | Data];
    {gun_data, ConnPid, StreamRef, fin, Data} ->
      [Data];
    {'DOWN', _MRef, process, _ConnPid, Reason} ->
      error_logger:error_msg("Oops!"),
      exit(Reason)
  after 1000 ->
          exit(timeout)
  end.

postFormImpl(Path, Form) ->
  fun() ->
      {ok, ConnPid} = gun:open("localhost", 3000),
      {ok, _Protocol} = gun:await_up(ConnPid),


      StreamRef = gun:post(ConnPid, Path, [{ <<"content-type">>, <<"application/x-www-form-urlencoded">> }], Form),


      handleResult(ConnPid, StreamRef)
  end.
