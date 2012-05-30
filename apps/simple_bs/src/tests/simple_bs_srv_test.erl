-module(simple_bs_srv_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-ifdef(TEST).
  app_start_test() ->
    ok = application:start(simple_bs),
    ?assertNot(undefined == whereis(simple_bs_sup)).
    
  simple_bs_test()->
    ?assert({ok,0} == gen_server:call(simple_bs_srv,get_balance)),
    ok = simple_bs_srv:insert(2),
    ok = simple_bs_srv:withdrawl(4),
    ?assert({ok,-2} == gen_server:call(simple_bs_srv,get_balance)),
    ok = simple_bs_srv:withdrawl(5),
    ok = simple_bs_srv:insert(8),
    ?assert({ok,1} == gen_server:call(simple_bs_srv,get_balance)),
    simple_bs_srv:get_balance().
        
  app_stop_test()->
    ok = application:stop(simple_bs).
-endif.