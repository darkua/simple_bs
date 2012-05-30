%%%-------------------------------------------------------------------
%%% File:      simple_bs_srv.erl
%%% @author    Sergio Veiga <> []
%%% @copyright 2012 Sergio Veiga
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2012-05-30 by Sergio Veiga
%%%-------------------------------------------------------------------
-module(simple_bs_srv).
-author('').

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  insert/1,
  withdrawl/1,
  get_balance/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {balance=0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @spec insert(N::number()) -> ok
%% @doc Inserts Money into the bank system!
%% @end 
%%--------------------------------------------------------------------
insert(N) when erlang:is_number(N), N>=0->
    gen_server:cast(?MODULE,{insert,N}).


%%--------------------------------------------------------------------
%% @spec withdrawl(N::number()) -> ok
%% @doc Witdrawl Money from the bank system
%% @end
%%--------------------------------------------------------------------
withdrawl(N) when erlang:is_number(N), N>=0->
    gen_server:cast(?MODULE,{withdrawl,N}).

%%--------------------------------------------------------------------
%% @spec get_balance() -> BalanceInfo::list()
%% @doc Check Bank System Balance
%% @end
%%--------------------------------------------------------------------
get_balance()->
    {ok,B} = gen_server:call(?MODULE,get_balance),
    io:format("Bank System Balance : ~p~n",[B]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(get_balance, _From, State) -> {reply, Reply, State}
%% @doc hanlde call to get balance state
%% @end 
%%--------------------------------------------------------------------
handle_call(get_balance, _From, State) ->
    Reply = {ok,State#state.balance},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast({insert,N::number()}, State) -> {noreply, State}
%% @doc handle cast to add N into state balance
%% @end 
%%--------------------------------------------------------------------
handle_cast({insert,N}, State) ->
    {noreply, State#state{balance=State#state.balance+N}};  


%%--------------------------------------------------------------------
%% @spec handle_cast({withdrawl,N::number()}, State) -> {noreply, State}
%% @doc handle cast to withdrawl N from state balance
%% @end 
%%--------------------------------------------------------------------
handle_cast({withdrawl,N}, State) ->
    {noreply, State#state{balance=State#state.balance-N}}.


%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
