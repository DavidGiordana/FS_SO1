-module(filesystem).
-compile(export_all).

% @author David Giordana
% @author Gabriel Antelo
% @author Maximiliano Ibalborde

%El archivo utuliza los elementos externos:
% lists:
%    nth/2
%    sublist/2
%    map/2
%    seq/2
%    append/2
%    append/1
%    member/2
%    filter/2
%    max/1
% string:
%    substr/3
%    substr/2
%    words/2
%    join/2
% random:
%    seed/1
%    uniform/1
% parser:
%    parse/1
% filemodule:
%    readFile/3
%    writeFile/4

% START HERE
inicio() ->
    lists:map(fun(N) -> spawn(?MODULE, worker, [N]) end, lists:seq(1,5)),
    server().

% Inicia el server
server() ->
    D = gen_tcp:listen(8000, [{active, true}]),
    case D of
        {ok, ListenSocket}->
            clearScreen(),
            dispatcher(ListenSocket, 1);
        _Else ->
            io:format("El puerto está ocupado, aguarde unos momentos y vuelva a intentarlo~n")
    end.

% Recibe los clientes
%
%* ListenSocket: Socket del server
%* N: Numero del proximo cliente a crear
dispatcher(ListenSocket, N) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, dispatcher, [ListenSocket, N+1]),
    io:format("Ingresa el cliente ~p~n", [N]),
    cliente(Socket, N).

% Cliente que intenta recibir el CON
%
%* Socket: Socket para enviar y recibir mensajes
%* N: Numero del cliente
cliente(Socket, N) ->
    receive
        {tcp, Socket, Msg} ->
            Str = parser:removeExtraCharacters(Msg),
            {Cmd, _} = parser:parse(Str),
            case Cmd of
                con ->
                    gen_tcp:send(Socket, "OK ID " ++ integer_to_list(N) ++ "\n"),
					Worker = getRandomWorker(),
					register(getUserName(N), self()),
					io:format("Cliente ~p conectado, Worker ~p asignado~n",[N, Worker]),
                    cliente(Socket, N, Worker);
                _Else -> gen_tcp:send(Socket, "Error: Cliente no conectado\n")
            end
    end,
    cliente(Socket, N).

% Recibe las solicitudes de los clientes y las responde
%
%* Socket: Socket para enviar y recibir mensajes
%* ClientNumber: Numero del cliente
%* Worker: identificador del worker asignado
cliente(Socket, ClientNumber, Worker) ->
	receive
        {tcp, Socket, Msg} ->
            Str = parser:removeExtraCharacters(Msg),
            io:format("El cliente ~p envio el mensaje ~p~n", [ClientNumber, Str]),
            {Com, Args} = parser:parse(Str),
            if
                Com == error ->
                    [Temp] = Args,
                    gen_tcp:send(Socket, "ERROR " ++ Temp ++ "\n"),
					cliente(Socket, ClientNumber, Worker);
                true ->
                    Worker ! {clientmsg, Com, Args, ClientNumber},
                    receive
                        {error, Msg2} ->
                            io:format("ERROR ~p~n", [Msg2]),
                            gen_tcp:send(Socket, "ERROR " ++ Msg2 ++ "\n"),
                            cliente(Socket, ClientNumber, Worker);
                        {ok, Msg2} ->
                            io:format("OK ~p~n", [Msg2]),
                            gen_tcp:send(Socket, "OK " ++ Msg2 ++ "\n"),
                            cliente(Socket, ClientNumber, Worker);
                        {bye} -> gen_tcp:send(Socket, "OK\n"),
                        		 gen_tcp:close(Socket)
                    end
            end
    end.

% Worker (worker esclavo colector de diamantes)
%
%* PidList: Lista de Pids de los demas workers
%* Files: Lista de archivos
%* Cache: Cache para procesamiento interno
%* RequestList: Lista de solicitudes de clientes
%* LastFD: Ultimo descriptor de archivos utilizado
%* MsgCounter: Contador de mensajes
%* Safe: Indica si se envio el mensaje a otro worker, se la utiliza par aevitar envias
% de manera erronea (errores de conteo)
%* ToDelete: Lista de archivos a eliminar
worker(L)->
    register(list_to_atom([$a | integer_to_list(L)]), self()),
    PidList = lists:filter(fun(X) -> getWorkerName(L) =/= X end,[a1,a2,a3,a4,a5]),
    worker(PidList, [], [], [], 0, 0, no, []).
worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, Safe, ToDelete) ->
    %Bloque de procesamiento
    if
        length(RequestList) == 0 -> ok;
        true ->
            [{Command, Arguments, ClientId} | Tail] = RequestList,
            case Command of
                lsd ->
                    if
                        MsgCounter == 0 ->
                            worker(PidList, Files, Files, RequestList, LastFD, 1, no, ToDelete);
                        MsgCounter == 5 ->
                            getUserName(ClientId) ! {ok, listToString(filemodule:getFileList(Cache))},
                        	worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                        Safe == no ->
                            lists:nth(MsgCounter, PidList) ! {giveListReq, self()},
                            worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, yes, ToDelete);
                        true -> ok
                    end;

                del ->
                    [FileName] = Arguments,
            	       if
                           MsgCounter == 0 ->
                                FileState = filemodule:isUsableFile(Files, name, FileName, ClientId),
	                             case FileState of
                                     closed ->
                                        getUserName(ClientId) ! {ok, ""},
			                            worker(PidList, filemodule:removeFile(Files, FileName), Cache, Tail, LastFD, 0, no, ToDelete);
                                    noinlist ->
                                        worker(PidList, Files, Cache, RequestList, LastFD, 1, no, ToDelete);
                                    _Else ->
                                        getUserName(ClientId) ! {error, "El archivo esta abierto"},
                                        worker(PidList, Files, Cache, Tail, LastFD, MsgCounter, no, ToDelete)
                                 end;
    		               MsgCounter == 5 ->
                                case Cache of
    							    [ok] ->
                                        getUserName(ClientId) ! {ok, ""},
					    				worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
    								[opened] ->
                                        getUserName(ClientId) ! {error, "El archivo esta abierto"},
    						    		worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
    						    	_Else ->
                                        getUserName(ClientId) ! {error, "El archivo no existe"},
						    			worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete)
    						   	end;
    		              Safe == no ->
                              lists:nth(MsgCounter, PidList) ! {deleteFileReq, FileName, self()},
                              worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, yes, ToDelete);
                          true -> ok
                	end;

                rm ->
                    [FileName] = Arguments,
            	       if
                          MsgCounter == 0 ->
                                FileState = filemodule:isUsableFile(Files, name, FileName),
	                            case FileState of
                                    closed ->
                                        getUserName(ClientId) ! {ok, ""},
			                            worker(PidList, filemodule:removeFile(Files, FileName), Cache, Tail, LastFD, 0, no, ToDelete);
                                    noinlist ->
                                        worker(PidList, Files, Cache, RequestList, LastFD, 1, no, ToDelete);
                                    _Else ->
                                        getUserName(ClientId) ! {ok, ""},
                                        worker(PidList, Files, Cache, Tail, LastFD, MsgCounter, no, addUnique(FileName, ToDelete))
                                 end;
    		              MsgCounter == 5 ->
                                case Cache of
    							    [ok] ->
                                        getUserName(ClientId) ! {ok, ""},
					    				worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
    						    	_Else ->
                                        getUserName(ClientId) ! {error, "El archivo no existe"},
						    			worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete)
    						   	end;
                          Safe == no ->
                              lists:nth(MsgCounter, PidList) ! {rmFileReq, FileName, self()},
                              worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, yes, ToDelete);
                          true -> ok
                	end;

                cre ->
                    [FileName] = Arguments,
                    if
                        MsgCounter == 0 ->
                            T1 = filemodule:existFile(Files, FileName),
                                case T1 of
                                    yes ->
                                        getUserName(ClientId) ! {error, "El archivo ya existe"},
                                        worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                    _Else ->
                                        worker(PidList, Files, [], RequestList, LastFD, 1, no, ToDelete)
                                end;
                        MsgCounter == 5 ->
                            T1 = filemodule:existFile(Cache, FileName),
                            case T1 of
                                yes ->
                                    getUserName(ClientId) ! {error, "El archivo ya existe"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else ->
                                    getUserName(ClientId) ! {ok, ""},
                            	    worker(PidList, filemodule:createFile(Files, FileName), [], Tail, LastFD, 0, no, ToDelete)
                            end;
                            Safe == no ->
                                lists:nth(MsgCounter, PidList) ! {giveListReq, self()},
                                worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, yes, ToDelete);
                            true -> ok
                        end;

                opn ->
                    [FileName] = Arguments,
            		if
                        MsgCounter == 0 ->
                            FileState = filemodule:isUsableFile(Files, name, FileName, ClientId),
                			case FileState of
                                closed ->
                                    pidCast(PidList, {incFD}),
                                    getUserName(ClientId) ! {ok, "FD " ++ integer_to_list(LastFD + 1)},
                                    worker(PidList, filemodule:openFile(FileName, Files, ClientId, LastFD + 1), Cache, Tail, LastFD + 1, 0, no, ToDelete);
                                Temp when (Temp == noallow) or (Temp == ok) ->
                                    getUserName(ClientId) ! {error, "El archivo ya esta abierto"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else ->
                                    worker(PidList, Files, Cache, RequestList, LastFD, 1, no, ToDelete)
                            end;
                        MsgCounter == 5 ->
                            case Cache of
                                [ok, NewFD] ->
                                    getUserName(ClientId) ! {ok, "FD " ++ integer_to_list(NewFD)},
                            		worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                [opened, _] ->
                                    getUserName(ClientId) ! {error, "El archivo esta abierto"},
                        			worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else ->
                                    getUserName(ClientId) ! {error, "El archivo no existe"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete)
                            end;
                        Safe == no ->
                            lists:nth(MsgCounter, PidList) ! {openFileReq, FileName, self(), ClientId},
                            worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, yes, ToDelete);
                        true -> ok
                    end;

                wrt ->
                    [Fd, Size, String] = Arguments,
                    if
                        MsgCounter == 0 ->
                            FileState = filemodule:isUsableFile(Files, fd, Fd, ClientId),
                            case FileState of
                                closed ->
                                    getUserName(ClientId) ! {error, "El archivo esta cerrado"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                ok ->
                                    getUserName(ClientId) ! {ok, ""},
                                    worker(PidList, filemodule:writeFile(Files, Fd, Size, String), [], Tail, LastFD, 0, no, ToDelete);
                                noallow ->
                                    getUserName(ClientId) ! {error, "El archivo está en uso"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else ->
                                    worker(PidList, Files, Cache, RequestList, LastFD, 1, no, ToDelete)
                            end;
                        MsgCounter == 5 ->
                            case Cache of
                                [closed] ->
                                    getUserName(ClientId) ! {error, "El archivo esta cerrado"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                [denied] ->
                                    getUserName(ClientId) ! {error, "acceso denegado"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                [ok] ->
                                    getUserName(ClientId) ! {ok, ""},
                                	worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else ->
                                    getUserName(ClientId) ! {error, "No hay archivo"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete)
                            end;
                        Safe == no ->
                            lists:nth(MsgCounter, PidList) ! {writeFileReq, Fd, self(), ClientId, String, Size},
                            worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, yes, ToDelete);
                        true -> ok
                    end;

                rea ->
                    [Fd, Size] = Arguments,
                    if
                        MsgCounter == 0 ->
                            FileState = filemodule:isUsableFile(Files, fd, Fd, ClientId),
                            case FileState of
                                closed ->
                                    getUserName(ClientId) ! {error, "El archivo esta cerrado"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                ok ->
                                    {List, Readed} = filemodule:readFile(Files, Fd, Size),
                                    getUserName(ClientId) ! {ok, "SIZE " ++ integer_to_list(length(Readed)) ++ " " ++ Readed},
        		                    worker(PidList, List, [], Tail, LastFD, 0, no, ToDelete);
                                noallow ->
                                    getUserName(ClientId) ! {error, "El archivo está en uso"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else -> worker(PidList, Files, Cache, RequestList, LastFD, 1, no, ToDelete)
                            end;
                        MsgCounter == 5 ->
                            case Cache of
                                [closed] ->
                                    getUserName(ClientId) ! {error, "El archivo esta cerrado"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                [ok, Text] ->
                                    getUserName(ClientId) ! {ok, "SIZE " ++ integer_to_list(length(Text)) ++ " " ++ Text},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                [denied] ->
                                    getUserName(ClientId) ! {error, "El archivo está en uso"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else ->
                                    getUserName(ClientId) ! {error, "No existe el archivo"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete)
                            end;
                        Safe == no ->
                            lists:nth(MsgCounter, PidList) ! {readFileReq, self(), Fd, ClientId, Size},
                            worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, yes, ToDelete);
                        true -> ok
                    end;

                clo ->
                    [Fd] = Arguments,
                    if
                        MsgCounter == 0 ->
                            FileState = filemodule:isUsableFile(Files, fd, Fd, ClientId),
                            case FileState of
                                closed ->
                                    getUserName(ClientId) ! {error, "El archivo esta cerrado"},
                                	worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                ok ->
                                    getUserName(ClientId) ! {ok, ""},
                                    FileName = filemodule:getNameOfFd(Files, Fd),
                                    T = lists:member(FileName, ToDelete),
                                    if
                                        T ->
                                            worker(PidList, filemodule:removeFile(Files, FileName), [], Tail, LastFD, 0, no, lists:delete(FileName, ToDelete));
                                        true ->
                                            worker(PidList, filemodule:closeFile(Files, Fd), [], Tail, LastFD, 0, no, ToDelete)
                                    end;

                                noallow ->
                                    getUserName(ClientId) ! {error, "No tienes permiso sobre este archivo"},
                                	worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else -> worker(PidList, Files, [], RequestList, LastFD, 1, no, ToDelete)
                            end;
                        MsgCounter == 5 ->
                            case Cache of
                                [closed] ->
                                    getUserName(ClientId) ! {error, "El archivo esta cerrado"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                [ok] ->
                                    getUserName(ClientId) ! {ok, ""},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                [denied] ->
                                    getUserName(ClientId) ! {error, "No tienes permiso sobre este archivo"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete);
                                _Else ->
                                    getUserName(ClientId) ! {error, "El descriptor no corresponde a un archivo abierto"},
                                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDelete)
                            end;
                        Safe == no ->
                            lists:nth(MsgCounter, PidList) ! {closeFileReq, self(), Fd, ClientId},
                            worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, yes, ToDelete);
                        true -> ok
                    end;

                bye ->
                    justKill(PidList, ClientId),
                    getUserName(ClientId) ! {bye},
                    {Files, ToDel} = filemodule:closeAllUserFile(Files, ClientId, ToDelete),
                    worker(PidList, Files, [], Tail, LastFD, 0, no, ToDel);

                _Else ->
                    [Error] = Arguments,
                    getUserName(ClientId) ! {error, Error}
        end
    end,
    %Bloque de recepcion
    receive
        {clientmsg, Com, Args, ClientId1} ->
            T2 = lists:append(RequestList, [{Com, Args, ClientId1}]),
            worker(PidList, Files, Cache, T2, LastFD, MsgCounter, Safe, ToDelete);

        {giveListReq, WorkerPid} ->
            WorkerPid ! {giveListRes, Files};

        {giveListRes, FileReceived} ->
            worker(PidList, Files, lists:append(Cache, FileReceived), RequestList, LastFD, MsgCounter + 1, no, ToDelete);

        {incFD} ->
             worker(PidList, Files, Cache, RequestList, LastFD + 1, MsgCounter, Safe, ToDelete);

        {deleteFileReq, FileName1, WorkerPid} ->
            FileState1 = filemodule:isUsableFile(Files, name, FileName1),
            case FileState1 of
	               closed ->
                        WorkerPid ! {deleteFileRes, ok},
   						worker(PidList, filemodule:removeFile(Files, FileName1), Cache, RequestList, LastFD, MsgCounter, Safe, ToDelete);
			   		ok ->
                        WorkerPid ! {deleteFileRes, opened};
	              _Else1 -> WorkerPid ! {deleteFileRes, error}
		   	end;

        {deleteFileRes, BooleanState} ->
			case BooleanState of
				ok ->
                    worker(PidList, Files, [ok], RequestList, LastFD, 5, no, ToDelete);
				opened ->
                    worker(PidList, Files, [opened], RequestList, LastFD, 5, no, ToDelete);
				_Else1 ->
                    worker(PidList, Files, [error], RequestList, LastFD, MsgCounter + 1, no, ToDelete)
            end;

        {rmFileReq, FileName1, WorkerPid} ->
              FileState1 = filemodule:isUsableFile(Files, name, FileName1),
              case FileState1 of
                   closed ->
                        WorkerPid ! {rmFileRes, ok},
                        T2 = lists:member(FileName1, ToDelete),
                        if
                            T2 ->
                                worker(PidList, filemodule:removeFile(Files, FileName1), Cache, RequestList, LastFD, MsgCounter, Safe, lists:delete(FileName1, ToDelete));
                            true ->
                                worker(PidList, filemodule:removeFile(Files, FileName1), Cache, RequestList, LastFD, MsgCounter, Safe, ToDelete)
                        end;
                    ok ->
                        WorkerPid ! {rmFileRes, ok},
                        worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, Safe, addUnique(FileName1, ToDelete));
                  _Else1 ->
                        WorkerPid ! {rmFileRes, error}
            end;

        {rmFileRes, BooleanState} ->
            case BooleanState of
                ok -> worker(PidList, Files, [ok], RequestList, LastFD, 5, no, ToDelete);
                _Else1 -> worker(PidList, Files, [error], RequestList, LastFD, MsgCounter + 1, no, ToDelete)
            end;

        {openFileReq, FileName1, WorkerPid, ClientId1} ->
            FileState1 = filemodule:isUsableFile(Files, name, FileName1, ClientId1),
            case FileState1 of
                closed ->
                    WorkerPid ! {openFileRes, yes, LastFD + 1},
                    pidCast(PidList, incFD),
                    worker(PidList, filemodule:openFile(FileName1, Files, ClientId1, LastFD + 1), Cache, RequestList, LastFD + 1, MsgCounter, Safe, ToDelete);
                Temp1 when (Temp1 == ok) or (Temp1 == noallow) ->
                    WorkerPid ! {openFileRes, already, error};
                _Else1 ->
                    WorkerPid ! {openFileRes, error, error}
            end;

        {openFileRes, Data, Fd1} ->
            case Data of
                yes ->
                    worker(PidList, Files, [ok, Fd1], RequestList, LastFD, 5, no, ToDelete);
                already ->
                    worker(PidList, Files, [opened, error], RequestList, LastFD, 5, no, ToDelete);
                _Else1 ->
                    worker(PidList, Files, [error, error], RequestList, LastFD, MsgCounter + 1, no, ToDelete)
            end;

        {writeFileReq, Fd1, WorkerPid, ClientId1, String1, Size1} ->
            FileState1 = filemodule:isUsableFile(Files, fd, Fd1, ClientId1),
           case FileState1 of
               closed ->
                    WorkerPid ! {writeFileRes, closed};
               ok ->
                    WorkerPid ! {writeFileRes, ok},
                    worker(PidList, filemodule:writeFile(Files, Fd1, Size1, String1), Cache, RequestList, LastFD, MsgCounter, Safe, ToDelete);
                noallow ->
                    WorkerPid ! {writeFileRes, denied};
                _Else1 ->
                    WorkerPid ! {writeFileRes, error}
            end;

        {writeFileRes, Data} ->
            case Data of
                closed -> worker(PidList, Files, [closed], RequestList, LastFD, 5, no, ToDelete);
                ok -> worker(PidList, Files, [ok], RequestList, LastFD, 5, no, ToDelete);
                denied -> worker(PidList, Files, [denied], RequestList, LastFD, 5, no, ToDelete);
                _Else1 -> worker(PidList, Files, [error], RequestList, LastFD, MsgCounter + 1, no, ToDelete)
            end;

        {readFileReq, WorkerPid, Fd1, ClientId1, Size1} ->
            FileState1 = filemodule:isUsableFile(Files, fd, Fd1, ClientId1),
            case FileState1 of
                closed ->
                    WorkerPid ! {readFileRes, closed, error};
                ok ->
                    {List2, NewList1} = filemodule:readFile(Files, Fd1, Size1),
 					WorkerPid ! {readFileRes, ok, NewList1},
                    worker(PidList, List2, Cache, RequestList, LastFD, MsgCounter, Safe, ToDelete);
                noallow ->
                    WorkerPid ! {readFileRes, denied, error};
                _Else1 ->
                    WorkerPid ! {readFileRes, error, error}
            end;

        {readFileRes, Data, NewList1} ->
            case Data of
                closed -> worker(PidList, Files, [closed], RequestList, LastFD, 5, no, ToDelete);
                ok -> worker(PidList, Files, [ok, NewList1], RequestList, LastFD, 5, no, ToDelete);
                denied -> worker(PidList, Files, [denied], RequestList, LastFD, 5, no, ToDelete);
                _Else1 -> worker(PidList, Files, [error], RequestList, LastFD, MsgCounter + 1, no, ToDelete)
            end;

        {closeFileReq, WorkerPid, Fd1, ClientId1} ->
            FileState1 = filemodule:isUsableFile(Files, fd, Fd1, ClientId1),
                case FileState1 of
                    closed ->
                        WorkerPid ! {closeFileRes, closed};
                    ok ->
                        WorkerPid ! {closeFileRes, ok},
                        FileName1 = filemodule:getNameOfFd(Files, Fd1),
                        T2 = lists:member(FileName1, ToDelete),
                        if
                            T2 ->
                                worker(PidList, filemodule:removeFile(Files, FileName1), [], RequestList, LastFD, MsgCounter, Safe, lists:delete(FileName1, ToDelete));
                            true ->
                                worker(PidList, filemodule:closeFile(Files, Fd1), [], RequestList, LastFD, MsgCounter, Safe, ToDelete)
                        end;
                    noallow ->
                        WorkerPid ! {closeFileRes, denied};
                    _Else1 -> WorkerPid ! {closeFileRes, error}
                end;

          {closeFileRes, Argument} ->
              case Argument of
                 closed ->
                    worker(PidList, Files, [closed], RequestList, LastFD, 5, no, ToDelete);
                 ok ->
                    worker(PidList, Files, [ok], RequestList, LastFD, 5, no, ToDelete);
                 denied ->
                    worker(PidList, Files, [denied], RequestList, LastFD, 5, no, ToDelete);
                 _Else1 ->
                    worker(PidList, Files, [error], RequestList, LastFD, MsgCounter + 1, no, ToDelete)
             end;

        {bye, ClientId1} ->
            {Files1, ToDel1} = filemodule:closeAllUserFile(Files, ClientId1, ToDelete),
            worker(PidList, Files1, Cache, RequestList, LastFD, MsgCounter, Safe, ToDel1)
    end,
    worker(PidList, Files, Cache, RequestList, LastFD, MsgCounter, Safe, ToDelete).

%********************************************************************************%
%                           Funciones auxiliares
%********************************************************************************%

%Retorna el identificador de un worker al azar
getRandomWorker() ->
	random:seed(erlang:now()),
    Y = random:uniform(5),
    getWorkerName(Y).

%Dado un entero retorna el identificador de worker
%
%* X: Numero de worker
getWorkerName(X) ->
    list_to_atom([$a | integer_to_list(X)]).

% Retorna el identificador de un usuario dado su numero
%
%* N: Numero de usuario
getUserName(N)->
    H = "usr" ++ integer_to_list(N),
    list_to_atom(H).

% Dada una lista de Strings concatena los String separados por un espacio
%
%* X: Lista de Strings
listToString([])->
    "";
listToString(X)->
    string:join(X, " ").

% Limpia la pantalla
clearScreen()->
    io:format("\e[H\e[J").

% Broadcast a una lista de pids
%
%* [H|T]: Lista de Pids
%* Args: Mensaje a enviar
pidCast([], _Args) ->
	ok;
pidCast([H|T], Args) ->
	H ! Args,
	pidCast(T, Args).

% Envia la order a los demas workers para que cierren los archivos openeds por un usuario
%
%* PidList: Lista de PIDs
%* UserId: Identificador de usuario
justKill(PidList, UserId)->
	pidCast(PidList, {bye, UserId}).


% Agrega un elemento a la lista solo si este no está presente
%
%* Item: Elemento a agregar a la lista
%* List: Lista base
addUnique(Item, List)->
    T = lists:member(Item, List),
    if
        T ->
            List;
        true ->
            lists:append(List, [Item])
    end.
