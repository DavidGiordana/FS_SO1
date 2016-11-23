-module(filemodule).
-compile(export_all).

%********************************************************************************%
%                           Funciones de archivos
% Los archivos tienen la estructura {Name, Content, Fd, UserId, Index}
%********************************************************************************%

% @author David Giordana
% @author Gabriel Antelo
% @author Maximiliano Ibalborde

% Intenta leer un archivo de una lista
%
%* FileList: Lista de archivos
%* Fd: descriptor de archivo
%* Size: Tamaño a leer
%
%> {Tabla de archivos resultante, cadena leida}
readFile(FileList, Fd, Size) ->
    readFile(FileList, Fd, Size, [], "").
readFile([], _, _, Cache, Readed) ->
    {Cache, Readed};
readFile([H|T], Fd, Size, Cache, Readed) ->
    {Name, Content, FD, UserId, Index} = H,
    ContentSize = length(Content),
    if
        Fd == FD ->
            if
                ContentSize == 0 ->
                    Temp = lists:append([Cache, [H], T]),
                    {Temp, ""};
                Index == ContentSize ->
                    Temp = lists:append(Cache, [H], T),
                    {Temp, ""};
                true ->
                    Temp = string:sub_string(Content, Index, Index + Size),
                    NewFile = {Name, Content, FD, UserId, min(ContentSize, Index + Size)},
                    {lists:append([Cache, [NewFile], T]), Temp}
            end;
        true ->
            readFile(T, Fd, Size, lists:append(Cache, [H]), Readed)
    end.

% Intenta escribir en un archivo
%
%* FileList: Lista de archivos
%* Fd: descriptor de archivo
%* Size: Tamaño a escribir
%* Word: Texto a escribir
%
%> Tabla de archivos resultante
writeFile(FileList, Fd, Size, Word) ->
    writeFile(FileList, Fd, Size, Word, []).
writeFile([], _, _, _, Cache) ->
    Cache;
writeFile([H|T], Fd, Size, Word, Cache) ->
    {Name, Content, FD, User, Index} = H,
    NewContent = Content ++ string:sub_string(Word, 1, Size),
    if
        Fd == FD ->
            lists:append([Cache, [{Name, NewContent, FD, User, Index}], T]);
        true ->
            writeFile(T, Fd, Size, Word, lists:append(Cache, [H]))
    end.

% Intenta eliminar un archivo
%
%* FileList: Lista de archivos
%* FileName: Nombre del archivo
%
%> Tabla de archivos resultante
removeFile(FileList, FileName) ->
	removeFile(FileList, FileName, []).
removeFile([], _, Cache) ->
	Cache;
removeFile([H|T], FileName, Cache) ->
	{Name, _, _, _, _} = H,
	if
        FileName == Name ->
            lists:append(Cache, T);
		true ->
            removeFile(T, FileName, lists:append(Cache, [H]))
	end.

% Retorna la lista de nombres de archivos
%
%* FileList: Lista de archivos
%
%> Lista de nombres de archivos resultante
getFileList(FileList) ->
    getFileList(FileList, []).
getFileList([], Cache)->
    Cache;
getFileList([H|T], Cache)->
    {Name, _, _, _, _} = H,
    getFileList(T, lists:append(Cache, [Name])).

% Retorna la tabla de archivos con un nuevo archivo dentro basandose en un nombre
%
%* FileList: Lista de archivos
%* Name: Nombre del archivo
%
%> Tabla de archivos resultante
createFile(FileList, Name) ->
    lists:append(FileList, [{Name, [], 0, 0, 0}]).

% Retorna un archivo
%
%* [H|T]: Lista de archivos
%* Key: Indica el tipo de patron de busqueda (medainte nombre o mediante descriptor)
%* Value: Patron a buscar
%
%> Archivo en la lista
%> error -> El archivo no está en la lista
getFile([], _, _)->
    error;
getFile([H|T], Key, Value)->
    {Name, _, Fd, _, _} = H,
    case Key of
        name ->
            if
                Value == Name ->
                    H;
                true ->
                    getFile(T, Key, Value)
            end;
        fd ->
            if
                Value == Fd ->
                    H;
                true ->
                    getFile(T, Key, Value)
            end;
        _Else -> error
    end.

% Dado un descriptor de archivo y una lista de archivos retorna el nombre del mismo
%
%* FileList: Lista de archivos
%* Fd: Descriptor de archivos
%
%> Nombre del archivo
%> error -> No existe archivo con ese descriptor
getNameOfFd(FileList, Fd)->
    Data = getFile(FileList, fd, Fd),
    case Data of
        {Name, _, _, _, _} ->
            Name;
        _Else ->
            error
    end.

% Intenta cerrar un archivo
%
%* FileList: Lista de archivos
%* Fd: Descriptor de archivos
%
%> Tabla de archivos resultante
closeFile(FileList, Fd)->
    closeFile(FileList, Fd, []).
closeFile([], _, New)->
    New;
closeFile([H|T], Fd, New)->
    {Name, Content, FD, _, _} = H,
    case FD of
        Fd   ->
            lists:append([New, [{Name, Content, 0, 0, 0}], T]);
        _Else ->
            closeFile(T, Fd, lists:append(New, [H]))
    end.

% Cierra todos los archivos correspondientes a un usuario
%
%* FileList: Lista de archivos
%* UserId: Usuario
%* ToDelete: Archivos a eliminar
%
%> Tabla de archivos resultante
closeAllUserFile(FileList, UserId, ToDelete)->
    closeAllUserFile(FileList, UserId, ToDelete, []).
closeAllUserFile([], _, ToDelete, Cache)->
    {Cache, ToDelete};
closeAllUserFile([H|T], UserId, ToDelete, Cache)->
    {Name, Content, _, Usr, _} = H,
    if
        Usr == UserId ->
            S = lists:member(Name, ToDelete),
            if
                S ->
                    closeAllUserFile(T, UserId, lists:delete(Name, ToDelete), Cache);
                true ->
                    closeAllUserFile(T, UserId, ToDelete, lists:append(Cache, [{Name, Content, 0, 0, 0}]))
            end;
        true ->
            closeAllUserFile(T, UserId, ToDelete, lists:append(Cache, [H])) end.

% Indica si existe un archivo
%
%* Files: Lista de archivos
%* FileName: Nombre del archivo
%
%> yes -> El archivo existe | no
existFile([], _)->
    no;
existFile([{Name, _, _, _, _} | T], FileName)->
    if
        FileName == Name ->
            yes;
        true ->
            existFile(T, FileName)
    end.

% Intenta abrir un archivo
%
%* File: Archivo a abrir
%* FileList: Lista de archivos
%* UserId: Usuario
%* Fd: descriptor
%
%> Tabla de archivos resultante
openFile(File, FileList, UserId, Fd)->
    openFile(File, FileList, UserId, Fd, []).
openFile(_, [], _, _, Cache)->
	io:format("No deberia pasar~n"),
    Cache;
openFile(File, [H | T], UserId, Fd, Cache)->
    {Name, Content, FD, _, _} = H,
    if
        File == Name ->
            lists:append([Cache, [{Name, Content, Fd, UserId, 1}], T]);
        true ->
            openFile(File, T, UserId, FD, lists:append(Cache, [H]))
    end.

% Indica el estado del archivo
%
%* FileList: Lista de archivos
%* Key: Clave de búsqueda
%* Value: Valor de busqueda
%* UserId: Identificador de usuario
%
%> ok -> El archivo está abierto y puede usarse
%> noallow -> El archivo está abierto pero no está permitido trabajar con el
%> closed -> El archivo está cerrado
%> noinlist -> El archivo no está en la lista
isUsableFile(FileList, Key, Value, UserId) ->
    Data = getFile(FileList, Key, Value),
    case Data of
        {_, _, FD, Usr, _} ->
            if
                FD == 0 ->
                    closed;
                Usr =/= UserId ->
                    noallow;
                true ->
                    ok
            end;
        _Else ->
            noinlist
    end.

% Indica el estado del archivo
%
%* FileList: Lista de archivos
%* Key: Clave de búsqueda
%* Value: Valor de busqueda
%
%> ok -> El archivo está abierto
%> closed -> El archivo está cerrado
%> noinlist -> El archivo no está en la lista
isUsableFile(FileList, Key, Value) ->
    Data = getFile(FileList, Key, Value),
    case Data of
        {_, _, FD, _, _} ->
            if
                FD == 0 ->
                    closed;
                true ->
                    ok
            end;
        _Else ->
            noinlist
    end.
