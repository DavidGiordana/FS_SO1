-module(parser).
-compile(export_all).

%********************************************************************************%
%                                       Parser                                   %
%********************************************************************************%

% @author David Giordana
% @author Gabriel Antelo
% @author Maximiliano Ibalborde

% Parser
%
%* Str: String a parsear
parse(String)->
    Tok = tokens(),
    if
        length(String) < 3 ->
            {error, ["Parse error, Comando inválido"]};
        true ->
            {Com, Args} = splitFirst(String),
            NArgs = string:words(Args),
            case Com of
                "CON" ->
                    {con, []};
                "LSD" ->
                    {lsd, []};
                "DEL" ->
                    if
                        NArgs < 1 ->
                            {error, ["Parse error, Argumentos inválidos"]};
                        true ->
                            {L1, _} = splitFirst(Args),
                            {del, [L1]}
                    end;
                "CRE" ->
                    if
                        NArgs < 1 ->
                            {error, ["Parse error, Argumentos inválidos"]};
                        true ->
                            {L1, _} = splitFirst(Args),
                            {cre, [L1]}
                    end;
                "OPN" ->
                    if
                        NArgs < 1 ->
                            {error, ["Parse error, Argumentos inválidos"]};
                        true ->
                            {L1, _} = splitFirst(Args),
                            {opn, [L1]}
                    end;
                "WRT" ->
                    if
                        NArgs < 5 ->
                            {error, ["Parse error, Argumentos inválidos"]};
                        true ->
                            {L1, LL1} = splitFirst(Args),
                            {L2, LL2} = splitFirst(LL1),
                            {L3, LL3} = splitFirst(LL2),
                            {L4, LL4} = splitFirst(LL3),
                            {L5, _} = splitFirst(LL4),
                            if
                                (L1 == "FD") and (L3 == "SIZE") ->
                                    {wrt, [list_to_integer(L2), list_to_integer(L4), L5]};
                                true ->
                                    {error, ["Parse error, Argumentos inválidos"]}
                            end
                    end;
                "REA" ->
                    if
                        NArgs < 4 ->
                            {error, ["Parse error, Argumentos inválidos"]};
                        true ->
                            {L1, LL1} = splitFirst(Args),
                            {L2, LL2} = splitFirst(LL1),
                            {L3, LL3} = splitFirst(LL2),
                            {L4, _} = splitFirst(LL3),
                            if
                                (L1 == "FD") and (L3 == "SIZE") ->
                                    {rea, [list_to_integer(L2), list_to_integer(L4)]};
                                true ->
                                    {error, ["Parse error, Argumentos inválidos"]}
                            end
                    end;
                "CLO" ->
                    if
                        NArgs < 2 ->
                            {error, ["Parse error, Argumentos inválidos"]};
                        true ->
                            {L1, L2} = splitFirst(Args),
                            {L3, _} = splitFirst(L2),
                            if
                                L1 == "FD" ->
                                    {clo, [list_to_integer(L3)]};
                                true ->
                                    {error, ["Parse error, Argumentos inválidos"]}
                            end
                    end;
                "BYE" ->
                    {bye, []};
                _Else ->
                    {error, ["Parse error, Comando inválido"]}
            end
    end.

% Divide un String en una tupla con la primer palabra en la primer componente y el resto en la segunda
%
%* Str: Cadena a analizar
splitFirst(Str)->
    splitFirst(Str, 1).
splitFirst(Str, Idx)->
    Tok = tokens(),
    if
        length(Str) < Idx ->
            {Str, []};
        true ->
                B = lists:member(lists:nth(Idx, Str), Tok),
                if
                    B ->
                        {string:substr(Str, 1, Idx - 1), string:substr(Str, Idx + 1)};
                    true ->
                        splitFirst(Str, Idx + 1)
                end
    end.

% Elimina los caracteres inútiles de un String
% Los caracteres son resultado del envia via web
%
%* Str: Cadena a analizar
removeExtraCharacters(Str)->
    removeExtraCharacters(Str, true).
removeExtraCharacters([], _)->
    [];
removeExtraCharacters(Str, Start)->
    Tok = tokens(),
    [_|T] = Str,
    B = isSubString(Tok, getFirstCharacterString(Str)),
    if
        B ->
            removeExtraCharacters(T, Start);
        Start ->
            removeExtraCharacters(lists:reverse(Str), false);
        true ->
            lists:reverse(Str)
    end.

% Dado un String retorna una cadena con el primer caractere
%
%* Str: Cadena a analizar
getFirstCharacterString([])->
    "";
getFirstCharacterString(Str)->
    lists:sublist(Str, 1, 1).

% Determina si una cadena está contenida en otra
%
% Str: Cadena principal
% SubStr: Cadena patron
isSubString(Str, SubStr)->
    not (string:str(Str, SubStr) == 0).

% Retorna los tokens de separacion
tokens()->
    " \r\n".
