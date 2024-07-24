unit uFuncoes;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 05/11/2013
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, StdCtrls, Types;

function EnDeCrypt(const Value : String) : String;
function CountChar(const Texto: string; Caractere: Char): Integer;

implementation

function EnDeCrypt(const Value : String) : String;
var
  CharIndex : integer;
begin
  Result := Value;
  for CharIndex := 1 to Length(Value) do
    Result[CharIndex] := chr(not(ord(Value[CharIndex])));
end;

function CountChar(const Texto: string; Caractere: Char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Texto) do
  begin
    if Texto[i] = Caractere then
      Inc(Result);
  end;
end;

end.
