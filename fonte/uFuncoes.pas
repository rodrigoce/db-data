unit uFuncoes;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 05/11/2013
}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Menus, {LazUTF8,} base64;

function EnCrypt(const Value : String) : String;
function DeCrypt(const Value : String) : String;
function CountChar(const Texto: string; Caractere: Char): Integer;

implementation

function EnCrypt(const Value : String) : AnsiString;
var
  Value2: string;
  CharIndex: Integer;
  countCarac, i: Integer;
begin
  Result := '';
  Value2 := EncodeStringBase64(Value);

  for CharIndex := 1 to Length(Value2) do
    Result := Result + chr(ord(Value2[CharIndex]) + 3);

  Randomize;
  countCarac := Length(Result);
  for i := 1 to countCarac * 3 do
  begin
    Result := Result + Result[Random(countCarac) + 1];
  end;
end;

function DeCrypt(const Value : String) : AnsiString;
var
  Value2: string;
  CharIndex: Integer;
  countCarac: Integer;
begin
  Result := '';
  countCarac := Trunc(Length(Value) / 4);
  Value2 := Copy(Value, 1, countCarac);

  for CharIndex := 1 to Length(Value2) do
    Result := Result + chr(ord(Value2[CharIndex]) - 3);

  Result := DecodeStringBase64(Result);
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
