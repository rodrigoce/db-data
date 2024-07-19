unit uLog;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 18/10/2013
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DB, sqldb;

type

  // singleton parttner
  TLog = class
    private
      FmyFile : TextFile;
    private
      class function getArquivo: TLog;
    public
      class procedure logString(texto: string);
      class procedure logInt(valor: Integer);
      class procedure fecharLog;
  end;

  var
    __Log: TLog;

implementation

{ TLog }

class procedure TLog.fecharLog;
begin
  CloseFile(getArquivo.FmyFile);
end;

class function TLog.getArquivo: TLog;
begin
  if __Log = nil then
  begin
    __Log := TLog.Create;
    AssignFile(__Log.FmyFile, 'c:\ModelView.log');
    Rewrite(__Log.FmyFile);
    __Log.logString('ModelView - Log');
    Result := __Log;
  end
  else
  begin
    Result := __Log;
  end;

end;

class procedure TLog.logInt(valor: Integer);
begin
  WriteLn(getArquivo.FmyFile, FormatDateTime('dd/mm/yyy hh:nn:ss', now), ' - ', valor);
end;

class procedure TLog.logString(texto: string);
begin
  WriteLn(getArquivo.FmyFile, FormatDateTime('dd/mm/yyy hh:nn:ss', now), ' - ', texto);
end;

end.
