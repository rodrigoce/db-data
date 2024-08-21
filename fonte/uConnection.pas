unit uConnection;

{$MODE Delphi}

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DB, sqldb, oracleconnection, uAppFile, DateUtils;

type

  { TDBConnection }

  TDBConnection = class
  private
    class var FCnn: TOracleConnection;
    class var FTransaction: TSQLTransaction;
    class var lastUsedTime: TDateTime;
    class procedure RenewCnn;
    class procedure ExecPingTest;
  public
    class function GetCnn: TOracleConnection;
    class procedure CloseCnn;
    class function GetSIDDescription(SID: string; AppFile: TAppFile): string;
  end;


implementation

uses uFuncoes, uVariaveisGlobais;

  { TDBConnection }

class procedure TDBConnection.CloseCnn;
begin
  if FCnn <> nil then
  begin
    FCnn.Close;
    FreeAndNil(FCnn);
  end;
end;

class function TDBConnection.GetSIDDescription(SID: string; AppFile: TAppFile
  ): string;
var
  slOrigTNS, slCleanTNS: TStringList;
  i, aux, linhaIni, linhaFim, countAbre, countFecha: integer;
begin
  Result := '';
  slOrigTNS := TStringList.Create;
  slCleanTNS := TStringList.Create;
  slOrigTNS.LoadFromFile(AppFile.TNSPath);

  for i := 0 to slOrigTNS.Count - 1 do
  begin
    slOrigTNS[i] := Trim(slOrigTNS[i]);
    if Length(slOrigTNS[i]) > 0 then
      if slOrigTNS[i][1] <> '#' then
        slCleanTNS.Append(StringReplace(slOrigTNS[i], ' ', '', [rfReplaceAll]));
  end;
  slOrigTNS.Free;

  linhaIni := -1;
  linhaFim := -1;
  countAbre := 0;
  countFecha := 0;
  for i := 0 to slCleanTNS.Count - 1 do
  begin
    if linhaIni = -1 then
    begin
      aux := Pos(slCleanTNS[i], SID + '=');
      if aux > 0 then
        linhaIni := i;
    end;

    if linhaIni > -1 then
    begin
      countAbre := countAbre + CountChar(slCleanTNS[i], '(');
      countFecha := countFecha + CountChar(slCleanTNS[i], ')');

      if countAbre > 0 then
        if countAbre = countFecha then
        begin
          linhaFim := i;
          break;
        end;
    end;
  end;

  for i := linhaIni to linhaFim do
  begin
    Result := Result + slCleanTNS[i];
  end;

  Result := StringReplace(Result, SID + '=', '', [rfReplaceAll]);
end;

class procedure TDBConnection.RenewCnn;
begin
  CloseCnn;

  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Active := False;
  //FTransaction.Params.Add('isc_tpb_read_committed');
  //FTransaction.Params.Add('isc_tpb_wait');

  FCnn := TOracleConnection.Create(nil);
  FCnn.LoginPrompt := False;
  FCnn.KeepConnection := True;

  FCnn.DatabaseName := GetSIDDescription(AppFile.SID, AppFile);
  FCnn.UserName := AppFile.UserName;
  FCnn.Password := AppFile.Password;
  //FCnn.CharSet := 'utf8'; isso resolve o problema mas meu client parece estar muito desatualizado.

  FCnn.Transaction := FTransaction;
  FCnn.Open();

  lastUsedTime := Now;
end;

class procedure TDBConnection.ExecPingTest;
var
  q: TSQLQuery;
begin
  if MinutesBetween(lastUsedTime, Now) > 1 then
  begin
    q := TSQLQuery.Create(nil);
    q.DataBase := FCnn;
    q.SQL.Text := 'select 1 as PingTest from dual';
    try
      q.ExecSQL;
      lastUsedTime := Now;
    except
      RenewCnn;
    end;
    q.Free;
  end;
end;

class function TDBConnection.GetCnn: TOracleConnection;
begin
  if FCnn = nil then
    RenewCnn;

  ExecPingTest;

  Result := FCnn;
end;

end.
