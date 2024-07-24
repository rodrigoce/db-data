unit uConnection;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 21/05/2013
}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DB, sqldb, oracleconnection;

type

  // singleton parttner

  { TConexao }

  TConexao = class
  private
    FConexao: TOracleConnection;
    FTransaction: TSQLTransaction;
  public
    class function GetConexao: TOracleConnection;
    class procedure FecharConexao;
    class function GetSIDDescription(SID: string): string;
  end;

var
  __Conexao: TConexao;

implementation

uses uFuncoes, uVariaveisGlobais;

  { TConexao }

class procedure TConexao.FecharConexao;
begin
  if __Conexao <> nil then
  begin
    __Conexao.FConexao.Close;
    FreeAndNil(__Conexao);
  end;
end;

class function TConexao.GetSIDDescription(SID: string): string;
var
  slOrigTNS, slCleanTNS: TStringList;
  i, aux, linhaIni, linhaFim, countAbre, countFecha: integer;
begin
  Result := '';
  slOrigTNS := TStringList.Create;
  slCleanTNS := TStringList.Create;
  slOrigTNS.LoadFromFile(IniFile.ReadString('conexao', 'tnsnames', ''));

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

class function TConexao.GetConexao: TOracleConnection;
var
  SID, User, Senha: string;
begin
  if __Conexao = nil then
  begin
    __Conexao := TConexao.Create;
    with __Conexao do
    begin
      FTransaction := TSQLTransaction.Create(nil);
      FTransaction.Active := False;
      //FTransaction.Params.Add('isc_tpb_read_committed');
      //FTransaction.Params.Add('isc_tpb_wait');

      FConexao := TOracleConnection.Create(nil);
      FConexao.LoginPrompt := False;
      FConexao.KeepConnection := True;

      SID := IniFile.ReadString('conexao', 'sid', '');
      User := IniFile.ReadString('conexao', 'user', '');
      Senha := EnDeCrypt(IniFile.ReadString('conexao', 'pwd', ''));

      FConexao.LoginPrompt := False;

      if IniFile.ReadInteger('conexao', 'banco', 0) = 0 then // oracle
      begin
        FConexao.DatabaseName := GetSIDDescription(SID);
        FConexao.UserName := User;
        FConexao.Password := Senha;
      end
      else
      begin
        {FConexao.ConnectionString :=
          'Provider=MSDASQL.1;Persist Security Info=False;Data Source=digs;';
        FConexao.Provider := 'MSDASQL.1';}
      end;

      FConexao.Transaction := FTransaction;
      FConexao.Open();
    end;
  end;

  Result := __Conexao.FConexao;
end;

end.
