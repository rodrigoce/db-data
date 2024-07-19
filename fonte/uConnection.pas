unit uConnection;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 21/05/2013
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DB, sqldb, oracleconnection;

type

  // singleton parttner
  TConexao = class
    private
      FConexao: TOracleConnection;
    public
      class function GetConexao: TOracleConnection;
      class procedure FecharConexao;
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

class function TConexao.GetConexao: TOracleConnection;
var
  SID, User, Senha: string;
begin
  if __Conexao = nil then
  begin
    __Conexao := TConexao.Create;
    with __Conexao do
    begin
      FConexao := TOracleConnection.Create(nil);

      SID := IniFile.ReadString('conexao', 'sid', '');
      User := IniFile.ReadString('conexao', 'user', '');
      Senha := EnDeCrypt(IniFile.ReadString('conexao', 'pwd', ''));

      FConexao.LoginPrompt := False;

      if IniFile.ReadInteger('conexao', 'banco', 0) = 0 then // oracle
      begin
        {FConexao.ConnectionString :=
          'Provider=OraOLEDB.Oracle.1;Password=' + Senha + ';Persist Security Info=True;' +
          'User ID=' + User + ';Data Source=' + SID;
        FConexao.Provider := 'OraOLEDB.Oracle.1';}
      end
      else
      begin
        {FConexao.ConnectionString :=
          'Provider=MSDASQL.1;Persist Security Info=False;Data Source=digs;';
        FConexao.Provider := 'MSDASQL.1';}
      end;

      FConexao.Open();
    end;
  end;

  Result := __Conexao.FConexao;
end;

end.
