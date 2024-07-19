unit uConfigConexao;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 01/10/2013
}
interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, ExtCtrls;

type
  TFormConfigConexao = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edUser: TEdit;
    edSenha: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edSID: TEdit;
    btOK: TButton;
    rgBanco: TRadioGroup;
    lbl1: TLabel;
    procedure btOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure ConfigConexao;
  end;

var
  FormConfigConexao: TFormConfigConexao;

implementation

uses uVariaveisGlobais, uConnection, uFuncoes;

{$R *.lfm}

{ TFormConfigConexao }

procedure TFormConfigConexao.btOKClick(Sender: TObject);
var
  erro: Boolean;
  msg: string;
begin
  IniFile.WriteInteger('conexao', 'banco', rgBanco.ItemIndex);
  IniFile.WriteString('conexao', 'sid', edSID.Text);
  IniFile.WriteString('conexao', 'user', edUser.Text);
  IniFile.WriteString('conexao', 'pwd', EnDeCrypt(edSenha.Text));

  TConexao.FecharConexao;
  try
    erro := False;
    TConexao.GetConexao;
  except on e: Exception do
    begin
      erro := True;
      msg := e.Message;
    end;
  end;

  if not erro then
    Close
  else
    Application.MessageBox(PChar('Erro ao tentar estabelecer conexão com o banco de dados. ' + msg),
      'ATENÇÃO', MB_OK + MB_ICONSTOP);

end;

class procedure TFormConfigConexao.ConfigConexao;
begin
  Application.CreateForm(TFormConfigConexao, FormConfigConexao);
  with FormConfigConexao do
  begin
    rgBanco.ItemIndex := IniFile.ReadInteger('conexao', 'banco', 0);
    edSID.Text := IniFile.ReadString('conexao', 'sid', '');
    edUser.Text := IniFile.ReadString('conexao', 'user', '');
    edSenha.Text := EnDeCrypt(IniFile.ReadString('conexao', 'pwd', ''));
    ShowModal;
    Free;
  end;
end;

end.
