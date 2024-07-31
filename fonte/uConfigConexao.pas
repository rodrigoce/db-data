unit uConfigConexao;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 01/10/2013
}
interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, EditBtn, uAppFile;

type

  { TFormConfigConexao }

  TFormConfigConexao = class(TForm)
    edTNS: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    edUser: TEdit;
    edSenha: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edSID: TEdit;
    btOK: TButton;
    Label5: TLabel;
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

uses uVariaveisGlobais, uConnection;

{$R *.lfm}

{ TFormConfigConexao }

procedure TFormConfigConexao.btOKClick(Sender: TObject);
var
  erro: Boolean;
  msg: string;
begin
  AppFile.SID := edSID.Text;
  AppFile.UserName := edUser.Text;
  AppFile.Password := edSenha.Text;
  AppFile.TNSPath := edTNS.Text;

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
    edSID.Text := AppFile.SID;
    edUser.Text := AppFile.UserName;
    edSenha.Text := AppFile.Password;
    edTNS.Text := AppFile.TNSPath;
    ShowModal;
    Free;
  end;
end;

end.
