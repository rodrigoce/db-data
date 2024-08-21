unit uMoverObjetos;

{$MODE Delphi}

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormMoverObjetos = class(TForm)
    Label1: TLabel;
    Cima: TButton;
    Baixo: TButton;
    Esquerda: TButton;
    Direita: TButton;
    procedure CimaClick(Sender: TObject);
    procedure BaixoClick(Sender: TObject);
    procedure EsquerdaClick(Sender: TObject);
    procedure DireitaClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure MoverObjetos;
  end;

var
  FormMoverObjetos: TFormMoverObjetos;

implementation

uses uFeaturesHandler, uPrincipal;

{$R *.lfm}

procedure TFormMoverObjetos.BaixoClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt.Y := 20;
  pt.X := 0;
  FormPrincipal.FeaturesHandler.CurrentDiagram.MoverTudo(pt);
end;

procedure TFormMoverObjetos.CimaClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt.Y := -20;
  pt.X := 0;
  FormPrincipal.FeaturesHandler.CurrentDiagram.MoverTudo(pt);
end;

procedure TFormMoverObjetos.DireitaClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt.X := 20;
  pt.Y := 0;
  FormPrincipal.FeaturesHandler.CurrentDiagram.MoverTudo(pt);
end;

procedure TFormMoverObjetos.EsquerdaClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt.X := -20;
  pt.Y := 0;
  FormPrincipal.FeaturesHandler.CurrentDiagram.MoverTudo(pt);
end;

class procedure TFormMoverObjetos.MoverObjetos;
begin
  Application.CreateForm(TFormMoverObjetos, FormMoverObjetos);
  with FormMoverObjetos do
  begin
    ShowModal;
    Free;
  end;
end;

end.
