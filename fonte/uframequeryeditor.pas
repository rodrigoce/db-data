unit uFrameQueryEditor;

{$mode objfpc}{$H+}

{
Criado por: Rodrigo Castro Eleot�rio
Data: 07/11/2014
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DBGrids, StdCtrls, SynEdit, SynHighlighterSQL, DB,
  BufDataset, SQLDB, LazUTF8;

type

  { TFrameQueryEditor }

  TFrameQueryEditor = class(TFrame)
    btExecuteQuery: TButton;
    btFechar: TButton;
    buff: TBufDataset;
    DBGrid1: TDBGrid;
    memoSQL: TSynEdit;
    Panel1: TPanel;
    Ds: TDataSource;
    Panel2: TPanel;
    sqlHighLight: TSynSQLSyn;
    SQLQuery1: TSQLQuery;
    procedure btFecharClick(Sender: TObject);
    procedure btExecuteQueryClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
    FOwnerTabela: string;
  public
    { Public declarations }
    procedure AvaliarEExecutarQuery;
    procedure ObterAmostra(OwnerTabela: string);
  end;

implementation

uses uObterMetaDados, uPrincipal;

{$R *.lfm}

{ TFrameQueryEditor }

procedure TFrameQueryEditor.AvaliarEExecutarQuery;
const securityMsg = 'Por motivos de seguran�a a primeira palavra da instru��o deve iniciar com o key word "select".' +
  LineEnding + 'Isso porque n�o foi implementado controle de transa��es para desfazer updates e deletes.';
var
  passou: Boolean;
begin
  passou := CompareText(Copy(memoSQL.Lines.Text, 1, 7), 'select ') = 0;
  if not passou then
  begin
    Application.MessageBox(securityMsg, 'Aten��o', MB_OK + MB_ICONSTOP);
    Exit;
  end;

  passou := Pos(' rownum ', memoSQL.Lines.Text) > 0;

  if not passou then
  begin
    Application.MessageBox('A key word "rownum" deve estar presente para evitar travamentos com massa de dado muito longa.',
      'Aten��o', MB_OK + MB_ICONSTOP);
    Exit;
  end;

  try
    Screen.Cursor := crHourGlass;
    buff.Close;
    TObterMetaDados.PopularBufferComSelect(memoSQL.Lines.Text, buff);
  finally
    Screen.Cursor := crDefault;
  end;

end;

procedure TFrameQueryEditor.btExecuteQueryClick(Sender: TObject);
begin
  AvaliarEExecutarQuery;
end;

procedure TFrameQueryEditor.Panel1Resize(Sender: TObject);
{var
  pnW: Integer;}
begin
{  pnW := Trunc(Panel1.Width / 2);
  btExecuteQuery.Left := Trunc(pnW - btExecuteQuery.Width + btExecuteQuery.Width / 2) - 2;
  btFechar.Left := Trunc(pnW + btExecuteQuery.Width - btExecuteQuery.Width / 2) + 2;}
end;

procedure TFrameQueryEditor.btFecharClick(Sender: TObject);
begin
  FormPrincipal.FeaturesHandler.FreeOpenedFeature(FOwnerTabela); // this already free this frame
end;

procedure TFrameQueryEditor.ObterAmostra(OwnerTabela: string);
begin
  FOwnerTabela := OwnerTabela;
  memoSQL.Lines.Clear;
  memoSQL.Lines.Add('select * from ' + OwnerTabela + ' where rownum <= 200');
  AvaliarEExecutarQuery;
end;

end.



