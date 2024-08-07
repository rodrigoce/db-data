unit uFrameQueryEditor;

{$mode objfpc}{$H+}

{
Criado por: Rodrigo Castro Eleotério
Data: 07/11/2014
}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DBGrids, StdCtrls, SynEdit, SynHighlighterSQL, DB,
  BufDataset, SQLDB;

type

  { TFrameConsultaDados }

  TFrameConsultaDados = class(TFrame)
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

{ TFrameConsultaDados }

procedure TFrameConsultaDados.AvaliarEExecutarQuery;
var
  passou: Boolean;
begin
  passou := Copy(memoSQL.Lines.Text, 1, 6) = 'select ';
  if not passou then
  begin
    Application.MessageBox(PChar('Por motivos de segurança a primeira palavra da instrução deve iniciar com o key word "select".' +
      LineEnding + 'Isso porque não foi implementado controle de transações para desfazer updates e deletes.'),
      'Atenção', MB_OK + MB_ICONSTOP);
    Exit;
  end;

  passou := Pos(' rownum ', memoSQL.Lines.Text) > 0;

  if not passou then
  begin
    Application.MessageBox('A key word "rownum" deve estar presente para evitar travamentos com massa de dado muito longa.',
      'Atenção', MB_OK + MB_ICONSTOP);
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

procedure TFrameConsultaDados.btExecuteQueryClick(Sender: TObject);
begin
  AvaliarEExecutarQuery;
end;

procedure TFrameConsultaDados.Panel1Resize(Sender: TObject);
{var
  pnW: Integer;}
begin
{  pnW := Trunc(Panel1.Width / 2);
  btExecuteQuery.Left := Trunc(pnW - btExecuteQuery.Width + btExecuteQuery.Width / 2) - 2;
  btFechar.Left := Trunc(pnW + btExecuteQuery.Width - btExecuteQuery.Width / 2) + 2;}
end;

procedure TFrameConsultaDados.btFecharClick(Sender: TObject);
begin
  FormPrincipal.FeaturesHandler.FreeOpenedFeature(FOwnerTabela); // this already free this frame
end;

procedure TFrameConsultaDados.ObterAmostra(OwnerTabela: string);
begin
  FOwnerTabela := OwnerTabela;
  memoSQL.Lines.Clear;
  memoSQL.Lines.Add('select * from ' + OwnerTabela + ' where rownum <= 200');
  AvaliarEExecutarQuery;
end;

end.



