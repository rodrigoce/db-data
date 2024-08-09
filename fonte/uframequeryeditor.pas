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
  BufDataset, SQLDB, LazUTF8;

type

  { TFrameQueryEditor }

  TFrameQueryEditor = class(TFrame)
    btExecuteQuery: TButton;
    btFechar: TButton;
    buff: TBufDataset;
    GridData: TDBGrid;
    memoSQL: TSynEdit;
    Panel1: TPanel;
    Ds: TDataSource;
    Panel2: TPanel;
    sqlHighLight: TSynSQLSyn;
    SQLQuery1: TSQLQuery;
    procedure btFecharClick(Sender: TObject);
    procedure btExecuteQueryClick(Sender: TObject);
    procedure buffAfterOpen(DataSet: TDataSet);
    procedure buffBeforeOpen(DataSet: TDataSet);
    procedure GridDataTitleClick(Column: TColumn);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
    FOwnerTabela: string;
    FOldIndex: string;
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
const securityMsg = 'Por motivos de segurança a primeira palavra da instrução deve iniciar com o key word "select".' +
  LineEnding + 'Isso porque não foi implementado controle de transações para desfazer updates e deletes.';
var
  passou: Boolean;
begin
  passou := CompareText(Copy(memoSQL.Lines.Text, 1, 7), 'select ') = 0;
  if not passou then
  begin
    Application.MessageBox(securityMsg, 'Atenção', MB_OK + MB_ICONSTOP);
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

procedure TFrameQueryEditor.btExecuteQueryClick(Sender: TObject);
begin
  AvaliarEExecutarQuery;
end;

procedure TFrameQueryEditor.buffAfterOpen(DataSet: TDataSet);
var
  indexedCol: string;
  //
   ColWidth : array of cardinal;
   Counter, avaliacoes, qtdeMax  : integer;
begin
  buff.IndexFieldNames := FOldIndex;
  if buff.IndexFieldNames <> '' then
  begin
    indexedCol := StringReplace(buff.IndexFieldNames, ' DESC', '', [rfReplaceAll]);
    if Pos(' DESC', buff.IndexFieldNames) > 0 then
      GridData.Columns.ColumnByFieldname(indexedCol).Title.Font.Color := clPurple
    else
      GridData.Columns.ColumnByFieldname(indexedCol).Title.Font.Color := clBlue;
  end;
  GridData.AutoAdjustColumns;

  //
    SetLength(ColWidth,buff.FieldCount);

  for Counter := 0 to buff.FieldCount - 1 do
  begin
    ColWidth[Counter] := Max(ColWidth[Counter],GridData.Canvas.TextWidth(GridData.Columns[Counter].Title.Caption) + 8);
    GridData.Columns[Counter].Width := ColWidth[Counter];
  end;

  try
    with buff do
    begin
      DisableControls;
      avaliacoes := 0;
      qtdeMax := Min(50, RecordCount);

      while (avaliacoes <= qtdeMax) do
      begin
        for Counter := 0 to ( FieldCount - 1 ) do
        begin
          ColWidth[Counter] := Max(ColWidth[Counter],GridData.Canvas.TextWidth(Fields[Counter].AsString) + 8);
          GridData.Columns[Counter].Width := ColWidth[Counter];
        end;
        Inc(avaliacoes);
        Next;
      end;
      First;
      EnableControls;
    end;
  finally
    Finalize(ColWidth);
  end;

end;

procedure TFrameQueryEditor.buffBeforeOpen(DataSet: TDataSet);

begin
  FOldIndex := buff.IndexFieldNames;
  buff.IndexFieldNames := '';
end;

procedure TFrameQueryEditor.GridDataTitleClick(Column: TColumn);
var
  col: TCollectionItem;
begin
  for col in GridData.Columns do
    (col as TColumn).Title.Font.Color := clBlack;

  if buff.Active then
  begin
    if buff.IndexFieldNames = Column.FieldName then
    begin
      buff.IndexFieldNames := Column.FieldName + ' DESC';
      Column.Title.Font.Color := clPurple;
    end
    else
    begin
      buff.IndexFieldNames := Column.FieldName;
      Column.Title.Font.Color := clBlue;
    end;
  end;
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



