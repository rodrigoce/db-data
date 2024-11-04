unit uIncludeTables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, StdCtrls,
  uVariaveisGlobais, uAppFile, Generics.Collections, BufDataset, DB,
  StrUtils, Types;

type

  { TFormIncludedTables }

  TFormIncludedTables = class(TForm)
    buf: TBufDataset;
    bufdiagramas: TStringField;
    bufocorrencias: TLongintField;
    bufowner: TStringField;
    buftabela: TStringField;
    ds: TDataSource;
    grid: TDBGrid;
    labCount: TLabel;
    procedure gridTitleClick(Column: TColumn);
  private

  public
    class procedure Open;
  end;

var
  FormIncludedTables: TFormIncludedTables;

implementation

{$R *.lfm}

{ TFormIncludedTables }

procedure TFormIncludedTables.gridTitleClick(Column: TColumn);
var
  col: TCollectionItem;
begin
  for col in grid.Columns do
    (col as TColumn).Title.Font.Color := clBlack;

  if buf.Active then
  begin
    if buf.IndexFieldNames = Column.FieldName then
    begin
      buf.IndexFieldNames := Column.FieldName + ' DESC';
      Column.Title.Font.Color := clPurple;
    end
    else
    begin
      buf.IndexFieldNames := Column.FieldName;
      Column.Title.Font.Color := clBlue;
    end;
    buf.First;
  end;
end;

class procedure TFormIncludedTables.Open;
var
  diagrama: TFileDiagram;
  entidade: TFileEntity;
  dic: specialize TDictionary<string, TStringList>;
  fullTableName: string;
  sl: TStringList;
  key: string;
  strParts: TStringDynArray;
begin
  Application.CreateForm(TFormIncludedTables, FormIncludedTables);
  with FormIncludedTables do
  begin
    dic := specialize TDictionary<string, TStringList>.Create();
    for diagrama in AppFile.Diagrams do
    begin
      for entidade in diagrama.Entities do
      begin
        fullTableName := entidade.Owner + '.' + entidade.Table;
        if dic.ContainsKey(fullTableName) then
          dic[fullTableName].Add(diagrama.Title)
        else
        begin
          sl := TStringList.Create;
          sl.Add(diagrama.Title);
          sl.Delimiter := ',';
          dic.Add(fullTableName, sl);
        end;
      end;
    end;

    buf.CreateDataset;
    for key in dic.Keys do
    begin
      strParts := SplitString(key, '.');
      buf.Append;
      bufowner.Value := strParts[0];
      buftabela.Value := strParts[1];
      bufocorrencias.Value := dic[key].Count;
      bufdiagramas.Value := dic[key].DelimitedText;
      buf.Post;
      dic[key].Free;
    end;
    buf.First;
    grid.AutoAdjustColumns;
    labCount.Caption := IntToStr(buf.RecordCount) + ' tabelas';
    gridTitleClick(grid.Columns[1]);
    dic.Free;

    ShowModal;

  end;
end;

end.

