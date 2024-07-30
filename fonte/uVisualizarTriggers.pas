unit uVisualizarTriggers;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DBGrids, SynHighlighterXML, SynEdit,
  SynHighlighterSQL, IpHtml, DB, BufDataset, uERNotationsCore;

type

  { TFormVisualizarTriggers }

  TFormVisualizarTriggers = class(TForm)
    btFechar: TButton;
    buffer: TBufDataset;
    DBGrid1: TDBGrid;
    memoSQL: TSynEdit;
    Panel1: TPanel;
    Label1: TLabel;
    Ds: TDataSource;
    Panel2: TPanel;
    sqlHighLight: TSynSQLSyn;
    procedure btFecharClick(Sender: TObject);
    procedure bufferAfterScroll(DataSet: TDataSet);
    procedure FecharClick(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure VisualizarTriggers(entity: TEntity);
  end;

var
  FormVisualizarTriggers: TFormVisualizarTriggers;

implementation

uses uObterMetaDados;

{$R *.lfm}

{ TFormVisualizarTriggers }

procedure TFormVisualizarTriggers.bufferAfterScroll(DataSet: TDataSet);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := StringReplace(buffer.FieldByName('description').AsString, #10, sLineBreak, [rfReplaceAll]);
  memoSQL.Lines.Clear;
  memoSQL.Lines.AddStrings(sl);
  sl.Clear;
  sl.Text := StringReplace(buffer.FieldByName('trigger_body').AsString, #10, sLineBreak, [rfReplaceAll]);
  memoSQL.Lines.AddStrings(sl);
  memoSQL.SelStart := 1;
  sl.Free;
end;

procedure TFormVisualizarTriggers.btFecharClick(Sender: TObject);
begin
  Close;
end;

procedure TFormVisualizarTriggers.FecharClick(Sender: TObject);
begin

end;

procedure TFormVisualizarTriggers.Panel2Resize(Sender: TObject);
begin
  btFechar.Left := Trunc((Panel2.Width / 2) - (btFechar.Width / 2));
end;

class procedure TFormVisualizarTriggers.VisualizarTriggers(entity: TEntity);
begin
  Application.CreateForm(TFormVisualizarTriggers, FormVisualizarTriggers);
  with FormVisualizarTriggers do
  begin
    TObterMetaDados.ObterTriggers(entity.SchemaOwner, entity.NomeTabela, buffer);
    ShowModal;
    Free;
  end;
end;

end.
