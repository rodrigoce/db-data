unit uVisualizarTriggers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, DBGrids, SynHighlighterXML, uPSComponent,
  DB, BufDataset, uERNotationsCore;

type

  { TFormVisualizarTriggers }

  TFormVisualizarTriggers = class(TForm)
    btFechar: TButton;
    buffer: TBufDataset;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Ds: TDataSource;
    Panel2: TPanel;
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
begin
  Memo1.Lines.Clear;
  memo1.Lines.Add(StringReplace(buffer.FieldByName('description').AsString, #10, #13#10, [rfReplaceAll]));
  memo1.Lines.Add(StringReplace(buffer.FieldByName('trigger_body').AsString, #10, #13#10, [rfReplaceAll]));
  memo1.SelStart := 1;
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
