unit uVisualizarTriggers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, DBGrids, DB, BufDataset, uERNotationsCore;

type

  { TFormVisualizarTriggers }

  TFormVisualizarTriggers = class(TForm)
    cds: TBufDataset;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Fechar: TButton;
    Ds: TDataSource;
    procedure cdsAfterScroll(DataSet: TDataSet);
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

procedure TFormVisualizarTriggers.cdsAfterScroll(DataSet: TDataSet);
begin
  Memo1.Lines.Clear;
  memo1.Lines.Add(cds.FieldByName('description').AsWideString);
  memo1.Lines.Add(cds.FieldByName('trigger_body').AsVariant);
end;

class procedure TFormVisualizarTriggers.VisualizarTriggers(entity: TEntity);
begin
  Application.CreateForm(TFormVisualizarTriggers, FormVisualizarTriggers);
  with FormVisualizarTriggers do
  begin
    TObterMetaDados.ObterTriggers(entity.SchemaOwner, entity.NomeTabela, cds);
    ShowModal;
    Free;
  end;
end;

end.
