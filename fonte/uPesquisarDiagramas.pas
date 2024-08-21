unit uPesquisarDiagramas;

{$MODE Delphi}

{
  2013 by Rodrigo Castro Eleotério
  2024 ported from Delphi to FreePascal/Lazarus by Rodrigo Castro Eleotério
}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BufDataset, DB, DBGrids, StdCtrls, uFeaturesHandler;

type

  { TFormPesquisarDiagramas }

  TFormPesquisarDiagramas = class(TForm)
    DBGrid1: TDBGrid;
    ds: TDataSource;
    Label1: TLabel;
    edPesquisa: TEdit;
    btCarregar: TButton;
    btFechar: TButton;
    procedure edPesquisaChange(Sender: TObject);
    procedure HabilitaBotoes;
    procedure btFecharClick(Sender: TObject);
    procedure btCarregarClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
  private
    { Private declarations }

    FFeaturesHandler: TFeaturesHandler;
    const FDefaultFilter = 'status <> "E"';
  public
    { Public declarations }
    class procedure PesquiarDiagramas(cdsDiagramas: TBufDataSet; FeaturesHandler: TFeaturesHandler);
  end;

var
  FormPesquisarDiagramas: TFormPesquisarDiagramas;

implementation

{$R *.lfm}

{ TFormPesquisarDiagramas }

procedure TFormPesquisarDiagramas.btCarregarClick(Sender: TObject);
begin
  FFeaturesHandler.OpenEntityContainer(ds.DataSet.FieldByName('id').AsString);
end;

procedure TFormPesquisarDiagramas.btFecharClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPesquisarDiagramas.DBGrid1DblClick(Sender: TObject);
begin
  btCarregar.Click;
end;

procedure TFormPesquisarDiagramas.edPesquisaChange(Sender: TObject);
var
  filter: string;
begin
  ds.DataSet.FilterOptions := [foCaseInsensitive];

  if edPesquisa.GetTextLen > 0 then
    filter := FDefaultFilter + ' and titulo = "*' + edPesquisa.Text + '*"'
  else
    filter := FDefaultFilter;

  ds.DataSet.Filter := filter;
  ds.DataSet.First;
end;

procedure TFormPesquisarDiagramas.HabilitaBotoes;
begin
  btCarregar.Enabled := ds.DataSet.RecordCount > 0;
end;

class procedure TFormPesquisarDiagramas.PesquiarDiagramas(
  cdsDiagramas: TBufDataSet; FeaturesHandler: TFeaturesHandler);
begin
  Application.CreateForm(TFormPesquisarDiagramas, FormPesquisarDiagramas);
  with FormPesquisarDiagramas do
  begin
    FFeaturesHandler := FeaturesHandler;
    ds.DataSet := cdsDiagramas;
    // não mostrar os excluídos
    ds.DataSet.Filter := FDefaultFilter;
    ds.DataSet.Filtered := True;
    //
    HabilitaBotoes;
    ShowModal;
    ds.DataSet.Filtered := False;
  end;
end;

end.
