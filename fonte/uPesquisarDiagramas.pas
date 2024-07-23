unit uPesquisarDiagramas;

{$MODE Delphi}

{
Criado por: Rodrigo Castro Eleotério
Data: 10/10/2013
}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BufDataset, DB, Grids, DBGrids, StdCtrls, uDiagramaManager;

type

  { TFormPesquisarDiagramas }

  TFormPesquisarDiagramas = class(TForm)
    DBGrid1: TDBGrid;
    ds: TDataSource;
    Edit1: TEdit;
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

    FDiagramaManager: TDiagramaManager;
    const FDefaultFilter = 'status <> ''E''';
  public
    { Public declarations }
    class procedure PesquiarDiagramas(cdsDiagramas: TBufDataSet; diagramaManager: TDiagramaManager);
  end;

var
  FormPesquisarDiagramas: TFormPesquisarDiagramas;

implementation

{$R *.lfm}

{ TFormPesquisarDiagramas }

procedure TFormPesquisarDiagramas.btCarregarClick(Sender: TObject);
begin
  FDiagramaManager.OpenEntityContainer(ds.DataSet.FieldByName('id').AsString);
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
begin
  //ds.DataSet.Locate('titulo', edPesquisa.Text, [loPartialKey, loCaseInsensitive])
  if edPesquisa.GetTextLen > 0 then
    ds.DataSet.Filter := FDefaultFilter + ' and lower(titulo) = ''*' + LowerCase(edPesquisa.Text) + '*'''
  else
    ds.DataSet.Filter := FDefaultFilter;
  edit1.Tag:= edit1.Tag + 1;
  edit1.Text := ds.DataSet.Filter + ' - ' + inttostr(edit1.Tag);
end;

procedure TFormPesquisarDiagramas.HabilitaBotoes;
begin
  btCarregar.Enabled := ds.DataSet.RecordCount > 0;
end;

class procedure TFormPesquisarDiagramas.PesquiarDiagramas(
  cdsDiagramas: TBufDataSet; diagramaManager: TDiagramaManager);
begin
  Application.CreateForm(TFormPesquisarDiagramas, FormPesquisarDiagramas);
  with FormPesquisarDiagramas do
  begin
    FDiagramaManager := diagramaManager;
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
